// Copyright 2011 MIT Haystack Observatory
// 
// This file is part of Mark6.
// 
// Mark6 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 2 of the License.
// 
// Mark6 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Mark6.  If not, see <http://www.gnu.org/licenses/>.

//
// Author: David Lapsley <dlapsley@haystack.mit.edu>
//

// C includes
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// C++ includes.
#include <sstream>
#include <iostream>

// Framework includes.

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <stats_writer.h>

const double DEFAULT_ALPHA = 0.5;
const int DEFAULT_PAGE_LENGTH = 10;

StatsWriter::StatsWriter(const int id,
			 const std::string& stats_file,
			 const int stats_interval,
			 const double command_interval):
  Threaded(id, command_interval),
  _stats_interval(stats_interval),
  _num_packets(0),
  _num_bytes(0),
  _dropped_packets(0),
  _buffer_size(0),
  _stats_stream(),
  _csv_stream(),
  _state(IDLE),
  _mutex(),
  _average_packet_rate(0),
  _average_byte_rate(0),
  _last_num_packets(0),
  _last_num_bytes(0),
  _interval(0),
  _ALPHA(DEFAULT_ALPHA),
  _BPS_TO_MBPS(8/1e6),
  _PAGE_LENGTH(10)
{
  const std::string stats_stream_name = stats_file + std::string(".sts");
  _stats_stream.open(stats_stream_name.c_str(),
		     std::fstream::out | std::fstream::trunc);

  const std::string csv_stream_name = stats_file + std::string(".csv");
  _csv_stream.open(csv_stream_name.c_str(),
		   std::fstream::out | std::fstream::trunc);
  _start_time.tv_sec = 0;
  _start_time.tv_usec = 0;
  _last_time.tv_sec = 0;
  _last_time.tv_usec = 0;
}

StatsWriter::~StatsWriter() {
  _stats_stream.close();
  _csv_stream.close();
}

void StatsWriter::start() {
  _running = true;
  _thread = boost::thread(&StatsWriter::run, this);
}

void StatsWriter::join() {
  _thread.join();
}

void StatsWriter::run() {
  LOG4CXX_DEBUG(logger, "StatsWriter Running...");

  Timer run_timer;
  Timer command_timer;
  
  try {
    // Main processing loop.
    while (_running) {

      // State machine.
      switch (_state) {
      case WRITE_TO_DISK:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_write_to_disk();
	break;

      case IDLE:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_idle();
	break;

      case STOP:
	handle_stop();
	break;

      default:
	LOG4CXX_ERROR(logger, "Unknown state.");
	break;
      }
    }
    LOG4CXX_DEBUG(logger, "elapsed run time: " << run_timer.elapsed());
  } catch(std::exception &ex) {
    LOG4CXX_ERROR(logger, "error: " << ex.what());
  }
}

void StatsWriter::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void StatsWriter::cmd_write_to_disk() {
  LOG4CXX_DEBUG(logger, "Received WRITE_TO_DISK");
  _state = WRITE_TO_DISK;
}

void StatsWriter::handle_stop() {
  _running = false;
}

void StatsWriter::handle_idle() {
  usleep(_command_interval*1000000);
}

void StatsWriter::handle_write_to_disk() {
  struct timeval end_time;
  struct timeval diff, instant_diff;

  // Cache stats variables.
  boost::uint64_t num_packets;
  boost::uint64_t num_bytes;
  {
    boost::mutex::scoped_lock lock(_mutex);
    num_packets = _num_packets;
    num_bytes = _num_bytes;
  }

  // Update time variables.
  if (_start_time.tv_sec == 0) {
    gettimeofday(&_start_time, NULL);
  }
  if (_last_time.tv_sec == 0) {
    gettimeofday(&_last_time, NULL);
  }
  gettimeofday(&end_time, NULL);

  // Elapsed _interval.
  timersub(&end_time, &_start_time, &diff);
  timersub(&end_time, &_last_time, &instant_diff);

  const double delta_seconds = diff.tv_sec + diff.tv_usec/1000000.0;
  const double instant_delta_seconds = instant_diff.tv_sec
    + instant_diff.tv_usec/1000000.0;
  if (delta_seconds <= 0 || instant_delta_seconds <=0)
    return;

  const double instant_packet_rate = (num_packets - _last_num_packets)
    /instant_delta_seconds;
  const double instant_byte_rate = _BPS_TO_MBPS*(num_bytes - _last_num_bytes)
    /instant_delta_seconds;
  const double lifetime_packet_rate = num_packets/delta_seconds;
  const double lifetime_byte_rate = _BPS_TO_MBPS*num_bytes/delta_seconds;
  const double average_buffer_size = _buffer_size/delta_seconds;
  const double instant_drop_rate = _dropped_packets/delta_seconds;

  _average_packet_rate = (1.0-_ALPHA)*_average_packet_rate
    + _ALPHA*instant_packet_rate;
  _average_byte_rate = (1.0-_ALPHA)*_average_byte_rate
    + _ALPHA*instant_byte_rate;

  if (_interval % _PAGE_LENGTH == 0) {
    _stats_stream
      << HLINE
      << std::endl
      << "|"
      << std::setw(10) << "time" << " "
      << std::setw(10) << "pkt rate" << " "
      << std::setw(10) << "pkt rate"   << " "
      << std::setw(10) << "pkt rate"  << " "
      << "|"
      << std::setw(10) << "mbps" << " "
      << std::setw(10) << "mbps"   << " "
      << std::setw(10) << "mbps"  << " "
      << "|\n"
      << "|"
      << std::setw(10) << "(s)" << " "
      << std::setw(10) << "(inst)" << " "
      << std::setw(10) << "(lifetime)"   << " "
      << std::setw(10) << "(average)"  << " "
      << "|"
      << std::setw(10) << "(inst)" << " "
      << std::setw(10) << "(lifetime)"   << " "
      << std::setw(10) << "(average)"  << " "
      << "|\n"
      << HLINE
      << std::endl;
  }
  ++_interval;

  _stats_stream
    << "|"
    << std::setw(10) << delta_seconds << " "
    << std::setw(10) << instant_packet_rate << " "
    << std::setw(10) << lifetime_packet_rate << " "
    << std::setw(10) << _average_packet_rate << " "
    << "|"
    << std::setw(10) << instant_byte_rate << " "
    << std::setw(10) << lifetime_byte_rate << " "
    << std::setw(10) << _average_byte_rate << " "
    << "|\n";
  
  _csv_stream
    <<  delta_seconds << " "
    <<  instant_packet_rate << " "
    <<  lifetime_packet_rate << " "
    <<  _average_packet_rate << " "
    <<  instant_byte_rate << " "
    <<  lifetime_byte_rate << " "
    <<  _average_byte_rate << " "
    << instant_drop_rate << " "
    << average_buffer_size
    << std::endl;
  
  _dropped_packets = 0;
  _buffer_size = 0;

  // Update state.
  gettimeofday(&_last_time, NULL);
  _last_num_packets = num_packets;
  _last_num_bytes = num_bytes;

  sleep(_stats_interval);
}

void StatsWriter::update(const boost::uint64_t& packets,
			 const boost::uint64_t& bytes,
			 const boost::uint64_t& dropped_packets,
			 const boost::uint64_t& buffer_size) {
  boost::mutex::scoped_lock lock(_mutex);
  _num_packets += packets;
  _num_bytes += bytes;
  _dropped_packets += dropped_packets;
  _buffer_size = buffer_size;
}
