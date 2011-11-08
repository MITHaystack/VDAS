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

#ifndef _STATS_WRITER_H_
#define _STATS_WRITER_H_

// C includes.

// C++ includes
//! \todo Track this down and fix.
#undef max // Undef to fix broken macro collision with fstream
#undef min // Undef to fix broken macro collision with fstream
#include <iostream>
#include <fstream>

// Framework includes.
#include <cstddef>    // for std::size_t
#include <boost/thread/mutex.hpp>

// Local includes
#include <mark6.h>
#include <threaded.h>


//! This class maintains counters, averages, etc. for tracking the statistics
//! of a monitored object. The monitored object periodically calls the update()
//! method which then updates the statistics counters etc. and then updates
//! instantaneous, exponentially averaged, and long term statistics.
//! Periodically, the data are written out to a CSV file with a name that is 
//! unique to the monitored object.
class StatsWriter: public Threaded {

  //---------------------------------------------------------------------------
  // Public API
  //---------------------------------------------------------------------------
 public:
  //! Constructor.
  //! \param id A unique id for this object. Used for logging.
  //! \param stats_file The name of the statistics CSV file.
  //! \param command_interval The main executin thread in run() will attempt
  //!        to check for new commands every command_interval seconds. The
  //!        actual interval between checks may be larger than this if the 
  //!        execution thread spends longer than command_interval processing
  //!        individual tasks.
  StatsWriter(const int id,
	      const std::string& stats_file,
	      const int stats_interval,
	      const double command_interval);

  //! Destructor.
  virtual ~StatsWriter();

  //! Start the main processing loop run() in its own thread of execution.
  virtual void start();

  //! Wait for the main processing loop/thread to exit.
  virtual void join();

  //! External API command. Insert a STOP command into the object's command
  //! queue for processing.
  virtual void cmd_stop();

  //! External API command. Insert a WRITE_TO_DISK command into the object's
  //! command queue for processing.
  void cmd_write_to_disk();

  //! Custom API. Update statistics.
  //! \param packets The total number of packets that have been processed
  //!        so far.
  //! \param bytes The total number of bytes that have been processed so far.
  void update(const boost::uint64_t& packets,
	      const boost::uint64_t& bytes,
	      const boost::uint64_t& dropped_packets,
	      const boost::uint64_t& buffer_size);


 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------

  //! The interval between internal statistics updates and dumps to disk.
  const int _stats_interval;

  //! The total number of packets that have been processed so far.
  boost::uint64_t _num_packets;

  //! The total number of bytes that have been processed so far.
  boost::uint64_t _num_bytes;

  //! The number of dropped packets.
  boost::uint64_t _dropped_packets;
 
  //! The size of the file buffer.
  boost::uint64_t _buffer_size;

  //! Nicely formatted statistics output (useful for real-time stats output).
  std::ofstream _stats_stream;

  //! CSV formatted statistics dump for postprocessing.
  std::ofstream _csv_stream;

  //! The state of the object.
  volatile enum { IDLE, WRITE_TO_DISK, STOP } _state;

  //! Mutex for serializing multi-threaded access to shared resources.
  boost::mutex _mutex;

  //! The time logging was started.
  struct timeval _start_time;

  //! The most recent statistics update time.
  struct timeval _last_time;

  //! The exponentially weighted moving average packet rate.
  double _average_packet_rate;

  //! The exponentially weighted moving average byte rate.
  double _average_byte_rate;

  //! The number of packets that had been processed at the last statistics
  //! update.
  boost::uint64_t _last_num_packets;

  //! The number of bytes that had been processed at the last statistics
  //! update.
  boost::uint64_t _last_num_bytes;

  //! The "number" of the current measurement interval. Used for formatting
  //! purposes. Every time statistics information is output, interval is 
  //! incremented.
  long _interval;

  //! The parameter used in the exponentially weighted moving average.
  const double _ALPHA;

  //! A constant that converts from BPS to MBPS.
  const double _BPS_TO_MBPS;

  //! A formatting constant. Controls how many lines are output to a "page"
  //! for the _stats_stream.
  const int _PAGE_LENGTH;


  //---------------------------------------------------------------------------
  // Internal methods
  //---------------------------------------------------------------------------

  //! Main processing loop.
  virtual void run();

  //! Command handler. Handles the STOP command/state. On receiving this
  //! command, the FileWriter object will stop processing and exit the 
  //! run() method at the next opportunity.  
  virtual void handle_stop();

  //! Command handler. Handles the IDLE state.
  virtual void handle_idle();

  //! Command handler. Handles the WRITE_TO_DISK command/state. On receiving
  //! this command, the StatsWriter object will start tracking statistics,
  //! and periodically updating them, and writing them to disk.
  virtual void handle_write_to_disk();
};

#endif // _STATS_WRITER_H_

