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
#include <linux/falloc.h>
#include <stdlib.h>
#define TRANSLATE
#ifdef TRANSLATE
#include <pcap.h>
#include <netinet/ip.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>

//C++ includes
#include <list>
#include <sstream>
#include <string>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp> 

#define ETHER_TYPE_IP (0x0800)
#define ETHER_TYPE_8021Q (0x8100)
#define UDP_PROTOCOL_NUMBER (17)
#define UDP_HEADER_LENGTH (8)

// C++ includes.
#include <sstream>
#include <iostream>
#include <map>

// Framework includes.
#include <boost/crc.hpp>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <file_writer.h>
#include <stats_writer.h>

namespace fs = boost::filesystem;

typedef std::map<int, int> map_t;

FileWriter::FileWriter(const int id,
		       const int write_block_size,
		       const int  write_blocks,
		       const std::string& capture_file,
		       const int poll_timeout,
		       StatsWriter* const sw,
		       const double command_interval,
		       const unsigned long file_size,
		       const bool preallocated=false,
		       const bool directio=false,
		       const bool translate=false):
  Threaded(id, command_interval),
  _WRITE_BLOCK_SIZE(write_block_size),
  _WRITE_BLOCKS(write_blocks),
  _POLL_TIMEOUT(poll_timeout),
  _sw(sw),
  _pfd(),
  _write_bufs(),
  _free_bufs(),
  _state (IDLE),
  _write_bufs_mutex(),
  _free_bufs_mutex(),
  _capture_file(capture_file),
  _file_size(file_size),
  _preallocated(preallocated),
  _directio(directio),
  _translate(translate)
{
  void* buf;
  for (int i=0; i<write_blocks; i++) {
    if (posix_memalign(&buf, getpagesize(), write_block_size) != 0) {
      LOG4CXX_ERROR(logger, "FileWriter buffer allocation failed.");
      throw std::string("Memalign failed.");
    }
    _free_bufs.push_back(static_cast<boost::uint8_t*>(buf));
  }
}

FileWriter::~FileWriter() {
  close();
#ifdef TRANSLATE
  if (_translate) {
    LOG4CXX_INFO(logger, "Starting translation on : " << _capture_file);

    // open the pcap file 
    pcap_t *handle; 
    char errbuf[PCAP_ERRBUF_SIZE];
    struct pcap_pkthdr header;
    const u_char* packet;

    handle = pcap_open_offline(_capture_file.c_str(), errbuf);
    if (handle == NULL) {
      LOG4CXX_ERROR(logger, "Couldn't open pcap file " << _capture_file
		    << " " << errbuf);
      exit(1);
    }

    fs::path p(_capture_file.c_str());
    const std::string directory(p.parent_path().string());
    const std::string filename(p.stem());
    map_t fd_map;

    // Packet processing loop.
    while (packet = pcap_next(handle, &header) ) {
      int fd = -1;
      u_char *pkt_ptr = (u_char *)packet;
      int ether_type = ((int)(pkt_ptr[12]) << 8) | (int)pkt_ptr[13]; 
      int ether_offset = 0; 
      if (ether_type == ETHER_TYPE_IP)
	ether_offset = 14; 
      else if (ether_type == ETHER_TYPE_8021Q)
	ether_offset = 18; 
      else 
	LOG4CXX_ERROR(logger, "Unknown ethernet type skipping...");
      
      // Parse the IP header 
      pkt_ptr += ether_offset;
      struct ip *ip_hdr = (struct ip *)pkt_ptr;
      const int header_length = ip_hdr->ip_hl*4;
      const int packet_length = ntohs(ip_hdr->ip_len);
      if (ip_hdr->ip_p == UDP_PROTOCOL_NUMBER) {
	pkt_ptr += header_length;
	struct udphdr* udp_hdr = (struct udphdr*)pkt_ptr;
	const int dport = ntohs(udp_hdr->dest);
	std::map<int, int>::iterator it = fd_map.find(dport);
	if (it != fd_map.end()) {
	  fd = fd_map[dport];
	} else {
	  std::ostringstream oss (std::ostringstream::out);
	  oss << directory << "/" << filename << "-" << dport << ".vdif";
	  fd_map[dport] = ::open(oss.str().c_str(),
				 O_WRONLY | O_CREAT, S_IRWXU);
	}

	const int udp_length = ntohs(udp_hdr->len) - UDP_HEADER_LENGTH;
	pkt_ptr += UDP_HEADER_LENGTH;
	u_char* data = pkt_ptr;
	int nb = ::write(fd, data, udp_length);
      }
    }
    
    pcap_close(handle);  //close the pcap file 

    BOOST_FOREACH(map_t::value_type& v, fd_map) {
      ::close(v.second);
    }
  }
#endif // TRANSLATE
}

void FileWriter::start() {
  _running = true;
  _thread = boost::thread(&FileWriter::run, this);
}

void FileWriter::join() {
  _thread.join();
}

void FileWriter::run() {
  LOG4CXX_INFO(logger, "FileWriter Running...");

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

void FileWriter::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void FileWriter::cmd_write_to_disk() {
  LOG4CXX_INFO(logger, "Received WRITE_TO_DISK");
  _state = WRITE_TO_DISK;
}

void FileWriter::handle_stop() {
  _running = false;
}

void FileWriter::handle_idle() {
  usleep(_command_interval*1000000);
}

void FileWriter::handle_write_to_disk() {
  write_block();
}

int FileWriter::open() {
  LOG4CXX_INFO(logger, "Opening FileWriter file: " << _capture_file);

  // Open files for each path.
  int ret=0;

  if (_directio) {
    _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_CREAT | O_DIRECT,
		     S_IRWXU);
  } else {
    _pfd.fd = ::open(_capture_file.c_str(), O_WRONLY | O_CREAT, S_IRWXU);
  }
 
  if (_pfd.fd<0) {
    LOG4CXX_ERROR(logger, "Unable to open file: " << _capture_file
		  << " - " << strerror(errno));
    _pfd.fd = -1;
    ret = -1;
  } else {
    LOG4CXX_DEBUG(logger, "File: " << _capture_file << " fd: "
		  << _pfd.fd);
    _pfd.events = POLLOUT;
  }

  if (_preallocated) {
    off_t len = _file_size * 1000000;
    LOG4CXX_INFO(logger, "Preallocating  " << len/1000000 << " MBytes");

    // Scope errno locally for fallocate.
    int myerrno = fallocate(_pfd.fd, FALLOC_FL_KEEP_SIZE, 0, len);
    if (myerrno != 0) {
      LOG4CXX_ERROR(logger, "Fallocate() failed: " << strerror(myerrno));
      return -1;
    }
    
    if (::lseek(_pfd.fd, 0, SEEK_SET) < 0) {
      LOG4CXX_ERROR(logger, "Unable to seek to beginning of file: "
		    << _capture_file
		    << " - " << strerror(errno));
      _pfd.fd = -1;
      ret = -1;
    }
  } else {
    LOG4CXX_DEBUG(logger, "Successfully seeked.");
  }

  // Debug message.
  LOG4CXX_DEBUG(logger, "pfd: " << _pfd.fd);

  return ret;
}

int FileWriter::close() {
  if ( (_pfd.fd>0) && (::close(_pfd.fd)<0) ) {
    LOG4CXX_ERROR(logger, "Unable to close fd: " << _pfd.fd
		  << " - " << strerror(errno));
    return -1;
  }
  return 0;
}

boost::uint8_t* FileWriter::malloc_buffer() {
  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_POLL_TIMEOUT*1000);

  boost::mutex::scoped_lock lock(_free_bufs_mutex); 
  while (_free_bufs.empty())
    if (!_read_cond.timed_wait(lock, timeout))
      return 0;
 
  boost::uint8_t* b = _free_bufs.front();
  _free_bufs.pop_front();

  return b;
}

bool FileWriter::free_buffer(boost::uint8_t* buf) {

  boost::system_time timeout = boost::get_system_time() 
    + boost::posix_time::milliseconds(_POLL_TIMEOUT*1000);

  // Never block on free because total buffer pool is fixed.
  // This assumes only buffers allocated from this FileWriter
  // are returned to it.
  boost::mutex::scoped_lock lock(_free_bufs_mutex);    
  _free_bufs.push_back(buf);
  _read_cond.notify_one();
  return true;
}

bool FileWriter::write(boost::uint8_t* b) {
  boost::mutex::scoped_lock lock(_write_bufs_mutex);
  _write_bufs.push_back(b);
// #define BLOCKING
#ifdef BLOCKING
  _write_cond.notify_one();
#endif
  return true;
}

void FileWriter::write_block() {
  boost::uint8_t* buf;
  boost::uint64_t buf_len;

#ifdef BLOCKING
  boost::system_time timeout = boost::get_system_time() 
  	+ boost::posix_time::milliseconds(_POLL_TIMEOUT*1000);
#endif

  {
    boost::mutex::scoped_lock lock(_write_bufs_mutex); 
#ifdef BLOCKING
    while (_write_bufs.empty())
    	if (!_write_cond.timed_wait(lock, timeout))
    		return;
#else
    if (_write_bufs.empty())
      return;
#endif
    buf = _write_bufs.front();
    // FIXME
    buf_len = _write_bufs.size();
    _write_bufs.pop_front();
  }

  write(buf, _WRITE_BLOCK_SIZE);
}

bool FileWriter::write(boost::uint8_t* buf,
		       const int buf_len) {
  // Write buffer to disk.
  int bytes_left = buf_len;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(_pfd.fd, &buf[bytes_written], bytes_left);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Write error: " << strerror(errno));
    }
  }
  if (_sw)
    _sw->update(1, bytes_written, 0, buf_len);
  free_buffer(buf);
}

bool FileWriter::write_unbuffered(boost::uint8_t* buf,
				  const boost::uint32_t len) {
  boost::uint64_t buf_len = 0;
  {
    boost::mutex::scoped_lock lock(_write_bufs_mutex);
    buf_len = _write_bufs.size();
  }
  // Write buffer to disk.
  int bytes_left = len;
  int bytes_written = 0;
  while (bytes_left) {
    int nb = ::write(_pfd.fd, &buf[bytes_written], bytes_left);
    if (nb > 0) {
      bytes_left -= nb;
      bytes_written += nb;
    } else {
      LOG4CXX_ERROR(logger, "Write error: " << strerror(errno));
    }
  }
  if (_sw)
    _sw->update(1, bytes_written, 0, buf_len);
  return true;
}

