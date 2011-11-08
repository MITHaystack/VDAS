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
#include <assert.h>
#include <netinet/udp.h>

// C++ includes.
#include <iostream>

// Framework includes.

// Local includes.
#include <mark6.h>
#include <logger.h>
#include <pfr.h>
#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>

using namespace std;

const int PCAP_HEADER_LENGTH = 24;

struct PcapPacketHeader {         
  boost::uint32_t ts_sec;         // timestamp seconds
  boost::uint32_t ts_usec;        // timestamp microseconds
  boost::uint32_t incl_len;       // number of octets of packet saved in file
  boost::uint32_t orig_len;       // actual length of packet
};

const int PCAP_PACKET_HEADER_LENGTH = 16;

NetReader::NetReader(const int id,
		     const std::string interface,
		     const int snaplen,
		     const int payload_length,
		     const int buffer_size,
		     const bool promiscuous,
		     FileWriter* const fw,
		     StatsWriter* const sw,
		     const double command_interval):
  Threaded(id, command_interval),
  _interface(interface),
  _snaplen(snaplen),
  _payload_length(payload_length),
  _buffer_size(buffer_size),
  _promiscuous(promiscuous),
  _fw(fw),
  _sw(sw),
  _ring(0),
  _net_buf(0),
  _state(IDLE),
  _frame_drop_log(),
  _frame_drops(0),
  _frames_received(0) {
  _ring = new PFR(interface.c_str(), snaplen, _promiscuous);
  _net_buf = new boost::uint8_t[snaplen];
}

NetReader::~NetReader() {
  delete _ring;
}

void NetReader::start() {
  _running = true;
  _thread = boost::thread(&NetReader::run, this);
}

void NetReader::join() {
  _thread.join();
}

void NetReader::run() {
  LOG4CXX_INFO(logger, "NetReader Running...");

  Timer run_timer;
  Timer command_timer;
  
  try {
    // Main processing loop.
    while (_running) {

      // State machine.
      switch (_state) {

      case READ_FROM_NETWORK:
	if (command_timer.elapsed() > _command_interval) {
	  command_timer.restart();
	  continue;
	}
	handle_read_from_network();
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

void NetReader::cmd_stop() {
  LOG4CXX_INFO(logger, "Received STOP");
  _state = STOP;
}

void NetReader::cmd_read_from_network() {
  LOG4CXX_INFO(logger, "Received READ_FROM_NETWORK...");
  _state = READ_FROM_NETWORK;
}

void NetReader::handle_stop() {
  _running = false;
  BOOST_FOREACH(std::string& s, _frame_drop_log) {
    LOG4CXX_INFO(logger, "fdl: intf=" << _interface << "," << s);
  }
}

void NetReader::handle_idle() {
  usleep(_command_interval*1000000);
}

boost::uint8_t PCAP_HEADER[] = {
	0xd4, 0xc3, 0xb2, 0xa1, 0x2,  0x0,  0x4,  0x0,
	0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,	
	0xff, 0xff, 0x0,  0x0,  0x1,  0x0,  0x0,  0x0
};

void NetReader::handle_read_from_network() {
  struct pfring_pkthdr hdr;
  int payload_length;
  boost::uint8_t* payload_ptr;
  
  int bytes_read = 0;
  static int remainder_len = 0;
  static boost::uint8_t remainder_buf[9000];
  static bool first_write = true;
  static int start_second = 0;
  static boost::uint8_t net_buf[9000];
  static PcapPacketHeader pph;
  static ProtocolMap pmap;
  static ProtocolMap::iterator curr;
  
#define DYNAMIC_BUFFER
#ifdef DYNAMIC_BUFFER
  int bytes_left = _buffer_size;
  boost::uint8_t* file_buf = _fw->malloc_buffer();
  if (!file_buf) {
    LOG4CXX_ERROR(logger, "File buffer's full: timedout");
    return;
  }
  assert(file_buf);
#else
  const int BUFFER_SIZE = 1048576;
  int bytes_left = BUFFER_SIZE;
  static boost::uint8_t file_buf[BUFFER_SIZE];
#endif
  boost::uint64_t dropped_packets = 0;
  boost::uint64_t num_packets = 0;
  boost::uint64_t num_bytes = 0;

  if (first_write) {
    memcpy(remainder_buf, PCAP_HEADER, PCAP_HEADER_LENGTH);
    remainder_len = PCAP_HEADER_LENGTH;
    first_write = false;
  }

  // Copy partial packet.
  if (remainder_len > 0) {
    memcpy(file_buf, remainder_buf, remainder_len);
    bytes_read += remainder_len;
    bytes_left -= remainder_len;
    remainder_len = 0;
  }

  // Fill the new file_buf;
  int dport = 0;
  while (bytes_left > 0) {
    if (_ring->get_next_packet(&hdr, &net_buf[PCAP_PACKET_HEADER_LENGTH],
			       _snaplen) > 0) {
      // Successful read.
      if (start_second == 0) {
	start_second = hdr.ts.tv_sec;
	continue;
      }

      if (hdr.ts.tv_sec < start_second + 2)
	continue;

      // Extract offsets etc. from pfring structures.
      struct pfring_extended_pkthdr& pep(hdr.extended_hdr);
      struct pkt_parsing_info& ppi(pep.parsed_pkt);
      struct pkt_offset po(ppi.offset);
      const boost::uint16_t eth_offset(po.eth_offset);
      const boost::uint16_t l3_offset(po.l3_offset);
      const boost::uint16_t l4_offset(po.l4_offset);
      const boost::uint16_t payload_offset(po.payload_offset);

      struct udphdr* udp_hdr = (struct udphdr*)net_buf + l4_offset;
      dport = ntohs(udp_hdr->dest);

      // Get payload information.
      payload_ptr = net_buf;
      payload_length = hdr.len + PCAP_PACKET_HEADER_LENGTH;

      const unsigned int eps = VDH::epoch_seconds(net_buf + payload_offset + 16);
      const unsigned int daf = VDH::data_frame(net_buf + payload_offset + 16);
      // unsigned int len = VDH::length(net_buf + payload_offset + 16);
      // unsigned int sid = VDH::station_id(net_buf + payload_offset + 16);
      const unsigned int tid = VDH::thread_id(net_buf + payload_offset + 16);
      const unsigned int str = VDH::stream_id(net_buf + payload_offset + 16);

      curr = pmap.find(str);
      if (curr == pmap.end()) {
	LOG4CXX_INFO(logger, "New stream identified: stream_id=" << str);
	VdifStreamState s(str, tid);
	pmap[str] = s;
      }

      VdifStreamState& vss(pmap[str]);
      if (!vss.valid_frame(daf)) {
	++_frame_drops;
	_frame_drop_log.push_back(pmap[str].str());
      }
      ++_frames_received;

      vss.update(eps, daf);

#ifdef DUMP
      cout << "Packet dump.\n";
      cout << "caplen:         " << hdr.caplen << endl;
      cout << "len:            " << hdr.len << endl;
      cout << "parsed_hdr_len: " << pep.parsed_header_len << endl;
      cout << "eth_offset:     " << eth_offset << endl;
      cout << "l3_offset:      " << l3_offset << endl;
      cout << "l4_offset:      " << l4_offset << endl;
      cout << "payload_offset: " << payload_offset << endl;
      cout << "payload_length: " << payload_length << endl;

      const int dumplen(128);
      for (int i=0; i<dumplen; i++) {
	printf("%02x ", (unsigned char)net_buf[i + payload_offset]);
	if ((i+1)%4 == 0)
	  printf("\n");
      }
      printf("\n");
#endif // DUMP

      pph.ts_sec = hdr.ts.tv_sec;
      pph.ts_usec = hdr.ts.tv_usec;
      pph.incl_len = hdr.len;
      pph.orig_len = hdr.len;
      memcpy(net_buf, &pph, PCAP_PACKET_HEADER_LENGTH);

      // Check for validity of capture.
      if (hdr.caplen != _snaplen) {
	  // || hdr.caplen != hdr.len
	  // || payload_length != _payload_length) {
	LOG4CXX_DEBUG(logger, "Short capture(caplen/snaplen"
		      "len/payload_length/PAYLOAD_LENGTH) -> ("
		      << hdr.caplen << "/" << _snaplen << "/"
		      << hdr.len << "/" << payload_length << "/"
		      << _payload_length << ")");
	continue;
      }

      // Update stats.
      num_packets++;
      num_bytes += hdr.len;
    } else {
      LOG4CXX_ERROR(logger, "Error while calling get_next_packet(): "
		    << strerror(errno));
      continue;
    }

    // Accumulate or flush data.
    if (bytes_left < payload_length) {
      // Copy fragment into buffer.
      memcpy(&file_buf[bytes_read], payload_ptr, bytes_left);
      bytes_read += bytes_left;
      
      // Copy remainder into remainder_buf.
      remainder_len = payload_length - bytes_left;
      memcpy(remainder_buf, payload_ptr + bytes_left, remainder_len);

      if (!_fw->write(file_buf)) {
	LOG4CXX_ERROR(logger, "Packet drop on port: " << dport
		      << " Total dropped: " << dropped_packets);
	++dropped_packets;
      }

      // Update stats.
      _sw->update(num_packets, num_bytes, dropped_packets, 0);
      bytes_left = 0;
      break;
    } else if (bytes_left == payload_length) {
      // Copy captured payload to file buffer.
      memcpy(&file_buf[bytes_read], payload_ptr, payload_length);
      bytes_read += payload_length;
      bytes_left -= payload_length;

      if (!_fw->write(file_buf)) {
	LOG4CXX_ERROR(logger, "Packet drop on port: " << dport
		      << " Total dropped: " << dropped_packets);
	++dropped_packets;
      }
    } else {
      // Copy captured payload to file buffer.
      memcpy(&file_buf[bytes_read], payload_ptr, payload_length);
      bytes_read += payload_length;
      bytes_left -= payload_length;
    }
  }
}

