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
#include <stdlib.h>
#include <iostream>
#include <stdio.h>
#include <pcap.h>
#include <netinet/ip.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// C++ includes
#include <map>

// Framework includes.
#include <boost/crc.hpp>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

// Local includes.
#include <disk2vlbi.h>
#include <mark6.h>
#include <logger.h>

namespace fs = boost::filesystem;

typedef std::map<int, int> map_t;

Disk2vlbi::Disk2vlbi(const std::string capture_file,
		     const unsigned long long size) {
  LOG4CXX_INFO(logger, "Starting translation on : " << capture_file);

  // open the pcap file 
  pcap_t *handle; 
  char errbuf[PCAP_ERRBUF_SIZE];
  struct pcap_pkthdr header;
  const u_char* packet;

  handle = pcap_open_offline(capture_file.c_str(), errbuf);
  if (handle == NULL) {
    LOG4CXX_ERROR(logger, "Couldn't open pcap file " << capture_file
		  << " " << errbuf);
    exit(1);
  }

  fs::path p(capture_file.c_str());
  const std::string directory(p.parent_path().string());
  const std::string filename(p.stem());
  map_t fd_map;

  // Packet processing loop.
  unsigned long long mbytes_written = 0;
  unsigned long long bytes_written = 0;
  while (packet = pcap_next(handle, &header) ) {
    if ( (size>0) && (mbytes_written>size) )
      break;

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
      if (nb>0) {
	bytes_written += nb;

	if (bytes_written > 1048576) {
	  ++mbytes_written;
	  bytes_written = 0;
	}
      }
    }
  }
    
  pcap_close(handle);  //close the pcap file 

  BOOST_FOREACH(map_t::value_type& v, fd_map) {
    ::close(v.second);
  }
}
