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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// C++ includes
#include <fstream>

// Framework includes.
#include <boost/crc.hpp>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

// Local includes.
#include <vdif.h>
#include <scan_check.h>
#include <mark6.h>
#include <logger.h>

namespace fs = boost::filesystem;

typedef std::map<int, int> map_t;

ScanCheck::ScanCheck(const std::string scan_file,
		     const int frame_size) {
  LOG4CXX_INFO(logger, "Starting check on : " << scan_file);

  std::ifstream input_file(scan_file.c_str());
  
  unsigned char* buf = new unsigned char[frame_size];

  ProtocolMap pmap;
  ProtocolMap::iterator curr;
  unsigned long long frames_dropped(0);
  unsigned long long frames_received(0);
  unsigned long long valid_frames(0);

  while (!input_file.eof()) {
    input_file.read((char*)buf, frame_size);
    
    const unsigned int eps = VDH::epoch_seconds(buf);
    const unsigned int daf = VDH::data_frame(buf);
    unsigned int len = VDH::length(buf);
    unsigned int sid = VDH::station_id(buf);
    const unsigned int tid = VDH::thread_id(buf);
    const unsigned int str = VDH::stream_id(buf);

    curr = pmap.find(str);
    if (curr == pmap.end()) {
      LOG4CXX_INFO(logger, "New stream identified: stream_id=" << str);
      VdifStreamState s(str, tid);
      pmap[str] = s;
    }

    VdifStreamState& vss(pmap[str]);
    if (!vss.valid_frame(daf) && frames_received>0) {
      ++frames_dropped;
      std::cout
	<< "dropped frame: " << pmap[str].str() << std::endl
	<< "eps=" << eps << " " 
	<< "daf=" << daf << " "
	<< "len=" << len << " "
	<< "sid=" << sid << " "
	<< "tid=" << tid << " "
	<< "str=" << str
	<< std::endl;

    } else {
      ++valid_frames;
    }

    ++frames_received;

    vss.update(eps, daf);

#if DUMP
    std::cout
      << "eps=" << eps << " " 
      << "daf=" << daf << " "
      << "len=" << len << " "
      << "sid=" << sid << " "
      << "tid=" << tid << " "
      << "str=" << str
      << std::endl;
#endif
  }
  std::cout
    << "frames_received=" << frames_received << " "
    << "valid_frames=" << valid_frames << " "
    << "sequence_breaks=" << frames_dropped
    << "duration=" << frames_received*1.0/MAX_DATA_FRAME << " s"
    << std::endl;

  delete [] buf;
}
