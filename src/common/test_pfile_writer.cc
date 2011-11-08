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

// C includes.
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <sstream>
#include <list>
#include <algorithm>

// Boost includes.
#include <boost/foreach.hpp>

//Local includes.
#include <mark6.h>
#include <logger.h>
#include <stats_writer.h>
#include <pfile_writer.h>
#include <test_pfile_writer.h>

using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestPFileWriter);

void
TestPFileWriter :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestPFileWriter :: tearDown (void)
{
}

void
TestPFileWriter::basic(void)
{
  std::cout << "TestPFileWriter::basic()" << std::endl;
  const int id = 0;
  std::list<std::string> capture_files;
  const int N = 8;
  for (int i=0; i<N; i++) {
    std::ostringstream oss;
    oss << "/mnt/disk" << i << "/test_pfile_writer-basic-" << i << ".dat";
    capture_files.push_back(oss.str());
  }

  // const boost::uint32_t write_block_size(1048576);
  const boost::uint32_t write_block_size(4096*256);
  // const boost::uint32_t write_blocks(512);
  const boost::uint32_t write_blocks(1024);
  const boost::uint32_t poll_timeout = 1000; // ms
  const double command_interval = 1; //s
  const int NUM_BLOCKS = 10000;
  const unsigned long file_size(NUM_BLOCKS);
  const bool preallocated(true);
  const bool directio(true);


  const std::string stats_file("/tmp/test_pfile_write-stats.dat");
  const int stats_interval(1);

  StatsWriter sw(id, stats_file, stats_interval, command_interval);
  PFileWriter pfw(id, write_block_size, write_blocks, capture_files,
				  poll_timeout, &sw, command_interval, file_size, preallocated,
				  directio);

  sw.start();
  sw.cmd_write_to_disk();

  pfw.open();
  pfw.start();

  LOG4CXX_DEBUG(logger, "Started pfile writer.");
    
  pfw.cmd_write_to_disk();

  const int header_size(10);
  for (int i=0; i<NUM_BLOCKS; ++i) {
	boost::uint8_t* buf = pfw.malloc_buffer();
	for (boost::uint32_t i=0; i<header_size; ++i) 
		buf[i] = static_cast<uint8_t>(i);
	pfw.write(buf);
  }

  // pfw.test_write(buf, write_block_size, NUM_BLOCKS);

  pfw.cmd_stop();
  pfw.join();

  pfw.close();

  sw.cmd_stop();
  sw.join();

  LOG4CXX_DEBUG(logger, "Joined file writer.");
}
