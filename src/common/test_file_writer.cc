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
#include <file_writer.h>
#include <test_file_writer.h>

using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestFileWriter);

void
TestFileWriter :: setUp (void)
{
  // set up test environment (initializing objects)
}

void
TestFileWriter :: tearDown (void)
{
}

void
TestFileWriter::basic(void)
{
  std::cout << "TestFileWriter::basic()" << std::endl;
  const int id = 0;
  const std::string capture_file("/tmp/test_file_writer-basic-1.dat");
  const std::string file_name("/tmp/test1.m6");
  const boost::uint32_t write_block_size(1048576);
  const boost::uint32_t write_blocks(512);
  const boost::uint32_t poll_timeout = 1000; // ms
  const double command_interval = 1; //s
  const unsigned long file_size(0);
  const bool preallocated(false);
  const bool directio(true);
  const bool translate(false);

  const std::string stats_file("/tmp/test_file_write-stats.dat");
  const int stats_interval(1);

  StatsWriter sw(id, stats_file, stats_interval, command_interval);
  FileWriter fw(id, write_block_size, write_blocks, capture_file,
		poll_timeout, &sw, command_interval, file_size,
		preallocated, directio, translate);

  sw.start();
  sw.cmd_write_to_disk();

  fw.open();
  fw.start();

  LOG4CXX_DEBUG(logger, "Started file writer.");
    
  const int NUM_BLOCKS = 100;
  boost::uint8_t* buf = fw.malloc_buffer();
  for (boost::uint32_t i=0; i<write_block_size; ++i) 
    buf[i] = static_cast<uint8_t>(i);

  for (int i=0; i<NUM_BLOCKS; ++i) {
    fw.write(buf);
  }

  fw.cmd_write_to_disk();

  sleep(10);

  fw.cmd_stop();
  fw.join();

  fw.close();

  sw.cmd_stop();
  sw.join();

  LOG4CXX_DEBUG(logger, "Joined file writer.");
}
