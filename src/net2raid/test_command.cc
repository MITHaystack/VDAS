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
#include <vector>
#include <algorithm>

// Boost includes.
#include <boost/foreach.hpp>

//Local includes.
#include <mark6.h>
#include <m6logger.h>
#include <command.h>
#include <setup_command.h>
#include <start_command.h>
#include <stop_command.h>
#include <teardown_command.h>
#include <test_command.h>

using namespace std;
using namespace boost;

CPPUNIT_TEST_SUITE_REGISTRATION (TestCommand);

FileWriter *FILE_WRITER;
StatsWriter *FILE_WRITER_STATS;
NetReader *NET_READER;
StatsWriter *NET_READER_STATS;

const int PAYLOAD_LENGTH(8268);
const string LOG_PREFIX("/tmp/");
const int FILE_WRITER_ID(0);
const int FILE_WRITER_STATS_ID(1);
const int NET_READER_ID(2);
const int NET_READER_STATS_ID(3);

void TestCommand::setUp (void)
{
  // set up test environment (initializing objects)
}

void TestCommand::tearDown (void)
{
}

void TestCommand::null_command(void)
{
    cout << "TestCommand::null_command()" << endl;
    NullCommand nc;
    nc.execute();
    cout << "to_string() " << nc.to_string() << endl;
    nc.dump();

    cout << endl;
}

void TestCommand::setup_command(void)
{
    cout << "TestCommand::setup_command()" << endl;
    vector<string> params;
    params.push_back("setup");
    // params.push_back("8224"); // snaplen (VDIF)
    params.push_back("8266"); // snaplen (MARK5B)
    params.push_back("1"); // promiscuous
    params.push_back("42"); // duration
    params.push_back("30000"); // file_size
    params.push_back("eth0"); // interface
    params.push_back("/tmp/setup_test.dat"); // capture_file
    params.push_back("0"); // smp_affinity
    params.push_back("128"); // ring_buffers
    params.push_back("1024"); // write_blocks
    params.push_back("512"); // pages_per_buffer

    SetupCommand sc(params);
    cout << "to_string() " << sc.to_string() << endl;
    sc.dump();

    sc.execute();

    cout << endl;
}


void TestCommand::start_command(void)
{
    cout << "TestCommand::start_command()" << endl;
    vector<string> params;
    params.push_back("start");

    StartCommand sc(params);
    cout << "to_string() " << sc.to_string() << endl;
    sc.dump();

    cout << endl;
}

void TestCommand::stop_command(void)
{
    cout << "TestCommand::stop_command()" << endl;
    vector<string> params;
    params.push_back("stop");

    StopCommand sc(params);
    cout << "to_string() " << sc.to_string() << endl;
    sc.dump();

    cout << endl;
}

void TestCommand::teardown_command(void)
{
    cout << "TestCommand::teardown_command()" << endl;
    INFO("TestCommand::teardown_command()");
    vector<string> params;
    params.push_back("teardown");

    StartCommand td(params);
    cout << "to_string() " << td.to_string() << endl;
    td.dump();

    td.execute();

    cout << endl;
}


