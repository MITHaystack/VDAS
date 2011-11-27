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

#ifndef _TEST_FILE_WRITER_H_
#define _TEST_FILE_WRITER_H_

#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>

// Globals required to represent state.
extern FileWriter *FILE_WRITER;
extern StatsWriter *FILE_WRITER_STATS;
extern NetReader *NET_READER;
extern StatsWriter *NET_READER_STATS;

extern const int PAYLOAD_LENGTH;
extern const std::string LOG_PREFIX;
extern const int FILE_WRITER_ID;
extern const int FILE_WRITER_STATS_ID;
extern const int NET_READER_ID;
extern const int NET_READER_STATS_ID;

class TestCommand : public CPPUNIT_NS :: TestFixture
{
    CPPUNIT_TEST_SUITE(TestCommand);
    CPPUNIT_TEST(null_command);
    CPPUNIT_TEST(setup_command);
    CPPUNIT_TEST(start_command);
    CPPUNIT_TEST(stop_command);
    CPPUNIT_TEST(teardown_command);
    CPPUNIT_TEST_SUITE_END();

    public:
        void setUp(void);
        void tearDown(void);

    protected:
        void null_command(void);
        void setup_command(void);
        void start_command(void);
        void stop_command(void);
        void teardown_command(void);
};


#endif /*TEST_FILE_WRITER_H_*/
