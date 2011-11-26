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
#include <sys/wait.h>
#include <sys/types.h>
#include <sched.h>
#include <unistd.h>

// C++ includes.
#include <iostream>
#include <string>

// Framework includes.

// Local includes.
#include <net2raid.h>
#include <command.h>
#include <command_factory.h>

// Namespaces.
using namespace std; // Clean up long lines.

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const string DEFAULT_INTERFACES("eth0");
const int DEFAULT_SNAPLEN(8224);
const bool DEFAULT_PROMISCUOUS(true);
const string DEFAULT_LOG_NAME("net2raid");
// const int DEFAULT_PAYLOAD_LENGTH(8224);
const int DEFAULT_PAYLOAD_LENGTH(8268);
const int DEFAULT_SMP_AFFINITY(0);
const int DEFAULT_WRITE_BLOCKS(128);
const string LOG_PREFIX("/opt/mit/mark6/log/");

// Other constants.
const int MAX_SNAPLEN(9014);
const int PAYLOAD_LENGTH(DEFAULT_PAYLOAD_LENGTH);
const int DISK_RAMP_UP_TIME(2);

const int FILE_WRITER_ID(0);
const int FILE_WRITER_STATS_ID(1);
const int NET_READER_ID(2);
const int NET_READER_STATS_ID(3);

// Global variables. These represent the state of the recording session
// and are accessed by the Command* classes.
FileWriter* FILE_WRITER(0);
NetReader* NET_READER(0);
StatsWriter* FILE_WRITER_STATS(0);
StatsWriter* NET_READER_STATS(0);

//----------------------------------------------------------------------
// Program entry point.
//----------------------------------------------------------------------
int main(int argc, char* argv[]) {
    const int log_level(LOG_INFO);

    // Configure log subsystem.
    init_logger(DEFAULT_LOG_NAME, log_level);

    // Start processing.
    try {
        INFO("Starting.");
        CommandFactory f;
        while (true) {
            Command cmd = f.get_next_command(cin);
            cmd.execute();
        }
    } catch (std::exception& e) {
        cerr << e.what() << endl;
    }
    return 0;
}
