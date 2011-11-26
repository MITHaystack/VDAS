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

#ifndef _START_COMMAND_H_
#define _START_COMMAND_H_

// C includes.
// C++ includes.
#include <string>
#include <iostream>
#include <list>

// Framework includes.
// Local includes.
#include <net2raid.h>
#include <command.h>

using namespace std;

class StartCommand: public Command {
    // Variables to store options.
    public:
        StartCommand(const list<string>& params) {}
        void execute() {
            INFO("StartCommand executing.");

            // State is stored globally.
            FILE_WRITER_STATS->start();
            FILE_WRITER_STATS->cmd_write_to_disk();

            FILE_WRITER->open();
            FILE_WRITER->start();
            FILE_WRITER->cmd_write_to_disk();

            NET_READER_STATS->start();
            NET_READER_STATS->cmd_write_to_disk();

            // sleep(DISK_RAMP_UP_TIME);

            NET_READER->start();
            NET_READER->cmd_read_from_network();
            INFO("Started capture process.");
        }
        string to_string() { return string("start"); }
        void dump() {
            INFO("StartCommand {}");
        }
};

#endif // _START_COMMAND_H_
