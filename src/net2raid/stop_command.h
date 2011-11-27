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

#ifndef _STOP_COMMAND_H_
#define _STOP_COMMAND_H_

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


class StopCommand: public Command {
    // Variables to store options.
    public:
        StopCommand(const vector<string>& params) {}
        void execute() {
            INFO("StartCommand executing.");

            // State is stored globally.
            NET_READER_STATS->cmd_stop();
            NET_READER_STATS->join();

            NET_READER->cmd_stop();
            NET_READER->join();

            FILE_WRITER_STATS->cmd_stop();
            FILE_WRITER_STATS->join();

            FILE_WRITER->cmd_stop();
            FILE_WRITER->join();
            INFO("Stopped capture process.");
        }
        string to_string() { return string("stop"); }
        void dump() {
            INFO("StopCommand {}");
        }
};

#endif // _STOP_COMMAND_H_
