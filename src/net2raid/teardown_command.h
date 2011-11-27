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

#ifndef _TEARDOWN_COMMAND_H_
#define _TEARDOWN_COMMAND_H_

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


class TeardownCommand: public Command {
    // Variables to store options.
    public:
        TeardownCommand(const vector<string>& params) {}
        void execute() {
            INFO("TeardownCommand: executing.");

            // State is stored globally.
            delete NET_READER;
            delete NET_READER_STATS;
            delete FILE_WRITER;
            delete FILE_WRITER_STATS;

            NET_READER = (NetReader*)0;
            NET_READER_STATS = (StatsWriter*)0;
            FILE_WRITER = (FileWriter*)0;
            FILE_WRITER_STATS = (StatsWriter*)0;
        }
        string to_string() { return string("teardown"); }
        void dump() {
            INFO("TeardownCommand BEGIN DUMP");
            INFO("TeardownCommand END DUMP");
        }
};

#endif // _TEARDOWN_COMMAND_H_
