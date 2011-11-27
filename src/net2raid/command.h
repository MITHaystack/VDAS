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

#ifndef _COMMAND_H_
#define _COMMAND_H_

// C includes.
// C++ includes.
#include <string>
#include <iostream>
#include <vector>

// Framework includes.
// Local includes.
#include <net2raid.h>

using namespace std;

class Command {
    public:
        Command() {}
        Command(const vector<string>& params) {}
        virtual void execute(){}
        virtual string to_string() {}
        virtual void dump() {}
};


class NullCommand: public Command {
    public:
        NullCommand(): Command() {}
        void execute() {
            INFO("NullCommand: executing.");
        }
        string to_string() { return string("null"); }
        void dump() {
            INFO("NullCommand: BEGIN DUMP");
            INFO("NullCommand: END DUMP");
        }
};

#endif // _COMMAND_H_
