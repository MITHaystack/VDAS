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

#ifndef _COMMAND_FACTORY_
#define _COMMAND_FACTORY_

// C includes.
// C++ includes.
#include <string>
#include <iostream>
#include <sstream>
#include <list>

// Framework includes.
// Local includes.
#include <command.h>
#include <setup_command.h>
#include <start_command.h>
#include <stop_command.h>
#include <teardown_command.h>

using namespace std;

// Retrieves the next command form the specified istream.
class CommandFactory {
    public:
        CommandFactory() {}

        Command get_next_command(istream& in) {
            string command;
            getline(in, command);
            if (!command.length())
                return NullCommand();

            istringstream iss(command);
            std::list<string> strings;
            while (iss) {
                string s;
                iss >> s;
                strings.push_back(s);
            }

            if (strings.empty())
                return NullCommand();

            string command_type(strings.front());

            // Command "interpreter".
            try {
                if (command_type.compare("setup")) {
                    return SetupCommand(strings);
                } else if (command_type.compare("start")) {
                    return StartCommand(strings);
                } else if (command_type.compare("stop")) {
                    return StopCommand(strings);
                } else if (command_type.compare("teardown")) {
                    return TeardownCommand(strings);
                } else {
                    return NullCommand();
                }
            } catch (std::exception& e) {
                ERR("CommandFactory: " << e.what());
            } 
            return NullCommand();
        }
};

#endif // _COMMAND_FACTORY_
