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

// Local includes.
#include <m6logger.h>

const std::string LOGGER_NAME("mark6");

// Global logger definition.
void init_logger(const std::string log_name, const int priority)
{
  openlog(log_name.c_str(), LOG_NDELAY | LOG_PID, LOG_USER);
  setlogmask(priority);
}

void close_logger() {
  closelog();
}

#ifdef TEST
int main(int argc, char** argv) {
  init_logger("mylog", LOG_MASK(LOG_INFO) | LOG_MASK(LOG_DEBUG));

  DEBUG(std::string("this is cool5."));
  INFO(std::string("this is cool6."));
  close_logger();
}
#endif
