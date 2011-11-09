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

#ifndef _M6LOGGER_H_
#define _M6LOGGER_H_

//! This module manages the mark6 logging faciliity.

// C includes
#include <syslog.h>

// C++ includes
#include <string>
#include <sstream>

// Framework includes.
#define DEBUG(message) {\
    std::ostringstream oss; \
    oss << message;\
    syslog(LOG_DEBUG, oss.str().c_str()); }

#define INFO(message) {\
    std::ostringstream oss; \
    oss << message;\
    syslog(LOG_INFO, oss.str().c_str()); }

#define NOTICE(message) {\
    std::ostringstream oss; \
    oss << message;\
    syslog(LOG_NOTICE, oss.str().c_str()); }

#define WARNING(message) {\
    std::ostringstream oss; \
    oss << message;\
    syslog(LOG_WARNING, oss.str().c_str()); }

#define ERR(message) {\
    std::ostringstream oss; \
    oss << message;\
    syslog(LOG_ERR, oss.str().c_str()); }

#define CRIT(message) {\
    std::ostringstream oss; \
    oss << message;\
    syslog(LOG_CRIT, oss.str().c_str()); }

//! Initialize logger.
extern void init_logger(const std::string log_config);

//! Close logger.
extern void close_logger();

#endif // _M6LOGGER_H_
