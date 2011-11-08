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

#ifndef _MARK6_H_
#define _MARK6_H_

//! A collection of common header files, definitions, and name spaces
//! used across the mark6 software suite.

// Common definitions.
#include <sys/time.h>

// Framework includes.
#include <boost/cstdint.hpp>  // for boost::uint16_t
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/foreach.hpp>
#include <boost/circular_buffer.hpp>

using namespace boost;
using namespace boost::posix_time;
using namespace boost::gregorian;

//! Convert CDR data/time to epoch.
extern time_t date_time_to_epoch(const std::string& start_date,
				 const std::string& start_time);

// Convenience class for keeping IP and port information together.
//! \todo Is this really required?
struct IPEndpoint {
  std::string _ip_address;
  int _port;

  friend std::ostream& operator<<(std::ostream& out, const IPEndpoint& ep) {
    out
      << "IPEndpoint {\n"
      << "_ip_address:" << ep._ip_address << std::endl
      << "_port:" << ep._port << std::endl
      << "}";
    return out;
  }

  bool operator() (IPEndpoint ep1, IPEndpoint ep2) {
    if (ep1._ip_address == ep2._ip_address)
      return ep1._port < ep2._port;
    return ep1._ip_address < ep2._ip_address;
  }
};

//! Very useful class for timing intervals.
class Timer {
 private:
  //! Start time.
  struct timeval _start;

  //! Stop time.
  struct timeval _stop;

  //! Duration of time that has elapsed since _start was set.
  struct timeval _duration;
 public:

  //! Constructor. Start is set so interval measurement begins as soon as the
  //! Timer object is constructed.
  Timer() {
    timerclear(&_start);
    timerclear(&_stop);
    timerclear(&_duration);
    gettimeofday(&_start, NULL);
  }

  //! Restart the timer by resetting start time.
  void restart() {
    gettimeofday(&_start, NULL);
  }

  //! Calculate and return the total amount of time that has elapsed since
  //! _start was last initialized.
  //! \return Amount of time (in seconds) since _start was initialized.
  double elapsed() {
    gettimeofday(&_stop, NULL);
    timersub(&_stop, &_start, &_duration);
    return static_cast<double>(_duration.tv_sec)
      + static_cast<double>(_duration.tv_usec)/1000000.0;
  }
};

//! Control messages. \todo Remove these and keep them in specific class
//! header files.
enum  MessageType { MSG_WRITE_TO_DISK, MSG_READ_FROM_DISK, MSG_STOP };

//! Obsolete. \todo Remove this.
struct ControlMessage {
  MessageType _type;
};

//! Useful macro for drawing horizontal lines in print statements.
#define HLINE "+------------------------------------------------------------------------------+"

#endif // _MARK6_H_
