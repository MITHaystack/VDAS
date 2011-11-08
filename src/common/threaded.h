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

#ifndef _THREADED_H_
#define _THREADED_H_

// C includes.

// C++ includes

// Framework includes.
#include <boost/thread/thread.hpp>

// Local includes
#include <mark6.h>

//! This is the Abstract Base Class for all classes that require a separate
//! thread for executing their main procesing loop.
class Threaded {

 public:
  //---------------------------------------------------------------------------
  // Public API
  //---------------------------------------------------------------------------

  //! Constructor.
  //! \param id A unique id for this object. Used for logging.
  //! \param command_interval The main executin thread in run() will attempt
  //!        to check for new commands every command_interval seconds. The
  //!        actual interval between checks may be larger than this if the 
  //!        execution thread spends longer than command_interval processing
  //!        individual tasks.
 Threaded(const int id,
	  const int command_interval):
  _id(id),
    _command_interval(command_interval),
    _running(false),
    _thread() {}

  //! Destructor.
  virtual ~Threaded() {}

  //! Start the main processing loop run() in its own thread of execution.
  virtual void start() = 0;

  //! Wait for the main processing loop/thread to exit.
  virtual void join() = 0;
  
  //! External API command. Insert a STOP command into the object's command
  //! queue for processing.
  virtual void cmd_stop() = 0;


 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------

  //! A unique identifier for this object.
  const int _id;

  //! The command interval.
  const int _command_interval;

  //! A flag used in the while() conditional at the top of the run() method.
  //! The main thread of execution only continuines to execute while _running
  //! is true.
  bool _running;

  //! The thread data structure.
  boost::thread _thread;


  //---------------------------------------------------------------------------
  // Internal methods
  //---------------------------------------------------------------------------

  //! Main processing loop.
  virtual void run() = 0;

  //! Command handler. Handles the STOP command/state. On receiving this
  //! command, the FileWriter object will stop processing and exit the 
  //! run() method at the next opportunity.
  virtual void handle_stop() = 0;

  //! Command handler. Handles the IDLE state.
  virtual void handle_idle() = 0;
};

#endif // _THREADED_H_
