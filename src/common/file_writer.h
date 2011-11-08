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

#ifndef _FILEWRITER_H_
#define _FILEWRITER_H_

// C includes.
#include <poll.h>

// C++ includes
#include <list>

// Framework includes.
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>


// Local includes
#include <mark6.h>
#include <threaded.h>

class StatsWriter;

struct Timeout {
  std::string _msg;
Timeout(): _msg("Buffer timeout.") {}
};

//! Manages the high speed writing of data to file.
//! Includes a circular buffer for storing buffers to be written, as well as
//! a state machine that controls the operation of the thread. The class
//! is an "active" object that runs in its own thread of execution. External
//! objects can interact with it via the start(), join(), cmd_XXX(), 
//! write(), open(), and close() methods.
class FileWriter: public Threaded {

 public:
  //---------------------------------------------------------------------------
  // Public API
  //---------------------------------------------------------------------------

  //! Constructor
  //! \param id A unique id for this object. Used for logging.
  //! \param write_block_size The size of the indidivual write blocks to be
  //!        written to disk. WRITE_BLOCK bytes of data will be written to
  //!        disk each time write_block() is called.
  //! \param write_blocks The total number of write blocks to buffer
  //!        internally.
  //! \param capture_file The file to capture data to.
  //! \param poll_timeout \todo Obsolete parameter.
  //! \param sw A pointer to a StatsWriter object. Performance data will be 
  //!        logged using this object.
  //! \param command_interval The main executin thread in run() will attempt
  //!        to check for new commands every command_interval seconds. The
  //!        actual interval between checks may be larger than this if the 
  //!        execution thread spends longer than command_interval processing
  //!        individual tasks.
  //! \param file_size Size of file in MB.
  //! \param preallocated Whether or not file has been preallocated using 
  //!        fallocate.
  //! \param directio Whether or not to use DIRECT_IO and bypass linux
  //!        page cache.
  //! \param translate Whether or not to translate the file once it has been recorded.
  FileWriter(const int id,
	     const int write_block_size,
	     const int write_blocks,
	     const std::string& capture_file,
	     const int poll_timeout,
	     StatsWriter * const sw,
	     const double command_interval,
	     const unsigned long file_size,
	     const bool preallocated,
	     const bool directio,
	     const bool translate);

  //! Destructor.
  virtual ~FileWriter();

  //! Start the main processing loop run() in its own thread of execution.
  virtual void start();

  //! Wait for the main processing loop/thread to exit.
  virtual void join();

  //! External API command. Insert a STOP command into the object's command
  //! queue for processing.
  virtual void cmd_stop();

  //! External API command. Insert a WRITE_TO_DISK command into the object's
  //! command queue for processing.
  void cmd_write_to_disk();

  //! Custom API. Open capture file on disk.
  virtual int open();

  //! Custom API. Close capture file on disk.
  virtual int close();

  //! External API. Write buffer to disk. The supplied buffer is stored in the
  //! internal buffer queue where it waits to be written to disk by the main
  //! processing loop.
  //! \param buf The buffer to be written to disk.
  //! \param buf Length of buffer to be written.
  bool write(boost::uint8_t* buf);

  bool write_unbuffered(boost::uint8_t* buf, const boost::uint32_t len);

  boost::uint8_t* malloc_buffer();
  bool free_buffer(boost::uint8_t* buf);

 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------

  //! The size of blocks to be written to disc.
  const int _WRITE_BLOCK_SIZE;

  //! The maximum number of write blocks that can be buffered at any one time.
  const int _WRITE_BLOCKS;

  //! The timeout for blocking operations.
  const int _POLL_TIMEOUT;

  //! Encapsulates the file descriptor of the file this object manages.
  struct pollfd _pfd;

  //! A circular buffer that contains all of the buffers waiting to be 
  //! written to disk.
  std::list<boost::uint8_t*> _write_bufs;
  std::list<boost::uint8_t*> _free_bufs;

  //! The state of the object.
  volatile enum { IDLE, WRITE_TO_DISK, STOP } _state;

  //! Mutex that protects the write buffer from multi-threaded access.
  boost::mutex _write_bufs_mutex;

  //! Mutex that protects free list from multi-threaded access.
  boost::mutex _free_bufs_mutex;

  
  //! Condition variable that signals blocked readers (i.e. pop() operations)
  //! when a new buffer is re-inserted back into the pool.
  boost::condition_variable _read_cond;
  
  //! Condition variable that signals blocked writers (i.e. push() operations)
  //! when a new buffer is popped from the pool.
  boost::condition_variable _write_cond;

  //! The name of the file data will be written to.
  const std::string _capture_file;

  //! Reference to statistics writer object for capturing throughput
  //! statistics to CSV file.
  StatsWriter* const _sw;

  //! Size of file to write in MB.
  unsigned long _file_size;

  //! Whether or not file has been pre-allocated using fallocate().
  bool _preallocated;

  //! Whether or not to use DIRECT_IO and bypass the linux page cache
  //! (Can provide performance improvements.)
  bool _directio;

  //! Whether or not to translate file from pcap format to native format
  //! after recording has finished.
  bool _translate;

  //---------------------------------------------------------------------------
  // Internal methods
  //---------------------------------------------------------------------------

  //! Main processing loop.
  virtual void run();

  //! Command handler. Handles the STOP command/state. On receiving this
  //! command, the FileWriter object will stop processing and exit the 
  //! run() method at the next opportunity.
  virtual void handle_stop();

  //! Command handler. Handles the IDLE state.
  virtual void handle_idle();

  //! Command handler. Handles the WRITE_TO_DISK command/state. On receiving
  //! this command, the FileWriter object will start reading data from its
  //! internal buffers and writing them to disk.
  virtual void handle_write_to_disk();

  //! Writes a block to disk using the specified file descriptor.
  //! \param fd The file descriptor to which the data will be sent.
  virtual void write_block();

  //! Writes  a block to disk.
  //! \param buf The buffer to write to disk.
  //! \param buf_len The length of the buffer.
  //! return true if successful, false if not.
  virtual bool write(boost::uint8_t* buf, const int buf_len);
};

#endif // _FILEWRITER_H_
