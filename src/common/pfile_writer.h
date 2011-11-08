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

#ifndef _PFILEWRITER_H_
#define _PFILEWRITER_H_

// C includes.
#include <poll.h>

// C++ includes
#include <list>

// Framework includes.

// Local includes
#include <mark6.h>
#include <file_writer.h>

class StatsWriter;

//! Manages the high speed writing of data to file.
//! Includes a circular buffer for storing buffers to be written, as well as
//! a state machine that controls the operation of the thread. The class
//! is an "active" object that runs in its own thread of execution. External
//! objects can interact with it via the start(), join(), cmd_XXX(), 
//! write(), open(), and close() methods.
class PFileWriter: public FileWriter {

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
  //! \param capture_files A list of capture files to use for the stored data.
  //! \param poll_timeout (poll timeout in ms).
  //! \param sw A pointer to a StatsWriter object. Performance data will be 
  //!        logged using this object.
  //! \param command_interval The main executin thread in run() will attempt
  //!        to check for new commands every command_interval seconds. The
  //!        actual interval between checks may be larger than this if the 
  //!        execution thread spends longer than command_interval processing
  //!        individual tasks.
  //! \param file_size Size of file to write in MB.
  //! \param preallocated Whether or not file has been preallocated using 
  //!        fallocate.
  //! \param directio Whether or not to use DIRECT_IO and bypass linux
  //!        page cache.
  PFileWriter(const int id,
	      const int write_block_size,
	      const int write_blocks,
	      const std::list<std::string>& capture_files,
	      const int poll_timeout,
	      StatsWriter * const sw,
	      const double command_interval,
	      const unsigned long file_size,
	      const bool preallocated,
	      const bool directio);

  //! Destructor.
  virtual ~PFileWriter();

  using FileWriter::start;
  using FileWriter::join;
  using FileWriter::cmd_stop;
  using FileWriter::cmd_write_to_disk;

  //! Custom API. Open capture file on disk.
  virtual int open();

  //! Custom API. Close capture file on disk.
  virtual int close();

  using FileWriter::write;
  using FileWriter::write_unbuffered;

  using FileWriter::malloc_buffer;
  using FileWriter::free_buffer;

 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------
  std::list<std::string> _capture_files;
  struct pollfd* _pfds;
  int* _fds;
  nfds_t _nfds;
  int _pfds_idx;

  //---------------------------------------------------------------------------
  // Internal methods
  //---------------------------------------------------------------------------

  //! Writes  a block to disk.
  //! \param buf The buffer to write to disk.
  //! \param buf_len The length of the buffer.
  //! return true if successful, false if not.
  virtual bool write(boost::uint8_t* buf, const int buf_len);

 public:
  //! Test method to write n blocks to disk.
  //! \param buf The buffer to write to disk.
  //! \param buf_len The length of the buffer.
  //! \param n Number of times to write buffer.
  //! return true if successful, false if not.
  bool test_write(boost::uint8_t* buf, const int buf_len, const int n);
};

#endif // _PFILEWRITER_H_
