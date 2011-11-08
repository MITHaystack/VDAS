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

#ifndef _NET_READER_H_
#define _NET_READER_H_

// C includes.

// C++ includes
#include <map>
#include <sstream>
#include <list>

// Framework includes.
#include <boost/crc.hpp>      // for boost::crc_basic, boost::crc_optimal
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>

// Local includes
#include <mark6.h>
#include <threaded.h>
#include <vdif.h>

// External class declarations.
class FileWriter;
class StatsWriter;
struct PFR;

//! Manages high speed capture of data from network interface using PF_RING.
//! Data are read in from an assigned interface using the PF_RING API.
//! The data are read in to specially allocated and sized buffers, that have
//! been optimized for disk throughput. Once a buffer is full, it is passed 
//! on to a FileWriter to be written to disk.
class NetReader: public Threaded {

 public:
  //---------------------------------------------------------------------------
  // Public API
  //---------------------------------------------------------------------------

  //! Constructor.
  //! \param id A unique identifies for this object. Used in logging.
  //! \param interface The name of the network interface from which data will
  //!        be captured.
  //! \param snaplen The total amount of data to be captured (data packet
  //!        headers from layer2 and up are included in the count).
  //! \param payload_length The total length of the UDP payload. This is the
  //!        amount of data that needs to be extracted from each packet and
  //!        then stored to disk.
  //! \param buffer_size The size of the optimal buffers that are used to 
  //!        transfer data from network interface to disk. Typically, these
  //!        are a multiple of the system page size, and aligned to page size
  //!        boundaries (e..g, page size of 256B, buffer size of 1048576B).
  //! \param promiscuous Wheter or not to put the network interface into 
  //!        promiscuous mode.
  //! \param fw A pointer to a FileWriter object.
  //! \param sw A pointer to a StatsWriter object.
  //! \param command_interval The main executin thread in run() will attempt
  //!        to check for new commands every command_interval seconds. The
  //!        actual interval between checks may be larger than this if the 
  //!        execution thread spends longer than command_interval processing
  //!        individual tasks.
  //!        \todo Robustify this.
  NetReader(const int id,
	    const std::string interface,
	    const int snaplen,
	    const int payload_length,
	    const int buffer_size,
	    const bool promiscuous,
	    FileWriter* const fw,
	    StatsWriter* const sw,
	    const double command_interval);

  //! Destructor.
  virtual ~NetReader();

  //! Start the main processing loop run() in its own thread of execution.
  virtual void start();

  //! Wait for the main processing loop/thread to exit.
  virtual void join();

  //! External API command. Insert a STOP command into the object's command
  //! queue for processing.
  virtual void cmd_stop();

  //! External API command. Insert a READ_FROM_NETWORK command into the
  //! object's command queue for processing.
  virtual void cmd_read_from_network();


 protected:
  //---------------------------------------------------------------------------
  // Internal data members
  //---------------------------------------------------------------------------

  //! The name of the network interface from which to capture data (e.g.,
  //! eth2).
  const std::string _interface;

  //! The total amount of bytes to capture from the packet (e.g. 9000).
  const int _snaplen;

  //! The length of the payload (i.e. the amount of data to extract and 
  //! write to disk).
  const int _payload_length;

  //! The size of the buffer being filled. These buffers are optimized
  //! for disk throughput and have custom memory allocation and alignment
  //! mechanisms. Typically, these buffers are orders of magnitude larger
  //! than a packet (e.g. 1 MB v. 9KB).
  const int _buffer_size;

  //! Whether or not to open the network interface in promiscuous mode.
  const bool _promiscuous;

  //! A pointer to the FileWriter object that will write the data to disk.
  //! Once a buffer is filled, it is passed in to the FileWriter object
  //! where it is queued awaiting transfer to disk.
  FileWriter* const _fw;

  //! A pointer to a StatsWriter object that will keep track of throughput
  //! statistics for this object. The StatsWriter will also write these 
  //! statistics to CSV file on disk.
  StatsWriter* const _sw;

  //! A pointer to the PF_RING data structure that tracks the PF_RING state
  //! and data structures.
  PFR* _ring;

  //! A pointer to the temporary buffer used to store data recently captured
  //! from the network interface.
  boost::uint8_t* _net_buf;
  
  //! State variable. This variable is changed in response to cmd_X() commands
  //! applied to this object. The variable is used in the run() method to
  //! determine what state the object is currently in, so that the appropriate
  //! handle_X() method can be called.
  volatile enum { IDLE, READ_FROM_NETWORK, STOP } _state;

  //! List of frame drop records;
  std::list<std::string> _frame_drop_log;

  //! Total number of frame drops;
  unsigned long long _frame_drops;

  //! Total number of frames received.
  unsigned long long _frames_received;

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

  //! Command handler. Handles the READ_FROM_NETWORK state/command. On
  //! receiving this command, the NetReader object will start to read data
  //! from the network interface and pass filled buffers to the FileWriter
  //! object.
  virtual void handle_read_from_network();
};

#endif // _NET_READER_H_

