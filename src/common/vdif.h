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

#ifndef _VDIF_H_
#define _VDIF_H_

// C includes.

// C++ includes
#include <map>
#include <sstream>

// Framework includes.
#include <cstddef>    // for std::size_t
#include <boost/thread/thread.hpp>

// Local includes
#include <mark6.h>

// Protocol tracking data structures.
// #define MAX_DATA_FRAME 16777215
#define MAX_DATA_FRAME 31250

struct VdifStreamState {
  unsigned int _station_id;
  unsigned int _thread_id;
  unsigned int _stream_id;

  unsigned int _epoch_second;
  unsigned int _data_frame;
  unsigned int _sequence_breaks;
  unsigned long long _frames_received;
  unsigned long long _frames_lost;
  unsigned int _last_data_frame;
  

  VdifStreamState& operator=(VdifStreamState& other) {
    _station_id = other._station_id;
    _thread_id = other._thread_id;
    _stream_id = other._stream_id;
    _epoch_second = other._epoch_second;
    _data_frame = other._data_frame;
    _sequence_breaks = other._sequence_breaks;
    _frames_received = other._frames_received;
    _frames_lost = other._frames_lost;
    _last_data_frame = other._last_data_frame;
    return *this;
  }

VdifStreamState():
  _station_id(0), _thread_id(0), _stream_id(0), _epoch_second(0),
    _data_frame(0), _sequence_breaks(0), _frames_received(0), _frames_lost(0),
    _last_data_frame(0)
  {}

VdifStreamState(const VdifStreamState& other):
  _station_id(other._station_id), _thread_id(other._thread_id),
    _stream_id(other._stream_id), _epoch_second(other._epoch_second),
    _data_frame(other._data_frame), _sequence_breaks(other._sequence_breaks),
    _frames_received(other._frames_received),
    _frames_lost(other._frames_lost),
    _last_data_frame(other._last_data_frame)
  {}

VdifStreamState(const unsigned int sid, const unsigned int tid):
  _station_id(sid), _thread_id(tid), _stream_id(tid<<16 | sid),
    _epoch_second(0), _data_frame(0), _sequence_breaks(0),
    _frames_received(0), _frames_lost(0),
    _last_data_frame(0)
  {}

  void update(const unsigned int eps, const unsigned int df) {
    _last_data_frame = df;
    if (valid_frame(df)) {
      ++_frames_received;
    } else {
      ++_sequence_breaks;
      ++_frames_lost;
    }
    _data_frame = (df + 1) % MAX_DATA_FRAME;
    _epoch_second = eps;
  }

  bool valid_frame(const unsigned int df) {
    return _data_frame==df?true:false;
  }

  std::string str() {
    std::ostringstream oss (std::ostringstream::out);
    oss
      << "id=" << _station_id << ":" << _thread_id << ","
      << "eps=" << _epoch_second << ","
      << "daf=" << _data_frame << ","
      << "ldf=" << _last_data_frame << ","
      << "sqb=" << _sequence_breaks << ","
      << "frr=" << _frames_received << ","
      << "frl=" << _frames_lost;
    return oss.str();
  }
};

typedef std::map<unsigned int, VdifStreamState> ProtocolMap;

// Partial vdif data structure.
class VDH {
public:
  static unsigned int epoch_seconds(unsigned char* h) {
    return ((h[3]&0x7)<<24) | (h[2]<<16) | (h[1]<<8) | h[0];
  }

  static unsigned int ref_epoch(unsigned char* h) {
    return h[7] & 0x3f;
  }

  static unsigned int data_frame(unsigned char* h) {
    return (h[6]<<16) | (h[5]<<8) | h[4];
  }

  static unsigned int length(unsigned char* h) {
    return (h[10]<<16) | (h[9]<<8) | h[8];
  }

  static unsigned int station_id(unsigned char* h) {
    return (h[13]<<8) | h[12];
  }

  static unsigned int thread_id(unsigned char* h) {
    return ((h[15]&0x3)<<8) | h[14];
  }

  static unsigned int stream_id(unsigned char* h) {
    return ((h[14]&0x3)<<16) | (h[13]<<8) | h[12];
  }
};


#endif // _VDIF_H_

