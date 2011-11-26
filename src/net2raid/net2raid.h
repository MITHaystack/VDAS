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

#ifndef _NET2RAID_H_
#define _NET2RAID_H_

// C includes.
// C++ includes.
// Framework includes.
// Local includes.
#include <mark6.h>
#include <m6logger.h>
#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>

// Namespaces.
using namespace std; // Clean up long lines.

//----------------------------------------------------------------------
// Declarations
//----------------------------------------------------------------------
void cli();

//----------------------------------------------------------------------
// Global variables.
//----------------------------------------------------------------------
  
// DEFAULTS
extern const string DEFAULT_INTERFACES;
extern const int DEFAULT_SNAPLEN;
extern const bool DEFAULT_PROMISCUOUS;
extern const int DEFAULT_TIME;
extern const string DEFAULT_LOG_NAME;
extern const int DEFAULT_PAYLOAD_LENGTH;
extern const int DEFAULT_SMP_AFFINITY;
extern const int DEFAULT_WRITE_BLOCKS;
extern const int DEFAULT_RATE;
extern const string LOG_PREFIX;

// Other constants.
extern const int MAX_SNAPLEN;
extern const int STATS_SLEEP;
extern const int PAYLOAD_LENGTH;
extern const int DISK_RAMP_UP_TIME;

extern const int LOCAL_PAGES_PER_BUFFER;

extern int LOCAL_PAGE_SIZE;
extern int BUFFER_SIZE;

extern FileWriter* FILE_WRITER;
extern NetReader* NET_READER;
extern StatsWriter* FILE_WRITER_STATS;
extern StatsWriter* NET_READER_STATS;
extern const string LOG_PREFIX;

extern const int FILE_WRITER_ID;
extern const int FILE_WRITER_STATS_ID;
extern const int NET_READER_ID;
extern const int NET_READER_STATS_ID;

#endif // _NET2RAID_H_

