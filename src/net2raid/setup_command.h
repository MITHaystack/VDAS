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

#ifndef _SETUP_COMMAND_H_
#define _SETUP_COMMAND_H_

// C includes.
// C++ includes.
#include <string>
#include <iostream>
#include <sstream>
#include <list>
#include <exception>

// Framework includes.
// Local includes.
#include <net2raid.h>
#include <command.h>

using namespace std;


class SetupCommand: public Command {
    int snaplen;
    bool promiscuous;
    int duration;
    int file_size;
    string interface;
    string capture_file;
    int smp_affinity;
    int ring_buffers;
    int write_blocks;
    int pages_per_buffer;

    const int NUM_PARAMETERS;
            
    public:
    SetupCommand(const vector<string>& params):
        NUM_PARAMETERS(10)
    {
        if (params.size() != NUM_PARAMETERS + 1)
            throw runtime_error("SetupCommand: invalid number of parameters.");

        istringstream(params[1]) >> snaplen;
        istringstream(params[2]) >> promiscuous;
        istringstream(params[3]) >> duration;
        istringstream(params[4]) >> file_size;
        interface = params[5];
        capture_file = params[6];
        istringstream(params[7]) >> smp_affinity;
        istringstream(params[8]) >> ring_buffers;
        istringstream(params[9]) >> write_blocks;
        istringstream(params[10]) >> pages_per_buffer;
    }

    void execute() {
        // State is stored globally.
        INFO("Setup capture process.");
        // Assorted constants.
        const int SETUP_COMMAND_INTERVAL(1);
        const int STATS_INTERVAL(1);
        const int POLL_TIMEOUT(1);
        const bool PREALLOCATED(true);
        const bool DIRECTIO(true);
        const unsigned long FILE_SIZE(file_size);

        // Set SMP affinity.
        const unsigned int cpu_setsize (sizeof(cpu_set_t));
        cpu_set_t mask;
        const pid_t mypid(0);
        CPU_ZERO(&mask);
        CPU_SET(smp_affinity, &mask);
        if (sched_setaffinity(mypid, cpu_setsize, &mask) < 0)
            ERR("Unble to set process affinity.");

        // Setup buffer pool.
        const int BUFFER_SIZE(getpagesize()*pages_per_buffer);

        // Create FileWriter threads.
        FILE_WRITER_STATS = new StatsWriter(
                FILE_WRITER_STATS_ID,
                LOG_PREFIX + string("fw_") + interface,
                STATS_INTERVAL,
                SETUP_COMMAND_INTERVAL);
        FILE_WRITER = new FileWriter(
                FILE_WRITER_ID,
                BUFFER_SIZE,
                write_blocks,
                capture_file,
                POLL_TIMEOUT,
                (StatsWriter* const)FILE_WRITER_STATS,
                SETUP_COMMAND_INTERVAL,
                FILE_SIZE,
                PREALLOCATED,
                DIRECTIO,
                false);

        // Create NetReader threads.
        NET_READER_STATS = new StatsWriter(
                NET_READER_STATS_ID,
                LOG_PREFIX + string("nr_") + interface,
                STATS_INTERVAL,
                SETUP_COMMAND_INTERVAL);

        NET_READER = new NetReader(
                NET_READER_ID,
                interface,
                snaplen,
                PAYLOAD_LENGTH,
                BUFFER_SIZE,
                promiscuous,
                (FileWriter* const)FILE_WRITER,
                (StatsWriter* const)NET_READER_STATS,
                SETUP_COMMAND_INTERVAL);
    }
    string to_string() { return string("setup"); }
    void dump() {
        INFO("SetupCommand {}");
        INFO("snaplen: " << snaplen);
        INFO("promiscuous: " << promiscuous);
        INFO("duration: " << duration);
        INFO("file_size: " << file_size);
        INFO("interface: " << interface);
        INFO("capture_file: " << capture_file);
        INFO("smp_affinity: " << smp_affinity);
        INFO("ring_buffers: " << ring_buffers);
        INFO("write_blocks: " << write_blocks);
        INFO("pages_per_buffer: " << pages_per_buffer);
    }
};

#endif // _SETUP_COMMAND_H_
