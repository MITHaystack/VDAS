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

#ifndef _COMMAND_H_
#define _COMMAND_H_

// C includes.
// C++ includes.
#include <string>
#include <iostream>
#include <list>

// Framework includes.
// Local includes.
#include <net2raid.h>

using namespace std;

class Command {
    public:
        Command() {}
        Command(const list<string>& params) {}
        virtual void execute(){}
        virtual string to_string() {}
        virtual void dump() {}
};


class NullCommand: public Command {
    public:
        NullCommand(): Command() {}
        void execute() {
            INFO("NullCommand executing.");
        }
        string to_string() { return string("null"); }
        void dump() {
            INFO("NullCommand {}");
        }
};


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
    SetupCommand(const list<string>& params):
        NUM_PARAMETERS(10)
    {

#if 0
            ostringstream oss;
            oss << snaplen << " "
                << promiscuous << " "
                << duration << " "
                << file_size << " "
                << interface << " "
                << capture_file << " "
                << smp_affinity << " "
                << ring_buffers << " "
                << write_blocks << " "
                << pages_per_buffer;
            return oss.str();
        }
#endif

    }
    void execute() {
        // State is stored globally.
        INFO("Setup capture process.");
        // Assorted constants.
        const int COMMAND_INTERVAL(1);
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
                COMMAND_INTERVAL);
        FILE_WRITER = new FileWriter(
                FILE_WRITER_ID,
                BUFFER_SIZE,
                write_blocks,
                capture_file,
                POLL_TIMEOUT,
                (StatsWriter* const)FILE_WRITER_STATS,
                COMMAND_INTERVAL,
                FILE_SIZE,
                PREALLOCATED,
                DIRECTIO,
                false);

        // Create NetReader threads.
        NET_READER_STATS = new StatsWriter(
                NET_READER_STATS_ID,
                LOG_PREFIX + string("nr_") + interface,
                STATS_INTERVAL,
                COMMAND_INTERVAL);

        NET_READER = new NetReader(
                NET_READER_ID,
                interface,
                snaplen,
                PAYLOAD_LENGTH,
                BUFFER_SIZE,
                promiscuous,
                (FileWriter* const)FILE_WRITER,
                (StatsWriter* const)NET_READER_STATS,
                COMMAND_INTERVAL);
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


class StartCommand: public Command {
    // Variables to store options.
    public:
        StartCommand(const list<string>& params) {}
        void execute() {
            INFO("StartCommand executing.");

            // State is stored globally.
            FILE_WRITER_STATS->start();
            FILE_WRITER_STATS->cmd_write_to_disk();

            FILE_WRITER->open();
            FILE_WRITER->start();
            FILE_WRITER->cmd_write_to_disk();

            NET_READER_STATS->start();
            NET_READER_STATS->cmd_write_to_disk();

            // sleep(DISK_RAMP_UP_TIME);

            NET_READER->start();
            NET_READER->cmd_read_from_network();
            INFO("Started capture process.");
        }
        string to_string() { return string("start"); }
        void dump() {
            INFO("StartCommand {}");
        }
};


class StopCommand: public Command {
    // Variables to store options.
    public:
        StopCommand(const list<string>& params) {}
        void execute() {
            INFO("StartCommand executing.");

            // State is stored globally.
            NET_READER_STATS->cmd_stop();
            NET_READER_STATS->join();

            NET_READER->cmd_stop();
            NET_READER->join();

            FILE_WRITER_STATS->cmd_stop();
            FILE_WRITER_STATS->join();

            FILE_WRITER->cmd_stop();
            FILE_WRITER->join();
            INFO("Stopped capture process.");
        }
        string to_string() { return string("stop"); }
        void dump() {
            INFO("StopCommand {}");
        }
};


class TeardownCommand: public Command {
    // Variables to store options.
    public:
        TeardownCommand(const list<string>& params) {}
        void execute() {
            INFO("TeardownCommand executing.");

            // State is stored globally.
            delete NET_READER;
            delete NET_READER_STATS;
            delete FILE_WRITER;
            delete FILE_WRITER_STATS;

            NET_READER = (NetReader*)0;
            NET_READER_STATS = (StatsWriter*)0;
            FILE_WRITER = (FileWriter*)0;
            FILE_WRITER_STATS = (StatsWriter*)0;
        }
        string to_string() { return string("teardown"); }
        void dump() {
            INFO("TeardownCommand {}");
        }
};

#endif // _COMMAND_H_
