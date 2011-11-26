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

// C includes.
#include <fcntl.h>
#include <sys/select.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>
#include <sched.h>
#include <unistd.h>

// C++ includes.
#include <iostream>
#include <iomanip>
#include <string>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp> 

// Local includes.
#include <mark6.h>
#include <m6logger.h>
#include <pfr.h>
#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>
#include <record_session.h>

// Namespaces.
namespace po = boost::program_options;
using namespace std; // Clean up long lines.

//----------------------------------------------------------------------
// Declarations
//----------------------------------------------------------------------
void main_cli(const pid_t child_pid,
        const int child_fd);
void child_cli(const int parent_fd);

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const string DEFAULT_INTERFACES("eth0");
const int DEFAULT_SNAPLEN(8224);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
const string DEFAULT_LOG_NAME("net2raid");
// const int DEFAULT_PAYLOAD_LENGTH(8224);
const int DEFAULT_PAYLOAD_LENGTH(8268);
const int DEFAULT_SMP_AFFINITY(0);
const int DEFAULT_WRITE_BLOCKS(128);
const int DEFAULT_RATE(4000);
const string LOG_PREFIX("/opt/mit/mark6/log/");

// Other constants.
const int MAX_SNAPLEN(9014);
const int STATS_SLEEP(1);
const int PAYLOAD_LENGTH(DEFAULT_PAYLOAD_LENGTH);
const int DISK_RAMP_UP_TIME(2);

enum {
    FILE_WRITER_ID, FILE_WRITER_STATS_ID, NET_READER_ID, NET_READER_STATS_ID };

//----------------------------------------------------------------------
// Global variables.
//----------------------------------------------------------------------
const int LOCAL_PAGES_PER_BUFFER(256);
// const int LOCAL_PAGES_PER_BUFFER(64);

int LOCAL_PAGE_SIZE(0);
int BUFFER_SIZE(0);

FileWriter* FILE_WRITER(0);
NetReader* NET_READER(0);
StatsWriter* FILE_WRITER_STATS(0);
StatsWriter* NET_READER_STATS(0);

//----------------------------------------------------------------------
// Utility functions.
//----------------------------------------------------------------------
void sigproc(int sig) {
    static int called = 0;

    if (called)
        return;
    else
        called = 1;

    // Join threads.
    NET_READER->cmd_stop();
    NET_READER_STATS->cmd_stop();

    FILE_WRITER->cmd_stop();
    FILE_WRITER_STATS->cmd_stop();
}

//----------------------------------------------------------------------
// Program entry point.
//----------------------------------------------------------------------
int main (int argc, char* argv[]) {
    const int log_level(LOG_INFO);

    // Configure log subsystem.
    init_logger(DEFAULT_LOG_NAME, log_level);

    // Start processing.
    pid_t child_pid;
    int child_fd;
    try {
        DEBUG("Starting.");
        int fd[2];
        if (pipe(fd) < 0) {
            ERR("Pipe error");
            exit(1);
        }

        pid_t pid;
        if ( (pid = fork()) < 0) {
            ERR("Unable to fork. Exiting.");
            exit(1);
        } else if (pid == 0) {
            // Child. Do stuff then exit.
            child_cli(fd[0]);
            return 0;
        } else {
            // Parent.
            DEBUG("Parent still here after fork.");

            // Clean up pipe for communicating with child.
            // fd[1] will be write fd.
            close(fd[0]);
            child_fd = fd[1];
        }
    } catch (std::exception& e) {
        cerr << e.what() << endl;
    }

    INFO("Launching main.\n");

    main_cli(child_pid, child_fd);

    return 0;
}

void main_cli(const pid_t child_pid,
        const int child_fd) {
    RecordSession r;
    while (true) {
        string cmd;
        cin >> cmd;
        if (cmd.compare("setup") == 0) {
            INFO("Starting recording session.");
            r.init(cin);
            r.dump();
            const string cmd("setup " + r.to_string());
            write(child_fd, cmd.c_str(), cmd.length() + 1);
        } else if (cmd.compare("start") == 0) {
            INFO("Received start()");
            write(child_fd, "start\n", 6);
            sleep(r.duration);
        } else if (cmd.compare("stop") == 0) {
            INFO("Received stop()");
            write(child_fd, "stop\n", 5);
            break;
        } else if (cmd.compare("teardown") == 0) {
            INFO("Received teardown()");
            write(child_fd, "teardown\n", 9);
        } else {
            ERR("Unknown command: " << cmd);
        }
    }
    waitpid(child_pid, NULL, 0);
    INFO("PID: " << (int)child_pid << " terminated...");
}

void child_cli(int parent_fd) {
    DEBUG("Started child_cli");

    RecordSession r;

    while (true) {
        string cmd;
        cin >> cmd;

        // Wait for threads to finish.
        if (cmd.compare("setup") == 0) {
            r.init(cin);
            r.dump();

            // Assorted constants.
            const int COMMAND_INTERVAL(1);
            const int STATS_INTERVAL(1);
            const int POLL_TIMEOUT(1);
            const bool PREALLOCATED(true);
            const bool DIRECTIO(true);
            const unsigned long FILE_SIZE(r.duration*r.rate/8); // MB

            // Set SMP affinity.
            const unsigned int cpu_setsize (sizeof(cpu_set_t));
            cpu_set_t mask;
            const pid_t mypid(0);
            CPU_ZERO(&mask);
            CPU_SET(r.smp_affinity, &mask);
            if (sched_setaffinity(mypid, cpu_setsize, &mask) < 0)
                ERR("Unble to set process affinity.");

            // Setup buffer pool.
            const int BUFFER_SIZE(getpagesize()*r.pages_per_buffer);

            // Create FileWriter threads.
            FILE_WRITER_STATS = new StatsWriter(
                    FILE_WRITER_STATS_ID,
                    LOG_PREFIX + string("fw_") + r.interface,
                    STATS_INTERVAL,
                    COMMAND_INTERVAL);
            FILE_WRITER = new FileWriter(
                    FILE_WRITER_ID,
                    BUFFER_SIZE,
                    r.write_blocks,
                    r.capture_file,
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
                    LOG_PREFIX + string("nr_") + r.interface,
                    STATS_INTERVAL,
                    COMMAND_INTERVAL);

            NET_READER = new NetReader(
                    NET_READER_ID,
                    r.interface,
                    r.snaplen,
                    PAYLOAD_LENGTH,
                    BUFFER_SIZE,
                    r.promiscuous,
                    (FileWriter* const)FILE_WRITER,
                    (StatsWriter* const)NET_READER_STATS,
                    COMMAND_INTERVAL);
        } else if (cmd.compare("start") == 0) {
            // Start!
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
        } else if (cmd.compare("stop") == 0) {
            NET_READER_STATS->cmd_stop();
            NET_READER_STATS->join();

            NET_READER->cmd_stop();
            NET_READER->join();

            FILE_WRITER_STATS->cmd_stop();
            FILE_WRITER_STATS->join();

            FILE_WRITER->cmd_stop();
            FILE_WRITER->join();
        } else if (cmd.compare("teardown")) {
            delete NET_READER_STATS;
            delete NET_READER;
            delete FILE_WRITER_STATS;
            delete FILE_WRITER;

            INFO("Stopped capture process.");
        }
    }
}
