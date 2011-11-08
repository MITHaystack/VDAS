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
#include <logger.h>
#include <pfr.h>
#include <file_writer.h>
#include <net_reader.h>
#include <stats_writer.h>

// Namespaces.
namespace po = boost::program_options;
using namespace std; // Clean up long lines.

//----------------------------------------------------------------------
// Declarations
//----------------------------------------------------------------------
void banner();
void main_cli(const int time,
	      const vector<pid_t>& child_pids,
	      const vector<int>& child_fds);
void child_cli(const int parent_fd);

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const string DEFAULT_INTERFACES("eth0");
const int DEFAULT_SNAPLEN(8224);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
// TODO: make environment variable.
const string DEFAULT_LOG_CONFIG("/opt/mit/mark6/etc/net2raid-log.cfg");
// const int DEFAULT_PAYLOAD_LENGTH(8224);
const int DEFAULT_PAYLOAD_LENGTH(8268);
const int DEFAULT_SMP_AFFINITY(0);
const int DEFAULT_WRITE_BLOCKS(128);
const int DEFAULT_RATE(4000);
const int DEFAULT_TRANSLATE(false);

// Other constants.
const int MAX_SNAPLEN(9014);
const int STATS_SLEEP(1);
const int PAYLOAD_LENGTH(DEFAULT_PAYLOAD_LENGTH);
// TODO: make environment variable.
const string LOG_PREFIX("/opt/mit/mark6/log/");
const int DISK_RAMP_UP_TIME(2);

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
// Print usage message.
// @param desc Options description message.
// @return None.
void
usage(const po::options_description& desc) {
  cout
    << "net2raid [options]" << endl
    << desc;
}

void
sigproc(int sig) {
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
int
main (int argc, char* argv[]) {
  // Variables to store options.
  string log_config; 
  string config;
  int snaplen;
  bool promiscuous;
  int time;
  int rate;
  vector<string> interfaces;
  vector<string> capture_files;
  vector<int> smp_affinities;
  int ring_buffers;
  int write_blocks;
  bool translate;
  int pages_per_buffer;

  // Declare supported options, defaults, and variable bindings.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")

    ("v", "print version message")

    ("snaplen",
     po::value<int>(&snaplen)->default_value(DEFAULT_SNAPLEN),
     "capture snap length")

    ("promiscuous",
     po::value<bool>(&promiscuous)->default_value(DEFAULT_PROMISCUOUS),
     "enable promiscuous mode")

    ("time",
     po::value<int>(&time)->default_value(DEFAULT_TIME),
     "capture interval")

    ("rate",
     po::value<int>(&rate)->default_value(DEFAULT_RATE),
     "individual file rate (Mbps)")

    ("log_config",
     po::value<string>(&log_config)->default_value(DEFAULT_LOG_CONFIG),
     "log configuration file")

    ("interfaces",
     po::value< vector<string> >(&interfaces)->multitoken(),
     "list of interfaces from which to capture data")

    ("capture_files",
     po::value< vector<string> >(&capture_files)->multitoken(),
     "list of capture files")

    ("smp_affinities",
     po::value< vector<int> >(&smp_affinities)->multitoken(),
     "smp processor affinities")

    ("write_blocks",
     po::value<int>(&write_blocks)->default_value(DEFAULT_WRITE_BLOCKS),
     "per thread number of write blocks")

    ("pages_per_buffer",
     po::value<int>(&pages_per_buffer)->default_value(LOCAL_PAGES_PER_BUFFER),
     "size of write blocks in pages(4096B).")

    ("translate",
     po::value<bool>(&translate)->default_value(DEFAULT_TRANSLATE),
     "enable payload translation/extraction")
    ;

  // Parse options.
  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  // Configure log subsystem.
  init_logger(log_config);

  // Check various options.
  if (vm.count("help")) {
    usage(desc);
    return 1;
  }

  if (vm.count("v")) {
    cout << "net2raid version: 0.1"
	      << endl;
    return 1;
  }

  const int NUM_INTERFACES = interfaces.size();
  if (capture_files.size() != NUM_INTERFACES) {
    LOG4CXX_ERROR(logger, "Arg length mismatch, capture_files");
    usage(desc);
    return 1;
  }

  banner();

  cout << setw(20) << left << "interfaces:";
  BOOST_FOREACH(string s, interfaces)
    cout << s << " ";
  cout << endl;

  cout << setw(20) << left << "capture_files:";
  BOOST_FOREACH(string s, capture_files)
    cout << s << " ";
  cout << endl;

  cout << setw(20) << left << "smp_affinities:";
  BOOST_FOREACH(int s, smp_affinities)
    cout << s << " ";
  cout << endl;

  cout
    << setw(20) << left << "snaplen:" << snaplen << endl
    << setw(20) << left << "promiscuous:" << promiscuous << endl
    << setw(20) << left << "time:" << time << endl
    << setw(20) << left << "log_config:" << log_config << endl
    << setw(20) << left << "write_blocks:" << write_blocks << endl
    << setw(20) << left << "num_interfaces:" << NUM_INTERFACES << endl
    << setw(20) << left << "translate:" << translate << endl;

  // Start processing.
  vector<pid_t> child_pids;
  vector<int> child_fds;
  try {
    LOG4CXX_DEBUG(logger, "Starting.");

    const int COMMAND_INTERVAL(1);
    const int STATS_INTERVAL(1);
    const int POLL_TIMEOUT(1);
    const bool PREALLOCATED(true);
    const bool DIRECTIO(true);
    const bool TRANSLATE(true);
    const unsigned long FILE_SIZE(time*rate/8); // MB

    pid_t pid;
    for (int i=0; i<NUM_INTERFACES; i++) {
      int fd[2];
      if (pipe(fd) < 0) {
	LOG4CXX_ERROR(logger, "Pipe error");
	exit(1);
      }

      if ( (pid = fork()) < 0) {
	LOG4CXX_ERROR(logger, "Unable to fork. Exiting.");
	exit(1);
      } else if (pid == 0) {
	// Child. Do stuff then exit.
	LOG4CXX_DEBUG(logger, "Forked child: " << i);

	// Clean pipe for receiving commands from parent. fd[0] will be
	// read fd.
	close(fd[1]);

	// Setup shutdown handler.
	signal(SIGINT, sigproc);

	// Set SMP affinity.
	const unsigned int cpu_setsize (sizeof(cpu_set_t));
	cpu_set_t mask;
	const pid_t mypid(0);
	CPU_ZERO(&mask);
	CPU_SET(smp_affinities[i], &mask);
	if (sched_setaffinity(mypid, cpu_setsize, &mask) < 0)
	  LOG4CXX_ERROR(logger, "Unble to set process affinity.");

	// Setup buffer pool.
	const int BUFFER_SIZE(getpagesize()*pages_per_buffer);

	// Create FileWriter threads.
	FILE_WRITER_STATS
	  = new StatsWriter(i,
			    LOG_PREFIX + string("fw_") + interfaces[i],
			    STATS_INTERVAL,
			    COMMAND_INTERVAL);
	FILE_WRITER
	  = new FileWriter(i,
			   BUFFER_SIZE,
			   write_blocks,
			   capture_files[i],
			   POLL_TIMEOUT,
			   (StatsWriter* const)FILE_WRITER_STATS,
			   COMMAND_INTERVAL,
			   FILE_SIZE,
			   PREALLOCATED,
			   DIRECTIO,
			   translate);
	FileWriter * const FW(FILE_WRITER);

	// Create NetReader threads.
	NET_READER_STATS
	  = new StatsWriter(i+1,
			    LOG_PREFIX + string("nr_") + interfaces[i],
			    STATS_INTERVAL,
			    COMMAND_INTERVAL);

	NET_READER
	  = new NetReader(i,
			  interfaces[i],
			  snaplen,
			  PAYLOAD_LENGTH,
			  BUFFER_SIZE,
			  promiscuous,
			  (FileWriter* const)FILE_WRITER,
			  (StatsWriter* const)NET_READER_STATS,
			  COMMAND_INTERVAL);

	// Wait for threads to finish.
	child_cli(fd[0]);
	return 0;
      } else {
	// Parent.
	LOG4CXX_DEBUG(logger, "Parent still here after fork.");

	// Clean up pipe for communicating with child. fd[1] will be write fd.
	close(fd[0]);
	child_fds.push_back(fd[1]);
      }
    }
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  LOG4CXX_INFO(logger, "Launching main.\n");

  main_cli(time, child_pids, child_fds);

  return 0;
}

void main_cli(const int time, const vector<pid_t>& child_pids,
	      const vector<int>& child_fds) {
  // START recording.
  cout
    << "Received start()\n"
    << "Recording for " << time << " seconds\n";

  BOOST_FOREACH(int fd, child_fds) {
    LOG4CXX_INFO(logger, "Starting child fd: " << fd);
    write(fd, "start\n", 6);
  }

  sleep(time);

  cout << "Received stop()";
  BOOST_FOREACH(int fd, child_fds)
    write(fd, "stop\n", 5);

  BOOST_FOREACH(pid_t p, child_pids) {
    waitpid(p, NULL, 0);
    cout << "PID: " << (int)p << " terminated..." << endl;
  }
}

void child_cli(int parent_fd) {
  LOG4CXX_DEBUG(logger, "Started child_cli");

  FILE* parent_file = fdopen(parent_fd, "r");
  if (parent_file == NULL) {
    LOG4CXX_ERROR(logger, "Unable to create file stream.");
    exit(1);
  }
    
  int bytes_read;
  size_t nbytes(256);
  char* line_read;
  while (true) {
    line_read = (char*)malloc(nbytes+1);
    bytes_read = getline(&line_read, &nbytes, parent_file);
    if (bytes_read < 0) {
      LOG4CXX_INFO(logger, "Closed CLI stream.");
      free(line_read);
      break;
    } else {
      string s(line_read);
      free (line_read);
      vector<string> results;
      string cmd;
      split(results, s, is_any_of(" \t"));
      if (results.size() == 0)
	continue;
      
      cmd = results[0];
      trim(cmd);
      if (cmd.compare("start") == 0) {
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
	LOG4CXX_INFO(logger, "Started capture process.");
      } else if (cmd.compare("stop") == 0) {
	NET_READER_STATS->cmd_stop();
	NET_READER_STATS->join();

	NET_READER->cmd_stop();
	NET_READER->join();

	FILE_WRITER_STATS->cmd_stop();
	FILE_WRITER_STATS->join();

	FILE_WRITER->cmd_stop();
	FILE_WRITER->join();
	LOG4CXX_INFO(logger, "Stopped capture process.");
      }
    }
  }
}

void banner() {
  cout
    << HLINE
    << endl
    << "|                                                                              |\n"
    << "|                              Net2disk v0.5                                   |\n"
    << "|                                                                              |\n"
    << "|                                                                              |\n"
    << "|                  Copyright 2011 MIT Haystack Observatory                     |\n"
    << "|                          dlapsley@haystack.mit.edu                           |\n"
    << "|                                                                              |\n"
    << HLINE
    << endl
    << endl;
}
