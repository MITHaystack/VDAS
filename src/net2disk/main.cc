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
void cli();

//----------------------------------------------------------------------
// Constants
//----------------------------------------------------------------------

// Option defaults.
const string DEFAULT_INTERFACES("eth0");
const int DEFAULT_SNAPLEN(8224);
const bool DEFAULT_PROMISCUOUS(true);
const int DEFAULT_TIME(30);
const string DEFAULT_LOG_CONFIG("/opt/mit/mark6/etc/net2disk-log.cfg");
// const int DEFAULT_PAYLOAD_LENGTH(8224);
const int DEFAULT_PAYLOAD_LENGTH(8268);
const int DEFAULT_SMP_AFFINITY(0);
const int DEFAULT_WRITE_BLOCKS(128);
const int DEFAULT_RATE(4000);

// Other constants.
const int MAX_SNAPLEN(9014);
const int STATS_SLEEP(1);
const int PAYLOAD_LENGTH(DEFAULT_PAYLOAD_LENGTH);
const string LOG_PREFIX("/opt/mit/mark6/log/");
const int DISK_RAMP_UP_TIME(2);

//----------------------------------------------------------------------
// Global variables.
//----------------------------------------------------------------------
const int LOCAL_PAGES_PER_BUFFER(256);

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
    << "net2disk [options]" << endl
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
  string interface;
  vector<string> capture_files;
  int smp_affinity;
  int write_blocks;

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
     po::value< string >(&interface)->multitoken(),
     "list of interfaces from which to capture data")

    ("capture_files",
     po::value< vector<string> >(&capture_files)->multitoken(),
     "capture files")

    ("smp_affinity",
     po::value< int >(&smp_affinity)->multitoken(),
     "smp processor affinities")

    ("write_blocks",
     po::value<int>(&write_blocks)->default_value(DEFAULT_WRITE_BLOCKS),
     "per thread number of write blocks")
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
    cout << "net2disk version: 0.1" << endl;
    return 1;
  }

  banner();

  cout << setw(20) << left << "capture_files:";
  BOOST_FOREACH(string s, capture_files)
    cout << s << " ";
  cout << endl;

  cout
    << setw(20) << left << "smp_affinity:" << smp_affinity << endl
    << setw(20) << left << "snaplen:" << snaplen << endl
    << setw(20) << left << "promiscuous:" << promiscuous << endl
    << setw(20) << left << "time:" << time << endl
    << setw(20) << left << "log_config:" << log_config << endl
    << setw(20) << left << "write_blocks:" << write_blocks << endl;

  // Start processing.
  try {
    LOG4CXX_DEBUG(logger, "Starting.");

    const int COMMAND_INTERVAL(1);
    const int STATS_INTERVAL(1);
    const int POLL_TIMEOUT(1);

    // Setup shutdown handler.
    signal(SIGINT, sigproc);

    // Set SMP affinity.
    const unsigned int cpu_setsize (sizeof(cpu_set_t));
    cpu_set_t mask;
    const pid_t mypid(0);
    CPU_ZERO(&mask);
    CPU_SET(smp_affinity, &mask);
    if (sched_setaffinity(mypid, cpu_setsize, &mask) < 0)
      LOG4CXX_ERROR(logger, "Unble to set process affinity.");

    // Setup buffer pool.
    const int BUFFER_SIZE(getpagesize()*LOCAL_PAGES_PER_BUFFER);

    // Create FileWriter threads.
    const int FILE_WRITER_STATS_ID(0);
    const int FILE_WRITER_ID(1);
    const int NET_READER_STATS_ID(2);
    const int NET_READER_ID(3);

#ifdef FILE_WRITER
    FILE_WRITER_STATS
      = new StatsWriter(FILE_WRITER_STATS_ID,
			LOG_PREFIX + string("fw_") + interfaces[i],
			STATS_INTERVAL,
			COMMAND_INTERVAL);
    FILE_WRITER
      = new FileWriter(FILE_WRITER_ID,
		       BUFFER_SIZE,
		       write_blocks,
		       capture_files[i],// FIXME
		       POLL_TIMEOUT,
		       (StatsWriter* const)FILE_WRITER_STATS,
		       COMMAND_INTERVAL);
    FileWriter * const FW(FILE_WRITER);

    // Create NetReader threads.
    NET_READER_STATS
      = new StatsWriter(NET_READER_STATS_ID,
			LOG_PREFIX + string("nr_") + interfaces[i],
			STATS_INTERVAL,
			COMMAND_INTERVAL);

    NET_READER
      = new NetReader(NET_READER_ID,
		      interfaces,
		      snaplen,
		      PAYLOAD_LENGTH,
		      BUFFER_SIZE,
		      promiscuous,
		      (FileWriter* const)FILE_WRITER,
		      (StatsWriter* const)NET_READER_STATS,
		      COMMAND_INTERVAL);
#endif

    cli();
  } catch (std::exception& e) {
    cerr << e.what() << endl;
  }

  return 0;
}


void cli() {
  LOG4CXX_DEBUG(logger, "Started child_cli");

  const int nbytes(256);
  char line[nbytes];

  while (true) {
    cout << ">>";
    if (!cin.good()) {
      LOG4CXX_ERROR(logger, "CLI Input stream closed. Exiting...");
      break;
    }

    cin.getline(line, nbytes);
    string line_read(line);
    trim(line_read);
    if (line_read.size() == 0) {
      continue;
    }

    vector<string> results;
    string cmd;
    split(results, line_read, is_any_of(" \t"));
    if (results.size() > 0) {
      cmd = results[0];

      if (cmd.compare("start") == 0) {
	LOG4CXX_DEBUG(logger, "cli() received start cmd");

#ifdef FILE_WRITER
	cout << "Starting file writer stats...\n";
	FILE_WRITER_STATS->start();
	FILE_WRITER_STATS->cmd_write_to_disk();
	cout << "Started.\n";

	cout << "Starting file writer...\n";
	FILE_WRITER->open();
	FILE_WRITER->start();
	FILE_WRITER->cmd_write_to_disk();
	cout << "Sta[rted.\n";

	cout << "Starting net reader stats...\n";
	NET_READER_STATS->start();
	NET_READER_STATS->cmd_write_to_disk();
	cout << "Started.\n";

	// sleep(DISK_RAMP_UP_TIME);

	cout << "Starting net reader...\n";
	NET_READER->start();
	NET_READER->cmd_read_from_network();
#endif
	cout << "Started.\n";
      } else if (cmd.compare("stop") == 0) {
#ifdef FILE_WRITER
	NET_READER_STATS->cmd_stop();
	NET_READER_STATS->join();
	cout << "Stopped net reader stats.\n";
      
	NET_READER->cmd_stop();
	NET_READER->join();
	cout << "Stopped net reader.\n";
      
	FILE_WRITER_STATS->cmd_stop();
	FILE_WRITER_STATS->join();
	cout << "Stopped file writer stats.\n";

	FILE_WRITER->cmd_stop();
	FILE_WRITER->join();
#endif
	cout << "Stopped file writer.\n";
      } else if (cmd.compare("quit") == 0) {
	exit(0);
      }
    }
  }
}

void banner() {
  cout
    << HLINE
    << endl
    << "|                                                                              |\n"
    << "|                              Net2disk v0.1                                   |\n"
    << "|                                                                              |\n"
    << "|                                                                              |\n"
    << "|                  Copyright 2011 MIT Haystack Observatory                     |\n"
    << "|                            del@haystack.mit.edu                              |\n"
    << "|                                                                              |\n"
    << HLINE
    << endl
    << endl;
}
