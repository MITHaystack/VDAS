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

//C++ includes
#include <string>

// Framework includes.
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/algorithm/string.hpp> 

// Local includes.
#include <logger.h>
#include <scan_check.h>

// Namespaces.
namespace po = boost::program_options;

const std::string DEFAULT_LOG_CONFIG("/opt/mit/mark6/etc/net2raid-log.cfg");

void
usage(const po::options_description& desc) {
  std::cout
    << "scancheck [options]" << std::endl
    << desc;
}

int
main(int argc, char *argv[]) {
  // Variables to store options.
  std::string input_file;
  std::string output_prefix;
  int frame_size;

  // Declare supported options, defaults, and variable bindings.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("frame_size",
     po::value<int>(&frame_size)->default_value(8224),
     "total number of megabytes to output (across all files)")
    ("input_file",
     po::value<std::string>(&input_file),
     "input file");

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  // Configure log subsystem.
  init_logger(DEFAULT_LOG_CONFIG);

  if (vm.count("help") || argc < 3) {
    usage(desc);
    return 1;
  }
  
  ScanCheck sc(input_file, frame_size);

  return(0);
}


