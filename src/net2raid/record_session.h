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
// C++ includes.
#include <string>
#include <iostream>

// Framework includes.
// Local includes.

using namespace std;

class RecordSession {
    // Variables to store options.
    public:
        int snaplen;
        bool promiscuous;
        int duration;
        int rate;
        string interface;
        string capture_file;
        int smp_affinity;
        int ring_buffers;
        int write_blocks;
        int pages_per_buffer;

    public:
        RecordSession() {}

        void init(istream& in) {
            in  >> snaplen
                >> promiscuous
                >> duration
                >> rate
                >> interface
                >> capture_file
                >> smp_affinity
                >> ring_buffers
                >> write_blocks
                >> pages_per_buffer;
        }

        string to_string() {
            ostringstream oss;
            oss << snaplen << " "
                << promiscuous << " "
                << duration << " "
                << rate << " "
                << interface << " "
                << capture_file << " "
                << smp_affinity << " "
                << ring_buffers << " "
                << write_blocks << " "
                << pages_per_buffer;
            return oss.str();
        }

        void dump() {
            INFO("snaplen: " << snaplen);
            INFO("promiscuous: " << promiscuous);
            INFO("duration: " << duration);
            INFO("rate: " << snaplen);
            INFO("interface: " << interface);
            INFO("capture_file: " << capture_file);
            INFO("smp_affinity: " << smp_affinity);
            INFO("ring_buffers: " << ring_buffers);
            INFO("write_blocks: " << write_blocks);
            INFO("pages_per_buffer: " << pages_per_buffer);
        }
};

