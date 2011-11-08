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

#ifndef DISK2VLBI_H
#define DISK2VLBI_H

#include <string>

#define ETHER_TYPE_IP (0x0800)
#define ETHER_TYPE_8021Q (0x8100)
#define UDP_PROTOCOL_NUMBER (17)
#define UDP_HEADER_LENGTH (8)

class Disk2vlbi {
 public:
  Disk2vlbi(const std::string capture_file, const unsigned long long size);
};

#endif // DISK2VLBI_H
