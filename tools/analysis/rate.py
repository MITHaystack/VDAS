#!/usr/bin/python

# Copyright 2011 MIT Haystack Observatory
#
# This file is part of Mark6.
# 
# Mark6 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2 of the License.
# 
# Mark6 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Mark6.  If not, see <http://www.gnu.org/licenses/>.

import os, sys
from stat import *

bytes_written = 0
number_of_disks = 7
mount_prefix = '/mnt/disk'
for i in range(number_of_disks):
	file_name = '%s%d/test.m6'%(mount_prefix, i)
	size = os.stat(file_name)[ST_SIZE]
	print size
	bytes_written += size


time=30
rate=8e9
throughput=bytes_written*8/(time*1000000)

print time, ' s'
print rate, ' bps'
print bytes_written/1e6, ' MB'
print throughput, ' Mbps'
