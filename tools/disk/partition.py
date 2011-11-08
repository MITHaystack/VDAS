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

import sys
import copy
import commands
import re
import pprint
import time
import getopt
from os import mkdir
from os.path import exists, isdir
from xml.etree.ElementTree import fromstring, dump
import pexpect
from subprocess import Popen, PIPE

class DiskHardwareProfile(object):

    LSHW_COMMAND = '/usr/bin/lshw -xml'
    HW_KEY_FIELD = 'serial'
    HW_FIELDS = [
        'product', 'vendor', 'physid', 'businfo', 'logicalname', 'dev',
        'version', 'size'
        ]
    
    def __init__(self, hwinfo_file = None):
        hwinfo = None
        if hwinfo_file is None:
            hwinfo = commands.getoutput(DiskHardwareProfile.LSHW_COMMAND)
        else:
            hwinfo = ''.join(open(hwinfo_file).readlines())
        hwinfo_tree = fromstring(hwinfo)

        self._disks = dict()

        root = hwinfo_tree.find('node')
        bridge_nodes = filter(lambda x: x.get('class') == 'bridge',
                              root.findall('node'))
        for n in bridge_nodes:
            disk_nodes = filter(lambda x: x.get('class') == 'disk',
                                n.findall('node/node/node/node'))
            for d in disk_nodes:
                key = d.find(DiskHardwareProfile.HW_KEY_FIELD).text
                self._disks[key] = dict()
                for f in DiskHardwareProfile.HW_FIELDS:
                    self._disks[key][f] = d.find(f).text

        pp = pprint.PrettyPrinter()
        pp.pprint(self._disks)

    def get_logical_names(self):
        result = [ self._disks[x]['logicalname']
                   for x in self._disks.keys()]
        result.sort()
        return result


class Condition(object):
    '''Conditions and partitions an entire disk pack.

    
    '''

    FDISK_CMD = '/sbin/fdisk'
    MKFS_CMD = '/sbin/mkfs'
    TUNE2FS_CMD = '/sbin/tune2fs'
    E2FSCK_CMD = '/sbin/e2fsck'
    DUMPE2FS_CMD = '/sbin/dumpe2fs'
    MOUNT_CMD = '/bin/mount'
    MOUNT_OPTS = 'default,data=writeback,noatime,nodiratime'
    UNMOUNT_CMD = '/bin/umount'

    def __init__(self, disks):
        self._disks = copy.copy(disks)
        self._mount_points = [
            '/mnt/disk%d'%(ord(x[-1]) - ord('b'),)
            for x in self._disks
            ]

    def partition(self):

        start_time = time.time()
        for d in self._disks:
            print '-- Creating fresh partitions on ', d

            disk_start_time = time.time()
            cmd = '%s %s'%(Condition.FDISK_CMD, d)
            child = pexpect.spawn(cmd)

            # Function aliasing to make things look neater.
            expect = child.expect
            sendline = child.sendline
        
            # Show existing partitions.
            expect('p\): ')
            sendline('p')

            # Delete partitions
            print child.before
            for i in range(1, 5):
            	expect('p\): ')
                sendline('d')
                j = expect([ 'No partition.*\): ', 'Selected.*\): ', '4\): ' ])
                if j == 0:
                    print 'No partition defined.'
                    break
                elif j == 1:
                    print 'One partition defined.'
                    break
                elif j == 2:
                    print 'N partitions defined.'
                    sendline('%d'%i)
                    print child.before

            # Create a new partition
            sendline('n')
            print child.before

            expect('^')
            sendline('p')
            print child.before

            expect('\): ')
            sendline('1')
            print child.before

            expect('\): ')
            sendline('')
            print child.before

            expect('\): ')
            sendline('')
            print child.before

            # Write results to partition table and exit.
            expect('help\): ')
            sendline('p')
            print child.before

            expect('help\): ')
            sendline('w')
            print child.before

            child.wait()

            disk_stop_time = time.time()
            disk_duration = disk_stop_time -disk_start_time
            print 'duration=%f'%disk_duration

        print 'total_duration=%s'%(time.time() - start_time,)


    def mkfs(self):
        start_time = time.time()

        for d in self._disks:
            disk_start_time = time.time()
            print '-- Creating fresh ext4 filesystem on', d
            
            cmd = ' '.join([ Condition.MKFS_CMD, '-t', 'ext4', '%s1'%d ])
            print cmd

            p = Popen(cmd, shell=True)
            p.wait()

            print 'duration=%f'%(time.time() - disk_start_time, )

        print 'total_duration=%f'%(time.time() - start_time,)


    def tunefs(self):
        start_time = time.time()

        for d in self._disks:
            disk_start_time = time.time()
            print '-- Tuning fresh ext4 filesystem on', d

            # Enable writeback mode. This mode will typically provide 
            # the best ext4 performance.
            cmd = ' '.join([ Condition.TUNE2FS_CMD, '-o',
                             'journal_data_writeback', '%s1'%d ])
            print cmd
            p = Popen(cmd, shell=True)
            p.wait()

            # Delete the has_journal option.
            cmd = ' '.join([ Condition.TUNE2FS_CMD, '-O',
                             '^has_journal', '%s1'%d ])
            print cmd
            p = Popen(cmd, shell=True)
            p.wait()

            # Required fsck.
            cmd = ' '.join([ Condition.E2FSCK_CMD, '-f', '%s1'%d ])
            print cmd
            p = Popen(cmd, shell=True)
            p.wait()

            # Check fs options.
            cmd = ' '.join([ Condition.DUMPE2FS_CMD, '%s1'%d ])
            print cmd
            p = Popen(cmd, shell=True)
            p.wait()

            print 'duration=%f'%(time.time() - disk_start_time, )

        print 'total_duration=%f'%(time.time() - start_time,)


    def mount_devs(self):

        # Check mount points.
        for d in self._mount_points:
            if not exists(d):
                mkdir(d)

        # Mount disks
        start_time = time.time()
        for d, m in zip(self._disks, self._mount_points):
            disk_start_time = time.time()

            print '-- Mounting %s on %s'%(d, m)
            cmd = ' '.join([
                    Condition.MOUNT_CMD, '-t', 'ext4',
                    '%s1'%d, m
                    ])
            # '-o', Condition.MOUNT_OPTS, '%s1'%d, m
            print cmd
            p = Popen(cmd, shell=True)
            p.wait()
                
            print 'duration=%f'%(time.time() - disk_start_time, )

        print 'total_duration=%f'%(time.time() - start_time,)

    def unmount_devs(self):

        # Unmount all mounted devices.
        start_time = time.time()
        for d in self._mount_points:
            disk_start_time = time.time()

            print 'Unmounting %s'%d
            cmd = ' '.join([Condition.UNMOUNT_CMD, d])
            print cmd
            p = Popen(cmd, shell=True)
            p.wait()

            print 'duration=%f'%(time.time() - disk_start_time, )

        print 'total_duration=%f'%(time.time() - start_time,)


def usage():
    print '''
usage: partition -p -c -t -m -u -C -f <hw profile> -h
                 -d Devices to operate on (last letter, comma separated)
                 -p Partition devices
                 -c Create file systems on devices
                 -t Tune file systems
                 -m Mount disks
                 -u Unmount disks
                 -C Condition module (i.e., -p, -m, -t, and -M)
                 -f Extract hardware profile from specified xml file
                 -h Display this message.
'''

#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
if __name__ == '__main__':
    OPTIONS = 'd:pctmuCf:h'
    hwinfo_file = None
    partition_devices = False
    create_file_systems = False
    tune_file_systems = False
    mount_disks = False
    unmount_disks = False
    condition_module = False
    devices = None
    try:
        opts, args = getopt.getopt(sys.argv[1:], OPTIONS)
        for o, a in opts:
            if o == '-d':
                devices = str(a)
                devices = [ '/dev/sd%s'%x for x in devices.split(',') ]
            elif o == '-p':
                partition_devices = True
            elif o == '-c':
                create_file_systems = True
            elif o == '-t':
                tune_file_systems = True
            elif o == '-m':
                mount_disks = True
            elif o == '-u':
                unmount_disks = True
            elif o == '-C':
                condition_module = True
            elif o == '-f':
                hwinfo_file = str(a)
            elif o == '-h':
                usage()
                sys.exit(2)
            else:
                assert False, 'Unhandled option: %s'%o
    except getopt.GetoptError, err:
        usage()
        sys.exit(2)

    
    DHP = DiskHardwareProfile(hwinfo_file)

    if partition_devices or condition_module:
        Condition(devices).partition()
    elif create_file_systems or condition_module:
        Condition(devices).mkfs()
    elif tune_file_systems or condition_module:
        Condition(devices).tunefs()
    elif mount_disks or condition_module:
        Condition(devices).mount_devs()
    elif unmount_disks:
        Condition(devices).unmount_devs()
    else:
        print 'Hardware devices available: ', DHP.get_logical_names()        


