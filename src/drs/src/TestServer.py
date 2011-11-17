#!/usr/bin/python

## Copyright 2011 MIT Haystack Observatory
##
## This file is part of Mark6 VDAS.
##
## Mark6 VDAS is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2 of the License.
##
## Mark6 VDAS is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Mark6 VDAS.    If not, see <http://www.gnu.org/licenses/>.

'''
Author:     del@haystack.mit.edu
Date:         11/12/2011
Description:

    Unit test class for Server class.
'''

import os
import socket
import subprocess
import sys
import time
import unittest
import SocketServer
import threading

import Server
import TCPRequestHandler
import Utils


class TestServer(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_construct(self):
        HOST = 'localhost'
        PORT = 14242
        vsi_server = Server.Server(
            (HOST, PORT), TCPRequestHandler.TCPRequestHandler)
        vsi_thread = threading.Thread(target=vsi_server.serve_forever)
        vsi_thread.start()

        # Connection to the server.
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((HOST, PORT))

        cmds = [
            'input_stream?;',
            'input_stream=add:stream_a:vdif:eth2:4505;',
            'input_stream=add:stream_b:vdif:eth3:4506;',
            'input_stream=add:stream_c:vdif:eth4:4507;',
            'input_stream=add:stream_d:vdif:eth5:4508;',
            'input_stream=add:stream_e:vdif:eth6:4509;',
            'input_stream=dismount:stream_e;',
            'input_stream?;',
            'input_stream?stream_a;',

            # 'record=on:starttime:duration:datasize:scanname:experimentname:stationcode;',
            # 'mod_init=msn:disks;',
            # 'mod_init?;',
            # 'record?;',
            # 'rtime?volref:datarate;',
            # 'scan_check=volref:scanname;',
            # 'scan_info?volref:scanname;',
            # 'status?;',
            # 'sys_info?;',
            # 'vol_cmd=action:volref1:volref2;',
            # 'vol_stack?;',
            # 'VSM=volref:VSM;',
            # 'VSM?volref;',
            # 'VSM=erasemaskenable:playmaskenable:recordmaskenable;',
            # 'VSM?;',
        ]

        for cmd in cmds:
            sock.sendall('%s\n'%cmd)
            resp = sock.recv(1024)
            print 'got', resp

        sock.sendall('exit;\n')

        vsi_server.shutdown()
        vsi_thread.join()


if __name__ == '__main__':
    Utils.set_log_level(0)
    suite = unittest.TestLoader().loadTestsFromTestCase(TestServer)
    unittest.TextTestRunner(verbosity=2).run(suite)
