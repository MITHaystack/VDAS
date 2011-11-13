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
'''

import fcntl
import os
import socket
import subprocess
import sys
import time
import unittest
import SocketServer
from threading import Thread

class TestTCPServer(SocketServer.TCPServer):

    def server_bind(self): 
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1) 
        self.socket.bind(self.server_address)


class TestRequestHandler(SocketServer.BaseRequestHandler):

    def handle(self):
        running = True
        while running:
            r = self.request.recv(1024).strip()
            try:
                self.request.sendall('%s\n'%r)
            except:
                running = False

PROMPT='mark6>'

class TestClient(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_construct(self):
        HOST='localhost'
        PORT=14242
        vsi_server = TestTCPServer((HOST, PORT), TestRequestHandler)
        vsi_thread = Thread(target=vsi_server.serve_forever)
        vsi_thread.start()

        time.sleep(0.5)

        args = ['python', './drs-client.py', '-l', '0' ]
        p = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE)

        stdout, stdin = p.stdout, p.stdin

        cmds = [ 'hello;', 'how;', 'are;', 'you;' ]
        for cmd in cmds:
            stdin.write('%s\n'%cmd)
            stdin.flush()

        for cmd in cmds:
            rsp1 = stdout.readline().strip()
            rsp2 = stdout.readline().strip()
            self.assertEqual(rsp1, '%s%s'%(PROMPT, cmd))

        stdin.write('exit;\n')
        r = stdout.readline()

        try:
            vsi_server.shutdown()
        except Exception, e:
            print 'Service is closed: %s'%e

        vsi_thread.join()


if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TestClient)
    unittest.TextTestRunner(verbosity=2).run(suite)

