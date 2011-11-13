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

    Unit test class for drs-client application.
'''

import os
import socket
import subprocess
import sys
import threading
import time
import unittest
import SocketServer

import Client
import Utils

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
        vsi_thread = threading.Thread(target=vsi_server.serve_forever)
        vsi_thread.start()

        # This will be the client's sink.
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((HOST, PORT))

        # One end will be the client's source.
        (r, w) = os.pipe()
        cmd_source = os.fdopen(r, 'r')
        cli_in = os.fdopen(w, 'w')

        (r, w) = os.pipe()
        cli_out = os.fdopen(r, 'r')
        rsp_sink = os.fdopen(w, 'w')

        # Use dependency injection to construct the client.
        vsi_client = Client.Client(
            cmd_source=cmd_source, cmd_sink=sock, rsp_source=sock,
            rsp_sink=rsp_sink)
        client_thread = threading.Thread(target=vsi_client.run)
        client_thread.start()

        cmds = [ 'hello;', 'how;', 'are;', 'you;' ]
        for cmd in cmds:
            cli_in.write('%s\n'%cmd)
            cli_in.flush()

        for cmd in cmds:
            rsp = cli_out.readline().strip()
            self.assertEqual(rsp, '%s%s'%(PROMPT, cmd))

        cli_in.write('exit;\n')
        cli_in.flush()

        time.sleep(0.1)

        try:
            sock.close()
            vsi_server.shutdown()
        except Exception, e:
            print 'Service is closed: %s'%e

        client_thread.join()
        vsi_thread.join()


if __name__ == '__main__':
    Utils.set_log_level(0)
    suite = unittest.TestLoader().loadTestsFromTestCase(TestClient)
    unittest.TextTestRunner(verbosity=2).run(suite)
