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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Mark6 VDAS.  If not, see <http://www.gnu.org/licenses/>.

'''
Author:   del@haystack.mit.edu
Date:     5/12/2011
Description:

'''

import logging
import socket
import sys
import Queue
import SocketServer

import Parser


class TCPRequestHandler(SocketServer.BaseRequestHandler):
    '''Implements Command Pattern.

    Receives incoming commands from a socket. Accumulates characters
    into full command strings. Passes strings on to Parser which generates
    an executable Command object.  Executes the command object and then
    returns response across TCP socket.
    '''

    STATEMENT_TERMINATOR = ';'

    def __init__(self, request, client_address, server):
        self._queue = Queue.Queue()
        SocketServer.BaseRequestHandler.__init__(self, request,
                                                 client_address,
                                                 server)

    def handle(self):
        logging.debug('Handle')
        ST = TCPRequestHandler.STATEMENT_TERMINATOR
        
        received_data = []
        v = Parser.Parser()

        while True:
            try:
                b = self.request.recv(1).strip()
                if not len(b):
                    continue

                received_data.append(b)

                # Accumulate characters until statement terminator found.
                if cmp(b, ST) == 0:
                    received_command = ''.join(received_data).strip()
                    if len(received_command) <= 1:
                        continue

                    if received_command == 'exit;':
                        break

                    try:
                        # Parse command and lookup handler.
                        command = v.parse(received_command)
                        response = command.execute()
                        self.request.sendall(response)
                    except Exception, e:
                        self.request.sendall(str(e))

                    received_data = []
            except socket.timeout, e:
                received_data = []
                logging.error('handle: Received timeout')
            except Exception, e:
                received_data = []
                logging.error('handle: Other exception: %s'%e)

