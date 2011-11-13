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
Date:         5/12/2011
Description:

Implements the Mark6 VDAS serving functionality.
'''

import getopt
import logging
import optparse
import socket
import sys
import SocketServer
import TCPRequestHandler as TCPRequestHandler

from threading import Thread
from Utils import get_pypath_resource
from Utils import set_log_level


class ThreadedTCPServer(SocketServer.ThreadingTCPServer):

    def server_bind(self): 
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1) 
        self.socket.bind(self.server_address)
                # self.socket.settimeout(5.0)


class Server(object):

    def __init__(self, vsi_server):
        self._vsi_server = vsi_server
        self._vsi_thread = Thread(target=self._vsi_server.serve_forever)

    def start(self):
        self._vsi_thread.setDaemon(True)
        self._vsi_thread.start()

    def shutdown(self):
        self._vsi_server.shutdown()

    def join(self):
        self._vsi_thread.join()


# Default parameters.
DEFAULT_VSIS_PORT = 14242
DEFAULT_VSIS_HOST = 'localhost'
DEFAULT_LOG_LEVEL = '2'

def main():
    parser = optparse.OptionParser()
    parser.add_option(
        '-p', '--port', dest='port', help='VSIS TCP port.',
        default=DEFAULT_VSIS_PORT)
    parser.add_option(
        '-H', '--host', dest='host', help='VSIS host.',
        default=DEFAULT_VSIS_HOST)
    parser.add_option(
        '-l', '--log_level', dest='log_level', help='Log level(0,..4).',
        default=DEFAULT_LOG_LEVEL)

    (o, a) = parser.parse_args()

    set_log_level(o.log_level)

    logging.info("Starting server")
    # TODO
    # CONFIGURATION_FILE='dimino6.xml'
    # logging.info("%s"%get_pypath_resource(CONFIGURATION_FILE))

    vsi_server = ThreadedTCPServer((o.host, o.port),
        TCPRequestHandler.TCPRequestHandler)

    s = Server(vsi_server)
    s.start()
    s.join()


if __name__ == '__main__':
    main()
