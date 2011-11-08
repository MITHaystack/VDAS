## Copyright 2011 MIT Haystack Observatory
##
## This file is part of dimino6.
##
## dimino6 is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## dimino6 is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with dimino6.  If not, see <http://www.gnu.org/licenses/>.

'''
Author:   del@haystack.mit.edu
Date:     5/12/2011
Description:

Implements the dimino6 serving functionality.
'''

from threading import Thread
import getopt
import logging
import sys
import socket
import SocketServer
import mark6.vsis.TCPRequestHandler as TCPRequestHandler
from Utils import get_pypath_resource


class ThreadedTCPServer(SocketServer.ThreadingTCPServer):

    def server_bind(self): 
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1) 
        self.socket.bind(self.server_address)
        # self.socket.settimeout(5.0)


class Server(object):

    def __init__(self, host, vsi_port):
        
        self._vsi_server = ThreadedTCPServer((host, vsi_port),
                                             TCPRequestHandler.TCPRequestHandler)
        self._vsi_thread = Thread(target=self._vsi_server.serve_forever)
        
    def start(self):
        self._vsi_thread.setDaemon(True)
        self._vsi_thread.start()
        
    def shutdown(self):
        self._vsi_server.shutdown()
        
    def join(self):
        self._vsi_thread.join()


def usage():
    print '''
usage: d6server -p <vsis port>
                -i <vsis ip>
                -m <log level: 0..4>
                -h
'''


if __name__ == '__main__':

    # Default parameters.
    VSIS_PORT = 14242
    VSIS_HOST = 'localhost'
    LEVEL = logging.INFO
    
    # Process options
    OPTIONS = 'p:i:m:h'
    try:
        opts, args = getopt.getopt(sys.argv[1:], OPTIONS)
    except getopt.GetoptError, err:
        print str(err)
        usage()
        sys.exit(2)

    for o, a in opts:
        if o == '-p':
            VSIS_PORT = int(a)
        elif o == '-i':
            VSIS_HOST = str(a)
        elif o == '-m':
            l = int(a)
            if l == 0:
                LEVEL = logging.DEBUG
            elif l == 1:
                LEVEL = logging.INFO
            elif l == 2:
                LEVEL = logging.WARNING
            elif l == 3:
                LEVEL = logging.ERROR
            else:
                LEVEL = logging.CRITICAL
        elif o == '-h':
            usage()
            sys.exit(2)
        else:
            assert False, "Unhandled option"

    logging.basicConfig(level=LEVEL)
    
    logging.info("Starting server")
    CONFIGURATION_FILE='dimino6.xml'
    logging.info("%s"%get_pypath_resource(CONFIGURATION_FILE))
    s = Server(VSIS_HOST, VSIS_PORT)
    s.start()
    s.join()
