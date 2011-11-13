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

import socket
import logging
import re
import sys
import getopt
import optparse

from Utils import set_log_level


class Client(socket.socket):
    '''
    This class implements command line parsing functionality.
    '''


    def __init__(self, host, port, debug=False):
        '''Constructor.

        Args:
            debug: Whether or not we are in debug mode (commands not
                    sent in debug mode).

        Returns:
            None.
        '''
        self._host = host
        self._port = port
        self._debug = debug

    def run(self):
        '''Main execution loop.'''
        try:
            self.connect((self._host, self._port))
        except Exception, e:
            print 'Unable to connect to server: %s'%e

        VALID_RE = re.compile('.*;')
        
        while True:
            try:
                line = raw_input("mark6>")
                line = line.strip()
                if line == 'exit;':
                    return
                
                if not re.match(VALID_RE, line):
                    continue
                
                if len(line) == 0:
                    continue
                
                self.sendall(line)
                response = self.recv(2048)
                print response.strip()
            except Exception, e:
                print 'Exception: %s'%e
                try:
                    self.connect((self._host, self._port))
                except:
                    pass

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
        '-H', '--host', dest='host', help='VSIS host address.',
        default=DEFAULT_VSIS_HOST)
    parser.add_option(
        '-l', '--log_level', dest='log_level', help='Log level(0,..,4).',
        default=DEFAULT_LOG_LEVEL)
    parser.add_option(
        '-f', '--input_file', dest='input_file',
        help='Input command file (for batch mode).')
    parser.add_option(
        '-t', '--test', action='store_true', dest='test', help='Run tests.',
        default=False)

    (o, a) = parser.parse_args()

    set_log_level(o.log_level)

    Client(o.host, o.port).run()


if __name__ == '__main__':
    main()
