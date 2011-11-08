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

'''

import socket
import logging
import re
import sys
import getopt
import optparse


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
        self.connect((self._host, self._port))

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


def main():
	parser = optparse.OptionParser()
	parser.add_option(
        '-p', '--port', dest='port', help='VSIS TCP port.', default=14242)
	parser.add_option(
        '-h', '--host', dest='host', help='VSIS host address.',
        default='localhost')
	parser.add_option(
        '-l', '--log_level', dest='log_level', help='Log level.',
        default='1')
	parser.add_option(
        '-f', '--input_file', dest='input_file',
        help='Input command file (for batch mode).')
	parser.add_option(
        '-t', '--test', action='store_true', dest='test', help='Run tests.',
        default=False)

	(o, a) = parser.parse_args()

    log_level = 0
    l = int(o.log_level)
    if l == 0:
        log_level = logging.DEBUG
    elif l == 1:
        log_level = logging.INFO
    elif l == 2:
        log_level = logging.WARNING
    elif l == 3:
        log_level = logging.ERROR
    else:
        log_level = logging.CRITICAL
    logging.basicConfig(level=log_level)
    
    Client(o.host, o.port).run()


if __name__ == '__main__':
    main()
