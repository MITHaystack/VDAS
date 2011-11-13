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

import logging
import re
import sys
import optparse
import socket

from Utils import set_log_level
from Client import Client


# Default parameters.
DEFAULT_VSIS_PORT = 14242
DEFAULT_VSIS_HOST = '127.0.0.1'
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

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        sock.connect((o.host, o.port))
        Client(cmd_source=sys.stdin, cmd_destination=sock).run()
    except Exception, e:
        logging.error('Unable to connect to DRS server: %s'%e)

if __name__ == '__main__':
    main()
