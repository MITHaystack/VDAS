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

import logging
import socket
import sys
import threading
import SocketServer
import TCPRequestHandler as TCPRequestHandler

from Utils import set_log_level


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

    try:
        vsi_server = Server.Server(
            (o.host, o.port), TCPRequestHandler.TCPRequestHandler)

        vsi_thread = threading.Thread(target=vsi_server.serve_forever)
        vsi_thread.setDaemon(True)
        vsi_thread.start()
        vsi_thread.join()
    except Exception, e:
        logging.error('Server error: %s'%e)


if __name__ == '__main__':
    main()
