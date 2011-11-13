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

DRS Client module.
'''

import re
import sys


class Client(object):
    '''
    This class implements command line parsing functionality.

    It reads commands from a CLI, does some basic format checking and then
    forwards the command to the DRS server. The response is then read from
    the DRS server and written back to the CLI.

    The class was designed using the Dependency Injection pattern to facilitate
    flexibility and testing.
    '''

    PROMPT='mark6>'

    def __init__(self, cmd_source, cmd_sink, rsp_source, rsp_sink):
        '''Constructor.

        Args:
            cmd_source: A file-like object from which to read commands.
            rsp_sink: A file-like object to which responses will be sent.
            cmd_sink: A file-like object to which commands will be sent.
            rsp_source: A file-like object from which to read responses.

        Returns:
            None.
        '''
        self._cmd_source = cmd_source
        self._cmd_sink = cmd_sink
        self._rsp_source = rsp_source
        self._rsp_sink = rsp_sink

    def run(self):
        '''Main execution loop.'''
        VALID_RE = re.compile('.*;')
        running = True        
        while running:
            try:
                self._rsp_sink.write(Client.PROMPT)
                self._rsp_sink.flush()
                line = self._cmd_source.readline()
                line = line.strip()
                if line == 'exit;':
                    return
                
                if not re.match(VALID_RE, line):
                    continue
                
                if len(line) == 0:
                    continue
                
                self._cmd_sink.sendall(line)
                response = self._rsp_source.recv(2048)
                self._rsp_sink.write(response)
                self._rsp_sink.flush()
            except Exception, e:
                print 'Exception: %s'%e
                running = False

