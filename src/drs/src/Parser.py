## Copyright 2011 MIT Haystack Observatory
##
## This file is part of Mark6 VDAS.
##
## Mark6 VDAS is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 3 of the License.
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
import string

import Command
import Response
import State

# Globals
CMD_RE_STRING = "(\w+)=((\w+)(:\w+)*);"
QRY_RE_STRING = "(\w+)\\?((\w+)(:\w+)*)*;"
EXIT_RE_STRING = "exit;"

# In re form.
CMD_RE = re.compile(CMD_RE_STRING)
QRY_RE = re.compile(QRY_RE_STRING)
EXIT_RE = re.compile(EXIT_RE_STRING)


class Parser(object):
    '''Implements command pattern.

    Parses an incoming query string and generates an executable command
    object based on this. Returns the object to the caller for execution.
    '''
    
    def __init__(self):
        pass

    def re_match(self, s, regexp, re_type):
        s = s.strip()
        m = re.match(regexp, s)
        result = None
        if m is not None:
            groups = list(m.groups())
            result = {
                'type': re_type,
                'name': groups[0].lower(),
                'params': [] }

            if len(groups) > 1 and groups[1]:
                result['params'].extend(groups[1].split(':'))

        return result

    def parse(self, s):
        parsed = self.re_match(s, CMD_RE, 'CMD')
        if not parsed:
            parsed = self.re_match(s, QRY_RE, 'QRY')

        if not parsed:
            raise Exception('invalid command string: %s'%s)

        # Get the appropriate command object.
        if parsed['name'] not in Command.COMMANDS:
            raise Exception('invalid command: %s', parsed['name'])
        else:
            # e.g., input_stream -> InputStreamQuery || InputStreamCOmmand
            parsed_name = ''.join(string.capwords(parsed['name'], '_').split('_'))
            if cmp(parsed['type'], 'QRY') == 0:
                command = parsed_name + 'Query'
            elif cmp(parsed['type'], 'CMD') == 0:
                command = parsed_name + 'Command'
            else:
                raise Exception('invalid command type: %s', parsed['type'])

        # Retrieve the command class from the Command module. 
        command = getattr(Command, command)
        return command(parsed)

