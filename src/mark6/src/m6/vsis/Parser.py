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

import re
import mark6.vsis.Command as VSISC

import re


CMD_RE_STRING = "(\w+)=((\w+)(:\w+)*);"
QRY_RE_STRING = "(\w+)\\?((\w+)(:\w+)*)*;"
EXIT_RE_STRING = "exit;"

CMD_RE = re.compile(CMD_RE_STRING)
QRY_RE = re.compile(QRY_RE_STRING)
EXIT_RE = re.compile(EXIT_RE_STRING)

class Parser(object):
    
    def __init__(self):
        pass

    def parse(self, s):
        s = s.strip()
        m = re.match(CMD_RE, s)
        result = { 'type': None }
        if m is not None:
            groups = list(m.groups())
            result['type'] = 'CMD'
            result['name'] = groups[0].lower()
            result['params'] = []
            if len(groups) > 1 and groups[1]:
                result['params'].extend(groups[1].split(':'))
        else:
            m = re.match(QRY_RE, s)
            if m is not None:
                groups = list(m.groups())
                result['type'] = 'QRY'
                result['name'] = groups[0].lower()
                result['params'] = []
                if len(groups) > 1 and groups[1]:
                    result['params'].extend(groups[1].split(':'))
                    
        return result
