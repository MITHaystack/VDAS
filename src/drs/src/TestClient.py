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
Date:     11/12/2011
Description:
'''


import unittest
import subprocess
import time

class TestClient(unittest.TestCase):

  def setUp(self):
    pass

  def tearDown(self):
    pass

  def test_construct(self):
    args = ['python', './drs-server.py' ]
    pid = subprocess.Popen(args).pid

    time.sleep(1)

    args = ['python', './drs-client.py', '-l', '0' ]
    p = subprocess.Popen(args, stdin=subprocess.PIPE)
    p.stdin.write('exit;')


if __name__ == '__main__':
  suite = unittest.TestLoader().loadTestsFromTestCase(TestClient)
  unittest.TextTestRunner(verbosity=2).run(suite)

