'''
Created on Oct 3, 2011

@author: dlapsley
'''

import sys
import os
import logging

def get_pypath_resource(resource_name):
    for path in sys.path:
        if os.path.exists(os.path.join(path, resource_name)):
            return os.path.join(path, resource_name)
    raise Exception('Resource %s could not be found' % resource_name)

def set_log_level(level):
  LOG_LEVEL_MAP = {
      '0':  logging.DEBUG,
      '1':  logging.INFO,
      '2':  logging.WARNING,
      '3':  logging.ERROR,
      '4':  logging.CRITICAL
      }
  try:
    level = LOG_LEVEL_MAP.get(o.log_level, logging.INFO)
  except:
    level = logging.INFO

  logging.basicConfig(level=level)
