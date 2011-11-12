'''
Created on Oct 3, 2011

@author: dlapsley
'''

import sys
import os

def get_pypath_resource(resource_name):
    for path in sys.path:
        if os.path.exists(os.path.join(path, resource_name)):
            return os.path.join(path, resource_name)
    raise Exception('Resource %s could not be found' % resource_name)