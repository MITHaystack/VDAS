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

        
COMMANDS = set([
    'input_stream',
    'mod_init',
    'record',
    'vol_cmd',
    'vsm',
    'vsm_mask',
    'disk_info',
    'error',
    'rtime',
    'scan_check',
    'scan_info',
    'status',
    'sys_info',
    'vol_stack',
    ])

class Command(object):

    '''
    Parse incoming VSIS commands.
    '''

    def __init__(self, parsed):
        params = parsed['params']
        for p, P in zip(params, self.__class__.PARAMS):
            self.__dict__[P] = p

    def __getitem__(self, key):
        return self.__dict__.get(key, None)

class Query(Command):
    pass


class DiskInfoQuery(Query):
    PARAMS = [
        'info_type',
        'vol_ref',
        ]


class ErrorQuery(Query):
    PARAMS = [
        'dimino6_return_code',
        ]


# SUPPORTED
class InputStreamCommand(Command):
    PARAMS = [
        'action',
        'stream_label',
        'data_format',
        'interface_id',
        'filter_address',
        ]


# SUPPORTED
class InputStreamQuery(Query):
    PARAMS = [
        'stream_label',
        ]


class ModInitCommand(Command):
    PARAMS = [
        'msn',
        'number_disks',
        ]


class ModInitQuery(Query):
    PARAMS = [
        ]


# SUPPORTED
class RecordCommand(Command):
    PARAMS = [
        'action',
        'start_time',
        'duration',
        'data_size',
        'scan_name',
        'experiment_name',
        'station_code',
        ]


# SUPPORTED
class RecordQuery(Query):
    PARAMS = []


# SUPPORTED
class RtimeQuery(Query):
    PARAMS = [
        'volref',
        'data_rate',
        ]


class ScanCheckCommand(Command):
    PARAMS = [
        'volref',
        'scan_name',
        ]


class ScanInfoQuery(Query):
    PARAMS = [
        'volref',
        'scan_name',
        ]


# SUPPORTED
class StatusQuery(Query):
    PARAMS = []

    
class SysInfoQuery(Query):
    PARAMS = []


class VolCmdCommand(Command):
    PARAMS = [
        'action',
        'volref1',
        'volref2',
        'volref3',
        'volref4',
        'volref5',
        'volref6',
        'volref7',
        'volref8',
        ]


class VolStackQuery(Query):
    PARAMS = [
        'volref',
        ]


class VSMQuery(Command):
    PARAMS = [
        'volref',
        ]

    
class VSMCommand(Command):
    PARAMS = [
        'volref',
        'VSM',
        ]

    
class VSMMaskCommand(Command):
    PARAMS = [
        'erase_mask_enable',
        'play_mask_enable',
        'record_mask_enable',
        ]


class VSMMaskQuery(Query):
    PARAMS = []

