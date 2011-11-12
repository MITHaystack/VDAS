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

import logging

#-----------------------------------------------------------
class Response(object):

    def __init__(self, **kwargs):
        # Return parameters.
        self._params = []
        
        # List of return parameters.
        self._list_params = []
        try:
            for n in self.__class__.PARAMS:
                self.append_param(value=kwargs[n])
        except Exception, e:
            logging.error('Response exception: %s'%e)

        try:
            for l in kwargs['list']:
                list_param = [ '\n' ]
                for k, v in zip(self.__class__.LIST_PARAMS, l):
                    list_param.append(v)
                list_param_string = ':'.join(list_param)
                self.append_list_param(list_param_string)
                    # TODO: More checking.
        except Exception, e:
            logging.error('Response exception: %s'%e)

    def append_param(self, value):
        self._params.append(value)
        
    def append_list_param(self, value):
        self._list_params.append(value)
        
    def __str__(self):
        resp = ''.join([
            '!',
            self.__class__.NAME,
            '?',
            ':'.join(self._params)
            ])
        list_resp = ''
        if len(self._list_params):
            list_resp = ''.join(self._list_params)
        
        return resp + list_resp[:-2] + ';'

#-----------------------------------------------------------
class DiskInfo(Response):

    NAME = 'disk_info'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'type',
        ]
    LIST_PARAMS = [
        'volref', 'msn', 'num_disks', 'disk_info',
        ]

#-----------------------------------------------------------
class Error(Response):

    NAME = 'error'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'dimino6_error_message',
        ]
    

#-----------------------------------------------------------
class SetInputStream(Response):

    NAME = 'input_stream'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        ]


class GetInputStream(Response):

    NAME = 'input_stream'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        # TODO: Correct VSIS spec - no nested optional parameters
        ]
    LIST_PARAMS = [
        'stream_label', 'data_format', 'interface_id', 'filter_address',
        ]


#-----------------------------------------------------------
class SetModInit(Response):

    NAME = 'mod_init'
    PARAMS = [
        'MSN',
        'num_disks',
        ]


class GetModInit(Response):

    NAME = 'mod_init'
    PARAMS = [
        'return_code',
        ]



#-----------------------------------------------------------
class SetRecord(Response):

    NAME = 'record'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        ]


class GetRecord(Response):

    NAME = 'record'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'status',
        'scan_number',
        'scan_name',
        ]


#-----------------------------------------------------------
class Rtime(Response):

    NAME = 'rtime'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'volref',
        'data_rate',
        'remaining_time',
        'remaining_gb',
        'remaining_percent',
        ]


#-----------------------------------------------------------
class ScanCheck(Response):

    NAME = 'scan_check'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'volref',
        'scan_num',
        'scan_label',
        'num_streams',
        ]
    LIST_PARAMS = [
        'stream_label', 'status', 'data_format', 'start_time',
        'duration', 'datasize', 'stream_rate',
        ]


#-----------------------------------------------------------
class ScanInfo(Response):

    NAME = 'scan_info'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'volref',
        'mark6_sn',
        'scan_number',
        'scan_label',
        'status',
        'start_time',
        'duration',
        'num_data_streams',
        ]
    LIST_PARAMS = [
        # TODO: fix VSIS-spec.
        'msn', 'num_disks', 'disk_datasizes',
        ]


#-----------------------------------------------------------
class Status(Response):

    NAME = 'record'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'status_word',
        ]


#-----------------------------------------------------------
class SysInfo(Response):

    NAME = 'sys_info'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'system_type',
        'mark6_sn',
        'os_type_rev',
        'dimino6_version_number',
        'command_set_revision',
        'available_ram',
        'num_data_disks_supported',
        'num_ethernet_input_ports',
        ]
    LIST_PARAMS = [
        'portref', 'portspeed',
        ]


#-----------------------------------------------------------
class SetVolCmd(Response):

    NAME = 'vol_cmd'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        ]

class GetVolCmd(Response):

    NAME = 'vol_cmd'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        ]
    LIST_PARAMS = [
        'volref',
        ]

#-----------------------------------------------------------
class VolStack(Response):

    NAME = 'vol_stack'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        ]
    LIST_PARAMS = [
        'volref', 'ex_msn', 'num_disks_nominal', 'num_disks_discovered',
        'percent_full', 'module_status',
        ]

#-----------------------------------------------------------
class SetVSM(Response):

    NAME = 'set_vsm'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'VSM',
        ]

class GetVSM(Response):

    NAME = 'set_vsm'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        ]
    LIST_PARAMS = [
        'volref', 'msn', 'vsm',
        ]

#-----------------------------------------------------------
class SetVSMMask(Response):

    NAME = 'set_vsm'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'erase_mask_enable',
        'play_mask_enable',
        'record_mask_enable'
        ]

class GetVSMMask(Response):

    NAME = 'set_vsm'
    PARAMS = [
        'return_code',
        'dimino6_return_code',
        'erase_mask_enable',
        'play_mask_enable',
        'record_mask_enable'
        ]
