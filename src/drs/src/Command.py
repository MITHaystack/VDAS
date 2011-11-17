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

import Response
import State
        
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
            setattr(self, P, p)

    def __getitem__(self, key):
        return getattr(self, key, None)

    def todict(self):
        return dict([(x, getattr(self, x)) for x in self.__class__.PARAMS])

    def todict_generator(self):
        for x in self.__class__.PARAMS:
            yield (x, getattr(self, x))

    def __iter__(self):
        return self.todict_generator()

    def execute(self):
        raise Exception('Please implement me.')


class Query(Command):
    pass


class DiskInfoQuery(Query):
    PARAMS = [
        'info_type',
        'vol_ref',
        ]

    def execute(self):
        logging.debug('params: %s'%q.parsed())
        r = Response.DiskInfo(return_code='0',
                dimino6_return_code='0',
                type='type',
                list=[])
        return str(r)


class ErrorQuery(Query):
    PARAMS = [
        'dimino6_return_code',
        ]

    def execute(self):
        logging.debug('params: %s'%q.parsed())
#        r = Response.error(return_code='0',
#                        dimino6_return_code='0',
#                        dimino6_error_message='There was an error.',
#                        list=[])
#        return str(r)



class InputStreamCommand(Command):
    PARAMS = [
        'action',
        'stream_label',
        'data_format',
        'interface_id',
        'filter_address',
        ]

    def execute(self):
        if self.action == 'add':
            State.add_input_stream(self.stream_label,
                                   self.data_format,
                                   self.interface_id,
                                   self.filter_address)            
        elif self.action == 'dismount':
            State.dismount_input_stream(self.stream_label)
        else:
            print 'Invalid input_stream_command action'
        
        r = Response.SetInputStream(
                return_code='0', dimino6_return_code='0', list=[])
        return str(r)



class InputStreamQuery(Query):
    PARAMS = [
        'stream_label',
        ]

    def execute(self):
        s = State.get_input_stream(getattr(self, 'stream_label', None))
        l = []
        for e in s:
            l.append([e['stream_label'],
                      e['data_format'],
                      e['interface_id'],
                      e['filter_address']])

        r = Response.GetInputStream(
            return_code='0', dimino6_return_code='0', list=l)

        return str(r)
            


class ModInitCommand(Command):
    PARAMS = [
        'msn',
        'number_disks',
        ]


class ModInitQuery(Query):
    PARAMS = [
        ]


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

    def execute(self):
        logging.info('params: %s'%s.__dict__)
        
        if self.action == 'on':
            State.on_record_session(self.start_time,
                                    self.duration,
                                    self.data_size,
                                    self.scan_name,
                                    self.experiment_name,
                                    self.station_code)
        elif self.action == 'off':
            State.off_record_session(self)
        else:
            print 'Invalid record action'
        
        # TODO: fill in parameters.
        return Response.SetRecord(
            return_code='0', dimino6_return_code='0', list=[])


class RecordQuery(Query):
    PARAMS = []


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

