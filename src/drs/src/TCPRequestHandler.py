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

import logging
import socket
import sys
import Queue
import SocketServer

import Command
import Parser
import Response
import State
    

class TCPRequestHandler(SocketServer.BaseRequestHandler):

    STATEMENT_TERMINATOR = ';'

    def __init__(self, request, client_address, server):
        self._queue = Queue.Queue()
        SocketServer.BaseRequestHandler.__init__(self, request,
                                                 client_address,
                                                 server)

    def handle(self):
        logging.debug('Handle')
        ST = TCPRequestHandler.STATEMENT_TERMINATOR
        
        received_data = []
        v = Parser.Parser()

        while True:
            try:
                b = self.request.recv(1)
                received_data.append(b)
                if cmp(b, ST) == 0:
                    received_command = ''.join(received_data).strip()
                    if received_command == 'exit;':
                        break

                    try:
                        parsed = v.parse(received_command)
                        logging.debug('Parsed: %s'%parsed)
                        if parsed['name'] not in Command.COMMANDS:
                            self.request.sendall('Invalid command.')
                        else:
                            if cmp(parsed['type'], 'QRY') == 0:
                                handler = 'handle_' + parsed['name'] + '_query'
                            elif cmp(parsed['type'], 'CMD') == 0:
                                handler = 'handle_' + parsed['name'] + '_command'
                            else:
                                print 'Invalid command type'
                                raise Exception()
                            
                            h = TCPRequestHandler.__dict__[handler]\
                                .__get__(self, TCPRequestHandler)
                            r = h(parsed)
                            self.request.sendall(r)
                    except Exception, e:
                        self.request.sendall('Error parsing command!')
                        logging.debug('Parser errror: %s'%e)

                    received_data = []
            except socket.timeout, e:
                received_data = []
                logging.error('handle: Received timeout')
            except Exception, e:
                received_data = []
                logging.error('handle: Other exception: %s'%e)

#    def handle_disk_info_query(self, p):
#        logging.debug('handle_disk_info_query')
#        q = Command.DiskInfoQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        r = Response.DiskInfo(return_code='0',
#                           dimino6_return_code='0',
#                           type='type',
#                           list=[])
#        return str(r)
#
#    def handle_error_query(self, p):
#        logging.debug('handle_error_query')
#        q = Command.ErrorQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        r = Response.error(return_code='0',
#                        dimino6_return_code='0',
#                        dimino6_error_message='There was an error.',
#                        list=[])
#        return str(r)

    def handle_input_stream_query(self, p):
        logging.info('handle_input_stream_query')
        q = Command.InputStreamQuery(p)
        s = State.get_input_stream(q['stream_label'])
        l = []
        for e in s:
            l.append([e['stream_label'],
                      e['data_format'],
                      e['interface_id'],
                      e['filter_address']])
        
        r = Response.GetInputStream(return_code='0',
                                 dimino6_return_code='0',
                                 list=l)
        return str(r)
            
    def handle_input_stream_command(self, p):
        logging.debug('handle_input_stream_command')
        c = Command.InputStreamCommand(p)
        
        if c['action'] == 'add':
            State.add_input_stream(c['stream_label'],
                                   c['data_format'],
                                   c['interface_id'],
                                   c['filter_address'])            
        elif c['action'] == 'dismount':
            State.dismount_input_stream(c['stream_label'])
        else:
            print 'Invalid input_stream_command action'
        
        r = Response.SetInputStream(return_code='0', dimino6_return_code='0', list=[])
        return str(r)


#    def handle_mod_init_query(self, p):
#        logging.debug('handle_mod_init_query')
#        q = Command.ModInitQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        # TODO: translate response to Response.
#        r = Response.GetModInit(return_code='0', list=[])
#        return str(r)
#
#    def handle_mod_init_command(self, p):
#        logging.debug('handle_mod_init_command')
#        c = Command.ModInitCommand(p)
#        logging.debug('params: %s'%c.parsed())
#        # TODO: translate response to Response.
#        r = Response.SetModInit(return_code='0',
#                             dimino6_return_code='0',
#                             list=[])
#        return str(r)
#
#    def handle_record_query(self, p):
#        logging.debug('handle_record_query')
#        q = Command.RecordQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        x3c = X3CC.GetRecordStatus()
#        resp = self.send_x3c_cmd(x3c)
#        logging.debug('RESPONSE: %s'%resp)
#        params = resp['params']
#        # TODO: fill in parameters.
#        r = Response.GetRecord(return_code=resp['retval'],
#                            dimino6_return_code='0',
#                            status='NI',
#                            scan_number='NI',
#                            scan_name=params['scanName']['value'],
#                            list=[])
#        return str(r)

    def handle_record_command(self, p):
        logging.info('handle_record_command')
        c = Command.RecordCommand(p)
        logging.info('params: %s'%c.__dict__)
        
        if c['action'] == 'on':
            State.on_record_session(c['start_time'],
                                    c['duration'],
                                    c['data_size'],
                                    c['scan_name'],
                                    c['experiment_name'],
                                    c['station_code'])
        elif c['action'] == 'off':
            State.off_record_session(c)
        else:
            print 'Invalid record action'
        
        # TODO: fill in parameters.
        r = Response.SetRecord(return_code='0',
                            dimino6_return_code='0',
                            list=[])
        return str(r)

#    def handle_rtime_query(self, p):
#        logging.debug('handle_rtime_query')
#        q = Command.RtimeQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        P = q.parsed()
#        x3c = X3CC.GetVolumeUsage(volumeName=P['volref'],
#                                  recordingSpeed=P['data_rate'])
#        resp = self.send_x3c_cmd(x3c)
#        logging.debug('RESPONSE: %s'%resp)
#        params = resp['params']
#        # TODO: fill in parameters.
#        r = Response.Rtime(return_code=resp['retval'],
#                        dimino6_return_code='0',
#                        volref='NI',
#                        data_rate='NI',
#                        remaining_time=params['freeTime']['value'],
#                        remaining_gb=params['freeSpace']['value'],
#                        remaining_percent='NI',
#                        list=[])
#        return str(r)

#    def handle_scan_check_command(self, p):
#        logging.debug('handle_scan_check_command')
#        c = Command.ScanCheckCommand(p)
#        logging.debug('params: %s'%c.parsed())
#        # TODO: translate response to Response.
#        r = Response.ScanCheck(return_code='0',
#                            dimino6_return_code='0',
#                            volref='0',
#                            scan_num='100',
#                            scan_label='4',
#                            num_streams='5',
#                            list=[])
#        return str(r)
#
#    def handle_scan_info_query(self, p):
#        logging.debug('handle_scan_info_query')
#        q = Command.ScanInfoQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        # TODO: translate response to Response.
#        r = Response.ScanInfo(return_code='0',
#                           dimino6_return_code='0',
#                           volref='0',
#                           mark6_sn='M60001',
#                           scan_number='1',
#                           scan_label='SCAN0001',
#                           status='active',
#                           start_time='00:00:00',
#                           duration='2',
#                           num_data_streams='2',
#                           list=[])
#        return str(r)
#
#    def handle_status_query(self, p):
#        logging.info('handle_status_query')
#        q = Command.StatusQuery(p)
#        logging.info('params: %s'%q.parsed())
#        x3c = X3CC.GetSystemStatus()
#        resp = self.send_x3c_cmd(x3c)
#        logging.debug('RESPONSE: %s'%resp)
#        params = resp['params']
#        # TODO: fill in parameters.
#        r = Response.Status(return_code=resp['retval'],
#                         dimino6_return_code='0',
#                         status_word='NI',
#                         list=[])
#        return str(r)
#
#    def handle_sys_info_query(self, p):
#        logging.debug('handle_sys_info')
#        q = Command.SysInfoQuery(p)
#        logging.debug('params: %s'%q.parsed())
#        # TODO: translate response to Response.
#        r = Response.SysInfo(return_code='0',
#                          dimino6_return_code='0',
#                          system_type='Mark6',
#                          mark6_sn='M600001',
#                          os_type_rev='3',
#                          dimino6_version_number='2',
#                          command_set_revision='4',
#                          available_ram='3',
#                          num_data_disks_supported='5',
#                          num_ethernet_input_ports='4',
#                          list=[])
#        return str(r)
#
#
#    def handle_vol_cmd_command(self, p):
#        logging.debug('handle_vol_cmd_command')
#        c = Command.VolCmdCommand(p)
#        logging.debug('params: %s'%c.parsed())
#        # TODO: translate response to Response.
#        r = Response.SetVolCmd(return_code='0',
#                        dimino6_return_code='0',
#                        status_word='0x00',
#                        list=[])
#        return str(r)
#
#    # TODO: check if GetVolCmd is supported by VSIS.
#
#    def handle_vol_stack_query(self, p):
#        logging.debug('handle_vol_stack_query')
#        q = Command.VolStackQuery(p)
#        logging.debug('params: %s'%q.parsed())            
#        # TODO: translate response to Response.
#        r = Response.VolStack(return_code='0',
#                           dimino6_return_code='0',
#                           list=[])
#        return str(r)
#
#    def handle_vsm_query(self, p):
#        logging.debug('handle_vsm_query')
#        q = Command.VSMQuery(p)
#        logging.debug('params: %s'%q.parsed())            
#        # TODO: translate response to Response.
#        r = Response.GetVSM(return_code='0',
#                         dimino6_return_code='0',
#                         list=[])
#        return str(r)
#
#    def handle_vsm_command(self, p):
#        logging.debug('handle_vsm_command')
#        c = Command.VSMCommand(p)
#        logging.debug('params: %s'%c.parsed())
#        # TODO: translate response to Response.
#        r = Response.SetVSM(return_code='0',
#                         dimino6_return_code='0',
#                         list=[])
#        return str(r)
#
#    def handle_vsm_mask_query(self, p):
#        logging.debug('handle_vsm_mask_query')
#        q = Command.VSMMaskQuery(p)
#        logging.debug('params: %s'%q.parsed())            
#        # TODO: translate response to Response.
#        r = Response.GetVSMMask(return_code='0',
#                             dimino6_return_code='0',
#                             erase_mask_enable='True',
#                             play_mask_enable='False',
#                             record_mask_enable='False',
#                             list=[])
#        return str(r)
#
#    def handle_vsm_mask_command(self, p):
#        logging.debug('handle_vsm_mask_command')
#        c = Command.VSMMaskCommand(p)
#        logging.debug('params: %s'%c.parsed())
#        # TODO: translate response to Response.
#        r = Response.SetVSMMask(return_code='0',
#                             dimino6_return_code='0',
#                             erase_mask_enable='True',
#                             play_mask_enable='False',
#                             record_mask_enable='False',
#                             list=[])
#        return str(r)
