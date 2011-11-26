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

This module implements the operational state of the Mark6. It is not
persistent, but only exists while the DRS Server software is running.

State changes are made in respones to commands from the VSI-S command
interface.
'''

import pprint
import thread
import subprocess
import time

MARK6_EXEC=os.environ.get('MARK6_EXEC', None)
POLLING_INTERVAL=0.1

input_streams = dict()
record_session = dict()


def add_input_stream(stream_label, data_format, interface_id,
    filter_address):
    global input_streams

    if filter_address is None:
        filter_address = ''

    input_streams[stream_label] = {
                                   'stream_label': stream_label,
                                   'data_format':  data_format,
                                   'interface_id': interface_id,
                                   'filter_address':   filter_address
                                   }

def dismount_input_stream(stream_label):
    global input_streams
    
    del input_streams[stream_label]

def get_input_stream(stream_label=None):
    global input_streams
    
    if stream_label:
        return [ input_streams.get(stream_label, None) ]
    return input_streams.values()

def dump():
    global input_streams
    
    p = pprint.PrettyPrinter()
    p.pprint(input_streams)

def record(record_session):
    end_time = record_session['start_time'] + record_session['duration']
    if time.time() > end_time:
        raise Exception('late scan: %s'%record_session)

    while time.time() < record_session['start_time']:
        time.sleep(POLLING_INTERVAL)

    start_time = time.time()
    args = [
            MARK6_EXEC, record_session['scan_name'], record_session['duration']]
    p = subprocess.Popen(args)
    p.wait()

    
def on_record_session(start_time, duration, data_size, scan_name,
    experiment_name, station_code):
    global record_session

    record_session['start_time'] = start_time
    record_session['duration'] = duration
    record_session['data_size'] = data_size
    record_session['scan_name'] = scan_name
    record_session['experiment_name'] = experiment_name
    record_session['station_code'] = station_code

    thread.start_new_thread(record, record_session)

def off_record_session():
    global record_session
    
    record_session = dict()
    
def get_record_session():
    return record_session


