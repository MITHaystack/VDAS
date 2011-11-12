'''
Created on Oct 3, 2011

@author: dlapsley
'''

import pprint

input_streams = dict()
record_session = dict()


def add_input_stream(stream_label, data_format, interface_id, filter_address=None):
    global input_streams

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
    
def on_record_session(start_time, duration, data_size, scan_name, experiment_name,
                      station_code):
    global record_session

    record_session['start_time'] = start_time
    record_session['duration'] = duration
    record_session['data_size'] = data_size
    record_session['scan_name'] = scan_name
    record_session['experiment_name'] = experiment_name
    record_session['station_code'] = station_code

def off_record_session():
    global record_session
    
    record_session = dict()
    
def get_record_session():
    return record_session


