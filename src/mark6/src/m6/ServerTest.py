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

import logging
import socket
import time

import Server

VSIS_COMMANDS = [
#    'disk_info?info_type:volref;',
#    'error?dimino6_return_code;',
    'input_stream=add:stream_a:vdif:eth2:4505;',
    'input_stream=add:stream_b:vdif:eth3:4506;',
    'input_stream=add:stream_c:vdif:eth4:4507;',
    'input_stream=add:stream_d:vdif:eth5:4508;',
    'input_stream=add:stream_e:vdif:eth6:4509;',
    'input_stream=dismount:stream_e;',

    'input_stream?;',
    'input_stream?stream_a;',

    'record=on:starttime:duration:datasize:scanname:experimentname:stationcode;',

#    'mod_init=msn:disks;',
#    'mod_init?;',
#    'record?;',
#    'rtime?volref:datarate;',
#    'scan_check=volref:scanname;',
#    'scan_info?volref:scanname;',
#     'status?;',
#    'sys_info?;',
#    'vol_cmd=action:volref1:volref2;',
#    'vol_stack?;',
#    'VSM=volref:VSM;',
#    'VSM?volref;',
#    'VSM=erasemaskenable:playmaskenable:recordmaskenable;',
#    'VSM?;',
    ]



def client(ip, port, message):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((ip, port))
    sock.sendall(message)
    response = sock.recv(1024)
    print "CLIENT: %s" % response
    time.sleep(0.5)
    sock.close()


if __name__ == "__main__":
    ip = 'localhost'

    VSIS_PORT = 14242

    logging.basicConfig(level=logging.INFO)
    
    s = Server.Server('localhost', VSIS_PORT)
    s.start()

    for v in VSIS_COMMANDS:
        client(ip, VSIS_PORT, v)

    s.shutdown()
