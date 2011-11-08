#!/bin/bash

IPERF=/usr/bin/iperf
NUTTCP=/usr/bin/nuttcp
PING=/bin/ping

PORT=4242
WRITE_BUFFER_SIZE=32K
BANDWIDTH=4G
THREADS=1
TIME=30
INTERVAL=1

# ${NUTTCP} -S -P5000

# ${PING} -c 1 ${DEST_IP}

echo ${IPERF} -c ${DEST_IP} -u -p ${PORT}
${IPERF} -s -u \
	-p ${PORT} \
	-l ${WRITE_BUFFER_SIZE} \
	-t ${TIME} \
	-i ${INTERVAL}
	# -B ${INTERFACE}

