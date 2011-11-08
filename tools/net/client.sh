#!/bin/bash

IPERF=/usr/bin/iperf
NUTTCP=/usr/bin/nuttcp
PING=/bin/ping

DEST_IP=10.0.0.1
PORT=6242
# Sends 32K dgrams.
# WRITE_BUFFER_SIZE=32K
WRITE_BUFFER_SIZE=8k
# BANDWIDTH=1K
BANDWIDTH=4g
THREADS=1
TIME=10
INTERVAL=1

${PING} -c 1 ${DEST_IP}


${NUTTCP} -t -u -v \
	-l ${WRITE_BUFFER_SIZE} \
	-R${BANDWIDTH} \
	-T${TIME} \
	-P5000 \
	10.0.0.1

	#-p${PORT} \


# for DEST_IP in 10.0.0.1
# for DEST_IP in 10.0.0.1 10.0.0.5
# for DEST_IP in 10.0.0.1 10.0.0.5 10.0.0.9
#for DEST_IP in 10.0.0.1 10.0.0.5 10.0.0.9 10.0.0.13
#do
	#echo ${IPERF} -c ${DEST_IP} -u -p ${PORT}
	#${IPERF} -c ${DEST_IP} -u \
		#-p ${PORT} \
		#-l ${WRITE_BUFFER_SIZE} \
		#-b ${BANDWIDTH}  \
		#-P ${THREADS} \
		#-t ${TIME} \
		#-i ${INTERVAL} &
#done

#sleep 2
#
#for DEST_IP in 10.0.0.1 10.0.0.5 10.0.0.9 10.0.0.13
#do
	#ping -c 1 ${DEST_IP}
#done
