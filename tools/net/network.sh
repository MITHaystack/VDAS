#!/bin/bash

IFCONFIG=/sbin/ifconfig
INSMOD=/sbin/insmod

${IFCONFIG} eth0 down
${IFCONFIG} eth2 down
${IFCONFIG} eth3 down
${IFCONFIG} eth5 down

PF_RING_ROOT=/home/dlapsley/mark6/src/extern/PF_RING-4.7.0
PF_RING_MODULE=${PF_RING_ROOT}/kernel/pf_ring.ko
# ${INSMOD} ${PF_RING_MODULE} transparent_mode=1

${IFCONFIG} eth3 192.168.7.1 broadcast 192.168.7.3 netmask 255.255.255.252 mtu 9000
${IFCONFIG} eth3 up

${IFCONFIG} eth0 192.168.7.5 broadcast 192.168.7.7 netmask 255.255.255.252 mtu 9000
${IFCONFIG} eth0 up

# /0/100/1/0         eth1        network    82576 Gigabit Network Connection
# /0/100/1/0.1       eth4        network    82576 Gigabit Network Connection

# /0/100/5/0         eth0        network    82598EB 10-Gigabit AT CX4 Network Connection
# /0/100/5/0.1       eth2        network    82598EB 10-Gigabit AT CX4 Network Connection
# /0/100/7/0         eth3        network    82598EB 10-Gigabit AT CX4 Network Connection
# /0/100/7/0.1       eth5        network    82598EB 10-Gigabit AT CX4 Network Connection

