Mark6 VLBI Data Acquisition System (VDAS)
=========================================

The Mark6 VLBI Data Acquisition System is the next generation, high speed
VLBI Data Acquisition System (DAS) jointly developed by
`MIT Haystack Observatory <http://www.haystack.mit.edu/>`_ and
`NASA Goddard Space Flight Center High End
Computer Network Team <http://science.gsfc.nasa.gov/606.1/HECN.html>`_.

Features
--------

- Open-source software implementation (C/C++ and Python) released under the
  GPLv2. license.
- Open hardware platform based on inexpensive, high-performance
  COTS hardware
- 16Gbps sustained record and playback capability
- Easily upgradeable record/playback capability as COTS hardware becomes more
  capable (32-Gbps capable hardware currently being evaluated)
- General Ethernet packet recorder ĕasily adapted to other interfaces as well)
- Easily upgradeable on Moore's Law curve
- Debian Linux OS
- Record and playback as standard Linux files
- VLBI Disk-Adaptive File System (VDAFS, or "VDAF" for short)
- Smooth transition from Mark 5
- Leverages decade of operational experience with Mark5 system to provide a
  simpler, easier to maintain, and more robust system.

Overview
--------

Figure 1 provides a high level overview of the Mark6 System. The Mark6 system
consists of two logical components: (1) a 4U Controller, which contains
motherboard, CPU, RAM, RAID controllers, etc. and (2) one or more 8-disk
Modules, which is connected to the controller via SAS2, SFF-8088 connectors. The
Controller runs the Linux Operating System, and includes the control software
required to control the Mark6, as well as the data plane software required to
transfer data into and out of the system. 

.. figure:: http://dl.dropbox.com/u/18326850/Mark6/mark6-architecture.png
  :align: center
  :scale: 100%

  **Figure 1:** VLBI data are input/output to the system via one ore more
  10 Gbps Ethernet interfaces and stored/retrieved to/from one or more
  8-pack disk modules. Commands and responses are exchanged via a
  100 Mbps/1 Gbps Ethernet interface using either a VSI-S or XML-based
  machine-to-machine protocol.

The Mark6 is capable of operating in one of two modes:

1. **Data Input Module (DIM):** data are recorded from network interfaces onto
   disk.

2. **Data Output Module (DOM):** data are read from disk and played out over
  network interface.

Data are input and output to the system via one or more 10 Gigabit
Ethernet interfaces and is stored on one or more disk modules. Each
disk module comprises 8 SATA disks.

