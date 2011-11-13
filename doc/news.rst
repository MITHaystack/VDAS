Release Notes
=============

Release 0.1	7/5/2011
--------------------

David Lapsley<del@haystack.mit.edu>

DESCRIPTION
~~~~~~~~~~~

First internal release. End to end data path, supporting infrastructure.

FEATURES
~~~~~~~~

- Incorporated PF_RING code and module to get 8 Gbps NIC to Drive throughput.
- Implemented continuous integration and unit testing framework.
- Main program with options parsing.
- Support modules including:

	- BufferManager
	- ThreadPool
	- SocketManager
	- WriterTask

Release 0.2	7/7/2011
--------------------

David Lapsley<del@haystack.mit.edu>

DESCRIPTION
~~~~~~~~~~~

Major performance milestone.

Using a a single 10 GE network interface, and a single disk pack 
(with only 7 disks, one has failed), able to get ~ 7.6 Gbps net
to disk throughput (with 3% packet loss and ~95% utilization of a
single CPU ~ out of 16 total CPUs). Throughput on both transmit
and receive checked, and volume of data written over test interval
checked. Test duraiton was 30 seconds.

FEATURES
~~~~~~~~

- Ported code to debian squeeze (6.0.2) with 2.6.32 kernel.
- Removed source dependencies in favor of binary debian packages.
- Incorporated PF_RING module source into build tree.
- Upgraded "setup" build script. Includes more user accessible
  functionality. Also automatically installs all packages required
  to create a build/development machine from scratch.
- net2disk module now fully based on PF_RING architecture.
- Added tools subdirectory with network and disk configuration scripts.
  Disk configuration scripts include optimizations that disable
  journaling on ext4 file systems and enable driver level throughput
  optimizations.

Release 0.4	7/15/2011
---------------------

David Lapsley<del@haystack.mit.edu>

DESCRIPTION
~~~~~~~~~~~

Major performance milestone. Application software achieves maximum
network to disk throughput supported by hardware platform and drivers:
14.1 Gbps sustained for 60 seconds (still room to optimize platform
configuration). This was using 4 x 8-disk modules. Each module
configured as a separate RAID-0 array.

FEATURES
~~~~~~~~

- Finalized application architecture (multi-process architecture
  rather than mult-threaded)
- Optimized PF_RING driver parameters
- Optimized Myricom driver settings
- Optimized IRQ SMP processor affinity for LSI MegaRAID and 
  Myricom Ethernet devices.
- Optimized application process SMP processor affinity.
- Optimized application buffer sizes, and data path.
- Optimized network packet capture code.
- Optimized internal buffering code.
- Optimized disk writing code.
- Application includes the following major classes:

	  - NetReader: Reads data from PF_RING buffers and fills BufferPool
		  allocated buffers with the data.
	  - BufferPool: Internal buffers optimally sized and aligned for
		  Direct Disk IO. Includes optimized memory management.
	  - FileWriter: Manages memory to disk data transfer.
	  - StatsWriter: Instrumentation class. Periodically logs network and
		  disk performance statistics.

- Interactive command line interface that allows user to launch and
  stop capture threads in a convenient manner.
- Startup scripts that automatically set MegaRAID, Myricom, and
  Application processor affinities at startup.
- Disk module conditioning scripts that create and configure RAID
  arrays, label and partition virtual drives, create EXT4 file systems,
  and optimize file system parameters.

Release 0.4.1	9/20/2011
-----------------------

David Lapsley<del@haystack.mit.edu>

DESCRIPTION
~~~~~~~~~~~

Added control plane and client to the build. Extended build system
to do complete build and install.

FEATURES
~~~~~~~~

- Incorporated mark6 control plane code (inherited from previous
  dimino6 work).
- Incorporated tstmark6 control plane client.
- Extended build system to do full build and install in
  /opt/haystack/mark6.

Release 0.4.2	9/28/2011
-----------------------

David Lapsley<del@haystack.mit.edu>

DESCRIPTION
~~~~~~~~~~~

Validated 16 Gbps data capture functionality. Recording full
ethernet frames for robust on-disk framing.

FEATURES
~~~~~~~~

- Full ethernet frame capture for robust on-disk framing and ease of
  manipulation. Data capture files stored in standard "pcap" format
  to facilitate manipulation with tcpdump, editcap, and other pcap
  based applications.
- Full application installation (binaries, scripts, configuration,
  and logs) into /opt/mit/mark6 directory.
- Addition of disk2vlbi tool for converting captured pcap file format
  to native VLBI format (i.e. vdif, m5b).
- Migration of BufferPool functionality into FileWriter class to
  simplify/robustify code.
- Removal of BufferPool class.
- Addition of blocking semantics to disk buffer allocation (through
  FileWriter::buffer_malloc() method.
- Enhanced instrumentation (buffer level and packet drop metrics added).
- Automatic ethernet IRQ sensing in net2disk-run script.
- Incorporation of disk tools into distributed application (eventually will
  be integrated into control plane as conditioning/test scripts).

Release 0.5.0	10/24/2011
------------------------

David Lapsley<del@haystack.mit.edu>

DESCRIPTION
~~~~~~~~~~~

- Version to be used for 16 Gbps demonstration.

FEATURES
~~~~~~~~

- To be listed in official release.
