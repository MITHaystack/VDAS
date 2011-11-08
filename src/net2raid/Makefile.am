bin_PROGRAMS	=	net2raid
check_PROGRAMS	=	net2raidtest
dist_bin_SCRIPTS=	net2raid-run \
			net2raid \
			net2raid-graphs
sysconf_DATA	=	net2raid-log.cfg

net2raid_SOURCES= 	main.cc
net2raid_LDADD	= 	-L. \
			-L../common \
			-lcommon \
			-lreadline \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a

net2raidtest_SOURCES=	test_main.cc

net2raidtest_LDADD= 	-L. \
			-L../common \
			-lcommon
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a

INCLUDES	= 	-I../common \
			-I@top_srcdir@/src/net2raid \
			-I/usr/include \
			-I@PFRING_ROOT@/kernel \
			-I@PFRING_ROOT@/kernel/plugins \
			-I@PFRING_ROOT@/userland/lib \
			-I@PFRING_ROOT@/userland/libpcap-1.1.1-ring \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
TESTS		=	net2raidtest
