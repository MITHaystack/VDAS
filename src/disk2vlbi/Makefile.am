bin_PROGRAMS	=	disk2vlbi
dist_bin_SCRIPTS=	disk2vlbi

disk2vlbi_SOURCES	= 	main.cc
disk2vlbi_LDADD	= 	-L../common \
			-lcommon \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a

INCLUDES	= 	-I/usr/include  \
			-I../common \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

