check_PROGRAMS	=	commontest

commontest_SOURCES=	test_main.cc \
			disk2vlbi.cc \
			disk2vlbi.h \
			file_writer.h \
			file_writer.cc \
			net_reader.h \
			net_reader.cc \
			pfile_writer.h \
			pfile_writer.cc \
			queue.h \
			scan_check.h \
			scan_check.cc \
			stats_writer.h \
			stats_writer.cc \
			thread_pool.h \
			thread_pool.cc \
			threaded.h \
			writer_task.h \
			test_file_writer.h \
			test_file_writer.cc \
			test_pfile_writer.h \
			test_pfile_writer.cc \
			test_thread_pool.h \
			test_thread_pool.cc \
			test_writer_thread_pool.h \
			test_writer_thread_pool.cc 

commontest_LDADD= 	-L. \
			-lcommon \
			-L@BOOST_LIBDIR@ \
			-lboost_program_options-mt \
			-lboost_system-mt \
			-lboost_thread-mt \
			-lm \
			-lpthread \
			@PFRING_ROOT@/userland/libpcap-1.1.1-ring/libpcap.a \
			@PFRING_ROOT@/userland/lib/libpfring.a

noinst_LIBRARIES=	libcommon.a

libcommon_a_SOURCES	= \
			disk2vlbi.cc \
			disk2vlbi.h \
			pfr.h \
			pfr.cc \
			file_writer.h \
			file_writer.cc \
			logger.h \
			logger.cc \
			net_reader.h \
			net_reader.cc \
			pfile_writer.h \
			pfile_writer.cc \
			queue.h \
			scan_check.h \
			scan_check.cc \
			stats_writer.h \
			stats_writer.cc \
			thread_pool.h \
			thread_pool.cc \
			threaded.h \
			writer_task.h

INCLUDES	= 	-I@top_srcdir@/src/common \
			-I/usr/include \
			-I@PFRING_ROOT@/kernel \
			-I@PFRING_ROOT@/kernel/plugins \
			-I@PFRING_ROOT@/userland/lib \
			-I@PFRING_ROOT@/userland/libpcap-1.1.1-ring \
			-I@BOOST_INCDIR@

AM_CXXFLAGS	= 	-O4

TESTS_ENVIRONMENT=	LD_LIBRARY_PATH=/usr/local/lib:/usr/lib64/boost
TESTS		=	commontest
