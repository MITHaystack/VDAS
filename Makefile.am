MAINTAINERCLEANFILES		=	Makefile.in
SUBDIRS									=	src doc
dist_bin_SCRIPTS				=	tools/disk/disktool \
													tools/disk/fio  \
													tools/disk/condition-modules \
													tools/disk/mount-modules \
													tools/disk/unmount-modules

PYTHON_TOOLS						= drs-server drs-client

${PYTHON_TOOLS}:	python_wrapper
	cp $< $@

dist_bin_SCRIPTS				+=${PYTHON_TOOLS}

pythonlibdir						= $(libdir)/python/mark6
pythonlib_DATA					= src/drs/src/Command.py \
													src/drs/src/Parser.py \
													src/drs/src/Response.py \
													src/drs/src/Server.py \
													src/drs/src/ServerTest.py \
													src/drs/src/State.py \
													src/drs/src/TCPRequestHandler.py \
													src/drs/src/Utils.py

pythonlib_SCRIPTS				= src/drs/src/drs-server.py \
													src/drs/src/drs-client.py

sysconf_DATA						=	tools/disk/disktool.fio \
													tools/disk/disktool.rc \
													rc.mark6
logdir 									=	log
DISTCHECK_CONFIGURE_FLAGS=BOOST_LIBDIR=/usr/lib64/boost141 \
													BOOST_INCDIR=/usr/include/boost141

mark6logdir							=	${prefix}/log
mark6log_DATA						=	log/.dummy
EXTRA_DIST							=	setup ${mark6log_DATA}


distclean-local:
	-rm -rf Makefile.in\
		aclocal.m4\
		autom4te.cache\
		config\
		config.h\
		config.h.in\
		config.log\
		config.status\
		stamp-h1\
		drs-client\
		drs-server\
		src/Makefile\
		src/Makefile.in\
		src/common/.deps\
		src/disk2vlbi/.deps\
		src/scancheck/.deps\
		src/net2raid/.deps \
		src/common/Makefile.in \
		src/disk2vlbi/Makefile.in \
		src/extern/PF_RING-4.7.0 \
		src/net2raid/Makefile.in \
		src/scancheck/Makefile.in

