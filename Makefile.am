MAINTAINERCLEANFILES	=	Makefile.in
SUBDIRS			=	src doc
dist_bin_SCRIPTS	=	bin/mark6 \
				bin/tstmark6 \
				src/vdif/bin/vdif \
				tools/disk/disktool \
				tools/disk/fio  \
				tools/disk/condition-modules \
				tools/disk/mount-modules \
				tools/disk/unmount-modules

sysconf_DATA		=	tools/disk/disktool.fio \
				tools/disk/disktool.rc \
				rc.mark6 \
				tests/r1504/rc.mark6.gg \
				tests/r1504/rc.mark6.wf \
				tests/r1504/r1504-gg.xml \
				tests/r1504/r1504-wf.xml 
logdir 			= 	log
DISTCHECK_CONFIGURE_FLAGS=\
				BOOST_LIBDIR=/usr/lib64/boost141 \
				BOOST_INCDIR=/usr/include/boost141

mark6logdir		=	${prefix}/log
mark6log_DATA		=	log/.dummy
EXTRA_DIST		=	setup ${mark6log_DATA}


distclean-local:
	-rm -rf Makefile.in \
		aclocal.m4 \
		autom4te.cache \
		config \
		config.h \
		config.h.in \
		config.log \
		config.status \
		stamp-h1 \
		src/Makefile \
		src/Makefile.in 
