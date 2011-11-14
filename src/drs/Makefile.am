MAINTAINERCLEANFILES		=	Makefile.in

PYTHON_TOOLS				=	drs-server drs-client

${PYTHON_TOOLS}:	python_wrapper
	cp $< $@

dist_bin_SCRIPTS			=	${PYTHON_TOOLS}

pythonlibdir				=	$(libdir)/python/mark6
pythonlib_DATA				=	src/Client.py\
								src/Command.py\
								src/Parser.py\
								src/Response.py\
								src/Server.py\
								src/State.py\
								src/TCPRequestHandler.py\
								src/Utils.py


pythonlib_SCRIPTS			=	src/drs-server.py \
								src/drs-client.py


distclean-local:
	-rm -rf Makefile.in\
		python_wraper \
		drs-client\
		drs-server
