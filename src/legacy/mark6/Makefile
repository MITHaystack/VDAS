# Ubuntu pre-requisite packages.
# PREQ="ncurses-dev libssl-dev build-essential git-core"

# Erlang applications.


# Build tool.
REBAR=./rebar
TAR=/bin/tar
RM=/bin/rm
CP=/bin/cp
MKDIR=/bin/mkdir
CHMOD=/bin/chmod
PREFIX=/opt/mit/mark6

all:	clean generate

run:
	${MARK6_APP} console

# preq:
	# sudo /usr/bin/apt-get -y install ${PREQ}

node:
	${RM} -fr rel
	${MKDIR} rel
	cd rel; ../rebar create-node nodeid=mark6 force=1

compile:
	${REBAR} compile

generate:	compile node
	${CP} lib/mark6/src/mark6.app.src lib/mark6/ebin/mark6.app
	${CP} lib/mark6/src/reltool.config.src rel/reltool.config
	${RM} -fr rel/mark6
	${REBAR} generate force=1
	${CHMOD} u+x rel/mark6/bin/mark6

tests:
	${REBAR} eunit

install:	generate
	sudo ${CP} -r rel ${PREFIX}/lib

clean:
	${REBAR} clean
	${RM} -fr rel
	${RM} -fr lib/mark6/files
	${RM} -fr lib/mark6/reltool.config
