######################################################################
##
##  Copyright (C) 1994, Carnegie Mellon University
##  All rights reserved.
##
##  This code was produced by the Gwydion Project at Carnegie Mellon
##  University.  If you are interested in using this code, contact
##  "Scott.Fahlman@cs.cmu.edu" (Internet).
##
######################################################################
##
##  $Header: /home/housel/work/rcs/gd/src/mindy/comp/Attic/GNUmakefile,v 1.3 1994/03/31 10:14:48 wlott Exp $
##
##  This is the makefile for the Mindy byte-compiler.
##

include ../Config

LFLAGS = -i
LDLIBS = -lfl

OBJS = mindycomp.o parser.tab.o lexer.o src.o sym.o print.o expand.o \
	literal.o info.o lose.o lexenv.o envanal.o compile.o header.o \
	dump.o

SRCS = $(patsubst %.o,%.c,$(OBJS))


mc: ${OBJS}
ifdef VPATH
	${COMPILE.c} -DVERSION=\""`/bin/date`"\" $(VPATH)/version.c
else
	${COMPILE.c} -DVERSION=\""`/bin/date`"\" version.c
endif
	$(LINK.o) $^ version.o $(LOADLIBES) $(LDLIBS) -o ,$@
	mv ,$@ $@


parser.tab.c: parser.y
	bison -d $< -o $@

parser.tab.h: parser.tab.c

lexer.c: lexer.l

clean:
	rm -f ${OBJS} version.o parser.tab.c parser.tab.h lexer.c mc Depends

depend: ${SRCS}
	$(CC) -MM -E ${CFLAGS} ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends


ifndef IGNORE_DEPENDS

Depends:
	$(MAKE) depend IGNORE_DEPENDS=SET

include Depends

endif
