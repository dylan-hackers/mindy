######################################################################
##
## Copyright (c) 1994  Carnegie Mellon University
## All rights reserved.
## 
## Use and copying of this software and preparation of derivative
## works based on this software are permitted, including commercial
## use, provided that the following conditions are observed:
## 
## 1. This copyright notice must be retained in full on any copies
##    and on appropriate parts of any derivative works.
## 2. Documentation (paper or online) accompanying any system that
##    incorporates this software, or any part of it, must acknowledge
##    the contribution of the Gwydion Project at Carnegie Mellon
##    University.
## 
## This software is made available "as is".  Neither the authors nor
## Carnegie Mellon University make any warranty about the software,
## its performance, or its conformity to any specification.
## 
## Bug reports, questions, comments, and suggestions should be sent by
## E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
##
######################################################################
##
##  $Header: /home/housel/work/rcs/gd/src/mindy/comp/Attic/GNUmakefile,v 1.8 1994/06/29 03:33:52 wlott Exp $
##
##  This is the makefile for the Mindy byte-compiler.
##

include ../Config

CPPFLAGS = -I.
LFLAGS = -i
LDLIBS = -lfl

OBJS = mindycomp.o parser.tab.o lexer.o src.o sym.o print.o expand.o \
	literal.o info.o lose.o lexenv.o envanal.o compile.o header.o \
	dump.o free.o dup.o

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

parser.tab.o: parser.tab.c
	$(subst -Wall ,,$(COMPILE.c)) $< $(OUTPUT_OPTION)

lexer.c: lexer.l

lexer.o: lexer.c
	$(subst -Wall ,,$(COMPILE.c)) $< $(OUTPUT_OPTION)

clean:
	rm -f ${OBJS} version.o parser.tab.c parser.tab.h lexer.c mc Depends

depend: ${SRCS}
	$(CC) -MM -E ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends

install: mc
	rm -f $(DESTDIR)/bin/mc
	cp mc $(DESTDIR)/bin/mc


ifndef IGNORE_DEPENDS

Depends:
	$(MAKE) depend IGNORE_DEPENDS=SET

include Depends

endif
