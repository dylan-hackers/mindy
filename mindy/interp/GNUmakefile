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
##  $Header: /home/housel/work/rcs/gd/src/mindy/interp/Attic/GNUmakefile,v 1.13 1994/06/27 16:31:22 wlott Exp $
##
##  This is the makefile for the Mindy byte-interpreter.
##

include ../Config

CPPFLAGS = -I.

OBJS = bool.o class.o coll.o debug.o def.o error.o func.o gc.o init.o \
	interp.o list.o lose.o mindy.o misc.o module.o nlx.o num.o \
	obj.o print.o str.o sym.o thread.o type.o load.o value.o vec.o \
	char.o lexer.o parser.tab.o handler.o instance.o input.o driver.o \
	table.o fd.o buf.o weak.o brkpt.o
SRCS = $(patsubst %.o,%.c,$(OBJS))

LDLIBS = -lfl -lm

mindy: $(OBJS)
	$(LINK.o) $^ $(LOADLIBES) $(LDLIBS) -o ,mindy
	mv -f ,mindy mindy

lexer.c: lexer.l

lexer.o: lexer.c
	$(subst -Wall ,,$(COMPILE.c)) $< $(OUTPUT_OPTION)

parser.tab.c: parser.y
	bison -d $< -o $@

parser.tab.h: parser.tab.c

parser.tab.o: parser.tab.c
	$(subst -Wall ,,$(COMPILE.c)) $< $(OUTPUT_OPTION)

clean:
	rm -f ${OBJS} parser.tab.c parser.tab.h lexer.c mindy Depends


depend: ${SRCS}
	$(CC) -MM -E ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends

ifndef IGNORE_DEPENDS

Depends:
	$(MAKE) depend IGNORE_DEPENDS=SET

include Depends

endif
