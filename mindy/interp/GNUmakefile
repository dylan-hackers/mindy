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
##  $Header: /home/housel/work/rcs/gd/src/mindy/interp/Attic/GNUmakefile,v 1.2 1994/03/26 07:45:51 wlott Exp $
##
##  This is the makefile for the Mindy byte-interpreter.
##

include ../Config

OBJS = bool.o class.o coll.o debug.o def.o error.o func.o gc.o init.o \
	interp.o list.o lose.o mindy.o misc.o module.o nlx.o num.o \
	obj.o print.o str.o sym.o thread.o type.o load.o value.o vec.o \
	char.o lexer.o parser.tab.o handler.o instance.o input.o
SRCS = $(patsubst %.o,%.c,$(OBJS))

mindy: $(OBJS)
	$(CC) -o ,mindy $(CFLAGS) $(OBJS)
	mv -f ,mindy mindy

lexer.c: lexer.l

parser.tab.c: parser.y
	bison -d $< -o $@

parser.tab.h: parser.tab.c

clean:
	rm -f ${OBJS} parser.tab.c parser.tab.h lexer.c mindy Depends


depend: ${SRCS}
	$(CC) -MM -E ${CFLAGS} ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends

ifndef IGNORE_DEPENDS

Depends:
	$(MAKE) depend IGNORE_DEPENDS=SET

include Depends

endif
