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
##  $Header: /home/housel/work/rcs/gd/src/mindy/interp/Attic/GNUmakefile,v 1.12 1994/05/19 22:32:40 wlott Exp $
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
