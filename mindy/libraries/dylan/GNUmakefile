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
##  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/Attic/GNUmakefile,v 1.6 1994/05/03 12:09:21 wlott Exp $
##
##  This is the makefile for the Mindy runtime library, dylan.dbc.
##

include ../Config

MC = ../comp/mc
MINDYFLAGS = -lDylan

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = exports.dbc cmp.dbc coll.dbc array.dbc vec.dbc string.dbc list.dbc \
	deque.dbc stretchy.dbc table.dbc sort.dbc cond.dbc num.dbc char.dbc \
	func.dbc misc.dbc debug.dbc exit.dbc

dylan.dbc: ${OBJS}
	cat ${OBJS} > $@

clean:
	rm -f ${OBJS} dylan.dbc
