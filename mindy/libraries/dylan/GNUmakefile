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
##  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/Attic/GNUmakefile,v 1.10 1994/06/27 17:10:13 wlott Exp $
##
##  This is the makefile for the Mindy runtime library, dylan.dbc.
##

include ../Config

MC = ../comp/mc
MINDYFLAGS = -lDylan

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = exports.dbc cmp.dbc coll.dbc array.dbc vec.dbc string.dbc list.dbc \
	deque.dbc range.dbc stretchy.dbc table.dbc sort.dbc cond.dbc \
	num.dbc char.dbc func.dbc misc.dbc debug.dbc exit.dbc ext.dbc \
	multilock.dbc semaphore.dbc

dylan.dbc: ${OBJS}
	cat ${OBJS} > $@

clean:
	rm -f ${OBJS} dylan.dbc
