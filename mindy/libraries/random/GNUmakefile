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
##   $Header: /home/housel/work/rcs/gd/src/mindy/libraries/random/Attic/GNUmakefile,v 1.2 1994/06/15 19:30:12 dpierce Exp $
##
## This is the makefile for the Mindy random library, random.dbc.
##

include ../Config

MC = ../comp/mc
MINDYFLAGS = -lRandom

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = library.dbc distributions.dbc

random.dbc: ${OBJS}
	cat ${OBJS} > $@

clean:
	rm -f ${OBJS} random.dbc
