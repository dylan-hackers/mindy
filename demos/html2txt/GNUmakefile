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
##  $Header: /home/housel/work/rcs/gd/src/demos/html2txt/Attic/GNUmakefile,v 1.1 1994/06/29 00:05:16 wlott Exp $
##
##  This is the makefile for the "demo" directory
##

include ../Config

MC = ../comp/mc
MINDYFLAGS = -lhtml

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = html2txt.dbc

main: ${OBJS}

clean:
	rm -f ${OBJS}
