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
##  $Header: /home/housel/work/rcs/gd/src/demos/html2txt/Attic/GNUmakefile,v 1.3 1994/06/29 03:37:35 wlott Exp $
##
##  This is the makefile for the "demo" directory
##

include ../../Config

MC = ../../comp/mc
MINDYFLAGS = -lhtml

%.dbc: %.dylan
	$(MC) ${MINDYFLAGS} -o $@ $<

OBJS = html2txt.dbc

main: ${OBJS}

clean:
	rm -f ${OBJS}

install:
