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
##  $Header: /home/housel/work/rcs/gd/src/mindy/Attic/GNUmakefile,v 1.4 1994/06/29 03:33:08 wlott Exp $
##
##  This is the top-level source makefile for Mindy.  It lives at the top
## of the Mindy source tree, and recursivly runs make on each of the
## subdirectoies.
##

include Config

CSUBDIRS = interp comp
DYLANSUBDIRS = libraries demos
SUBDIRS = ${CSUBDIRS} ${DYLANSUBDIRS}

all: ${SUBDIRS}

.PHONY: ${SUBDIRS}

${SUBDIRS}:
	$(MAKE) -C $@

depend:
	$(foreach subdir,$(CSUBDIRS), \
		$(MAKE) -C $(subdir) depend IGNORE_DEPENDS=SET;)

clean:
	$(foreach subdir,$(SUBDIRS), \
		$(MAKE) -C $(subdir) clean IGNORE_DEPENDS=SET;)

${DESTDIR}/bin:
	mkdir ${DESTDIR}/bin

install: ${DESTDIR}/bin
	$(foreach subdir,$(SUBDIRS), \
		$(MAKE) -C $(subdir) install;)

${DYLANSUBDIRS}: comp
