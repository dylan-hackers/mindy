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
##  $Header: /home/housel/work/rcs/gd/src/mindy/Attic/GNUmakefile,v 1.3 1994/06/11 16:14:41 wlott Exp $
##
##  This is the top-level source makefile for Mindy.  It lives at the top
## of the Mindy source tree, and recursivly runs make on each of the
## subdirectoies.
##

CSUBDIRS = interp comp
DYLANSUBDIRS = runtime streams random
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

${DYLANSUBDIRS}: comp
