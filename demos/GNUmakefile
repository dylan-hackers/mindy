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
##  $Header: /home/housel/work/rcs/gd/src/demos/Attic/GNUmakefile,v 1.1 1994/06/29 00:03:10 wlott Exp $
##

SUBDIRS = hello-world cat

all: ${SUBDIRS}

.PHONY: ${SUBDIRS}

${SUBDIRS}:
	$(MAKE) -C $@

clean:
	$(foreach subdir,$(SUBDIRS), $(MAKE) -C $(subdir) clean; )
