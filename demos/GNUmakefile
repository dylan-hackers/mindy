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
##  $Header: /home/housel/work/rcs/gd/src/demos/Attic/GNUmakefile,v 1.2 1994/06/29 00:09:48 wlott Exp $
##

SUBDIRS = hello-world cat html2txt

all: ${SUBDIRS}

.PHONY: ${SUBDIRS}

${SUBDIRS}:
	$(MAKE) -C $@

clean:
	$(foreach subdir,$(SUBDIRS), $(MAKE) -C $(subdir) clean; )
