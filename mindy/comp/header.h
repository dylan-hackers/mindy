/**********************************************************************\
*
*  Copyright (C) 1994, Carnegie Mellon University
*  All rights reserved.
*
*  This code was produced by the Gwydion Project at Carnegie Mellon
*  University.  If you are interested in using this code, contact
*  "Scott.Fahlman@cs.cmu.edu" (Internet).
*
***********************************************************************
*
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/header.h,v 1.1 1994/03/24 21:49:16 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


void add_header_handler(char *key, void (*func)(char *value));
void read_header(FILE *file);
