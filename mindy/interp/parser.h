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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/parser.h,v 1.1 1994/03/24 21:49:47 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#define YYSTYPE obj_t
#include "parser.tab.h"
#define tok_EOF 0
extern obj_t parse_exprs(void);
