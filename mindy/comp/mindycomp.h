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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/mindycomp.h,v 1.1 1994/03/24 21:49:10 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

extern void *malloc(size_t len);
extern void *realloc(void *ptr, size_t len);
extern void free(void *ptr);

typedef int boolean;
#define TRUE 1
#define FALSE 0
extern char *current_file;

extern struct symbol *ModuleName;
extern struct symbol *LibraryName;
