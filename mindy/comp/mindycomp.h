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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/mindycomp.h,v 1.2 1994/04/08 14:26:18 wlott Exp $
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
extern boolean ParseOnly;

extern struct symbol *ModuleName;
extern struct symbol *LibraryName;
