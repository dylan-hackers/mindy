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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/sym.h,v 1.2 1994/03/31 10:16:37 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct symbol {
    unsigned hash;
    struct symbol *next;
    int handle;
    unsigned char name[0];
};

extern struct symbol *symbol(char *name);
extern struct symbol *gensym(void);
