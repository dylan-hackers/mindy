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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/vec.h,v 1.1 1994/03/24 21:49:47 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_SimpleObjectVectorClass;

struct sovec {
    obj_t class;
    int length;
    obj_t contents[0];
};

#define SOVEC(o) obj_ptr(struct sovec *, o)

extern obj_t make_vector(int length, obj_t *contents);
