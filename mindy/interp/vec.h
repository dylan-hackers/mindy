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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/vec.h,v 1.2 1994/04/28 04:47:55 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_SimpleObjectVectorClass;
extern obj_t obj_ByteVectorClass;

struct sovec {
    obj_t class;
    int length;
    obj_t contents[0];
};

struct bytevec {
    obj_t class;
    int length;
    unsigned char contents[0];
};

#define SOVEC(o) obj_ptr(struct sovec *, o)
#define BYTEVEC(o) obj_ptr(struct bytevec *, o)

extern obj_t make_vector(int length, obj_t *contents);
extern obj_t make_byte_vector(int length, unsigned char *contents);
