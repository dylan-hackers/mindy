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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/str.h,v 1.1 1994/03/24 21:49:40 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_ByteStringClass;

struct string {
    obj_t class;
    int len;
    unsigned char chars[0];
};

#define string_chars(str) (obj_ptr(struct string *, str)->chars)

extern obj_t make_string(char *chars);
extern obj_t alloc_string(int len);
