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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/char.h,v 1.1 1994/03/24 21:49:24 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_CharacterClass;

struct character {
    obj_t class;
    unsigned char c;
};

extern obj_t int_char(int c);

#define char_int(o) (obj_ptr(struct character *, o)->c)
