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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/handler.h,v 1.1 1994/03/24 21:49:44 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



struct handler {
    obj_t class;
    obj_t type;
    obj_t function;
    obj_t test;
    obj_t description;
    obj_t next;
};

#define HANDLER(o) obj_ptr(struct handler *, o)
