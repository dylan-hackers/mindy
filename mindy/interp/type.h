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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/type.h,v 1.1 1994/03/24 21:49:42 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_TypeClass;

/* Various type predicates. */
extern boolean instancep(obj_t thing, obj_t type);
extern boolean subtypep(obj_t type1, obj_t type2);
extern boolean overlapp(obj_t type1, obj_t type2);

/* Functions to create specializers. */
extern obj_t singleton(obj_t object);
extern obj_t subclass(obj_t object);
extern obj_t limited_integer(obj_t min, obj_t max);
extern obj_t type_union(obj_t type1, obj_t type2);

/* Extra dreck. */
extern void init_class_type_stuff(obj_t class);


extern void type_init_classes(void);
