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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/type.h,v 1.3 1994/04/24 21:41:53 rgs Exp $
*
* This file does whatever.
*
\**********************************************************************/


/* If this enumeration changes, you must also update "class.h" */
#ifndef type_Id_defined
#define type_Id_defined
enum type_Id {id_Singleton, id_Class, id_SubClass, id_LimInt, id_Union};
#endif

extern obj_t obj_TypeClass;

struct type {
    obj_t class;
    enum type_Id type_id;
};

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
extern obj_t intersect_limited_integers(obj_t lim1, obj_t lim2);
extern obj_t restrict_limited_integers(obj_t val, obj_t lim1, obj_t lim2);

extern void type_init_classes(void);
