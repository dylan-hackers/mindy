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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/class.h,v 1.2 1994/04/09 13:35:45 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


enum type_Id {id_Singleton, id_Class, id_SubClass, id_LimInt, id_Union};


extern obj_t obj_ClassClass;

struct class {
    obj_t class;
    enum type_Id type_id;
    boolean abstract_p;
    int (*scavenge)(struct object *ptr);
    obj_t (*transport)(obj_t object);
    void (*print)(obj_t object);
    obj_t debug_name;
    obj_t superclasses;
    obj_t cpl;
    obj_t direct_subclasses;
    obj_t all_subclasses;
};

#define CLASS(o) obj_ptr(struct class *, o)

extern obj_t make_abstract_class(void);
extern obj_t make_builtin_class(int (*scavenge)(struct object *ptr),
				obj_t transport(obj_t object));

extern void init_builtin_class(obj_t class, char *debug_name, ...);

extern void setup_class_supers(obj_t class, obj_t supers);

