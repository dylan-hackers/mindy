/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
*  All rights reserved.
*  
*  Use and copying of this software and preparation of derivative
*  works based on this software are permitted, including commercial
*  use, provided that the following conditions are observed:
*  
*  1. This copyright notice must be retained in full on any copies
*     and on appropriate parts of any derivative works.
*  2. Documentation (paper or online) accompanying any system that
*     incorporates this software, or any part of it, must acknowledge
*     the contribution of the Gwydion Project at Carnegie Mellon
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/interp/class.h,v 1.3 2000/01/24 04:58:09 andreas Exp $
*
\**********************************************************************/

/* If this enumeration changes, you must also update "type.h" */
#ifndef type_Id_defined
#define type_Id_defined
enum type_Id {
    id_Singleton, id_Class, id_SubClass, id_LimFixnum, id_LimBignum,
    id_Union, id_NoneOf
};
#endif

extern obj_t obj_ClassClass;
extern obj_t obj_StaticTypeClass; /* type of static pointer classes */

/* The first two elements of this must be kept in sync with 'struct type' */
struct class {
    obj_t class;
    enum type_Id type_id;
    boolean abstract_p;
    boolean sealed_p;
    struct library *library;
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

extern obj_t make_abstract_class(boolean sealed_p);
extern obj_t make_builtin_class(int (*scavenge)(struct object *ptr),
				obj_t (*transport)(obj_t object));

extern void init_builtin_class _ANSI_ARGS_((obj_t class, char *debug_name, ...));

extern void setup_class_supers(obj_t class, obj_t supers);

