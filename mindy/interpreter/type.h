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
\**********************************************************************/


/* If this enumeration changes, you must also update "class.h" */
#ifndef type_Id_defined
#define type_Id_defined
enum type_Id {
    id_Singleton, id_Class, id_SubClass, id_LimFixnum, id_LimBignum,
    id_Union, id_NoneOf
};
#endif

extern obj_t obj_TypeClass;

#define TYPE(x) obj_ptr(struct type *, x)

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
extern obj_t limited_fixnum(obj_t min, obj_t max);
extern obj_t limited_bignum(obj_t min, obj_t max);
extern obj_t type_union(obj_t type1, obj_t type2);
extern obj_t intersect_limited_fixnums(obj_t lim1, obj_t lim2);
extern obj_t intersect_limited_bignums(obj_t lim1, obj_t lim2);
extern obj_t restrict_limited_fixnums(obj_t val, obj_t lim1, obj_t lim2);
extern obj_t restrict_limited_bignums(obj_t val, obj_t lim1, obj_t lim2);
extern obj_t restrict_type(obj_t val, obj_t type);

/* Extra dreck. */
extern void init_class_type_stuff(obj_t class);

extern void type_init_classes(void);
