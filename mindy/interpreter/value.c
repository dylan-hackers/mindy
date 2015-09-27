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
* This file implements value cells.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "obj.h"
#include "class.h"
#include "value.h"

struct value_cell {
    obj_t class;
    obj_t value;
};

static obj_t obj_ValueCellClass = NULL;

obj_t make_value_cell(obj_t value)
{
    obj_t res = alloc(obj_ValueCellClass, sizeof(struct value_cell));

    obj_ptr(struct value_cell *, res)->value = value;

    return res;
}

obj_t value_cell_ref(obj_t value_cell)
{
    return obj_ptr(struct value_cell *, value_cell)->value;
}

obj_t value_cell_set(obj_t value_cell, obj_t value)
{
    return obj_ptr(struct value_cell *, value_cell)->value = value;
}


/* GC Routines */

static int scav_value_cell(struct object *ptr)
{
    scavenge(&((struct value_cell *)ptr)->value);
    return sizeof(struct value_cell);
}

static obj_t trans_value_cell(obj_t value_cell)
{
    return transport(value_cell, sizeof(struct value_cell), TRUE);
}


/* Init stuff */

void make_value_classes(void)
{
    obj_ValueCellClass = make_builtin_class(scav_value_cell, trans_value_cell);
    add_constant_root(&obj_ValueCellClass);
}

void init_value_classes(void)
{
    init_builtin_class(obj_ValueCellClass, "<value-cell>",
		       obj_ObjectClass, NULL);
}
