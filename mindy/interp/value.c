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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/value.c,v 1.1 1994/03/24 21:49:48 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

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
    return transport(value_cell, sizeof(struct value_cell));
}

void scavenge_value_roots(void)
{
    scavenge(&obj_ValueCellClass);
}


/* Init stuff */

void make_value_classes(void)
{
    obj_ValueCellClass = make_builtin_class(scav_value_cell, trans_value_cell);
}

void init_value_classes(void)
{
    init_builtin_class(obj_ValueCellClass, "<value-cell>",
		       obj_ObjectClass, NULL);
}
