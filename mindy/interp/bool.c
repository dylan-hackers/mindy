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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/bool.c,v 1.3 1994/04/10 19:00:10 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindy.h"
#include "print.h"
#include "gc.h"
#include "class.h"
#include "obj.h"
#include "bool.h"

struct bool {
    obj_t class;
};

obj_t obj_True = 0, obj_False = 0, obj_BooleanClass = 0;


/* Printer support. */

static void print_true(obj_t true)
{
    printf("#t");
}

static void print_false(obj_t false)
{
    printf("#f");
}


/* GC support. */

static int scav_bool(struct object *bool)
{
    return sizeof(struct bool);
}

static obj_t trans_bool(obj_t bool)
{
    return transport(bool, sizeof(struct bool));
}

void scavenge_bool_roots(void)
{
    scavenge(&obj_True);
    scavenge(&obj_False);
    scavenge(&obj_BooleanClass);
}


/* Init stuff. */

void make_bool_classes(void)
{
    obj_BooleanClass = make_abstract_class(TRUE);
    obj_True = alloc(make_builtin_class(scav_bool, trans_bool),
		     sizeof(struct bool));
    obj_False = alloc(make_builtin_class(scav_bool, trans_bool),
		      sizeof(struct bool));
}

void init_bool_classes(void)
{
    init_builtin_class(obj_BooleanClass, "<boolean>", obj_ObjectClass, NULL);
    init_builtin_class(obj_ptr(struct bool *, obj_True)->class,
		       "<true>", obj_BooleanClass, NULL);
    def_printer(obj_ptr(struct bool *, obj_True)->class, print_true);
    init_builtin_class(obj_ptr(struct bool *, obj_False)->class,
		       "<false>", obj_BooleanClass, NULL);
    def_printer(obj_ptr(struct bool *, obj_False)->class, print_false);
}
