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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/lexenv.c,v 1.1 1994/03/24 21:49:14 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindycomp.h"
#include "src.h"
#include "lexenv.h"

struct binding *make_binding(struct id *id, struct symbol *type,
			     boolean argument, int offset,
			     struct method *home, struct binding *next)
{
    struct binding *res = malloc(sizeof(struct binding));

    res->id = id;
    res->type = type;
    res->home = home;
    res->function = FALSE;
    res->closed_over = FALSE;
    res->set = FALSE;
    res->argument = argument;
    res->offset = offset;
    res->next = next;

    return res;
}

struct lexenv *make_lexenv(struct method *method, struct binding *bindings,
			   int depth)
{
    struct lexenv *res = malloc(sizeof(struct lexenv));

    res->method = method;
    res->bindings = bindings;
    res->depth = depth;

    return res;
}

