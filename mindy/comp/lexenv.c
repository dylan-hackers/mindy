/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
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
*     University.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports, questions, comments, and suggestions should be sent by
*  E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/comp/lexenv.c,v 1.1 1998/05/03 19:55:07 andreas Exp $
*
* This file implements the stuff to keep track of what is in the lexical
* environment.
*
\**********************************************************************/

#include "../compat/std-c.h"

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

