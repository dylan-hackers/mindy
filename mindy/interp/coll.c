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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/coll.c,v 1.6 1994/10/05 21:01:28 nkramer Exp $
*
* This file implements the collection framework.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "class.h"
#include "obj.h"
#include "gc.h"
#include "coll.h"

obj_t obj_CollClass = 0;
obj_t obj_ExKeyCollClass = 0;
obj_t obj_MutCollClass = 0;
obj_t obj_SeqClass = 0;
obj_t obj_MutExKeyCollClass = 0;
obj_t obj_MutSeqClass = 0;
obj_t obj_ArrayClass = 0;
obj_t obj_VectorClass = 0;
obj_t obj_StringClass = 0;


/* GC stuff. */

void scavenge_coll_roots(void)
{
    scavenge(&obj_CollClass);
    scavenge(&obj_ExKeyCollClass);
    scavenge(&obj_MutCollClass);
    scavenge(&obj_SeqClass);
    scavenge(&obj_MutExKeyCollClass);
    scavenge(&obj_MutSeqClass);
    scavenge(&obj_ArrayClass);
    scavenge(&obj_VectorClass);
    scavenge(&obj_StringClass);
}


/* Init stuff. */

void make_coll_classes(void)
{
    obj_CollClass = make_abstract_class(FALSE);
    obj_ExKeyCollClass = make_abstract_class(FALSE);
    obj_MutCollClass = make_abstract_class(FALSE);
    obj_SeqClass = make_abstract_class(FALSE);
    obj_MutExKeyCollClass = make_abstract_class(FALSE);
    obj_MutSeqClass = make_abstract_class(FALSE);
    obj_ArrayClass = make_abstract_class(FALSE);
    obj_VectorClass = make_abstract_class(FALSE);
    obj_StringClass = make_abstract_class(FALSE);
}

void init_coll_classes(void)
{
    init_builtin_class(obj_CollClass, "<collection>", obj_ObjectClass, NULL);
    init_builtin_class(obj_ExKeyCollClass, "<explicit-key-collection>",
		       obj_CollClass, NULL);
    init_builtin_class(obj_MutCollClass, "<mutable-collection>",
		       obj_CollClass, NULL);
    init_builtin_class(obj_SeqClass, "<sequence>", obj_CollClass, NULL);
    init_builtin_class(obj_MutExKeyCollClass,
		       "<mutable-explicit-key-collection>",
		       obj_MutCollClass, obj_ExKeyCollClass, NULL);
    init_builtin_class(obj_MutSeqClass, "<mutable-sequence>",
		       obj_MutCollClass, obj_SeqClass, NULL);
    init_builtin_class(obj_ArrayClass, "<array>", obj_MutSeqClass, NULL);
    init_builtin_class(obj_VectorClass, "<vector>", obj_ArrayClass, NULL);
    init_builtin_class(obj_StringClass, "<string>", obj_MutSeqClass, NULL);
}
