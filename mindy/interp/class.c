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
* $Header: /scm/cvs/src/mindy/interp/class.c,v 1.1 1998/05/03 19:55:12 andreas Exp $
*
* This file implements classes.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "type.h"
#include "list.h"
#include "sym.h"
#include "bool.h"
#include "obj.h"
#include "error.h"
#include "def.h"
#include "print.h"
#include "class.h"
#include "extern.h"
#include "instance.h"

obj_t obj_ClassClass = 0;
obj_t obj_StaticTypeClass = NULL; /* type of static pointer classes */



/* Class constructors. */

obj_t make_builtin_class(int (*scavenge)(struct object *ptr),
			 obj_t (*transport)(obj_t object))
{
    obj_t res = alloc(obj_ClassClass, sizeof(struct class));

    init_class_type_stuff(res);
    CLASS(res)->abstract_p = FALSE;
    CLASS(res)->sealed_p = TRUE;
    CLASS(res)->library = NULL;
    CLASS(res)->scavenge = scavenge;
    CLASS(res)->transport = transport;
    CLASS(res)->print = NULL;
    /* These really want to be an obj_t, but we don't have any good obj_t's */
    /* to fill them in with yet. */
    CLASS(res)->debug_name = NULL;
    CLASS(res)->superclasses = NULL;
    CLASS(res)->cpl = NULL;
    CLASS(res)->direct_subclasses = NULL;
    CLASS(res)->all_subclasses = NULL;

    return res;
}

static int scav_lose(struct object *ptr)
{
    lose("Found an instance of an abstract class?\n");
    return 0;
}

static obj_t trans_lose(obj_t obj)
{
    lose("Found an instance of an abstract class?\n");
    return NULL;
}

obj_t make_abstract_class(boolean sealed_p)
{
    obj_t res = make_builtin_class(scav_lose, trans_lose);

    CLASS(res)->abstract_p = TRUE;
    CLASS(res)->sealed_p = sealed_p;

    return res;
}


/* CPL computation. */

struct cpd {
    obj_t class;
    struct cpd_chain *supers;
    struct cpd_chain *after;
    int count;
};

struct cpd_chain {
    struct cpd *cpd;
    struct cpd_chain *next;
};

static struct cpd_chain *cpds = NULL;
static int class_count = 0;

static void push_cpd(struct cpd *cpd, struct cpd_chain **chain)
{
    struct cpd_chain *new = (struct cpd_chain *)malloc(sizeof(struct cpd));

    new->cpd = cpd;
    new->next = *chain;
    *chain = new;
}

static struct cpd *pop_cpd(struct cpd_chain **chainptr)
{
    struct cpd_chain *chain = *chainptr;
    struct cpd *cpd = chain->cpd;

    *chainptr = chain->next;
    free(chain);

    return cpd;
}

static void free_cpd_chain(struct cpd_chain *chain)
{
    while (chain != NULL) {
	struct cpd_chain *next = chain->next;
	free(chain);
	chain = next;
    }
}

static struct cpd *find_cpd(obj_t class);

static struct cpd *compute_cpd(obj_t class, obj_t supers)
{
    struct cpd *cpd = (struct cpd *)malloc(sizeof(struct cpd));

    cpd->class = class;
    cpd->supers = NULL;
    cpd->after = NULL;
    cpd->count = 0;
    push_cpd(cpd, &cpds);
    class_count++;

    if (supers != obj_Nil) {
	struct cpd *prev_super_cpd = find_cpd(HEAD(supers));
	push_cpd(prev_super_cpd, &cpd->supers);
	push_cpd(prev_super_cpd, &cpd->after);
	prev_super_cpd->count++;
	while ((supers = TAIL(supers)) != obj_Nil) {
	    struct cpd *super_cpd = find_cpd(HEAD(supers));
	    push_cpd(super_cpd, &cpd->supers);
	    push_cpd(super_cpd, &cpd->after);
	    push_cpd(super_cpd, &prev_super_cpd->after);
	    super_cpd->count += 2;
	    prev_super_cpd = super_cpd;
	}
    }
    return cpd;
}

static struct cpd *find_cpd(obj_t class)
{
    struct cpd_chain *ptr;

    for (ptr = cpds; ptr != NULL; ptr = ptr->next)
	if (ptr->cpd->class == class)
	    return ptr->cpd;

    return compute_cpd(class, CLASS(class)->superclasses);
}

static struct cpd *tie_breaker(struct cpd_chain **candidates, obj_t rcpl)
{
    obj_t remaining, supers;
    struct cpd_chain **prev, *ptr;

    for (remaining = rcpl; remaining != obj_Nil; remaining = TAIL(remaining)) {
	supers = CLASS(HEAD(remaining))->superclasses;
	for (prev = candidates; (ptr = *prev) != NULL; prev = &ptr->next)
	    if (memq(ptr->cpd->class, supers))
		return pop_cpd(prev);
    }
    lose("Can't happen.\n");
    return NULL;
}

static obj_t slow_compute_cpl(obj_t class, obj_t superclasses)
{
    struct cpd_chain *candidates;
    struct cpd *candidate;
    obj_t rcpl;
    int count;
    struct cpd_chain *after;

    cpds = NULL;
    class_count = 0;
    candidates = NULL;
    push_cpd(compute_cpd(class, superclasses), &candidates);
    free_cpd_chain(cpds);
    cpds = NULL;

    rcpl = obj_Nil;
    for (count = 0; count < class_count; count++) {
	if (candidates == NULL)
	    error("Inconsistent CPL");
	if (candidates->next != NULL)
	    candidate = tie_breaker(&candidates, rcpl);
	else
	    candidate = pop_cpd(&candidates);

	rcpl = pair(candidate->class, rcpl);

	free_cpd_chain(candidate->supers);
	for (after = candidate->after; after != NULL; after = after->next) {
	    after->cpd->count--;
	    if (after->cpd->count == 0)
		push_cpd(after->cpd, &candidates);
	}
	free_cpd_chain(candidate->after);
	free(candidate);
    }

    return nreverse(rcpl);
}

static obj_t compute_cpl(obj_t class, obj_t superclasses)
{
    if (superclasses == obj_Nil)
	return list1(class);
    else if (TAIL(superclasses) == obj_Nil)
	return pair(class, CLASS(HEAD(superclasses))->cpl);
    else
	return slow_compute_cpl(class, superclasses);
}


/* Class initialization. */

void setup_class_supers(obj_t class, obj_t supers)
{
    obj_t cpl, scan;
    boolean some_static = FALSE;

    for (scan = supers; scan != obj_Nil; scan = TAIL(scan)) {
	obj_t super = HEAD(scan);
	if (CLASS(super)->sealed_p
	      && CLASS(super)->library != CLASS(class)->library)
	    error("Can't add subclasses to sealed class %=", super);
	if (CLASS(super)->superclasses == obj_False
	      || CLASS(super)->superclasses == NULL)
	    error("Attempt to use %= before it is initialized", super);
	if (object_class(super) == obj_StaticTypeClass)
	    some_static = TRUE;
    }
    
    if (some_static) {
	/* If we inherit from a statically typed pointer class, then we must
	   be a statically typed pointer class.  We must therefore act like
	   one */
	assert(CLASS(class)->class == obj_DefinedClassClass);
	CLASS(class)->class = obj_StaticTypeClass;
	CLASS(class)->scavenge = scav_c_pointer;
	CLASS(class)->transport = trans_c_pointer;
	shrink(class, sizeof(struct defined_class), sizeof(struct class));
    }

    CLASS(class)->superclasses = supers;
    cpl = compute_cpl(class, supers);
    CLASS(class)->cpl = cpl;

    for (scan = TAIL(cpl); scan != obj_Nil; scan = TAIL(scan)) {
	obj_t super = HEAD(scan);
	CLASS(super)->all_subclasses
	    = pair(class, CLASS(super)->all_subclasses);
    }
    for (scan = supers; scan != obj_Nil; scan = TAIL(scan)) {
	obj_t super = HEAD(scan);
	CLASS(super)->direct_subclasses
	    = pair(class, CLASS(super)->direct_subclasses);
    }
}

static void vinit_builtin_class(obj_t class, char *name, va_list ap)
{
    obj_t super, supers;

    supers = obj_Nil;
    while ((super = va_arg(ap, obj_t)) != NULL)
	supers = pair(super, supers);
    supers = nreverse(supers);

    CLASS(class)->debug_name = symbol(name);
    setup_class_supers(class, supers);
    CLASS(class)->direct_subclasses = obj_Nil;
    CLASS(class)->all_subclasses = obj_Nil;

    define_class(name, class);
}
#if _USING_PROTOTYPES_
void init_builtin_class(obj_t class, char *name, ...)
{
    va_list ap;

    va_start(ap, name);
    vinit_builtin_class(class, name, ap);
    va_end(ap);
}
#else
void init_builtin_class(va_alist) va_dcl
{
    va_list ap;
    obj_t class;
    char *name;

    va_start(ap);
    class = va_arg(ap, obj_t);
    name = va_arg(ap, char *);
    vinit_builtin_class(class, name, ap);
    va_end(ap);
}
#endif


/* Dylan functions. */

static obj_t abstractp(obj_t class)
{
    return CLASS(class)->abstract_p ? obj_True : obj_False;
}

static obj_t class_name(obj_t class)
{
    return CLASS(class)->debug_name;
}

static obj_t direct_superclasses(obj_t class)
{
    return CLASS(class)->superclasses;
}

static obj_t direct_subclasses(obj_t class)
{
    return CLASS(class)->direct_subclasses;
}

static obj_t all_superclasses(obj_t class)
{
    return CLASS(class)->cpl;
}


/* Printer support. */

static void print_class(obj_t class)
{
    obj_t debug_name = CLASS(class)->debug_name;

    if (debug_name != NULL && debug_name != obj_False)
	printf("{class %s}", sym_name(debug_name));
    else
	printf("{anonymous class 0x%08lx}", (unsigned long)class);
}


/* GC stuff. */

static int scav_class(struct object *o)
{
    struct class *c = (struct class *)o;

    scavenge(&c->debug_name);
    scavenge(&c->superclasses);
    scavenge(&c->cpl);
    scavenge(&c->direct_subclasses);
    scavenge(&c->all_subclasses);

    return sizeof(struct class);
}

static obj_t trans_class(obj_t class)
{
    return transport(class, sizeof(struct class), FALSE);
}


/* Init stuff. */

void make_class_classes(void)
{
    obj_ClassClass = ptr_obj(0);
    obj_ClassClass = make_builtin_class(scav_class, trans_class);
    CLASS(obj_ClassClass)->class = obj_ClassClass;
    obj_StaticTypeClass = make_builtin_class(scav_class, trans_class);
    add_constant_root(&obj_ClassClass);
    add_constant_root(&obj_StaticTypeClass);
}

void init_class_classes(void)
{
    init_builtin_class(obj_ClassClass, "<class>", obj_TypeClass, NULL);
    def_printer(obj_ClassClass, print_class);
    init_builtin_class(obj_StaticTypeClass, "<static-pointer-class>",
		       obj_ClassClass, NULL);
    def_printer(obj_StaticTypeClass, print_class);
}

void init_class_functions(void)
{
    define_method("abstract?", list1(obj_ClassClass), FALSE, obj_False,
		  FALSE, obj_ObjectClass, abstractp);
    define_method("class-name", list1(obj_ClassClass), FALSE, obj_False,
		  FALSE, obj_ObjectClass, class_name);
    define_method("all-superclasses", list1(obj_ClassClass), FALSE, obj_False,
		  FALSE, obj_ObjectClass, all_superclasses);
    define_method("direct-superclasses", list1(obj_ClassClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, direct_superclasses);
    define_method("direct-subclasses", list1(obj_ClassClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, direct_subclasses);
}
