/**********************************************************************\
*
*  Copyright (c) 1994 Carnegie Mellon University All rights reserved.
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
* $Header: /scm/cvs/src/mindy/interp/instance.c,v 1.1 1998/05/03 19:55:14 andreas Exp $
*
* This file implements instances and user defined classes.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "obj.h"
#include "class.h"
#include "list.h"
#include "vec.h"
#include "type.h"
#include "bool.h"
#include "module.h"

#include "num.h"
#include "thread.h"
#include "func.h"
#include "sym.h"
#include "value.h"
#include "error.h"
#include "driver.h"
#include "def.h"
#include "print.h"
#include "instance.h"

struct slot_descr {
    obj_t class;
    obj_t name;
    enum slot_allocation alloc;
    obj_t creator;
    obj_t init_function_or_value;
    boolean init_function_p;
    obj_t init_keyword;
    boolean keyword_required;
    obj_t getter;
    obj_t getter_method;
    obj_t setter;
    obj_t setter_method;
    obj_t type;
    int desired_offset;
    boolean ever_missed;
};

#define SD(o) obj_ptr(struct slot_descr *, o)

struct initarg_descr {
    obj_t class;
    obj_t keyword;
    boolean required_p;
    obj_t type;
    obj_t init_function_or_value;
    boolean init_function_p;
    obj_t initializer;
};

#define INTD(o) obj_ptr(struct initarg_descr *, o)

struct inherited_descr {
    obj_t class;
    obj_t name;
    obj_t init_function_or_value;
    boolean init_function_p;
};

#define INHD(o) obj_ptr(struct inherited_descr *, o)

struct postable {
    obj_t class;
    obj_t alist;
};

#define PT(o) obj_ptr(struct postable *, o)

enum initializer_kind { slot_Initializer, initarg_slot_Initializer,
			initarg_Initializer, inherited_Initializer };

struct initializer {
    obj_t class;
    enum initializer_kind kind;
    obj_t slot;
    obj_t initarg;
    obj_t inherited;
};

#define INITIALIZER(o) obj_ptr(struct initializer *, o)

struct instance {
    obj_t class;
    obj_t slots[1];
};

#define INST(o) obj_ptr(struct instance *, o)

obj_t obj_DefinedClassClass = NULL;
static obj_t obj_SlotDescrClass = NULL;
static obj_t obj_InitargDescrClass = NULL;
static obj_t obj_InheritedDescrClass = NULL;
static obj_t obj_PosTableClass = NULL;
static obj_t obj_InitializerClass = NULL;


/* Accessor methods. */

static int find_position(obj_t pt, obj_t slot);

static void slow_instance_getter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t instance = args[0];
    obj_t class = INST(instance)->class;
    int index = find_position(DC(class)->instance_positions, datum);
    obj_t value = INST(instance)->slots[index];

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void fast_instance_getter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t instance = args[0];
    obj_t value = INST(instance)->slots[fixnum_value(datum)];

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void slow_instance_setter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];
    obj_t instance = args[1];
    obj_t class = INST(instance)->class;
    int index = find_position(DC(class)->instance_positions, datum);

    INST(instance)->slots[index] = value;

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void fast_instance_setter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];
    obj_t instance = args[1];

    INST(instance)->slots[fixnum_value(datum)] = value;

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void slow_each_subclass_getter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t instance = args[0];
    obj_t class = INST(instance)->class;
    int index = find_position(DC(class)->each_subclass_positions, datum);
    obj_t value = SOVEC(DC(class)->each_subclass_slots)->contents[index];

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void fast_each_subclass_getter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t instance = args[0];
    obj_t class = INST(instance)->class;
    int index = fixnum_value(datum);
    obj_t value = SOVEC(DC(class)->each_subclass_slots)->contents[index];

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void slow_each_subclass_setter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];
    obj_t instance = args[1];
    obj_t class = INST(instance)->class;
    int index = find_position(DC(class)->each_subclass_positions, datum);

    SOVEC(DC(class)->each_subclass_slots)->contents[index] = value;

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void fast_each_subclass_setter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];
    obj_t instance = args[1];
    obj_t class = INST(instance)->class;
    int index = fixnum_value(datum);

    SOVEC(DC(class)->each_subclass_slots)->contents[index] = value;

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void class_getter(obj_t method, struct thread *thread, obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = value_cell_ref(datum);

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void class_setter(obj_t method, struct thread *thread, obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];

    value_cell_set(datum, value);

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}


/* Position tables. */

static obj_t make_position_table(void)
{
    obj_t res = alloc(obj_PosTableClass, sizeof(struct postable));

    PT(res)->alist = obj_Nil;

    return res;
}

static void note_position(obj_t table, obj_t slot, int index)
{
    PT(table)->alist = pair(pair(slot, make_fixnum(index)), PT(table)->alist);

    if (!SD(slot)->ever_missed) {
	SD(slot)->ever_missed = TRUE;
	switch (SD(slot)->alloc) {
	  case alloc_INSTANCE:
	    set_method_iep(SD(slot)->getter_method, slow_instance_getter);
	    set_accessor_method_datum(SD(slot)->getter_method, slot);
	    if (SD(slot)->setter_method != obj_False) {
		set_method_iep(SD(slot)->setter_method, slow_instance_setter);
		set_accessor_method_datum(SD(slot)->setter_method, slot);
	    }
	    break;

	  case alloc_EACH_SUBCLASS:
	    set_method_iep(SD(slot)->getter_method, slow_each_subclass_getter);
	    set_accessor_method_datum(SD(slot)->getter_method, slot);
	    if (SD(slot)->setter_method != obj_False) {
		set_method_iep(SD(slot)->setter_method, slow_each_subclass_setter);
		set_accessor_method_datum(SD(slot)->setter_method, slot);
	    }
	    break;

	  default:
	    lose("Displacing a slot with allocation other than "
		 "instance or each_subclass?");
	    break;
	}
    }
}

static int find_position(obj_t pt, obj_t slot)
{
    obj_t scan;

    if (pt != obj_False) {
	for (scan = PT(pt)->alist; scan != obj_Nil; scan = TAIL(scan)) {
	    obj_t entry = HEAD(scan);

	    if (HEAD(entry) == slot)
		return fixnum_value(TAIL(entry));
	}
    }
    return SD(slot)->desired_offset;
}


/* Slot descriptors. */

static obj_t make_slot_descriptor(obj_t name, obj_t allocation,
				  obj_t getter, obj_t setter, obj_t type,
				  obj_t init_keyword, obj_t req_init_keyword,
				  obj_t init_function, obj_t init_value)
{
    obj_t res = alloc(obj_SlotDescrClass, sizeof(struct slot_descr));
    enum slot_allocation alloc
	= (enum slot_allocation)fixnum_value(allocation);

    SD(res)->name = name;
    SD(res)->alloc = alloc;
    SD(res)->creator = obj_False;
    if (init_function != obj_Unbound) {
	if (init_value != obj_Unbound)
	    error("Can't specify both an init-function: and an init-value:");
	check_type(init_function, obj_FunctionClass);
	SD(res)->init_function_or_value = init_function;
	SD(res)->init_function_p = TRUE;
    }
    else {
	if (init_value != obj_Unbound && type != obj_False)
	    check_type(init_value, type);
	SD(res)->init_function_or_value = init_value;
	SD(res)->init_function_p = FALSE;
    }
    if (req_init_keyword != obj_False) {
	if (init_function != obj_Unbound)
	    error("Can't mix required-init-keyword: and init-function:");
	if (init_value != obj_Unbound)
	    error("Can't mix required-init-keyword: and init-value:");
	if (init_keyword != obj_False)
	    error("Can't mix required-init-keyword: and init-keyword:");
	SD(res)->init_keyword = req_init_keyword;
	SD(res)->keyword_required = TRUE;
    }
    else {
	SD(res)->init_keyword = init_keyword;
	SD(res)->keyword_required = FALSE;
    }
    SD(res)->getter = getter;
    SD(res)->getter_method = obj_False;
    SD(res)->setter = setter;
    SD(res)->setter_method = obj_False;
    if (type == obj_False)
	SD(res)->type = obj_ObjectClass;
    else
	SD(res)->type = type;
    SD(res)->desired_offset = -1;
    SD(res)->ever_missed = FALSE;

    return res;
}

/* Initarg Descriptors */

static obj_t make_initarg_descr(obj_t keyword, obj_t required, obj_t type,
				obj_t init_function, obj_t init_value)
{
    obj_t res = alloc(obj_InitargDescrClass, sizeof(struct initarg_descr));

    INTD(res)->keyword = keyword;
    if (required != obj_False) {
	if (init_function != obj_Unbound || init_value != obj_Unbound)
	    error("Can't specify initial value for required init arg.");
	INTD(res)->required_p = TRUE;
    }
    else {
        INTD(res)->required_p = FALSE;
    }
    if (type == obj_False) {
        INTD(res)->type = obj_ObjectClass;
    }
    else {
        INTD(res)->type = type;
    }
    if (init_function != obj_Unbound) {
	if (init_value != obj_Unbound)
	    error("Can't specify both an init-function: and an init-value:");
	check_type(init_function, obj_FunctionClass);
	INTD(res)->init_function_or_value = init_function;
	INTD(res)->init_function_p = TRUE;
    }
    else {
	INTD(res)->init_function_or_value = init_value;
	INTD(res)->init_function_p = FALSE;
    }
    INTD(res)->initializer = obj_False;

    return res;
}

/* Inherited Descriptors */

static obj_t make_inherited_descr(obj_t name,
				  obj_t init_function, obj_t init_value)
{
    obj_t res = alloc(obj_InheritedDescrClass, sizeof(struct inherited_descr));

    INHD(res)->name = name;
    if (init_function != obj_Unbound) {
	if (init_value != obj_Unbound)
	    error("Can't specify both an init-function: and an init-value:");
	check_type(init_function, obj_FunctionClass);
	INHD(res)->init_function_or_value = init_function;
	INHD(res)->init_function_p = TRUE;
    }
    else {
	INHD(res)->init_function_or_value = init_value;
	INHD(res)->init_function_p = FALSE;
    }

    return res;
}


/* Initializers */

static struct variable *initialize_gf_variable = NULL;

static obj_t make_initializer(enum initializer_kind kind, obj_t slot,
			      obj_t initarg, obj_t inherited)
{
    obj_t res = alloc(obj_InitializerClass, sizeof(struct initializer));

    INITIALIZER(res)->kind = kind;
    INITIALIZER(res)->slot = slot;
    INITIALIZER(res)->initarg = initarg;
    INITIALIZER(res)->inherited = inherited;

    return res;
}

static obj_t slot_initializer(obj_t slot)
{
    return make_initializer(slot_Initializer, slot,
			    obj_False, obj_False);
}

static obj_t initarg_slot_initializer(obj_t slot, obj_t initarg)
{
    return make_initializer(initarg_slot_Initializer, slot,
			    initarg, obj_False);
}

static obj_t initarg_initializer(obj_t initarg)
{
    return make_initializer(initarg_Initializer, obj_False,
			    initarg, obj_False);
}

static obj_t inherited_initializer(obj_t slot, obj_t inherited)
{
    return make_initializer(inherited_Initializer, slot,
			    obj_False, inherited);
}

static boolean initializer_init_function_p(obj_t initializer)
{
    switch (INITIALIZER(initializer)->kind) {
      case slot_Initializer:
	return SD(INITIALIZER(initializer)->slot)->init_function_p;
	break;
      case initarg_slot_Initializer:
	return INTD(INITIALIZER(initializer)->initarg)->init_function_p;
	break;
      case initarg_Initializer:
	return INTD(INITIALIZER(initializer)->initarg)->init_function_p;
	break;
      case inherited_Initializer:
	return INHD(INITIALIZER(initializer)->inherited)->init_function_p;
	break;
      default:
	lose("Tried to get init_function_p from strange initializer.");
	return FALSE;
    }
}

static obj_t initializer_init_function_or_value(obj_t initializer)
{
    switch (INITIALIZER(initializer)->kind) {
      case slot_Initializer:
	return SD(INITIALIZER(initializer)->slot)->init_function_or_value;
	break;
      case initarg_slot_Initializer:
	return INTD(INITIALIZER(initializer)->initarg)->init_function_or_value;
	break;
      case initarg_Initializer:
	return INTD(INITIALIZER(initializer)->initarg)->init_function_or_value;
	break;
      case inherited_Initializer:
	return INHD(INITIALIZER(initializer)->inherited)->init_function_or_value;
	break;
      default:
	lose("Tried to get init_function_or_value from strange initializer.");
	return NULL;
    }
}

static void do_finish_initialization(struct thread *thread, obj_t *vals)
{
    obj_t inst_or_class = vals[-3];
    obj_t *old_sp = pop_linkage(thread);

    *old_sp = inst_or_class;
    thread->sp = old_sp + 1;

    do_return(thread, old_sp, old_sp);
}

static void do_init_value(struct thread *thread, obj_t *vals);

static void do_initializers(struct thread *thread, obj_t initializers)
{
    obj_t *sp = thread->sp;

    /* If there are initializers left, get the init-value or call the
       init-function, and give the value to do_init_value.

       If there are no initializers left, call the Dylan initialize
       function with the defaulted initargs. */

    if (initializers != obj_Nil) {
	obj_t initializer = HEAD(initializers);

	sp[-1] = initializers;
	sp[0] = initializer_init_function_or_value(initializer);
	thread->sp = sp + 1;
	if (initializer_init_function_p(initializer)) {
	    set_c_continuation(thread, do_init_value);
	    invoke(thread, 0);
	}
	else {
	    do_init_value(thread, sp);
	}
    }
    else {
	obj_t inst_or_class = sp[-3];
	obj_t initargs = sp[-2];
	int nargs;

	*sp++ = initialize_gf_variable->value;
	*sp++ = inst_or_class;
	for ( ; initargs != obj_Nil; initargs = TAIL(initargs)) {
	    obj_t initarg = HEAD(initargs);
	    *sp++ = INTD(initarg)->keyword;
	    *sp++ = INTD(initarg)->init_function_or_value;
	}
	nargs = sp - thread->sp - 1;
	thread->sp = sp;

	set_c_continuation(thread, do_finish_initialization);
	invoke(thread, nargs);
    }
}

static void do_init_value(struct thread *thread, obj_t *vals)
{
    obj_t inst_or_class = vals[-3];
    obj_t initializers = vals[-1];
    obj_t initializer = HEAD(initializers);
    obj_t value;
    obj_t slot;
    int index;
    obj_t initarg;

    if (thread->sp == vals)
	value = obj_False;
    else {
	value = vals[0];
	thread->sp = vals;
    }

    /* Initialize a slot if necessary */

    if (obj_ptr(struct object *, inst_or_class)->class
	  == obj_DefinedClassClass) {
	obj_t class = inst_or_class;

	switch (INITIALIZER(initializer)->kind) {
	  case slot_Initializer:
	  case initarg_slot_Initializer:
	  case inherited_Initializer:
	    slot = INITIALIZER(initializer)->slot;
	    if (value != obj_Unbound && !instancep(value, SD(slot)->type))
		type_error(value, SD(slot)->type);
	    switch (SD(slot)->alloc) {
	      case alloc_EACH_SUBCLASS:
		index 
		    = find_position(DC(class)->each_subclass_positions, slot);
		SOVEC(DC(class)->each_subclass_slots)->contents[index] = value;
		break;
	      case alloc_CLASS:
		value_cell_set(accessor_method_datum(SD(slot)->getter_method),
			       value);
		break;
	      default:
		lose("Tried to initialize a strange kind of class slot.");
	    }
	    break;
	  case initarg_Initializer:
	    break;
	  default:
	    lose("Strange kind of initializer.");
	}
    }
    else {
	obj_t instance = inst_or_class;
	obj_t class = INST(instance)->class;

	switch (INITIALIZER(initializer)->kind) {
	  case slot_Initializer:
	  case initarg_slot_Initializer:
	  case inherited_Initializer:
	    slot = INITIALIZER(initializer)->slot;
	    if (value != obj_Unbound && !instancep(value, SD(slot)->type))
	        type_error(value, SD(slot)->type);
	    switch (SD(slot)->alloc) {
	      case alloc_INSTANCE:
		index = find_position(DC(class)->instance_positions, slot);
		INST(instance)->slots[index] = value;
		break;
	      case alloc_EACH_SUBCLASS:
		index = find_position(DC(class)->each_subclass_positions, slot);
		SOVEC(DC(class)->each_subclass_slots)->contents[index] = value;
		break;
	      case alloc_CLASS:
		value_cell_set(accessor_method_datum(SD(slot)->getter_method),
			       value);
		break;
	      case alloc_VIRTUAL:
		/* Do nothing with the value. */
		break;
	      default:
		lose("Tried to initialize a strange kind of instance slot.");
	    }
	    break;
	  case initarg_Initializer:
	    break;
	  default:
	    lose("Strange kind of initializer.");
	}
    }

    /* Initialize an initarg if necessary */

    switch (INITIALIZER(initializer)->kind) {
      case initarg_slot_Initializer:
      case initarg_Initializer:
	initarg = INITIALIZER(initializer)->initarg;
	if (value != obj_Unbound && !instancep(value, INTD(initarg)->type))
	    type_error(value, INTD(initarg)->type);
	INTD(initarg)->init_function_or_value = value;
	INTD(initarg)->init_function_p = FALSE;
	break;
      case slot_Initializer:
      case inherited_Initializer:
	break;
      default:
	lose("Strange kind of initializer.");
    }

    do_initializers(thread, TAIL(initializers));
}

static void do_initialization(obj_t inst_or_class, obj_t initargs,
			      obj_t initializers)
{
    struct thread *thread = thread_current();
    obj_t *sp = thread->sp += 3;

    sp[-3] = inst_or_class;
    sp[-2] = initargs;
    do_initializers(thread, initializers);
    go_on();
}


/* Defined Classes */

static int scav_instance(struct object *ptr);
static obj_t trans_instance(obj_t instance);

obj_t make_defined_class(obj_t debug_name, struct library *library)
{
    obj_t res = alloc(obj_DefinedClassClass, sizeof(struct defined_class));

    init_class_type_stuff(res);
    DC(res)->abstract_p = FALSE;
    DC(res)->sealed_p = FALSE;
    DC(res)->library = library;
    DC(res)->scavenge = scav_instance;
    DC(res)->transport = trans_instance;
    DC(res)->print = NULL;
    DC(res)->debug_name = debug_name;
    DC(res)->superclasses = obj_False;
    DC(res)->cpl = obj_False;
    DC(res)->direct_subclasses = obj_Nil;
    DC(res)->all_subclasses = obj_Nil;
    DC(res)->new_slots = obj_False;
    DC(res)->all_slots = obj_False;
    DC(res)->new_initargs = obj_False;
    DC(res)->all_initargs = obj_False;
    DC(res)->new_inheriteds = obj_False;
    DC(res)->all_inheriteds = obj_False;
    DC(res)->valid_init_keywords = obj_False;
    DC(res)->valid_init_keywords_clock = obj_False;
    DC(res)->instance_positions = obj_False;
    DC(res)->instance_length = 0;
    DC(res)->instance_layout = obj_False;
    DC(res)->each_subclass_positions = obj_False;
    DC(res)->each_subclass_slots = obj_False;
    DC(res)->each_subclass_layout = obj_False;

    return res;
}

static void compute_lengths(obj_t class)
{
    obj_t scan, slots, layout;
    int instance_length = 0;
    int each_subclass_length = 0;
    int i;

    for (scan = TAIL(DC(class)->cpl); scan != obj_Nil; scan = TAIL(scan)) {
	obj_t super = HEAD(scan);
	if (obj_ptr(struct class *, super)->class == obj_DefinedClassClass) {
	    for (slots=DC(super)->new_slots;slots!=obj_Nil;slots=TAIL(slots)) {
		switch (SD(HEAD(slots))->alloc) {
		  case alloc_INSTANCE:
		    instance_length++;
		    break;
		  case alloc_EACH_SUBCLASS:
		    each_subclass_length++;
		    break;
		  case alloc_CLASS:
		  case alloc_VIRTUAL:
		    break;
		  default:
		    lose("Strange slot allocation.");
		}
	    }
	}
    }

    for (slots = DC(class)->new_slots; slots != obj_Nil; slots = TAIL(slots)) {
	obj_t slot = HEAD(slots);
	switch (SD(slot)->alloc) {
	  case alloc_INSTANCE:
	    SD(slot)->desired_offset = instance_length++;
	    break;
	  case alloc_EACH_SUBCLASS:
	    SD(slot)->desired_offset = each_subclass_length++;
	    break;
	  case alloc_CLASS:
	  case alloc_VIRTUAL:
	    break;
	  default:
	    lose("Strange slot allocation.");
	}
    }

    DC(class)->instance_length = instance_length;
    layout = make_vector(instance_length, NULL);
    DC(class)->instance_layout = layout;
    for (i = 0; i < instance_length; i++)
	SOVEC(layout)->contents[i] = obj_False;

    if (each_subclass_length > 0) {
	obj_t slots = make_vector(each_subclass_length, NULL);
	DC(class)->each_subclass_slots = slots;
	layout = make_vector(each_subclass_length, NULL);
	DC(class)->each_subclass_layout = layout;
	for (i = 0; i < each_subclass_length; i++) {
	    SOVEC(layout)->contents[i] = obj_False;
	    SOVEC(slots)->contents[i] = obj_Unbound;
	}
    }
}


/* Process Slot Specifications */

static void add_slot(obj_t class, obj_t new_slot, boolean inherited)
{
    obj_t new_getter = SD(new_slot)->getter;
    obj_t new_setter = SD(new_slot)->setter;
    obj_t slots;

    for (slots = DC(class)->all_slots; slots != obj_Nil; slots = TAIL(slots)) {
	obj_t slot = HEAD(slots);
	obj_t getter = SD(slot)->getter;
	obj_t setter = SD(slot)->setter;

	if (new_getter == getter)
	    if (inherited)
		error("Can't inherit slot %= from both %= and %=",
		      function_debug_name_or_self(getter), SD(slot)->creator,
		      SD(new_slot)->creator);
	    else
		error("Slot %= in %= clashes with the slot inherited from %=",
		      function_debug_name_or_self(getter), class,
		      SD(slot)->creator);
	if (new_getter == setter)
	    if (inherited)
		error("The getter for slot %= inherited from %= clashes with "
		      "the setter for slot %= inherited from %=",
		      function_debug_name_or_self(new_getter),
		      SD(new_slot)->creator,
		      function_debug_name_or_self(getter), SD(slot)->creator);
	    else
		error("The getter for slot %= in %= clashes with "
		      "the setter for slot %= inherited from %=",
		      function_debug_name_or_self(new_getter), class,
		      function_debug_name_or_self(getter), SD(slot)->creator);
	if (new_setter != obj_False) {
	    if (new_setter == getter)
		if (inherited)
		    error("The setter for slot %= inherited from %= clashes "
			  "with the getter for slot %= inherited from %=",
			  function_debug_name_or_self(new_getter),
			  SD(new_slot)->creator,
			  function_debug_name_or_self(getter),
			  SD(slot)->creator);
		else
		    error("The setter for slot %= in %= clashes "
			  "with the getter for slot %= inherited from %=",
			  function_debug_name_or_self(new_getter), class,
			  function_debug_name_or_self(getter),
			  SD(slot)->creator);
	    if (new_setter == setter)
		if (inherited)
		    error("The setter for slot %= inherited from %= clashes "
			  "with the setter for slot %= inherited from %=",
			  function_debug_name_or_self(new_getter),
			  SD(new_slot)->creator,
			  function_debug_name_or_self(getter),
			  SD(slot)->creator);
		else
		    error("The setter for slot %= in %= clashes "
			  "with the setter for slot %= inherited from %=",
			  function_debug_name_or_self(new_getter), class,
			  function_debug_name_or_self(getter),
			  SD(slot)->creator);
	}
    }

    DC(class)->all_slots = pair(new_slot, DC(class)->all_slots);
}

static obj_t classes_processed;
static obj_t displaced_instance_slots;
static obj_t displaced_each_subclass_slots;
static obj_t initializers;

static void inherit_slots(obj_t class, obj_t super)
{
    obj_t supers, new_slots;

    if (memq(super, classes_processed))
	return;
    classes_processed = pair(super, classes_processed);

    if (obj_ptr(struct class *, super)->class != obj_DefinedClassClass)
	return;

    for (supers=DC(super)->superclasses; supers!=obj_Nil; supers=TAIL(supers))
	inherit_slots(class, HEAD(supers));

    for (new_slots = DC(super)->new_slots;
	 new_slots != obj_Nil;
	 new_slots = TAIL(new_slots)) {
	obj_t new_slot = HEAD(new_slots);

	add_slot(class, new_slot, TRUE);

	switch (SD(new_slot)->alloc) {
	    int offset;
	  case alloc_INSTANCE:
	    offset = SD(new_slot)->desired_offset;
	    if (SOVEC(DC(class)->instance_layout)->contents[offset]
		  != obj_False)
		displaced_instance_slots
		    = pair(new_slot, displaced_instance_slots);
	    else
		SOVEC(DC(class)->instance_layout)->contents[offset] = new_slot;
	    break;

	  case alloc_EACH_SUBCLASS:
	    offset = SD(new_slot)->desired_offset;
	    if (SOVEC(DC(class)->each_subclass_layout)->contents[offset]
		  != obj_False)
		displaced_each_subclass_slots
		    = pair(new_slot, displaced_each_subclass_slots);
	    else {
		SOVEC(DC(class)->each_subclass_layout)->contents[offset] = new_slot;
		initializers = pair(slot_initializer(new_slot), initializers);
	    }
	    break;

	  case alloc_CLASS:
	  case alloc_VIRTUAL:
	    /* We don't need to do anything to inherit these. */
	    break;

	  default:
	    lose("Strange slot allocation.");
	}
    }
}

static obj_t compute_positions(obj_t displaced_slots, obj_t layout)
{
    int index = 0;
    obj_t res;

    if (displaced_slots == obj_Nil)
	return obj_False;

    res = make_position_table();
    while (displaced_slots != obj_Nil) {
	obj_t slot = HEAD(displaced_slots);
	while (SOVEC(layout)->contents[index] != obj_False)
	    index++;
	SOVEC(layout)->contents[index] = slot;
	note_position(res, slot, index);
	displaced_slots = TAIL(displaced_slots);
    }

    return res;
}

static void process_slot(obj_t class, obj_t slot)
{
    int offset = SD(slot)->desired_offset;
    obj_t value_cell;

    SD(slot)->creator = class;

    add_slot(class, slot, FALSE);

    switch (SD(slot)->alloc) {
      case alloc_INSTANCE:
	SOVEC(DC(class)->instance_layout)->contents[offset] = slot;
	SD(slot)->getter_method
	    = make_accessor_method(function_debug_name(SD(slot)->getter),
				   class, SD(slot)->type,
				   FALSE, make_fixnum(offset),
				   fast_instance_getter);
	add_method(SD(slot)->getter, SD(slot)->getter_method);
	if (SD(slot)->setter != obj_False) {
	    SD(slot)->setter_method
		= make_accessor_method(function_debug_name(SD(slot)->setter),
				       class, SD(slot)->type,
				       TRUE, make_fixnum(offset),
				       fast_instance_setter);
	    add_method(SD(slot)->setter, SD(slot)->setter_method);
	}
	break;

      case alloc_EACH_SUBCLASS:
	SOVEC(DC(class)->each_subclass_layout)->contents[offset] = slot;
	initializers = pair(slot_initializer(slot), initializers);
	SD(slot)->getter_method
	    = make_accessor_method(function_debug_name(SD(slot)->getter),
				   class, SD(slot)->type,
				   FALSE, make_fixnum(offset),
				   fast_each_subclass_getter);
	add_method(SD(slot)->getter, SD(slot)->getter_method);
	if (SD(slot)->setter != obj_False) {
	    SD(slot)->setter_method
		= make_accessor_method(function_debug_name(SD(slot)->setter),
				       class, SD(slot)->type, TRUE,
				       make_fixnum(offset),
				       fast_each_subclass_setter);
	    add_method(SD(slot)->setter, SD(slot)->setter_method);
	}
	break;

      case alloc_CLASS:
	value_cell = make_value_cell(obj_Unbound);
	initializers = pair(slot_initializer(slot), initializers);
	SD(slot)->getter_method
	    = make_accessor_method(function_debug_name(SD(slot)->getter),
				   class, SD(slot)->type,
				   FALSE, value_cell, class_getter);
	add_method(SD(slot)->getter, SD(slot)->getter_method);
	if (SD(slot)->setter != obj_False) {
	    SD(slot)->setter_method
		= make_accessor_method(function_debug_name(SD(slot)->setter),
				       class, SD(slot)->type,
				       TRUE, value_cell, class_setter);
	    add_method(SD(slot)->setter, SD(slot)->setter_method);
	}
	break;

      case alloc_VIRTUAL:
	/* Don't need to add any methods. */
	break;

      default:
	lose("Strange slot allocation.");
    }
}


/* Process Initarg Specifications */

#define conflicting_initargs(initarg1, initarg2) \
    (INTD(initarg1)->type != INTD(initarg2)->type \
     || INTD(initarg1)->required_p != INTD(initarg2)->required_p \
     || INTD(initarg1)->init_function_or_value \
       != INTD(initarg2)->init_function_or_value)

static void inherit_initargs(obj_t class, obj_t super)
{
    obj_t inh_initargs;
    obj_t new_initargs;
    obj_t all_initargs;

    if (obj_ptr(struct class *, super)->class != obj_DefinedClassClass)
        return;

    for (inh_initargs = DC(super)->new_initargs; inh_initargs != obj_Nil;
	 inh_initargs = TAIL(inh_initargs)) {
	obj_t inh_initarg = HEAD(inh_initargs);
	boolean redefined = FALSE;
	boolean inherited = FALSE;

	for (new_initargs = DC(class)->new_initargs; new_initargs != obj_Nil;
	     new_initargs = TAIL(new_initargs)) {
	    obj_t initarg = HEAD(new_initargs);

	    if (INTD(inh_initarg)->keyword == INTD(initarg)->keyword) {
		/* Check that the type is a subtype of the inherited */
		if (!subtypep(INTD(initarg)->type, INTD(inh_initarg)->type))
		    error("Incompatible init arg type for %=.",
			  INTD(initarg)->keyword);
		/* Determine whether initarg is required */
		if (INTD(inh_initarg)->required_p
		      && INTD(initarg)->init_function_or_value == obj_Unbound)
		    INTD(initarg)->required_p = TRUE;
		redefined = TRUE;
		break;
	    }
	}
	if (redefined)
	    break;
	for (all_initargs = DC(class)->all_initargs; all_initargs != obj_Nil;
	     all_initargs = TAIL(all_initargs)) {
	    obj_t initarg = HEAD(all_initargs);

	    if (INTD(inh_initarg)->keyword == INTD(initarg)->keyword) {
		/* Determine whether definitions are the same */
		if (conflicting_initargs(inh_initarg, initarg))
		    error("Conflicting inherited definitions of init arg %=",
			  INTD(initarg)->keyword);
		inherited = TRUE;
	    }
	}
	if (!redefined && !inherited) {
	    DC(class)->all_initargs =
	      pair(inh_initarg, DC(class)->all_initargs);
	}
    }
}


/* Process Inherited Specifications */

static obj_t maybe_add_override(obj_t override, obj_t overrides)
{
    obj_t scan;
    obj_t name = INHD(override)->name;

    for (scan = overrides; scan != obj_Nil; scan = TAIL(scan))
	if (INHD(HEAD(scan))->name == name)
	    return overrides;

    return pair(override, overrides);
}

static obj_t maybe_inherit_inheriteds(obj_t class, obj_t overrides)
{
    obj_t scan;

    if (CLASS(class)->class == obj_DefinedClassClass)
	for (scan = DC(class)->new_inheriteds;
	     scan != obj_Nil;
	     scan = TAIL(scan))
	    overrides = maybe_add_override(HEAD(scan), overrides);
    return overrides;
}

static obj_t process_inherited(obj_t class, obj_t inherited, obj_t overrides)
{
    obj_t slots, slot, inits;

    for (slots = DC(class)->all_slots; slots != obj_Nil; slots = TAIL(slots))
	if (SD(slot = HEAD(slots))->name == INHD(inherited)->name)
	    break;

    if (slots == obj_Nil) {
	error("Slot %= not inherited from any superclass",
	      INHD(inherited)->name);
    }
    else if (INHD(inherited)->init_function_or_value != obj_Unbound) {
	switch (SD(slot)->alloc) {
	  case alloc_INSTANCE:
	    overrides = pair(inherited, overrides);
	    break;

	  case alloc_EACH_SUBCLASS:
	    for (inits = initializers; inits != obj_Nil;
		 inits = TAIL(inits)) {
		obj_t init = HEAD(inits);
		
		if (INITIALIZER(init)->slot == slot) {
		    HEAD(inits) = inherited_initializer(slot, inherited);
		    break;
		}
	    }
	    if (inits == obj_Nil) {
		initializers
		    = pair(inherited_initializer(slot, inherited),
			   initializers);
	    }
	    break;
	  case alloc_CLASS:
	    if (INHD(inherited)->init_function_or_value != obj_Unbound)
		error("Can't init inherited class slot %=",
		      INHD(inherited)->name);
	    break;
	  case alloc_VIRTUAL:
	    if (INHD(inherited)->init_function_or_value != obj_Unbound)
		error("Can't init inherited virtual slot %=",
		      INHD(inherited)->name);
	    break;
	  default:
	    lose("Strange slot allocation.");
	}
    }

    return overrides;
}


/* Initialize Defined Class */

void init_defined_class(obj_t class, obj_t slots,
			obj_t initargs, obj_t inheriteds, obj_t abstractp)
{
    obj_t scan;
    obj_t overrides;

    if (object_class(class) != obj_DefinedClassClass) {
	if (slots != obj_Nil)
	    error("Cannot add slots to %= classes", object_class(class));
	if (initargs != obj_Nil)
	    error("Initialization args not supported for class %=.", class);
	if (object_class(class) == obj_StaticTypeClass)
	    /* Statically typed pointers have special requirements.  This
	       isn't the best place for this, but it must be done after all
	       superclasses have been fully initialized, so this is what
	       works */
	    for (scan = CLASS(class)->superclasses;
		 scan != obj_Nil; scan = TAIL(scan))
	    {
		obj_t super = HEAD(scan);
	    
		if (object_class(super) == obj_StaticTypeClass)
		    continue;

		if (object_class(super) == obj_DefinedClassClass) {
		    if (DC(super)->all_slots != obj_Nil
			| DC(super)->all_initargs != obj_Nil)
			error("Can't mix normal class %= with "
			      "statically typed pointer classes in %=",
			      super, class);
		} else if (!CLASS(super)->abstract_p)
		    error("Can't mix normal class %= with "
			  "statically typed pointer classes in %=",
			  super, class);
	    }
	if (inheriteds != obj_Nil)
	    error("Inherited slots are not accepted for class %=.", class);
	do_initialization(class, obj_Nil, obj_Nil);
    }

    DC(class)->abstract_p = !idp(abstractp, obj_False);
    DC(class)->new_slots = slots;
    DC(class)->all_slots = obj_Nil;
    DC(class)->new_initargs = initargs;
    DC(class)->all_initargs = initargs;

    compute_lengths(class);

    /* Process Slots */

    classes_processed = obj_Nil;
    displaced_instance_slots = obj_Nil;
    displaced_each_subclass_slots = obj_Nil;
    initializers = obj_Nil;

    for (scan = DC(class)->superclasses; scan != obj_Nil; scan = TAIL(scan))
	inherit_slots(class, HEAD(scan));

    DC(class)->instance_positions
	= compute_positions(displaced_instance_slots,
			    DC(class)->instance_layout);
    DC(class)->each_subclass_positions
	= compute_positions(displaced_each_subclass_slots,
			    DC(class)->each_subclass_layout);

    classes_processed = NULL;
    displaced_instance_slots = NULL;
    displaced_each_subclass_slots = NULL;

    for (scan = slots; scan != obj_Nil; scan = TAIL(scan))
	process_slot(class, HEAD(scan));

    /* Process Initargs */

    for (scan = TAIL(DC(class)->cpl); scan != obj_Nil; scan = TAIL(scan))
	inherit_initargs(class, HEAD(scan));

    /* Process Inheriteds */

    overrides = obj_Nil;
    for (scan = inheriteds; scan != obj_Nil; scan = TAIL(scan))
        overrides = process_inherited(class, HEAD(scan), overrides);
    DC(class)->new_inheriteds = overrides;
    for (scan = TAIL(DC(class)->cpl); scan != obj_Nil; scan = TAIL(scan))
	overrides = maybe_inherit_inheriteds(HEAD(scan), overrides);
    DC(class)->all_inheriteds = overrides;

    scan = initializers;
    initializers = NULL;
    do_initialization(class, obj_Nil, scan);
}
    

/* Make and Initialize Instances */

static obj_t dylan_make(obj_t class, obj_t key_and_value_pairs)
{
    error("Can't make instances of %= with the default make method.",
	  class);
    return NULL;
}

static obj_t compute_defaulted_initargs(obj_t class, obj_t keyword_arg_pairs)
{
    int i;
    int nkeys = SOVEC(keyword_arg_pairs)->length;
    obj_t supplied_initargs = obj_Nil;
    obj_t defaulted_initargs;
    obj_t supplieds;
    obj_t initargs;

    /* Get the supplied initialization arguments */
    if ((nkeys & 1) != 0) {
	/* I'm not sure this can ever happen (maybe the caller of this
	   function take care of this), but just in case... 
	 */
	error("More keywords than values supplied");
    }
    /* Iterate backwards so we don't change their order */
    for (i = nkeys-2; i >= 0; i -= 2) {
	obj_t initarg =
	  make_initarg_descr(SOVEC(keyword_arg_pairs)->contents[i],
			     obj_False, obj_False, obj_Unbound,
			     SOVEC(keyword_arg_pairs)->contents[i+1]);
	supplied_initargs = pair(initarg, supplied_initargs);
    }

    /* Augment supplied initialization arguments with defaults */

    defaulted_initargs = supplied_initargs;

    for (initargs = DC(class)->all_initargs; initargs != obj_Nil;
	 initargs = TAIL(initargs)) {
	obj_t initarg = HEAD(initargs);
	boolean found = FALSE;

	for (supplieds = supplied_initargs; supplieds != obj_Nil;
	     supplieds = TAIL(supplieds)) {
	    obj_t supplied = HEAD(supplieds);

	    if (INTD(initarg)->keyword == INTD(supplied)->keyword) {
		if (!instancep(INTD(supplied)->init_function_or_value,
			       INTD(initarg)->type))
		    error("Keyword arg %= must have type %=",
			  INTD(initarg)->keyword, INTD(initarg)->type);
	        found = TRUE;
		break;
	    }
	}
	if (!found) {
	    if (INTD(initarg)->required_p)
	        error("Required init arg %= not supplied",
		      INTD(initarg)->keyword);
	    else
		defaulted_initargs = pair(initarg, defaulted_initargs);
	}
    }
    return defaulted_initargs;
}

static void check_init_keyword_validity(obj_t class, obj_t initargs)
{
    obj_t scan;
    obj_t valid_keywords;
    obj_t initialize = initialize_gf_variable->value;
    obj_t init_methods_clock = generic_function_methods_clock(initialize);

    if (DC(class)->valid_init_keywords_clock == init_methods_clock)
	valid_keywords = DC(class)->valid_init_keywords;
    else {
	valid_keywords = obj_Nil;

	for (scan = DC(class)->all_slots; scan != obj_Nil; scan = TAIL(scan)) {
	    obj_t key = SD(HEAD(scan))->init_keyword;

	    if (key != obj_False && !memq(key, valid_keywords))
		valid_keywords = pair(key, valid_keywords);
	}

	for (scan = generic_function_methods(initialize);
	     scan != obj_Nil;
	     scan = TAIL(scan)) {
	    obj_t method = HEAD(scan);

	    if (subtypep(class, HEAD(function_specializers(method)))) {
		obj_t keys = function_keywords(method);

		if (function_all_keywords_p(method)) {
		    valid_keywords = obj_True;
		    break;
		}

		/* keys should always be a list, because this is a method on */
		/* initialize after all, but it doesn't hurt to make sure. */
		if (keys != obj_False) {
		    while (keys != obj_Nil) {
			obj_t key = HEAD(HEAD(keys));
			
			if (!memq(key, valid_keywords))
			    valid_keywords = pair(key, valid_keywords);
			
			keys = TAIL(keys);
		    }
		}
	    }
	}

	DC(class)->valid_init_keywords = valid_keywords;
	DC(class)->valid_init_keywords_clock = init_methods_clock;
    }

    if (valid_keywords != obj_True)
	for (scan = initargs; scan != obj_Nil; scan = TAIL(scan))
	    if (!memq(INTD(HEAD(scan))->keyword, valid_keywords))
		error("Invalid init keyword, %=, in make of %=",
		      INTD(HEAD(scan))->keyword, class);
}

static obj_t dylan_make_instance(obj_t class, obj_t keyword_arg_pairs)
{
    int length = DC(class)->instance_length;
    obj_t res = alloc(class,
		      sizeof(struct instance) + (length - 1) * sizeof(obj_t));
    obj_t defaulted_initargs;
    obj_t slots;
    obj_t initargs;
    obj_t inits;
    int i;

    /* Fill the instance in with something so that the garbage collector */
    /* doesn't get annoyed. */
    for (i = 0; i < length; i++)
	INST(res)->slots[i] = obj_Unbound;

    if (DC(class)->all_slots == obj_False)
	error("Attempt to make an instance of %= before\n"
	      "the define class for it has been processed.",
	      class);

    if (DC(class)->abstract_p == TRUE)
	error("Attempt to instantiate the abstract class %= with\n"
	      "the default make method.",
	      class);

    initializers = obj_Nil;

    defaulted_initargs = compute_defaulted_initargs(class, keyword_arg_pairs);

    check_init_keyword_validity(class, defaulted_initargs);

    for (slots = DC(class)->all_slots; slots != obj_Nil; slots = TAIL(slots)) {
	obj_t slot = HEAD(slots);
	boolean slot_initialized_p = FALSE;
	obj_t keyword = SD(slot)->init_keyword;

	/* Check for keyword init value */

	if (keyword != obj_False && !slot_initialized_p) {
	    obj_t initargs;
	    boolean suppliedp = FALSE;

	    for (initargs = defaulted_initargs; initargs != obj_Nil;
		 initargs = TAIL(initargs)) {
		obj_t initarg = HEAD(initargs);

		if (INTD(initarg)->keyword == keyword) {
		    obj_t initializer
		      = initarg_slot_initializer(slot, initarg);

		    INTD(initarg)->initializer = initializer;
		    initializers = pair(initializer, initializers);
		    slot_initialized_p = TRUE;
		    suppliedp = TRUE;
		    break;
		}
	    }
	    if (SD(slot)->keyword_required && !suppliedp)
		error("Missing required init-keyword %=", keyword);
	}

	/* Check for inherited spec init value */

	if (!slot_initialized_p) {
	    obj_t inheriteds;

	    for (inheriteds = DC(class)->all_inheriteds; inheriteds != obj_Nil;
		 inheriteds = TAIL(inheriteds)) {
		obj_t inherited = HEAD(inheriteds);
		
		if (SD(slot)->name == INHD(inherited)->name
		      && SD(slot)->alloc == alloc_INSTANCE) {
		    obj_t initializer
		      = inherited_initializer(slot, inherited);

		    initializers = pair(initializer, initializers);
		    slot_initialized_p = TRUE;
		    break;
		}
	    }
	}

	/* Check for slot spec init value */

	if (!slot_initialized_p && SD(slot)->alloc == alloc_INSTANCE) {
	    obj_t initializer = slot_initializer(slot);

	    initializers = pair(initializer, initializers);
	    slot_initialized_p = TRUE;
	}
    }

    for (initargs = defaulted_initargs; initargs != obj_Nil;
	 initargs = TAIL(initargs)) {
	obj_t initarg = HEAD(initargs);

	if (INTD(initarg)->initializer == obj_False) {
	    obj_t initializer = initarg_initializer(initarg);

	    INTD(initarg)->initializer = initializer;
	    initializers = pair(initializer, initializers);
	}
    }

    inits = initializers;
    initializers = NULL;
    do_initialization(res, defaulted_initargs, inits);

    return NULL;
}

static obj_t dylan_init(obj_t object, obj_t key_val_pairs)
{
    return obj_False;
}



/* Other routines. */

static obj_t dylan_slot_initialized_p(obj_t instance, obj_t getter)
{
    obj_t class = object_class(instance);
    obj_t scan, slot;
    int index;
    obj_t value = NULL;

    if (object_class(class) != obj_DefinedClassClass)
	error("%= doesn't access a slot in %=", getter, instance);

    for (scan = DC(class)->all_slots; scan != obj_Nil; scan = TAIL(scan)) {
	slot = HEAD(scan);
	if (SD(slot)->getter == getter) {
	    switch (SD(slot)->alloc) {
	      case alloc_INSTANCE:
		index = find_position(DC(class)->instance_positions, slot);
		value = INST(instance)->slots[index];
		break;
	      case alloc_EACH_SUBCLASS:
		index = find_position(DC(class)->each_subclass_positions, slot);
		value = INST(instance)->slots[index];
		break;
	      case alloc_CLASS:
		value = value_cell_ref(accessor_method_datum
				       (SD(slot)->getter_method));
		break;
	      case alloc_VIRTUAL:
		value = obj_False;
		break;
	      default:
		lose("Strange slot allocation.");
	    }
	    if (value == obj_Unbound)
		return obj_False;
	    else
		return obj_True;
	}
    }

    error("%= doesn't access a slot in %=", getter, instance);    
    return NULL;
}


/* Introspection stuff. */

static obj_t dylan_class_slot_descriptors(obj_t class)
{
    return obj_Nil;
}

static obj_t dylan_dc_slot_descriptors(obj_t class)
{
    return DC(class)->all_slots;
}

static obj_t dylan_slot_name(obj_t slot)
{
    return SD(slot)->name;
}

static obj_t dylan_slot_alloc(obj_t slot)
{
    switch (SD(slot)->alloc) {
      case alloc_INSTANCE:
	return symbol("instance");
      case alloc_CLASS:
	return symbol("class");
      case alloc_EACH_SUBCLASS:
	return symbol("each-subclass");
      case alloc_VIRTUAL:
	return symbol("virtual");
      default:
	lose("Bogus kind of allocation in slot descriptor");
	return obj_False;
    }
}

static obj_t dylan_slot_getter(obj_t slot)
{
    return SD(slot)->getter;
}

static obj_t dylan_slot_getter_method(obj_t slot)
{
    return SD(slot)->getter_method;
}

static obj_t dylan_slot_setter(obj_t slot)
{
    return SD(slot)->setter;
}

static obj_t dylan_slot_setter_method(obj_t slot)
{
    return SD(slot)->setter_method;
}

static obj_t dylan_slot_type(obj_t slot)
{
    return SD(slot)->type;
}

static void dylan_slot_value(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t *old_sp = args - 1;
    obj_t slot = args[0];
    obj_t instance = args[1];
    obj_t class = object_class(instance);
    int index;
    obj_t value = NULL;

    if (!instancep(instance, SD(slot)->creator))
	error("%= is not one of %='s slots", slot, instance);

    switch (SD(slot)->alloc) {
      case alloc_INSTANCE:
	index = find_position(DC(class)->instance_positions, slot);
	value = INST(instance)->slots[index];
	break;
      case alloc_EACH_SUBCLASS:
	index = find_position(DC(class)->each_subclass_positions, slot);
	value = INST(instance)->slots[index];
	break;
      case alloc_CLASS:
	value = value_cell_ref(accessor_method_datum
			       (SD(slot)->getter_method));
	break;
      case alloc_VIRTUAL:
	value = obj_Unbound;
	break;
      default:
	lose("Strange slot allocation.");
    }

    thread->sp = old_sp + 2;

    if (value == obj_Unbound) {
	old_sp[0] = obj_False;
	old_sp[1] = obj_False;
    }
    else {
	old_sp[0] = value;
	old_sp[1] = obj_True;
    }
	
    do_return(thread, old_sp, old_sp);
}

static obj_t dylan_slot_value_setter(obj_t value, obj_t slot, obj_t instance)
{
    obj_t class = object_class(instance);
    int index;

    if (!instancep(instance, SD(slot)->creator))
	error("%= is not one of %='s slots", slot, instance);

    check_type(value, SD(slot)->type);

    switch (SD(slot)->alloc) {
      case alloc_INSTANCE:
	index = find_position(DC(class)->instance_positions, slot);
	INST(instance)->slots[index] = value;
	break;
      case alloc_EACH_SUBCLASS:
	index = find_position(DC(class)->each_subclass_positions, slot);
	INST(instance)->slots[index] = value;
	break;
      case alloc_CLASS:
	value_cell_set(accessor_method_datum(SD(slot)->getter_method), value);
	break;
      case alloc_VIRTUAL:
	error("virtual slots cannot be set.");
	break;
      default:
	lose("Strange slot allocation.");
    }

    return value;
}

static obj_t dylan_slot_keyword_required_p(obj_t slot)
{
    return (SD(slot)->keyword_required) ? obj_True : obj_False;
}

static obj_t dylan_slot_init_keyword(obj_t slot)
{
    return SD(slot)->init_keyword;
}


/* Describe. */

void describe(obj_t thing)
{
    obj_t class = object_class(thing);
    obj_t slots;

    prin1(thing);
    printf(" is an instance of ");
    print(class);

    if (object_class(class) == obj_DefinedClassClass) {
	printf("and has the following slots:\n");

	for (slots=DC(class)->all_slots; slots != obj_Nil; slots=TAIL(slots)) {
	    obj_t slot = HEAD(slots);
	    int index, dummy;
	    obj_t value = NULL;

	    fputs(sym_name(SD(slot)->name), stdout);
	    switch (SD(slot)->alloc) {
	      case alloc_INSTANCE:
		index = find_position(DC(class)->instance_positions, slot);
		value = INST(thing)->slots[index];
		break;
	      case alloc_EACH_SUBCLASS:
		printf("[each-subclass]");
		index = find_position(DC(class)->each_subclass_positions, slot);
		value = INST(thing)->slots[index];
		break;
	      case alloc_CLASS:
		value = value_cell_ref(accessor_method_datum
				       (SD(slot)->getter_method));
		printf("[class]");
		break;
	      case alloc_VIRTUAL:
		printf("[virtual]\n");
		goto after_value_printing;
	      default:
		lose("Strange slot allocation.");
	    }

	    if (value == obj_Unbound)
		printf(" is unbound\n");
	    else {
		printf(": ");
		print(value);
	    }
	  after_value_printing:
	    dummy = 0;    /* Without this line, a few compilers will choke */
	}
    }
}


/* GC routines. */

static int scav_defined_class(struct object *ptr)
{
    struct defined_class *class = (struct defined_class *)ptr;

    scavenge(&class->debug_name);
    scavenge(&class->superclasses);
    scavenge(&class->cpl);
    scavenge(&class->direct_subclasses);
    scavenge(&class->all_subclasses);
    scavenge(&class->new_slots);
    scavenge(&class->all_slots);
    scavenge(&class->new_initargs);
    scavenge(&class->all_initargs);
    scavenge(&class->new_inheriteds);
    scavenge(&class->all_inheriteds);
    scavenge(&class->valid_init_keywords);
    scavenge(&class->valid_init_keywords_clock);
    scavenge(&class->instance_positions);
    scavenge(&class->instance_layout);
    scavenge(&class->each_subclass_positions);
    scavenge(&class->each_subclass_slots);
    scavenge(&class->each_subclass_layout);

    return sizeof(struct defined_class);
}

static obj_t trans_defined_class(obj_t class)
{
    return transport(class, sizeof(struct defined_class), FALSE);
}

static int scav_slot_descr(struct object *ptr)
{
    struct slot_descr *slot = (struct slot_descr *)ptr;

    scavenge(&slot->name);
    scavenge(&slot->creator);
    scavenge(&slot->init_function_or_value);
    scavenge(&slot->init_keyword);
    scavenge(&slot->getter);
    scavenge(&slot->getter_method);
    scavenge(&slot->setter);
    scavenge(&slot->setter_method);
    scavenge(&slot->type);

    return sizeof(struct slot_descr);
}

static obj_t trans_slot_descr(obj_t slot)
{
    return transport(slot, sizeof(struct slot_descr), FALSE);
}

static int scav_initarg_descr(struct object *ptr)
{
    struct initarg_descr *initarg = (struct initarg_descr *)ptr;

    scavenge(&initarg->keyword);
    scavenge(&initarg->type);
    scavenge(&initarg->init_function_or_value);

    return sizeof(struct initarg_descr);
}

static obj_t trans_initarg_descr(obj_t initarg)
{
    return transport(initarg, sizeof(struct initarg_descr), FALSE);
}

static int scav_inherited_descr(struct object *ptr)
{
    struct inherited_descr *inherited = (struct inherited_descr *)ptr;

    scavenge(&inherited->name);
    scavenge(&inherited->init_function_or_value);

    return sizeof(struct inherited_descr);
}

static obj_t trans_inherited_descr(obj_t inherited)
{
    return transport(inherited, sizeof(struct inherited_descr), FALSE);
}

static int scav_postable(struct object *ptr)
{
    struct postable *p = (struct postable *)ptr;

    scavenge(&p->alist);

    return sizeof(struct postable);
}

static obj_t trans_postable(obj_t p)
{
    return transport(p, sizeof(struct postable), FALSE);
}

static int scav_initializer(struct object *ptr)
{
    struct initializer *p = (struct initializer *)ptr;

    scavenge(&p->slot);
    scavenge(&p->initarg);
    scavenge(&p->inherited);

    return sizeof(struct initializer);
}

static obj_t trans_initializer(obj_t p)
{
    return transport(p, sizeof(struct initializer), FALSE);
}

static int scav_instance(struct object *ptr)
{
    struct instance *instance = (struct instance *)ptr;
    int nslots = DC(ptr->class)->instance_length;
    int i;

    for (i = 0; i < nslots; i++)
	scavenge(instance->slots + i);

    return sizeof(struct instance) + sizeof(obj_t)*(nslots - 1);
}

static obj_t trans_instance(obj_t instance)
{
    obj_t class = INST(instance)->class;
    int nslots = DC(class)->instance_length;

    return transport(instance,
		     sizeof(struct instance) + sizeof(obj_t)*(nslots-1),
		     FALSE);
}


/* Init stuff. */

void make_instance_classes(void)
{
    obj_DefinedClassClass
	= make_builtin_class(scav_defined_class, trans_defined_class);
    obj_SlotDescrClass = make_builtin_class(scav_slot_descr, trans_slot_descr);
    obj_InitargDescrClass =
      make_builtin_class(scav_initarg_descr, trans_initarg_descr);
    obj_InheritedDescrClass =
      make_builtin_class(scav_inherited_descr, trans_inherited_descr);
    obj_PosTableClass = make_builtin_class(scav_postable, trans_postable);
    obj_InitializerClass =
      make_builtin_class(scav_initializer, trans_initializer);

    add_constant_root(&obj_DefinedClassClass);
    add_constant_root(&obj_SlotDescrClass);
    add_constant_root(&obj_InitargDescrClass);
    add_constant_root(&obj_InheritedDescrClass);
    add_constant_root(&obj_PosTableClass);
    add_constant_root(&obj_InitializerClass);
}

void init_instance_classes(void)
{
    init_builtin_class(obj_DefinedClassClass, "<defined-class>",
		       obj_ClassClass, NULL);
    init_builtin_class(obj_SlotDescrClass, "<slot-descriptor>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_InitargDescrClass, "<initarg-descriptor>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_InheritedDescrClass, "<inherited-descriptor>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_PosTableClass, "<position-table>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_InitializerClass, "<initializer>",
		       obj_ObjectClass, NULL);
}

void init_instance_functions(void)
{
    obj_t obj_FalseClass = object_class(obj_False);

    define_function("make-slot",
		    listn(5, obj_ObjectClass, obj_FixnumClass,
			  obj_FunctionClass,
			  type_union(obj_FunctionClass, obj_FalseClass),
			  type_union(obj_TypeClass, obj_FalseClass)),
		    FALSE,
		    listn(4, pair(symbol("init-keyword"), obj_False),
			  pair(symbol("required-init-keyword"), obj_False),
			  pair(symbol("init-function"), obj_Unbound),
			  pair(symbol("init-value"), obj_Unbound)),
		    FALSE, obj_SlotDescrClass, make_slot_descriptor);
    define_function("make-initarg",
		    list2(obj_ObjectClass, obj_ObjectClass),
		    FALSE,
		    list3(pair(symbol("type"), obj_False),
			  pair(symbol("init-function"), obj_Unbound),
			  pair(symbol("init-value"), obj_Unbound)),
		    FALSE, obj_InitargDescrClass, make_initarg_descr);
    define_function("make-inherited",
		    list1(obj_ObjectClass),
		    FALSE,
		    list2(pair(symbol("init-function"), obj_Unbound),
			  pair(symbol("init-value"), obj_Unbound)),
		    FALSE, obj_InheritedDescrClass, make_inherited_descr);
    define_generic_function("make", list1(obj_TypeClass), 
			    FALSE, obj_Nil, TRUE,
			    list1(obj_ObjectClass), obj_False);
    define_method("make", list1(obj_ClassClass), TRUE, obj_Nil, FALSE,
		  obj_ObjectClass, dylan_make);
    define_method("make", list1(obj_DefinedClassClass), TRUE, obj_Nil, FALSE,
		  obj_ObjectClass, dylan_make_instance);
    define_generic_function("initialize", list1(obj_ObjectClass), 
			    FALSE, obj_Nil, TRUE,
			    obj_Nil, obj_ObjectClass);
    define_method("initialize", list1(obj_ObjectClass), TRUE, obj_Nil, FALSE,
		  obj_ObjectClass, dylan_init);
    initialize_gf_variable =
      find_variable(module_BuiltinStuff, symbol("initialize"), FALSE, TRUE);

    define_generic_function("slot-initialized?", 
			    list2(obj_ObjectClass, obj_GFClass), 
			    FALSE, obj_Nil, FALSE,
			    list1(obj_BooleanClass), obj_ObjectClass);
    define_method("slot-initialized?",
		  list2(obj_ObjectClass, obj_GFClass),
		  FALSE, obj_Nil, FALSE, obj_BooleanClass,
		  dylan_slot_initialized_p);

    define_method("slot-descriptors", list1(obj_ClassClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass,
		  dylan_class_slot_descriptors);
    define_method("slot-descriptors", list1(obj_DefinedClassClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass,
		  dylan_dc_slot_descriptors);
    define_method("slot-name", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_name);
    define_method("slot-allocation", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_alloc);
    define_method("slot-getter", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_getter);
    define_method("slot-getter-method", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_getter_method);
    define_method("slot-setter", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_setter);
    define_method("slot-setter-method", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_setter_method);
    define_method("slot-type", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, dylan_slot_type);
    define_generic_function("slot-value", 
			    list2(obj_SlotDescrClass, obj_ObjectClass), 
			    FALSE, obj_False, FALSE,
			    list2(obj_ObjectClass, obj_BooleanClass),
			    obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("slot-value"),
			     FALSE, FALSE)->value,
	       make_raw_method("slot-value",
			       list2(obj_SlotDescrClass, obj_ObjectClass),
			       FALSE, obj_False, FALSE,
			       list2(obj_ObjectClass, obj_BooleanClass),
			       obj_False, dylan_slot_value));
    define_method("slot-value-setter",
		  list3(obj_ObjectClass, obj_SlotDescrClass,obj_ObjectClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass,
		  dylan_slot_value_setter);
    define_method("keyword-required?", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_BooleanClass, 
		  dylan_slot_keyword_required_p);
    define_method("init-keyword", list1(obj_SlotDescrClass), FALSE,
		  obj_False, FALSE, obj_ObjectClass, 
		  dylan_slot_init_keyword);
}
