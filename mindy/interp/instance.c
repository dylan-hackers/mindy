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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/instance.c,v 1.16 1994/07/11 20:29:39 dpierce Exp $
*
* This file implements instances and user defined classes.
*
\**********************************************************************/

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
#include "instance.h"

struct defined_class {
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

    obj_t new_slots;
    obj_t all_slots;
    obj_t new_initargs;
    obj_t all_initargs;
    obj_t inheriteds;

    obj_t instance_positions;
    int instance_length;
    obj_t instance_layout;

    obj_t subclass_positions;
    obj_t subclass_slots;
    obj_t subclass_layout;
};

#define DC(o) obj_ptr(struct defined_class *, o)

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
    obj_t slots[0];
};

#define INST(o) obj_ptr(struct instance *, o)

static obj_t obj_DefinedClassClass = NULL;
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

static void slow_subclass_getter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t instance = args[0];
    obj_t class = INST(instance)->class;
    int index = find_position(DC(class)->subclass_positions, datum);
    obj_t value = SOVEC(DC(class)->subclass_slots)->contents[index];

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void fast_subclass_getter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t instance = args[0];
    obj_t class = INST(instance)->class;
    int index = fixnum_value(datum);
    obj_t value = SOVEC(DC(class)->subclass_slots)->contents[index];

    if (value == obj_Unbound) {
	push_linkage(thread, args);
	error("Unbound slot.");
    }

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void slow_subclass_setter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];
    obj_t instance = args[1];
    obj_t class = INST(instance)->class;
    int index = find_position(DC(class)->subclass_positions, datum);

    SOVEC(DC(class)->subclass_slots)->contents[index] = value;

    *old_sp = value;
    thread->sp = args;
    do_return(thread, old_sp, old_sp);
}

static void fast_subclass_setter(obj_t method, struct thread *thread,
				 obj_t *args)
{
    obj_t datum = accessor_method_datum(method);
    obj_t *old_sp = args-1;
    obj_t value = args[0];
    obj_t instance = args[1];
    obj_t class = INST(instance)->class;
    int index = fixnum_value(datum);

    SOVEC(DC(class)->subclass_slots)->contents[index] = value;

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

static void constant_getter(obj_t method, struct thread *thread, obj_t *args)
{
    obj_t value = accessor_method_datum(method);
    obj_t *old_sp = args-1;

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

	  case alloc_SUBCLASS:
	    set_method_iep(SD(slot)->getter_method, slow_subclass_getter);
	    set_accessor_method_datum(SD(slot)->getter_method, slot);
	    if (SD(slot)->setter_method != obj_False) {
		set_method_iep(SD(slot)->setter_method, slow_subclass_setter);
		set_accessor_method_datum(SD(slot)->setter_method, slot);
	    }
	    break;

	  default:
	    lose("Displacing a slot with allocation other than "
		 "instance or subclass?");
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
    if (alloc == alloc_CONSTANT) {
	if (init_value == obj_Unbound)
	    error("CONSTANT slots must have an init-value:");
	if (req_init_keyword != obj_False)
	    error("Can't use required-init-keyword: in constant slots.");
	if (init_keyword != obj_False)
	    error("Can't use init-keyword: in constant slots.");
    }
    if (init_function != obj_Unbound) {
	if (init_value != obj_Unbound)
	    error("Can't specify both an init-function: and an init-value:");
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
	    switch (SD(slot)->alloc) {
	      case alloc_SUBCLASS:
		index = find_position(DC(class)->subclass_positions, slot);
		SOVEC(DC(class)->subclass_slots)->contents[index] = value;
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
	    switch (SD(slot)->alloc) {
	      case alloc_INSTANCE:
		index = find_position(DC(class)->instance_positions, slot);
		INST(instance)->slots[index] = value;
		break;
	      case alloc_SUBCLASS:
		index = find_position(DC(class)->subclass_positions, slot);
		SOVEC(DC(class)->subclass_slots)->contents[index] = value;
		break;
	      case alloc_CLASS:
		value_cell_set(accessor_method_datum(SD(slot)->getter_method),
			       value);
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

obj_t make_defined_class(obj_t debug_name, struct library *library)
{
    static int scav_instance(struct object *ptr);
    static obj_t trans_instance(obj_t instance);

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
    DC(res)->inheriteds = obj_False;
    DC(res)->instance_positions = obj_False;
    DC(res)->instance_length = 0;
    DC(res)->instance_layout = obj_False;
    DC(res)->subclass_positions = obj_False;
    DC(res)->subclass_slots = obj_False;
    DC(res)->subclass_layout = obj_False;

    return res;
}

static void compute_lengths(obj_t class)
{
    obj_t scan, slots, layout;
    int instance_length = 0;
    int subclass_length = 0;
    int i;

    for (scan = TAIL(DC(class)->cpl); scan != obj_Nil; scan = TAIL(scan)) {
	obj_t super = HEAD(scan);
	if (obj_ptr(struct class *, super)->class == obj_DefinedClassClass) {
	    for (slots=DC(super)->new_slots;slots!=obj_Nil;slots=TAIL(slots)) {
		switch (SD(HEAD(slots))->alloc) {
		  case alloc_INSTANCE:
		    instance_length++;
		    break;
		  case alloc_SUBCLASS:
		    subclass_length++;
		    break;
		  case alloc_CLASS:
		  case alloc_CONSTANT:
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
	  case alloc_SUBCLASS:
	    SD(slot)->desired_offset = subclass_length++;
	    break;
	  case alloc_CLASS:
	  case alloc_CONSTANT:
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

    if (subclass_length > 0) {
	obj_t slots = make_vector(subclass_length, NULL);
	DC(class)->subclass_slots = slots;
	layout = make_vector(subclass_length, NULL);
	DC(class)->subclass_layout = layout;
	for (i = 0; i < subclass_length; i++) {
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
static obj_t displaced_subclass_slots;
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

	  case alloc_SUBCLASS:
	    offset = SD(new_slot)->desired_offset;
	    if (SOVEC(DC(class)->subclass_layout)->contents[offset]
		  != obj_False)
		displaced_subclass_slots
		    = pair(new_slot, displaced_subclass_slots);
	    else {
		SOVEC(DC(class)->subclass_layout)->contents[offset] = new_slot;
		initializers = pair(slot_initializer(new_slot), initializers);
	    }
	    break;

	  case alloc_CLASS:
	  case alloc_CONSTANT:
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

      case alloc_SUBCLASS:
	SOVEC(DC(class)->subclass_layout)->contents[offset] = slot;
	initializers = pair(slot_initializer(slot), initializers);
	SD(slot)->getter_method
	    = make_accessor_method(function_debug_name(SD(slot)->getter),
				   class, SD(slot)->type,
				   FALSE, make_fixnum(offset),
				   fast_subclass_getter);
	add_method(SD(slot)->getter, SD(slot)->getter_method);
	if (SD(slot)->setter != obj_False) {
	    SD(slot)->setter_method
		= make_accessor_method(function_debug_name(SD(slot)->setter),
				       class, SD(slot)->type, TRUE,
				       make_fixnum(offset),
				       fast_subclass_setter);
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

      case alloc_CONSTANT:
	SD(slot)->getter_method
	    = make_accessor_method(function_debug_name(SD(slot)->getter),
				   class, SD(slot)->type,
				   FALSE, SD(slot)->init_function_or_value,
				   constant_getter);
	add_method(SD(slot)->getter, SD(slot)->getter_method);
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

static void process_inherited(obj_t class, obj_t inherited)
{
    obj_t slots;

    for (slots = DC(class)->all_slots; slots != obj_Nil; slots = TAIL(slots)) {
	obj_t slot = HEAD(slots);
	obj_t inits;

	if (SD(slot)->name == INHD(inherited)->name) {
	    switch (SD(slot)->alloc) {
	      case alloc_INSTANCE:
		break;
	      case alloc_SUBCLASS:
	      case alloc_CLASS:
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
	      case alloc_CONSTANT:
		error("Can't change constant slot %=",
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
	    return;
	}
    }
    error("Slot %= not inherited from any superclass",
	  INHD(inherited)->name);
}


/* Initialize Defined Class */

void init_defined_class(obj_t class, obj_t slots,
			obj_t initargs, obj_t inheriteds)
{
    obj_t scan;

    DC(class)->new_slots = slots;
    DC(class)->all_slots = obj_Nil;
    DC(class)->new_initargs = initargs;
    DC(class)->all_initargs = initargs;
    DC(class)->inheriteds = inheriteds;

    compute_lengths(class);

    /* Process Slots */

    classes_processed = obj_Nil;
    displaced_instance_slots = obj_Nil;
    displaced_subclass_slots = obj_Nil;
    initializers = obj_Nil;

    for (scan = DC(class)->superclasses; scan != obj_Nil; scan = TAIL(scan))
	inherit_slots(class, HEAD(scan));

    DC(class)->instance_positions
	= compute_positions(displaced_instance_slots,
			    DC(class)->instance_layout);
    DC(class)->subclass_positions
	= compute_positions(displaced_subclass_slots,
			    DC(class)->subclass_layout);

    classes_processed = NULL;
    displaced_instance_slots = NULL;
    displaced_subclass_slots = NULL;

    for (scan = slots; scan != obj_Nil; scan = TAIL(scan))
	process_slot(class, HEAD(scan));

    /* Process Initargs */

    for (scan = TAIL(DC(class)->cpl); scan != obj_Nil; scan = TAIL(scan))
	inherit_initargs(class, HEAD(scan));

    /* Process Inheriteds */

    for (scan = inheriteds; scan != obj_Nil; scan = TAIL(scan))
        process_inherited(class, HEAD(scan));

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

static obj_t defaulted_initargs(obj_t class, obj_t keyword_arg_pairs)
{
    int i;
    int nkeys = SOVEC(keyword_arg_pairs)->length;
    obj_t supplied_initargs = obj_Nil;
    obj_t defaulted_initargs;
    obj_t supplieds;
    obj_t initargs;

    /* Get the supplied initialization arguments */

    for (i = 0; i < nkeys; i += 2) {
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

static obj_t dylan_make_instance(obj_t class, obj_t keyword_arg_pairs)
{
    obj_t res = alloc(class, sizeof(struct instance)
		               + DC(class)->instance_length * sizeof(obj_t));
    obj_t default_initargs;
    obj_t slots;
    obj_t initargs;
    obj_t inits;

    initializers = obj_Nil;

    default_initargs = defaulted_initargs(class, keyword_arg_pairs);

    for (slots = DC(class)->all_slots; slots != obj_Nil; slots = TAIL(slots)) {
	obj_t slot = HEAD(slots);
	boolean slot_initialized_p = FALSE;
	obj_t keyword = SD(slot)->init_keyword;

	/* Check for keyword init value */

	if (keyword != obj_False && !slot_initialized_p) {
	    obj_t initargs;
	    boolean suppliedp = FALSE;

	    for (initargs = default_initargs; initargs != obj_Nil;
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

	    for (inheriteds = DC(class)->inheriteds; inheriteds != obj_Nil;
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

    for (initargs = default_initargs; initargs != obj_Nil;
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
    do_initialization(res, default_initargs, inits);

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

    if (class != obj_DefinedClassClass)
	error("%= doens't access a slot in %=", getter, instance);

    for (scan = DC(class)->all_slots; scan != obj_Nil; scan = TAIL(scan)) {
	slot = HEAD(scan);
	if (SD(slot)->getter == getter) {
	    switch (SD(slot)->alloc) {
	      case alloc_INSTANCE:
		index = find_position(DC(class)->instance_positions, slot);
		value = INST(instance)->slots[index];
		break;
	      case alloc_SUBCLASS:
		index = find_position(DC(class)->subclass_positions, slot);
		value = INST(instance)->slots[index];
		break;
	      case alloc_CLASS:
		value = value_cell_ref(accessor_method_datum
				       (SD(slot)->getter_method));
		break;
	      case alloc_CONSTANT:
		value = accessor_method_datum(SD(slot)->getter_method);
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

    error("%= doens't access a slot in %=", getter, instance);    
    return NULL;
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
    scavenge(&class->instance_positions);
    scavenge(&class->instance_layout);
    scavenge(&class->subclass_positions);
    scavenge(&class->subclass_slots);
    scavenge(&class->subclass_layout);

    return sizeof(struct defined_class);
}

static obj_t trans_defined_class(obj_t class)
{
    return transport(class, sizeof(struct defined_class));
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
    return transport(slot, sizeof(struct slot_descr));
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
    return transport(initarg, sizeof(struct initarg_descr));
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
    return transport(inherited, sizeof(struct inherited_descr));
}

static int scav_postable(struct object *ptr)
{
    struct postable *p = (struct postable *)ptr;

    scavenge(&p->alist);

    return sizeof(struct postable);
}

static obj_t trans_postable(obj_t p)
{
    return transport(p, sizeof(struct postable));
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
    return transport(p, sizeof(struct initializer));
}

static int scav_instance(struct object *ptr)
{
    struct instance *instance = (struct instance *)ptr;
    int nslots = DC(ptr->class)->instance_length;
    int i;

    for (i = 0; i < nslots; i++)
	scavenge(instance->slots + i);

    return sizeof(struct instance) + nslots*sizeof(obj_t);
}

static obj_t trans_instance(obj_t instance)
{
    obj_t class = INST(instance)->class;
    int nslots = DC(class)->instance_length;

    return transport(instance, sizeof(struct instance) + nslots*sizeof(obj_t));
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
		    listn(5, obj_ObjectClass, obj_IntegerClass,
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
    define_generic_function("make", 1, FALSE, obj_Nil, TRUE,
			    obj_Nil, obj_ObjectClass);
    define_method("make", list1(obj_ClassClass), TRUE, obj_Nil, FALSE,
		  obj_ObjectClass, dylan_make);
    define_method("make", list1(obj_DefinedClassClass), TRUE, obj_Nil, FALSE,
		  obj_ObjectClass, dylan_make_instance);
    define_generic_function("initialize", 1, FALSE, obj_Nil, TRUE,
			    obj_Nil, obj_ObjectClass);
    define_method("initialize", list1(obj_ObjectClass), TRUE, obj_Nil, FALSE,
		  obj_ObjectClass, dylan_init);
    initialize_gf_variable =
      find_variable(module_BuiltinStuff, symbol("initialize"), FALSE, TRUE);
    define_method("slot-initialized?",
		  list2(obj_ObjectClass, obj_FunctionClass),
		  FALSE, obj_Nil, FALSE, obj_BooleanClass,
		  dylan_slot_initialized_p);
}
