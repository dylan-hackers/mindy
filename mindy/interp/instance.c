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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/instance.c,v 1.11 1994/05/31 18:09:26 nkramer Exp $
*
* This file does whatever.
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
    obj_t debug_name;
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

struct postable {
    obj_t class;
    obj_t alist;
};

#define PT(o) obj_ptr(struct postable *, o)

struct instance {
    obj_t class;
    obj_t slots[0];
};

#define INST(o) obj_ptr(struct instance *, o)

static obj_t obj_DefinedClassClass = NULL;
static obj_t obj_SlotDescrClass = NULL;
static obj_t obj_PosTableClass = NULL;


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
		set_method_iep(SD(slot)->getter_method, slow_instance_setter);
		set_accessor_method_datum(SD(slot)->setter_method, slot);
	    }
	    break;

	  case alloc_SUBCLASS:
	    set_method_iep(SD(slot)->getter_method, slow_subclass_getter);
	    set_accessor_method_datum(SD(slot)->getter_method, slot);
	    if (SD(slot)->setter_method != obj_False) {
		set_method_iep(SD(slot)->getter_method, slow_subclass_setter);
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

static obj_t make_slot_descriptor(obj_t debug_name, obj_t allocation,
				  obj_t getter, obj_t setter, obj_t type,
				  obj_t init_keyword, obj_t req_init_keyword,
				  obj_t init_function, obj_t init_value)
{
    obj_t res = alloc(obj_SlotDescrClass, sizeof(struct slot_descr));

    SD(res)->debug_name = debug_name;
    SD(res)->alloc = (enum slot_allocation)fixnum_value(allocation);
    SD(res)->creator = obj_False;
    if (init_function != obj_Unbound) {
	if (init_value != obj_Unbound)
	    error("Can't specify both an init-function: and an init-value:");
	SD(res)->init_function_or_value = init_function;
	SD(res)->init_function_p = TRUE;
    }
    else {
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


/* Stuff to run init functions. */

static struct variable *initialize_var = NULL;

static void do_finish_inits(struct thread *thread, obj_t *vals)
{
    obj_t inst_or_class = vals[-3];
    obj_t *old_sp = pop_linkage(thread);

    *old_sp = inst_or_class;
    thread->sp = old_sp + 1;

    do_return(thread, old_sp, old_sp);
}

static void do_next_init(struct thread *thread, obj_t *vals);

static void do_init_functions(struct thread *thread, obj_t init_functions)
{
    obj_t *sp = thread->sp;
    obj_t slot;

    if (init_functions == obj_Nil) {
	obj_t inst_or_class = sp[-3];
	obj_t key_val_pairs = sp[-2];
	int len = key_val_pairs==obj_False ? 0 : SOVEC(key_val_pairs)->length;
	int i;

	*sp++ = initialize_var->value;
	*sp++ = inst_or_class;
	for (i = 0; i < len; i++)
	    *sp++ = SOVEC(key_val_pairs)->contents[i];
	thread->sp = sp;

	set_c_continuation(thread, do_finish_inits);
	invoke(thread, len+1);
    }
    else {
	slot = HEAD(init_functions);
	sp[-1] = init_functions;
	thread->sp = sp+1;
	sp[0] = SD(slot)->init_function_or_value;
	set_c_continuation(thread, do_next_init);
	invoke(thread, 0);
    }
}

static void do_next_init(struct thread *thread, obj_t *vals)
{
    obj_t inst_or_class = vals[-3];
    obj_t init_functions = vals[-1];
    obj_t slot = HEAD(init_functions);
    obj_t value, class;
    int index;

    if (thread->sp == vals)
	value = obj_False;
    else {
	value = vals[0];
	thread->sp = vals;
    }

    switch (SD(slot)->alloc) {
      case alloc_INSTANCE:
	class = INST(inst_or_class)->class;
	index = find_position(DC(class)->instance_positions, slot);
	INST(inst_or_class)->slots[index] = value;
	break;
      case alloc_SUBCLASS:
	index = find_position(DC(inst_or_class)->subclass_positions, slot);
	SOVEC(DC(inst_or_class)->subclass_slots)->contents[index] = value;
	break;
      case alloc_CLASS:
	value_cell_set(accessor_method_datum(SD(slot)->getter_method), value);
	break;
      default:
	lose("Tried to use an init-function: with a strange kind of slot.");
    }

    do_init_functions(thread, TAIL(init_functions));
}

static void do_inits(obj_t inst_or_class, obj_t key_and_value_pairs,
		     obj_t init_functions)
{
    struct thread *thread = thread_current();
    obj_t *sp = thread->sp += 3;

    sp[-3] = inst_or_class;
    sp[-2] = key_and_value_pairs;
    do_init_functions(thread, init_functions);
    go_on();
}


/* Defined classes. */

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
static obj_t init_functions;

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
		if (SD(new_slot)->init_function_p)
		    init_functions = pair(new_slot, init_functions);
		SOVEC(DC(class)->subclass_slots)->contents[offset]
		    = SD(new_slot)->init_function_or_value;
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
	if (SD(slot)->init_function_p)
	    init_functions = pair(slot, init_functions);
	SOVEC(DC(class)->subclass_slots)->contents[offset]
	    = SD(slot)->init_function_or_value;
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
	if (SD(slot)->init_function_p)
	    init_functions = pair(slot, init_functions);
	value_cell = make_value_cell(SD(slot)->init_function_or_value);
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

void init_defined_class(obj_t class, obj_t slots)
{
    obj_t scan;

    DC(class)->new_slots = slots;
    DC(class)->all_slots = obj_Nil;

    compute_lengths(class);

    classes_processed = obj_Nil;
    displaced_instance_slots = obj_Nil;
    displaced_subclass_slots = obj_Nil;
    init_functions = obj_Nil;

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

    scan = init_functions;
    init_functions = NULL;
    do_inits(class, obj_False, scan);
}
    

/* Make and initialize. */

static obj_t dylan_make(obj_t class, obj_t key_and_value_pairs)
{
    error("Can't make instances of %= with the default make method.",
	  class);
    return NULL;
}

static obj_t dylan_make_instance(obj_t class, obj_t key_and_value_pairs)
{
    int nslots = DC(class)->instance_length;
    obj_t res = alloc(class, sizeof(struct instance) + nslots*sizeof(obj_t));
    int nkeys = SOVEC(key_and_value_pairs)->length;
    int index;
    obj_t init_functions = obj_Nil;
    obj_t slots;

    /* ### Need to compute the defaulted set of key_and_value_pairs. */

    for (slots = DC(class)->all_slots; slots != obj_Nil; slots = TAIL(slots)) {
	obj_t slot = HEAD(slots);
	obj_t keyword = SD(slot)->init_keyword;
	obj_t value = obj_Unbound;

	if (keyword != obj_False) {
	    for (index = 0; index < nkeys; index += 2) {
		if (SOVEC(key_and_value_pairs)->contents[index] == keyword) {
		    value = SOVEC(key_and_value_pairs)->contents[index+1];
		    break;
		}
	    }
	    if (SD(slot)->keyword_required && value == obj_Unbound)
		error("Missing required init-keyword %=", keyword);
	}
	switch (SD(slot)->alloc) {
	  case alloc_INSTANCE:
	    if (value == obj_Unbound)
		if (SD(slot)->init_function_p)
		    init_functions = pair(slot, init_functions);
		else
		    value = SD(slot)->init_function_or_value;
	    index = find_position(DC(class)->instance_positions, slot);
	    INST(res)->slots[index] = value;
	    break;
	  case alloc_SUBCLASS:
	    if (value != obj_Unbound) {
		index = find_position(DC(class)->subclass_positions, slot);
		SOVEC(DC(class)->subclass_slots)->contents[index] = value;
	    }
	    break;
	  case alloc_CLASS:
	    if (value != obj_Unbound) {
		obj_t cell = accessor_method_datum(SD(slot)->getter_method);
		value_cell_set(cell, value);
	    }
	    break;
	  default:
	    if (value != obj_Unbound)
		lose("Tried to use an init-keyword: with a strange kind "
		     "of slot.");
	    break;
	}
    }

    do_inits(res, key_and_value_pairs, init_functions);
    /* do_inits never returns. */
    lose("do_inits actually returned?");
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

    scavenge(&slot->debug_name);
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
    obj_PosTableClass = make_builtin_class(scav_postable, trans_postable);
}

void init_instance_classes(void)
{
    init_builtin_class(obj_DefinedClassClass, "<defined-class>",
		       obj_ClassClass, NULL);
    init_builtin_class(obj_SlotDescrClass, "<slot-descriptor>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_PosTableClass, "<position-table>",
		       obj_ObjectClass, NULL);
}

void init_instance_functions(void)
{
    define_function("make-slot",
		    listn(5, obj_ObjectClass, obj_IntegerClass,
			  obj_FunctionClass,
			  type_union(obj_FunctionClass,
				     object_class(obj_False)),
			  obj_ObjectClass),
		    FALSE,
		    listn(4, pair(symbol("init-keyword"), obj_False),
			  pair(symbol("required-init-keyword"), obj_False),
			  pair(symbol("init-function"), obj_Unbound),
			  pair(symbol("init-value"), obj_Unbound)),
		    obj_SlotDescrClass, make_slot_descriptor);
    define_method("make", list1(obj_ClassClass), TRUE, obj_Nil,
		  obj_ObjectClass, dylan_make);
    define_method("make", list1(obj_DefinedClassClass), TRUE, obj_Nil,
		  obj_ObjectClass, dylan_make_instance);
    define_method("initialize", list1(obj_ObjectClass), TRUE, obj_Nil,
		  obj_ObjectClass, dylan_init);
    initialize_var = find_variable(module_BuiltinStuff, symbol("initialize"),
				   FALSE, TRUE);
    define_function("slot-initialized?",
		    list2(obj_ObjectClass, obj_FunctionClass),
		    FALSE, obj_Nil, obj_BooleanClass,
		    dylan_slot_initialized_p);
}
