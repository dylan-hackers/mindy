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
* $Header: /scm/cvs/src/mindy/interp/def.c,v 1.1 1998/05/03 19:55:12 andreas Exp $
*
* This file implements the stuff to install definitions.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "module.h"
#include "sym.h"
#include "thread.h"
#include "func.h"
#include "list.h"
#include "bool.h"
#include "obj.h"
#include "def.h"
#include "type.h"
#include "instance.h"
#include "error.h"
#include "class.h"

static void maybe_copy_methods(obj_t new_gf, obj_t old_gf)
{
    obj_t methods;

    if (old_gf == obj_Unbound)
	return;
    check_type(new_gf, obj_GFClass);
    check_type(old_gf, obj_GFClass);

    methods = generic_function_methods(old_gf);

    while (methods != obj_Nil) {
	add_method(new_gf, HEAD(methods));
	methods = TAIL(methods);
    }
}


/* Stuff to define builtin stuff. */

void define(char *name, obj_t value)
{
    obj_t namesym = symbol(name);
    struct variable *var;

    define_variable(module_BuiltinStuff, namesym, var_Variable);
    var = find_variable(module_BuiltinStuff, namesym, FALSE, TRUE);
    maybe_copy_methods(value, var->value);
    var->value = value;
    var->function = func_Maybe;
}

void define_constant(char *name, obj_t value)
{
    obj_t namesym = symbol(name);
    struct variable *var;

    define_variable(module_BuiltinStuff, namesym, var_Constant);
    var = find_variable(module_BuiltinStuff, namesym, FALSE, TRUE);
    maybe_copy_methods(value, var->value);
    var->value = value;
    var->function = func_Maybe;
}

void define_function(char *name, obj_t specializers, boolean restp,
		     obj_t keywords, boolean all_keys, obj_t result_type,
		     obj_t (*func)())
{
    define_constant(name,
		    make_builtin_method(name, specializers, restp, keywords,
					all_keys, result_type, func));
}

void define_generic_function(char *name, obj_t specializers, boolean restp,
			     obj_t keys, boolean all_keys, obj_t result_types,
			     obj_t more_results_type)
{
    obj_t namesym = symbol(name);
    struct variable *var;
    obj_t gf = make_generic_function(namesym, specializers, 
				     restp, keys, all_keys,
				     result_types, more_results_type);

    define_variable(module_BuiltinStuff, namesym, var_GenericFunction);
    var = find_variable(module_BuiltinStuff, namesym, FALSE, TRUE);
    maybe_copy_methods(gf, var->value);
    var->value = gf;
    var->function = func_Always;
}

void define_method(char *name, obj_t specializers, boolean restp,
		   obj_t keywords, boolean all_keys, obj_t result_type,
		   obj_t (*func)())
{
    obj_t namesym = symbol(name);
    obj_t method = make_builtin_method(name, specializers, restp,
				       keywords, all_keys, result_type, func);
    struct variable *var;
    obj_t gf;

    define_variable(module_BuiltinStuff, namesym, var_Method);
    var = find_variable(module_BuiltinStuff, namesym, FALSE, TRUE);
    gf = var->value;
    if (gf == obj_Unbound) {
	gf = make_default_generic_function(namesym, method);
	var->value = gf;
	var->function = func_Always;
    }
    else
	check_type(gf, obj_GFClass);
    add_method(gf, method);
}

void define_class(char *name, obj_t value)
{
    obj_t namesym = symbol(name);
    struct variable *var;

    define_variable(module_BuiltinStuff, namesym, var_Class);
    var = find_variable(module_BuiltinStuff, namesym, FALSE, TRUE);
    maybe_copy_methods(value, var->value);
    var->value = value;
    var->function = func_No;
}


/* Stuff to define/initialize defined stuff. */

static obj_t init_variable(obj_t var_obj, obj_t value, obj_t type)
{
    struct variable *var = obj_rawptr(var_obj);

    maybe_copy_methods(value, var->value);
    var->value = value;
    var->type = type;
    if (type != obj_False && subtypep(type, obj_FunctionClass))
	var->function = func_Always;
    else if (instancep(value, obj_FunctionClass))
	var->function = func_Yes;
    else
	var->function = func_No;

    return var->name;
}

static obj_t defmethod(obj_t var_obj, obj_t method)
{
    struct variable *var = obj_rawptr(var_obj);
    obj_t gf = var->value;
    obj_t old;

    if (gf == obj_Unbound) {
	gf = make_default_generic_function(var->name, method);
	var->value = gf;
	var->function = func_Always;
    }
    else
	check_type(gf, obj_GFClass);
    old = add_method(gf, method);

    if (old != obj_False)
	error("Definition of %= clashes with %=", method, old);

    return var->name;
}

static obj_t defgeneric(obj_t var_obj, obj_t signature, obj_t restp,
			obj_t keywords, obj_t all_keys, obj_t result_types,
			obj_t more_results_type)
{
    struct variable *var = obj_rawptr(var_obj);
    obj_t gf = var->value;

    if (more_results_type == obj_True)
	more_results_type = obj_ObjectClass;

    if (gf == obj_Unbound) {
	var->value = make_generic_function(var->name, signature,
					   restp != obj_False, keywords,
					   all_keys != obj_False, result_types,
					   more_results_type);
	var->function = func_Always;
    }
    else
	set_gf_signature(gf, signature, restp != obj_False, keywords,
			 all_keys != obj_False, result_types,
			 more_results_type);

    return var->name;
}

static obj_t defclass1(obj_t class, obj_t superclasses)
{
    setup_class_supers(class, superclasses);

    return class;
}

static obj_t defclass2(obj_t class, obj_t slots,
		       obj_t initargs, obj_t inheriteds, obj_t abstractp)
{
    init_defined_class(class, slots, initargs, inheriteds, abstractp);

    /* init_defined_class doesn't return */
    lose("init_defined_class actually returned?\n");
    return NULL;
}

static obj_t defslot(obj_t getter, obj_t setter)
{
    struct variable *var;

    if (setter != obj_False) {
	obj_t specializers = list2(obj_ObjectClass, obj_ObjectClass);
	var = obj_rawptr(setter);
	if (var->value == obj_Unbound)
	    var->value = make_generic_function(var->name, specializers,
					       FALSE, obj_False,
					       FALSE, obj_Nil,
					       obj_ObjectClass);
    }

    var = obj_rawptr(getter);
    if (var->value == obj_Unbound) {
	obj_t specializers = list1(obj_ObjectClass);
	var->value = make_generic_function(var->name, specializers,
					   FALSE, obj_False,
					   FALSE, obj_Nil, obj_ObjectClass);
    }

    return var->name;
}


/* Init stuff. */

void init_def_functions(void)
{
    define_function("init-variable",
		    list3(obj_ObjectClass, obj_ObjectClass, obj_ObjectClass),
		    FALSE, obj_False, FALSE, obj_ObjectClass, init_variable);
    define_function("%define-method", list2(obj_ObjectClass, obj_ObjectClass),
		    FALSE, obj_False, FALSE, obj_ObjectClass, defmethod);
    define_function("%define-generic",
		    listn(7, obj_ObjectClass, obj_ObjectClass, obj_ObjectClass,
			  obj_ObjectClass, obj_ObjectClass, obj_ObjectClass,
			  obj_ObjectClass),
		    FALSE, obj_Nil, FALSE, obj_ObjectClass, defgeneric);
    define_function("%define-class-1",
		    list2(obj_ObjectClass, obj_ObjectClass),
		    FALSE, obj_False, FALSE, obj_ObjectClass, defclass1);
    define_function("%define-class-2",
		    listn(5, obj_ObjectClass, obj_ObjectClass,
			  obj_ObjectClass, obj_ObjectClass, obj_ObjectClass),
		    FALSE, obj_False, FALSE, obj_ObjectClass, defclass2);
    define_function("%define-slot", list2(obj_ObjectClass, obj_ObjectClass),
		    FALSE, obj_False, FALSE, obj_ObjectClass, defslot);
}
