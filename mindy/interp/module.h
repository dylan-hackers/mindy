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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/module.h,v 1.1 1994/03/24 21:49:31 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t obj_Unbound;

struct use {
    /* The name of the thing being used. */
    obj_t name;

    /* Either #t or a list of the names to import.  Duplicates and names */
    /* listed in the rename list should be removed. */
    obj_t import;

    /* Either #f or a string to prepend each imported name with. */
    obj_t prefix;

    /* A list of names not to include when import is #t.  Duplicates */
    /* should be removed.  If import is not #t, then this should be empty. */
    obj_t exclude;

    /* A list of pair(orig_name, local_name) for renamings that override */
    /* prefix.  These are taken in addition to import, and should not */
    /* duplicate names there. */
    obj_t rename;

    /* A list of local names to re-export, or #t for all imported names. */
    obj_t export;

    /* The next use in this defn. */
    struct use *next;
};

struct defn {
    /* Name of the thing this is the defn for. */
    obj_t name;

    /* Chain of use structures. */
    struct use *use;

    /* List of names in the export and create options. */
    obj_t exports;
    obj_t creates; /* Not used in libraries. */
};

extern void define_library(struct defn *defn);
extern struct library *find_library(obj_t name, boolean createp);

extern void define_module(struct library *library, struct defn *defn);
extern struct module *find_module(struct library *library, obj_t name,
				  boolean lose_if_not_there,
				  boolean lose_if_imported);

enum var_kind {
    var_Assumed, var_AssumedWriteable,
    var_Constant, var_Variable, var_Class,
    var_GenericFunction, var_Method
};

extern void define_variable(struct module *module, obj_t name,
			    enum var_kind kind);

struct variable {
    obj_t name;
    struct module *home;
    enum var_kind kind;
    obj_t value;
    obj_t type;
    enum { func_Yes, func_No, func_Maybe, func_Always } function;
};

extern struct variable *find_variable(struct module *module, obj_t name,
				      boolean writeable, boolean createp);

extern struct module *module_BuiltinStuff;

extern void list_libraries(void);
extern obj_t library_name(struct library *library);
extern void list_modules(struct library *library);
extern obj_t module_name(struct module *module);
