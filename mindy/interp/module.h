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
* $Header: /scm/cvs/src/mindy/interp/module.h,v 1.1 1998/05/03 19:55:16 andreas Exp $
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
    boolean defined;
    enum var_kind kind;
    obj_t binding;
    obj_t value;
    obj_t type;
    enum { func_Yes, func_No, func_Maybe, func_Always } function;
    obj_t ref_file;
    int ref_line;
};

extern struct variable *find_variable(struct module *module, obj_t name,
				      boolean writeable, boolean createp);

extern struct module *module_BuiltinStuff;

extern void list_libraries(void);
extern obj_t library_name(struct library *library);
extern void list_modules(struct library *library);
extern obj_t module_name(struct module *module);

extern void finalize_modules(void);
