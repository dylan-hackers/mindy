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
* $Header: /scm/cvs/src/mindy/interp/module.c,v 1.1 1998/05/03 19:55:16 andreas Exp $
*
* This file implements the module system.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "sym.h"
#include "list.h"
#include "bool.h"
#include "str.h"
#include "obj.h"
#include "module.h"
#include "class.h"
#include "type.h"
#include "thread.h"
#include "func.h"
#include "def.h"
#include "load.h"
#include "error.h"
#include "print.h"

obj_t obj_Unbound = NULL;
static obj_t obj_UnboundClass = NULL;

obj_t obj_NameClass = NULL;
obj_t obj_NamespaceClass = NULL;
obj_t obj_LibraryClass = NULL;
obj_t obj_ModuleClass = NULL;
obj_t obj_BindingClass = NULL;

#define BINDING(o) obj_ptr(struct binding *, o)
#define MODULE(o) obj_ptr(struct dylan_module *, o)
#define LIBRARY(o) obj_ptr(struct dylan_library *, o)

static boolean InitializingVars = TRUE;

struct bucket {
    obj_t symbol;
    void *datum;
    struct bucket *next;
};

struct table {
    char *owner;  /* Which namespace owns this table, so we can print
		     better error messages about name clashes */
    int entries;
    int threshold;
    int length;
    struct bucket **table;
};

struct library {
    obj_t name;
    obj_t dylan_library;
    struct defn *defn;
    struct table *modules;
    boolean loading;
    boolean busy;
    boolean completed;
    struct library *next;
};

struct entry {
    obj_t name;
    void *datum;
    boolean exported;
    char *template;
    obj_t p1;
    obj_t p2;
    obj_t p3;
};

struct module {
    obj_t name;
    obj_t dylan_module;
    struct library *home;
    struct defn *defn;
    struct table *variables;
    boolean busy;
    boolean completed;
    struct module *next;
};

struct var {
    boolean created;
    struct var *next;
    struct variable variable;
};

struct dylan_library {
    obj_t class;
    struct library *library;
};

struct dylan_module {
    obj_t class;
    struct module *module;
};

struct binding {
    obj_t class;
    struct variable *variable;
};

static struct table *LibraryTable = NULL;
static struct library *Libraries = NULL;
static struct module *Modules = NULL;
static struct var *Vars = NULL;

static struct library *library_Dylan = NULL;
struct module *module_BuiltinStuff = NULL;

static struct module *make_module(obj_t name, struct library *home);
static struct var *make_var(obj_t name, struct module *home, enum var_kind k);


/* Table manipulation stuff. */

static struct table *make_table(char *owner_of_table)
{
    struct table *table = (struct table *)malloc(sizeof(struct table));

    table->owner = owner_of_table;
    table->entries = 0;
    table->threshold = 96;
    table->length = 64;
    table->table = (struct bucket **)malloc(sizeof(struct bucket *)*64);
    memset(table->table, 0, sizeof(struct bucket *)*64);

    return table;
}

static void *table_lookup(struct table *table, obj_t symbol)
{
    unsigned hash = sym_hash(symbol);
    int index = hash % table->length;
    struct bucket *bucket;

    for (bucket = table->table[index]; bucket != NULL; bucket = bucket->next)
	if (bucket->symbol == symbol)
	    return bucket->datum;
    return NULL;
}

/* Returns a list of all the symbols used as keys in the table
 */
static obj_t get_all_keys(struct table *table)
{
    int length = table->length;
    int i;
    obj_t res = obj_Nil;

    for (i = 0; i < length; i++) {
	struct bucket *bucket;

	for (bucket = table->table[i]; bucket != NULL; bucket = bucket->next) {
	    res = pair(bucket->symbol, res);
	}
    }
    return res;
}    

static void rehash_table(struct table *table)
{
    int length = table->length;
    int new_length = (length < 1024) ? (length << 1) : (length + 1024);
    struct bucket **new_table
	= (struct bucket **)malloc(sizeof(struct bucket *) * new_length);
    int i;

    memset(new_table, 0, sizeof(struct bucket *) * new_length);

    for (i = 0; i < length; i++) {
	struct bucket *bucket, *next;

	for (bucket = table->table[i]; bucket != NULL; bucket = next) {
	    int index = sym_hash(bucket->symbol) % new_length;

	    next = bucket->next;
	    bucket->next = new_table[index];
	    new_table[index] = bucket;
	}
    }

    free(table->table);

    table->threshold = (new_length * 3) >> 1;
    table->length = new_length;
    table->table = new_table;
}

static void table_add(struct table *table, obj_t symbol, void *datum)
{
    unsigned hash = sym_hash(symbol);
    int index = hash % table->length;
    struct bucket *bucket;

    for (bucket = table->table[index]; bucket != NULL; bucket = bucket->next)
	if (bucket->symbol == symbol) {
	    bucket->datum = datum;
	    return;
	}

    bucket = (struct bucket *)malloc(sizeof(struct bucket));
    bucket->symbol = symbol;
    bucket->datum = datum;
    bucket->next = table->table[index];
    table->table[index] = bucket;

    table->entries++;
    if (table->entries >= table->threshold)
	rehash_table(table);
}

static void *table_remove(struct table *table, obj_t symbol)
{
    unsigned hash = sym_hash(symbol);
    int index = hash % table->length;
    struct bucket *bucket, **prev;

    for (prev = &table->table[index];
	 (bucket = *prev) != NULL;
	 prev = &bucket->next)
	if (bucket->symbol == symbol) {
	    void *res = bucket->datum;
	    *prev = bucket->next;
	    free(bucket);
	    table->entries--;
	    return res;
	}

    return NULL;
}


/* Utilities */
static char *safe_sym_name( obj_t t)
{
    return object_class(t)==obj_SymbolClass ? sym_name(t) : "??";
}

static obj_t format_entry_origin( struct entry *entry)
{
    int len;
    obj_t origin;

    len = strlen( entry->template) + 1;
    len += strlen( safe_sym_name( entry->p1));
    len += strlen( safe_sym_name( entry->p2));
    len += strlen( safe_sym_name( entry->p3));
    origin = alloc_byte_string(len);
    sprintf( string_chars(origin), 
	    entry->template, safe_sym_name(entry->p1), 
	    safe_sym_name(entry->p2), safe_sym_name(entry->p3));
    return origin;
}

static void make_entry(struct table *table, obj_t name, void *datum,
		       boolean exported, char *template, obj_t p1, obj_t p2,
			obj_t p3)
{
    struct entry *old_entry = table_lookup(table, name);
    struct entry *entry;

    if (old_entry && old_entry->datum == datum)
	return;

    entry = (struct entry *)malloc(sizeof(struct entry));

    entry->name = name;
    entry->datum = datum;
    entry->exported = exported;
    entry->template = template;
    entry->p1 = p1;
    entry->p2 = p2;
    entry->p3 = p3;

    if (old_entry)
        error("In %s, %s clashes with %s", make_byte_string(table->owner),
              format_entry_origin(entry), format_entry_origin(old_entry));

    table_add(table, name, entry);
}

static obj_t prepend_prefix(obj_t name, struct use *use)
{
    char *prefix;
    char *local_name;
    obj_t res;

    if (use->prefix == obj_False)
	return name;

    prefix = (char *)obj_ptr(struct string *, use->prefix)->chars;
    local_name = (char *)malloc(strlen(prefix) + strlen(sym_name(name)) + 1);
    strcpy(local_name, prefix);
    strcat(local_name, sym_name(name));
    res = symbol(local_name);
    free(local_name);

    return res;
}

static obj_t prefix_or_rename(obj_t name, struct use *use)
{
    obj_t ptr;

    for (ptr = use->rename; ptr != obj_Nil; ptr = TAIL(ptr))
	if (name == HEAD(HEAD(ptr)))
	    return TAIL(HEAD(ptr));
    return prepend_prefix(name, use);
}

static boolean exported(obj_t name, struct use *use)
{
    return use->export == obj_True || memq(name, use->export);
}

static void add_use(struct defn *defn, obj_t name)
{
    struct use *use = malloc(sizeof(struct use));    

    use->name = name;
    use->import = obj_True;
    use->prefix = obj_False;
    use->exclude = obj_Nil;
    use->rename = obj_Nil;
    use->export = obj_Nil;
    use->next = defn->use;

    defn->use = use;
}


/* Libraries. */

static struct library *make_library(obj_t name)
{
    struct library *library = malloc(sizeof(struct library));
    char *owner = (char *) malloc(strlen("library ") 
				  + strlen(sym_name(name)) + 1);
    strcpy(owner, "library ");
    strcat(owner, sym_name(name));

    library->name = name;
    library->dylan_library = NULL;
    library->defn = NULL;
    library->modules = make_table(owner);
    library->completed = FALSE;
    library->loading = FALSE;
    library->busy = FALSE;
    library->next = Libraries;
    Libraries = library;

    table_add(LibraryTable, name, library);

    return library;
}

struct library *find_library(obj_t name, boolean createp)
{
    struct library *library = table_lookup(LibraryTable, name);

    if (library == NULL && createp) {
	struct defn *defn = malloc(sizeof(struct defn));
	obj_t dylan_user = symbol("Dylan-User");
	struct module *module;

	library = make_library(name);
	module = make_module(dylan_user, library);

	module->defn = defn;

	defn->name = dylan_user;
	defn->use = NULL;
	defn->exports = obj_Nil;
	defn->creates = obj_Nil;

	add_use(defn, symbol("Introspection"));
	add_use(defn, symbol("Threads"));
	add_use(defn, symbol("File-Descriptors"));
	add_use(defn, symbol("Cheap-IO"));
	add_use(defn, symbol("System"));
	add_use(defn, symbol("Extensions"));
	add_use(defn, symbol("Dylan"));

	make_entry(library->modules, module->name, module, FALSE,
		   "module %s implicitly defined in library %s",
		   module->name, library->name, obj_False);
    }

    return library;
}

void define_library(struct defn *defn)
{
    struct library *library = find_library(defn->name, TRUE);

    if (library->defn)
	error("Library %s multiply defined.\n", defn->name);

    library->defn = defn;
}

static void complete_library(struct library *library)
{
    obj_t ptr;
    struct defn *defn;
    struct use *use;

    defn = library->defn;
    if (defn == NULL) {
	if (library->loading)
	    error("needed the definition of library %s before the "
		  "define library was found",
		  library->name);
	library->loading = TRUE;
	load_library(library->name);
	if (library->defn == NULL)
	    error("loaded library %s, but never found the define library.\n",
		  library->name);
	library->loading = FALSE;
	return;
    }

    if (library->busy)
	error("Library %s circularly defined.\n", library->name);
    library->busy = TRUE;

    for (ptr = defn->exports; ptr != obj_Nil; ptr = TAIL(ptr)) {
	obj_t name = HEAD(ptr);
	struct module *module = make_module(name, library);

	make_entry(library->modules, name, module, TRUE,
		   "module %s defined in library %s",
		   name, library->name, obj_False);
    }

    for (use = library->defn->use; use != NULL; use = use->next) {
	obj_t use_name = use->name;
	struct library *used_library = find_library(use_name, TRUE);
	obj_t imports = obj_Nil;

	if (!used_library->completed)
	    complete_library(used_library);

	if (use->import == obj_True) {
	    struct table *modules = used_library->modules;
	    int i;
	    for (i = 0; i < modules->length; i++) {
		struct bucket *bucket;
		for (bucket = modules->table[i];
		     bucket != NULL;
		     bucket = bucket->next) {
		    struct entry *entry = (struct entry *)bucket->datum;
		    obj_t name = entry->name;
		    if (entry->exported && !memq(name, use->exclude)) {
			obj_t local_name = prefix_or_rename(name, use);
			make_entry(library->modules, local_name, entry->datum,
				   exported(local_name, use),
				   name == local_name
				   ? "module %s imported from library %s"
				   :"module %s imported from library %s as %s",
				   name, use_name, local_name);
			imports = pair(local_name, imports);
		    }
		}
	    }
	}
	else {
	    for (ptr = use->import; ptr != obj_Nil; ptr = TAIL(ptr)) {
		obj_t name = HEAD(ptr);
		obj_t local_name = prepend_prefix(name, use);
		struct entry *entry
		    = table_lookup(used_library->modules, name);
		if (!entry || !entry->exported)
		    error("library %s can't import module %s from library %s "
			  "because it isn't exported.\n",
			  library->name, name, use_name);
		make_entry(library->modules, local_name, entry->datum,
			   exported(local_name, use),
			   name == local_name
			    ? "module %s imported from library %s"
			    : "module %s imported from library %s as %s",
			   name, use_name, local_name);
		imports = pair(local_name, imports);
	    }
	    for (ptr = use->rename; ptr != obj_Nil; ptr = TAIL(ptr)) {
		obj_t rename = HEAD(ptr);
		obj_t name = HEAD(rename);
		obj_t local_name = TAIL(rename);
		struct entry *entry
		    = table_lookup(used_library->modules, name);
		if (!entry || !entry->exported)
		    error("library %s can't import module %s from library %s"
			  "because it isn't exported.\n",
			  library->name, name, use_name);
		make_entry(library->modules, local_name, entry->datum,
			   exported(local_name, use),
			   name == local_name
			    ? "module %s imported from library %s"
			    : "module %s imported from library %s as %s",
			   name, use_name, local_name);
		imports = pair(local_name, imports);
	    }
	}
	if (use->export != obj_True)
	    for (ptr = use->export; ptr != obj_Nil; ptr = TAIL(ptr))
		if (!memq(HEAD(ptr), imports))
		    error("library %s can't re-export module %s because it "
			  "doesn't import it in the first place",
			  library->name, HEAD(ptr));
    }

    library->busy = FALSE;
    library->completed = TRUE;
}


/* Modules */

static struct module *make_module(obj_t name, struct library *home)
{
    struct module *module = malloc(sizeof(struct module));
    char *owner = (char *) malloc(strlen("module ") 
				  + strlen(sym_name(name)) + 1);
    strcpy(owner, "module ");
    strcat(owner, sym_name(name));

    module->name = name;
    module->dylan_module = NULL;
    module->home = home;
    module->defn = NULL;
    module->variables = make_table(owner);
    module->busy = FALSE;
    module->completed = FALSE;
    module->next = Modules;
    Modules = module;

    return module;
}

struct module *find_module(struct library *library, obj_t name,
			   boolean lose_if_not_there, boolean lose_if_imported)
{
    struct entry *entry;
    struct module *module;

    if ((entry = table_lookup(library->modules, name)) == NULL) {
	if (!lose_if_not_there)
	    return NULL;

	if (!library->completed) {
	    complete_library(library);
	    if ((entry = table_lookup(library->modules, name)) == NULL)
		error("Unknown module %s in library %s",
		      name, library->name);
	}
	else
	    error("Unknown module %s in library %s", name, library->name);
    }

    module = entry->datum;

    if (lose_if_imported && module->home != library)
	error("Can't add code to module %s in library %s because it "
	      "is imported from library %s",
	      module->name,
	      library->name,
	      module->home->name);

    return module;
}

void define_module(struct library *library, struct defn *defn)
{
    struct entry *entry;
    struct module *module;

    entry = table_lookup(library->modules, defn->name);
    if (entry == NULL && !library->completed) {
	complete_library(library);
	entry = table_lookup(library->modules, defn->name);
    }
    if (entry == NULL) {
	module = make_module(defn->name, library);
	make_entry(library->modules, defn->name, module, FALSE,
		   "module %s internal to library %s",
		   defn->name, library->name, obj_False);
    }
    else {
	module = entry->datum;

	if (module->home != library)
	    error("Can't define %s in library %s because its home "
		    "is library %s.\n",
		  format_entry_origin(entry), library->name, module->home->name);

	if (module->defn)
	    error("Module %s multiply defined.\n", defn->name);
    }

    module->defn = defn;
}

static void
    make_and_export_var(struct module *module, obj_t name, boolean created)
{
    struct entry *entry;
    struct var *var;

    entry = table_lookup(module->variables, name);
    if (entry) {
	var = entry->datum;
	if (var->variable.home == module) {
	    table_remove(module->variables, name);
	    free(entry);
	}
	else
	    var = make_var(name, module, var_Assumed);
    }
    else
	var = make_var(name, module, var_Assumed);

    make_entry(module->variables, name, var, TRUE,
	       created
	       ? "variable %s created in module %s"
	       : "variable %s defined in module %s",
	       name, module->name, obj_False);

    var->created = created;
}

static void complete_module(struct module *module)
{
    obj_t ptr;
    struct defn *defn;
    struct use *use;

    if (!module->home->completed)
	complete_library(module->home);

    if (module->busy)
	error("Module %s circularly defined.", module->name);
    module->busy = TRUE;

    defn = module->defn;
    if (defn == NULL)
	error("Attempt to use module %s before it is defined.", module->name);

    for (ptr = defn->exports; ptr != obj_Nil; ptr = TAIL(ptr))
	make_and_export_var(module, HEAD(ptr), FALSE);
    for (ptr = defn->creates; ptr != obj_Nil; ptr = TAIL(ptr))
	make_and_export_var(module, HEAD(ptr), TRUE);

    for (use = module->defn->use; use != NULL; use = use->next) {
	obj_t use_name = use->name;
	struct module *used_module;
	obj_t imports = obj_Nil;

	if (module->name == symbol("Dylan-User"))
	    used_module = find_module(library_Dylan, use_name, TRUE, FALSE);
	else
	    used_module = find_module(module->home, use_name, TRUE, FALSE);

	if (!used_module->completed)
	    complete_module(used_module);

	if (use->import == obj_True) {
	    struct table *modules = used_module->variables;
	    int i;
	    for (i = 0; i < modules->length; i++) {
		struct bucket *bucket;
		for (bucket = modules->table[i];
		     bucket != NULL;
		     bucket = bucket->next) {
		    struct entry *entry = (struct entry *)bucket->datum;
		    obj_t name = entry->name;
		    if (entry->exported && !memq(name, use->exclude)) {
			obj_t local_name = prefix_or_rename(name, use);
			make_entry(module->variables, local_name, entry->datum,
				   exported(local_name, use),
				   name == local_name
				   ? "variable %s imported from module %s"
				   : "variable %s imported from "
				     "module %s as %s",
				   name, use_name, local_name);
			imports = pair(local_name, imports);
		    }
		}
	    }
	}
	else {
	    for (ptr = use->import; ptr != obj_Nil; ptr = TAIL(ptr)) {
		obj_t name = HEAD(ptr);
		obj_t local_name = prepend_prefix(name, use);
		struct entry *entry
		    = table_lookup(used_module->variables, name);
		if (!entry || !entry->exported)
		    error("module %s can't import variable %s from module %s "
			  "because it isn't exported.",
			  module->name, name, use_name);
		make_entry(module->variables, local_name, entry->datum,
			   exported(local_name, use),
			   name == local_name
			    ? "variable %s imported from module %s"
			    : "variable %s imported from module %s as %s",
			   name, use_name, local_name);
		imports = pair(local_name, imports);
	    }
	    for (ptr = use->rename; ptr != obj_Nil; ptr = TAIL(ptr)) {
		obj_t rename = HEAD(ptr);
		obj_t name = HEAD(rename);
		obj_t local_name = TAIL(rename);
		struct entry *entry
		    = table_lookup(used_module->variables, name);
		if (!entry || !entry->exported)
		    error("module %s can't import variable %s from module %s"
			  "because it isn't exported.",
			  module->name, name, use_name);
		make_entry(module->variables, local_name, entry->datum,
			   exported(local_name, use),
			   name == local_name
			    ? "variable %s imported from module %s"
			    : "variable %s imported from module %s as %s",
			   name, use_name, local_name);
		imports = pair(local_name, imports);
	    }
	}
	if (use->export != obj_True)
	    for (ptr = use->export; ptr != obj_Nil; ptr = TAIL(ptr))
		if (!memq(HEAD(ptr), imports))
		    error("module %s can't re-export variable %s because it "
			  "doesn't import it in the first place",
			  module->name, HEAD(ptr));
    }

    module->busy = FALSE;
    module->completed = TRUE;
}


/* Variables. */

static struct var *make_var(obj_t name, struct module *home,enum var_kind kind)
{
    struct var *var = malloc(sizeof(struct var));

    var->created = FALSE;
    var->next = Vars;
    Vars = var;

    var->variable.name = name;
    var->variable.home = home;
    var->variable.defined = FALSE;
    var->variable.kind = kind;
    var->variable.binding = NULL;
    var->variable.value = obj_Unbound;
    var->variable.type = obj_False;
    var->variable.function = func_Maybe;
    var->variable.ref_file = obj_False;
    var->variable.ref_line = 0;

    return var;
}

struct var *find_var(struct module *module, obj_t name, boolean writeable,
		     boolean createp)
{
    struct entry *entry = table_lookup(module->variables, name);
    struct var *var;

    if (entry == NULL) {
	if (createp) {
	    if (!module->completed && !InitializingVars) {
		complete_module(module);
		entry = table_lookup(module->variables, name);
	    }
	}
	else
	    return NULL;
    }
    if (entry == NULL) {
	var = make_var(name, module,
		       writeable ? var_AssumedWriteable : var_Assumed);
	make_entry(module->variables, name, var, FALSE,
		   "variable %s internal to module %s",
		   name, module->name, obj_False);
    }
    else {
	var = entry->datum;
	if (writeable) {
	    switch (var->variable.kind) {
	      case var_Assumed:
		var->variable.kind = var_AssumedWriteable;
		break;
	      case var_AssumedWriteable:
	      case var_Variable:
		break;
	      default:
		error("Constant %s in module %s library %s is not changeable.",
		      name, module->name, module->home->name);
	    }
	}
    }
    return var;
}

struct variable
    *find_variable(struct module *module, obj_t name, boolean writeable,
		   boolean createp)
{
    struct var *var = find_var(module, name, writeable, createp);

    if (var)
	return &var->variable;
    else
	return NULL;
}

void define_variable(struct module *module, obj_t name, enum var_kind kind)
{
    struct var *var = find_var(module, name, kind == var_Variable, TRUE);

    switch (var->variable.kind) {
      case var_Assumed:
	if (kind != var_Method)
	    var->variable.kind = kind;
	break;
      case var_AssumedWriteable:
	if (kind == var_Variable)
	    var->variable.kind = var_Variable;
	else if (kind != var_Method)
	    error("Constant %s in module %s library %s is not changeable.",
		  name, module->name, module->home->name);
	break;
      default:
	if (kind != var_Method)
	    error("variable %s in module %s multiply defined.",
		  name, module->name);
	break;
    }

    if (var->created) {
	if (var->variable.home == module)
	    error("variable %s must be defined by a user of module %s\n",
		  name, module->name);
	else if (kind!=var_Method && var->variable.home->home!=module->home)
	    error("variable %s must be defined in library %s, not %s\n",
		  name, var->variable.home->home->name,
		  module->home->name);
    }
    else {
	if (kind != var_Method && var->variable.home != module)
	    error("variable %s must be defined in module %s, not %s\n",
		  name, var->variable.home->name, module->name);
    }

    var->variable.defined = TRUE;
}


/* Debugger support. */

void list_libraries(void)
{
    struct library *library;

    for (library = Libraries; library != NULL; library = library->next) {
	fputs(sym_name(library->name), stdout);
	if (library->completed)
	    printf("\n");
	else if (library->defn)
	    printf(" [defined, but not filled in.]\n");
	else
	    printf(" [no definition]\n");
    }
}

obj_t library_name(struct library *library)
{
    return library->name;
}

void list_modules(struct library *library)
{
    struct table *table = library->modules;
    int i;
    struct bucket *bucket;
    
    for (i = 0; i < table->length; i++) {
	for (bucket = table->table[i]; bucket != NULL; bucket = bucket->next) {
	    struct entry *entry = bucket->datum;
	    struct module *module = entry->datum;

	    printf("%c%c ",
		   entry->exported ? 'x' : ' ',
		   module->home == library ? ' ' : 'i');
	    fputs(sym_name(entry->name), stdout);
	    if (module->completed)
		printf("\n");
	    else if (module->defn)
		printf(" [defined, but not filled in.]\n");
	    else
		printf(" [no definition]\n");
	}
    }
}

obj_t module_name(struct module *module)
{
    return module->name;
}



/* Dylan interface */

static obj_t make_binding(struct variable *variable)
{
    if (variable->binding == NULL) {
	variable->binding = alloc(obj_BindingClass, sizeof(struct binding));
	BINDING(variable->binding)->variable = variable;
    }
    return variable->binding;
}

static obj_t make_dylan_module(struct module *module)
{
    if (module->dylan_module == NULL) {
	module->dylan_module = alloc(obj_ModuleClass, 
				     sizeof(struct dylan_module));
	MODULE(module->dylan_module)->module = module;
    }
    return module->dylan_module;
}

static obj_t make_dylan_library(struct library *library)
{
    if (library->dylan_library == NULL) {
	library->dylan_library = alloc(obj_LibraryClass, 
				     sizeof(struct dylan_library));
	LIBRARY(library->dylan_library)->library = library;
    }
    return library->dylan_library;
}

static obj_t dylan_resolve_name_in_module(obj_t name, obj_t module)
{
    /* name is symbol */
    struct variable *variable 
	= find_variable(MODULE(module)->module, name, FALSE, FALSE);
    if (variable == NULL) {
	return obj_False;
    } else {
	return make_binding(variable);
    }
}

static obj_t dylan_resolve_name_in_library(obj_t name, obj_t library)
{
    /* name is symbol */
    struct module *module
	= find_module(LIBRARY(library)->library, name, FALSE, FALSE);
    if (module == NULL) {
	return obj_False;
    } else {
	return make_dylan_module(module);
    }
}

static obj_t dylan_binding_value(obj_t binding)
{
    switch (BINDING(binding)->variable->kind) {
      case var_Assumed: case var_AssumedWriteable: 
	error("Binding %= is undefined, and so has no value", binding);
	return obj_False;  /* stop compiler warning */
      default:
	return BINDING(binding)->variable->value;
    }
}

/* return the type constraint on the binding, if any
 */
static obj_t dylan_binding_type(obj_t binding)
{
    return BINDING(binding)->variable->type;
}

static obj_t dylan_binding_kind(obj_t binding)
{
    switch (BINDING(binding)->variable->kind) {
      case var_Assumed: case var_AssumedWriteable: return symbol("undefined");
      case var_Constant: return symbol("constant");
      case var_Variable: return symbol("variable");
      case var_Class: return symbol("class");
      case var_GenericFunction: return symbol("generic-function");
      case var_Method: return symbol("method");
      default: 
	lose("Unknown kind of binding");
	return symbol("unknown");  /* Should never be reached, but suppresses
				      a compiler warning.. */
    }
}

static obj_t dylan_visible_bindings(obj_t module)
{
    return get_all_keys(MODULE(module)->module->variables);
}

static obj_t dylan_visible_modules(obj_t library)
{
    return get_all_keys(LIBRARY(library)->library->modules);
}

static obj_t dylan_exported_bindings(obj_t module)
{
    return MODULE(module)->module->defn->exports;
}

static obj_t dylan_exported_modules(obj_t library)
{
    return LIBRARY(library)->library->defn->exports;
}

static obj_t dylan_get_all_libraries(void)
{
    struct library *library;
    obj_t result = obj_Nil;

    for (library = Libraries; library != NULL; library = library->next) {
	result = pair(make_dylan_library(library), result);
    }
    return result;
}

static obj_t dylan_get_all_modules(void)
{
    struct module *module;
    obj_t result = obj_Nil;

    for (module = Modules; module != NULL; module = module->next) {
	result = pair(make_dylan_module(module), result);
    }
    return result;
}

static obj_t dylan_name_home_for_bindings(obj_t binding)
{
    return BINDING(binding)->variable->home->dylan_module;
}

static obj_t dylan_name_home_for_modules(obj_t module)
{
    return MODULE(module)->module->home->dylan_library;
}

static obj_t dylan_binding_name(obj_t binding)
{
    return BINDING(binding)->variable->name;
}

static obj_t dylan_module_name(obj_t module)
{
    return MODULE(module)->module->name;
}

static obj_t dylan_library_name(obj_t library)
{
    return LIBRARY(library)->library->name;
}

#if 0
/* A list of lists of lists... of symbols.  You'll have to flatten the
 * result to get anything useful out of this.  
 */
static obj_t all_exported_bindings (struct module *module)
{
    struct use *use;
    obj_t result = obj_Nil;

    for (use = module->defn->use; use != NULL; use = use->next) {
	if (use->export == obj_True) {
	    if (use->import == obj_True) {
		obj_t use_name = use->name;
		struct module *used_module;

		if (module->name == symbol("Dylan-User")) {
		    used_module = find_module(library_Dylan, use_name, 
					      TRUE, FALSE);
		} else {
		    used_module = find_module(module->home, use_name, 
					      TRUE, FALSE);
		}
		result = pair(all_exported_bindings(used_module), result);
	    } else {
		/* Not correct -- use->imports are not local names */
		result = pair(use->import, result);
	    }
	} else {
	    result = pair(use->export, result);
	}
    }
    return result;
}

static obj_t dylan_all_exported_bindings(obj_t dylan_module)
{
    return all_exported_bindings(MODULE(dylan_module)->module);
}
#endif



/* GC stuff. */

static int scav_binding(struct object *o)
{
    return sizeof(struct binding);
}

static obj_t trans_binding(obj_t binding)
{
    return transport(binding, sizeof(struct binding), TRUE);
}

static int scav_dylan_module(struct object *o)
{
    return sizeof(struct dylan_module);
}

static obj_t trans_dylan_module(obj_t module)
{
    return transport(module, sizeof(struct dylan_module), TRUE);
}

static int scav_dylan_library(struct object *o)
{
    return sizeof(struct dylan_library);
}

static obj_t trans_dylan_library(obj_t library)
{
    return transport(library, sizeof(struct dylan_library), TRUE);
}


static int scav_unbound(struct object *ptr)
{
    return sizeof(struct object);
}

static obj_t trans_unbound(obj_t unbound)
{
    return transport(unbound, sizeof(struct object), TRUE);
}

static void scav_use(struct use *use)
{
    scavenge(&use->name);
    scavenge(&use->import);
    scavenge(&use->prefix);
    scavenge(&use->exclude);
    scavenge(&use->rename);
    scavenge(&use->export);
}

static void scav_defn(struct defn *defn)
{
    struct use *use;

    scavenge(&defn->name);
    scavenge(&defn->exports);
    scavenge(&defn->creates);

    for (use = defn->use; use != NULL; use = use->next)
	scav_use(use);
}

static void scav_table(struct table *table, boolean of_entries)
{
    int i;
    struct bucket *bucket;
    
    for (i = 0; i < table->length; i++) {
	for (bucket = table->table[i]; bucket != NULL; bucket = bucket->next) {
	    scavenge(&bucket->symbol);
	    if (of_entries) {
		struct entry *entry = bucket->datum;
		scavenge(&entry->name);
		scavenge(&entry->p1);
		scavenge(&entry->p2);
		scavenge(&entry->p3);
	    }
	}
    }
}

static void scav_var(struct var *var)
{
    scavenge(&var->variable.name);
    if (var->variable.binding != NULL) {
	scavenge(&var->variable.binding);
    }
    scavenge(&var->variable.value);
    scavenge(&var->variable.type);
    scavenge(&var->variable.ref_file);
}

static void scav_module(struct module *module)
{
    scavenge(&module->name);
    if (module->dylan_module != NULL) {
	scavenge(&module->dylan_module);
    }
    scav_defn(module->defn);
    scav_table(module->variables, TRUE);
}

static void scav_library(struct library *library)
{
    scavenge(&library->name);
    if (library->dylan_library != NULL) {
	scavenge(&library->dylan_library);
    }
    scav_defn(library->defn);
    scav_table(library->modules, TRUE);
}

void scavenge_module_roots(void)
{
    struct library *library;
    struct module *module;
    struct var *var;

    scav_table(LibraryTable, FALSE);
    for (library = Libraries; library != NULL; library = library->next)
	scav_library(library);
    for (module = Modules; module != NULL; module = module->next)
	scav_module(module);
    for (var = Vars; var != NULL; var = var->next)
	scav_var(var);
}


/* Initialization stuff. */

void make_module_classes(void)
{
    obj_UnboundClass = make_builtin_class(scav_unbound, trans_unbound);
    obj_NameClass = make_abstract_class(TRUE);
    obj_NamespaceClass = make_abstract_class(TRUE);
    obj_LibraryClass
	= make_builtin_class(scav_dylan_library, trans_dylan_library);
    obj_ModuleClass
	= make_builtin_class(scav_dylan_module, trans_dylan_module);
    obj_BindingClass
	= make_builtin_class(scav_binding, trans_binding);
    add_constant_root(&obj_UnboundClass);
    add_constant_root(&obj_NameClass);
    add_constant_root(&obj_NamespaceClass);
    add_constant_root(&obj_LibraryClass);
    add_constant_root(&obj_ModuleClass);
    add_constant_root(&obj_BindingClass);
}

void init_modules(void)
{
    obj_t dylan = symbol("Dylan");
    obj_t stuff = symbol("Builtin-Stuff");

    LibraryTable = make_table("the table of all libraries");

    library_Dylan = find_library(dylan, TRUE);

    {
	/* Define the dylan-user library. */
	struct defn *defn = malloc(sizeof(*defn));
	struct use *use = malloc(sizeof(*use));
	defn->name = symbol("Dylan-User");
	defn->use = NULL;
	defn->exports = obj_Nil;
	defn->creates = NULL;
	add_use(defn, dylan);
	define_library(defn);
    }
	
    module_BuiltinStuff = make_module(stuff, library_Dylan);
    make_entry(library_Dylan->modules, stuff, module_BuiltinStuff, FALSE,
	       "module %s internal to library %s", stuff, dylan, obj_False);

    obj_Unbound = alloc(obj_UnboundClass, sizeof(struct object));
    add_constant_root(&obj_Unbound);
}

void init_module_classes(void)
{
    init_builtin_class(obj_UnboundClass, "<unbound-marker>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_NameClass, "<name>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_NamespaceClass, "<namespace>",
		       obj_ObjectClass, NULL);
    init_builtin_class(obj_LibraryClass, "<library>",
		       obj_NamespaceClass, NULL);
    init_builtin_class(obj_ModuleClass, "<module>",
		       obj_NameClass, obj_NamespaceClass, NULL);
    init_builtin_class(obj_BindingClass, "<binding>",
		       obj_NameClass, NULL);
}

void done_initializing_vars(void)
{
    InitializingVars = FALSE;
}

void finalize_modules(void)
{
    struct library *library;
    struct module *module;
    struct var *var;
    boolean warning_printed = FALSE;

    for (library = Libraries; library != NULL; library = library->next)
	if (!library->completed)
	    complete_library(library);
    for (module = Modules; module != NULL; module = module->next)
	if (!module->completed)
	    complete_module(module);

    for (var = Vars; var != NULL; var = var->next) {
	if (var->variable.defined) {
	    switch (var->variable.kind) {
	      case var_Assumed:
		var->variable.kind = var_GenericFunction;
		break;

	      case var_AssumedWriteable:
		error("Constant %s in module %s library %s is not changeable.",
		      var->variable.name, var->variable.home->name,
		      var->variable.home->home->name);
		break;

	      default:
		break;
	    }
	}
    }

    for (library = Libraries; library != NULL; library = library->next) {
	boolean library_printed = FALSE;
	for (module = Modules; module != NULL; module = module->next) {
	    if (module->home == library) {
		boolean module_printed = FALSE;
		for (var = Vars; var != NULL; var = var->next) {
		    if (var->variable.home==module && !var->variable.defined) {
			if (!warning_printed) {
			    fprintf(stderr, "Warning: the following variables "
				    "are undefined:\n");
			    warning_printed = TRUE;
			}
			if (!library_printed) {
			    fprintf(stderr, "  in library %s:\n",
				    sym_name(library->name));
			    library_printed = TRUE;
			}
			if (!module_printed) {
			    fprintf(stderr, "    in module %s:\n",
				    sym_name(module->name));
			    module_printed = TRUE;
			}
			fprintf(stderr, "      %s",
				sym_name(var->variable.name));
			if (var->variable.ref_file != obj_False) {
			    fprintf(stderr, " [%s, line %d]",
				    string_chars(var->variable.ref_file),
				    var->variable.ref_line);
			}
			fprintf(stderr, "\n");
		    }
		}
	    }
	}
    }
}

void init_module_functions (void)
{
    obj_t false_or_binding
	= type_union(obj_BindingClass, singleton(obj_False));
    obj_t false_or_module
	= type_union(obj_ModuleClass, singleton(obj_False));
    obj_t false_or_name
	= type_union(obj_NameClass, singleton(obj_False));

    define_function("binding-value", list1(obj_BindingClass), FALSE, obj_Nil,
		    FALSE, list1(obj_ObjectClass), dylan_binding_value);
    define_function("binding-type", list1(obj_BindingClass), FALSE, obj_Nil,
		    FALSE, list1(obj_ObjectClass), dylan_binding_type);
    define_function("binding-kind", list1(obj_BindingClass), FALSE, obj_Nil,
		    FALSE, list1(obj_SymbolClass), dylan_binding_kind);
    define_function("get-all-libraries", obj_Nil, FALSE, obj_Nil,
		    FALSE, list1(obj_ListClass), dylan_get_all_libraries);
    define_function("get-all-modules", obj_Nil, FALSE, obj_Nil,
		    FALSE, list1(obj_ListClass), dylan_get_all_modules);

    define_function("binding-name", list1(obj_BindingClass), FALSE, obj_Nil,
		    FALSE, list1(obj_SymbolClass), dylan_binding_name);
    define_function("module-name", list1(obj_ModuleClass), FALSE, obj_Nil,
		    FALSE, list1(obj_SymbolClass), dylan_module_name);
    define_function("library-name", list1(obj_LibraryClass), FALSE, obj_Nil,
		    FALSE, list1(obj_SymbolClass), dylan_library_name);

    define_generic_function("resolve-name", 
			    list2(obj_SymbolClass, obj_NamespaceClass),
			    FALSE, obj_False, FALSE, list1(false_or_name),
			    obj_False);
    define_method("resolve-name", list2(obj_SymbolClass, obj_ModuleClass),
		   FALSE, obj_False, FALSE, false_or_binding,
		  dylan_resolve_name_in_module);
    define_method("resolve-name", list2(obj_SymbolClass, obj_LibraryClass),
		   FALSE, obj_False, FALSE, false_or_module,
		  dylan_resolve_name_in_library);

    define_generic_function("exported-names", list1(obj_NamespaceClass),
			    FALSE, obj_False, FALSE, list1(obj_ListClass), 
			    obj_False);
    define_method("exported-names", list1(obj_ModuleClass),
		  FALSE, obj_False, FALSE, obj_ListClass,
		  dylan_exported_bindings);
    define_method("exported-names", list1(obj_LibraryClass),
		  FALSE, obj_False, FALSE, obj_ListClass,
		  dylan_exported_modules);
    
    define_generic_function("visible-names", list1(obj_NamespaceClass),
			    FALSE, obj_False, FALSE, list1(obj_ListClass), 
			    obj_False);
    define_method("visible-names", list1(obj_ModuleClass),
		  FALSE, obj_False, FALSE, obj_ListClass,
		  dylan_visible_bindings);
    define_method("visible-names", list1(obj_LibraryClass),
		  FALSE, obj_False, FALSE, obj_ListClass,
		  dylan_visible_modules);
    
    define_generic_function("name-home", list1(obj_NameClass),
			    FALSE, obj_False, FALSE, 
			    list1(obj_NamespaceClass), obj_False);
    define_method("name-home", list1(obj_BindingClass),
		  FALSE, obj_False, FALSE, obj_ModuleClass,
		  dylan_name_home_for_bindings);
    define_method("name-home", list1(obj_ModuleClass),
		  FALSE, obj_False, FALSE, obj_LibraryClass,
		  dylan_name_home_for_modules);
}
