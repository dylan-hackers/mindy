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
* $Header: /scm/cvs/src/mindy/interp/load.c,v 1.1 1998/05/03 19:55:18 andreas Exp $
*
* This file implements the loader.
*
\**********************************************************************/

#include "../compat/std-c.h"
#include "../compat/std-os.h"

#include <ctype.h>

#include "mindy.h"
#include "bool.h"
#include "list.h"
#include "module.h"
#include "str.h"
#include "sym.h"
#include "num.h"
#include "thread.h"
#include "interp.h"
#include "func.h"
#include "obj.h"
#include "gc.h"
#include "class.h"
#include "char.h"
#include "driver.h"
#include "debug.h"
#include "instance.h"
#include "vec.h"
#include "error.h"
#include "def.h"
#include "../comp/fileops.h"
#include "load.h"

#if BUFSIZ > 4096
#define BUFFER_SIZE BUFSIZ
#else
#define BUFFER_SIZE 4096
#endif

struct form {
    obj_t method;
    struct form *next;
};

struct queue {
    struct form *head;
    struct form **tail;
};

struct load_state {
    struct queue everything;
    struct queue classes;
    struct queue top_level_forms;
};

static struct load_state State;

struct load_info {
    char *name;
    int fd;
    unsigned char *buffer, *ptr, *end;
    obj_t *table, *table_end;
    int next_handle;
    boolean swap_bytes;
    boolean done;
    struct library *library;
    struct module *module;
    obj_t mtime;
    obj_t source_file;
};

static obj_t (*opcodes[256])(struct load_info *info);

/* The library we think we're loading, to see if that's what we really found */
static obj_t currently_loading = NULL;


/* Utility routines. */

static int safe_read(struct load_info *info, void *ptr, int bytes)
{
    int count = read(info->fd, ptr, bytes);

    if (count < 0)
	error("error loading %s: %s",
	      make_byte_string(info->name),
	      make_byte_string(strerror(errno)));
    if (count == 0)
	error("premature EOF loading %s", make_byte_string(info->name));

    return count;
}

static void read_bytes(struct load_info *info, void *ptr, int bytes)
{
    int count = info->end - info->ptr;

    while (1) {
	if (bytes <= count) {
	    memcpy(ptr, info->ptr, bytes);
	    info->ptr += bytes;
	    return;
	}

	memcpy(ptr, info->ptr, count);
	ptr = (char *)ptr + count;
	bytes -= count;
	info->ptr = info->end = info->buffer;

	while (bytes > BUFFER_SIZE) {
	    count = safe_read(info, ptr, bytes);
	    ptr = (char *)ptr + count;
	    bytes -= count;
	}
	
	if (bytes == 0)
	    return;
	
	count = safe_read(info, info->buffer, BUFFER_SIZE);
	info->end = info->buffer + count;
    }
}

static void read_ordered_bytes(struct load_info *info, void *ptr, int bytes)
{
    if (info->swap_bytes) {
	unsigned char *dst = (unsigned char *)ptr + bytes;
	unsigned char *src = info->ptr;
	unsigned char *end = info->end;

	while (end-src < dst-(unsigned char *)ptr) {
	    while (src < end)
		*--dst = *src++;
	    src = info->buffer;
	    end = src + safe_read(info, src, BUFFER_SIZE);
	}
	while (dst > (unsigned char *)ptr)
	    *--dst = *src++;
	info->ptr = src;
	info->end = end;
    }
    else
	read_bytes(info, ptr, bytes);
}

static int read_byte(struct load_info *info)
{
    unsigned char *ptr = info->ptr;

    if (ptr == info->end) {
	ptr = info->buffer;
	info->end = ptr + safe_read(info, ptr, BUFFER_SIZE);
    }
    info->ptr = ptr+1;

    return *ptr;
}

static void unread_byte(struct load_info *info)
{
    if (info->ptr == info->buffer)
	lose("unread_byte used while buffer empty.");

    info->ptr--;
}

static unsigned short read_ushort(struct load_info *info)
{
    unsigned short res;

    read_ordered_bytes(info, &res, sizeof(res));

    return res;
}

static short read_short(struct load_info *info)
{
    short res;

    read_ordered_bytes(info, &res, sizeof(res));

    return res;
}

static int read_int(struct load_info *info)
{
    int res;

    read_ordered_bytes(info, &res, sizeof(res));

    return res;
}

static long read_long(struct load_info *info)
{
    long res;

    read_ordered_bytes(info, &res, sizeof(res));

    return res;
}

static obj_t read_thing(struct load_info *info)
{
    int byte = read_byte(info);

    return (*opcodes[byte])(info);
}


/* Actual loader operations. */

static obj_t fop_flame(struct load_info *info)
{
    lose("Bogus opcode in %s\n", info->name);
    return NULL;
}

static void check_size(struct load_info *info, int desired, char *what)
{
    int bytes = read_byte(info);

    if (bytes != desired)
	error("Wrong sized %s in %s: should be %d but is %d",
	      make_byte_string(what), make_byte_string(info->name),
	      make_fixnum(desired), make_fixnum(bytes));
}

static obj_t fop_header(struct load_info *info)
{
    short x;
    long magic;
    int major_version, minor_version;

    major_version = read_byte(info);
    minor_version = read_byte(info);

    if (major_version < file_MajorVersion)
	error("Obsolete .dbc file: %s", make_byte_string(info->name));
    if ((major_version > file_MajorVersion)
        | (minor_version > file_MinorVersion))
	error("Obsolete version of Mindy for %s",
	      make_byte_string(info->name));

    check_size(info, sizeof(short), "short");
    check_size(info, sizeof(int), "int");
    check_size(info, sizeof(long), "long");
    check_size(info, sizeof(float), "float");
    check_size(info, sizeof(double), "double");
    check_size(info, sizeof(long double), "long double");

    read_bytes(info, &x, sizeof(short));
    info->swap_bytes = (x != 1);

    magic = read_int(info);

    if (magic != dbc_MagicNumber)
	error("Invalid .dbc file: %s", make_byte_string(info->name));
	
    return obj_False;
}

static int next_handle(struct load_info *info)
{
    int res = info->next_handle++;
    return res;
}

static obj_t store(struct load_info *info, obj_t value, int handle)
{
    int size = info->table_end - info->table;

    if (handle >= size) {
	if (handle < 16*1024) {
	    if (size == 0)
		size = 1024;
	    while (handle >= size)
		size *= 2;
	}
	else
	    size = ((handle + 16*1024-1) / (16*1024)) * 16*1024;
	if (info->table)
	    info->table = realloc(info->table, sizeof(obj_t) * size);
	else
	    info->table = malloc(sizeof(obj_t) * size);
	info->table_end = info->table + size;
    }

    info->table[handle] = value;

    return value;
}

static obj_t fop_store(struct load_info *info)
{
    int handle = next_handle(info);
    return(store(info, read_thing(info), handle));
}

static obj_t ref(struct load_info *info, int index)
{
    int table_size = info->table_end - info->table;

    if (index < 0 || index >= table_size)
	lose("Bogus ref index %d, should be >= 0 and < %d\n",
	     index, table_size);

    return info->table[index];
}

static obj_t fop_short_ref(struct load_info *info)
{
    return ref(info, read_ushort(info));
}

static obj_t fop_ref(struct load_info *info)
{
    return ref(info, read_int(info));
}

static obj_t fop_false(struct load_info *info)
{
    return obj_False;
}

static obj_t fop_true(struct load_info *info)
{
    return obj_True;
}

static obj_t fop_unbound(struct load_info *info)
{
    return obj_Unbound;
}

static obj_t fop_signed_byte(struct load_info *info)
{
    return make_fixnum((signed char)read_byte(info));
}

static obj_t fop_signed_short(struct load_info *info)
{
    return make_fixnum(read_short(info));
}

static obj_t fop_signed_int(struct load_info *info)
{
    return make_fixnum(read_int(info));
}

static obj_t fop_signed_long(struct load_info *info)
{
    return make_fixnum(read_long(info));
}

static obj_t fop_char(struct load_info *info)
{
    return int_char(read_byte(info));
}

static obj_t fop_single_float(struct load_info *info)
{
    float f;

    read_ordered_bytes(info, &f, 4);

    return make_single(f);
}

static obj_t fop_double_float(struct load_info *info)
{
    double d;

    read_ordered_bytes(info, &d, sizeof(d));

    return make_double(d);
}

static obj_t fop_extended_float(struct load_info *info)
{
    long double d;

    read_ordered_bytes(info, &d, sizeof(d));

    return make_extended(d);
}

static obj_t fop_short_string(struct load_info *info)
{
    int len = read_byte(info);
    obj_t res = alloc_byte_string(len);

    read_bytes(info, string_chars(res), len);

    return res;
}

static obj_t fop_string(struct load_info *info)
{
    int len = read_int(info);
    obj_t res = alloc_byte_string(len);

    read_bytes(info, string_chars(res), len);

    return res;
}

static obj_t fop_short_symbol(struct load_info *info)
{
    return store(info, symbol((char *)string_chars(fop_short_string(info))),
		 next_handle(info));
}

static obj_t fop_symbol(struct load_info *info)
{
    return store(info, symbol((char *)string_chars(fop_string(info))),
		 next_handle(info));
}

static obj_t fop_nil(struct load_info *info)
{
    return obj_Nil;
}

static obj_t read_list(struct load_info *info, int len, boolean dotted)
{
    obj_t result, *prev;

    prev = &result;

    while (len-- > 0) {
	obj_t new = pair(read_thing(info), obj_False);
	*prev = new;
	prev = &TAIL(new);
    }

    if (dotted)
	*prev = read_thing(info);
    else
	*prev = obj_Nil;

    return result;
}

static obj_t fop_list1(struct load_info *info)
{
    return pair(read_thing(info), obj_Nil);
}

static obj_t fop_list2(struct load_info *info)
{
    return read_list(info, 2, FALSE);
}

static obj_t fop_list3(struct load_info *info)
{
    return read_list(info, 3, FALSE);
}

static obj_t fop_list4(struct load_info *info)
{
    return read_list(info, 4, FALSE);
}

static obj_t fop_list5(struct load_info *info)
{
    return read_list(info, 5, FALSE);
}

static obj_t fop_list6(struct load_info *info)
{
    return read_list(info, 6, FALSE);
}

static obj_t fop_list7(struct load_info *info)
{
    return read_list(info, 7, FALSE);
}

static obj_t fop_list8(struct load_info *info)
{
    return read_list(info, 8, FALSE);
}

static obj_t fop_listn(struct load_info *info)
{
    return read_list(info, read_byte(info)+9, FALSE);
}

static obj_t fop_dotted_list1(struct load_info *info)
{
    return read_list(info, 1, TRUE);
}

static obj_t fop_dotted_list2(struct load_info *info)
{
    return read_list(info, 2, TRUE);
}

static obj_t fop_dotted_list3(struct load_info *info)
{
    return read_list(info, 3, TRUE);
}

static obj_t fop_dotted_list4(struct load_info *info)
{
    return read_list(info, 4, TRUE);
}

static obj_t fop_dotted_list5(struct load_info *info)
{
    return read_list(info, 5, TRUE);
}

static obj_t fop_dotted_list6(struct load_info *info)
{
    return read_list(info, 6, TRUE);
}

static obj_t fop_dotted_list7(struct load_info *info)
{
    return read_list(info, 7, TRUE);
}

static obj_t fop_dotted_list8(struct load_info *info)
{
    return read_list(info, 8, TRUE);
}

static obj_t fop_dotted_listn(struct load_info *info)
{
    return read_list(info, read_byte(info)+9, TRUE);
}

static obj_t read_vector(struct load_info *info, int len)
{
    obj_t res = make_vector(len, NULL);
    int i;

    for (i = 0; i < len; i++)
	SOVEC(res)->contents[i] = read_thing(info);

    return res;
}

static obj_t fop_vector0(struct load_info *info)
{
    return read_vector(info, 0);
}

static obj_t fop_vector1(struct load_info *info)
{
    return read_vector(info, 1);
}

static obj_t fop_vector2(struct load_info *info)
{
    return read_vector(info, 2);
}

static obj_t fop_vector3(struct load_info *info)
{
    return read_vector(info, 3);
}

static obj_t fop_vector4(struct load_info *info)
{
    return read_vector(info, 4);
}

static obj_t fop_vector5(struct load_info *info)
{
    return read_vector(info, 5);
}

static obj_t fop_vector6(struct load_info *info)
{
    return read_vector(info, 6);
}

static obj_t fop_vector7(struct load_info *info)
{
    return read_vector(info, 7);
}

static obj_t fop_vector8(struct load_info *info)
{
    return read_vector(info, 8);
}

static obj_t fop_vectorn(struct load_info *info)
{
    int len = read_byte(info);

    if (len == 255)
	len = read_int(info)+9+254+(1<<16);
    else if (len == 254)
	len = read_ushort(info)+9+254;
    else
	len += 9;

    return read_vector(info, len);
}

static obj_t fop_value_cell(struct load_info *info)
{
    return rawptr_obj(find_variable(info->module, read_thing(info), FALSE, TRUE));
}

static obj_t fop_writable_value_cell(struct load_info *info)
{
    return rawptr_obj(find_variable(info->module, read_thing(info), TRUE, TRUE));
}

static obj_t fop_builtin_value_cell(struct load_info *info)
{
    return rawptr_obj(find_variable(module_BuiltinStuff, read_thing(info),
				    FALSE, TRUE));
}

static obj_t fop_builtin_writable_value_cell(struct load_info *info)
{
    return rawptr_obj(find_variable(module_BuiltinStuff, read_thing(info),
				    TRUE, TRUE));
}

static obj_t fop_note_reference(struct load_info *info)
{
    int line = read_int(info);
    obj_t var_obj = read_thing(info);
    struct variable *var = obj_rawptr(var_obj);

    if (var->ref_file == obj_False) {
	var->ref_file = info->source_file;
	var->ref_line = line;
    }

    return var_obj;
}

static obj_t read_component(struct load_info *info, int nconst, int nbytes)
{
    obj_t debug_name = read_thing(info);
    int frame_size = fixnum_value(read_thing(info));
    obj_t debug_info = read_thing(info);
    obj_t res = make_component(debug_name, frame_size, info->mtime,
			       info->source_file, debug_info, nconst, nbytes);
    int i;

    for (i = 0; i < nconst; i++)
	obj_ptr(struct component *, res)->constant[i] = read_thing(info);
    read_bytes(info, &obj_ptr(struct component *, res)->constant[nconst],
	       nbytes);

    return res;
}

static obj_t fop_short_component(struct load_info *info)
{
    int nconst = read_byte(info);
    int nbytes = read_ushort(info);

    return read_component(info, nconst, nbytes);
}

static obj_t fop_component(struct load_info *info)
{
    int nconst = read_int(info);
    int nbytes = read_int(info);

    return read_component(info, nconst, nbytes);
}

static obj_t read_method(struct load_info *info, int param_info,
			 int nclosure_vars)
{
    boolean restp = param_info & 1;
    boolean all_keys = param_info & 2;
    int nkeys = (param_info>>2)-1;
    obj_t keys;

    if (nkeys == -1)
	keys = obj_False;
    else {
	obj_t *prev = &keys;
	while (nkeys-- > 0) {
	    obj_t key = read_thing(info);
	    obj_t def = read_thing(info);
	    obj_t keyinfo = pair(key, def);
	    obj_t new = list1(keyinfo);
	    *prev = new;
	    prev = &TAIL(new);
	}
	*prev = obj_Nil;
    }

    return make_method_info(restp, keys, all_keys, read_thing(info),
			    nclosure_vars);
}

static obj_t fop_short_method(struct load_info *info)
{
    int param_info = read_byte(info);
    int nclosure_vars = read_byte(info);

    return read_method(info, param_info, nclosure_vars);
}

static obj_t fop_method(struct load_info *info)
{
    int param_info = read_int(info);
    int nclosure_vars = read_int(info);

    return read_method(info, param_info, nclosure_vars);
}

static obj_t fop_in_library(struct load_info *info)
{
    obj_t name = read_thing(info);
    if (currently_loading != NULL && name != currently_loading) 
	error("Trying to library %s, but found library %s in file:\n  %s",
	      currently_loading, name, make_byte_string(info->name));

    info->library = find_library(name, TRUE);
    if (CurLibrary == NULL)
	CurLibrary = info->library;
    return name;
}

static obj_t fop_in_module(struct load_info *info)
{
    obj_t name = read_thing(info);
    info->module = find_module(info->library, name, TRUE, TRUE);
    if (CurLibrary == info->library && CurModule == NULL)
	CurModule = info->module;
    return name;
}

static obj_t fop_source_file(struct load_info *info)
{
    info->mtime = read_thing(info);
    info->source_file = read_thing(info);
    return info->source_file;
}

static obj_t make_top_level_method(obj_t component)
{
    obj_t method_info = make_method_info(FALSE, obj_False, FALSE,
					 component, 0);
    return make_byte_method(method_info, obj_Nil, obj_Nil, obj_ObjectClass,
			    NULL);
}

static obj_t queue_form(struct queue *queue, obj_t component)
{
    struct form *new = malloc(sizeof(*new));

    new->method = make_top_level_method(component);
    new->next = NULL;

    *queue->tail = new;
    queue->tail = &new->next;

    return function_debug_name_or_self(new->method);
}

static obj_t fop_top_level_form(struct load_info *info)
{
    return queue_form(&State.top_level_forms, read_thing(info));
}

static obj_t fop_define_class(struct load_info *info)
{
    obj_t name = read_thing(info);
    struct variable *var;
    obj_t slot;

    define_variable(info->module, name, var_Class);
    var = find_variable(info->module, name, FALSE, TRUE);

    if (var->value != obj_Unbound)
	error("Can't both define class and define method %s", name);

    var->value = make_defined_class(name, info->library);

    while ((slot = read_thing(info)) != obj_False)
	define_variable(info->module, slot, var_Method);

    queue_form(&State.classes, read_thing(info));
    queue_form(&State.top_level_forms, read_thing(info));

    return name;
}

static obj_t fop_define_generic(struct load_info *info)
{
    obj_t name = read_thing(info);
    obj_t tlf = read_thing(info);

    define_variable(info->module, name, var_GenericFunction);
    queue_form(&State.top_level_forms, tlf);

    return name;
}

static obj_t fop_define_method(struct load_info *info)
{
    obj_t name = read_thing(info);
    obj_t tlf = read_thing(info);

    define_variable(info->module, name, var_Method);
    queue_form(&State.top_level_forms, tlf);

    return name;
}

static obj_t fop_define_constant(struct load_info *info)
{
    int num_names = fixnum_value(read_thing(info));
    int i;

    for (i = 0; i < num_names; i++)
	define_variable(info->module, read_thing(info), var_Constant);
    return queue_form(&State.top_level_forms, read_thing(info));
}

static obj_t fop_define_variable(struct load_info *info)
{
    int num_names = fixnum_value(read_thing(info));
    int i;

    for (i = 0; i < num_names; i++)
	define_variable(info->module, read_thing(info), var_Variable);
    return queue_form(&State.top_level_forms, read_thing(info));
}

static struct defn *read_defn(struct load_info *info, boolean read_creates)
{
    struct defn *defn = malloc(sizeof(struct defn));
    struct use *use, **prev;
    obj_t name;

    defn->name = read_thing(info);
    prev = &defn->use;
    while ((name = read_thing(info)) != obj_False) {
	use = malloc(sizeof(struct use));
	use->name = name;
	use->import = read_thing(info);
	use->exclude = read_thing(info);
	use->prefix = read_thing(info);
	use->rename = read_thing(info);
	use->export = read_thing(info);
	*prev = use;
	prev = &use->next;
    }
    *prev = NULL;
    defn->exports = read_thing(info);
    if (read_creates)
	defn->creates = read_thing(info);
    else
	defn->creates = obj_Nil;

    return defn;
}

static obj_t fop_define_library(struct load_info *info)
{
    struct defn *defn = read_defn(info, FALSE);

    define_library(defn);

    return defn->name;
}

static obj_t fop_define_module(struct load_info *info)
{
    struct defn *defn = read_defn(info, TRUE);

    define_module(info->library, defn);

    return defn->name;
}

static obj_t fop_done(struct load_info *info)
{
    info->done = TRUE;
    return obj_False;
}


/* Interface routines. */

static void skip_header(struct load_info *info)
{
    int c;

    while ((c = read_byte(info)) == '#')
	while ((c = read_byte(info)) != '\n')
	    ;

    if (c != fop_HEADER)
	error("Invalid .dbc file: %s", make_byte_string(info->name));

    unread_byte(info);
}

static void load_group(struct load_info *info)
{
    info->done = FALSE;
    info->next_handle = 0;

    skip_header(info);

    while (!info->done)
	read_thing(info);
}

struct load_info *make_load_info(char *name, int fd)
{
    struct load_info *info
	= (struct load_info *)malloc(sizeof(struct load_info));

    info->name = name;
    info->fd = fd;
    info->buffer = (unsigned char *)malloc(BUFFER_SIZE);
    info->ptr = info->end = info->buffer;
    info->table = info->table_end = 0;
    info->swap_bytes = FALSE;
    info->done = FALSE;
    info->library = NULL;
    info->module = NULL;
    info->mtime = make_fixnum(0);
    info->source_file = obj_False;

    return info;
}

static void free_load_info(struct load_info *info)
{
    if (info->table)
	free(info->table);
    free(info->buffer);
    free(info);
}

void load(char *name)
{
    int fd;
    struct load_info *info;

    if (strcmp(name, "-") == 0)
      fd = 0;
    else {
#if WIN32
      fd = open(name, O_RDONLY | O_BINARY, 0);
#else
      fd = open(name, O_RDONLY, 0);
#endif
    }
    if (fd < 0)
	error("Error loading %s: %s\n",
	      make_byte_string(name),
	      make_byte_string(strerror(errno)));

    info = make_load_info(name, fd);

    while (1) {
	load_group(info);
	if (info->ptr == info->end) {
	    int count = read(fd, info->buffer, BUFFER_SIZE);
	    if (count < 0)
		error("error loading %s: %s",
		      make_byte_string(name),
		      make_byte_string(strerror(errno)));
	    if (count == 0)
		break;
	    info->ptr = info->buffer;
	    info->end = info->ptr + count;
	}
    }
    if (info->fd != 0)
      close(info->fd);
    free_load_info(info);
}


/* Library loading. */

#ifdef WIN32
#    define SEPARATOR_CHAR ';'
#else
#    define SEPARATOR_CHAR ':'
#endif

void load_library(obj_t name)
{
    char *load_path = getenv("DYLANPATH");
    char path[MAXPATHLEN];
    char default_path[MAXPATHLEN];
    char *start, *ptr, *src, *dst;
    int c;
    obj_t was_loading;

    was_loading = currently_loading;
    currently_loading = name;

    if (load_path == NULL) {
	/* no load path, compute default_path */
	char *dylandir = getenv("DYLANDIR");
	char* next = default_path;
	*next++ = '.';
	*next++ = SEPARATOR_CHAR;
	if (dylandir == NULL) {
	    memcpy(next, LIBDIR, strlen(LIBDIR));
	    next += strlen(LIBDIR);
	}
	else {
	    memcpy(next, dylandir, strlen(dylandir));
	    next += strlen(dylandir);
	    memcpy(next, "/lib/dylan", strlen("/lib/dylan"));
	    next += strlen("/lib/dylan");
	}
	*next = '\0';
	load_path = default_path;
    }

    start = load_path;
    ptr = load_path;
    do {
	c = *ptr;
	if (c == SEPARATOR_CHAR || c == '\0') {
	    int len = ptr - start;
	    if (len) {
		memcpy(path, start, len);
		path[len++] = '/';
	    }
	    dst = path+len;
	    for (src = sym_name(name); *src != '\0'; src++)
		if (isupper(*src))
		    *dst++ = tolower(*src);
		else
		    *dst++ = *src;
	    strcpy(dst, "-lib.dbc");
	    if (access(path, R_OK) == 0) {
		load(path);
		currently_loading = was_loading;
		return;
	    }
	    strcpy(dst, ".dbc");
	    if (access(path, R_OK) == 0) {
		load(path);
		currently_loading = was_loading;
		return;
	    }
	    start = ptr+1;
	}
	ptr++;
    } while (c != '\0');

    error("Can't find library %s", name);
}


/* Stuff to run the inits. */

static void do_next_init(struct thread *thread);

static void did_form(struct thread *thread, obj_t *vals)
{
    thread->sp = vals;
    do_next_init(thread);
}

static void do_next_init(struct thread *thread)
{
    if (State.everything.head) {
	struct form *tlf = State.everything.head;
	struct form *next = tlf->next;

	State.everything.head = next;
	if (next == NULL)
	    State.everything.tail = &State.everything.head;

	*thread->sp++ = tlf->method;

	free(tlf);

	set_c_continuation(thread, did_form);
	invoke(thread, 0);
    }
    else
	do_return(thread, pop_linkage(thread), thread->sp);
}

static void do_first_init(struct thread *thread, int nargs)
{
    assert(nargs == 0);
    push_linkage(thread, thread->sp);

    /* Move the class inits to the end of the everything list. */
    if (State.classes.head) {
	*State.everything.tail = State.classes.head;
	State.everything.tail = State.classes.tail;

	State.classes.head = NULL;
	State.classes.tail = &State.classes.head;
    }

    /* Move the tlfs to the end of the everything list. */
    if (State.top_level_forms.head) {
	*State.everything.tail = State.top_level_forms.head;
	State.everything.tail = State.top_level_forms.tail;

	State.top_level_forms.head = NULL;
	State.top_level_forms.tail = &State.top_level_forms.head;
    }

    do_next_init(thread);
}

void load_do_inits(struct thread *thread)
{
    *thread->sp++ = make_raw_function("init", obj_Nil, FALSE, obj_False, FALSE,
				      obj_Nil, obj_ObjectClass,
				      do_first_init);
    invoke(thread, 0);
}


/* Dylan interface. */

static void dylan_load(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t name = args[0];

    push_linkage(thread, args);

    load(string_chars(name));

    thread->sp = pop_linkage(thread);
    load_do_inits(thread);
}

static void dylan_load_library(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t name = args[0];
    push_linkage(thread, args);

    load_library(name);

    thread->sp = pop_linkage(thread);
    load_do_inits(thread);
}


/* GC hooks */

void scavenge_load_roots(void)
{
    struct form *tlf;

    for (tlf = State.everything.head; tlf != NULL; tlf = tlf->next)
	scavenge(&tlf->method);
    for (tlf = State.classes.head; tlf != NULL; tlf = tlf->next)
	scavenge(&tlf->method);
    for (tlf = State.top_level_forms.head; tlf != NULL; tlf = tlf->next)
	scavenge(&tlf->method);
}


/* Init stuff. */

void init_load_functions(void)
{
    define_generic_function("load", list1(obj_ByteStringClass),
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("load"),
			     FALSE, FALSE)->value,
	       make_raw_method("load", list1(obj_ByteStringClass),
			       FALSE, obj_False, FALSE, obj_Nil,
			       obj_False, dylan_load));
    define_generic_function("load-library", list1(obj_SymbolClass),
			    FALSE, obj_False, FALSE,
			    obj_Nil, obj_False);
    add_method(find_variable(module_BuiltinStuff, symbol("load-library"),
			     FALSE, FALSE)->value,
	       make_raw_method("load-library", list1(obj_SymbolClass),
			       FALSE, obj_False, FALSE, obj_Nil,
			       obj_False, dylan_load_library));
}

void init_loader(void)
{
    int i;

    for (i = 0; i < 256; i++)
	opcodes[i] = fop_flame;

    opcodes[fop_HEADER] = fop_header;
    opcodes[fop_STORE] = fop_store;
    opcodes[fop_SHORT_REF] = fop_short_ref;
    opcodes[fop_REF] = fop_ref;
    opcodes[fop_FALSE] = fop_false;
    opcodes[fop_TRUE] = fop_true;
    opcodes[fop_UNBOUND] = fop_unbound;
    opcodes[fop_SIGNED_BYTE] = fop_signed_byte;
    opcodes[fop_SIGNED_SHORT] = fop_signed_short;
    opcodes[fop_SIGNED_INT] = fop_signed_int;
    opcodes[fop_SIGNED_LONG] = fop_signed_long;
    opcodes[fop_CHAR] = fop_char;
    opcodes[fop_SINGLE_FLOAT] = fop_single_float;
    opcodes[fop_DOUBLE_FLOAT] = fop_double_float;
    opcodes[fop_EXTENDED_FLOAT] = fop_extended_float;
    opcodes[fop_SHORT_STRING] = fop_short_string;
    opcodes[fop_STRING] = fop_string;
    opcodes[fop_SHORT_SYMBOL] = fop_short_symbol;
    opcodes[fop_SYMBOL] = fop_symbol;
    opcodes[fop_NIL] = fop_nil;
    opcodes[fop_LIST1] = fop_list1;
    opcodes[fop_LIST2] = fop_list2;
    opcodes[fop_LIST3] = fop_list3;
    opcodes[fop_LIST4] = fop_list4;
    opcodes[fop_LIST5] = fop_list5;
    opcodes[fop_LIST6] = fop_list6;
    opcodes[fop_LIST7] = fop_list7;
    opcodes[fop_LIST8] = fop_list8;
    opcodes[fop_LISTN] = fop_listn;
    opcodes[fop_DOTTED_LIST1] = fop_dotted_list1;
    opcodes[fop_DOTTED_LIST2] = fop_dotted_list2;
    opcodes[fop_DOTTED_LIST3] = fop_dotted_list3;
    opcodes[fop_DOTTED_LIST4] = fop_dotted_list4;
    opcodes[fop_DOTTED_LIST5] = fop_dotted_list5;
    opcodes[fop_DOTTED_LIST6] = fop_dotted_list6;
    opcodes[fop_DOTTED_LIST7] = fop_dotted_list7;
    opcodes[fop_DOTTED_LIST8] = fop_dotted_list8;
    opcodes[fop_DOTTED_LISTN] = fop_dotted_listn;
    opcodes[fop_VECTOR0] = fop_vector0;
    opcodes[fop_VECTOR1] = fop_vector1;
    opcodes[fop_VECTOR2] = fop_vector2;
    opcodes[fop_VECTOR3] = fop_vector3;
    opcodes[fop_VECTOR4] = fop_vector4;
    opcodes[fop_VECTOR5] = fop_vector5;
    opcodes[fop_VECTOR6] = fop_vector6;
    opcodes[fop_VECTOR7] = fop_vector7;
    opcodes[fop_VECTOR8] = fop_vector8;
    opcodes[fop_VECTORN] = fop_vectorn;
    opcodes[fop_VALUE_CELL] = fop_value_cell;
    opcodes[fop_WRITABLE_VALUE_CELL] = fop_writable_value_cell;
    opcodes[fop_BUILTIN_VALUE_CELL] = fop_builtin_value_cell;
    opcodes[fop_BUILTIN_WRITABLE_VALUE_CELL] = fop_builtin_writable_value_cell;
    opcodes[fop_NOTE_REFERENCE] = fop_note_reference;
    opcodes[fop_SHORT_COMPONENT] = fop_short_component;
    opcodes[fop_COMPONENT] = fop_component;
    opcodes[fop_METHOD] = fop_method;
    opcodes[fop_SHORT_METHOD] = fop_short_method;
    opcodes[fop_IN_LIBRARY] = fop_in_library;
    opcodes[fop_IN_MODULE] = fop_in_module;
    opcodes[fop_SOURCE_FILE] = fop_source_file;
    opcodes[fop_TOP_LEVEL_FORM] = fop_top_level_form;
    opcodes[fop_DEFINE_CONSTANT] = fop_define_constant;
    opcodes[fop_DEFINE_VARIABLE] = fop_define_variable;
    opcodes[fop_DEFINE_GENERIC] = fop_define_generic;
    opcodes[fop_DEFINE_METHOD] = fop_define_method;
    opcodes[fop_DEFINE_CLASS] = fop_define_class;
    opcodes[fop_DEFINE_LIBRARY] = fop_define_library;
    opcodes[fop_DEFINE_MODULE] = fop_define_module;
    opcodes[fop_DONE] = fop_done;

    State.everything.head = NULL;
    State.everything.tail = &State.everything.head;
    State.classes.head = NULL;
    State.classes.tail = &State.classes.head;
    State.top_level_forms.head = NULL;
    State.top_level_forms.tail = &State.top_level_forms.head;
}
