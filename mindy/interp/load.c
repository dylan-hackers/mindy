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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/load.c,v 1.5 1994/04/09 13:35:58 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <string.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/param.h>
#include <ctype.h>
#ifdef MACH
extern int open(const void *path, int flags, int mode);
extern int close(int fd);
extern int read(int fd, void *ptr, int bytes);
extern int access(const void *path, int flags);
#endif
#ifdef hpux
#include <unistd.h>
#endif

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
#include "../comp/fileops.h"
#include "load.h"

extern char *strerror(int errnum);
extern char *getenv(char *name);

#define BUFFER_SIZE 4096

struct top_level_form {
    obj_t method;
    struct top_level_form *next;
};

struct load_state {
    struct top_level_form *top_level_forms;
    struct top_level_form **top_level_forms_tail;
};

static struct load_state State = {NULL};

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
    obj_t source_file;
};

static obj_t (*opcodes[256])(struct load_info *info);


/* Utility routines. */

static int safe_read(struct load_info *info, void *ptr, int bytes)
{
    int count = read(info->fd, ptr, bytes);

    if (count < 0)
	error("error loading ~A: ~A",
	      make_string(info->name),
	      make_string(strerror(errno)));
    if (count == 0)
	error("premature EOF loading ~A", make_string(info->name));

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
	ptr += count;
	bytes -= count;
	info->ptr = info->end = info->buffer;

	while (bytes > BUFFER_SIZE) {
	    count = safe_read(info, ptr, bytes);
	    ptr += count;
	    bytes -= count;
	}
	
	if (bytes == 0)
	    return;
	
	count = safe_read(info, info->buffer, BUFFER_SIZE);
	info->end = info->buffer + count;
    }
}

void read_ordered_bytes(struct load_info *info, void *ptr, int bytes)
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

int read_byte(struct load_info *info)
{
    unsigned char *ptr = info->ptr;

    if (ptr == info->end) {
	ptr = info->buffer;
	info->end = ptr + safe_read(info, ptr, BUFFER_SIZE);
    }
    info->ptr = ptr+1;

    return *ptr;
}

unsigned short read_uint2(struct load_info *info)
{
    unsigned short res;

    assert(sizeof(res) == 2);

    read_ordered_bytes(info, &res, 2);

    return res;
}

int read_int4(struct load_info *info)
{
    int res;

    assert(sizeof(int)==4);

    read_ordered_bytes(info, &res, 4);

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

static obj_t fop_comment(struct load_info *info)
{
    while (read_byte(info) != '\n')
	;
    return obj_False;
}

static obj_t fop_byte_order(struct load_info *info)
{
    short x;

    assert(sizeof(x)==2);

    read_bytes(info, &x, 2);

    if (x == 1)
	info->swap_bytes = FALSE;
    else if (x == 256)
	info->swap_bytes = TRUE;
    else
	lose("Strange byte order.\n");

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
    return ref(info, read_uint2(info));
}

static obj_t fop_ref(struct load_info *info)
{
    return ref(info, read_int4(info));
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

static obj_t fop_signed_8(struct load_info *info)
{
    return make_fixnum((signed char)read_byte(info));
}

static obj_t fop_signed_16(struct load_info *info)
{
    short value;

    read_ordered_bytes(info, &value, 2);

    return make_fixnum(value);
}

static obj_t fop_signed_32(struct load_info *info)
{
    int value;

    read_ordered_bytes(info, &value, 4);

    return make_fixnum(value);
}

static obj_t fop_char(struct load_info *info)
{
    return int_char(read_byte(info));
}

static obj_t fop_single_float(struct load_info *info)
{
    float f;

    assert(sizeof(f) == 4);

    read_ordered_bytes(info, &f, 4);

    return make_single(f);
}

static obj_t fop_double_float(struct load_info *info)
{
    double d;

    assert(sizeof(d) == 8);

    read_ordered_bytes(info, &d, sizeof(d));

    return make_double(d);
}

static obj_t fop_short_string(struct load_info *info)
{
    int len = read_byte(info);
    obj_t res = alloc_string(len);

    read_bytes(info, string_chars(res), len);

    return res;
}

static obj_t fop_string(struct load_info *info)
{
    int len = read_int4(info);
    obj_t res = alloc_string(len);

    read_bytes(info, string_chars(res), len);

    return res;
}

static obj_t fop_short_symbol(struct load_info *info)
{
    return store(info, symbol(string_chars(fop_short_string(info))),
		 next_handle(info));
}

static obj_t fop_symbol(struct load_info *info)
{
    return store(info, symbol(string_chars(fop_string(info))),
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
	len = read_int4(info)+9+254+(1<<16);
    else if (len == 254)
	len = read_uint2(info)+9+254;
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

static obj_t read_component(struct load_info *info, int nconst, int nbytes)
{
    obj_t debug_name = read_thing(info);
    int frame_size = fixnum_value(read_thing(info));
    obj_t debug_info = read_thing(info);
    obj_t res = make_component(debug_name, frame_size, info->source_file,
			       debug_info, nconst, nbytes);
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
    int nbytes = read_uint2(info);

    return read_component(info, nconst, nbytes);
}

static obj_t fop_component(struct load_info *info)
{
    int nconst = read_int4(info);
    int nbytes = read_int4(info);

    return read_component(info, nconst, nbytes);
}

static obj_t read_method(struct load_info *info, int param_info,
			 int nclosure_vars)
{
    int nkeys = (param_info>>1)-1;
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

    return make_method_info(param_info & 1, keys, read_thing(info),
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
    int param_info = read_int4(info);
    int nclosure_vars = read_int4(info);

    return read_method(info, param_info, nclosure_vars);
}

static obj_t fop_in_library(struct load_info *info)
{
    obj_t name = read_thing(info);
    info->library = find_library(name, TRUE);
    if (CurLibrary == NULL)
	CurLibrary = info->library;
    return name;
}

static obj_t fop_in_module(struct load_info *info)
{
    obj_t name = read_thing(info);
    info->module = find_module(info->library, name, TRUE, TRUE);
    if (CurModule == NULL)
	CurModule = info->module;
    return name;
}

static obj_t fop_source_file(struct load_info *info)
{
    info->source_file = read_thing(info);
    return info->source_file;
}

static obj_t make_top_level_method(obj_t component)
{
    obj_t method_info = make_method_info(FALSE, obj_False, component, 0);
    return make_byte_method(method_info, obj_Nil, obj_Nil, obj_ObjectClass,
			    NULL);
}

static obj_t queue_top_level_form(obj_t component)
{
    struct top_level_form *new = malloc(sizeof(*new));

    new->method = make_top_level_method(component);
    new->next = NULL;

    *State.top_level_forms_tail = new;
    State.top_level_forms_tail = &new->next;

    return function_debug_name_or_self(new->method);
}

static obj_t fop_top_level_form(struct load_info *info)
{
    return queue_top_level_form(read_thing(info));
}

static obj_t fop_define_class(struct load_info *info)
{
    obj_t name = read_thing(info);
    struct variable *var;
    obj_t slot;

    define_variable(info->module, name, var_Class);
    var = find_variable(info->module, name, FALSE, TRUE);

    if (var->value != obj_Unbound)
	lose("Defining a class in an already initialized variable?");
    var->value = make_defined_class(name);

    while ((slot = read_thing(info)) != obj_False)
	define_variable(info->module, slot, var_Method);

    queue_top_level_form(read_thing(info));

    return name;
}

static obj_t fop_define_generic(struct load_info *info)
{
    obj_t name = read_thing(info);
    obj_t tlf = read_thing(info);

    define_variable(info->module, name, var_GenericFunction);
    queue_top_level_form(tlf);

    return name;
}

static obj_t fop_define_method(struct load_info *info)
{
    obj_t name = read_thing(info);
    obj_t tlf = read_thing(info);

    define_variable(info->module, name, var_Method);
    queue_top_level_form(tlf);

    return name;
}

static obj_t fop_define_constant(struct load_info *info)
{
    int num_names = fixnum_value(read_thing(info));
    int i;

    for (i = 0; i < num_names; i++)
	define_variable(info->module, read_thing(info), var_Constant);
    return queue_top_level_form(read_thing(info));
}

static obj_t fop_define_variable(struct load_info *info)
{
    int num_names = fixnum_value(read_thing(info));
    int i;

    for (i = 0; i < num_names; i++)
	define_variable(info->module, read_thing(info), var_Variable);
    return queue_top_level_form(read_thing(info));
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

static void load_group(struct load_info *info)
{
    info->done = FALSE;
    info->next_handle = 0;
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
    int fd = open(name, O_RDONLY, 0);
    struct load_info *info;

    if (fd < 0)
	error("Error loading ~A: ~A\n",
	      make_string(name),
	      make_string(strerror(errno)));

    info = make_load_info(name, fd);

    while (1) {
	load_group(info);
	if (info->ptr == info->end) {
	    int count = read(fd, info->buffer, BUFFER_SIZE);
	    if (count < 0)
		error("error loading ~A: ~A",
		      make_string(name),
		      make_string(strerror(errno)));
	    if (count == 0)
		break;
	    info->ptr = info->buffer;
	    info->end = info->ptr + count;
	}
    }
    close(info->fd);
    free_load_info(info);
}


/* Library loading. */

void load_library(obj_t name)
{
    char *load_path = getenv("MINDYPATH");
    char path[MAXPATHLEN];
    char *start, *ptr, *src, *dst;
    int c;

    if (load_path == NULL)
	load_path = "/afs/cs.cmu.edu/project/gwydion/mindy/lib";

    start = load_path;
    ptr = load_path;
    do {
	c = *ptr;
	if (c == ':' || c == '\0') {
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
	    strcpy(dst, ".dbc");
	    if (access(path, R_OK) == 0) {
		load(path);
		return;
	    }
	    start = ptr+1;
	}
	ptr++;
    } while (c != '\0');

    error("Can't find library ~S", name);
}


/* Stuff to run the inits. */

static void do_next_init(struct thread *thread);

static void did_top_level_form(struct thread *thread, obj_t *vals)
{
    thread->sp = vals;
    do_next_init(thread);
}

static void do_next_init(struct thread *thread)
{
    if (State.top_level_forms) {
	struct top_level_form *tlf = State.top_level_forms;
	State.top_level_forms = tlf->next;

	*thread->sp++ = tlf->method;

	free(tlf);

	set_c_continuation(thread, did_top_level_form);
	invoke(thread, 0);
    }
    else
	do_return(thread, pop_linkage(thread), thread->sp);
}

static void do_first_init(struct thread *thread, int nargs)
{
    assert(nargs == 0);
    push_linkage(thread, thread->sp);
    do_next_init(thread);
}

void load_do_inits(struct thread *thread)
{
    *thread->sp++ = make_raw_function("init", 0, FALSE, obj_False,
				      obj_Nil, obj_ObjectClass,
				      do_first_init);
    invoke(thread, 0);
}


/* GC hooks */

void scavenge_load_roots(void)
{
    struct top_level_form *tlf;

    for (tlf = State.top_level_forms; tlf != NULL; tlf = tlf->next)
	scavenge(&tlf->method);
}


/* Init stuff. */

void init_loader(void)
{
    int i;

    for (i = 0; i < 256; i++)
	opcodes[i] = fop_flame;

    opcodes[fop_COMMENT] = fop_comment;
    opcodes[fop_BYTE_ORDER] = fop_byte_order;
    opcodes[fop_STORE] = fop_store;
    opcodes[fop_SHORT_REF] = fop_short_ref;
    opcodes[fop_REF] = fop_ref;
    opcodes[fop_FALSE] = fop_false;
    opcodes[fop_TRUE] = fop_true;
    opcodes[fop_UNBOUND] = fop_unbound;
    opcodes[fop_SIGNED_8] = fop_signed_8;
    opcodes[fop_SIGNED_16] = fop_signed_16;
    opcodes[fop_SIGNED_32] = fop_signed_32;
    opcodes[fop_CHAR] = fop_char;
    opcodes[fop_SINGLE_FLOAT] = fop_single_float;
    opcodes[fop_DOUBLE_FLOAT] = fop_double_float;
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

    State.top_level_forms_tail = &State.top_level_forms;
}
