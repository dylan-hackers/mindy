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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/gc.c,v 1.23 1996/01/22 23:00:40 wlott Exp $
*
* This file is the garbage collector.
*
\**********************************************************************/

#include "../compat/std-c.h"

#ifdef hpux
#include <sys/mman.h>
#endif

#include "mindy.h"
#include "class.h"
#include "gc.h"
#include "weak.h"
#include "table.h"
#include "module.h"
#include "bool.h"
#include "sym.h"
#include "num.h"
#include "error.h"

extern void scavenge_thread_roots(void);
extern void scavenge_bool_roots(void);
extern void scavenge_class_roots(void);
extern void scavenge_coll_roots(void);
extern void scavenge_func_roots(void);
extern void scavenge_instance_roots(void);
extern void scavenge_interp_roots(void);
extern void scavenge_list_roots(void);
extern void scavenge_num_roots(void);
extern void scavenge_obj_roots(void);
extern void scavenge_vec_roots(void);
extern void scavenge_str_roots(void);
extern void scavenge_char_roots(void);
extern void scavenge_symbol_roots(void);
extern void scavenge_type_roots(void);
extern void scavenge_module_roots(void);
extern void scavenge_value_roots(void);
extern void scavenge_debug_roots(void);
extern void scavenge_handler_roots(void);
extern void scavenge_load_roots(void);
extern void scavenge_nlx_roots(void);
extern void scavenge_driver_roots(void);
extern void scavenge_buffer_roots(void);
extern void scavenge_weak_roots(void);
extern void scavenge_brkpt_roots(void);
extern void scavenge_table_roots(void);
extern void scavenge_c_roots(void);

#define CHECKGC 0

boolean TimeToGC = FALSE;

struct block {
    struct block *next;
    void *base;
    void *end;
    void *fill;
};

#define BLOCK_SIZE (128*1024)
#define DEFAULT_BYTES_CONSED_BETWEEN_GCS (2*1024*1024)

static struct block *FreeBlocks = NULL;
static struct block *UsedBlocks = NULL;
#if CHECKGC
static struct block *OldBlocks = NULL;
#endif
static struct block *cur_block = NULL;
static void *cur_fill = NULL, *cur_end = NULL;
static int BytesInUse = 0;
static int BytesConsedBetweenGCs = DEFAULT_BYTES_CONSED_BETWEEN_GCS;
static int GCTrigger = DEFAULT_BYTES_CONSED_BETWEEN_GCS;

static int bytes_in_use(void)
{
    if (cur_block)
	return BytesInUse + ((char *)cur_fill - (char *)cur_block->base);
    else
	return BytesInUse;
}

void *raw_alloc(int bytes)
{
    void *result;

    if (bytes < 0)
	/* We don't return NULL because trying to allocate a negative number */
	/* of bytes is a sign that something real bad is trying to happen. */
	lose("Can't allocate a negative number of bytes: %d", bytes);

    /* round bytes up to the next dual-word boundy. */
    bytes = (bytes + 7) & ~7;

    if (bytes > BLOCK_SIZE - sizeof(struct block))
	return NULL;

    if ((char *)cur_fill + bytes > (char *)cur_end) {
	struct block *block;

	if (FreeBlocks) {
	    block = FreeBlocks;
	    FreeBlocks = block->next;
	}
	else {
#ifdef hpux
	    block = mmap(NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE,
			 MAP_ANONYMOUS | MAP_VARIABLE | MAP_PRIVATE,
			 -1, 0);
            if (block == (void *)-1)
		lose("Heap is full!  Can't allocate %d bytes", bytes);
#else
	    block = malloc(BLOCK_SIZE);
            if (block == NULL)
		lose("Heap is full!  Can't allocate %d bytes", bytes);
#endif
	    block->base = (char *)block + sizeof(struct block);
	    block->end = (char *)block + BLOCK_SIZE;
	}
	block->next = 0;

	if (cur_block) {
	    BytesInUse += (char *)cur_fill - (char *)cur_block->base;
	    cur_block->fill = cur_fill;
	    cur_block->next = block;
	    if (BytesInUse > GCTrigger)
		TimeToGC = TRUE;
	}
	else
	    UsedBlocks = block;

	cur_block = block;
	cur_fill = block->base;
	cur_end = block->end;
    }

    result = cur_fill;
    cur_fill = (char *)cur_fill + bytes;

    return result;
}

obj_t alloc(obj_t class, int bytes)
{
#if CHECKGC
    unsigned int *ptr;
#else
    void *ptr;
#endif
    obj_t result;

    if (class == NULL)
	lose("Tried to allocate a class that hasn't been created yet.");

#if CHECKGC
    if (class != ptr_obj(NULL)
	  && *obj_ptr(int *, class) == 0xfacefeed)
	lose("Tried to allocate a class that wasn't scavenged.");

    ptr = raw_alloc(bytes + sizeof(int)*2);
#else
    ptr = raw_alloc(bytes);
#endif

    if (ptr == NULL)
	error("Can't allocate %d bytes for %s",
	      make_fixnum(bytes),
	      CLASS(class)->debug_name);

#if CHECKGC
    ptr[0] = 0xbeadbabe;
    ptr[1] = bytes;

    result = ptr_obj(ptr + 2);
#else
    result = ptr_obj(ptr);
#endif

    obj_ptr(struct object *, result)->class = class;

    return result;
}

void shrink(obj_t obj, int new_bytes)
{
#if CHECKGC
    unsigned int *ptr = obj_ptr(unsigned int *, obj) - 2;

    if (new_bytes > ptr[1])
	lose("Can't shrink a %d byte object to %d bytes.", ptr[1], new_bytes);

    ptr[1] = new_bytes;
#endif    
}

struct forwarding_pointer {
    obj_t marker;
    obj_t new_value;
};

void scavenge(obj_t *addr)
{
    obj_t obj = *addr;

    if (obj_is_ptr(obj)) {
	obj_t class = obj_ptr(struct object *, obj)->class;
	if (class == ForwardingMarker)
	    *addr = obj_ptr(struct forwarding_pointer *, obj)->new_value;
	else
	    *addr = obj_ptr(struct class *, class)->transport(obj);
    }
}

obj_t transport(obj_t obj, int bytes)
{
#if CHECKGC
    unsigned int *new;
    unsigned int *ptr = obj_ptr(unsigned int *, obj) - 2;
#else
    void *new;
#endif
    obj_t new_obj;

#if CHECKGC
    if (ptr[0] != 0xbeadbabe)
	lose("Someone called transport with a bogus object.");
    if (ptr[1] != bytes)
	lose("Someone told transport that %d byte object was %d bytes.",
	     ptr[1], bytes);

    new = raw_alloc(bytes + sizeof(int)*2);
#else
    new = raw_alloc(bytes);
#endif

    if (new == NULL)
	lose("raw_alloc failed duing GC");

#if CHECKGC
    new_obj = ptr_obj(new + 2);

    memcpy(new, ptr, bytes + sizeof(int)*2);
#else
    new_obj = ptr_obj(new);
    memcpy(new, obj_ptr(void *, obj), bytes);
#endif

    obj_ptr(struct forwarding_pointer *, obj)->marker = ForwardingMarker;
    obj_ptr(struct forwarding_pointer *, obj)->new_value = new_obj;

    return new_obj;
}

static void scavenge_newspace(void)
{
    struct block *block = UsedBlocks;
    void *ptr, *end;
    obj_t class;
    int bytes;

    while (block != 0) {
	ptr = block->base;
	/* The reason for this double loop is so that we don't have to */
	/* do the block->next conditional each time around the inner loop. */
	while (ptr < (end = (block->next ? block->fill : cur_fill))) {
	    do {
#if CHECKGC
		unsigned int *header = ptr;
		if (header[0] != 0xbeadbabe)
		    lose("Scavenge_newspace found a bogus object.");
		ptr = (char *)ptr + sizeof(int)*2;
#endif
		scavenge((obj_t *)ptr);
		class = *(obj_t *)ptr;
		bytes = obj_ptr(struct class *, class)->scavenge(ptr);
#if CHECKGC
		if (header[1] != bytes)
		    lose("Some scavenger claimed a %d byte object "
			 "was %d bytes.",
			 header[1], bytes);
#endif
		ptr = (char *)ptr + ((bytes + 7) & ~7);
	    } while (ptr < end);
	}
	block = block->next;
    }
}

void collect_garbage(void)
{
    struct block *old_blocks = UsedBlocks;
    int bytes_at_start = bytes_in_use();
    int bytes_at_end;
    boolean print_message
	= find_variable(module_BuiltinStuff,
			symbol("*print-GC-messages*"),
			FALSE, TRUE)->value != obj_False;

    if (print_message) {
	fprintf(stderr, "[GCing with %d bytes in use...", bytes_at_start);
	fflush(stderr);
    }

    BytesInUse = 0;
    UsedBlocks = 0;
    cur_block = 0;
    cur_fill = 0;
    cur_end = 0;

    scavenge_thread_roots();
    scavenge_bool_roots();
    scavenge_class_roots();
    scavenge_coll_roots();
    scavenge_func_roots();
    scavenge_instance_roots();
    scavenge_interp_roots();
    scavenge_list_roots();
    scavenge_num_roots();
    scavenge_obj_roots();
    scavenge_vec_roots();
    scavenge_str_roots();
    scavenge_char_roots();
    scavenge_symbol_roots();
    scavenge_type_roots();
    scavenge_module_roots();
    scavenge_value_roots();
    scavenge_debug_roots();
    scavenge_handler_roots();
    scavenge_load_roots();
    scavenge_nlx_roots();
    scavenge_driver_roots();
    scavenge_buffer_roots();
    scavenge_weak_roots();
    scavenge_brkpt_roots();
    scavenge_table_roots();
    scavenge_c_roots();

    scavenge_newspace();

    break_weak_pointers();

#if CHECKGC
    {
	struct block *block, *next;
	for (block = OldBlocks; block != NULL; block = next) {
	    next = block->next;
	    block->next = FreeBlocks;
	    FreeBlocks = block;
	}
	OldBlocks = NULL;
	for (block = old_blocks; block != NULL; block = next) {
	    unsigned int *ptr;
	    next = block->next;
	    block->next = OldBlocks;
	    OldBlocks = block;
	    for (ptr = block->base; ptr < (unsigned int *)block->end; ptr++)
		*ptr = 0xfacefeed;
	}
    }
#else
    while (old_blocks != 0) {
	struct block *next = old_blocks->next;
	old_blocks->next = FreeBlocks;
	FreeBlocks = old_blocks;
	old_blocks = next;
    }
#endif

    bytes_at_end = bytes_in_use();
    GCTrigger = bytes_at_end + BytesConsedBetweenGCs;
    TimeToGC = FALSE;

    if (print_message) {
	fprintf(stderr, "reclaimed %d leaving %d]\n",
		bytes_at_start - bytes_at_end,
		bytes_at_end);
	fflush(stderr);
    }

    table_gc_hook();
}

void init_gc_functions(void)
{
    struct variable *var;
    obj_t namesym = symbol("*print-GC-messages*");

    define_variable(module_BuiltinStuff, namesym, var_Variable);
    var = find_variable(module_BuiltinStuff, namesym, FALSE, TRUE);
    var->function = func_No;
    var->type = obj_BooleanClass;
    var->value = obj_False;

    {
	char *str = getenv("BYTES_CONSED_BETWEEN_GCS");

	if (str) {
	    int bcbgcs = atoi(str);
	    if (bcbgcs < DEFAULT_BYTES_CONSED_BETWEEN_GCS) {
		fprintf(stderr,
			"Bogus value for BYTES_CONSED_BETWEEN_GCS, using %d\n",
			DEFAULT_BYTES_CONSED_BETWEEN_GCS);
	    }
	    else {
		GCTrigger += bcbgcs - BytesConsedBetweenGCs;
		BytesConsedBetweenGCs = bcbgcs;
	    }
	}
    }
}
