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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/gc.c,v 1.7 1994/04/12 19:47:16 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindy.h"
#include "class.h"
#include "gc.h"
#include "weak.h"
#include "table.h"

extern void scavenge_thread_roots(void);
extern void scavenge_bool_roots(void);
extern void scavenge_class_roots(void);
extern void scavenge_coll_roots(void);
extern void scavenge_func_roots(void);
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


#define CHECKGC 1

boolean TimeToGC = FALSE;

struct block {
    struct block *next;
    void *base;
    void *end;
    void *fill;
};

#define BLOCK_SIZE (128*1024)
#define BYTES_CONSED_BETWEEN_GCS (2*1024*1024)

static struct block *FreeBlocks = 0;
static struct block *UsedBlocks = 0;
#if CHECKGC
static struct block *OldBlocks = 0;
#endif
static struct block *cur_block = 0;
static void *cur_fill = 0, *cur_end = 0;
static int BytesInUse = 0;
static int GCTrigger = BYTES_CONSED_BETWEEN_GCS;

static int bytes_in_use(void)
{
    if (cur_block)
	return BytesInUse + (cur_fill - cur_block->base);
    else
	return BytesInUse;
}

void *raw_alloc(int bytes)
{
    void *result;

    /* round bytes up to the next dual-word boundy. */
    bytes = (bytes + 7) & ~7;

    if (bytes > BLOCK_SIZE - sizeof(struct block))
	lose("Can't allocate %d bytes, %d at most.",
	     bytes, BLOCK_SIZE - sizeof(struct block));

    if (cur_fill + bytes > cur_end) {
	struct block *block;

	if (FreeBlocks) {
	    block = FreeBlocks;
	    FreeBlocks = block->next;
	}
	else {
	    block = malloc(BLOCK_SIZE);
	    block->base = (void *)block + sizeof(struct block);
	    block->end = (void *)block + BLOCK_SIZE;
	}
	block->next = 0;

	if (cur_block) {
	    BytesInUse += cur_fill - cur_block->base;
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
    cur_fill += bytes;

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
	  && (unsigned int)obj_ptr(struct class *, class)->class == 0xfacefeed)
	lose("Tried to allocate a class that wasn't scavenged.");

    ptr = raw_alloc(bytes + 8);
    ptr[0] = 0xbeadbabe;
    ptr[1] = bytes;

    result = ptr_obj(ptr + 2);
#else
    ptr = raw_alloc(bytes);
    result = ptr_obj(ptr);
#endif

    obj_ptr(struct object *, result)->class = class;

    return result;
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

    new = raw_alloc(bytes + 8);
    new_obj = ptr_obj(new + 2);

    memcpy(new, ptr, bytes + 8);
#else
    new = raw_alloc(bytes);
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
		ptr += 8;
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
		ptr += (bytes + 7) & ~7;
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

    printf("[GCing with %d bytes in use...", bytes_at_start);
    fflush(stdout);

    TimeToGC = FALSE;
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
    GCTrigger = bytes_at_end + BYTES_CONSED_BETWEEN_GCS;

    printf("reclaimed %d leaving %d]\n",
	   bytes_at_start - bytes_at_end,
	   bytes_at_end);

    table_gc_hook();
}
