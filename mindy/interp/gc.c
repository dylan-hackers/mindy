/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
*     University, and the Gwydion Dylan Maintainers.
*  
*  This software is made available "as is".  Neither the authors nor
*  Carnegie Mellon University make any warranty about the software,
*  its performance, or its conformity to any specification.
*  
*  Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
*  comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
*  Also, see http://www.gwydiondylan.org/ for updates and documentation. 
*
***********************************************************************
*
* $Header: /scm/cvs/src/mindy/interp/gc.c,v 1.8 2003/08/21 00:49:08 bruce Exp $
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
#include "thread.h"
#include "func.h"
#include "def.h"
#include "list.h"
#include "str.h"
#include "obj.h"
#include <config.h>

extern void scavenge_thread_roots(void);
extern void scavenge_symbol_roots(void);
extern void scavenge_module_roots(void);
extern void scavenge_debug_roots(void);
extern void scavenge_load_roots(void);
extern void scavenge_driver_roots(void);
extern void scavenge_brkpt_roots(void);
extern void scavenge_c_roots(void);

/* A 'DWORD' is 64 bits in a 32-bit arch and 128 bits in a 64-bit arch */
#define ALIGN_DWORD(p) (((p)+(sizeof(void *)*2)-1) & ~((sizeof(void *)*2)-1))

#ifdef was_hpux
#define PURIFY 1
#endif

boolean TimeToGC = FALSE;

struct space {
    int n_blocks;
    struct block *blocks;
    struct block *cur_block;
    void *cur_fill;
    void *cur_end;
    int bytes_in_use;
    int gc_trigger;
    struct block *scan_block;
    void *scan_ptr;
    int volatility;
    obj_t hash_state;
};

struct block {
    struct space *space;
    struct block *next;
    void *base;
    void *end;
    void *fill;
};

struct ref_list {
    obj_t **refs;
    int alloc;
    int used;
};

#if defined (__arch64__) || defined(__alpha__) || defined(__ia64__)
#define BLOCK_SIZE (256*1024)
#else
#define BLOCK_SIZE (128*1024)
#endif
#define DEFAULT_BYTES_CONSED_BETWEEN_GCS (2*1024*1024)

#if PURIFY
static boolean Purifying = FALSE;
#endif
static int BytesConsedBetweenGCs = DEFAULT_BYTES_CONSED_BETWEEN_GCS;

static struct space _Dynamic0Space
     = {0, NULL, NULL, NULL, NULL, 0, DEFAULT_BYTES_CONSED_BETWEEN_GCS,
	    NULL, NULL, 1, NULL};
#define Dynamic0Space (&_Dynamic0Space)
static struct space _Dynamic1Space 
    = {0, NULL, NULL, NULL, NULL, 0, 0, NULL, NULL, 1, NULL};
#define Dynamic1Space (&_Dynamic1Space)

#if PURIFY
static struct space _StaticSpace
    = {0, NULL, NULL, NULL, NULL, 0, 0, NULL, NULL, 0, NULL};
#define StaticSpace (&_StaticSpace)
static struct space _ReadOnlySpace
    = {0, NULL, NULL, NULL, NULL, 0, 0, NULL, NULL, 0, NULL};
#define ReadOnlySpace (&_ReadOnlySpace)
#endif

/* This is either Dynamic0Space or Dynamic1Space during normal operations */
/* and NULL during a GC or Purify */
static struct space *CurrentSpace = Dynamic0Space;

/* These are NULL during normal operations and one is Dynamic0Space and the */
/* other Dynamic1Space during a GC or Purify. */
static struct space *NewSpace = NULL;
static struct space *OldSpace = NULL;

static struct block *FreeBlocks = NULL;
#if GD_DEBUG
static struct block *OldBlocks = NULL;
#endif

static struct ref_list _ConstantRoots = {NULL, 0, 0};
#define ConstantRoots (&_ConstantRoots)
static struct ref_list _VariableRoots = {NULL, 0, 0};
#define VariableRoots (&_VariableRoots)

static struct variable *print_messages_var = NULL;


/* Reference lists. */

static void add_reference(struct ref_list *list, obj_t *ref)
{
    if (list->alloc == list->used) {
	if (list->refs == NULL) {
	    list->refs = malloc(sizeof(obj_t *) * (list->alloc = 32));
	} else {
	    list->refs = realloc(list->refs,
				 sizeof(obj_t *) * (list->alloc *= 2));
        }
    }
    list->refs[list->used++] = ref;
}

void add_constant_root(obj_t *ref)
{
    if (obj_is_ptr(*ref))
	add_reference(ConstantRoots, ref);
}

void add_variable_root(obj_t *ref)
{
    add_reference(VariableRoots, ref);
}


/* Allocation stuff. */

#if PURIFY
static struct block *object_block(obj_t obj)
{
    return (struct block *)((unsigned long)obj & ~(BLOCK_SIZE - 1));
}
#endif

static int bytes_in_use(struct space *space)
{
    if (space->cur_block)
	return (space->bytes_in_use
		+ ((char *)space->cur_fill - (char *)space->cur_block->base));
    else
	return space->bytes_in_use;
}


static struct block *alloc_block(void)
{
#ifdef hpux
#if PURIFY
    struct block *block;

    block = (struct block *)
	mmap(NULL, BLOCK_SIZE * 2, PROT_READ | PROT_WRITE,
	     MAP_ANONYMOUS | MAP_VARIABLE | MAP_PRIVATE,
	     -1, 0);
    if (block == (void *)-1)
	return NULL;
    if ((unsigned long)block & (BLOCK_SIZE-1)) {
	struct block *aligned
	    = (struct block *)(((unsigned long)block + BLOCK_SIZE)
			       & ~(BLOCK_SIZE-1));
	long before = (char *)aligned - (char *)block;
	if (munmap((caddr_t)block, before))
	    lose("munmap flamed.");
	if (munmap((caddr_t)((char *)aligned + BLOCK_SIZE),
		   BLOCK_SIZE - before))
	    lose("munmap flamed.");

	return aligned;
    }
    else {
	struct block *extra = (struct block *)((char *)block + BLOCK_SIZE);

	extra->space = NULL;
	extra->next = FreeBlocks;
	extra->base = extra;
	extra->end = (char *)extra + BLOCK_SIZE;
	FreeBlocks = extra;

	return block;
    }
#else
    struct block *block;

    block = (struct block *)
	mmap(NULL, BLOCK_SIZE, PROT_READ | PROT_WRITE,
	     MAP_ANONYMOUS | MAP_VARIABLE | MAP_PRIVATE,
	     -1, 0);
    if (block == (void *)-1)
	return NULL;
    else
	return block;
#endif
#else
    return malloc(BLOCK_SIZE);
#endif
}

static void grow_space (struct space *space)
{
    struct block *block;

    if (FreeBlocks) {
	block = FreeBlocks;
	FreeBlocks = block->next;
    }
    else {
	block = alloc_block();
	if (block == NULL)
	    lose("Heap is full!\n");
	block->base = (char *)block + ALIGN_DWORD(sizeof(struct block));
	block->end = (char *)block + BLOCK_SIZE;
    }
#if PURIFY
    block->space = space;
#endif
    block->next = NULL;

    /* Do we already have some blocks in this space? */
    if (space->blocks) {
	/* Yes, so finish off the old block and point it at the new. */
	space->bytes_in_use
	    += (char *)space->cur_fill - (char *)space->cur_block->base;
	space->cur_block->fill = space->cur_fill;
	space->cur_block->next = block;
	if (space->gc_trigger && space->bytes_in_use > space->gc_trigger)
	    TimeToGC = TRUE;
    }
    else
	/* No, so just make this this first block. */
	space->blocks = block;

    space->n_blocks++;
    space->cur_block = block;
    space->cur_fill = block->base;
    space->cur_end = block->end;
}

static void *raw_alloc(int bytes, struct space *space)
{
    void *result;

    if (bytes < 0)
	/* We don't return NULL because trying to allocate a negative number */
	/* of bytes is a sign that something real bad is trying to happen. */
	lose("Can't allocate a negative number of bytes: %d", bytes);

    /* round bytes up to the next dual-word boundy. */
    bytes = ALIGN_DWORD(bytes);

    /* check to see if the object fits in the current block */
    if ((char *)space->cur_fill + bytes > (char *)space->cur_end) {
	/* check to see if it can fit in a block. */
	if (bytes > BLOCK_SIZE - ALIGN_DWORD(sizeof(struct block))) {
	    /* it can't fit, so complain it is too large. */
	    return NULL;
	} else {
	    /* extend the space by another block. */
	    grow_space(space);
        }
    }
    result = space->cur_fill;
    space->cur_fill = (char *)result + bytes;

    return result;
}

obj_t alloc(obj_t class, int bytes)
{
#ifdef GD_DEBUG
    unsigned long *ptr;
#else
    void *ptr;
#endif
    obj_t result;

    if (class == NULL)
	lose("Tried to allocate a class that hasn't been created yet.");

#ifdef GD_DEBUG

    if (class != ptr_obj(NULL)
	  && *obj_ptr(unsigned long *, class) == COLLECTED_COOKIE)
	lose("Tried to allocate a class that wasn't scavenged.");

    ptr = raw_alloc(bytes + sizeof(unsigned long)*2, CurrentSpace);
#else
    ptr = raw_alloc(bytes, CurrentSpace);
#endif

    if (ptr == NULL)
	error("Can't allocate %d bytes for %s",
	      make_fixnum(bytes),
	      CLASS(class)->debug_name);

#ifdef GD_DEBUG
    ptr[0] = ALLOC_HEADER_COOKIE;
    ptr[1] = bytes;

    result = ptr_obj(ptr + 2);
#else
    result = ptr_obj(ptr);
#endif

    obj_ptr(struct object *, result)->class = class;

    return result;
}

void shrink(obj_t obj, int old_bytes, int new_bytes)
{
#ifdef GD_DEBUG
    unsigned long *ptr = obj_ptr(unsigned long *, obj) - 2;

    if (ptr[0] != ALLOC_HEADER_COOKIE)
        lose("Bogus pointer passed to shrink().");

    if (ptr[1] != old_bytes)
	lose("Someone lied to shrink about the old size.");

    if (new_bytes > old_bytes)
	lose("Can't shrink a %d byte object to %d bytes.", ptr[1], new_bytes);

    ptr[1] = new_bytes;
#endif
    memset(obj_ptr(char *, obj) + new_bytes, 0, ALIGN_DWORD(old_bytes) - new_bytes);
}

struct forwarding_pointer {
    obj_t marker;
    obj_t new_value;
};

void scavenge(obj_t *addr)
{
    obj_t obj = *addr;

    if (obj_is_ptr(obj)
#if PURIFY
	&& object_block(obj)->space == OldSpace
#endif
	) {
	obj_t class = obj_ptr(struct object *, obj)->class;
	if (class == ForwardingMarker) {
	    *addr = obj_ptr(struct forwarding_pointer *, obj)->new_value;
	} else {
	    ASSERT_VALID_OBJ(class);
	    *addr = obj_ptr(struct class *, class)->transport(obj);
	}
    }
}

obj_t transport(obj_t obj, int bytes, boolean read_only)
{
#ifdef GD_DEBUG
    unsigned long *new;
    unsigned long *ptr = obj_ptr(unsigned long *, obj) - 2;
#else
    void *new;
    void *ptr = obj_ptr(void *, obj);
#endif
    obj_t new_obj;
    int raw_bytes = bytes;

#ifdef GD_DEBUG
    if (ptr[0] != ALLOC_HEADER_COOKIE)
	lose("Someone called transport with a bogus object.");
    if (ptr[1] != bytes)
	lose("Someone told transport that %d byte object was %d bytes.",
	     ptr[1], bytes);

    raw_bytes += sizeof(long)*2;

#endif
#if PURIFY
    if (Purifying) {
	if (read_only) {
	    new = raw_alloc(raw_bytes, ReadOnlySpace);
	} else {
	    new = raw_alloc(raw_bytes, StaticSpace);
        }
    } else
#endif
	new = raw_alloc(raw_bytes, NewSpace);

    if (new == NULL)
	lose("raw_alloc failed during GC");

#ifdef GD_DEBUG
    new_obj = ptr_obj(new + 2);
#else
    new_obj = ptr_obj(new);
#endif

    memcpy(new, ptr, raw_bytes);

    obj_ptr(struct forwarding_pointer *, obj)->marker = ForwardingMarker;
    obj_ptr(struct forwarding_pointer *, obj)->new_value = new_obj;

    return new_obj;
}

static void scavenge_ref_list(struct ref_list *list)
{
    int i;

    for (i = 0; i < list->used; i++)
	scavenge(list->refs[i]);
}


/* This is called after all top-level root objects have been moved into the
   new heap.  This goes after the contained objects by calling the 
   class-specific scavenger on each object and having it move additional
   objects into the end of the heap.  These objects will be encountered later
   by the still-running scavenge space and processed recursively.  The net 
   effect is a breadth-first search & copy. */
static boolean scavenge_space(struct space *space)
{
    struct block *block = space->scan_block;
    void *ptr = space->scan_ptr;
    void *end;
    obj_t class;
    int bytes;

    if (block == space->cur_block && ptr >= space->cur_fill)
	return FALSE;

    while (block != NULL) {
	/* The reason for this double loop is so that we don't have to */
	/* do the block->next conditional each time around the inner loop. */
	while (ptr < (end = (block->next ? block->fill : space->cur_fill))) {
	    do {
#ifdef GD_DEBUG
		unsigned long *header = ptr;
		if (header[0] != ALLOC_HEADER_COOKIE)
		    lose("Scavenge_space found a bogus object.");
                ptr = (char *)ptr + sizeof(long)*2;
#endif
		scavenge((obj_t *)ptr);
		class = *(obj_t *)ptr;
		bytes = obj_ptr(struct class *, class)->scavenge(ptr);
#ifdef GD_DEBUG
		if (header[1] != bytes)
		    lose("Some scavenger claimed a %d byte object "
			 "was %d bytes.",
			 header[1], bytes);
#endif
		ptr = (char *)ptr + ALIGN_DWORD(bytes);
	    } while (ptr < end);
	}
	space->scan_block = block;
	space->scan_ptr = ptr;

	block = block->next;
	if (block != NULL) {
	    ptr = block->base;
	}
    }

    return TRUE;
}

void collect_garbage(boolean purify)
{
    int bytes_at_start, blocks_at_start;
    int bytes_at_end;
    boolean print_message = print_messages_var->value != obj_False;
    char strbuf[256];

#if PURIFY
    Purifying = purify;
#endif

    bytes_at_start = bytes_in_use(CurrentSpace);
    blocks_at_start = CurrentSpace->n_blocks;

    if (print_message) {
#if PURIFY
	if (purify) {
	    sprintf(strbuf, "[Purifying with %d bytes (%d blocks) in use...",
		    bytes_at_start, blocks_at_start);
	} else
#endif
	    sprintf(strbuf, "[GCing with %d bytes (%d blocks) in use...",
		    bytes_at_start, blocks_at_start);
	fputs(strbuf, stderr);
	fflush(stderr);
    }

    OldSpace = CurrentSpace;
    if (OldSpace == Dynamic0Space)
	NewSpace = Dynamic1Space;
    else
	NewSpace = Dynamic0Space;
    CurrentSpace = NULL;

    weak_pointer_gc_setup();
    if (OldSpace->hash_state != NULL) {
	invalidate_hash_state(OldSpace->hash_state);
	OldSpace->hash_state = NULL;
    }

    /* These all go through the first level of their in-mindy-heap objects
       moving them and leaving a forwarding_pointer behind. */
    scavenge_thread_roots();
    scavenge_symbol_roots();
    scavenge_module_roots();
    scavenge_debug_roots();
    scavenge_load_roots();
    scavenge_driver_roots();
    scavenge_brkpt_roots();
    scavenge_c_roots();
    scavenge_ref_list(ConstantRoots);
    scavenge_ref_list(VariableRoots);
#if PURIFY
    if (ReadOnlySpace->hash_state != NULL)
	scavenge(&ReadOnlySpace->hash_state);
    if (StaticSpace->hash_state != NULL)
	scavenge(&StaticSpace->hash_state);

    StaticSpace->scan_block = StaticSpace->blocks;
    StaticSpace->scan_ptr = StaticSpace->scan_block->base;
    if (purify) {
	if (ReadOnlySpace->scan_block == NULL) {
	    ReadOnlySpace->scan_block = ReadOnlySpace->blocks;
	    ReadOnlySpace->scan_ptr = ReadOnlySpace->scan_block->base;
	}
	ConstantRoots->used = 0;
	/* Note: we want a logical or here so that we scavenge both each */
	/* time though the loop. */
	while (scavenge_space(ReadOnlySpace) | scavenge_space(StaticSpace))
	    ;
    }
    else {
	scavenge_space(StaticSpace);
	NewSpace->scan_block = NewSpace->blocks;
	NewSpace->scan_ptr = NewSpace->scan_block->base;
	scavenge_space(NewSpace);
	if (scavenge_space(ReadOnlySpace))
	    lose("A regular GC added something to read only space?");
	if (scavenge_space(StaticSpace))
	    lose("A regular GC added something to static space?");
    }
#else
    NewSpace->scan_block = NewSpace->blocks;
    NewSpace->scan_ptr = NewSpace->scan_block->base;
    scavenge_space(NewSpace);
#endif

    break_weak_pointers();

#ifdef GD_DEBUG
    {
	struct block *block, *next;
	for (block = OldBlocks; block != NULL; block = next) {
	    next = block->next;
	    block->next = FreeBlocks;
	    FreeBlocks = block;
	}
	OldBlocks = NULL;
	for (block = OldSpace->blocks; block != NULL; block = next) {
	    unsigned long *ptr;
	    next = block->next;
	    block->next = OldBlocks;
	    OldBlocks = block;
	    for (ptr = block->base; ptr < (unsigned long *)block->end; ptr++)
		*ptr = COLLECTED_COOKIE;
	}
    }
#else
    if (OldSpace->blocks != NULL) {
	OldSpace->cur_block->next = FreeBlocks;
	FreeBlocks = OldSpace->blocks;
    }
#endif
    OldSpace->n_blocks = 0;
    OldSpace->blocks = NULL;
    OldSpace->cur_block = NULL;
    OldSpace->cur_fill = NULL;
    OldSpace->cur_end = NULL;
    OldSpace->bytes_in_use = 0;
    OldSpace->gc_trigger = 0;
    OldSpace->scan_block = NULL;
    OldSpace->scan_ptr = NULL;

    CurrentSpace = NewSpace;
    NewSpace = NULL;
    OldSpace = NULL;

    bytes_at_end = bytes_in_use(CurrentSpace);
    CurrentSpace->gc_trigger = bytes_at_end + BytesConsedBetweenGCs;
    TimeToGC = FALSE;

    if (print_message) {
#if PURIFY
	if (purify)
	    sprintf(strbuf, "finished with %d bytes (%d blocks) permanent]\n",
		    bytes_in_use(StaticSpace) + bytes_in_use(ReadOnlySpace),
		    StaticSpace->n_blocks + ReadOnlySpace->n_blocks);
	else
#endif
	    sprintf(strbuf, "reclaimed %d leaving %d (%d blocks)]\n",
		    bytes_at_start - bytes_at_end,
		    bytes_at_end, CurrentSpace->n_blocks);
	fputs(strbuf, stderr);
	fflush(stderr);
#if PURIFY
	{
	    int bytes = bytes_at_start + bytes_at_end + bytes_in_use(StaticSpace) + bytes_in_use(ReadOnlySpace);
	    int blocks = blocks_at_start + CurrentSpace->n_blocks + StaticSpace->n_blocks + ReadOnlySpace->n_blocks;
	    sprintf(strbuf,
		    "[overall heap, %d bytes, %d blocks, density %2.4f%%.]\n",
		    bytes, blocks,
		    (double)bytes * 100 / (blocks * BLOCK_SIZE));
	    fputs(strbuf, stderr);
	    fflush(stderr);
	}
#endif
    }
}


/* interface functions. */

boolean object_collected(obj_t obj)
{
#if PURIFY
    return object_block(obj)->space == OldSpace
	&& obj_ptr(struct object *, obj)->class != ForwardingMarker;
#else
    return obj_ptr(struct object *, obj)->class != ForwardingMarker;
#endif
}

obj_t pointer_hash_state(obj_t pointer)
{
    struct space *space;

#if PURIFY
    space = object_block(pointer)->space;
#else
    space = CurrentSpace;
#endif

    if (space->hash_state == NULL)
	space->hash_state = make_hash_state(space->volatility);

    return space->hash_state;
}


/* Dylan interfaces. */

static void dylan_gc(obj_t self, struct thread *thread, obj_t *args)
{
    obj_t purify = args[0];
    obj_t *old_sp = args-1;

    collect_garbage(purify != obj_False);

    thread->sp = old_sp;
    do_return(thread, old_sp, old_sp);
}

void init_gc_functions(void)
{
    obj_t namesym = symbol("*print-GC-messages*");

    define_variable(module_BuiltinStuff, namesym, var_Variable);
    print_messages_var = find_variable(module_BuiltinStuff, namesym,
				       FALSE, TRUE);
    print_messages_var->function = func_No;
    print_messages_var->type = obj_BooleanClass;
    print_messages_var->value = obj_False;

    define_constant("collect-garbage",
		    make_raw_method("collect-garbage", obj_Nil, FALSE,
				    list1(pair(symbol("purify"), obj_False)),
				    FALSE, obj_Nil, obj_False, dylan_gc));

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
		CurrentSpace->gc_trigger += bcbgcs - BytesConsedBetweenGCs;
		BytesConsedBetweenGCs = bcbgcs;
	    }
	}
    }
}
