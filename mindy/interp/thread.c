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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/thread.c,v 1.1 1994/03/24 21:49:41 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include "mindy.h"
#include "gc.h"
#include "bool.h"
#include "class.h"
#include "thread.h"
#include "obj.h"
#include "interp.h"
#include "func.h"
#include "num.h"

#define STACK_SIZE (1*1024*1024)

static struct thread_list *AllThreads = NULL;
static struct thread_list **AllThreadsTail = &AllThreads;
static int NextId = 0;

static struct thread *Current = NULL;
static struct thread *Runnable = NULL;

static obj_t obj_LockClass, obj_EventClass;

static void remove_from_lock(struct thread *thread);
static void add_to_lock(struct thread *thread);
static void remove_from_event(struct thread *thread);



struct thread_list *all_threads(void)
{
    return AllThreads;
}



/* Scheduling stuff. */

struct thread *thread_current()
{
    return Current;
}

void thread_set_current(struct thread *thread)
{
    Current = thread;
}

struct thread *thread_pick_next()
{
    if ((Current = Runnable) != NULL) {
	Runnable = Runnable->next;
	return Current;
    }
    else if (AllThreads != NULL)
	pause(pause_DeadLocked);
    else
	pause(pause_NothingToRun);
}


/* Utilities. */

static void suspend_thread(struct thread *thread)
{
    if (thread == Runnable) {
	if (thread == thread->next)
	    Runnable = NULL;
	else {
	    Runnable = thread->next;
	    *thread->prev = Runnable;
	    Runnable->prev = thread->prev;
	}
    }
    else {
	*thread->prev = thread->next;
	thread->next->prev = thread->prev;
    }

    thread->next = NULL;
    thread->prev = NULL;
}

static void wakeup_thread(struct thread *thread)
{
    if (thread->suspend_count == 0) {
	if (Runnable) {
	    thread->next = Runnable;
	    thread->prev = Runnable->prev;
	    *thread->prev = thread;
	    Runnable->prev = &thread->next;
	}
	else {
	    thread->next = thread;
	    thread->prev = &thread->next;
	    Runnable = thread;
	}
	thread->status = status_Running;
    }
    else
	thread->status = status_Suspended;
}

static void return_false(struct thread *thread)
{
    obj_t *old_sp = thread->sp++;
    *old_sp = obj_False;
    do_return(thread, old_sp, old_sp);
}

static void flame(struct thread *thread)
{
    lose("Exited thread attempted to keep running.\n");
}

static void stop_thread(struct thread *thread, obj_t *vals)
{
    assert(Current == thread);

    thread_kill(thread);

    go_on();
}

static void start_thread(struct thread *thread)
{
    obj_t *old_sp = obj_rawptr(thread->datum);

    thread->advance = NULL;
    thread->datum = obj_False;

    invoke(thread, (thread->sp - old_sp) - 1);
}


/* Thread creation. */

struct thread *thread_create(obj_t debug_name)
{
    struct thread_list *list = malloc(sizeof(*list));
    struct thread *thread = malloc(STACK_SIZE);
    int i, j;

    list->thread = thread;
    list->next = NULL;
    *AllThreadsTail = list;
    AllThreadsTail = &list->next;

    thread->debug_name = debug_name;
    thread->id = NextId++;
    thread->next = NULL;
    thread->prev = NULL;
    thread->suspend_count = 1;
    thread->advance = start_thread;
    thread->status = status_Suspended;
    thread->datum = rawptr_obj(thread+1);
    thread->stack_base = (obj_t *)(thread+1);
    thread->stack_end = (obj_t *)(((void *)thread) + STACK_SIZE);
    thread->sp = thread->stack_base;
    thread->fp = NULL;
    thread->component = 0;
    thread->pc = 0;
    thread->cur_catch = obj_False;
    thread->cur_uwp = NULL;
    thread->handlers = obj_False;

    set_c_continuation(thread, stop_thread);

    return thread;
}


/* Pushing escape frames. */

static void pop_escape_frame(struct thread *thread, obj_t *vals)
{
    thread->sp = vals;
    thread_pop_escape(thread);
    go_on();
}

void thread_push_escape(struct thread *thread)
{
    switch (thread->status) {
      case status_Running:
	suspend_thread(thread);
	break;

      case status_Suspended:
	*thread->sp++ = make_fixnum(thread->suspend_count);
	break;

      case status_Debuggered:
	break;

      case status_Blocked:
	remove_from_lock(thread);
	break;

      case status_Waiting:
	remove_from_event(thread);
	break;
    }

    *thread->sp++ = thread->datum;
    *thread->sp++ = make_fixnum((int)thread->status);
    *thread->sp++ = rawptr_obj(thread->advance);

    thread->advance = start_thread;
    thread->status = status_Suspended;
    thread->suspend_count = 1;
    thread->datum = rawptr_obj(thread->sp);
    set_c_continuation(thread, pop_escape_frame);
}

void thread_pop_escape(struct thread *thread)
{
    thread->advance = obj_rawptr(*--thread->sp);
    thread->status = (enum status)fixnum_value(*--thread->sp);
    thread->datum = *--thread->sp;
    
    switch (thread->status) {
      case status_Running:
	break;

      case status_Suspended:
	suspend_thread(thread);
	thread->suspend_count = fixnum_value(*--thread->sp);
	break;

      case status_Debuggered:
	suspend_thread(thread);
	break;

      case status_Blocked:
	add_to_lock(thread);
	break;

      case status_Waiting:
	thread->status = status_Running;
	break;
    }
}


/* Thread destruction */

void thread_kill(struct thread *thread)
{
    struct thread_list *list, **prev;

    switch (thread->status) {
      case status_Running:
	suspend_thread(thread);
	break;
      case status_Suspended:
      case status_Debuggered:
	break;
      case status_Blocked:
	remove_from_lock(thread);
	break;
      case status_Waiting:
	remove_from_event(thread);
	break;
    }

    for (prev = &AllThreads; (list = *prev) != NULL; prev = &list->next) {
	if (list->thread == thread) {
	    struct thread_list *next = list->next;
	    *prev = next;
	    if (next == NULL)
		AllThreadsTail = prev;
	    free(list);
	    break;
	}
    }
    assert(list != NULL);

    if (Current == thread)
	Current = NULL;

    free(thread);
}


/* Thread suspending and restarting. */

void thread_debuggered(struct thread *thread, obj_t condition)
{
    assert(thread == Current);

    suspend_thread(thread);
    thread->status = status_Debuggered;
    thread->datum = condition;
    pause(pause_DebuggerInvoked);
}

void thread_buggered(struct thread *thread)
{
    if (thread->status != status_Debuggered)
	lose("Trying to bugger a thread that wasn't originally Debuggered?");
    else {
	wakeup_thread(thread);
	thread->status = status_Running;
	thread->datum = obj_False;
    }
}

void thread_suspend(struct thread *thread)
{
    if (thread->suspend_count++ == 0 && thread->status == status_Running) {
	suspend_thread(thread);
	thread->status = status_Suspended;
    }
}

void thread_restart(struct thread *thread)
{
    if (thread->suspend_count > 0) {
	thread->suspend_count--;
	if (thread->suspend_count == 0 && thread->status == status_Suspended)
	    wakeup_thread(thread);
    }
}


/* Locks */

struct lock {
    obj_t class;
    boolean locked;
    struct thread *waiting;
    struct thread **last;
};

#define LOCK(o) obj_ptr(struct lock *, o)

static void dylan_make_lock(struct thread *thread, int nargs)
{
    obj_t res = alloc(obj_LockClass, sizeof(struct lock));
    obj_t *old_sp = thread->sp - 1;

    assert(nargs == 0);

    LOCK(res)->locked = FALSE;
    LOCK(res)->waiting = NULL;
    LOCK(res)->last = &LOCK(res)->waiting;

    *old_sp = res;

    do_return(thread, old_sp, old_sp);
}

static void dylan_query_lock(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t lock = args[0];
    obj_t *old_sp = args-1;

    assert(nargs == 1);

    if (LOCK(lock)->locked)
	*old_sp = obj_True;
    else
	*old_sp = obj_False;

    thread->sp = old_sp + 1;

    do_return(thread, old_sp, old_sp);
}

static void dylan_grab_lock(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t lock = args[0];

    assert(nargs == 1);

    if (LOCK(lock)->locked) {
	push_linkage(thread, args);
	suspend_thread(thread);
	*LOCK(lock)->last = thread;
	LOCK(lock)->last = &thread->next;
	thread->next = NULL;
	thread->prev = NULL;
	thread->status = status_Blocked;
	thread->datum = lock;

	go_on();
    }
    else {
	LOCK(lock)->locked = TRUE;
	thread->sp = args - 1;
	return_false(thread);
    }
}

static void release_lock(obj_t lock)
{
    struct thread *waiting = LOCK(lock)->waiting;

    if (waiting != NULL) {
	struct thread *next = waiting->next;

	LOCK(lock)->waiting = next;
	if (next == NULL)
	    LOCK(lock)->last = &LOCK(lock)->waiting;
	wakeup_thread(waiting);
	waiting->datum = obj_False;
	waiting->sp = pop_linkage(waiting);
	waiting->advance = return_false;
    }
    else
	LOCK(lock)->locked = FALSE;
}

static void dylan_release_lock(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t lock = args[0];

    assert(nargs == 1);

    if (!LOCK(lock)->locked) {
	push_linkage(thread, args);
	error("~S is already unlocked.", lock);
    }
    release_lock(lock);
    thread->sp = args - 1;
    return_false(thread);
}

static void remove_from_lock(struct thread *thread)
{
    obj_t lock = thread->datum;
    struct thread *scan, **prev;

    prev = &LOCK(lock)->waiting;
    while (1) {
	scan = *prev;
	if (scan == NULL)
	    lose("Tried to remove a thread from an lock it "
		 "wasn't waiting on.");
	if (scan == thread) {
	    *prev = thread->next;
	    if (thread->next == NULL)
		LOCK(lock)->last = prev;
	    return;
	}
    }
}

static void add_to_lock(struct thread *thread)
{
    obj_t lock = thread->datum;

    if (LOCK(lock)->locked) {
	suspend_thread(thread);
	*LOCK(lock)->last = thread;
	LOCK(lock)->last = &thread->next;
	thread->next = NULL;
	thread->prev = NULL;
    }
    else {
	LOCK(lock)->locked = TRUE;

	thread->datum = obj_False;
	thread->sp = pop_linkage(thread);
	thread->advance = return_false;
    }
}


/* Events. */

struct event {
    obj_t class;
    struct thread *waiting;
    struct thread **last;
};

#define EVENT(o) obj_ptr(struct event *, o)

static void dylan_make_event(struct thread *thread, int nargs)
{
    obj_t res = alloc(obj_EventClass, sizeof(struct event));
    obj_t *old_sp = thread->sp - 1;

    assert(nargs == 0);

    EVENT(res)->waiting = NULL;
    EVENT(res)->last = &EVENT(res)->waiting;

    *old_sp = res;

    do_return(thread, old_sp, old_sp);
}

static void dylan_event_wait(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 2;
    obj_t event = args[0];
    obj_t lock = args[1];

    assert(nargs == 2);

    push_linkage(thread, args);

    if (!LOCK(lock)->locked)
	error("~S is already unlocked.", lock);

    suspend_thread(thread);
    *EVENT(event)->last = thread;
    EVENT(event)->last = &thread->next;
    thread->prev = NULL;
    thread->next = NULL;
    thread->status = status_Waiting;
    thread->datum = event;

    release_lock(lock);

    go_on();
}

static void dylan_event_signal(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t event = args[0];
    struct thread *waiting;

    assert(nargs == 1);

    waiting = EVENT(event)->waiting;

    if (waiting != NULL) {
	struct thread *next = waiting->next;

	EVENT(event)->waiting = next;
	if (next == NULL)
	    EVENT(event)->last = &EVENT(event)->waiting;
	wakeup_thread(waiting);
	waiting->datum = obj_False;
	waiting->sp = pop_linkage(waiting);
	waiting->advance = return_false;
    }

    thread->sp = args - 1;
    return_false(thread);
}
    
static void dylan_event_broadcast(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t event = args[0];
    struct thread *waiting;

    assert(nargs == 1);

    waiting = EVENT(event)->waiting;

    while (waiting != NULL) {
	struct thread *next = waiting->next;

	wakeup_thread(waiting);
	waiting->datum = obj_False;
	waiting->sp = pop_linkage(waiting);
	waiting->advance = return_false;

	waiting = next;
    }
    EVENT(event)->waiting = NULL;
    EVENT(event)->last = &EVENT(event)->waiting;

    thread->sp = args - 1;
    return_false(thread);
}

static void remove_from_event(struct thread *thread)
{
    obj_t event = thread->datum;
    struct thread *scan, **prev;

    prev = &EVENT(event)->waiting;
    while (1) {
	scan = *prev;
	if (scan == NULL)
	    lose("Tried to remove a thread from an event it "
		 "wasn't waiting on.");
	if (scan == thread) {
	    *prev = thread->next;
	    if (thread->next == NULL)
		EVENT(event)->last = prev;
	    thread->datum = obj_False;
	    thread->sp = pop_linkage(thread);
	    thread->advance = return_false;
	    return;
	}
    }
}


/* GC stuff. */

static int scav_lock(struct object *o)
{
    return sizeof(struct lock);
}

static obj_t trans_lock(obj_t lock)
{
    return transport(lock, sizeof(struct lock));
}

static int scav_event(struct object *o)
{
    return sizeof(struct event);
}

static obj_t trans_event(obj_t event)
{
    return transport(event, sizeof(struct event));
}

static scav_thread(struct thread *thread)
{
    obj_t *ptr;

    scavenge(&thread->datum);
    scavenge(&thread->component);
    scavenge(&thread->cur_catch);

    for (ptr = thread->stack_base; ptr < thread->sp; ptr++)
	scavenge(ptr);
}

void scavenge_thread_roots(void)
{
    struct thread_list *list;

    for (list = AllThreads; list != NULL; list = list->next)
	scav_thread(list->thread);

    scavenge(&obj_LockClass);
    scavenge(&obj_EventClass);
}


/* Init stuff. */

void make_thread_classes(void)
{
    obj_LockClass = make_builtin_class(scav_lock, trans_lock);
    obj_EventClass = make_builtin_class(scav_event, trans_event);
}

void init_thread_classes(void)
{
    init_builtin_class(obj_LockClass, "<lock>", obj_ObjectClass, NULL);
    init_builtin_class(obj_EventClass, "<event>", obj_ObjectClass, NULL);
}
