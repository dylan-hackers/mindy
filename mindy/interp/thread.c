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
* $Header: /scm/cvs/src/mindy/interp/thread.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file implements threads, and the various synchronization
* primitives.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "bool.h"
#include "class.h"
#include "thread.h"
#include "obj.h"
#include "driver.h"
#include "func.h"
#include "num.h"
#include "list.h"
#include "def.h"
#include "type.h"
#include "error.h"

#define STACK_SIZE (1*1024*1024)

static struct thread_list *AllThreads = NULL;
static struct thread_list **AllThreadsTail = &AllThreads;
static int NextId = 0;

static struct thread *Current = NULL;
static struct thread *Runnable = NULL;

static obj_t obj_ThreadClass, obj_LockClass, obj_SpinLockClass, obj_EventClass;

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

static obj_t dylan_current_thread()
{
    return Current->thread_obj;
}

void thread_set_current(struct thread *thread)
{
    Current = thread;
}

struct thread *thread_pick_next()
{
    struct thread *thread = Runnable;

    if (thread)
	Runnable = thread->next;

    Current = thread;

    return thread;
}


/* Utilities. */

static void set_status(struct thread *thread, enum thread_status status)
{
    thread->status = status;
    THREAD(thread->thread_obj)->status = status;
}

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
	set_status(thread, status_Running);
    }
    else
	set_status(thread, status_Suspended);
}

static void return_false(struct thread *thread)
{
    obj_t *old_sp = pop_linkage(thread);

    *old_sp = obj_False;
    thread->sp = old_sp + 1;

    do_return(thread, old_sp, old_sp);
#if SLOW_LONGJMP
    go_on();
#endif
}

static void stop_thread(struct thread *thread, obj_t *vals)
{
    obj_t thread_obj = thread->thread_obj;

    assert(Current == thread);

    thread_kill(thread);

    THREAD(thread_obj)->status = status_Exited;

    mindy_pause(pause_PickNewThread);
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
    obj_t thread_obj = alloc(obj_ThreadClass, sizeof(struct thread_obj));
    struct thread_list *list = malloc(sizeof(*list));
    struct thread *thread = malloc(STACK_SIZE);

    THREAD(thread_obj)->thread = thread;
    THREAD(thread_obj)->debug_name = debug_name;

    list->thread = thread;
    list->next = NULL;
    *AllThreadsTail = list;
    AllThreadsTail = &list->next;

    memset(thread, 0, STACK_SIZE);
    thread->thread_obj = thread_obj;
    thread->id = NextId++;
    thread->next = NULL;
    thread->prev = NULL;
    thread->suspend_count = 1;
    thread->advance = start_thread;
    set_status(thread, status_Suspended);
    thread->datum = rawptr_obj(thread+1);
    thread->stack_base = (obj_t *)(thread+1);
    thread->stack_end = (obj_t *)(((char *)thread) + STACK_SIZE);
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

static obj_t dylan_spawn_thread(obj_t debug_name, obj_t func)
{
    struct thread *thread = thread_create(debug_name);

    *thread->sp++ = func;

    thread_restart(thread);

    return thread->thread_obj;
}



/* Pushing escape frames. */

static void pop_escape_frame(struct thread *thread, obj_t *vals)
{
    thread->sp = vals;
    thread_pop_escape(thread);
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

      default:
	lose("strange thread status.");
    }

    *thread->sp++ = thread->datum;
    *thread->sp++ = make_fixnum((int)thread->status);
    *thread->sp++ = rawptr_obj(thread->advance);
    *thread->sp++ = thread->component;
    *thread->sp++ = make_fixnum(thread->pc);

    thread->advance = start_thread;
    set_status(thread, status_Suspended);
    thread->suspend_count = 1;
    thread->datum = rawptr_obj(thread->sp);
    set_c_continuation(thread, pop_escape_frame);
}

void thread_pop_escape(struct thread *thread)
{
    thread->pc = fixnum_value(*--thread->sp);
    thread->component = *--thread->sp;
    thread->advance = obj_rawptr(*--thread->sp);
    set_status(thread, (enum thread_status)fixnum_value(*--thread->sp));
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
	set_status(thread, status_Running);
	break;

      default:
	lose("strange thread status.");
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

      default:
	lose("strange thread status.");
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

    THREAD(thread->thread_obj)->thread = NULL;
    THREAD(thread->thread_obj)->status = status_Killed;

    free(thread);
}

static obj_t dylan_kill_thread(obj_t thread_obj)
{
    struct thread *thread = THREAD(thread_obj)->thread;
    boolean me = (thread == Current);

    thread_kill(thread);

    if (me)
	mindy_pause(pause_PickNewThread);

    return thread_obj;
}
    


/* Thread suspending and restarting. */

void thread_debuggered(struct thread *thread, obj_t condition)
{
    assert(thread == Current);

    suspend_thread(thread);
    set_status(thread, status_Debuggered);
    thread->datum = condition;
    mindy_pause(pause_DebuggerInvoked);
}

void thread_buggered(struct thread *thread)
{
    if (thread->status != status_Debuggered)
	lose("Trying to bugger a thread that wasn't originally Debuggered?");
    else {
	wakeup_thread(thread);
	set_status(thread, status_Running);
	thread->datum = obj_False;
    }
}

void thread_suspend(struct thread *thread)
{
    if (thread->suspend_count++ == 0 && thread->status == status_Running) {
	suspend_thread(thread);
	set_status(thread, status_Suspended);
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

obj_t make_lock(void)
{
    obj_t res = alloc(obj_SpinLockClass, sizeof(struct lock));

    LOCK(res)->locked = FALSE;
    LOCK(res)->waiting = NULL;
    LOCK(res)->last = &LOCK(res)->waiting;

    return res;
}

boolean lock_query(obj_t lock)
{
    return LOCK(lock)->locked;
}

static obj_t dylan_lock_query(obj_t lock)
{
    if (lock_query(lock))
	return obj_True;
    else
	return obj_False;
}

void lock_grab(struct thread *thread, obj_t lock,
	       void (*advance)(struct thread *thread))
{
    if (LOCK(lock)->locked) {
	suspend_thread(thread);
	*LOCK(lock)->last = thread;
	LOCK(lock)->last = &thread->next;
	thread->next = NULL;
	thread->prev = NULL;
	set_status(thread, status_Blocked);
	thread->datum = lock;
	thread->advance = advance;

	mindy_pause(pause_PickNewThread);
    }
    else {
	LOCK(lock)->locked = TRUE;
	advance(thread);
    }
}

static obj_t dylan_lock_grab(obj_t lock)
{
    lock_grab(Current, lock, return_false);
    /* lock_grab doesn't return. */
    lose("lock_grab actually returned?");
    return NULL;
}

void lock_release(obj_t lock)
{
    struct thread *waiting = LOCK(lock)->waiting;

    if (waiting != NULL) {
	struct thread *next = waiting->next;

	LOCK(lock)->waiting = next;
	if (next == NULL)
	    LOCK(lock)->last = &LOCK(lock)->waiting;
	wakeup_thread(waiting);
	waiting->datum = obj_False;
    }
    else
	LOCK(lock)->locked = FALSE;
}

static obj_t dylan_lock_release(obj_t lock)
{
    if (!LOCK(lock)->locked)
	error("%= is already unlocked.", lock);

    lock_release(lock);

    return obj_False;
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
    }
}


/* Events. */

struct event {
    obj_t class;
    struct thread *waiting;
    struct thread **last;
};

#define EVENT(o) obj_ptr(struct event *, o)

obj_t make_event(void)
{
    obj_t res = alloc(obj_EventClass, sizeof(struct event));
    
    EVENT(res)->waiting = NULL;
    EVENT(res)->last = &EVENT(res)->waiting;

    return res;
}

void event_wait(struct thread *thread, obj_t event, obj_t lock,
		void (*advance)(struct thread *thread))
{
    if (lock != obj_False && !LOCK(lock)->locked)
	error("%= is already unlocked.", lock);

    suspend_thread(thread);
    *EVENT(event)->last = thread;
    EVENT(event)->last = &thread->next;
    thread->prev = NULL;
    thread->next = NULL;
    set_status(thread, status_Waiting);
    thread->datum = event;
    thread->advance = advance;

    if (lock != obj_False)
	lock_release(lock);

    mindy_pause(pause_PickNewThread);
}    

static obj_t dylan_event_wait(obj_t event, obj_t lock)
{
    event_wait(Current, event, lock, return_false);
    /* event_wait doesn't return. */
    lose("event_wait actually returned?\n");
    return NULL;
}

obj_t event_signal(obj_t event)
{
    struct thread *waiting;

    waiting = EVENT(event)->waiting;

    if (waiting != NULL) {
	struct thread *next = waiting->next;

	EVENT(event)->waiting = next;
	if (next == NULL)
	    EVENT(event)->last = &EVENT(event)->waiting;
	wakeup_thread(waiting);
	waiting->datum = obj_False;
    }

    return obj_False;
}

obj_t event_broadcast(obj_t event)
{
    struct thread *waiting;
    
    waiting = EVENT(event)->waiting;

    while (waiting != NULL) {
	struct thread *next = waiting->next;

	wakeup_thread(waiting);
	waiting->datum = obj_False;

	waiting = next;
    }
    EVENT(event)->waiting = NULL;
    EVENT(event)->last = &EVENT(event)->waiting;

    return obj_False;
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
	    return;
	}
    }
}


/* GC stuff. */

static int scav_thread_obj(struct object *o)
{
    struct thread_obj *t = (struct thread_obj *)o;

    scavenge(&t->debug_name);

    return sizeof(struct thread_obj);
}

static obj_t trans_thread_obj(obj_t t)
{
    return transport(t, sizeof(struct thread_obj), TRUE);
}

static int scav_lock(struct object *o)
{
    struct lock *lock = (struct lock *)o;

    if (lock->waiting == NULL)
	lock->last = &lock->waiting;

    return sizeof(struct lock);
}

static obj_t trans_lock(obj_t lock)
{
    return transport(lock, sizeof(struct lock), TRUE);
}

static int scav_event(struct object *o)
{
    struct event *event = (struct event *)o;

    if (event->waiting == NULL)
	event->last = &event->waiting;

    return sizeof(struct event);
}

static obj_t trans_event(obj_t event)
{
    return transport(event, sizeof(struct event), TRUE);
}

static void scav_thread(struct thread *thread)
{
    obj_t *ptr;

    scavenge(&thread->thread_obj);
    scavenge(&thread->datum);
    scavenge(&thread->component);
    scavenge(&thread->cur_catch);
    scavenge(&thread->handlers);

    for (ptr = thread->stack_base; ptr < thread->sp; ptr++)
	scavenge(ptr);
    memset(thread->sp, 0, (thread->stack_end - thread->sp) * sizeof(obj_t));
}

void scavenge_thread_roots(void)
{
    struct thread_list *list;

    for (list = AllThreads; list != NULL; list = list->next)
	scav_thread(list->thread);
}


/* Init stuff. */

void make_thread_classes(void)
{
    obj_ThreadClass = make_builtin_class(scav_thread_obj, trans_thread_obj);
    obj_LockClass = make_abstract_class(FALSE);
    obj_SpinLockClass = make_builtin_class(scav_lock, trans_lock);
    obj_EventClass = make_builtin_class(scav_event, trans_event);

    add_constant_root(&obj_ThreadClass);
    add_constant_root(&obj_LockClass);
    add_constant_root(&obj_SpinLockClass);
    add_constant_root(&obj_EventClass);
}

void init_thread_classes(void)
{
    init_builtin_class(obj_ThreadClass, "<thread>", obj_ObjectClass, NULL);
    init_builtin_class(obj_LockClass, "<lock>", obj_ObjectClass, NULL);
    init_builtin_class(obj_SpinLockClass, "<spinlock>", obj_LockClass, NULL);
    init_builtin_class(obj_EventClass, "<event>", obj_ObjectClass, NULL);
}

void init_thread_functions(void)
{
    define_function("spawn-thread", list2(obj_ObjectClass, obj_FunctionClass),
		    FALSE, obj_False, FALSE, obj_ThreadClass,
		    dylan_spawn_thread);
    define_function("current-thread", obj_Nil, FALSE, obj_False, FALSE,
		    obj_ThreadClass, dylan_current_thread);
    define_function("kill-thread", list1(obj_ThreadClass), FALSE, obj_False,
		    FALSE, obj_ThreadClass, dylan_kill_thread);

    define_method("make", list1(singleton(obj_LockClass)), FALSE, obj_Nil,
		  FALSE, obj_SpinLockClass, make_lock);
    define_method("make", list1(singleton(obj_SpinLockClass)), FALSE, obj_Nil,
		  FALSE, obj_SpinLockClass, make_lock);
    define_method("locked?", list1(obj_SpinLockClass), FALSE, obj_False,
		  FALSE, obj_BooleanClass, dylan_lock_query);
    define_method("grab-lock", list1(obj_SpinLockClass), FALSE, obj_False,
		  FALSE, obj_ObjectClass, dylan_lock_grab);
    define_method("release-lock", list1(obj_SpinLockClass), FALSE, obj_False,
		  FALSE, obj_ObjectClass, dylan_lock_release);

    define_method("make", list1(singleton(obj_EventClass)), FALSE, obj_Nil,
		  FALSE, obj_EventClass, make_event);
    define_method("wait-for-event", list2(obj_EventClass, obj_SpinLockClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass, dylan_event_wait);
    define_method("signal-event", list1(obj_EventClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass, event_signal);
    define_method("broadcast-event", list1(obj_EventClass),
		  FALSE, obj_False, FALSE, obj_ObjectClass, event_broadcast);
}
