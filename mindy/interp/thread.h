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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/thread.h,v 1.2 1994/03/27 02:07:24 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


enum thread_status {
    status_Running,
    status_Suspended,
    status_Debuggered,
    status_Blocked,
    status_Waiting
};

struct thread_list {
    struct thread *thread;
    struct thread_list *next;
};

struct thread {
    int id;
    obj_t debug_name;
    struct thread *next, **prev;
    void (*advance)(struct thread *thread);
    enum thread_status status;
    int suspend_count;
    obj_t datum;
    obj_t *stack_base, *stack_end;
    obj_t *sp, *fp;
    obj_t component;
    unsigned pc;
    obj_t cur_catch;
    struct uwp *cur_uwp;
    obj_t handlers;
};

extern struct thread_list *all_threads(void);
extern struct thread *thread_current(void);
extern void thread_set_current(struct thread *thread);
extern struct thread *thread_pick_next(void);
extern struct thread *thread_create(obj_t debug_name);
extern void thread_push_escape(struct thread *thread);
extern void thread_pop_escape(struct thread *thread);
extern void thread_kill(struct thread *thread);
extern void thread_debuggered(struct thread *thread, obj_t condition);
extern void thread_buggered(struct thread *thread);
extern void thread_suspend(struct thread *thread);
extern void thread_restart(struct thread *thread);

extern obj_t make_lock(void);
extern boolean lock_query(obj_t lock);
extern void lock_grab(struct thread *thread, obj_t lock,
		      void advance(struct thread *thread));
extern void lock_release(obj_t lock);

extern obj_t make_event(void);
extern void event_wait(struct thread *thread, obj_t event, obj_t lock,
		       void (*advance)(struct thread *thread));
extern obj_t event_signal(obj_t event);
extern obj_t event_broadcast(obj_t event);
