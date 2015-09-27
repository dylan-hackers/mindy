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
\**********************************************************************/


enum thread_status {
    status_Running,
    status_Suspended,
    status_Debuggered,
    status_Blocked,
    status_Waiting,
    status_Exited,
    status_Killed
};

struct thread_list {
    struct thread *thread;
    struct thread_list *next;
};

struct thread_obj {
    obj_t class;
    struct thread *thread;
    obj_t debug_name;
    enum thread_status status;
};

#define THREAD(o) obj_ptr(struct thread_obj *, o)

#define C_CONTINUATION_MARKER rawptr_obj(0xf00)
#define obj_is_saved_c_function(o) \
  (((unsigned long)(o) & 0xffff) == (unsigned long)C_CONTINUATION_MARKER)
#define save_c_function_hi(c) \
  rawptr_obj((unsigned long)(c) & ~1)
#define save_c_function_lo(c) \
  rawptr_obj((((unsigned long)(c) & 1) << 16) | (unsigned long)C_CONTINUATION_MARKER)
#define restore_c_function(lo, hi) \
  ((void (*)(struct thread *thread, obj_t *vals))((unsigned long)hi | ((unsigned long)(lo) >> 16)))

struct thread {
    int id;
    obj_t thread_obj;
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
    void (*c_continuation)(struct thread *thread, obj_t *vals);
};

extern struct thread_list *all_threads(void);
extern struct thread *thread_current(void);
extern void thread_set_current(struct thread *thread);
extern struct thread *thread_pick_next(void);
extern struct thread *thread_make(obj_t debug_name);
extern void thread_push_escape(struct thread *thread);
extern void thread_pop_escape(struct thread *thread);
extern void thread_kill(struct thread *thread);
extern void thread_debuggered(struct thread *thread, obj_t condition);
extern void thread_buggered(struct thread *thread);
extern void thread_stop(struct thread *thread);
extern void thread_restart(struct thread *thread);

extern obj_t make_lock(void);
extern boolean lock_query(obj_t lock);
extern void lock_grab(struct thread *thread, obj_t lock,
		      void (*advance)(struct thread *thread));
extern void lock_release(obj_t lock);

extern obj_t make_event(void);
extern void event_wait(struct thread *thread, obj_t event, obj_t lock,
		       void (*advance)(struct thread *thread));
extern obj_t event_signal(obj_t event);
extern obj_t event_broadcast(obj_t event);
