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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/interp.h,v 1.1 1994/03/24 21:49:27 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct component {
    obj_t class;
    obj_t debug_name;
    int n_constants;
    int length;
    obj_t constant[0];
};


extern void set_byte_continuation(struct thread *thread, obj_t component);
extern void do_byte_return(struct thread *thread, obj_t *old_sp, obj_t *vals);
extern obj_t make_component(obj_t debug_name, int constants, int bytecode_len);

enum pause_reason {
    pause_NoReason, pause_NothingToRun, pause_DeadLocked, pause_Interrupted,
    pause_DebuggerInvoked, pause_HitBreakpoint, pause_DebuggerCommandFinished
};

extern enum pause_reason do_stuff(void);
extern enum pause_reason single_step(struct thread *);
extern void go_on(void);
extern void pause(enum pause_reason reason);
