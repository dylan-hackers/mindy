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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/driver.h,v 1.2 1994/03/27 03:15:08 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

enum pause_reason {
    pause_NoReason, pause_PickNewThread, pause_NothingToRun, pause_Interrupted,
    pause_DebuggerInvoked, pause_HitBreakpoint, pause_DebuggerCommandFinished
};

extern enum pause_reason do_stuff(void);
extern enum pause_reason single_step(struct thread *);
extern void go_on(void);
extern void pause(enum pause_reason reason);

extern void wait_for_input(struct thread *thread, int fd,
			   void (*advance)(struct thread *thread));
extern void wait_for_output(struct thread *thread, int fd,
			    void (*advance)(struct thread *thread));

extern void set_interrupt_handler(void (*handler)(void));
extern void clear_interrupt_handler(void);
