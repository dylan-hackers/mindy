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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/driver.h,v 1.3 1994/06/27 16:31:45 wlott Exp $
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
