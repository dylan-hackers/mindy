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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/gc.h,v 1.2 1994/04/09 13:35:53 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t alloc(obj_t class, int bytes);
extern void scavenge(obj_t *addr);
extern obj_t transport(obj_t obj, int bytes);

extern void collect_garbage(void);

extern boolean TimeToGC;

#define ForwardingMarker ((obj_t)(0xDEADBEEF))
