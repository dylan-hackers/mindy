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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/envanal.h,v 1.1 1994/03/24 21:49:15 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct closes_over {
    struct binding *binding;
    int offset;
    struct closes_over *next;
    struct closes_over *over;
};

extern void environment_analysis(struct body *program);
