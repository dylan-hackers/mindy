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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/value.h,v 1.1 1994/03/24 21:49:49 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern obj_t make_value_cell(obj_t value);
extern obj_t value_cell_ref(obj_t value_cell);
extern obj_t value_cell_set(obj_t value_cell, obj_t value);
