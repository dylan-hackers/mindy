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
* $Header: /scm/cvs/src/mindy/interp/def.h,v 1.1 1998/05/03 19:55:12 andreas Exp $
*
\**********************************************************************/


extern void define(char *name, obj_t value);
extern void define_constant(char *name, obj_t value);
extern void define_function(char *name, obj_t specializers, boolean restp,
			    obj_t keys, boolean all_keys, obj_t result_type,
			    obj_t (*func)());
extern void define_generic_function(char *name, obj_t specializers, 
				    boolean restp,
				    obj_t keys, boolean all_keys,
				    obj_t res_types, obj_t more_results_type);
extern void define_method(char *name, obj_t specializers, boolean restp,
			  obj_t keywords, boolean all_keys, obj_t result_type,
			  obj_t (*func)());
extern void define_class(char *name, obj_t class);

