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
* $Header: /scm/cvs/src/mindy/interp/vec.h,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
\**********************************************************************/


extern obj_t obj_SimpleVectorClass;
extern obj_t obj_SimpleObjectVectorClass;
extern obj_t obj_ByteVectorClass;

struct sovec {
    obj_t class;
    int length;
    obj_t contents[1];
};

struct bytevec {
    obj_t class;
    int length;
    unsigned char contents[1];
};

#define SOVEC(o) obj_ptr(struct sovec *, o)
#define BYTEVEC(o) obj_ptr(struct bytevec *, o)

#define vector_data(vec) (obj_ptr(struct bytevec *, vec)->contents)

extern obj_t make_vector(int length, obj_t *contents);
extern obj_t make_byte_vector(int length, unsigned char *contents);
