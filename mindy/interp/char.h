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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/char.h,v 1.4 1994/10/05 21:01:21 nkramer Exp $
*
\**********************************************************************/


extern obj_t obj_CharacterClass;
extern obj_t obj_ByteCharacterClass;

struct character {
    obj_t class;
    unsigned char high_byte, low_byte;
};

extern obj_t int_char(int c);

#define char_int(o) (((obj_ptr(struct character *, o)->high_byte) << 8) \
                     + (obj_ptr(struct character *, o)->low_byte))
