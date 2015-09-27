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
***********************************************************************
*
* This file defines the byte opcodes.
*
\**********************************************************************/

#define op_TRAP 0
#define op_BREAKPOINT 1
#define op_RETURN_SINGLE 2
#define op_MAKE_VALUE_CELL 3
#define op_VALUE_CELL_REF 4
#define op_VALUE_CELL_SET 5
#define op_MAKE_METHOD 6
#define op_CHECK_TYPE 7
#define op_CHECK_TYPE_FUNCTION 8
#define op_CANONICALIZE_VALUE 9
#define op_PUSH_BYTE 10
#define op_PUSH_INT 11
#define op_CONDITIONAL_BRANCH 12
#define op_BRANCH 13
#define op_PUSH_NIL 14
#define op_PUSH_UNBOUND 15
#define op_PUSH_TRUE 16
#define op_PUSH_FALSE 17
#define op_DUP 18
#define op_DOT_TAIL 19
#define op_DOT_FOR_MANY 20
#define op_DOT_FOR_SINGLE 21

#define op_PUSH_CONSTANT 0x20
#define op_PUSH_ARG 0x30
#define op_POP_ARG 0x40
#define op_PUSH_LOCAL 0x50
#define op_POP_LOCAL 0x60
#define op_CALL_TAIL 0x70
#define op_CALL_FOR_MANY 0x80
#define op_CALL_FOR_SINGLE 0x90
#define op_PUSH_VALUE 0xa0
#define op_PUSH_FUNCTION 0xb0
#define op_POP_VALUE 0xc0

#define op_PLUS 0xf0
#define op_MINUS 0xf1
#define op_LT 0xf2
#define op_LE 0xf3
#define op_EQ 0xf4
#define op_IDP 0xf5
#define op_NE 0xf6
#define op_GE 0xf7
#define op_GT 0xf8
