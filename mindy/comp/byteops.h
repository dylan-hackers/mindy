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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/byteops.h,v 1.3 1994/04/14 19:13:51 wlott Exp $
*
* This file does whatever.
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
