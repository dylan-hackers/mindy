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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/fileops.h,v 1.1 1994/03/24 21:49:15 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



#define fop_FLAME 0
#define fop_COMMENT '#'
#define fop_BYTE_ORDER 1
#define fop_STORE 2
#define fop_SHORT_REF 3
#define fop_REF 4

#define fop_FALSE 5
#define fop_TRUE 6
#define fop_UNBOUND 7

#define fop_SIGNED_8 8
#define fop_SIGNED_16 9
#define fop_SIGNED_32 10
#define fop_CHAR 11
#define fop_SINGLE_FLOAT 12
#define fop_DOUBLE_FLOAT 13

#define fop_SHORT_STRING 14
#define fop_STRING 15
#define fop_SHORT_SYMBOL 16
#define fop_SYMBOL 17
#define fop_SHORT_KEYWORD 18
#define fop_KEYWORD 19

#define fop_NIL 20
#define fop_LIST1 21
#define fop_LIST2 22
#define fop_LIST3 23
#define fop_LIST4 24
#define fop_LIST5 25
#define fop_LIST6 26
#define fop_LIST7 27
#define fop_LIST8 28
#define fop_LISTN 29

#define fop_DOTTED_LIST1 30
#define fop_DOTTED_LIST2 31
#define fop_DOTTED_LIST3 32
#define fop_DOTTED_LIST4 33
#define fop_DOTTED_LIST5 34
/* Note: 35 is taken by '#' */
#define fop_DOTTED_LIST6 36
#define fop_DOTTED_LIST7 37
#define fop_DOTTED_LIST8 38
#define fop_DOTTED_LISTN 39

#define fop_VECTOR0 40
#define fop_VECTOR1 41
#define fop_VECTOR2 42
#define fop_VECTOR3 43
#define fop_VECTOR4 44
#define fop_VECTOR5 45
#define fop_VECTOR6 46
#define fop_VECTOR7 47
#define fop_VECTOR8 48
#define fop_VECTORN 49

#define fop_VALUE_CELL 50
#define fop_WRITABLE_VALUE_CELL 51
#define fop_BUILTIN_VALUE_CELL 52
#define fop_BUILTIN_WRITABLE_VALUE_CELL 53

#define fop_SHORT_COMPONENT 55
#define fop_COMPONENT 56
#define fop_SHORT_METHOD 57
#define fop_METHOD 58

#define fop_IN_LIBRARY 60
#define fop_IN_MODULE 61
#define fop_TOP_LEVEL_FORM 62
#define fop_DEFINE_CONSTANT 63
#define fop_DEFINE_VARIABLE 64
#define fop_DEFINE_GENERIC 65
#define fop_DEFINE_METHOD 66
#define fop_DEFINE_CLASS 67
#define fop_DEFINE_LIBRARY 68
#define fop_DEFINE_MODULE 69

#define fop_DONE 255
