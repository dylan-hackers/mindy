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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/lexer.h,v 1.1 1994/03/24 21:48:56 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


extern int yylex(void);
extern FILE *yyin;

struct token {
    int length;
    int line;
    unsigned char chars[0];
};

extern int line_count;
