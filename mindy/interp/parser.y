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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/parser.y,v 1.7 1994/06/27 16:32:29 wlott Exp $
*
* This file is the parser for the debugger.
*
\**********************************************************************/

%{
#include "mindy.h"
#include "parser.h"
#include "list.h"
#include "sym.h"
#include "bool.h"
#include "lexer.h"

static void yyerror(char *);

static obj_t result;

%}

%token tok_ERROR
%token tok_LPAREN
%token tok_RPAREN
%token tok_DEBUGVAR
%token tok_ARG
%token tok_LITERAL
%token tok_SYMBOL
%token tok_KEYWORD
%token tok_COMMA

%%

start:		exprlist			{ result = $1; }
	|	/* epsilon */			{ result = obj_Nil; }
;

exprlist:	expr				{ $$ = list1($1); }
	|	expr tok_COMMA exprlist		{ $$ = pair($1, $3); }
;

expr:		leaf				{ $$ = $1; }
	|	expr tok_LPAREN tok_RPAREN
		    { $$ = list2(symbol("funcall"), $1); }
	|	expr tok_LPAREN arglist tok_RPAREN
		    { $$ = pair(symbol("funcall"), pair($1, $3)); }
;

leaf:		tok_DEBUGVAR		{ $$ = pair(symbol("debug-var"), $1); }
	|	tok_ARG			{ $$ = pair(symbol("arg"), $1); }
	|	tok_LITERAL		{ $$ = pair(symbol("literal"), $1); }
	|	tok_KEYWORD		{ $$ = pair(symbol("literal"), $1); }
	|	tok_SYMBOL		{ $$ = pair(symbol("variable"),$1); }
;

arglist:	expr more_args			{ $$ = pair($1, $2); }
	|	tok_KEYWORD expr more_args
		    { $$ = pair(pair(symbol("literal"),$1),pair($2, $3)); }
;

more_args:	/* epsilon */			{ $$ = obj_Nil; }
	|	tok_COMMA arglist		{ $$ = $2; }
;

%%

static void yyerror(char *msg)
{
}

obj_t parse_exprs()
{
    if (yyparse())
	return obj_False;
    else
	return result;
}

