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
* $Header: /scm/cvs/src/mindy/interp/parser.y,v 1.1 1998/05/03 19:55:18 andreas Exp $
*
* This file is the parser for the debugger.
*
\**********************************************************************/

%{
#include "../compat/std-c.h"

#include "mindy.h"
#include "lexer.h"
#include "parser.h"
#include "list.h"
#include "str.h"
#include "sym.h"
#include "num.h"
#include "bool.h"


static void yyerror(char *);

static obj_t result;

%}

%token tok_TRUE
%token tok_FALSE
%token tok_ERROR
%token tok_LPAREN
%token tok_RPAREN
%token tok_DEBUGVAR
%token tok_ARG
%token tok_NUMBER
%token tok_CHARACTER
%token tok_STRING
%token tok_ADDRESS
%token tok_SYMBOL
%token tok_KEYWORD
%token tok_COMMA
%token tok_EXTERN_NAME

%%

start:
		command exprlist
		    { result = pair($1,$2); }
	|	command
		    { result = pair($1,obj_Nil); }
	|	/* epsilon */
		    { result = obj_Nil; }
	|	error
		    { result = make_byte_string("command error: try ``help''"); }
;

command:	tok_SYMBOL
;

exprlist:	expr
		    { $$ = list1($1); }
	|	expr tok_COMMA exprlist
		    { $$ = pair($1, $3); }
;

expr:		leaf
		    { $$ = $1; }
	|	expr tok_LPAREN tok_RPAREN
		    { $$ = list2(symbol("funcall"), $1); }
	|	expr tok_LPAREN arglist tok_RPAREN
		    { $$ = pair(symbol("funcall"), pair($1, $3)); }
;

leaf:		tok_DEBUGVAR
		    { $$ = pair(symbol("debug-var"), $1); }
	|	tok_ARG
		    { $$ = pair(symbol("arg"), $1); }
	|	tok_SYMBOL
		    { $$ = list2(symbol("variable"), $1); }
	|	tok_EXTERN_NAME
		    { $$ = pair(symbol("variable"), $1); }
	|	literal
		    { $$ = pair(symbol("literal"), $1); }
;

literal:	tok_TRUE
		    { $$ = obj_True; }
	|	tok_FALSE
		    { $$ = obj_False; }
	|	tok_KEYWORD
	|	tok_STRING
	|	tok_CHARACTER
	|	tok_NUMBER
	|	tok_ADDRESS
;

arglist:	expr more_args
		    { $$ = pair($1, $2); }
	|	tok_KEYWORD expr more_args
		    { $$ = pair(pair(symbol("literal"),$1),pair($2, $3)); }
;

more_args:	/* epsilon */
		    { $$ = obj_Nil; }
	|	tok_COMMA arglist
		    { $$ = $2; }
;

%%

static void yyerror(char *msg)
{
    /* don't need to do anything except abort the parse, which is
       automatically done for us 
       */
}

YYSTYPE parse_command(char *input)
{
    extern YYPARSE_RETURN_TYPE yyparse();
    yyinput_setter(input);
    return yyparse() ? obj_False : result;
}
