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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/Attic/lexer.l,v 1.8 1994/06/27 16:32:07 wlott Exp $
*
* This file is the lexer for the debugger.
*
\**********************************************************************/


%{
#include <stdlib.h>
#include <string.h>

#include "mindy.h"
#include "str.h"
#include "sym.h"
#include "num.h"
#include "bool.h"
#include "lexer.h"
#include "parser.h"

#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) result = yy_input(buf, max_size)

static int yy_input(char *buf, int max_size);

static obj_t token_as_address(void);
static obj_t token_as_string(void);
static obj_t token_as_symbol(void);
static obj_t token_as_keyword(void);

%}

DIGIT		[0-9]
HEXDIGIT	[0-9a-fA-F]
IDENTIFIER	[-!$%&*+/0-9<=>?@A-Z^_a-z~]+

%%

\(			return tok_LPAREN;
\)			return tok_RPAREN;
"#"[tT]			yylval = obj_True; return tok_LITERAL;
"#"[fF]			yylval = obj_False; return tok_LITERAL;
"$"			yylval = make_fixnum(-1); return tok_DEBUGVAR;
"$$"			yylval = make_fixnum(-2); return tok_DEBUGVAR;
"$"-?{DIGIT}+	yylval = make_fixnum(atoi(yytext+1)); return tok_DEBUGVAR;
"$a"{DIGIT}+	yylval = make_fixnum(atoi(yytext+2)); return tok_ARG;
0x{HEXDIGIT}+		yylval = token_as_address(); return tok_LITERAL;
-?{DIGIT}+	yylval = make_fixnum(atoi(yytext)); return tok_LITERAL;
\"[^"\\]*(\\.[^"\\])*\"	yylval = token_as_string(); return tok_LITERAL;
{IDENTIFIER}		yylval = token_as_symbol(); return tok_SYMBOL;
{IDENTIFIER}:		yylval = token_as_keyword(); return tok_KEYWORD;
,			return tok_COMMA;
[ \t\n]+		;
.			return tok_ERROR;

%%

static char *readptr;
static int remaining;

void lex_init(void)
{
    yy_current_buffer = yy_create_buffer( yyin, YY_BUF_SIZE );
}

void lex_setup(char *input, int len)
{
    readptr = input;
    remaining = len;
    yyrestart(NULL);
}

static int yy_input(char *buf, int max_size)
{
    if (remaining == 0)
	return YY_NULL;
    if (remaining <= max_size) {
	int count = remaining;
	memcpy(buf, readptr, remaining);
	readptr = NULL;
	remaining = 0;
	return count;
    }
    else {
	memcpy(buf, readptr, max_size);
	readptr += max_size;
	remaining -= max_size;
	return max_size;
    }
}

static obj_t token_as_address(void)
{
    unsigned long addr = 0;
    int i;

    for (i = 2; i < yyleng; i++) {
	int c = yytext[i];
	if ('0' <= c && c <= '9')
	    addr = (addr<<4) + (c-'0');
	else if ('a' <= c && c <= 'f')
	    addr = (addr<<4) + 10 + (c-'a');
	else if ('A' <= c && c <= 'F')
	    addr = (addr<<4) + 10 + (c-'A');
	else
	    abort();
    }

    return (obj_t)addr;
}

static obj_t token_as_string(void)
{
    int len = yyleng - 2;
    int i;
    char *ptr;
    obj_t str;

    for (i = 1; i < yyleng-1; i++)
	if (yytext[i] == '\\') {
	    len--;
	    i++;
	}

    str = alloc_string(len);
    ptr = string_chars(str);

    for (i = 1; i < yyleng-1; i++)
	if (yytext[i] == '\\') {
	    i++;
	    switch (yytext[i]) {
	      case 'n':
		*ptr++ = '\n';
		break;
	      case '"':
		*ptr++ = '\"';
		break;
	      case 't':
		*ptr++ = '\t';
		break;
	      case 'b':
		*ptr++ = '\b';
		break;
	      case 'r':
		*ptr++ = '\r';
		break;
	      default:
		*ptr++ = yytext[i];
		break;
	    }
	}
	else
	    *ptr++ = yytext[i];
    *ptr = '\0';

    return str;
}

static obj_t token_as_symbol(void)
{
    char buffer[256];

    memcpy(buffer, yytext, yyleng);
    buffer[yyleng] = '\0';

    return symbol(buffer);
}

static obj_t token_as_keyword(void)
{
    char buffer[256];

    memcpy(buffer, yytext, yyleng-1);
    buffer[yyleng-1] = '\0';

    return symbol(buffer);
}
