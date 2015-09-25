%{
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
* $Header: /scm/cvs/src/mindy/comp/lexer.l,v 1.9 2003/04/17 17:08:12 brent Exp $
*
* This file is the lexical analizer.
*
\**********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "src.h"
#include "parser-tab.h"


extern int isatty();
extern void warn();

int line_count = 1;

#define is(type) return (yylval.token = make_token(yytext, yyleng)), type;

static void skip_multi_line_comment(void);
static int make_header_key(void);
static int make_header_val(void);
static int make_header_end(void);
  
#define YY_DECL int internal_yylex(void)

%}

%x ini key val etc

D	[0-9]
E	[esdx][-+]?{D}+

A	[a-zA-Z]
G	[!&*<=>|^$%@_]
S	[-+~?/]

DGS	({D}|{G}|{S})
ADGS	({A}|{DGS})

N	((({G}{DGS}*)?{A}{ADGS}*)|({D}{DGS}*({A}{DGS}+)*{A}{A}{ADGS}*))

O	(":="|"+"|"-"|"*"|"/"|"="|"=="|"<"|">"|"<="|">="|"~="|"~=="|"&"|"|"|"^")

STR	\"(([ !#-\[\]-~])|(\\["'\\abefnrt0<]))*\"

EOL	"\n"|"\r"|"\r\n"|"\n\r"
%%

<INITIAL>.			BEGIN(ini); yyless(0);

<ini>#!.*{EOL}			BEGIN(key); warn(line_count, "ignoring initial #! interpreter comment\n"); line_count++; 

<ini,key>[a-zA-Z][-a-zA-Z0-9]*:	BEGIN(val); return make_header_key();
<ini,key>[ \t]*{EOL}		BEGIN(etc); return make_header_end();

<val>.*{EOL}([ \t]+.+{EOL})*	BEGIN(key); return make_header_val();

<etc>[ \t\f]+			;
<etc>{EOL}			line_count++;

<etc>"//".*			;
<etc>"/*"			skip_multi_line_comment();

<etc>abstract			is(ABSTRACT);
<etc>above			is(ABOVE);
<etc>begin			is(DBEGIN);
<etc>below			is(BELOW);
<etc>block			is(BLOCK);
<etc>by				is(BY);
<etc>case			is(CASE);
<etc>class			is(CLASS);
<etc>cleanup			is(CLEANUP);
<etc>concrete			is(CONCRETE);
<etc>constant			is(CONSTANT);
<etc>define			is(DEFINE);
<etc>domain			is(DOMAIN);
<etc>each-subclass		is(EACH_SUBCLASS);
<etc>else			is(ELSE);
<etc>elseif			is(ELSEIF);
<etc>end			is(END);
<etc>exception			is(EXCEPTION);
<etc>finally			is(FINALLY);
<etc>for			is(FOR);
<etc>free			is(FREE);
<etc>from			is(FROM);
<etc>function			is(FUNCTION);
<etc>generic			is(GENERIC);
<etc>handler			is(HANDLER);
<etc>if				is(IF);
<etc>in				is(IN);
<etc>inherited			is(INHERITED);
<etc>inline			is(INLINE);
<etc>instance			is(INSTANCE);
<etc>keyed-by			is(KEYED_BY);
<etc>keyword			is(KEYWORD_RESERVED_WORD);
<etc>let			is(LET);
<etc>local			is(LOCAL);
<etc>method			is(METHOD);
<etc>open			is(OPEN);
<etc>otherwise			is(OTHERWISE);
<etc>primary			is(PRIMARY);
<etc>required			is(REQUIRED);
<etc>sealed			is(SEALED);
<etc>select			is(SELECT);
<etc>slot			is(SLOT);
<etc>then			is(THEN);
<etc>to				is(TO);
<etc>unless			is(UNLESS);
<etc>until			is(UNTIL);
<etc>using			is(USING);
<etc>variable			is(VARIABLE);
<etc>virtual			is(VIRTUAL);
<etc>while			is(WHILE);

<etc>module			is(MODULE);
<etc>library			is(LIBRARY);
<etc>export			is(EXPORT);
<etc>create			is(CREATE);
<etc>use			is(USE);
<etc>all			is(ALL);

<etc>"prefix:"			is(PREFIX_KEYWORD);
<etc>"import:"			is(IMPORT_KEYWORD);
<etc>"exclude:"			is(EXCLUDE_KEYWORD);
<etc>"export:"			is(EXPORT_KEYWORD);
<etc>"rename:"			is(RENAME_KEYWORD);
<etc>"while:"			is(WHILE_KEYWORD);
<etc>"until:"			is(UNTIL_KEYWORD);

<etc>"("			is(LPAREN);
<etc>")"			is(RPAREN);
<etc>","			is(COMMA);
<etc>"."			is(DOT);
<etc>";"			is(SEMI);
<etc>"["			is(LBRACKET);
<etc>"]"			is(RBRACKET);
<etc>"{"			is(LBRACE);
<etc>"}"			is(RBRACE);
<etc>"::"			is(COLON_COLON);
<etc>"-"			is(MINUS);
<etc>"~"			is(TILDE);
<etc>"="			is(EQUAL);
<etc>"=="			is(EQUAL_EQUAL);
<etc>"=>"			is(ARROW);
<etc>"#("			is(SHARP_PAREN);
<etc>"#["			is(SHARP_BRACKET);
<etc>"#t"			is(SHARP_T);
<etc>"#f"			is(SHARP_F);
<etc>"#next"			is(NEXT);
<etc>"#rest"			is(REST);
<etc>"#key"			is(KEY);
<etc>"#all-keys"		is(ALL_KEYS);

<etc>[-+]?{D}+			is(INTEGER);
<etc>#x[0-9a-fA-F]+		is(INTEGER);
<etc>#o[0-7]+			is(INTEGER);
<etc>#b[01]+			is(INTEGER);

<etc>[-+]?{D}*\.{D}+{E}?	is(FLOAT);
<etc>[-+]?{D}+\.{D}*{E}?	is(FLOAT);
<etc>[-+]?{D}+{E}		is(FLOAT);

<etc>'[ -&(-\[\]-~]'		is(CHARACTER);
<etc>'\\["'\\abefnrt0]'		is(CHARACTER);
<etc>'\\<[0-9a-fA-F]{1,4}>'	is(CHARACTER);

<etc>{STR}			is(STRING);

<etc>{O}			is(BINARY_OPERATOR);
<etc>{N}			is(SYMBOL);
<etc>\\{O}			is(SYMBOL);
<etc>\\{N}			is(SYMBOL);
<etc>\\~			is(SYMBOL);
<etc>{N}:			is(KEYWORD);
<etc>#{STR}			is(SYMBOL_LITERAL);

<etc>"#if"			is(FEATURE_IF);

<etc>"#elseif"			is(FEATURE_ELSE_IF);
<etc>"#else"			is(FEATURE_ELSE);
<etc>"#endif"			is(FEATURE_ENDIF);

<ini,key,val,etc>.		is(BOGUS);

%%

static void skip_multi_line_comment(void)
{
    int depth = 1;
    int c, prev = '\0';
    
    while (1) {
	c = input();
	switch (c) {
	  case EOF:
	    return;
	  case '\n':
	  case '\r':
	    line_count++;
	    prev = c;
	    break;
	  case '/':
	    if (prev == '*')
		if (--depth == 0)
		    return;
		else
		    prev = 0;
	    else
		prev = c;
	    break;
	  case '*':
	    if (prev == '/') {
		depth++;
		prev = 0;
	    }
	    else
		prev = c;
	    break;
	  default:
	    prev = c;
	    break;
	}
    }
}

static int make_header_key()
{
  yylval.token = make_token(yytext, yyleng-1);
  return HEADER_KEY;
}
static int make_header_val()
{
  char *p1, *p2, tentative_line_terminator;
  int skipped = 0;

  for (p1 = p2 = yytext; p1 < yytext + yyleng; ) {
    /* skip initial spaces */
    while (p1 < yytext + yyleng && (*p1 == ' ' || *p1 == '\t')) {
      p1 += 1;
      skipped += 1;
    }
    /* copy to end of line */
    while (p1 < yytext + yyleng && (*p1 != '\n' && *p1 != '\r')) {
      *p2++ = *p1++;
    }
    /* copy and count newline */
    *p2++ = *p1++;
    line_count++;
    /* check for weird (Mac and Windows) line-endings */
    tentative_line_terminator = *(p1 - 1);
    if((*p1 == '\n' || *p1 == '\r') && tentative_line_terminator != *p1)
    {
      *p2++ = *p1++;
      skipped++;
    }
  }
  /* make the token, dropping the last newline */
  yylval.token = make_token(yytext, yyleng - skipped - 1);
  return HEADER_VAL;
}
static int make_header_end()
{
  line_count += 1;
  yylval.token = yylval.token = make_token(yytext, yyleng);
  return HEADER_END;
}

int yywrap() { return 1; }
