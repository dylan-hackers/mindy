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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/lexer.l,v 1.2 1994/03/30 05:59:38 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

%{
#include "mindycomp.h"
#include "lexer.h"
#include "src.h"
#include "parser.tab.h"

int line_count = 1;

#define is(type) \
    do { yylval.token = make_token(yytext, yyleng); return type; } while(0)

static void skip_multi_line_comment(void);
static struct token *make_token(char *ptr, int len);

%}

D	[0-9]
E	E[-+]?{D}+

IOC	[!&*+/<=>?|^$%@_]
OC	[-~]|{IOC}
ON	{IOC}{OC}*
O	-|~|:=|{ON}
A0	[a-z0-9]
S	{ON}?{A0}({A0}|{OC})*

STR	\"(([ !#-\[\]-~])|(\\["\\bfnrt]))*\"

%%

[ \t\f]+	;
[\n]		line_count++;

"//".*		;
"/*"		skip_multi_line_comment();

abstract	is(ABSTRACT);
above		is(ABOVE);
begin		is(DBEGIN);
below		is(BELOW);
block		is(BLOCK);
by		is(BY);
case		is(CASE);
class		is(CLASS);
cleanup		is(CLEANUP);
concrete	is(CONCRETE);
constant	is(CONSTANT);
define		is(DEFINE);
else		is(ELSE);
elseif		is(ELSEIF);
end		is(END);
exception	is(EXCEPTION);
finally		is(FINALLY);
for		is(FOR);
from		is(FROM);
generic		is(GENERIC);
handler		is(HANDLER);
if		is(IF);
in		is(IN);
inherited	is(INHERITED);
instance	is(INSTANCE);
keyword		is(KEYWORD_RESERVED_WORD);
let		is(LET);
local		is(LOCAL);
method		is(METHOD);
open		is(OPEN);
otherwise	is(OTHERWISE);
primary		is(PRIMARY);
required	is(REQUIRED);
seal		is(SEAL);
sealed		is(SEALED);
select		is(SELECT);
slot		is(SLOT);
subclass	is(SUBCLASS);
then		is(THEN);
to		is(TO);
unless		is(UNLESS);
until		is(UNTIL);
variable	is(VARIABLE);
virtual		is(VIRTUAL);
while		is(WHILE);

module		is(MODULE);
library		is(LIBRARY);
export		is(EXPORT);
create		is(CREATE);
use		is(USE);
all		is(ALL);

"prefix:"	is(PREFIX_OPTION);
"import:"	is(IMPORT_OPTION);
"exclude:"	is(EXCLUDE_OPTION);
"export:"	is(EXPORT_OPTION);
"rename:"	is(RENAME_OPTION);

"("		is(LPAREN);
")"		is(RPAREN);
","		is(COMMA);
"."		is(DOT);
";"		is(SEMI);
"["		is(LBRACKET);
"]"		is(RBRACKET);
"{"		is(LBRACE);
"}"		is(RBRACE);
"::"		is(COLON_COLON);
"-"		is(MINUS);
"~"		is(TILDE);
"="		is(EQUAL);
"=="		is(EQUAL_EQUAL);
"=>"		is(ARROW);
"#("		is(SHARP_PAREN);
"#["		is(SHARP_BRACKET);
"#t"		is(SHARP_T);
"#f"		is(SHARP_F);
"#next"		is(NEXT);
"#rest"		is(REST);
"#key"		is(KEY);
"#all-keys"	is(ALL_KEYS);

[-+]?{D}+	is(INTEGER);
#x[0-9a-f]+	is(INTEGER);
#o[0-7]+	is(INTEGER);
#b[01]+		is(INTEGER);

[-+]?{D}*\.{D}+{E}?	is(FLOAT);
[-+]?{D}+\.{D}*{E}?	is(FLOAT);
[-+]?{D}+{E}		is(FLOAT);

'[ -&(-\[\]-~]'	is(CHARACTER);
'\\['\\bfnrt]'	is(CHARACTER);

{STR}		is(STRING);

{O}		is(BINARY_OPERATOR);
{S}		is(SYMBOL);
\\{O}		is(SYMBOL);
{S}:		is(KEYWORD);
#{STR}		is(SYMBOL_LITERAL);

.		is(BOGUS);

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

static struct token *make_token(char *ptr, int len)
{
    struct token *token = malloc(sizeof(struct token) + len + 1);

    token->length = len;
    bcopy(ptr, token->chars, len);
    token->line = line_count;
    token->chars[len] = 0;

    return token;
}
