#ifndef lint
static char yysccsid[] = "@(#)yaccpar 1.8 (Berkeley) 01/20/91";
#endif
#define YYBYACC 1
#line 33 "parser.y"
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

#line 23 "parser-tab.tab.c"
#define tok_TRUE 257
#define tok_FALSE 258
#define tok_ERROR 259
#define tok_LPAREN 260
#define tok_RPAREN 261
#define tok_DEBUGVAR 262
#define tok_ARG 263
#define tok_NUMBER 264
#define tok_CHARACTER 265
#define tok_STRING 266
#define tok_ADDRESS 267
#define tok_SYMBOL 268
#define tok_KEYWORD 269
#define tok_COMMA 270
#define tok_EXTERN_NAME 271
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    0,    0,    1,    2,    2,    3,    3,    3,
    4,    4,    4,    4,    4,    6,    6,    6,    6,    6,
    6,    6,    5,    5,    7,    7,
};
short yylen[] = {                                         2,
    2,    1,    0,    1,    1,    1,    3,    1,    3,    4,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    2,    3,    0,    2,
};
short yydefred[] = {                                      0,
    4,    5,    0,    0,   16,   17,   11,   12,   21,   20,
   19,   22,   13,   18,   14,    1,    0,    8,   15,    0,
    0,    9,    0,    0,    0,    7,    0,    0,   23,   10,
   24,   26,
};
short yydgoto[] = {                                       3,
    4,   16,   17,   18,   25,   19,   29,
};
short yysindex[] = {                                   -223,
    0,    0,    0, -242,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0, -217,    0,    0, -257,
 -242,    0, -242, -212, -259,    0, -212, -227,    0,    0,
    0,    0,
};
short yyrindex[] = {                                      3,
    0,    0,    0,   13,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   17,    0,    0,    0,
    0,    0, -214, -243,    0,    0, -243,    0,    0,    0,
    0,    0,
};
short yygindex[] = {                                      0,
    0,   -2,   29,    0,    4,    0,    1,
};
#define YYTABLESIZE 58
short yytable[] = {                                       5,
    6,   30,    3,   22,    7,    8,    9,   10,   11,   12,
   13,   23,    2,   15,    5,    6,    6,   25,   26,    7,
    8,    9,   10,   11,   12,   13,   14,   31,   15,    5,
    6,   32,    1,    0,    7,    8,    9,   10,   11,   12,
   13,   23,   20,   15,    2,   18,   18,   20,   24,    0,
    0,   27,   21,    0,    0,   18,   24,   28,
};
short yycheck[] = {                                     257,
  258,  261,    0,  261,  262,  263,  264,  265,  266,  267,
  268,  269,    0,  271,  257,  258,    0,  261,   21,  262,
  263,  264,  265,  266,  267,  268,  269,   27,  271,  257,
  258,   28,  256,   -1,  262,  263,  264,  265,  266,  267,
  268,  269,  260,  271,  268,  260,  261,  260,   20,   -1,
   -1,   23,  270,   -1,   -1,  270,   28,  270,
};
#define YYFINAL 3
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 271
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"tok_TRUE","tok_FALSE",
"tok_ERROR","tok_LPAREN","tok_RPAREN","tok_DEBUGVAR","tok_ARG","tok_NUMBER",
"tok_CHARACTER","tok_STRING","tok_ADDRESS","tok_SYMBOL","tok_KEYWORD",
"tok_COMMA","tok_EXTERN_NAME",
};
char *yyrule[] = {
"$accept : start",
"start : command exprlist",
"start : command",
"start :",
"start : error",
"command : tok_SYMBOL",
"exprlist : expr",
"exprlist : expr tok_COMMA exprlist",
"expr : leaf",
"expr : expr tok_LPAREN tok_RPAREN",
"expr : expr tok_LPAREN arglist tok_RPAREN",
"leaf : tok_DEBUGVAR",
"leaf : tok_ARG",
"leaf : tok_SYMBOL",
"leaf : tok_EXTERN_NAME",
"leaf : literal",
"literal : tok_TRUE",
"literal : tok_FALSE",
"literal : tok_KEYWORD",
"literal : tok_STRING",
"literal : tok_CHARACTER",
"literal : tok_NUMBER",
"literal : tok_ADDRESS",
"arglist : expr more_args",
"arglist : tok_KEYWORD expr more_args",
"more_args :",
"more_args : tok_COMMA arglist",
};
#endif
#ifndef YYSTYPE
typedef int YYSTYPE;
#endif
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#ifdef YYSTACKSIZE
#ifndef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#endif
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH 500
#endif
#endif
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#line 133 "parser.y"

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
#line 182 "parser-tab.tab.c"
#define YYABORT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
yyparse()
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register char *yys;
    extern char *getenv();

    if (yys = getenv("YYDEBUG"))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if (yyn = yydefred[yystate]) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("yydebug: state %d, reading %d (%s)\n", yystate,
                    yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("yydebug: state %d, shifting to state %d\n",
                    yystate, yytable[yyn]);
#endif
        if (yyssp >= yyss + yystacksize - 1)
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#ifdef lint
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#ifdef lint
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("yydebug: state %d, error recovery shifting\
 to state %d\n", *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yyss + yystacksize - 1)
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("yydebug: error recovery discarding state %d\n",
                            *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("yydebug: state %d, error recovery discards token %d (%s)\n",
                    yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("yydebug: state %d, reducing by rule %d (%s)\n",
                yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 71 "parser.y"
{ result = pair(yyvsp[-1],yyvsp[0]); }
break;
case 2:
#line 73 "parser.y"
{ result = pair(yyvsp[0],obj_Nil); }
break;
case 3:
#line 75 "parser.y"
{ result = obj_Nil; }
break;
case 4:
#line 77 "parser.y"
{ result = make_byte_string("command error: try ``help''"); }
break;
case 6:
#line 84 "parser.y"
{ yyval = list1(yyvsp[0]); }
break;
case 7:
#line 86 "parser.y"
{ yyval = pair(yyvsp[-2], yyvsp[0]); }
break;
case 8:
#line 90 "parser.y"
{ yyval = yyvsp[0]; }
break;
case 9:
#line 92 "parser.y"
{ yyval = list2(symbol("funcall"), yyvsp[-2]); }
break;
case 10:
#line 94 "parser.y"
{ yyval = pair(symbol("funcall"), pair(yyvsp[-3], yyvsp[-1])); }
break;
case 11:
#line 98 "parser.y"
{ yyval = pair(symbol("debug-var"), yyvsp[0]); }
break;
case 12:
#line 100 "parser.y"
{ yyval = pair(symbol("arg"), yyvsp[0]); }
break;
case 13:
#line 102 "parser.y"
{ yyval = list2(symbol("variable"), yyvsp[0]); }
break;
case 14:
#line 104 "parser.y"
{ yyval = pair(symbol("variable"), yyvsp[0]); }
break;
case 15:
#line 106 "parser.y"
{ yyval = pair(symbol("literal"), yyvsp[0]); }
break;
case 16:
#line 110 "parser.y"
{ yyval = obj_True; }
break;
case 17:
#line 112 "parser.y"
{ yyval = obj_False; }
break;
case 23:
#line 121 "parser.y"
{ yyval = pair(yyvsp[-1], yyvsp[0]); }
break;
case 24:
#line 123 "parser.y"
{ yyval = pair(pair(symbol("literal"),yyvsp[-2]),pair(yyvsp[-1], yyvsp[0])); }
break;
case 25:
#line 127 "parser.y"
{ yyval = obj_Nil; }
break;
case 26:
#line 129 "parser.y"
{ yyval = yyvsp[0]; }
break;
#line 402 "parser-tab.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("yydebug: after reduction, shifting from state 0 to\
 state %d\n", YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("yydebug: state %d, reading %d (%s)\n",
                        YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("yydebug: after reduction, shifting from state %d \
to state %d\n", *yyssp, yystate);
#endif
    if (yyssp >= yyss + yystacksize - 1)
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
