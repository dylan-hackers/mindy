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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/info.c,v 1.2 1994/04/06 17:43:44 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindycomp.h"
#include "src.h"
#include "sym.h"
#include "info.h"


/* Binop infos. */

struct binop_info_chain {
    struct symbol *symbol;
    struct binop_info_chain *next;
    struct binop_info info;
};

static struct binop_info_chain *BinopInfos = NULL;
static struct binop_info DefaultBinopInfo = {0, TRUE};

struct binop_info *lookup_binop_info(struct id *id)
{
    struct binop_info_chain *p;

    for (p = BinopInfos; p != NULL; p = p->next)
	if (p->symbol == id->symbol)
	    return &p->info;
    return &DefaultBinopInfo;
}

static void push_binop_info(char *name, int prec, boolean left)
{
    struct binop_info_chain *new = malloc(sizeof(struct binop_info_chain));

    new->symbol = symbol(name);
    new->info.precedence = prec;
    new->info.left_assoc = left;
    new->next = BinopInfos;
    BinopInfos = new;
}


/* Function infos. */

struct function_info_chain {
    struct symbol *symbol;
    boolean internal;
    struct function_info_chain *next;
    struct function_info info;
};

static struct function_info_chain *FunctionInfos = NULL;

struct function_info *lookup_function_info(struct id *id, boolean createp)
{
    struct function_info_chain *p;

    for (p = FunctionInfos; p != NULL; p = p->next)
	if (p->symbol == id->symbol && p->internal == id->internal)
	    return &p->info;
    if (createp) {
	p = malloc(sizeof(struct function_info_chain));
	p->symbol = id->symbol;
	p->internal = id->internal;
	p->next = FunctionInfos;
	FunctionInfos = p;
	p->info.srctran = NULL;
	return &p->info;
    }
    return NULL;
}


/* Init stuff. */

void init_info(void)
{
    push_binop_info(":=", 1, FALSE);
    push_binop_info("|", 2, FALSE);
    push_binop_info("&", 3, FALSE);
    push_binop_info("<", 4, TRUE);
    push_binop_info("<=", 4, TRUE);
    push_binop_info("=", 4, TRUE);
    push_binop_info("==", 4, TRUE);
    push_binop_info("~=", 4, TRUE);
    push_binop_info(">=", 4, TRUE);
    push_binop_info(">", 4, TRUE);
    push_binop_info("+", 5, TRUE);
    push_binop_info("-", 5, TRUE);
    push_binop_info("*", 6, TRUE);
    push_binop_info("/", 6, TRUE);
}
