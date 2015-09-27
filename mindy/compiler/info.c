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
* This file maintains info about builtin/magic functions/names.
*
\**********************************************************************/

#include "../compat/std-c.h"

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
    push_binop_info("~=", 4, TRUE);
    push_binop_info("==", 4, TRUE);
    push_binop_info("~==", 4, TRUE);
    push_binop_info(">=", 4, TRUE);
    push_binop_info(">", 4, TRUE);
    push_binop_info("+", 5, TRUE);
    push_binop_info("-", 5, TRUE);
    push_binop_info("*", 6, TRUE);
    push_binop_info("/", 6, TRUE);
    push_binop_info("^", 7, FALSE);
}
