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
* $Header: /scm/cvs/src/mindy/comp/sym.c,v 1.1 1998/05/03 19:55:09 andreas Exp $
*
* This file implements symbols.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include <ctype.h>

#include "mindycomp.h"
#include "sym.h"

struct symbol *sym_DefineClass1 = NULL;
struct symbol *sym_DefineClass2 = NULL;
struct symbol *sym_DefineDomain = NULL;
struct symbol *sym_DefineGeneric = NULL;
struct symbol *sym_DefineMethod = NULL;
struct symbol *sym_DefineSlot = NULL;
struct symbol *sym_Or = NULL;
struct symbol *sym_Plus = NULL;
struct symbol *sym_Less = NULL;
struct symbol *sym_LessEqual = NULL;
struct symbol *sym_Object = NULL;
struct symbol *sym_Type = NULL;
struct symbol *sym_Eq = NULL;
struct symbol *sym_DylanUser = NULL;
struct symbol *sym_Apply = NULL;
struct symbol *sym_Aref = NULL;
struct symbol *sym_Catch = NULL;
struct symbol *sym_CheckType = NULL;
struct symbol *sym_Class = NULL;
struct symbol *sym_Do = NULL;
struct symbol *sym_Element = NULL;
struct symbol *sym_Error = NULL;
struct symbol *sym_FindVariable = NULL;
struct symbol *sym_ForwardIterationProtocol = NULL;
struct symbol *sym_Getter = NULL;
struct symbol *sym_InitVariable = NULL;
struct symbol *sym_Instance = NULL;
struct symbol *sym_List = NULL;
struct symbol *sym_MakeInherited = NULL;
struct symbol *sym_MakeInitarg = NULL;
struct symbol *sym_MakeNextMethodFunction = NULL;
struct symbol *sym_MakeSlot = NULL;
struct symbol *sym_Negative = NULL;
struct symbol *sym_NegativeP = NULL;
struct symbol *sym_NextMethod = NULL;
struct symbol *sym_Not = NULL;
struct symbol *sym_PopHandler = NULL;
struct symbol *sym_PushHandler = NULL;
struct symbol *sym_Setter = NULL;
struct symbol *sym_Singleton = NULL;
struct symbol *sym_Each_Subclass = NULL;
struct symbol *sym_Throw = NULL;
struct symbol *sym_Uwp = NULL;
struct symbol *sym_Values = NULL;
struct symbol *sym_Virtual = NULL;

static struct table {
    int entries;
    int threshold;
    int length;
    struct symbol **table;
} Symbols;

static unsigned hash_name(char *name)
{
    unsigned char *ptr;
    unsigned hash = 0;

    for (ptr = (unsigned char *)name; *ptr; ptr++)
	hash = ((hash<<5)|(hash>>27)) ^ (*ptr & ~('a'^'A'));

    return hash;
}

static boolean same_name(char *name1, char *name2)
{
    char c1, c2;

    while (1) {
	c1 = *name1++;
	c2 = *name2++;
	
	if (c1) {
	    if ((isupper(c1) ? tolower(c1) : c1)
		!= (isupper(c2) ? tolower(c2) : c2))
		return FALSE;
	}
	else if (c2)
	    return FALSE;
	else
	    return TRUE;
    }
}

static void rehash_table(struct table *table)
{
    int new_length;
    struct symbol **new_table;
    struct symbol **ptr;
    int i;

    if (table->length < 1024)
	new_length = table->length << 1;
    else
	new_length = table->length + 1024;

    new_table = malloc(sizeof(struct symbol *) * new_length);

    ptr = new_table;
    for (i = 0; i < new_length; i++)
	*ptr++ = NULL;

    ptr = table->table;
    for (i = 0; i < table->length; i++) {
	struct symbol *id, *next;
	for (id = *ptr++; id != NULL; id = next) {
	    int index = id->hash % new_length;
	    next = id->next;
	    id->next = new_table[index];
	    new_table[index] = id;
	}
    }

    free(table->table);
    table->table = new_table;
    table->length = new_length;
    table->threshold = (new_length * 3) / 2;
}

static struct symbol *intern(char *name, struct table *table)
{
    unsigned hash = hash_name(name);
    int index = hash % table->length;
    struct symbol *id;

    for (id = table->table[index]; id != NULL; id = id->next)
	if (id->hash == hash && same_name(name, (char *)id->name))
	    return id;

    id = malloc(sizeof(struct symbol) + strlen(name) + 1);
    id->hash = hash;
    id->next = table->table[index];
    id->handle = -1;
    table->table[index] = id;
    strcpy((char *)id->name, name);
    
    table->entries++;
    if (table->entries >= table->threshold)
	rehash_table(table);

    return id;
}

struct symbol *symbol(char *name)
{
    return intern(name, &Symbols);
}

struct symbol *gensym(void)
{
    static int counter = 0;
    static int rollover = 10;
    static int digits = 1;

    struct symbol *res = malloc(sizeof(struct symbol) + 2 + digits);

    res->hash = (unsigned long)res;
    res->next = NULL;
    res->handle = -1;
    sprintf((char *)res->name, "g%d", counter++);

    if (counter == rollover) {
	digits++;
	rollover *= 10;
    }

    return res;
}


/* Init stuff. */

static void init_table(struct table *table)
{
    struct symbol **ptr;
    int i;

    table->entries = 0;
    table->threshold = 96;
    table->length = 64;
    table->table = (struct symbol **)malloc(sizeof(struct symbol *)*64);
    ptr = table->table;
    for (i = 0; i < 64; i++)
	*ptr++ = NULL;
}

void init_sym_table(void)
{
    init_table(&Symbols);

    sym_DefineClass1 = symbol("%define-class-1");
    sym_DefineClass2 = symbol("%define-class-2");
    sym_DefineDomain = symbol("%define-sealed-domain");
    sym_DefineGeneric = symbol("%define-generic");
    sym_DefineMethod = symbol("%define-method");
    sym_DefineSlot = symbol("%define-slot");
    sym_Or = symbol("|");
    sym_Plus = symbol("+");
    sym_Less = symbol("<");
    sym_LessEqual = symbol("<=");
    sym_Object = symbol("<object>");
    sym_Type = symbol("<type>");
    sym_Eq = symbol("==");
    sym_DylanUser = symbol("Dylan-User");
    sym_Apply = symbol("apply");
    sym_Aref = symbol("aref");
    sym_Catch = symbol("catch");
    sym_CheckType = symbol("check-type");
    sym_Class = symbol("class");
    sym_Do = symbol("do");
    sym_Element = symbol("element");
    sym_Error = symbol("error");
    sym_FindVariable = symbol("find-variable");
    sym_ForwardIterationProtocol = symbol("forward-iteration-protocol");
    sym_Getter = symbol("getter");
    sym_InitVariable = symbol("init-variable");
    sym_Instance = symbol("instance");
    sym_List = symbol("list");
    sym_MakeInherited = symbol("make-inherited");
    sym_MakeInitarg = symbol("make-initarg");
    sym_MakeNextMethodFunction = symbol("make-next-method-function");
    sym_MakeSlot = symbol("make-slot");
    sym_Negative = symbol("negative");
    sym_NegativeP = symbol("negative?");
    sym_NextMethod = symbol("next-method");
    sym_Not = symbol("~");
    sym_PopHandler = symbol("pop-handler");
    sym_PushHandler = symbol("push-handler");
    sym_Setter = symbol("setter");
    sym_Singleton = symbol("singleton");
    sym_Each_Subclass = symbol("each-subclass");
    sym_Throw = symbol("throw");
    sym_Uwp = symbol("uwp");
    sym_Values = symbol("values");
    sym_Virtual = symbol("virtual");
}
