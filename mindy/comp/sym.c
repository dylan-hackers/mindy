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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/sym.c,v 1.2 1994/03/31 10:16:37 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <ctype.h>

#include "mindycomp.h"
#include "sym.h"

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
	if (id->hash == hash && same_name(name, id->name))
	    return id;

    id = malloc(sizeof(struct symbol) + strlen(name) + 1);
    id->hash = hash;
    id->next = table->table[index];
    id->handle = -1;
    table->table[index] = id;
    strcpy(id->name, name);
    
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

    struct symbol *res = malloc(sizeof(struct symbol) + 1 + digits);

    res->hash = (unsigned long)res;
    res->next = NULL;
    res->handle = -1;
    sprintf(res->name, "g%d", counter++);

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

void init_id_tables(void)
{
    init_table(&Symbols);
}
