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
* $Header: /scm/cvs/src/mindy/interp/sym.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file implements symbols.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include <ctype.h>

#include "mindy.h"
#include "gc.h"
#include "bool.h"
#include "class.h"
#include "obj.h"
#include "coll.h"
#include "str.h"
#include "def.h"
#include "list.h"
#include "type.h"
#include "print.h"
#include "sym.h"
#include "num.h"

obj_t obj_SymbolClass = NULL;

struct symbol {
    obj_t class;
    obj_t name;
    obj_t next;
    unsigned hash;
};

#define SYMBOL(x) obj_ptr(struct symbol *, x)

static int symtab_entries = 0;
static int symtab_threshold = 0;
static int symtab_length = 0;
static obj_t *symtab_table = NULL;

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

static void rehash_table(void)
{
    int new_length;
    obj_t *new_table;
    obj_t *ptr;
    int i;

    if (symtab_length < 1024)
	new_length = symtab_length << 1;
    else
	new_length = symtab_length + 1024;

    new_table = (obj_t *)malloc(sizeof(obj_t)*new_length);

    ptr = new_table;
    for (i = 0; i < new_length; i++)
	*ptr++ = obj_False;

    ptr = symtab_table;
    for (i = 0; i < symtab_length; i++) {
	obj_t sym, next;
	for (sym = *ptr++; sym != obj_False; sym = next) {
	    int index = SYMBOL(sym)->hash % new_length;
	    next = SYMBOL(sym)->next;
	    SYMBOL(sym)->next = new_table[index];
	    new_table[index] = sym;
	}
    }

    free(symtab_table);
    symtab_table = new_table;
    symtab_length = new_length;
    symtab_threshold = (new_length * 3) / 2;
}

obj_t symbol(char *name)
{
    unsigned hash = hash_name(name);
    int index = hash % symtab_length;
    obj_t sym;

    for (sym = symtab_table[index];
	 sym != obj_False;
	 sym = SYMBOL(sym)->next) {
	if (SYMBOL(sym)->hash == hash) {
	    if (same_name(name, string_chars(SYMBOL(sym)->name)))
		return sym;
	}
    }

    sym = alloc(obj_SymbolClass, sizeof(struct symbol));
    SYMBOL(sym)->name = make_byte_string(name);
    SYMBOL(sym)->next = symtab_table[index];
    SYMBOL(sym)->hash = hash;
    symtab_table[index] = sym;
    
    symtab_entries++;
    if (symtab_entries >= symtab_threshold)
	rehash_table();

    return sym;
}

char *sym_name(obj_t sym)
{
    return string_chars(SYMBOL(sym)->name);
}

unsigned sym_hash(obj_t sym)
{
    return SYMBOL(sym)->hash;
}



/* Dylan functions. */

/* The following as methods only work on <byte-string>s */

static obj_t string_as_symbol(obj_t class, obj_t string)
{
    return symbol((char *)obj_ptr(struct string *, string)->chars);
}

static obj_t symbol_as_string(obj_t class, obj_t symbol)
{
    return SYMBOL(symbol)->name;
}

static obj_t symbol_object_hash(obj_t sym)
{
    return make_fixnum(sym_hash(sym));
}    


/* Printing. */

static void print_symbol(obj_t symbol)
{
    printf("#\"%s\"", sym_name(symbol));
}


/* GC stuff. */

static int scav_sym(struct object *o)
{
    struct symbol *sym = (struct symbol *)o;

    scavenge(&sym->name);
    scavenge(&sym->next);

    return sizeof(struct symbol);
}

static obj_t trans_sym(obj_t sym)
{
    return transport(sym, sizeof(struct symbol), FALSE);
}

void scavenge_symbol_roots(void)
{
    int i;
    obj_t *ptr;

    ptr = symtab_table;
    for (i = 0; i < symtab_length; i++)
	scavenge(ptr++);
}


/* Init stuff. */

void make_sym_classes(void)
{
    obj_SymbolClass = make_builtin_class(scav_sym, trans_sym);
    add_constant_root(&obj_SymbolClass);
}

void init_symbol_tables(void)
{
    obj_t *ptr;
    int i;

    symtab_entries = 0;
    symtab_threshold = 96;
    symtab_length = 64;
    symtab_table = (obj_t *)malloc(sizeof(obj_t)*64);
    ptr = symtab_table;
    for (i = 0; i < 64; i++)
	*ptr++ = obj_False;
}

void init_sym_classes(void)
{
    init_builtin_class(obj_SymbolClass, "<symbol>", obj_ObjectClass, NULL);
    def_printer(obj_SymbolClass, print_symbol);
}

void init_sym_functions(void)
{
    define_generic_function("as", list2(obj_TypeClass, obj_ObjectClass),
			    FALSE, obj_False, FALSE,
			    list1(obj_ObjectClass), obj_False);
    define_method("as", list2(singleton(obj_SymbolClass), obj_ByteStringClass),
		  FALSE, obj_False, FALSE, obj_SymbolClass, string_as_symbol);
    define_method("as", list2(singleton(obj_StringClass), obj_SymbolClass),
		  FALSE, obj_False, FALSE, obj_ByteStringClass,
		  symbol_as_string);
    /* same method as above, only for singleton <byte-string> */
    define_method("as", list2(singleton(obj_ByteStringClass), obj_SymbolClass),
		  FALSE, obj_False, FALSE, obj_ByteStringClass,
		  symbol_as_string);
    define_method("symbol-hash", list1(obj_SymbolClass), FALSE, obj_False,
		  FALSE, obj_FixnumClass, symbol_object_hash);
}
