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
* This file implements the various kinds of literal constants.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "literal.h"
#include "lose.h"

struct literal_list {
    struct literal *head;
    struct literal **tail;
};


struct literal *make_true_literal(void)
{
    struct literal *res = malloc(sizeof(struct literal));

    res->kind = literal_TRUE;
    res->next = NULL;
    res->line = 0;

    return res;
}

struct literal *make_false_literal(void)
{
    struct literal *res = malloc(sizeof(struct literal));

    res->kind = literal_FALSE;
    res->next = NULL;
    res->line = 0;

    return res;
}

struct literal *make_unbound_literal(void)
{
    struct literal *res = malloc(sizeof(struct literal));

    res->kind = literal_UNBOUND;
    res->next = NULL;
    res->line = 0;

    return res;
}

struct literal *make_string_literal(char *str)
{
    int len = strlen(str);
    struct string_literal *res = malloc(sizeof(struct string_literal)
					+ len + 1 - sizeof(res->chars));

    res->kind = literal_STRING;
    res->next = NULL;
    res->line = 0;
    res->length = len;

    strcpy((char *)res->chars, str);

    return (struct literal *)res;
}

struct literal *make_character_literal(int c)
{
    struct character_literal *res = malloc(sizeof(struct character_literal));

    res->kind = literal_CHARACTER;
    res->next = NULL;
    res->line = 0;
    res->value = c;

    return (struct literal *)res;
}

struct literal *make_integer_literal(long value)
{
    struct integer_literal *res = malloc(sizeof(struct integer_literal));

    res->kind = literal_INTEGER;
    res->next = NULL;
    res->line = 0;
    res->value = value;

    return (struct literal *)res;
}

struct literal *make_symbol_literal(struct symbol *sym)
{
    struct symbol_literal *res = malloc(sizeof(struct symbol_literal));

    res->kind = literal_SYMBOL;
    res->next = NULL;
    res->line = 0;
    res->symbol = sym;

    return (struct literal *)res;
}

struct literal
    *make_dotted_list_literal(struct literal_list *guts, struct literal *tail)
{
    struct list_literal *res = malloc(sizeof(struct list_literal));

    res->kind = literal_LIST;
    res->next = NULL;
    res->line = 0;
    if (tail != NULL && tail->kind == literal_LIST) {
	*guts->tail = ((struct list_literal *)tail)->first;
	free(tail);
	res->tail = NULL;
    }
    else
	res->tail = tail;
    res->first = guts->head;

    free(guts);

    return (struct literal *)res;
}

struct literal *make_list_literal(struct literal_list *guts)
{
    return make_dotted_list_literal(guts, NULL);
}

struct literal *make_vector_literal(struct literal_list *guts)
{
    struct vector_literal *res = malloc(sizeof(struct vector_literal));

    res->kind = literal_VECTOR;
    res->next = NULL;
    res->line = 0;
    res->first = guts->head;

    free(guts);

    return (struct literal *)res;
}

struct literal_list *make_literal_list(void)
{
    struct literal_list *res = malloc(sizeof(struct literal_list));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct literal_list *add_literal(struct literal_list *list,
				 struct literal *literal)
{
    *list->tail = literal;
    list->tail = &literal->next;

    return list;
}

void free_literal(struct literal *literal)
{
    struct literal *part, *next;

    switch (literal->kind) {
      case literal_SYMBOL:
      case literal_INTEGER:
      case literal_SINGLE_FLOAT:
      case literal_DOUBLE_FLOAT:
      case literal_EXTENDED_FLOAT:
      case literal_CHARACTER:
      case literal_STRING:
      case literal_TRUE:
      case literal_FALSE:
      case literal_UNBOUND:
	break;
      case literal_LIST:
	if (((struct list_literal *)literal)->tail)
	    free_literal(((struct list_literal *)literal)->tail);
	/* Fall though */
      case literal_VECTOR:
	for (part = ((struct vector_literal *)literal)->first;
	     part != NULL;
	     part = next) {
	    next = part->next;
	    free_literal(part);
	}
	break;
      default:
	lose("Bogus literal kind.");
    }
    free(literal);
}

struct literal *dup_literal(struct literal *literal)
{
    size_t size = 0;
    struct literal *res, *tail;
    struct literal *l, **prev;

    switch (literal->kind) {
      case literal_SYMBOL:
	size = sizeof(struct symbol_literal);
	break;
      case literal_INTEGER:
	size = sizeof(struct integer_literal);
	break;
      case literal_SINGLE_FLOAT:
	size = sizeof(struct single_float_literal);
	break;
      case literal_DOUBLE_FLOAT:
	size = sizeof(struct double_float_literal);
	break;
      case literal_EXTENDED_FLOAT:
	size = sizeof(struct extended_float_literal);
	break;
      case literal_CHARACTER:
	size = sizeof(struct character_literal);
	break;
      case literal_STRING:
	size = sizeof(struct string_literal)
	    + ((struct string_literal *)literal)->length + 1
		  - sizeof(((struct string_literal *)literal)->chars);
	break;
      case literal_TRUE:
      case literal_FALSE:
      case literal_UNBOUND:
	size = sizeof(struct literal);
	break;
      case literal_LIST:
	size = sizeof(struct list_literal);
	break;
      case literal_VECTOR:
	size = sizeof(struct vector_literal);
	break;
      default:
	lose("Bogus literal kind.");
    }

    res = malloc(size);
    memcpy(res, literal, size);

    switch (literal->kind) {
      case literal_LIST:
	tail = ((struct list_literal *)literal)->tail;
	if (tail != NULL)
	    ((struct list_literal *)res)->tail = dup_literal(tail);
	else
	    ((struct list_literal *)res)->tail = NULL;
	/* Fall though */
      case literal_VECTOR:
	prev = &((struct vector_literal *)res)->first;
	for (l = *prev; l != NULL; l = l->next) {
	    *prev = dup_literal(l);
	    prev = &(*prev)->next;
	}
	break;
      default:
	break;
    }

    res->next = NULL;

    return res;
}
	    
	
