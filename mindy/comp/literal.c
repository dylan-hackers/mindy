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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/literal.c,v 1.3 1994/03/31 10:16:58 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindycomp.h"
#include "literal.h"

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
    struct string_literal *res = malloc(sizeof(struct string_literal)+len+1);

    res->kind = literal_STRING;
    res->next = NULL;
    res->line = 0;
    res->length = len;

    strcpy(res->chars, str);

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

struct literal *make_float_literal(double value)
{
    struct float_literal *res = malloc(sizeof(struct float_literal));

    res->kind = literal_FLOAT;
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

