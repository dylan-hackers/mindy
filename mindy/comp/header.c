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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/header.c,v 1.2 1994/03/30 05:55:50 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <ctype.h>

#include "mindycomp.h"
#include "header.h"
#include "lexer.h"			/* for "line_count" */

struct header_handler {
    char *key;
    void (*func)(char *value);
    struct header_handler *next;
};

static struct header_handler *handlers = NULL;

void add_header_handler(char *key, void (*func)(char *value))
{
    struct header_handler *new = malloc(sizeof(struct header_handler));

    new->key = key;
    new->func = func;
    new->next = handlers;
    handlers = new;
}

static void process_header(char *key, char *value)
{
    struct header_handler *handler;

    for (handler = handlers; handler != NULL; handler = handler->next) {
	if (strcmp(key, handler->key) == 0) {
	    (*handler->func)(value);
	    return;
	}
    }
}

static char *buffer = NULL;
static char *buffer_end = NULL;

static char *append(char *fill, int c)
{
    if (fill == buffer_end) {
	int offset = fill - buffer;
	if (buffer) {
	    int new_len = (buffer_end - buffer) * 2;
	    buffer = realloc(buffer, new_len);
	    buffer_end = buffer + new_len;
	}
	else {
	    buffer = malloc(1024);
	    buffer_end = buffer + 1024;
	}
	fill = buffer + offset;
    }

    *fill = c;
    return fill+1;
}

static void scan_one_header(FILE *file, int c)
{
    char *fill = buffer;
    char *value;

    do {
	if (isupper(c))
	    fill = append(fill, tolower(c));
	else
	    fill = append(fill, c);
	c = getc(file);
    } while (c != ':' && c != EOF && c != '\n');
    fill = append(fill, '\0');

    if (c != ':') {
	fprintf(stderr, "%s:%d: Bogus header: ``%s''\n",
		current_file, line_count, buffer);
	exit(1);
    }

    value = fill;

    while (1) {
	while ((c = getc(file)) == ' ' || c == '\t')
	    ;
	while (c != EOF && c != '\n') {
	    fill = append(fill, c);
	    c = getc(file);
	}
	if (c == EOF)
	    break;
	else
	    line_count++;
	c = getc(file);
	if (c == EOF)
	    break;
	if (c != ' ' && c != '\t') {
	    ungetc(c, file);
	    break;
	}
	fill = append(fill, '\n');
    }
    fill = append(fill, '\0');

    process_header(buffer, value);
}

void read_header(FILE *file)
{
    int c;

    while ((c = getc(file)) != EOF && c != '\n')
	scan_one_header(file, c);
    if (c == '\n')
	line_count++;
}
