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
* $Header: /scm/cvs/src/mindy/comp/header.c,v 1.1 1998/05/03 19:55:07 andreas Exp $
*
* This file deals with the file headers.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "header.h"

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

void process_header(char *key, char *value)
{
    struct header_handler *handler;

    for (handler = handlers; handler != NULL; handler = handler->next) {
	if (handler->key == key
	 || (handler->key != NULL && strcasecmp(key, handler->key) == 0)) {
	    (*handler->func)(value);
	    return;
	}
    }
}
