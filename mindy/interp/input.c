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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/input.c,v 1.1 1994/03/26 07:46:25 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindy.h"
#include "char.h"
#include "list.h"
#include "bool.h"


static obj_t dylan_getc(void)
{
    int c = getchar();

    if (c != EOF)
	return int_char(c);
    else
	return obj_False;
}

void init_input_functions(void)
{
    define_function("getc", obj_Nil, FALSE, obj_False, obj_CharacterClass,
		    dylan_getc);
}
