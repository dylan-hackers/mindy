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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/print.c,v 1.4 1994/06/11 02:23:42 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdarg.h>
#include <stdio.h>

#include "mindy.h"
#include "obj.h"
#include "class.h"
#include "bool.h"
#include "list.h"
#include "print.h"
#include "vec.h"
#include "char.h"
#include "str.h"
#include "thread.h"
#include "func.h"
#include "def.h"
#include "sym.h"
#include "error.h"
#include "num.h"
#include "type.h"

void def_printer(obj_t class, void (*print_fn)(obj_t object))
{
    obj_ptr(struct class *, class)->print = print_fn;
}

static int depth = 0;

void prin1(obj_t object)
{
    obj_t class = object_class(object);
    obj_t cpl = obj_ptr(struct class *, class)->cpl;
    obj_t debug_name;

    if (depth > 10) {
	putchar('#');
	return;
    }

    depth++;

    if (cpl) {
	while (cpl != obj_Nil) {
	    void (*print_fn)(obj_t object)
		= obj_ptr(struct class *, HEAD(cpl))->print;

	    if (print_fn != NULL) {
		print_fn(object);
		depth--;
		return;
	    }

	    cpl = TAIL(cpl);
	}
    }

    debug_name = obj_ptr(struct class *, class)->debug_name;
    if (debug_name != NULL && debug_name != obj_False)
	printf("{%s 0x%08lx}", sym_name(debug_name), (unsigned long)object);
    else
	printf("{0x%08lx}", (unsigned long)object);

    depth--;
}

void print(obj_t object)
{
    prin1(object);
    putchar('\n');
}

void format(char *fmt, ...)
{
    int args = count_format_args(fmt);
    obj_t vec = make_vector(args, NULL);
    int i;
    va_list ap;

    va_start(ap, fmt);
    for (i = 0; i < args; i++)
	SOVEC(vec)->contents[i] = va_arg(ap, obj_t);
    va_end(ap);

    vformat(fmt, SOVEC(vec)->contents);
}

int count_format_args(char *fmt)
{
    char *ptr;
    int args = 0;

    for (ptr = fmt; *ptr != '\0'; ptr++) {
	if (*ptr == '%') {
	    switch (*++ptr) {
              case 'd':
	      case 'b':
	      case 'o':
	      case 'x':
	      case 'c':
	      case 's':
	      case '=':
		args++;
		break;
	      case '%':
		break;
	      default:
		/* This may need to be a Dylan character, not a C character. */
		error("Unknown format directive in error msg: %%%c", *ptr);
	    }
	}
    }

    return args;
}

/* Works only for numbers greater than 0. If zero, prints nothing. */

void print_nonzero_in_binary(int number)
{
    if (number != 0) {
	print_nonzero_in_binary(number >> 1);
	fputc('0' + (number & 1), stdout);  /* Extract the low bit 
                                               and convert to ASCII */
    }
}

void print_number_in_binary(int number)
{
    if (number == 0)
	fputc('0', stdout);
    else
	print_nonzero_in_binary(number);
}


void vformat(char *fmt, obj_t *args)
{
    while (*fmt != '\0') {
	if (*fmt == '%') {
	    switch (*++fmt) {
              case 'd':
		check_type(*args, obj_IntegerClass);
		fprintf(stdout, "%d", fixnum_value(*args++));
		break;
              case 'b':
		check_type(*args, obj_IntegerClass);
		print_number_in_binary(fixnum_value(*args++));
		break;
              case 'o':
		check_type(*args, obj_IntegerClass);
		fprintf(stdout, "%o", fixnum_value(*args++));
		break;
              case 'x':
		check_type(*args, obj_IntegerClass);
		fprintf(stdout, "%x", fixnum_value(*args++));
		break;
	      case 'c':
		check_type(*args, obj_CharacterClass);
		fputc(char_int(*args++), stdout);
		break;
	      case '=':
		prin1(*args++);
		break;
	      case 's':		/* Gotta somehow have two cases,          */
				/* one for strings and another for errors */
		if (instancep(*args, obj_ByteStringClass)) {
		  fputs(string_chars(*args++), stdout);
		}
/* This will have to be commented out until we figure out how 
   to print conditions. */
/*		else if (instancep(*args, obj_ConditionClass)) {
		  fputs(conditionthing, *args++);
		}
*/
		else {
		  error("%= is neither a string nor a condition,"
			" and so can't be printed with %%s", *args++);
		}
		break;
	      case '%':
		putchar('%');
		break;
	      default:
		/* This may need to be a Dylan character, not a C character. */
		error("Unknown format directive in error msg: %%%c", *fmt);
	      }
	}
	else
	    putchar(*fmt);
	fmt++;
    }
}


/* Dylan routines */

static obj_t dylan_print(obj_t obj)
{
    print(obj);
    return obj;
}

static obj_t dylan_prin1(obj_t obj)
{
    prin1(obj);
    return obj;
}

static obj_t dylan_putc(obj_t obj)
{
    putchar(char_int(obj));
    return obj;
}

static obj_t dylan_puts(obj_t obj)
{
    fputs(string_chars(obj), stdout);
    return obj;
}

static void dylan_format(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - nargs;
    obj_t *old_sp;
    obj_t fmt = args[0];

    push_linkage(thread, args);

    check_type(fmt, obj_ByteStringClass);

    vformat(string_chars(fmt), args+1);

    old_sp = pop_linkage(thread);
    thread->sp = old_sp;

    do_return(thread, old_sp, old_sp);
}


/* Init stuff. */

void init_print_functions(void)
{
    define_function("print", list1(obj_ObjectClass), FALSE, obj_False, FALSE,
		    obj_ObjectClass, dylan_print);
    define_function("prin1", list1(obj_ObjectClass), FALSE, obj_False, FALSE,
		    obj_ObjectClass, dylan_prin1);
    define_function("putc", list1(obj_CharacterClass), FALSE, obj_False, FALSE,
		    obj_CharacterClass, dylan_putc);
    define_function("puts", list1(obj_ByteStringClass), FALSE, obj_False,
		    FALSE, obj_ByteStringClass, dylan_puts);
    define_constant("format",
		    make_raw_function("format", 1, TRUE, obj_False, FALSE,
				      obj_Nil, obj_False, dylan_format));
}
