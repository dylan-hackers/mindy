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
* This file implements the printer framework.
*
\**********************************************************************/

#include <stdarg.h>

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
        printf("{%s 0x%08lx}", sym_name(debug_name), (uintptr_t)object);
    else
        printf("{0x%08lx}", (uintptr_t)object);

    depth--;
}

/* Doug asks, "Why do we need \n after printing an object?" */
void print(obj_t object)
{
    prin1(object);
    fputc('\n', stdout);
}

static void vvformat(const char *fmt, va_list ap)
{
    int args = count_format_args(fmt);
    obj_t vec = make_vector(args, NULL);
    int i;

    for (i = 0; i < args; i++)
        SOVEC(vec)->contents[i] = va_arg(ap, obj_t);

    vformat(fmt, SOVEC(vec)->contents, args);
}

void format(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vvformat(fmt, ap);
    va_end(ap);
}

int count_format_args(const char *fmt)
{
    const char *ptr;
    int args = 0;

    for (ptr = fmt; *ptr != '\0'; ptr++) {
        if (*ptr == '%') {
            switch (*++ptr) {
              case 'd': case 'D':
              case 'b': case 'B':
              case 'o': case 'O':
              case 'x': case 'X':
              case 'c': case 'C':
              case 's': case 'S':
              case '=':
                args++;
                break;
              case '%':
                break;
              default:
                error("Unknown format directive in error msg: %%%c",
                      int_char(*ptr));
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
  fputc(char_int(obj), stdout);
  return obj;
}

static obj_t dylan_puts(obj_t obj)
{
  fputs(string_chars(obj), stdout);
  return obj;
}

static void dylan_format(struct thread* thread, int nargs)
{
  obj_t* args = thread->sp - nargs;
  obj_t* old_sp;
  obj_t fmt = args[0];

  push_linkage(thread, args);
  check_type(fmt, obj_ByteStringClass);
  vformat(string_chars(fmt), args+1, nargs-1);
  old_sp = pop_linkage(thread);
  thread->sp = old_sp;
  do_return(thread, old_sp, old_sp);
}

static obj_t dylan_fflush()
{
  fflush(stdout);
  return obj_False;
}

/* Refactored code appearing several places
 * in the switch in vformat to these functions (more_arguments and check_and)
 * Douglas M. Auclair, dauclair@hotmail.com */
int more_arguments(int nargs)
{
  if(--nargs < 0) {
    error("Not enough arguments to format");
  }
  return nargs;
}

int check_and_print_number(obj_t obj, int nargs, int base, const char* fmt)
{
  nargs = more_arguments(nargs);
  check_type(obj, obj_IntegerClass);
  if(obj_is_fixnum(obj))
    fprintf(stdout, fmt, fixnum_value(obj));
  else print_bignum(obj, base);
  return nargs;
}


void vformat(const char *fmt, obj_t *args, int nargs)
{
    while (*fmt != '\0') {
        if (*fmt == '%') {
            switch (*++fmt) {
              case 'd':
              case 'D':
                nargs = check_and_print_number(*args, nargs, 10, "%ld");
                args++;
                break;
              case 'b':
              case 'B':
                nargs = more_arguments(nargs);
                check_type(*args, obj_IntegerClass);
                if (obj_is_fixnum(*args))
                    print_number_in_binary(fixnum_value(*args++));
                else
                    print_bignum(*args++, 2);
                break;
              case 'o':
              case 'O':
                nargs = check_and_print_number(*args, nargs, 8, "%lo");
                args++;
                break;
              case 'x':
              case 'X':
                nargs = check_and_print_number(*args, nargs, 16, "%lx");
                args++;
                break;
              case 'c':
              case 'C':
                nargs = more_arguments(nargs);
                check_type(*args, obj_CharacterClass);
                fputc(char_int(*args++), stdout);
                break;
              case '=':
                nargs = more_arguments(nargs);
                prin1(*args++);
                break;
              case 's':
              case 'S':
                /* Gotta somehow have two cases,          */
                /* one for strings and another for errors */
                if (instancep(*args, obj_ByteStringClass)) {
                    nargs = more_arguments(nargs);
                    dylan_puts(*args++);
                }
                else if (instancep(*args, obj_SymbolClass)) {
                    fputs(sym_name(*args++), stdout);
                }
                /* Can't print conditions, because they are defined in */
                /* dylan. */
                else {
                  error("%= is neither a string nor a symbol,"
                        " and so can't be printed with %%s", *args++);
                }
                break;
              case '%':
                putchar('%');
                break;
              default:
                error("Unknown format directive in error msg: %%%c",
                      int_char(*fmt));
              }
        }
        else
            fputc(*fmt, stdout);
        fmt++;
    }
}

/* Init stuff. */

void init_print_functions(void)
{
    define_function("print", list1(obj_ObjectClass), false, obj_False, false,
                    obj_ObjectClass, dylan_print);
    define_function("prin1", list1(obj_ObjectClass), false, obj_False, false,
                    obj_ObjectClass, dylan_prin1);
    define_function("putc", list1(obj_CharacterClass), false, obj_False, false,
                    obj_CharacterClass, dylan_putc);
    define_function("puts", list1(obj_ByteStringClass), false, obj_False,
                    false, obj_ByteStringClass, dylan_puts);
    define_constant("format",
                    make_raw_function("format", list1(obj_ByteStringClass),
                                      true, obj_False, false,
                                      obj_Nil, obj_False, dylan_format));
    define_function("fflush", obj_Nil, false, obj_False, false,
                    obj_ObjectClass, dylan_fflush);
}
