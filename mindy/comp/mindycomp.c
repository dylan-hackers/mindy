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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/mindycomp.c,v 1.10 1994/11/10 20:22:19 nkramer Exp $
*
* This file is the main driver.
*
\**********************************************************************/

#include "../compat/std-c.h"
#include "../compat/std-os.h"

#include "mindycomp.h"
#include "parser.h"
#include "src.h"
#include "print.h"
#include "expand.h"
#include "envanal.h"
#include "lexer.h"
#include "header.h"
#include "sym.h"
#include "info.h"
#include "compile.h"
#include "dump.h"

struct body *Program = NULL;

struct symbol *LibraryName = NULL;
struct symbol *ModuleName = NULL;
boolean ParseOnly = FALSE;
boolean GiveWarnings = TRUE;

char *current_file = "<stdin>";

static int nerrors = 0;

static void verror(int line, char *msg, va_list ap)
{
    fprintf(stderr, "%s:%d: error: ", current_file, line);
    vfprintf(stderr, msg, ap);
    if (msg[strlen(msg)-1] != '\n')
	putc('\n', stderr);

    nerrors++;
}
#if _USING_PROTOTYPES_
void error(int line, char *msg, ...)
{
    va_list ap;

    va_start(ap, msg);
    verror(line, msg, ap);
    va_end(ap);
}
#else
void error(va_alist) va_dcl
{
    va_list ap;
    int line;
    char *msg;

    va_start(ap);
    line = va_arg(ap, int);
    msg = va_arg(ap, char *);
    verror(line, msg, ap);
    va_end(ap);
}
#endif

static void vwarn(int line, char *msg, va_list ap)
{
    if ( ! GiveWarnings)
        return;
    fprintf(stderr, "%s:%d: warning: ", current_file, line);
    vfprintf(stderr, msg, ap);
    if (msg[strlen(msg)-1] != '\n')
	putc('\n', stderr);
}

#if _USING_PROTOTYPES_
void warn(int line, char *msg, ...)
{
    va_list ap;

    va_start(ap, msg);
    vwarn(line, msg, ap);
    va_end(ap);
}
#else
void warn(va_alist) va_dcl
{
    va_list ap;
    int line;
    char *msg;

    va_start(ap);
    line = va_arg(ap, int);
    msg = va_arg(ap, char *);
    vwarn(line, msg, ap);
    va_end(ap);
}
#endif

static void usage(void)
{
    fprintf(stderr, "usage: mindycomp [-d[p][e]] [-l library-name] "
	    "[-o object-name] [-p] source-name\n");
    fprintf(stderr, "or:    mindycomp -x source-name [args ...]\n");
    exit(1);
}

static void set_module(char *value)
{
    if (ModuleName) {
	fprintf(stderr, "multiple module: file headers.\n");
	exit(1);
    }
    ModuleName = symbol(value);
}

static int case_insensitive_equal (const char *s1, const char *s2)
{
    char *p1=s1;
    char *p2=s2;
    for ( ; *p1 != 0 && *p2 != 0; p1++, p2++) {
	if (toupper(*p1) != toupper(*p2)) {
	    return 0;
	}
    }
    return (*p1 == *p2);
}

static void set_library(char *value)
{
    if (LibraryName && !case_insensitive_equal(LibraryName->name, value)) {
	fprintf(stderr,
		"Library name specified on the command line differs from\n"
		"the library name specified in the file.\n");
	exit(1);
    }
    free(LibraryName);
    LibraryName = symbol(value);
}

static void end_of_headers(char *value)
{
    if ( ! ModuleName) {
        warn(line_count-1, "no module: header, assuming Dylan-User\n");
	ModuleName = sym_DylanUser;
    }
}

static char *find_extension(char *source)
{
    char *slash = strrchr(source, '/');
    char *dot = strchr(slash ? slash : source, '.');

    if (dot)
	return dot+1;
    else
	return NULL;
}

static char *make_output_name(char *source, char *new_extension)
{
    char *extension = find_extension(source);
    int base_len = extension ? extension - source - 1 : strlen(source);
    char *output = malloc(base_len + strlen(new_extension) + 1);

    memcpy(output, source, base_len);
    strcpy(output + base_len, new_extension);

    return output;
}

void main(int argc, char *argv[])
{
    boolean print_parse = FALSE;
    boolean print_expanded = FALSE;
    char *arg;
    char *source_name = NULL;
    char *output_name = NULL;
    FILE *file;

    add_header_handler("module", set_module);
    add_header_handler("library", set_library);
    add_header_handler(NULL, end_of_headers);

    init_sym_table();
    init_info();
    init_expand();
    init_compile();

    while ((arg = *++argv) != NULL) {
	if (arg[0] == '-') {
	    switch (arg[1]) {
	      case 'd':
		if (arg[2] == '\0') {
		    print_parse = TRUE;
		    print_expanded = TRUE;
		}
		else {
		    char *ptr;
		    for (ptr = arg+2; *ptr != '\0'; ptr++) {
			switch (*ptr) {
			  case 'p':
			    print_parse = TRUE;
			    break;
			  case 'e':
			    print_expanded = TRUE;
			    break;
			  default:
			    fprintf(stderr, "Invalid thing to print: ``%c''\n",
				    *ptr);
			    usage();
			}
		    }
		}
		break;

	      case 'o':
		if (output_name != NULL) {
		    fprintf(stderr, "-o can only be used once.\n");
		    usage();
		}
		else if (arg[2] != '\0')
		    output_name = arg+2;
		else if (*++argv == NULL) {
		    fprintf(stderr, "-o must be followed by the output "
			    "file name.\n");
		    usage();
		}
		else
		    output_name = *argv;
		break;

	      case 'l':
		if (LibraryName != NULL) {
		    fprintf(stderr, "-l can only be used once.\n");
		    usage();
		}
		if (arg[2] != '\0')
		    LibraryName = symbol(arg+2);
		else if (*++argv == NULL) {
		    fprintf(stderr, "-l must be followed by the library "
			    "name.\n");
		    usage();
		}
		else
		    LibraryName = symbol(*argv);
		break;

	      case 'p':
		if (ParseOnly) {
		    fprintf(stderr, "-p can only be used once.\n");
		    usage();
		}
		if (arg[2] != '\0') {
		    fprintf(stderr, "noise after -p switch: %s\n", arg+2);
		    usage();
		}
		ParseOnly = TRUE;
		break;

	      case 'q':
		GiveWarnings = FALSE;
		break;

	      default:
		fprintf(stderr, "Invalid flag: ``%s''\n", arg);
		usage();
	    }
	}
	else if (source_name != NULL) {
	    fprintf(stderr, "Too many files\n");
	    usage();
	}
	else
	    source_name = arg;
    }

    if (source_name == NULL)
	usage();


    yyin = fopen(source_name, "r");
    if (yyin == NULL) {
	perror(source_name);
	exit(1);
    }

    current_file = source_name;

    yyparse();

    if (print_parse) {
	printf("================ Original Parse Tree ================\n");
	print_body(Program, 0);
    }

    if (nerrors != 0)
	exit(1);

    /* Do the various source-to-source expansions. */
    expand(Program);

    if (print_expanded) {
	printf("================ Expanded Parse Tree ================\n");
	print_body(Program, 0);
    }

    if (nerrors != 0)
	exit(1);

    /* Run environment analysis */
    if (!ParseOnly)
	environment_analysis(Program);

    if (nerrors != 0)
	exit(1);

    if (output_name == NULL)
	output_name = make_output_name(source_name,
				       ParseOnly ? ".parse" : ".dbc");


    if (strcmp(output_name, "-") == 0)
        file = stdout;
    else
        file = fopen(output_name, "w");

    if (file == NULL) {
	perror(output_name);
	exit(1);
    }

    dump_setup_output(source_name, file);

    /* Generate code. */
    if (ParseOnly)
	dump_program(Program);
    else
	compile(Program);

    dump_finalize_output();

    fclose(file);

    exit(0);
}
