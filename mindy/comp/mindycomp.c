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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/mindycomp.c,v 1.5 1994/04/10 21:09:57 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

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

char *current_file = "<stdin>";

static int nerrors = 0;

void error(int line, char *msg, ...)
{
    va_list ap;

    fprintf(stderr, "%s:%d: error: ", current_file, line);
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    if (msg[strlen(msg)-1] != '\n')
	putc('\n', stderr);

    nerrors++;
}

void warn(int line, char *msg, ...)
{
    va_list ap;

    fprintf(stderr, "%s:%d: warning: ", current_file, line);
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    if (msg[strlen(msg)-1] != '\n')
	putc('\n', stderr);
}


static void usage(void)
{
    fprintf(stderr, "usage: mindycomp [-d[p][e]] [-l library-name] "
	    "[-o object-name] [-p] source-name\n");
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

    /* This hack is necessary because flex ignores everything in the */
    /* stdio buffer, so we can't have touched the stream before calling */
    /* yyparse. */
    file = fopen(source_name, "r");
    read_header(file);
    fseek(yyin, ftell(file), 0);
    fclose(file);

    current_file = source_name;

    if (ModuleName == NULL) {
	warn(line_count-1, "no module: header, assuming Dylan-User\n");
	ModuleName = symbol("Dylan-User");
    }

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

    file = fopen(output_name, "w");
    if (file == NULL) {
	perror(output_name);
	exit(1);
    }

    dump_setup_output(source_name, file, ParseOnly ? "parse" : "compilation");

    /* Generate code. */
    if (ParseOnly)
	dump_program(Program);
    else
	compile(Program);

    dump_finalize_output();

    fclose(file);

    exit(0);
}
