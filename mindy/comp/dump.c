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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/dump.c,v 1.2 1994/03/25 05:00:19 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>

#include "mindycomp.h"
#include "src.h"
#include "literal.h"
#include "sym.h"
#include "fileops.h"
#include "compile.h"
#include "dump.h"
#include "version.h"
#include "envanal.h"

static FILE *File = NULL;
static int table_index = 0;

static void dump_literal(struct literal *literal);
static void dump_constant(struct constant *c);
static void dump_constant(struct constant *c);


/* Base output routines */

inline static void dump_byte(unsigned byte)
{
    putc(byte, File);
}

#define dump_op dump_byte
#define dump_int1 dump_byte

inline static dump_bytes(void *ptr, int bytes)
{
    int count;

    while (bytes > 0) {
	count = fwrite(ptr, 1, bytes, File);
	ptr += count;
	bytes -= count;
    }
}

inline static void dump_int2(short value)
{
    dump_bytes(&value, 2);
}

inline static void dump_int4(int value)
{
    dump_bytes(&value, 4);
}


/* Table manipulation */

static int implicit_store(void)
{
    return table_index++;
}

static int dump_store(void)
{
    dump_op(fop_STORE);
    return table_index++;
}

static int dump_ref(int handle)
{
    if (handle < (1<<16)) {
	dump_op(fop_SHORT_REF);
	dump_int2(handle);
    }
    else {
	dump_op(fop_REF);
	dump_int4(handle);
    }
}


/* Utility dumpers. */

static void dump_string_guts(int short_op, int long_op, char *str, int length)
{
    if (length < 256) {
	dump_op(short_op);
	dump_byte(length);
    }
    else {
	dump_op(long_op);
	dump_int4(length);
    }
    dump_bytes(str, length);
}

static void dump_integer(long value)
{
    if ((-1<<7) <= value && value < (1<<7)) {
	dump_op(fop_SIGNED_8);
	dump_int1(value);
    }
    else if ((-1<<15) <= value && value < (1<<15)) {
	dump_op(fop_SIGNED_16);
	dump_int2(value);
    }
    else {
	dump_op(fop_SIGNED_32);
	dump_int4(value);
    }
}

static void dump_keyword(struct keyword *keyword)
{
    if (keyword->handle != -1)
	dump_ref(keyword->handle);
    else {
	keyword->handle = implicit_store();
	dump_string_guts(fop_SHORT_KEYWORD, fop_KEYWORD, keyword->name,
			 strlen(keyword->name));
    }
}

static void dump_symbol(struct symbol *symbol)
{
    if (symbol->handle != -1)
	dump_ref(symbol->handle);
    else {
	symbol->handle = implicit_store();
	dump_string_guts(fop_SHORT_SYMBOL, fop_SYMBOL, symbol->name,
			 strlen(symbol->name));
    }
}


/* Literal dumping. */

static void dump_keyword_literal(struct keyword_literal *literal)
{
    dump_keyword(literal->keyword);
}

static void dump_symbol_literal(struct symbol_literal *literal)
{
    dump_symbol(literal->symbol);
}

static void dump_integer_literal(struct integer_literal *literal)
{
    dump_integer(literal->value);
}

static void dump_float_literal(struct float_literal *literal)
{
    lose("### Can't deal with float literals yet.");
}

static void dump_character_literal(struct character_literal *literal)
{
    dump_op(fop_CHAR);
    dump_byte(literal->value);
}

static void dump_string_literal(struct string_literal *literal)
{
    dump_string_guts(fop_SHORT_STRING, fop_STRING, literal->chars,
		     literal->length);
}

static void dump_list_literal(struct list_literal *literal)
{
    struct literal *part;
    int length;
    int i;

    length = 0;
    for (part = literal->first; part != NULL; part = part->next)
	length++;

    part = literal->first;
    while (length > 255+9) {
	dump_op(fop_DOTTED_LISTN);
	dump_byte(255);
	for (i = 0; i < 255+9; i++) {
	    dump_literal(part);
	    part = part->next;
	}
	length -= 255+9;
    }

    if (literal->tail)
	switch (length) {
	  case 0: lose("Zero element dotted list?\n");
	  case 1: dump_op(fop_DOTTED_LIST1); break;
	  case 2: dump_op(fop_DOTTED_LIST2); break;
	  case 3: dump_op(fop_DOTTED_LIST3); break;
	  case 4: dump_op(fop_DOTTED_LIST4); break;
	  case 5: dump_op(fop_DOTTED_LIST5); break;
	  case 6: dump_op(fop_DOTTED_LIST6); break;
	  case 7: dump_op(fop_DOTTED_LIST7); break;
	  case 8: dump_op(fop_DOTTED_LIST8); break;
	  default:
	    dump_op(fop_DOTTED_LISTN);
	    dump_byte(length - 9);
	    break;
	}
    else
	switch (length) {
	  case 0: dump_op(fop_NIL); break;
	  case 1: dump_op(fop_LIST1); break;
	  case 2: dump_op(fop_LIST2); break;
	  case 3: dump_op(fop_LIST3); break;
	  case 4: dump_op(fop_LIST4); break;
	  case 5: dump_op(fop_LIST5); break;
	  case 6: dump_op(fop_LIST6); break;
	  case 7: dump_op(fop_LIST7); break;
	  case 8: dump_op(fop_LIST8); break;
	  default:
	    dump_op(fop_LISTN);
	    dump_byte(length - 9);
	    break;
	}
    while (part != NULL) {
	dump_literal(part);
	part = part->next;
    }
    if (literal->tail)
	dump_literal(literal->tail);
}

static void dump_vector_literal(struct vector_literal *literal)
{
    struct literal *part;
    int length;
    int i;

    length = 0;
    for (part = literal->first; part != NULL; part = part->next)
	length++;

    switch (length) {
      case 0: dump_op(fop_VECTOR0); break;
      case 1: dump_op(fop_VECTOR1); break;
      case 2: dump_op(fop_VECTOR2); break;
      case 3: dump_op(fop_VECTOR3); break;
      case 4: dump_op(fop_VECTOR4); break;
      case 5: dump_op(fop_VECTOR5); break;
      case 6: dump_op(fop_VECTOR6); break;
      case 7: dump_op(fop_VECTOR7); break;
      case 8: dump_op(fop_VECTOR8); break;
      default:
	dump_op(fop_VECTORN);
	if (length-9 < 254)
	    dump_byte(length-9);
	else if (length-9 < (1<<16)) {
	    dump_byte(254);
	    dump_int2(length-9);
	}
	else {
	    dump_byte(255);
	    dump_int4(length-9);
	}
	break;
    }
    for (part = literal->first; part != NULL; part = part->next)
	dump_literal(part);
}

static void dump_true_literal(struct literal *literal)
{
    dump_op(fop_TRUE);
}

static void dump_false_literal(struct literal *literal)
{
    dump_op(fop_FALSE);
}

static void dump_unbound_literal(struct literal *literal)
{
    dump_op(fop_UNBOUND);
}

static void (*LiteralDumpers[(int)literal_Kinds])() = {
    dump_keyword_literal, dump_symbol_literal, dump_integer_literal,
    dump_float_literal, dump_character_literal, dump_string_literal,
    dump_list_literal, dump_vector_literal, dump_true_literal,
    dump_false_literal, dump_unbound_literal
};

static void dump_literal(struct literal *literal)
{
    (LiteralDumpers[(int)literal->kind])(literal);
}



/* Method Dumping */

static void dump_component(struct component *c)
{
    struct constant *constant;
    struct block *block;
    int bytes;

    if (c->nconstants < 256 && c->bytes < (1<<16)) {
	dump_op(fop_SHORT_COMPONENT);
	dump_byte(c->nconstants);
	dump_int2(c->bytes);
    }
    else {
	dump_op(fop_COMPONENT);
	dump_int4(c->nconstants);
	dump_int4(c->bytes);
    }

    if (c->debug_name)
	dump_literal(c->debug_name);
    else
	dump_op(fop_FALSE);

    for (constant = c->constants; constant != NULL; constant = constant->next)
	dump_constant(constant);

    bytes = 0;
    for (block = c->blocks; block != NULL; block = block->next) {
	int count = block->end - block->bytes;
	dump_bytes(block->bytes, count);
	bytes += count;
    }
    if (bytes != c->bytes)
	lose("Planned on writing %d bytes, but ended up writing %d instead.",
	     c->bytes, bytes);
}

static void dump_method(struct method *method)
{
    struct param_list *params = method->params;
    struct keyword_param *k;
    int param_info, nkeys;
    int nclosure_vars;
    struct closes_over *over;

    if (params->rest_param)
	param_info = 1;
    else
	param_info = 0;
    if (params->allow_keys) {
	nkeys = 0;
	for (k = params->keyword_params; k != NULL; k = k->next)
	    nkeys++;
	param_info = param_info | (nkeys+1)<<1;
    }
	
    nclosure_vars = 0;
    for (over = method->closes_over; over != NULL; over = over->next)
	nclosure_vars++;
    
    if (param_info < 256 && nclosure_vars) {
	dump_op(fop_SHORT_METHOD);
	dump_byte(param_info);
	dump_byte(nclosure_vars);
    }
    else {
	dump_op(fop_METHOD);
	dump_int4(param_info);
	dump_int4(nclosure_vars);
    }

    for (k = params->keyword_params; k != NULL; k = k->next) {
	struct literal_expr *def = (struct literal_expr *)k->def;
	dump_keyword(k->keyword);
	if (def) {
	    if (def->kind != expr_LITERAL)
		lose("non-literal keyword default made it though expand?");
	    dump_literal(def->lit);
	}
	else
	    dump_op(fop_FALSE);
    }

    dump_component(method->component);
}

static void dump_varref(struct id *id, boolean written)
{
    if (id->internal)
	if (written)
	    dump_op(fop_BUILTIN_WRITABLE_VALUE_CELL);
	else
	    dump_op(fop_BUILTIN_VALUE_CELL);
    else
	if (written)
	    dump_op(fop_WRITABLE_VALUE_CELL);
	else
	    dump_op(fop_VALUE_CELL);

    dump_symbol(id->symbol);
}

static void dump_constant(struct constant *c)
{
    switch (c->kind) {
      case constant_LITERAL:
	dump_literal(c->u.literal);
	break;
      case constant_METHODDESC:
	dump_method(c->u.method);
	break;
      case constant_VARREF:
	dump_varref(c->u.varref.id, c->u.varref.written);
	break;
    }
}


/* Defconst and Defvar dumping. */

static void dump_defconst_or_var(struct param_list *params,
				 struct component *initializer)
{
    int count;
    struct param *p;

    count = 0;
    for (p = params->required_params; p != NULL; p = p->next)
	count++;
    if (params->rest_param)
	count++;

    dump_integer(count);
    for (p = params->required_params; p != NULL; p = p->next)
	dump_symbol(p->id->symbol);
    if (params->rest_param)
	dump_symbol(params->rest_param->symbol);

    dump_component(initializer);
}


/* Namespace (module and library) dumping. */


static void dump_defnamespace(struct defnamespace_constituent *c,
			      boolean dump_creates)
{
    struct use_clause *use;

    dump_literal(c->name);
    for (use = c->use_clauses; use != NULL; use = use->next) {
	dump_literal(use->name);
	dump_literal(use->import);
	dump_literal(use->exclude);
	dump_literal(use->prefix);
	dump_literal(use->rename);
	dump_literal(use->export);
    }
    dump_op(fop_FALSE);
    dump_literal(c->exported_literal);
    if (dump_creates)
	dump_literal(c->created_literal);
}


/* Interface to the output file dumper */

void dump_setup_output(char *source, FILE *file)
{
    struct stat buf;
    struct timeval tv;

    File = file;

    fprintf(File, "# compilation of %s\n", source);
    if (stat(source, &buf) >= 0)
	fprintf(File, "# last modified on %s", ctime(&buf.st_mtime));
    fprintf(File, "# compiled with the %s version of mindycomp\n", Version);
    gettimeofday(&tv, NULL);
    fprintf(File, "# at %s", ctime(&tv.tv_sec));

    dump_op(fop_BYTE_ORDER);
    dump_int2(1);
    dump_op(fop_IN_LIBRARY);
    if (LibraryName)
	dump_symbol(LibraryName);
    else
	dump_symbol(symbol("Dylan-User"));
    dump_op(fop_IN_MODULE);
    dump_symbol(ModuleName);
}

void dump_top_level_form(struct component *c)
{
    dump_op(fop_TOP_LEVEL_FORM);
    dump_component(c);
}

void dump_defmethod(struct id *name, struct component *c)
{
    dump_op(fop_DEFINE_METHOD);
    dump_symbol(name->symbol);
    dump_component(c);
}

void dump_defgeneric(struct id *name, struct component *tlf)
{
    dump_op(fop_DEFINE_GENERIC);
    dump_symbol(name->symbol);
    dump_component(tlf);
}

void dump_defclass(struct id *name, struct slot_spec *slots,
		   struct component *tlf)
{
    struct slot_spec *slot;

    dump_op(fop_DEFINE_CLASS);
    dump_symbol(name->symbol);
    for (slot = slots; slot != NULL; slot = slot->next) {
	dump_symbol(slot->getter->symbol);
	if (slot->setter)
	    dump_symbol(slot->setter->symbol);
    }
    dump_op(fop_FALSE);
    dump_component(tlf);
}

void dump_defconst(struct param_list *params, struct component *initializer)
{
    dump_op(fop_DEFINE_CONSTANT);
    dump_defconst_or_var(params, initializer);
}

void dump_defvar(struct param_list *params, struct component *initializer)
{
    dump_op(fop_DEFINE_VARIABLE);
    dump_defconst_or_var(params, initializer);
}

void dump_defmodule(struct defnamespace_constituent *c)
{
    dump_op(fop_DEFINE_MODULE);
    dump_defnamespace(c, TRUE);
}

void dump_deflibrary(struct defnamespace_constituent *c)
{
    dump_op(fop_DEFINE_LIBRARY);
    dump_defnamespace(c, FALSE);
}

void dump_finalize_output(void)
{
    dump_op(fop_DONE);
}
