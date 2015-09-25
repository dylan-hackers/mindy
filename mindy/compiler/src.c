/**********************************************************************\
*
*  Copyright (c) 1994  Carnegie Mellon University
*  Copyright (c) 1998, 1999, 2000, 2001, 2002  Gwydion Dylan Maintainers
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
* $Header: /scm/cvs/src/mindy/comp/src.c,v 1.4 2002/08/27 21:34:18 gabor Exp $
*
* This file implements the various nodes in the parse tree.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindycomp.h"
#include "sym.h"
#include "lexer.h"
#include "literal.h"
#include "src.h"
#include "info.h"
#include "lose.h"

struct local_methods {
    struct method *head;
    struct method **tail;
};

struct binop_series {
    struct binop *head;
    struct binop **tail;
};

struct arglist {
    struct argument *head;
    struct argument **tail;
};

struct block_epilog {
    struct exception_clause *inner;
    struct body *cleanup;
    struct exception_clause *outer;
};

struct incomplete_condition_body {
    struct constituent *constituents;
    struct condition_body *rest;
};

struct exception_clauses {
    struct exception_clause *head;
    struct exception_clause **tail;
};

struct superclass_list {
    struct superclass *head;
    struct superclass **tail;
};

struct for_header {
    struct for_clause *clauses;
    struct expr *until;
};

struct gf_suffix {
    struct return_type_list *rettypes;
    struct plist *plist;
};

struct to_part {
    enum to_kind kind;
    struct expr *expr;
};

struct class_guts {
    struct slot_spec *slots;
    struct slot_spec **slots_tail;
    struct initarg_spec *initargs;
    struct initarg_spec **initargs_tail;
    struct inherited_spec *inheriteds;
    struct inherited_spec **inheriteds_tail;
};

struct else_part {
    int else_line;
    struct body *alternate;
};

struct body *make_body(void)
{
    struct body *res = malloc(sizeof(struct body));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct body
    *add_constituent(struct body *body, struct constituent *constituent)
{
    if (constituent->kind == constituent_EXPR) {
	struct expr *expr = ((struct expr_constituent *)constituent)->expr;
	if (expr->kind == expr_BODY) {
	    struct body *insides = ((struct body_expr *)expr)->body;
	    struct constituent *c, **prev;

	    *body->tail = insides->head;
	    /* Note: we can't use insides->tail because that will point */
	    /* inside the bindings established inside this block */
	    for (prev = body->tail; (c = *prev) != NULL; prev = &c->next)
		;
	    body->tail = prev;

	    free(insides);
	    free(expr);
	    free(constituent);

	    return body;
	}
    }
	    
    *body->tail = constituent;

    switch (constituent->kind) {
      case constituent_LET:
      case constituent_LOCAL:
      case constituent_HANDLER:
	body->tail = ((struct binding_constituent *)constituent)->body->tail;
	((struct binding_constituent *)constituent)->body->tail = NULL;
	if (constituent->next)
	  lose("constituent has next when deep-nesting?");
	break;
      default:
	body->tail = &constituent->next;
	break;
    }

    return body;
}

struct body *make_expr_body(struct expr *expr)
{
    return add_constituent(make_body(), make_expr_constituent(expr));
}

struct constituent *make_define_constant(int line, struct bindings *bindings)
{
    struct defconst_constituent *res
	= malloc(sizeof(struct defconst_constituent));

    res->kind = constituent_DEFCONST;
    res->next = NULL;
    res->line = line;
    res->bindings = bindings;
    res->tlf = NULL;

    return (struct constituent *)res;
}

struct constituent *make_define_method(flags_t flags, struct method *method)
{
    struct defmethod_constituent *res
	= malloc(sizeof(struct defmethod_constituent));

    res->kind = constituent_DEFMETHOD;
    res->next = NULL;
    res->flags = flags;
    res->method = method;
    res->tlf = NULL;

    return (struct constituent *)res;
}

/* Implement "define function foo" as "define constant foo = method ..."
 */
struct constituent *make_define_function(flags_t ignored, 
					 struct method *method)
{
    struct param *func_name = make_param(method->name, NULL);
    struct defconst_constituent *res
	= malloc(sizeof(struct defconst_constituent));

    res->kind = constituent_DEFCONST;
    res->next = NULL;
    res->line = method->line;
    res->bindings = make_bindings(push_param(func_name, make_param_list()), 
				  make_method_ref(method));
    res->tlf = NULL;

    return (struct constituent *)res;
}

struct constituent *make_define_variable(int line, struct bindings *bindings)
{
    struct defvar_constituent *res = malloc(sizeof(struct defvar_constituent));

    res->kind = constituent_DEFVAR;
    res->next = NULL;
    res->line = line;
    res->bindings = bindings;
    res->tlf = NULL;

    return (struct constituent *)res;
}

struct constituent *make_expr_constituent(struct expr *expr)
{
    struct expr_constituent *res = malloc(sizeof(struct expr_constituent));

    res->kind = constituent_EXPR;
    res->next = NULL;
    res->expr = expr;

    return (struct constituent *)res;
}

struct constituent *make_let(struct bindings *bindings)
{
    struct let_constituent *res	= malloc(sizeof(struct let_constituent));

    res->kind = constituent_LET;
    res->next = NULL;
    res->body = make_body();
    res->offset = 0;
    res->bindings = bindings;
    res->required = 0;
    res->lexenv = NULL;
    res->inside = NULL;

    return (struct constituent *)res;
}

struct constituent
    *make_handler(struct expr *type, struct expr *func, struct plist *plist)
{
    struct handler_constituent *res
	= malloc(sizeof(struct handler_constituent));

    res->kind = constituent_HANDLER;
    res->next = NULL;
    res->body = make_body();
    res->type = type;
    res->func = func;
    res->plist = plist;

    return (struct constituent *)res;
}

struct constituent *make_local_constituent(struct local_methods *methods)
{
    struct local_constituent *res
	= malloc(sizeof(struct local_constituent));

    res->kind = constituent_LOCAL;
    res->next = NULL;
    res->body = make_body();
    res->offset = 0;
    res->methods = methods->head;
    res->lexenv = NULL;

    free(methods);

    return (struct constituent *)res;
}

struct constituent
    *make_top_level_form(char *debug_name, struct constituent *c)
{
    struct tlf_constituent *res = malloc(sizeof(struct tlf_constituent));

    c->next = NULL;

    res->kind = constituent_TOPLEVELFORM;
    res->next = NULL;
    res->form
	= make_top_level_method(debug_name, add_constituent(make_body(), c));

    return (struct constituent *)res;
}

struct expr *make_varref(struct id *var)
{
    struct varref_expr *res = malloc(sizeof(struct varref_expr));

    res->kind = expr_VARREF;
    res->analyzed = FALSE;
    res->var = var;
    res->home = NULL;
    res->binding = NULL;
    res->over = NULL;

    return (struct expr *)res;
}

struct expr *make_varset(struct id *var, struct expr *expr)
{
    struct varset_expr *res = malloc(sizeof(struct varset_expr));

    res->kind = expr_VARSET;
    res->analyzed = FALSE;
    res->var = var;
    res->home = NULL;
    res->binding = NULL;
    res->over = NULL;
    res->value = expr;
    res->type = NULL;

    return (struct expr *)res;
}

struct id *id(struct symbol *symbol)
{
    struct id *res = malloc(sizeof(struct id));

    res->symbol = symbol;
    res->internal = TRUE;
    res->line = 0;

    return res;
}

struct id *dup_id(struct id *id)
{
    struct id *res = malloc(sizeof(*res));

    memcpy(res, id, sizeof(*res));
    res->line = 0;

    return res;
}

struct id *make_id(struct token *token)
{
    char *ptr = (char *)token->chars;
    struct id *res;

    if (*ptr == '\\')
	ptr++;

    res = id(symbol(ptr));
    res->internal = FALSE;
    res->line = token->line;

    free(token);

    return res;
}

struct bindings *make_bindings(struct param_list *params, struct expr *expr)
{
    struct bindings *res = malloc(sizeof(struct bindings));

    res->params = params;
    res->expr = expr;

    return res;
}

struct param_list *make_param_list(void)
{
    struct param_list *res = malloc(sizeof(struct param_list));

    res->required_params = NULL;
    res->next_param = NULL;
    res->rest_param = NULL;
    res->allow_keys = FALSE;
    res->all_keys = FALSE;
    res->keyword_params = NULL;

    return res;
}

struct param_list *push_param(struct param *param, struct param_list *list)
{
    param->next = list->required_params;
    list->required_params = param;

    return list;
}

struct param_list *set_next_param(struct param_list *list, struct id *var)
{
    list->next_param = var;

    return list;
}

struct param_list *set_rest_param(struct param_list *list, struct id *var)
{
    list->rest_param = var;

    return list;
}

struct param *make_param(struct id *id, struct expr *type)
{
    struct param *res = malloc(sizeof(struct param));

    res->id = id;
    res->type = type;
    res->type_temp = NULL;
    res->next = NULL;

    return res;
}

struct param_list
    *push_keyword_param(struct keyword_param *param, struct param_list *list)
{
    param->next = list->keyword_params;
    list->keyword_params = param;

    return list;
}

struct param_list *allow_keywords(struct param_list *param_list)
{
    param_list->allow_keys = TRUE;

    return param_list;
}

struct param_list *allow_all_keywords(struct param_list *param_list)
{
    param_list->allow_keys = TRUE;
    param_list->all_keys = TRUE;

    return param_list;
}

struct keyword_param
    *make_keyword_param(struct token *key, struct id *sym, struct expr *type,
			struct expr *def)
{
    struct keyword_param *res = malloc(sizeof(struct keyword_param));

    if (key) {
	/* The keyword token has a trailing : */
	key->chars[key->length-1] = '\0';
	res->keyword = symbol((char *)key->chars);
	free(key);
    }
    else
	res->keyword = sym->symbol;

    res->id = sym;
    res->type = type;
    res->type_temp = NULL;
    res->def = def;
    res->next = NULL;

    return res;
}

struct local_methods
    *add_local_method(struct local_methods *methods, struct method *method)
{
    *methods->tail = method;
    methods->tail = &method->next_local;

    return methods;
}

struct local_methods *make_local_methods(void)
{
    struct local_methods *res = malloc(sizeof(struct local_methods));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct expr *make_literal_ref(struct literal *lit)
{
    struct literal_expr *res = malloc(sizeof(struct literal_expr));

    res->kind = expr_LITERAL;
    res->analyzed = FALSE;
    res->lit = lit;

    return (struct expr *)res;
}

struct expr *make_binop_series_expr(struct expr *operand,
				    struct binop_series *series)
{
    if (series->head) {
	struct binop_series_expr *res
	    = malloc(sizeof(struct binop_series_expr));

	res->kind = expr_BINOP_SERIES;
	res->analyzed = FALSE;
	res->first_operand = operand;
	res->first_binop = series->head;

	free(series);

	return (struct expr *)res;
    }
    else {
	free(series);
	return operand;
    }
}

struct binop_series *make_binop_series(void)
{
    struct binop_series *res = malloc(sizeof(struct binop_series));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct binop_series
    *add_binop(struct binop_series *series, struct binop *op,
	       struct expr *operand)
{
    *series->tail = op;
    series->tail = &op->next;
    op->operand = operand;

    return series;
}

struct binop *make_binop(struct id *id)
{
    struct binop *res = malloc(sizeof(struct binop));
    struct binop_info *info = lookup_binop_info(id);

    res->op = id;
    res->operand = NULL;
    res->precedence = info->precedence;
    res->left_assoc = info->left_assoc;
    res->next = NULL;

    return res;
}

static struct expr *make_unary_fn_call(struct expr *fn, struct expr *arg)
{
    struct arglist *args = make_argument_list();

    add_argument(args, make_argument(arg));

    return make_function_call(fn, args);
}    

struct expr *make_negate(int line, struct expr *expr)
{
    struct id *negative = id(sym_Negative);
    negative->line = line;
    return make_unary_fn_call(make_varref(negative), expr);
}

struct expr *make_not(int line, struct expr *expr)
{
    struct id *not = id(sym_Not);
    not->line = line;
    return make_unary_fn_call(make_varref(not), expr);
}

struct expr *make_singleton(int line, struct expr *expr)
{
    struct id *sing = id(sym_Singleton);
    sing->line = line;
    return make_unary_fn_call(make_varref(sing), expr);
}

struct expr *make_aref_or_element(int line, struct expr *expr,
				  struct arglist *args)
{
    struct argument *collection = make_argument(expr);
    struct id *func;

    collection->next = args->head;
    args->head = collection;
    /* This leaves args->tail wrong, but that doens't matter because */
    /* we just pass it directly to make_function_call */

    if (args->head->next != NULL && args->head->next->next == NULL)
	func = id(sym_Element);
    else
	func = id(sym_Aref);

    func->line = line;
    return make_function_call(make_varref(func), args);
}

struct expr *make_function_call(struct expr *expr, struct arglist *args)
{
    struct call_expr *res = malloc(sizeof(struct call_expr));

    res->kind = expr_CALL;
    res->analyzed = FALSE;
    res->func = expr;
    if (expr->kind == expr_VARREF)
	res->info = lookup_function_info(((struct varref_expr *)expr)->var,
					 FALSE);
    else
	res->info = NULL;
    res->args = args->head;

    free(args);

    return (struct expr *)res;
}

struct expr *make_method_ref(struct method *method)
{
    struct method_expr *res = malloc(sizeof(struct method_expr));

    res->kind = expr_METHOD;
    res->analyzed = FALSE;
    res->method = method;

    return (struct expr *)res;
}

struct expr *make_dot_operation(struct expr *arg, struct expr *func)
{
    struct dot_expr *res = malloc(sizeof(struct dot_expr));

    res->kind = expr_DOT;
    res->analyzed = FALSE;
    res->arg = arg;
    res->func = func;

    return (struct expr *)res;
}

struct arglist *make_argument_list(void)
{
    struct arglist *res = malloc(sizeof(struct arglist));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct arglist *add_argument(struct arglist *arglist, struct argument *arg)
{
    *arglist->tail = arg;
    while (arg->next != NULL)
	arg = arg->next;
    arglist->tail = &arg->next;

    return arglist;
}

struct argument *make_argument(struct expr *expr)
{
    struct argument *res = malloc(sizeof(struct argument));

    res->expr = expr;
    res->next = NULL;

    return res;
}

struct argument
    *make_keyword_argument(struct token *keyword, struct expr *expr)
{
    struct argument *keyarg
	= make_argument(make_literal_ref(parse_keyword_token(keyword)));

    keyarg->next = make_argument(expr);

    return keyarg;
}

struct plist *make_property_list(void)
{
    struct plist *res = malloc(sizeof(struct plist));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct plist
    *add_property(struct plist *plist, struct token *key, struct expr *expr)
{
    struct property *prop = malloc(sizeof(struct property));

    /* The keyword token has a trailing : */
    key->chars[key->length-1] = '\0';

    prop->line = key->line;
    prop->keyword = symbol((char *)key->chars);
    prop->expr = expr;
    prop->next = NULL;

    *plist->tail = prop;
    plist->tail = &prop->next;

    free(key);

    return plist;
}

struct return_type_list *make_return_type_list(boolean restp,
					       struct expr *rest)
{
    struct return_type_list *res = malloc(sizeof(struct return_type_list));

    res->req_types = NULL;
    res->req_types_tail = &res->req_types;
    res->req_types_list = NULL;
    res->restp = restp;
    res->rest_type = rest;
    res->rest_temp = NULL;
    res->rest_temp_varref = NULL;

    return res;
}

struct return_type_list *add_return_type(struct return_type_list *list,
					 struct expr *type)
{
    struct return_type *rtype = malloc(sizeof(struct return_type));

    rtype->type = type;
    rtype->temp = NULL;
    rtype->next = NULL;
    *list->req_types_tail = rtype;
    list->req_types_tail = &rtype->next;

    return list;
}

struct return_type_list
    *set_return_type_rest_type(struct return_type_list *list,
			       struct expr *type)
{
    list->restp = TRUE;
    list->rest_type = type;
    return list;
}

struct literal *parse_true_token(struct token *token)
{
    struct literal *res = make_true_literal();
    res->line = token->line;
    free(token);
    return res;
}

struct literal *parse_false_token(struct token *token)
{
    struct literal *res = make_false_literal();
    res->line = token->line;
    free(token);
    return res;
}

struct literal *parse_unbound_token(struct token *token)
{
    struct literal *res = make_unbound_literal();
    res->line = token->line;
    free(token);
    return res;
}

static int unicode_escape(char **c, int line)
{
    char *term;
    long  character;

    character = strtol(*c, &term, 16);
    if (((character == 0) && (term == *c)) || (character > 255))
        error(line, "invalid Unicode escape \\<%x>.", character);
    *c = term + 1;
    return character;
}

/* the semantics of this function are different than in previous
 * releases: before any character prefixed by '\' that was not
 * recognized as a valid escape was simply passed through untouched.
 * now it actually recognizes only the set of escapes defined in
 * the DRM. */
static int escape_char(char **c, int line)
{
    switch (*(*c)++) {
    case 'a': return '\007';
    case 'b': return '\b';
    case 'e': return '\033';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case '0': return '\0';
    case '\\': return '\\';
    case '"': return '"';
    case '\'': return '\'';
    case '<' : return unicode_escape(c, line);
    default:
        /* we should never get here since the lexer will barf long before */
        error(line, "invalid escape sequence, \\%c", *((*c)-1));
        return 0; /* silence compiler warning */
    }
}

struct literal *parse_string_token(struct token *token)
{
    struct string_literal *res;
    int length = token->length - 2;
    int i;
    char *src, *dst;

    src = (char *)token->chars + 1;
    for (i = length; i > 0; i--) {
        if (*src++ == '\\') {
            length--, i--;
            if (*src != '<') {
                src++;
            } else {
                while ((*src++ != '>') && (i > 0))
                    i--, length--;
            }
        }
    }

    res = malloc(sizeof(struct string_literal) 
		 + length + 1 - sizeof(res->chars));

    res->kind = literal_STRING;
    res->next = NULL;
    res->line = token->line;
    res->length = length;

    src = (char *)token->chars + 1;
    dst = (char *)res->chars;
    for (i = length; i > 0; i--) {
	int c = *src++;
	if (c == '\\')
	    *dst++ = escape_char(&src, res->line);
	else
	    *dst++ = c;
    }
    *dst++ = '\0';

    free(token);

    return (struct literal *)res;
}

struct literal
    *concat_string_token(struct literal *old_literal, struct token *token)
{
    struct string_literal *old = (struct string_literal *)old_literal;
    int old_length = old->length;
    char *old_string = (char *)old->chars;
    struct string_literal *res;
    int length = token->length - 2;
    int i;
    char *src, *dst;

    res = malloc(sizeof(struct string_literal)
		 + old_length + length + 1 - sizeof(res->chars));

    res->kind = literal_STRING;
    res->next = NULL;
    res->line = old_literal->line;

    strncpy((char *)res->chars, old_string, old_length);
    src = (char *)token->chars + 1;
    dst = (char *)res->chars + old_length;
    for (i = 0; i < length; i++) {
        int c = *src++;
        if (c == '\\') {
            char *t = src;
            *dst++ = escape_char(&src, res->line);
            length -= src - t;
	} else
	    *dst++ = c;
    }
    *dst++ = '\0';

    res->length = length + old_length;
    free(token);

    return (struct literal *)res;
}

struct literal *parse_character_token(struct token *token)
{
    int c = token->chars[1];
    struct literal *res;

    if (c == '\\') {
        char *cp = &token->chars[2];
        c = escape_char(&cp, token->line);
    }

    res = make_character_literal(c);
    res->line = token->line;

    free(token);

    return res;
}

struct literal *parse_integer_token(struct token *token)
{
    long value;
    int count, radix = 0;
    boolean negative;
    char *ptr, *remnant;
    struct literal *res;

    value = 0;
    count = token->length;
    ptr = (char *)token->chars;
    if (*ptr == '#') {
	switch (ptr[1]) {
	  case 'X': case 'x': radix = 16; break;
	  case 'O': case 'o': radix = 8; break;
	  case 'B': case 'b': radix = 2; break;
	  default:
	    lose("Strange radix marker ('%c') in integer literal", ptr[1]);
	}
	ptr += 2;
	count -= 2;
	negative = FALSE;
    }
    else {
	radix = 10;
	if (*ptr == '-') {
	    negative = TRUE;
	    count--;
	    ptr++;
	}
	else {
	    negative = FALSE;
	    if (*ptr == '+') {
		count--;
		ptr++;
	    }
	}
    }
    if (radix == 0)
	lose("No radix in integer literal?");

    value = strtoul(ptr, &remnant, radix);
    if (negative)
      value = -value;

    if (remnant == ptr)
      lose("Integer literal did not convert: %s\n", token->chars);

    if (*remnant != 0)
      lose("Integer literal did not convert completely: %s left %s\n", token->chars, remnant);
    
    res = make_integer_literal(value);
    res->line = token->line;

    free(token);

    return res;
}

struct literal *parse_float_token(struct token *token)
{
    unsigned char c, *ptr, *remnant;
    enum literal_kind kind = literal_SINGLE_FLOAT;
    struct literal *res = NULL;

    for (ptr = token->chars; (c = *ptr) != '\0'; ptr++) {
	if (c == 'e' || c == 'E')
	    break;
	if (c == 's' || c == 'S') {
	    *ptr = 'e';
	    break;
	}
	if (c == 'd' || c == 'D') {
	    *ptr = 'e';
	    kind = literal_DOUBLE_FLOAT;
	    break;
	}
	if (c == 'x' || c == 'X') {
	    *ptr = 'e';
	    kind = literal_EXTENDED_FLOAT;
	    break;
	}
    }

    switch (kind) {
      case literal_SINGLE_FLOAT:
	{
	    struct single_float_literal *r = malloc(sizeof(*r));
	    res = (struct literal *)r;
	    r->value = strtod((char *)token->chars, (char **)&remnant);
	    break;
	}
      case literal_DOUBLE_FLOAT:
	{
	    struct double_float_literal *r = malloc(sizeof(*r));
	    res = (struct literal *)r;
	    r->value = strtod((char *)token->chars, (char **)&remnant);
	    break;
	}
      case literal_EXTENDED_FLOAT:
	{
	    struct extended_float_literal *r = malloc(sizeof(*r));
	    res = (struct literal *)r;
	    r->value = strtod((char *)token->chars, (char **)&remnant);
	    break;
	}
      default:
	lose("Strange float literal kind.\n");
	break;
    }

    if (remnant == token->chars)
      lose("Float literal did not convert: %s\n", token->chars);

    if (*remnant != 0)
      lose("Float literal did not completely convert: %s left %s\n", token->chars, remnant);

    /* Other possible errors would be indicated by errno == ERANGE:
     * a result value of +/- HUGE_VAL returned indicates overflow,
     * a result value of 0 returned indicates underflow. */

    res->kind = kind;
    res->next = NULL;
    res->line = token->line;

    free(token);

    return res;
}

struct literal *parse_symbol_token(struct token *token)
{
    char *ptr = (char *)token->chars;
    struct literal *res;

    /* We modify the token here, but we don't care 'cause we will be */
    /* freeing it shortly. */

    if (*ptr == '\\')
	/* They used the \op quoting convention. */
	ptr++;

    res = make_symbol_literal(symbol(ptr));
    res->line = token->line;

    free(token);

    return res;
}

struct literal *parse_keyword_token(struct token *token)
{
    char *ptr = (char *)token->chars, *p = NULL;
    struct literal *res;

    /* We modify the token here, but we don't care 'cause we will be */
    /* freeing it shortly. */

    /* keyword tokens have a trailing : or " */
    ptr[token->length-1] = '\0';

    /* Sometimes they also have a leading #", in which case we need
     * to also process escape sequences in the string. */
    if (*ptr == '#') {
        ptr += 2;
        /* this allocation makes some assumptions: it assumes that the token
         * name will not grow when the escapes are decoded, so there will
         * be enough room in the buffer to hold the string. it also assumes
         * that the buffer is big enough to hold the entire name (in the
         * case where there are no escapes) */
        if ((p = (char *) malloc(token->length * sizeof(char))) != NULL) {
            char *dest = p, c;
            while ((c = *ptr++) != '\0')
                *dest++ = c != '\\' ? c : escape_char(&ptr, token->line);
            *dest = '\0';
            ptr = p;
        }
    }
    res = make_symbol_literal(symbol(ptr));
    res->line = token->line;

    free(token);
    if (p != NULL)
        free(p);

    return res;
}

struct expr *make_body_expr(struct body *body)
{
    if (body->head && body->head->kind == constituent_EXPR
	  && body->head->next == NULL) {
	struct expr *res = ((struct expr_constituent *)body->head)->expr;
	free(body->head);
	free(body);
	return res;
    }
    else {
	struct body_expr *res = malloc(sizeof(struct body_expr));

	res->kind = expr_BODY;
	res->analyzed = FALSE;
	res->body = body;

	return (struct expr *)res;
    }
}

struct expr *make_block(int line, struct id *exit, struct body *body,
			struct block_epilog *epilog)
{
    struct block_expr *res = malloc(sizeof(struct block_expr));

    res->kind = expr_BLOCK;
    res->analyzed = FALSE;
    res->line = line;
    res->exit_fun = exit;
    res->body = body;
    if (epilog) {
	res->inner = epilog->inner;
	res->cleanup = epilog->cleanup;
	res->outer = epilog->outer;
	free(epilog);
    }
    else {
	res->inner = NULL;
	res->cleanup = NULL;
	res->outer = NULL;
    }

    return (struct expr *)res;
}

struct expr *make_case(struct condition_body *body)
{
    struct case_expr *res = malloc(sizeof(struct case_expr));

    res->kind = expr_CASE;
    res->analyzed = FALSE;
    res->body = body;

    return (struct expr *)res;
}

static struct body *make_literal_body(struct literal *literal)
{
    return add_constituent(make_body(),
                           make_expr_constituent
                               (make_literal_ref(literal)));
}

struct expr *make_if(struct expr *cond, struct body *consequent,
		     struct else_part *else_part)
{
    struct if_expr *res = malloc(sizeof(struct if_expr));

    res->kind = expr_IF;
    res->analyzed = FALSE;
    res->cond = cond;
    if (consequent)
	res->consequent = consequent;
    else
	res->consequent = make_literal_body(make_false_literal());
    if (else_part) {
	res->else_line = else_part->else_line;
	res->alternate = else_part->alternate;
	free(else_part);
    }
    else {
	res->else_line = 0;
	res->alternate = make_literal_body(make_false_literal());
    }

    return (struct expr *)res;
}

struct else_part *make_else(int else_line, struct body *alternate)
{
    struct else_part *res = malloc(sizeof(*res));

    res->else_line = else_line;
    res->alternate = alternate;

    return res;
}

struct expr *make_for(struct for_header *header, struct body *body,
			     struct body *finally)
{
    struct for_expr *res = malloc(sizeof(struct for_expr));

    res->kind = expr_FOR;
    res->analyzed = FALSE;
    res->clauses = header->clauses;
    res->until = header->until;
    res->body = body;
    res->finally = finally;

    free(header);

    return (struct expr *)res;
}

struct expr *make_select(struct expr *expr, struct expr *by,
				struct condition_body *body)
{
    struct select_expr *res = malloc(sizeof(struct select_expr));

    res->kind = expr_SELECT;
    res->analyzed = FALSE;
    res->expr = expr;
    res->by = by;
    res->body = body;

    return (struct expr *)res;
}

struct expr *make_loop(struct body *body)
{
    struct loop_expr *res = malloc(sizeof(struct loop_expr));

    res->kind = expr_LOOP;
    res->analyzed = FALSE;
    res->body = body;
    res->position = 0;

    return (struct expr *)res;
}

struct expr *make_repeat(void)
{
    struct repeat_expr *res = malloc(sizeof(struct repeat_expr));

    res->kind = expr_REPEAT;
    res->analyzed = FALSE;
    res->loop = NULL;

    return (struct expr *)res;
}

struct block_epilog *make_block_epilog(struct exception_clauses *inner,
				       struct body *cleanup,
				       struct exception_clauses *outer)
{
    struct block_epilog *res = malloc(sizeof(struct block_epilog));

    if (inner) {
	res->inner = inner->head;
	free(inner);
    }
    else
	res->inner = NULL;
    res->cleanup = cleanup;
    if (outer) {
	res->outer = outer->head;
	free(outer);
    }
    else
	res->outer = NULL;

    return res;
}

struct for_header *make_for_header(struct expr *until)
{
    struct for_header *res = malloc(sizeof(struct for_header));

    res->clauses = NULL;
    res->until = until;

    return res;
}

struct for_header *push_for_clause(struct for_clause *clause,
				   struct for_header *header)
{
    clause->next = header->clauses;
    header->clauses = clause;

    return header;
}

struct exception_clauses *make_exception_clauses(void)
{
    struct exception_clauses *res = malloc(sizeof(struct exception_clauses));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct exception_clauses
    *add_exception_clause(struct exception_clauses *clauses,
			  struct exception_clause *clause)
{
    *clauses->tail = clause;
    clauses->tail = &clause->next;

    return clauses;
}

struct exception_clause
    *make_exception_clause(struct expr *type, struct id *condition,
			   struct plist *plist, struct body *body)
{
    struct exception_clause *res = malloc(sizeof(struct exception_clause));

    res->type = type;
    res->condition = condition;
    res->plist = plist;
    res->body = body;
    res->next = NULL;

    return res;
}

struct condition_body
    *push_condition_clause(struct condition_clause *clause,
			   struct condition_body *cond_body)
{
    struct condition_body *res = malloc(sizeof(struct condition_body));

    res->clause = clause;
    res->next = cond_body;

    return res;
}

struct condition_clause
    *make_otherwise_condition_clause(struct body *body)
{
    struct condition_clause *res = malloc(sizeof(struct condition_clause));

    res->conditions = NULL;
    res->body = body;

    return res;
}

struct incomplete_condition_body
    *make_incomplete_condition_clauses(struct constituent *constituent,
				       struct condition_body *rest)
{
    struct incomplete_condition_body *res
	= malloc(sizeof(struct incomplete_condition_body));

    res->constituents = constituent;
    res->rest = rest;

    return res;
}

struct incomplete_condition_body
    *push_condition_constituent(struct constituent *constituent,
				struct incomplete_condition_body *body)
{
    constituent->next = body->constituents;
    body->constituents = constituent;

    return body;
}

struct condition_body
    *complete_condition_clauses(struct condition_clause *clause,
				struct incomplete_condition_body *body)
{
    struct constituent *constit, *next;
    struct condition_body *res;

    for (constit = body->constituents; constit != NULL; constit = next) {
	next = constit->next;
	constit->next = NULL;
	add_constituent(clause->body, constit);
    }
    res = push_condition_clause(clause, body->rest);

    free(body);

    return res;
}

struct condition_clause
    *make_condition_clause(struct constituent *constituent)
{
    struct condition_clause *res = malloc(sizeof(struct condition_clause));

    res->conditions = NULL;
    res->body = add_constituent(make_body(), constituent);

    return res;
}

struct condition_clause
    *push_condition(struct expr *expr, struct condition_clause *clause)
{
    struct condition *cond = malloc(sizeof(struct condition));

    cond->cond = expr;
    cond->next = clause->conditions;
    clause->conditions = cond;

    return clause;
}

struct for_clause
    *make_equal_then_for_clause(struct param_list *vars, struct expr *equal,
				struct expr *then)
{
    struct equal_then_for_clause *res
	= malloc(sizeof(struct equal_then_for_clause));

    res->kind = for_EQUAL_THEN;
    res->next = NULL;
    res->vars = vars;
    res->equal = equal;
    res->then = then;

    return (struct for_clause *)res;
}

struct for_clause
    *make_in_for_clause(struct param *var, struct param *keyed_by,
			struct expr *collection, struct param *protocol)
{
    struct in_for_clause *res = malloc(sizeof(*res));
    struct param_list *vars = make_param_list();

    if (keyed_by)
	push_param(keyed_by, vars);
    push_param(var, vars);

    res->kind = for_IN;
    res->next = NULL;
    res->vars = vars;
    res->collection = collection;
    res->protocol = protocol;

    return (struct for_clause *)res;
}

struct for_clause
    *make_from_for_clause(struct param *var, struct expr *from,
			  struct to_part *to, struct expr *by)
{
    struct from_for_clause *res
	= malloc(sizeof(struct from_for_clause));

    res->kind = for_FROM;
    res->next = NULL;
    res->vars = push_param(var, make_param_list());
    res->from = from;
    if (to) {
	res->to_kind = to->kind;
	res->to = to->expr;
	free(to);
    }
    else {
	res->to_kind = to_UNBOUNDED;
	res->to = NULL;
    }
    res->by = by;

    return (struct for_clause *)res;
}

struct to_part *make_to(struct expr *expr)
{
    struct to_part *res = malloc(sizeof(struct to_part));

    res->kind = to_TO;
    res->expr = expr;

    return res;
}

struct to_part *make_above(struct expr *expr)
{
    struct to_part *res = malloc(sizeof(struct to_part));

    res->kind = to_ABOVE;
    res->expr = expr;

    return res;
}

struct to_part *make_below(struct expr *expr)
{
    struct to_part *res = malloc(sizeof(struct to_part));

    res->kind = to_BELOW;
    res->expr = expr;

    return res;
}

struct constituent
    *make_class_definition(struct id *name, struct superclass_list *supers,
			   struct class_guts *guts)
{
    struct defclass_constituent *res
	= malloc(sizeof(struct defclass_constituent));

    res->kind = constituent_DEFCLASS;
    res->next = NULL;
    res->flags = 0;
    res->name = name;
    res->supers = supers->head;
    free(supers);
    if (guts) {
	res->slots = guts->slots;
	res->initargs = guts->initargs;
	res->inheriteds = guts->inheriteds;
	free(guts);
    }
    else {
	res->slots = NULL;
	res->initargs = NULL;
	res->inheriteds = NULL;
    }
    res->tlf1 = NULL;
    res->tlf2 = NULL;

    return (struct constituent *)res;
}

struct constituent *set_class_flags(flags_t flags,
				    struct constituent *defclass)
{
    ((struct defclass_constituent *)defclass)->flags = flags;
    return defclass;
}

struct superclass_list *make_superclass_list(void)
{
    struct superclass_list *res = malloc(sizeof(struct superclass_list));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct superclass_list
    *add_superclass(struct superclass_list *list, struct expr *expr)
{
    struct superclass *sup = malloc(sizeof(struct superclass));

    sup->expr = expr;
    sup->next = NULL;

    *list->tail = sup;
    list->tail = &sup->next;

    return list;
}

struct class_guts *make_class_guts(void)
{
    struct class_guts *res = malloc(sizeof(*res));

    res->slots = NULL;
    res->slots_tail = &res->slots;
    res->initargs = NULL;
    res->initargs_tail = &res->initargs;
    res->inheriteds = NULL;
    res->inheriteds_tail = &res->inheriteds;

    return res;
}

static struct plist *add_init_expr (int line, struct expr *init_expr, 
				   struct plist *plist)
{
    if (init_expr != NULL) {
	/* If the init-expr is a literal, add it to the property list 
	   as a init-value:
	   */
	if (init_expr->kind == expr_LITERAL) {
	    plist = add_property(plist ? plist : make_property_list(),
				 make_token("init-value:", 11),
				 init_expr);
	} else {
	    /* Otherwise, turn the expression into an anonymous method
	       and add it to the property list as an init-function.
	       To do that, we need to create all sorts of parse-tree
	       stuff out of thin air, which ain't pretty.  
	       */
	    struct body *init_expr_body
		= add_constituent(make_body(), 
				  make_expr_constituent(init_expr));
	    struct method *init_method
		= make_method_description(make_param_list(), NULL, 
					  init_expr_body);
	    struct token *init_method_token;
	    
	    init_method->line = line;
	    init_method_token = make_token("init-function:", 14);
	    init_method_token->line = line;
	    plist = add_property(plist ? plist : make_property_list(),
				 init_method_token,
				 make_method_ref(init_method));
	}
    }
    return plist;
}

struct slot_spec
    *make_slot_spec(int line, flags_t flags, enum slot_allocation alloc,
		    struct id *name, struct expr *type, struct expr *init_expr,
		    struct plist *plist)
{
    struct slot_spec *res = malloc(sizeof(struct slot_spec));

    res->line = line;
    res->flags = flags;
    res->alloc = alloc;
    res->name = name;
    res->type = type;
    res->getter = NULL;
    res->setter = NULL;
    res->next = NULL;
    res->plist = add_init_expr(line, init_expr, plist);

    return res;
}

struct class_guts *add_slot_spec(struct class_guts *guts,
				 struct slot_spec *spec)
{
    *guts->slots_tail = spec;
    guts->slots_tail = &spec->next;

    return guts;
}

struct initarg_spec
  *make_initarg_spec(boolean required, struct token *key, struct plist *plist)
{
    struct initarg_spec *res = malloc(sizeof(*res));

    /* The keyword token has a trailing : */
    key->chars[key->length-1] = '\0';

    res->required = required;
    res->keyword = symbol((char *)key->chars);
    res->plist = plist;
    res->next = NULL;

    return res;
}

struct class_guts *add_initarg_spec(struct class_guts *guts,
				    struct initarg_spec *spec)
{
    *guts->initargs_tail = spec;
    guts->initargs_tail = &spec->next;

    return guts;
}

struct inherited_spec *make_inherited_spec(int line, struct id *name,
					   struct expr *init_expr,
					   struct plist *plist)
{
    struct inherited_spec *res = malloc(sizeof(*res));

    res->name = name;
    res->next = NULL;
    res->plist = add_init_expr(line, init_expr, plist);

    return res;
}

struct class_guts *add_inherited_spec(struct class_guts *guts,
				      struct inherited_spec *spec)
{
    *guts->inheriteds_tail = spec;
    guts->inheriteds_tail = &spec->next;

    return guts;
}

struct constituent
    *make_sealed_domain(struct id *name, struct arglist *types)
{
    struct defdomain_constituent *res
	= malloc(sizeof(struct defdomain_constituent));

    res->kind = constituent_DEFDOMAIN;
    res->next = NULL;
    res->name = name;
    res->types = types->head;
    res->tlf = NULL;

    free(types);

    return (struct constituent *)res;
}

struct constituent
    *set_sealed_domain_flags(flags_t flags, struct constituent *constituent)
{
    if (flags != flag_SEALED) {
	struct defdomain_constituent *defdomain
	    = (struct defdomain_constituent *)constituent;
	warn(defdomain->name->line,
	     "Bogus adjectives for define sealed domain of %s",
	     defdomain->name->symbol->name);
    }

    return constituent;
}

struct constituent
    *make_define_generic(struct id *name, struct param_list *params,
			 struct gf_suffix *suffix)
{
    struct defgeneric_constituent *res
	= malloc(sizeof(struct defgeneric_constituent));

    res->kind = constituent_DEFGENERIC;
    res->next = NULL;
    res->flags = 0;
    res->name = name;
    res->params = params;
    res->rettypes = suffix->rettypes;
    res->plist = suffix->plist;
    res->tlf = NULL;

    free(suffix);

    return (struct constituent *)res;
}

struct constituent *set_generic_flags(flags_t flags,
				    struct constituent *defgeneric)
{
    ((struct defgeneric_constituent *)defgeneric)->flags = flags;
    return defgeneric;
}

struct gf_suffix
    *make_gf_suffix(struct return_type_list *rettypes,
		    struct plist *plist)
{
    struct gf_suffix *res = malloc(sizeof(struct gf_suffix));

    res->rettypes = rettypes;
    res->plist = plist;

    return res;
}

struct method *set_method_source(struct token *source, struct method *method)
{
    method->line = source->line;

    return method;
}

struct method *set_method_name(struct id *name, struct method *method)
{
    method->name = name;
    method->line = name->line;
    method->debug_name = make_symbol_literal(name->symbol);

    return method;
}

struct method
    *make_method_description(struct param_list *params,
			     struct return_type_list *rettypes,
			     struct body *body)
{
    struct method *res = malloc(sizeof(struct method));

    res->name = NULL;
    res->line = 0;
    res->debug_name = NULL;
    res->top_level = FALSE;
    res->component = NULL;
    res->params = params;
    res->specializers = NULL;
    res->rettypes = rettypes;
    res->body = body;
    res->next_local = NULL;
    res->nargs = 0;
    res->lexenv = NULL;
    res->frame_size = 0;
    res->closes_over = NULL;
    res->lexenv_size = 0;
    res->parent = NULL;
    res->kids = NULL;
    res->next = NULL;

    return res;
}

struct method *make_top_level_method(char *debug_name, struct body *body)
{
    struct method *res = make_method_description(make_param_list(),NULL,body);

    res->debug_name = make_string_literal(debug_name);
    res->top_level = TRUE;
    res->specializers=make_literal_ref(make_list_literal(make_literal_list()));

    return res;
}

struct constituent *make_error_constituent(void)
{
    struct constituent *res = malloc(sizeof(struct constituent));

    res->kind = constituent_ERROR;
    res->next = NULL;

    return res;
}

struct expr *make_error_expression(void)
{
    struct expr *res = malloc(sizeof(struct expr));

    res->kind = expr_ERROR;
    res->analyzed = FALSE;

    return res;
}

struct defnamespace_constituent
    *make_define_namespace(enum constituent_kind kind)
{
    struct defnamespace_constituent *res = malloc(sizeof(*res));

    res->kind = kind;
    res->next = NULL;
    res->name = NULL;
    res->use_clauses = NULL;
    res->use_tail = &res->use_clauses;
    res->exported_variables = make_variable_names();
    res->created_variables = make_variable_names();
    res->exported_literal = NULL;
    res->created_literal = NULL;

    return res;
}

struct defnamespace_constituent *make_define_module(void)
{
    return make_define_namespace(constituent_DEFMODULE);
}

struct defnamespace_constituent *make_define_library(void)
{
    return make_define_namespace(constituent_DEFLIBRARY);
}

struct defnamespace_constituent
    *set_namespace_name(struct defnamespace_constituent *namespace,
			struct token *name)
{
    namespace->name = parse_symbol_token(name);

    return namespace;
}

struct defnamespace_constituent
    *add_use_clause(struct defnamespace_constituent *namespace,
		    struct use_clause *clause)
{
    *namespace->use_tail = clause;
    namespace->use_tail = &clause->next;

    return namespace;
}

struct defnamespace_constituent
    *add_exports(struct defnamespace_constituent *namespace,
		 struct variable_names *vars)
{
    *namespace->exported_variables->tail = vars->head;
    namespace->exported_variables->tail = vars->tail;
    free(vars);

    return namespace;
}

struct defnamespace_constituent
    *add_creates(struct defnamespace_constituent *namespace,
		 struct variable_names *vars)
{
    *namespace->created_variables->tail = vars->head;
    namespace->created_variables->tail = vars->tail;
    free(vars);

    return namespace;
}

struct use_clause
    *make_use_clause(struct token *symbol, struct use_options *options)
{
    struct use_clause *res = malloc(sizeof(*res));

    res->name = parse_symbol_token(symbol);
    res->options = options->head;
    res->next = NULL;
    res->import = NULL;
    res->exclude = NULL;
    res->prefix = NULL;
    res->rename = NULL;
    res->export = NULL;

    free(options);

    return res;
}

struct use_options *make_use_options(void)
{
    struct use_options *res = malloc(sizeof(*res));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct use_options
    *add_use_option(struct use_options *options, struct use_option *option)
{
    *options->tail = option;
    options->tail = &option->next;

    return options;
}

struct use_option *make_use_option(enum useopt_kind kind)
{
    struct use_option *res = malloc(sizeof(*res));

    res->kind = kind;
    res->next = NULL;

    return res;
}

struct use_option *make_prefix_option(struct token *token)
{
    struct prefix_option *res = malloc(sizeof(*res));

    res->kind = useopt_PREFIX;
    res->next = NULL;
    res->prefix = parse_string_token(token);

    return (struct use_option *) res;
}

struct variable_names *make_variable_names(void)
{
    struct variable_names *res = malloc(sizeof(*res));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct variable_names
    *add_variable_name(struct variable_names *names, struct token *token)
{
    struct variable_name *new = malloc(sizeof(*new));
    new->name = parse_symbol_token(token);
    new->next = NULL;

    *names->tail = new;
    names->tail = &new->next;
    
    return names;
}

struct renamings *make_renamings(void)
{
    struct renamings *res = malloc(sizeof(*res));

    res->head = NULL;
    res->tail = &res->head;

    return res;
}

struct renamings
    *add_renaming(struct renamings *names,
		  struct token *from, struct token *to)
{
    struct renaming *new = malloc(sizeof(*new));

    new->from = parse_symbol_token(from);
    new->to = parse_symbol_token(to);
    new->next = NULL;

    *names->tail = new;
    names->tail = &new->next;

    return names;
}

struct import_option *make_import_option(void)
{
    struct import_option *res = malloc(sizeof(*res));

    res->kind = useopt_IMPORT;
    res->next = NULL;
    res->vars = make_variable_names();
    res->renames = make_renamings();

    return res;
}

struct import_option
    *add_import(struct import_option *opt,
		struct token *from, struct token *to)
{
    if (to)
	opt->renames = add_renaming(opt->renames, from, to);
    else
	opt->vars = add_variable_name(opt->vars, from);

    return opt;
}


struct use_option *make_exclude_option(struct variable_names *vars)
{
    struct exclude_option *res = malloc(sizeof(*res));

    res->kind = useopt_EXCLUDE;
    res->next = NULL;
    res->vars = vars;

    return (struct use_option *) res;
}

struct use_option *make_export_option(struct variable_names *vars)
{
    struct export_option *res = malloc(sizeof(*res));

    res->kind = useopt_EXPORT;
    res->next = NULL;
    res->vars = vars;

    return (struct use_option *) res;
}

struct use_option *make_rename_option(struct renamings *lst)
{
    struct rename_option *res = malloc(sizeof(*res));

    res->kind = useopt_RENAME;
    res->next = NULL;
    res->renames = lst;

    return (struct use_option *) res;
}
