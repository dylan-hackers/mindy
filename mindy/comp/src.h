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
* $Header: /scm/cvs/src/mindy/comp/src.h,v 1.1 1998/05/03 19:55:09 andreas Exp $
*
\**********************************************************************/


typedef unsigned int flags_t;

#define flag_OPEN 1
#define flag_SEALED 2
#define flag_ABSTRACT 4
#define flag_CONCRETE 8
#define flag_PRIMARY 16
#define flag_FREE 32
#define flag_CONSTANT 64

struct body {
    struct constituent *head;
    struct constituent **tail;
};

enum constituent_kind {
    constituent_DEFCONST, constituent_DEFVAR, constituent_DEFMETHOD,
    constituent_DEFDOMAIN, constituent_DEFGENERIC, constituent_DEFCLASS,
    constituent_EXPR, constituent_LOCAL, constituent_HANDLER, constituent_LET,
    constituent_TOPLEVELFORM, constituent_ERROR, constituent_DEFMODULE,
    constituent_DEFLIBRARY, constituent_Kinds
};

struct constituent {
    enum constituent_kind kind;
    struct constituent *next;
};

struct defconst_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    int line;
    struct bindings *bindings;
    struct method *tlf;
};

struct defvar_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    int line;
    struct bindings *bindings;
    struct method *tlf;
};

struct defmethod_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    flags_t flags;
    struct method *method;
    struct method *tlf;
};

struct defdomain_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct id *name;
    struct argument *types;
    struct method *tlf;
};

struct defgeneric_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    flags_t flags;
    struct id *name;
    struct param_list *params;
    struct return_type_list *rettypes;
    struct plist *plist;
    struct method *tlf;
};

struct defclass_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    flags_t flags;
    struct id *name;
    struct superclass *supers;
    struct slot_spec *slots;
    struct initarg_spec *initargs;
    struct inherited_spec *inheriteds;
    struct method *tlf1;
    struct method *tlf2;
};

struct expr_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct expr *expr;
};
    
struct binding_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct body *body;
};

struct local_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct body *body;
    int offset;
    struct method *methods;
    struct lexenv *lexenv;
};

struct handler_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct body *body;
    struct expr *type;
    struct expr *func;
    struct plist *plist;
};

struct let_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct body *body;
    int offset;
    struct bindings *bindings;
    int required;
    struct lexenv *lexenv;
    struct binding *inside;
};

struct tlf_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct method *form;
};

struct defnamespace_constituent {
    enum constituent_kind kind;
    struct constituent *next;
    struct literal *name;
    struct use_clause *use_clauses;
    struct use_clause **use_tail;
    struct variable_names *exported_variables;
    struct variable_names *created_variables;
    struct literal *exported_literal;
    struct literal *created_literal;
};

enum expr_kind {
    expr_VARREF, expr_LITERAL, expr_CALL, expr_METHOD, expr_DOT,
    expr_BODY, expr_BLOCK, expr_CASE, expr_IF, expr_FOR, expr_SELECT,
    expr_VARSET, expr_BINOP_SERIES, expr_LOOP, expr_REPEAT,
    expr_ERROR, expr_Kinds
};

struct expr {
    enum expr_kind kind;
    boolean analyzed;
};

struct varref_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct id *var;
    struct method *home;
    struct binding *binding;
    struct closes_over *over;
};

struct literal_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct literal *lit;
};

struct call_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct expr *func;
    struct function_info *info;
    struct argument *args;
};

struct method_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct method *method;
};

struct dot_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct expr *arg;
    struct expr *func;
};

struct body_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct body *body;
};

struct block_expr {
    enum expr_kind kind;
    boolean analyzed;
    int line;
    struct id *exit_fun;
    struct body *body;
    struct exception_clause *inner;
    struct body *cleanup;
    struct exception_clause *outer;
};

struct case_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct condition_body *body;
};

struct if_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct expr *cond;
    struct body *consequent;
    int else_line;
    struct body *alternate;
};

struct for_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct for_clause *clauses;
    struct expr *until;
    struct body *body;
    struct body *finally;
};

struct select_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct expr *expr;
    struct expr *by;
    struct condition_body *body;
};

struct varset_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct id *var;
    struct method *home;
    struct binding *binding;
    struct closes_over *over;
    struct expr *value;
    struct varref_expr *type;
};

struct binop_series_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct expr *first_operand;
    struct binop *first_binop;
};

struct loop_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct body *body;
    int position;
};

struct repeat_expr {
    enum expr_kind kind;
    boolean analyzed;
    struct loop_expr *loop;
};

struct bindings {
    struct param_list *params;
    struct expr *expr;
};

struct param_list {
    struct param *required_params;
    struct id *next_param;
    struct id *rest_param;
    boolean allow_keys;
    boolean all_keys;
    struct keyword_param *keyword_params;
};

struct param {
    struct id *id;
    struct expr *type;
    struct symbol *type_temp;
    struct param *next;
};

struct keyword_param {
    struct symbol *keyword;
    struct id *id;
    struct expr *type;
    struct symbol *type_temp;
    struct expr *def;
    struct keyword_param *next;
};

struct id {
    struct symbol *symbol;
    boolean internal;
    int line;
};

struct method {
    struct id *name;
    int line;
    struct literal *debug_name;
    boolean top_level;
    struct component *component;
    struct param_list *params;
    struct expr *specializers;
    struct return_type_list *rettypes;
    struct body *body;
    struct method *next_local;
    int nargs;
    struct lexenv *lexenv;
    int frame_size;
    struct closes_over *closes_over;
    int lexenv_size;
    struct method *parent;
    struct method *kids;
    struct method *next;
};

struct binop {
    struct id *op;
    int precedence;
    boolean left_assoc;
    struct expr *operand;
    struct binop *next;
};

struct argument {
    struct expr *expr;
    struct argument *next;
};

struct plist {
    struct property *head;
    struct property **tail;
};

struct property {
    int line;
    struct symbol *keyword;
    struct expr *expr;
    struct property *next;
};

struct return_type_list {
    struct return_type *req_types;
    struct return_type **req_types_tail;
    struct expr *req_types_list;
    boolean restp;
    struct expr *rest_type;
    struct symbol *rest_temp;
    struct expr *rest_temp_varref;
};

struct return_type {
    struct expr *type;
    struct symbol *temp;
    struct return_type *next;
};

struct condition_body {
    struct condition_clause *clause;
    struct condition_body *next;
};

struct condition_clause {
    struct condition *conditions;
    struct body *body;
};

struct condition {
    struct expr *cond;
    struct condition *next;
};

struct exception_clause {
    struct expr *type;
    struct id *condition;
    struct plist *plist;
    struct body *body;
    struct exception_clause *next;
};

enum for_clause_kind {
    for_EQUAL_THEN, for_IN, for_FROM, for_Kinds
};

struct for_clause {
    enum for_clause_kind kind;
    struct for_clause *next;
    struct param_list *vars;
};

struct equal_then_for_clause {
    enum for_clause_kind kind;
    struct for_clause *next;
    struct param_list *vars;
    struct expr *equal;
    struct expr *then;
};

struct in_for_clause {
    enum for_clause_kind kind;
    struct for_clause *next;
    struct param_list *vars;
    struct expr *collection;
    struct param *protocol;
};

enum to_kind {
    to_TO, to_ABOVE, to_BELOW, to_UNBOUNDED, to_Kinds
};

struct from_for_clause {
    enum for_clause_kind kind;
    struct for_clause *next;
    struct param_list *vars;
    struct expr *from;
    enum to_kind to_kind;
    struct expr *to;
    struct expr *by;
};

struct superclass {
    struct expr *expr;
    struct superclass *next;
};

enum slot_allocation {
    alloc_INSTANCE, alloc_CLASS, alloc_EACH_SUBCLASS, 
    alloc_VIRTUAL, alloc_Kinds
};

struct slot_spec {
    int line;
    flags_t flags;
    enum slot_allocation alloc;
    struct id *name;
    struct expr *type;
    struct plist *plist;
    struct id *getter;
    struct id *setter;
    struct slot_spec *next;
};

struct initarg_spec {
    boolean required;
    struct symbol *keyword;
    struct plist *plist;
    struct initarg_spec *next;
};

struct inherited_spec {
    struct id *name;
    struct plist *plist;
    struct inherited_spec *next;
};

struct variable_names {
    struct variable_name *head;
    struct variable_name **tail;
};

struct variable_name {
    struct literal *name;
    struct variable_name *next;
};

enum useopt_kind {
    useopt_PREFIX, useopt_IMPORT, useopt_EXCLUDE, useopt_RENAME, useopt_EXPORT,
    useopt_IMPORT_ALL, useopt_EXPORT_ALL
};

struct use_options {
    struct use_option *head;
    struct use_option **tail;
};

struct use_option {
    enum useopt_kind kind;
    struct use_option *next;
};

struct prefix_option {
    enum useopt_kind kind;
    struct use_option *next;
    struct literal *prefix;
};

struct renamings {
    struct renaming *head;
    struct renaming **tail;
};

struct renaming {
    struct literal *from;
    struct literal *to;
    struct renaming *next;
};

struct import_option {
    enum useopt_kind kind;
    struct use_option *next;
    struct variable_names *vars;
    struct renamings *renames;
};

struct exclude_option {
    enum useopt_kind kind;
    struct use_option *next;
    struct variable_names *vars;
};

struct rename_option {
    enum useopt_kind kind;
    struct use_option *next;
    struct renamings *renames;
};

struct export_option {
    enum useopt_kind kind;
    struct use_option *next;
    struct variable_names *vars;
};

struct use_clause {
    struct literal *name;
    struct use_option *options;
    struct use_clause *next;
    struct literal *import;
    struct literal *exclude;
    struct literal *prefix;
    struct literal *rename;
    struct literal *export;
};

/* Various structures we reference, but don't know the details of. */
struct token;
struct local_methods;
struct binop_series;
struct arglist;
struct block_epilog;
struct incomplete_condition_body;
struct exception_clauses;
struct superclass_list;
struct for_header;
struct gf_suffix;
struct to_part;
struct class_guts;
struct else_part;

extern struct body *make_body(void);
extern struct body
    *add_constituent(struct body *body, struct constituent *constituent);
extern struct body *make_expr_body(struct expr *expr);
extern struct constituent
    *make_define_constant(int line, struct bindings *bindings);
extern struct constituent
    *make_define_method(flags_t flags, struct method *method);
extern struct constituent
    *make_define_function(flags_t flags, struct method *method);
extern struct constituent
    *make_define_variable(int line, struct bindings *bindings);
extern struct constituent *make_expr_constituent(struct expr *expr);
extern struct constituent *make_let(struct bindings *bindings);
extern struct constituent *make_handler(struct expr *type, struct expr *func,
					struct plist *plist);
extern struct constituent
    *make_local_constituent(struct local_methods *methods);
extern struct constituent
    *make_top_level_form(char *debug_name, struct constituent *c);
extern struct expr *make_varref(struct id *id);
extern struct expr *make_varset(struct id *var, struct expr *expr);
extern struct id *id(struct symbol *symbol);
extern struct id *dup_id(struct id *id);
extern struct id *make_id(struct token *token);
extern struct bindings *make_bindings(struct param_list *params,
				      struct expr *expr);
extern struct param_list *make_param_list(void);
extern struct param_list *push_param(struct param *param,
				     struct param_list *list);
extern struct param_list *set_rest_param(struct param_list *list,
					 struct id *id);
extern struct param *make_param(struct id *id, struct expr *type);
extern struct local_methods *add_local_method(struct local_methods *methods,
					      struct method *method);
extern struct local_methods *make_local_methods(void);
extern struct expr *make_literal_ref(struct literal *lit);
extern struct expr
    *make_binop_series_expr(struct expr *operand, struct binop_series *series);
extern struct binop_series *make_binop_series(void);
extern struct binop_series
    *add_binop(struct binop_series *series, struct binop *op,
	       struct expr *operand);
extern struct binop *make_binop(struct id *id);
extern struct expr *make_negate(int line, struct expr *expr);
extern struct expr *make_not(int line, struct expr *expr);
extern struct expr
    *make_aref_or_element(int line, struct expr *expr, struct arglist *args);
extern struct expr
    *make_function_call(struct expr *expr, struct arglist *args);
extern struct expr *make_method_ref(struct method *method);
extern struct expr *make_dot_operation(struct expr *expr, struct expr *fn);
extern struct arglist *make_argument_list(void);
extern struct arglist *add_argument(struct arglist *arglist,
				    struct argument *arg);
extern struct argument *make_argument(struct expr *expr);
extern struct argument
    *make_keyword_argument(struct token *keyword, struct expr *expr);
extern struct plist *make_property_list(void);
extern struct plist
    *add_property(struct plist *plist, struct token *keyword,
		  struct expr *expr);
extern struct return_type_list *make_return_type_list(boolean restp,
						      struct expr *rest);
extern struct return_type_list *add_return_type(struct return_type_list *l,
						struct expr *type);
extern struct return_type_list
    *set_return_type_rest_type(struct return_type_list *l,
			       struct expr *type);
extern struct literal *parse_true_token(struct token *token);
extern struct literal *parse_false_token(struct token *token);
extern struct literal *parse_string_token(struct token *token);
extern struct literal
    *concat_string_token(struct literal *old_literal, struct token *token);
extern struct literal *parse_character_token(struct token *token);
extern struct literal *parse_integer_token(struct token *token);
extern struct literal *parse_float_token(struct token *token);
extern struct literal *parse_symbol_token(struct token *token);
extern struct literal *parse_keyword_token(struct token *token);
extern struct expr *make_body_expr(struct body *body);
extern struct expr *make_block(int line, struct id *exit, struct body *body,
			       struct block_epilog *epilog);
extern struct expr *make_case(struct condition_body *body);
extern struct expr *make_if(struct expr *cond, struct body *consequent,
			    struct else_part *else_part);
extern struct else_part *make_else(int else_line, struct body *alternate);
extern struct expr *make_for(struct for_header *header, struct body *body,
			     struct body *finally);
extern struct expr *make_select(struct expr *expr, struct expr *by,
				struct condition_body *body);
extern struct expr *make_loop(struct body *body);
extern struct expr *make_repeat(void);
extern struct block_epilog *make_block_epilog(struct exception_clauses *inner,
					      struct body *cleanup,
					      struct exception_clauses *outer);
extern struct for_header *make_for_header(struct expr *until);
extern struct for_header *push_for_clause(struct for_clause *clause,
					  struct for_header *header);
extern struct exception_clauses *make_exception_clauses(void);
extern struct exception_clauses
    *add_exception_clause(struct exception_clauses *clauses,
			  struct exception_clause *clause);
extern struct exception_clause
    *make_exception_clause(struct expr *type, struct id *condition,
			   struct plist *plist, struct body *body);
extern struct condition_body
    *push_condition_clause(struct condition_clause *clause,
			   struct condition_body *cond_body);
extern struct condition_clause
    *make_otherwise_condition_clause(struct body *body);
extern struct incomplete_condition_body
    *make_incomplete_condition_clauses(struct constituent *constituent,
				       struct condition_body *rest);
extern struct incomplete_condition_body
    *push_condition_constituent(struct constituent *constituent,
				struct incomplete_condition_body *body);
extern struct condition_body
    *complete_condition_clauses(struct condition_clause *clause,
				struct incomplete_condition_body *body);
extern struct condition_clause
    *make_condition_clause(struct constituent *constituent);
extern struct condition_clause
    *push_condition(struct expr *cond, struct condition_clause *clause);
extern struct for_clause
    *make_equal_then_for_clause(struct param_list *vars, struct expr *equal,
				struct expr *then);
extern struct for_clause
    *make_in_for_clause(struct param *var, struct param *keyed_by,
			struct expr *collection, struct param *protocol);
extern struct for_clause
    *make_from_for_clause(struct param *var, struct expr *from,
			  struct to_part *to, struct expr *by);
extern struct to_part *make_to(struct expr *expr);
extern struct to_part *make_above(struct expr *expr);
extern struct to_part *make_below(struct expr *expr);
extern struct constituent
    *make_class_definition(struct id *name, struct superclass_list *supers,
			   struct class_guts *guts);
extern struct constituent
    *set_class_flags(flags_t flags, struct constituent *defclass);
extern struct superclass_list *make_superclass_list(void);
extern struct superclass_list
    *add_superclass(struct superclass_list *list, struct expr *expr);
extern struct class_guts *make_class_guts(void);
extern struct slot_spec
    *make_slot_spec(int line, flags_t flags, enum slot_allocation alloc,
		    struct id *name, struct expr *type, struct expr *init_expr,
		    struct plist *plist);
extern struct class_guts
    *add_slot_spec(struct class_guts *guts, struct slot_spec *spec);
extern struct initarg_spec
    *make_initarg_spec(boolean required, struct token *keyword,
		       struct plist *plist);
extern struct class_guts
    *add_initarg_spec(struct class_guts *guts, struct initarg_spec *spec);
extern struct inherited_spec
    *make_inherited_spec(int line, struct id *name, struct expr *init_expr, 
			 struct plist *plist);
extern struct class_guts
    *add_inherited_spec(struct class_guts *guts, struct inherited_spec *spec);
extern struct constituent
    *make_sealed_domain(struct id *name, struct arglist *types);
extern struct constituent
    *set_sealed_domain_flags(flags_t flags, struct constituent *sealed_domain);
extern struct constituent
    *make_define_generic(struct id *name, struct param_list *params,
			 struct gf_suffix *suffix);
extern struct constituent
    *set_generic_flags(flags_t flags, struct constituent *defgeneric);
extern struct gf_suffix
    *make_gf_suffix(struct return_type_list *rettypes,
		    struct plist *plist);
extern struct param_list
    *push_keyword_param(struct keyword_param *param, struct param_list *list);
extern struct param_list *allow_keywords(struct param_list *param_list);
extern struct param_list *allow_all_keywords(struct param_list *param_list);
extern struct keyword_param
    *make_keyword_param(struct token *keyword, struct id *sym,
			struct expr *type, struct expr *def);
extern struct method
    *set_method_source(struct token *source, struct method *method);
extern struct method
    *set_method_name(struct id *name, struct method *method);
extern struct method
    *make_top_level_method(char *debug_name, struct body *body);
extern struct method
    *make_method_description(struct param_list *params,
			     struct return_type_list *rettypes,
			     struct body *body);
extern struct expr *make_singleton(int line, struct expr *expr);
extern struct param_list
    *set_next_param(struct param_list *list, struct id *var);
extern struct constituent *make_error_constituent(void);
extern struct expr *make_error_expr(void);
extern struct defnamespace_constituent *make_define_module(void);
extern struct defnamespace_constituent *make_define_library(void);
extern struct defnamespace_constituent
    *set_namespace_name(struct defnamespace_constituent *namespace,
			struct token *name);
extern struct defnamespace_constituent
    *add_use_clause(struct defnamespace_constituent *namespace,
		    struct use_clause *clause);
extern struct defnamespace_constituent
    *add_exports(struct defnamespace_constituent *namespace,
		 struct variable_names *vars);
extern struct defnamespace_constituent
    *add_creates(struct defnamespace_constituent *namespace,
		 struct variable_names *vars);
extern struct use_clause
    *make_use_clause(struct token *symbol, struct use_options *options);
extern struct use_options *make_use_options(void);
extern struct use_options
    *add_use_option(struct use_options *options, struct use_option *option);
extern struct use_option *make_use_option(enum useopt_kind kind);
extern struct use_option *make_prefix_option(struct token *token);
extern struct variable_names *make_variable_names(void);
extern struct variable_names
    *add_variable_name(struct variable_names *names, struct token *token);
extern struct renamings *make_renamings(void);
extern struct renamings
    *add_renaming(struct renamings *names,
		  struct token *from, struct token *to);
extern struct import_option *make_import_option(void);
extern struct import_option
    *add_import(struct import_option *opt,
		struct token *from, struct token *to);
extern struct use_option *make_exclude_option(struct variable_names *vars);
extern struct use_option *make_export_option(struct variable_names *vars);
extern struct use_option *make_rename_option(struct renamings *lst);
