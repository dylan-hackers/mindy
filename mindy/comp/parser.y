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
* $Header: /scm/cvs/src/mindy/comp/parser.y,v 1.6 2002/07/27 13:37:04 housel Exp $
*
* This file is the grammar.
*
\**********************************************************************/

%{
#include "../compat/std-c.h"

#include "mindycomp.h"
#include "header.h"
#include "parser.h"
#include "lexer.h"
#include "feature.h"
#include "literal.h"
#include "src.h"
#include "sym.h"

struct token_list {
    int token;
    struct token_list *next;
} *yacc_recovery_list = NULL;

static void yyerror(char *);
static boolean verify_symbol_aux(struct id *id, struct token *against);
static void yacc_recover();
static void push_yacc_recovery(int token);
static void pop_yacc_recoveries(int count);

#define verify_symbol(id,sym) if (verify_symbol_aux(id,sym)) YYERROR

%}

%union {
    struct _nothing *nothing;
    struct token *token;
    struct body *body;
    struct constituent *constituent;
    struct expr *expr;
    struct bindings *bindings;
    struct param_list *param_list;
    struct param *param;
    struct keyword_param *keyword_param;
    struct id *id;
    struct local_methods *local_methods;
    struct method *method;
    struct binop_series *binop_series;
    struct binop *binop;
    struct literal *literal;
    struct arglist *arglist;
    struct argument *argument;
    struct plist *plist;
    struct return_type_list *return_type_list;
    struct literal_list *literal_list;
    struct block_epilog *block_epilog;
    struct condition_body *condition_body;
    struct incomplete_condition_body *incomplete_condition_body;
    struct condition_clause *condition_clause;
    struct for_header *for_header;
    struct exception_clauses *exception_clauses;
    struct exception_clause *exception_clause;
    struct for_clause *for_clause;
    struct to_part *to_part;
    struct superclass_list *superclass_list;
    struct class_guts *class_guts;
    struct slot_spec *slot_spec;
    struct initarg_spec *initarg_spec;
    boolean bool;
    struct inherited_spec *inherited_spec;
    enum slot_allocation slot_allocation;
    struct gf_suffix *gf_suffix;
    flags_t flags;
    struct else_part *else_part;

    struct defnamespace_constituent *defnamespace_constituent;
    struct variable_names *variable_names;
    struct use_clause *use_clause;
    struct use_options *use_options;
    struct use_option *use_option;
    struct import_option *import_option;
    struct renamings *renamings;
}

%token <token> HEADER_KEY
%token <token> HEADER_VAL
%token <token> HEADER_END

%token <token> BOGUS
%token <token> SYMBOL
%token <token> KEYWORD
%token <token> SYMBOL_LITERAL
%token <token> SHARP_T
%token <token> SHARP_F
%token <token> STRING
%token <token> CHARACTER
%token <token> INTEGER
%token <token> FLOAT
%token <token> BINARY_OPERATOR
%token <token> LPAREN
%token <token> RPAREN
%token <token> COMMA
%token <token> DOT
%token <token> SEMI
%token <token> LBRACKET
%token <token> RBRACKET
%token <token> LBRACE
%token <token> RBRACE
%token <token> COLON_COLON
%token <token> MINUS
%token <token> TILDE
%token <token> EQUAL
%token <token> EQUAL_EQUAL
%token <token> ARROW
%token <token> SHARP_PAREN
%token <token> SHARP_BRACKET
%token <token> NEXT
%token <token> REST
%token <token> KEY
%token <token> ALL_KEYS
%token <token> ABOVE
%token <token> ABSTRACT
%token <token> DBEGIN
%token <token> BELOW
%token <token> BLOCK
%token <token> BY
%token <token> CASE
%token <token> CLASS
%token <token> CLEANUP
%token <token> CONCRETE
%token <token> CONSTANT
%token <token> DEFINE
%token <token> DOMAIN
%token <token> EACH_SUBCLASS
%token <token> ELSE
%token <token> ELSEIF
%token <token> END
%token <token> EXCEPTION
%token <token> FINALLY
%token <token> FOR
%token <token> FREE
%token <token> FROM
%token <token> FUNCTION
%token <token> GENERIC
%token <token> HANDLER
%token <token> IF
%token <token> IN
%token <token> INHERITED
%token <token> INLINE
%token <token> INSTANCE
%token <token> KEYED_BY
%token <token> KEYWORD_RESERVED_WORD
%token <token> LET
%token <token> LOCAL
%token <token> METHOD
%token <token> OPEN
%token <token> OTHERWISE
%token <token> PRIMARY
%token <token> REQUIRED
%token <token> SEALED
%token <token> SELECT
%token <token> SLOT
%token <token> THEN
%token <token> TO
%token <token> UNLESS
%token <token> UNTIL
%token <token> USING
%token <token> VARIABLE
%token <token> VIRTUAL
%token <token> WHILE

%token <token> MODULE
%token <token> LIBRARY
%token <token> EXPORT
%token <token> CREATE
%token <token> USE
%token <token> ALL

/* These tokens sometimes have special significance and are sometimes just
   keywords */
%token <token> PREFIX_KEYWORD
%token <token> IMPORT_KEYWORD
%token <token> EXCLUDE_KEYWORD
%token <token> EXPORT_KEYWORD
%token <token> RENAME_KEYWORD
%token <token> UNTIL_KEYWORD
%token <token> WHILE_KEYWORD

/* Used by the lexer for conditional compilation. */
%token <token> FEATURE_IF
%token <token> FEATURE_ELSE_IF
%token <token> FEATURE_ELSE
%token <token> FEATURE_ENDIF

%type <nothing> dylan_file dylan_headers header_list
%type <nothing> dylan_program block_opt case_opt if_opt for_opt select_opt
%type <nothing> unless_opt until_opt while_opt class_opt function_opt
%type <nothing> begin_opt method_opt semi_opt
%type <nothing> arrow_opt
%type <token> symbol_opt
%type <body> body body_opt constituents cleanup_part
%type <body> final_part final_part_opt
%type <constituent> constituent defining_form local_declaration
%type <constituent> class_definition sealed_domain generic_function_definition
%type <bindings> bindings
%type <token> variable_name variable_name_opt any_variable_name_except_function any_variable_name_except_function_opt
%type <param_list> variables gf_parameters
%type <param_list> more_gf_parameters gf_rest_parameters
%type <param_list> gf_keyword_parameters_list gf_keyword_parameters_opt
%type <param_list> gf_keyword_parameters parameters more_parameters
%type <param_list> next_parameters rest_parameters keyword_parameters_list
%type <param_list> keyword_parameters_opt keyword_parameters
%type <param> variable positional_parameter
%type <keyword_param> keyword_parameter gf_keyword_parameter
%type <id> next_parameter rest_parameter
%type <local_methods> local_methods
%type <method> local_method anonymous_method named_method method_description named_function
%type <expr> expression operand leaf statement by_part by_part_opt 
%type <expr> slot_type_opt return_type_element keyword_parameter_type slot_init_expr_opt
%type <expr> keyword_parameter_default
%type <binop_series> binop_series
%type <binop> binop
%type <arglist> arguments_opt arguments
%type <argument> argument
%type <plist> property_list_opt property_list
%type <return_type_list> return_type return_type_list return_type_list_head
%type <literal> constant concat_string literal dotted_list
%type <literal_list> literals_opt literals
%type <block_epilog> block_epilog block_epilog_opt
%type <condition_body> condition_body complete_condition_clauses
%type <for_header> for_header
%type <exception_clauses> exception_clauses_opt exception_clauses
%type <exception_clause> exception_clause
%type <incomplete_condition_body> incomplete_condition_clauses
%type <condition_clause> condition_clause
%type <for_clause> for_clause
%type <to_part> to_part_opt
%type <superclass_list> superclasses
%type <class_guts> class_guts_opt class_guts
%type <slot_spec> slot_spec
%type <initarg_spec> initarg_spec
%type <bool> required_opt
%type <inherited_spec> inherited_spec
%type <slot_allocation> allocation
%type <gf_suffix> gf_suffix
%type <flags> flags slot_adjectives
%type <else_part> else_part else_part_opt

%type <constituent> module_definition library_definition 
%type <defnamespace_constituent> module_clauses_opt module_clauses
%type <defnamespace_constituent> library_clauses_opt library_clauses
%type <nothing> module_opt library_opt
%type <variable_names> export_clause create_clause 
%type <use_clause> use_clause 
%type <use_options> module_use_options 
%type <use_option> module_use_option prefix_option exclude_option
%type <use_option> rename_option export_option import_option
%type <renamings> rename_specs
%type <import_option> imports imports_opt
%type <variable_names> variable_name_set variable_names
%type <token> keyword keyword_opt

%%

dylan_file:
	dylan_headers dylan_program
;

dylan_headers:
	HEADER_END	{ process_header(NULL, NULL); free($1); }
    |	header_list HEADER_END	{ process_header(NULL, NULL); free($2); }
;

header_list:
	HEADER_KEY HEADER_VAL
	{ process_header((char *)$1->chars, (char *)$2->chars); free($1); free($2); }
    |	header_list HEADER_KEY HEADER_VAL
	{ process_header((char *)$2->chars, (char *)$3->chars); free($2); free($3); }
;

dylan_program:
	{ push_yacc_recovery(SEMI); }
	body_opt { Program = $2; $$ = NULL; }
;

body:
	constituents semi_opt	{ $$ = $1; }
;

body_opt:
	/* epsilon */	{ $$ = make_body(); }
    |	body		{ $$ = $1; }
;


constituents: 
    constituent
	{ $$ = add_constituent(make_body(), $1); }
    |	constituents SEMI constituent
	{ free($2); $$ = add_constituent($1, $3); }
;

constituent:
	defining_form		{ $$ = $1; }
    |	local_declaration	{ $$ = $1; }
    |	expression		{ $$ = make_expr_constituent($1); }
    |   error			{ $$ = make_error_constituent(); }
;

defining_form:
	DEFINE flags CLASS class_definition
	{ free($1); free($3); $$ = set_class_flags($2, $4); }
    |	DEFINE CONSTANT bindings
	{ free($1); $$ = make_define_constant($2->line, $3); free($2); }
    |   DEFINE flags DOMAIN sealed_domain
	{ free($1); free($3); $$ = set_sealed_domain_flags($2, $4); }
    |	DEFINE flags GENERIC generic_function_definition
	{ free($1); free($3); $$ = set_generic_flags($2, $4); }
    |	DEFINE flags FUNCTION named_function
	{ free($1); $$ = make_define_function($3->line, $4); free($3); }
    |	DEFINE flags METHOD named_method
	{ free($1); free($3); $$ = make_define_method($2, $4); }
    |	DEFINE VARIABLE bindings
	{ free($1); $$ = make_define_variable($2->line, $3); free($2); }
    |	DEFINE MODULE module_definition
	{ free($1); free($2); $$ = $3; }
    |	DEFINE LIBRARY library_definition
	{ free($1); free($2); $$ = $3; }
;

flags:
	/* epsilon */ { $$ = 0; }
    |	flags SEALED { free($2); $$ = $1 | flag_SEALED; }
    |	flags OPEN { free($2); $$ = $1 | flag_OPEN; }
    |	flags ABSTRACT { free($2); $$ = $1 | flag_ABSTRACT; }
    |	flags CONCRETE { free($2); $$ = $1 | flag_CONCRETE; }
    |	flags PRIMARY { free($2); $$ = $1 | flag_PRIMARY; }
    |	flags FREE { free($2); $$ = $1 | flag_FREE; }
    |   flags INLINE { free($2); $$ = $1; /* ignore inline directive */ }
;	

local_declaration:
	LET bindings
	{ free($1); $$ = make_let($2); }
    |	LET HANDLER variable_name EQUAL expression
	{ free($1); free($2); free($4);
	  $$ = make_handler(make_varref(make_id($3)), $5, NULL);
	}
    |	LET HANDLER LPAREN expression property_list_opt RPAREN EQUAL expression
	{ free($1); free($2); free($3); free($6); free($7);
	  $$ = make_handler($4, $8, $5);
	}
    |	LOCAL local_methods
	{ free($1); $$ = make_local_constituent($2); }
;

bindings:
	variable EQUAL expression
	{ free($2); $$ = make_bindings(push_param($1,make_param_list()), $3); }
    |	LPAREN variables RPAREN EQUAL expression
	{ free($1); free($3); free($4); $$ = make_bindings($2, $5); }
;

variables:
	rest_parameter
	{ $$ = set_rest_param(make_param_list(), $1); }
    |	variable
	{ $$ = push_param($1, make_param_list()); }
    |	variable COMMA variables
	{ free($2); $$ = push_param($1, $3); }
;

variable:
	variable_name { $$ = make_param(make_id($1), NULL); }
    |	variable_name COLON_COLON operand
	{ free($2); $$ = make_param(make_id($1), $3); }
;

variable_name:
        any_variable_name_except_function
    |	FUNCTION
;

any_variable_name_except_function:
	SYMBOL
    |	ABOVE
    |	ABSTRACT
    |	BY
    |	CLASS
    |	CONCRETE
    |	CONSTANT
    |   DOMAIN
    |	EACH_SUBCLASS
    |	FREE
    |	FROM
    |	IN
    |	INHERITED
    |   INLINE
    |	INSTANCE
    |	KEYED_BY
    |	KEYWORD_RESERVED_WORD
    |	OPEN
    |	PRIMARY
    |	SEALED
    |	SLOT
    |	THEN
    |	TO
    |   USING
    |	VARIABLE
    |	VIRTUAL
    |	MODULE
    |	LIBRARY
    |	EXPORT
    |	CREATE
    |	USE
    |	ALL
;

local_methods:
	local_method { $$ = add_local_method(make_local_methods(), $1); }
    |	local_methods COMMA local_method
	{ free($2); $$ = add_local_method($1, $3); }
;

local_method:
	method_opt named_method { $$ = $2; }
;

keyword:
	KEYWORD
    |	PREFIX_KEYWORD
    |	IMPORT_KEYWORD
    |	EXCLUDE_KEYWORD
    |	EXPORT_KEYWORD
    |	RENAME_KEYWORD
    |	UNTIL_KEYWORD
    |	WHILE_KEYWORD
;

expression:
	keyword { $$ = make_literal_ref(parse_keyword_token($1)); }
    |	operand { $$ = $1; }
    |	operand binop_series
	{ $$ = make_binop_series_expr($1, $2); }
;

binop_series:
	binop operand { $$ = add_binop(make_binop_series(), $1, $2); }
    |	binop_series binop operand { $$ = add_binop($1, $2, $3); }
;

binop:
	BINARY_OPERATOR { $$ = make_binop(make_id($1)); }
    |	MINUS { $$ = make_binop(make_id($1)); }
    |	EQUAL { $$ = make_binop(make_id($1)); }
    |	EQUAL_EQUAL { $$ = make_binop(make_id($1)); }
;

operand:
	MINUS operand { $$ = make_negate($1->line, $2); free($1); }
    |	TILDE operand { $$ = make_not($1->line, $2); free($1); }
    |	leaf { $$ = $1; }
;

leaf:
	constant { $$ = make_literal_ref($1); }
    |	variable_name { $$ = make_varref(make_id($1)); }
    |	leaf LBRACKET arguments_opt RBRACKET
	{ free($4); $$ = make_aref_or_element($2->line, $1, $3); free($2); }
    |	leaf LPAREN arguments_opt RPAREN
	{ free($2); free($4); $$ = make_function_call($1, $3); }
    |	anonymous_method { $$ = make_method_ref($1); }
    |	leaf DOT variable_name
	{ free($2);
	  $$ = make_dot_operation($1, make_varref(make_id($3)));
	}
    |	LPAREN expression RPAREN { $$ = $2; free($1); free($3); }
    |	statement { $$ = $1; }
;

arguments_opt:
	/* epsilon */ { $$ = make_argument_list(); }
    |	arguments { $$ = $1; }
;

arguments:
	argument { $$ = add_argument(make_argument_list(), $1); }
    |	arguments COMMA argument { free($2); $$ = add_argument($1, $3); }
;

argument:
	expression { $$ = make_argument($1); }
    |	keyword expression
	{ $$ = make_keyword_argument($1, $2); }
;

constant:
	SHARP_T { $$ = parse_true_token($1); }
    |	SHARP_F { $$ = parse_false_token($1); }
    |	concat_string { $$ = $1; }
    |	CHARACTER { $$ = parse_character_token($1); }
    |	INTEGER { $$ = parse_integer_token($1); }
    |	FLOAT { $$ = parse_float_token($1); }
    |	SYMBOL_LITERAL { $$ = parse_keyword_token($1); }
    |	SHARP_PAREN dotted_list RPAREN
	{ free($1); free($3); $$ = $2; }
    |	SHARP_PAREN literals_opt RPAREN
	{ free($1); free($3); $$ = make_list_literal($2); }
    |	SHARP_BRACKET literals_opt RBRACKET
	{ free($1); free($3); $$ = make_vector_literal($2); }
;

concat_string:
	STRING 			{ $$ = parse_string_token($1); }
    |	concat_string STRING	{ $$ = concat_string_token($1, $2); }
;

dotted_list:
	literals DOT literal
	{ free($2); $$ = make_dotted_list_literal($1, $3); }
;

literals_opt:
	/* epsilon */
	{ $$ = make_literal_list(); }
    |	literals
	{ $$ = $1; }
;

literals:
	literal { $$ = add_literal(make_literal_list(), $1); }
    |	literals COMMA literal
	{ free($2); $$ = add_literal($1, $3); }
;

literal:
	constant { $$ = $1; }
    |	keyword { $$ = parse_keyword_token($1); }
;

statement:
	DBEGIN { push_yacc_recovery(END); }
	body_opt END begin_opt
	{ free($1); free($4); free($5); $$ = make_body_expr($3);
          pop_yacc_recoveries(1); }
    |	BLOCK LPAREN variable_name_opt RPAREN
	{ push_yacc_recovery(END);
	  push_yacc_recovery(CLEANUP);
	  push_yacc_recovery(EXCEPTION); }
	body block_epilog_opt END block_opt
	{ free($2); free($4); free($8); pop_yacc_recoveries(3); 
	  $$ = make_block($1->line, $3 ? make_id($3) : NULL, $6, $7);
	  free($1);
	}
    |	CASE { push_yacc_recovery(END); }
	condition_body END case_opt
	{ free($1); free($4); $$ = make_case($3);
          pop_yacc_recoveries(1); }
    |	IF LPAREN expression RPAREN { push_yacc_recovery(END);
				      push_yacc_recovery(ELSE); }
	body else_part_opt END if_opt
	{ free($1); free($2); free($4); free($8); $$ = make_if($3, $6, $7);
          pop_yacc_recoveries(2); }
    |	FOR LPAREN for_header RPAREN { push_yacc_recovery(END);
				       push_yacc_recovery(FINALLY); }
	body_opt final_part_opt END for_opt
	{ free($1); free($2); free($4); free($8); $$ = make_for($3, $6, $7);
          pop_yacc_recoveries(2); }
    |	SELECT LPAREN expression by_part_opt RPAREN {push_yacc_recovery(END);}
		condition_body END select_opt
	{ free($1);free($2);free($5);free($8); $$ = make_select($3, $4, $7);
          pop_yacc_recoveries(1); }
    |	UNLESS LPAREN expression RPAREN { push_yacc_recovery(END); }
	body END unless_opt
	{ free($1);free($2);free($4);free($7);
	  $$ = make_if($3, NULL, make_else(0, $6));
          pop_yacc_recoveries(1); }
    |	UNTIL LPAREN expression RPAREN { push_yacc_recovery(END); }
        body_opt END until_opt
	{ free($1);free($2);free($4);free($7); pop_yacc_recoveries(1);
	  $$ = make_for(make_for_header($3), $6, NULL);
	}
    |	WHILE LPAREN expression RPAREN { push_yacc_recovery(END); }
	body_opt END while_opt
	{ free($2);free($4);free($7);pop_yacc_recoveries(1);
	  $$ = make_for(make_for_header(make_not($1->line, $3)), $6, NULL);
	  free($1);
	}
;

block_epilog_opt:
	/* epsilon */ { $$ = NULL; }
    |	block_epilog { $$ = $1; }
;

block_epilog:
	exception_clauses_opt cleanup_part exception_clauses_opt
	{ $$ = make_block_epilog($1, $2, $3); }
    |	exception_clauses
	{ $$ = make_block_epilog($1, NULL, NULL); }
;

condition_body:
	complete_condition_clauses { $$ = $1; }
;

for_header:
	UNTIL expression
	{ warn($1->line, "Use UNTIL: instead of UNTIL inside for loop");
	  free($1); $$ = make_for_header($2); }
    |	UNTIL_KEYWORD expression
	{ free($1); $$ = make_for_header($2); }
    |	WHILE expression
	{ warn($1->line, "Use WHILE: instead of WHILE in for loop");
	  $$ = make_for_header(make_not($1->line, $2)); free($1); 
        }
    |	WHILE_KEYWORD expression
	{ $$ = make_for_header(make_not($1->line, $2)); free($1); }
    |	for_clause
	{ $$ = push_for_clause($1, make_for_header(NULL)); }
    |	for_clause COMMA for_header
	{ free($2); $$ = push_for_clause($1, $3); }
;

exception_clauses_opt:
	/* epsilon */ { $$ = NULL; }
    |	exception_clauses { $$ = $1; }
;

exception_clauses:
	exception_clause
	{ $$ = add_exception_clause(make_exception_clauses(), $1); }
    |	exception_clauses exception_clause
	{ $$ = add_exception_clause($1, $2); }
;

exception_clause:
	EXCEPTION variable_name body
	{ 
	  warn($1->line, "Token following EXCEPTION must be surrounded "
	       "by parentheses");
	  free($1);
	  $$ = make_exception_clause(make_varref(make_id($2)),
				     NULL, NULL, $3);
	}
    |	EXCEPTION LPAREN expression property_list_opt RPAREN body
	{ free($1); free($2); free($5);
	  $$ = make_exception_clause($3, NULL, $4, $6);
	}
    |	EXCEPTION LPAREN variable_name COLON_COLON expression
		property_list_opt RPAREN body
	{ free($1); free($2); free($4); free($7);
	  $$ = make_exception_clause($5, make_id($3), $6, $8);
	}
;

complete_condition_clauses:
	OTHERWISE arrow_opt body
	{ free($1);
	  $$ = push_condition_clause(make_otherwise_condition_clause($3),
				     NULL);
	}
    |	condition_clause semi_opt
	{ $$ = push_condition_clause($1, NULL); }
    |	condition_clause SEMI complete_condition_clauses
	{ free($2); $$ = push_condition_clause($1, $3); }
    |	condition_clause SEMI incomplete_condition_clauses
	{ free($2); $$ = complete_condition_clauses($1, $3); }
;

incomplete_condition_clauses:
	constituent semi_opt
	{ $$ = make_incomplete_condition_clauses($1, NULL); }
    |	constituent SEMI complete_condition_clauses
	{ free($2); $$ = make_incomplete_condition_clauses($1, $3); }
    |	constituent SEMI incomplete_condition_clauses
	{ free($2); $$ = push_condition_constituent($1, $3); }
;

condition_clause:
	expression ARROW constituent
	{ free($2); $$ = push_condition($1, make_condition_clause($3)); }
    |	expression COMMA condition_clause
	{ free($2); $$ = push_condition($1, $3); }
;

for_clause:
	variable EQUAL expression THEN expression
	{ free($2); free($4);
	$$=make_equal_then_for_clause(push_param($1,make_param_list()),$3,$5);}
    |	LPAREN variables RPAREN EQUAL expression THEN expression
	{ free($1); free($3); free($4); free($6);
	  $$ = make_equal_then_for_clause($2, $5, $7); }
    |	variable IN expression 
	{ free($2); $$ = make_in_for_clause($1, NULL, $3, NULL); }
    |	variable KEYED_BY variable IN expression
	{ free($2); free($4); $$ = make_in_for_clause($1, $3, $5, NULL); }
    |	variable IN expression USING variable
	{ free($2); free($4); $$ = make_in_for_clause($1, NULL, $3, $5); }
    |	variable KEYED_BY variable IN expression USING variable
	{ free($2); free($4); $$ = make_in_for_clause($1, $3, $5, $7); }
    |	variable FROM expression to_part_opt by_part_opt
	{ free($2); $$ = make_from_for_clause($1, $3, $4, $5); }
;

by_part:	BY expression { free($1); $$ = $2; } ;
cleanup_part:	CLEANUP body { free($1); $$ = $2; } ;
final_part:	FINALLY body { free($1); $$ = $2; } ;

else_part:
	ELSE body { $$ = make_else($1->line, $2); free($1); }
    |	ELSEIF LPAREN expression RPAREN body else_part_opt
	{ free($2); free($4);
	  $$ = make_else($1->line, make_expr_body(make_if($3, $5, $6)));
	  free($1); 
	}
;

to_part_opt:
	/* epsilon */ { $$ = NULL; }
    |	TO expression { free($1); $$ = make_to($2); }
    |	ABOVE expression { free($1); $$ = make_above($2); }
    |	BELOW expression { free($1); $$ = make_below($2); }
;

class_definition:
	variable_name LPAREN superclasses RPAREN
		class_guts_opt END class_opt symbol_opt
	{ struct id *id = make_id($1);
	  verify_symbol(id, $8); free($2); free($4); free($6);
	  $$ = make_class_definition(id, $3, $5);
	}
;

superclasses:
	expression
	{ $$ = add_superclass(make_superclass_list(), $1); }
    |	superclasses COMMA expression
	{ free($2); $$ = add_superclass($1, $3); }
;

class_guts_opt:
	/* epsilon */ { $$ = NULL; }
    |	class_guts semi_opt { $$ = $1; }
;

class_guts:
	slot_spec
	{ $$ = add_slot_spec(make_class_guts(), $1); }
    |	initarg_spec
	{ $$ = add_initarg_spec(make_class_guts(), $1); }
    |	inherited_spec
	{ $$ = add_inherited_spec(make_class_guts(), $1); }
    |	class_guts SEMI slot_spec
	{ free($2); $$ = add_slot_spec($1, $3); }
    |	class_guts SEMI initarg_spec
	{ free($2); $$ = add_initarg_spec($1, $3); }
    |	class_guts SEMI inherited_spec
	{ free($2); $$ = add_inherited_spec($1, $3); }
;

slot_spec:
	slot_adjectives allocation SLOT variable_name slot_type_opt 
		slot_init_expr_opt property_list_opt
	{
	    int line = $3->line;
	    free($3);
	    $$ = make_slot_spec(line, $1, $2, $4 ? make_id($4) : NULL, 
				$5, $6, $7);
	}
;

slot_adjectives:
	/* epsilon */ { $$ = 0; }
    |	flags SEALED { free($2); $$ = $1 | flag_SEALED; }
    |	flags CONSTANT { free($2); $$ = $1 | flag_CONSTANT; }
;	

slot_init_expr_opt:
	/* epsilon */ { $$ = NULL; }
    |	EQUAL expression { free($1); $$ = $2; }
;

slot_type_opt:
	/* epsilon */ { $$ = NULL; }
    |	COLON_COLON operand { free($1); $$ = $2; }
;

initarg_spec:
	required_opt KEYWORD_RESERVED_WORD KEYWORD property_list_opt
	{ free($2); $$ = make_initarg_spec($1, $3, $4); }
;

required_opt:
	REQUIRED { free($1); $$ = TRUE; }
    |	/* epsilon */ { $$ = FALSE; }
;

inherited_spec:
	INHERITED SLOT variable_name slot_init_expr_opt property_list_opt
	{ 	    
	    int line = $3->line;
	    free($1); free($2); 
	    $$ = make_inherited_spec(line, make_id($3), $4, $5); 
	}
;


allocation:
	/* epsilon */ { $$ = alloc_INSTANCE; }
    |	INSTANCE { free($1); $$ = alloc_INSTANCE; }
    |	CLASS { free($1); $$ = alloc_CLASS; }
    |	EACH_SUBCLASS { free($1); $$ = alloc_EACH_SUBCLASS; }
    |	VIRTUAL { free($1); $$ = alloc_VIRTUAL; }
;

property_list_opt:
	/* epsilon */ { $$ = make_property_list(); }
    |	property_list { $$ = $1; }
;

property_list:
	COMMA keyword expression
	{ free($1); $$ = add_property(make_property_list(), $2, $3); }
    |	property_list COMMA keyword expression
	{ free($2); $$ = add_property($1, $3, $4); }
;

sealed_domain:
	variable_name LPAREN arguments_opt RPAREN
	{ free($2); free($4);
	  $$ = make_sealed_domain(make_id($1), $3);
	}
;

generic_function_definition:
	variable_name LPAREN gf_parameters RPAREN gf_suffix
	{ free($2); free($4);
	  $$ = make_define_generic(make_id($1), $3, $5);
	}
;

gf_suffix:
	property_list_opt
	{ $$ = make_gf_suffix(NULL, $1); }
    |	ARROW return_type_element property_list_opt
	{ free($1);
	  $$ = make_gf_suffix
	          (add_return_type(make_return_type_list(FALSE, NULL), $2),
		   $3);
	}
    |	ARROW LPAREN return_type_list RPAREN property_list_opt
	{ free($1); free($2); free($4); $$ = make_gf_suffix($3, $5); }
;

gf_parameters:
	/* epsilon */ { $$ = make_param_list(); }
    |	positional_parameter more_gf_parameters
	{ $$ = push_param($1, $2); }
    |	gf_rest_parameters { $$ = $1; }
;

more_gf_parameters:
	/* epsilon */
	{ $$ = make_param_list(); }
    |	COMMA positional_parameter more_gf_parameters
	{ free($1); $$ = push_param($2, $3); }
    |	COMMA gf_rest_parameters
	{ free($1); $$ = $2; }
;

gf_rest_parameters:
	rest_parameter
	{ $$ = set_rest_param(make_param_list(), $1); }
    |	rest_parameter COMMA gf_keyword_parameters_list
	{ free($2); $$ = set_rest_param($3, $1); }
    |	gf_keyword_parameters_list
	{ $$ = $1; }
;

gf_keyword_parameters_list:
	KEY gf_keyword_parameters_opt { free($1); $$ = $2; }
;

gf_keyword_parameters_opt:
	/* epsilon */ { $$ = allow_keywords(make_param_list()); }
    |	COMMA ALL_KEYS
	{ free($1); free($2); $$ = allow_all_keywords(make_param_list()); }
    |	gf_keyword_parameters { $$ = $1; }
;

gf_keyword_parameters:
	gf_keyword_parameter
	{ $$ = push_keyword_param($1, allow_keywords(make_param_list())); }
    |	gf_keyword_parameter COMMA ALL_KEYS
	{ free($2); free($3);
	  $$ = push_keyword_param($1, allow_all_keywords(make_param_list())); }
    |	gf_keyword_parameter COMMA gf_keyword_parameters
	{ free($2); $$ = push_keyword_param($1, $3); }
;

gf_keyword_parameter:
	keyword variable_name_opt keyword_parameter_type
	{ $$ = make_keyword_param($1, $2 ? make_id($2) : NULL, $3, NULL); }
    |	variable_name keyword_parameter_type
	{ $$ = make_keyword_param(NULL, make_id($1), $2, NULL); }
;

anonymous_method:
	METHOD method_description END method_opt
	{ free($3); $$ = set_method_source($1, $2); }
;

named_function:
	variable_name method_description END FUNCTION FUNCTION
	{ struct id *id = make_id($1);
	  free($3); free($4); verify_symbol(id, $5);
	  $$ = set_method_name(id, $2);
	}
      | variable_name method_description END function_opt any_variable_name_except_function_opt
	{ struct id *id = make_id($1);
	  free($3); verify_symbol(id, $5);
	  $$ = set_method_name(id, $2);
	}
;

named_method:
	variable_name method_description END method_opt variable_name_opt
	{ struct id *id = make_id($1);
	  free($3); verify_symbol(id, $5);
	  $$ = set_method_name(id, $2);
	}
;

method_description:
	LPAREN parameters RPAREN return_type body
	{ free($1); free($3);
	  $$ = make_method_description($2, $4, $5);
	}
    |	LPAREN parameters RPAREN return_type
	{ free($1); free($3);
	  $$ = make_method_description($2, $4, make_body());
	}
    |	LPAREN parameters RPAREN semi_opt body_opt
	{ free($1); free($3);
	  $$ = make_method_description($2, NULL, $5);
	}
;

return_type:
	ARROW return_type_list SEMI
	{ free($1); free($3); $$ = $2; }
    |	ARROW LPAREN return_type_list RPAREN semi_opt
	{ free($1); free($2); free($4); $$ = $3; }
;

return_type_list:
	/* epsilon */
	{ $$ = make_return_type_list(FALSE, NULL); }
    |	REST return_type_element
	{ free($1); $$ = make_return_type_list(TRUE, $2); }
    |	return_type_list_head
	{ $$ = $1; }
    |	return_type_list_head COMMA REST return_type_element
	{ free($2); free($3); $$ = set_return_type_rest_type($1, $4); }
;

return_type_list_head:
	return_type_element
	{ $$ = add_return_type(make_return_type_list(FALSE, NULL), $1); }
    |	return_type_list_head COMMA return_type_element
	{ free($2); $$ = add_return_type($1, $3); }
;

return_type_element:
	variable_name 
        { warn($1->line, "Return value has name but no type");
	  free($1); $$ = NULL; }
    |	variable_name COLON_COLON expression { free($1); free($2); $$ = $3; }
;

parameters:
	/* epsilon */ { $$ = make_param_list(); }
    |	positional_parameter more_parameters { $$ = push_param($1, $2); }
    |	next_parameters { $$ = $1; }
;

more_parameters:
	/* epsilon */
	{ $$ = make_param_list(); }
    |	COMMA positional_parameter more_parameters
	{ free($1); $$ = push_param($2, $3); }
    |	COMMA next_parameters
	{ free($1); $$ = $2; }
;

positional_parameter:
	variable_name
	{ $$ = make_param(make_id($1), NULL); }
    |	variable_name COLON_COLON expression
	{ free($2); $$ = make_param(make_id($1), $3); }
    |	variable_name EQUAL_EQUAL expression
	{ $$ = make_param(make_id($1), make_singleton($2->line, $3));
	  free($2); }
;

next_parameters:
	next_parameter
	{ $$ = set_next_param(make_param_list(), $1); }
    |	next_parameter COMMA rest_parameters
	{ free($2); $$ = set_next_param($3, $1); }
    |	rest_parameters
	{ $$ = $1; }
;

next_parameter:
	NEXT variable_name { free($1); $$ = make_id($2); }
;

rest_parameters:
	rest_parameter
	{ $$ = set_rest_param(make_param_list(), $1); }
    |	rest_parameter COMMA keyword_parameters_list
	{ free($2); $$ = set_rest_param($3, $1); }
    |	keyword_parameters_list
	{ $$ = $1; }
;

rest_parameter:
	REST variable_name { free($1); $$ = make_id($2); }
;

keyword_parameters_list:
	KEY keyword_parameters_opt
	{ free($1); $$ = $2; }
;

keyword_parameters_opt:
	/* epsilon */ { $$ = allow_keywords(make_param_list()); }
    |	COMMA ALL_KEYS
	{ free($1); free($2); $$ = allow_all_keywords(make_param_list()); }
    |	keyword_parameters { $$ = $1; }
;

keyword_parameters:
	keyword_parameter
	{ $$ = push_keyword_param($1, allow_keywords(make_param_list())); }
    |	keyword_parameter COMMA ALL_KEYS
	{ free($2); free($3);
	  $$ = push_keyword_param($1, allow_all_keywords(make_param_list())); }
    |	keyword_parameter COMMA keyword_parameters
	{ free($2); $$ = push_keyword_param($1, $3); }
;

keyword_parameter:
	keyword_opt variable_name keyword_parameter_type
		keyword_parameter_default
	{ $$ = make_keyword_param($1, make_id($2), $3, $4); }
    |	keyword_opt variable_name LPAREN expression RPAREN
	{ warn($2->line,
	       "``foo (val)'' keyword default syntax obsolete.  "
	       "Use ``foo = val'' instead.");
	  free($3); free($5);
	  $$ = make_keyword_param($1, make_id($2), NULL, $4); }
;

keyword_opt:
	/* epsilon */ { $$ = NULL; }
    |	keyword { $$ = $1; }
;	

keyword_parameter_type:
	/* epsilon */ { $$ = NULL; }
    |	COLON_COLON operand { free($1); $$ = $2; }
;

keyword_parameter_default:
	/* epsilon */ { $$ = NULL; }
    |	EQUAL expression { free($1); $$ = $2; }
;

module_definition:
	variable_name module_clauses_opt END module_opt symbol_opt
	{ if ($5) {
	      if (strcasecmp((char *)$1->chars, (char *)$5->chars) != 0) {
		  error($5->line, "mismatched name, ``%s'' isn't ``%s''",
			$5->chars, $1->chars);
		  yacc_recover();
	      }
	      free($5);
	  } 
	  free($3);
	  $$ = (struct constituent *) set_namespace_name($2, $1);
	}
;

module_opt:
	/* empty */ 		{ $$ = NULL; }
    |	MODULE			{ free($1); $$ = NULL; }
;

module_clauses_opt:
	/* empty */		{ $$ = make_define_module(); }
    |	module_clauses
    |	module_clauses SEMI	{ $$ = $1; free($2); }
;

module_clauses:
	use_clause
	{ $$ = add_use_clause(make_define_module(), $1); }
    |	export_clause
	{ $$ = add_exports(make_define_module(), $1); }
    |	create_clause
	{ $$ = add_creates(make_define_module(), $1); }
    |	module_clauses SEMI use_clause
	{ $$ = add_use_clause($1, $3); free($2); }
    |	module_clauses SEMI export_clause
	{ $$ = add_exports($1, $3); free($2); }
    |	module_clauses SEMI create_clause
	{ $$ = add_creates($1, $3); free($2); }
;

export_clause:
	EXPORT variable_names	{ free($1); $$ = $2; }
;

create_clause:
	CREATE variable_names	{ free($1); $$ = $2; }
;

use_clause:
	USE variable_name module_use_options
	{ free($1); $$ = make_use_clause($2, $3); }
;

module_use_options:
	/* empty */		{ $$ = make_use_options(); }
    |	module_use_options COMMA module_use_option
	{ free($2); $$ = add_use_option($1, $3); }
;

module_use_option:
	prefix_option
    |	import_option
    |	exclude_option
    |	rename_option
    |	export_option
;

prefix_option:
	PREFIX_KEYWORD STRING
	{ $$ = make_prefix_option($2); free($1); }
;

import_option:
	IMPORT_KEYWORD ALL
	{ $$ = make_use_option(useopt_IMPORT_ALL); free($1); free($2); }
    |	IMPORT_KEYWORD LBRACE imports_opt RBRACE
	{ $$ = (struct use_option *) $3; free($1); free($2); free($4); }
;

imports_opt:
	/* empty */		{ $$ = make_import_option(); }
    |	imports			{ $$ = $1; }
;

imports:
	variable_name
	{ $$ = add_import(make_import_option(), $1, NULL); }
    |	variable_name ARROW variable_name
	{ $$ = add_import(make_import_option(), $1, $3); free($2); }
    |	imports COMMA variable_name
	{ $$ = add_import($1, $3, NULL); free($2); }
    |	imports COMMA variable_name ARROW variable_name	
	{ $$ = add_import($1, $3, $5); free($2); free($4); }
;

exclude_option:
	EXCLUDE_KEYWORD variable_name_set
	{ $$ = make_exclude_option($2); free($1); }
;

export_option:
	EXPORT_KEYWORD ALL
	{ $$ = make_use_option(useopt_EXPORT_ALL); free($1); free($2); }
    |	EXPORT_KEYWORD variable_name_set
	{ $$ = make_export_option($2); free($1); }
;

rename_option:
	RENAME_KEYWORD LBRACE RBRACE
	{ $$ = make_rename_option(make_renamings());
	  free($1); free($2); free($3); }
    |	RENAME_KEYWORD LBRACE rename_specs RBRACE
	{ $$ = make_rename_option($3); free($1); free($2); free($4); }
;

rename_specs:
	variable_name ARROW variable_name
	{ $$ = add_renaming(make_renamings(), $1, $3); free($2); }
    |	rename_specs COMMA variable_name ARROW variable_name
	{ $$ = add_renaming($1, $3, $5); free($2); free($4); }
;

variable_name_set:
	LBRACE RBRACE	{ $$ = make_variable_names(); free($1); free($2); }
    |	LBRACE variable_names RBRACE
	{ $$ = $2; free($1); free($3); }
;

variable_names:
	variable_name
	{ $$ = add_variable_name(make_variable_names(), $1); }
    |	variable_names COMMA variable_name
	{ $$ = add_variable_name($1, $3); free($2); }
;

library_definition:
	variable_name library_clauses_opt END library_opt symbol_opt
	{ if ($5) {
	      if (strcasecmp((char *)$1->chars, (char *)$5->chars) != 0) {
		  error($5->line, "mismatched name, ``%s'' isn't ``%s''",
			$5->chars, $1->chars);
		  yacc_recover();
	      }
	      free($5);
	  } 
	  free($3);
	  $$ = (struct constituent *) set_namespace_name($2, $1);
	}
;

library_opt:
	/* empty */ 		{ $$ = NULL; }
    |	LIBRARY			{ free($1); $$ = NULL; }
;

library_clauses_opt:
	/* empty */		{ $$ = make_define_library(); }
    |	library_clauses
    |	library_clauses SEMI	{ $$ = $1; free($2); }
;

library_clauses:
	use_clause
	{ $$ = add_use_clause(make_define_library(), $1); }
    |	export_clause
	{ $$ = add_exports(make_define_library(), $1); }
    |	library_clauses SEMI use_clause
	{ $$ = add_use_clause($1, $3); free($2); }
    |	library_clauses SEMI export_clause
	{ $$ = add_exports($1, $3); free($2); }
;


begin_opt:	{ $$ = NULL; } | DBEGIN { free($1); $$ = NULL; } ;
block_opt:	{ $$ = NULL; } | BLOCK { free($1); $$ = NULL; } ;
case_opt:	{ $$ = NULL; } | CASE { free($1); $$ = NULL; } ;
if_opt:		{ $$ = NULL; } | IF { free($1); $$ = NULL; } ;
for_opt:	{ $$ = NULL; } | FOR { free($1); $$ = NULL; } ;
select_opt:	{ $$ = NULL; } | SELECT { free($1); $$ = NULL; } ;
unless_opt:	{ $$ = NULL; } | UNLESS { free($1); $$ = NULL; } ;
until_opt:	{ $$ = NULL; } | UNTIL { free($1); $$ = NULL; } ;
while_opt:	{ $$ = NULL; } | WHILE { free($1); $$ = NULL; } ;
class_opt:	{ $$ = NULL; } | CLASS { free($1); $$ = NULL; } ;
function_opt:	{ $$ = NULL; } | FUNCTION { free($1); $$ = NULL; } ;
method_opt:	{ $$ = NULL; } | METHOD { free($1); $$ = NULL; } ;
semi_opt:	{ $$ = NULL; } | SEMI { free($1); $$ = NULL; } ;
arrow_opt:	{ $$ = NULL; } | ARROW { free($1); $$ = NULL; } ;

symbol_opt:	{ $$ = NULL; } | SYMBOL { $$ = $1; } ;
variable_name_opt: { $$ = NULL; } | variable_name { $$ = $1; } ;
any_variable_name_except_function_opt:
              /* epsilon */                       { $$ = NULL; } 
              | any_variable_name_except_function { $$ = $1; } 
;

by_part_opt:	{ $$ = NULL; } | by_part { $$ = $1; } ;
else_part_opt:	{ $$ = NULL; } | else_part { $$ = $1; } ;
final_part_opt:	{ $$ = NULL; } | final_part { $$ = $1; } ;

%%

static void yyerror(char *msg)
{
    if (yylval.token)
	error(yylval.token->line, "%s at or before `%s'\n",
	      msg, yylval.token->chars);
    else
	error(line_count, "%s at end-of-file\n", msg);
    yacc_recover();
}

static boolean verify_symbol_aux(struct id *id, struct token *token)
{
    if (token) {
	int line = token->line;
	char *ptr = (char *)token->chars;

	if (*ptr == '\\')
	    ptr++;

	if (strcasecmp((char *)id->symbol->name, ptr)) {
	    error(line, "mismatched name, ``%s'' isn't ``%s''",
		  token->chars, id->symbol->name);
	    free(token);
	    return TRUE;
	}
	else
	    free(token);
    }
    return FALSE;
}

static void yacc_recover()
{
    while (yychar) {
	struct token_list *rlist = yacc_recovery_list;
	while (rlist)
	    if (rlist->token == yychar)
		return;
	    else
		rlist = rlist->next;
	yychar = yylex();
    }
    yyclearin;
}

static void push_yacc_recovery(int token)
{
    struct token_list *newrec;

    newrec = malloc(sizeof(*newrec));
    newrec->token = token;
    newrec->next = yacc_recovery_list;
    yacc_recovery_list = newrec;
}

static void pop_yacc_recoveries(int count)
{
    for ( ; count-- > 0; )
	yacc_recovery_list = yacc_recovery_list->next;
}

struct token *make_token(char *ptr, int len)
{
    struct token *token = malloc(sizeof(struct token) 
				 + len + 1 - sizeof(token->chars));

    token->length = len;
    memcpy(token->chars, ptr, len);
    token->line = line_count;
    token->chars[len] = 0;

    return token;
}

