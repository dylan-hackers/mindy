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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/literal.h,v 1.4 1994/04/09 00:09:03 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



enum literal_kind {
    literal_SYMBOL, literal_INTEGER, literal_FLOAT,
    literal_CHARACTER, literal_STRING, literal_LIST, literal_VECTOR,
    literal_TRUE, literal_FALSE, literal_UNBOUND, literal_Kinds
};

struct literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
};

struct symbol_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    struct symbol *symbol;
};

struct integer_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    long value;
};

struct float_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    double value;
};

struct character_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    char value;
};

struct string_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    int length;
    unsigned char chars[0];
};

struct vector_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    struct literal *first;
};

struct list_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    struct literal *first;
    struct literal *tail;
};

struct literal_list;

extern struct literal *make_unbound_literal(void);
extern struct literal *make_true_literal(void);
extern struct literal *make_false_literal(void);
extern struct literal *make_string_literal(char *str);
extern struct literal *make_character_literal(int c);
extern struct literal *make_integer_literal(long value);
extern struct literal *make_float_literal(double value);
extern struct literal *make_symbol_literal(struct symbol *sym);
extern struct literal *make_list_literal(struct literal_list *guts);
extern struct literal *make_vector_literal(struct literal_list *guts);
extern struct literal *make_dotted_list_literal(struct literal_list *list,
						struct literal *tail);
extern struct literal_list *make_literal_list(void);
extern struct literal_list *add_literal(struct literal_list *list,
					struct literal *literal);

extern void free_literal(struct literal *literal);
extern struct literal *dup_literal(struct literal *literal);
