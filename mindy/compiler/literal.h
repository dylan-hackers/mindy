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
\**********************************************************************/



enum literal_kind {
    literal_SYMBOL, literal_INTEGER, literal_SINGLE_FLOAT,
    literal_DOUBLE_FLOAT, literal_EXTENDED_FLOAT, literal_CHARACTER,
    literal_STRING, literal_LIST, literal_VECTOR, literal_TRUE, literal_FALSE,
    literal_UNBOUND, literal_Kinds
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

struct single_float_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    float value;
};

struct double_float_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    double value;
};

struct extended_float_literal {
    enum literal_kind kind;
    struct literal *next;
    int line;
    long double value;
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
    unsigned char chars[4];
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
