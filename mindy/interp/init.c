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
* $Header: /scm/cvs/src/mindy/interp/init.c,v 1.1 1998/05/03 19:55:13 andreas Exp $
*
* This file initializes everything else.
*
\**********************************************************************/

#include "init.h"

extern void make_class_classes(void);
extern void make_obj_classes(void);
extern void make_type_classes(void);
extern void make_bool_classes(void);
extern void make_sym_classes(void);
extern void make_func_classes(void);
extern void make_coll_classes(void);
extern void make_str_classes(void);
extern void make_vec_classes(void);
extern void make_list_classes(void);
extern void make_char_classes(void);
extern void make_num_classes(void);
extern void make_interp_classes(void);
extern void make_thread_classes(void);
extern void make_module_classes(void);
extern void make_value_classes(void);
extern void make_nlx_classes(void);
extern void make_handler_classes(void);
extern void make_instance_classes(void);
extern void make_buffer_classes(void);
extern void make_weak_classes(void);
extern void make_table_classes(void);
extern void make_c_classes(void);

extern void init_nil(void);
extern void init_symbol_tables(void);
extern void init_modules(void);

extern void init_obj_classes(void);
extern void init_type_classes(void);
extern void init_class_classes(void);
extern void init_bool_classes(void);
extern void init_sym_classes(void);
extern void init_func_classes(void);
extern void init_coll_classes(void);
extern void init_str_classes(void);
extern void init_vec_classes(void);
extern void init_list_classes(void);
extern void init_char_classes(void);
extern void init_num_classes(void);
extern void init_interp_classes(void);
extern void init_thread_classes(void);
extern void init_module_classes(void);
extern void init_value_classes(void);
extern void init_nlx_classes(void);
extern void init_handler_classes(void);
extern void init_instance_classes(void);
extern void init_buffer_classes(void);
extern void init_weak_classes(void);
extern void init_table_classes(void);
extern void init_c_classes(void);

extern void init_obj_functions(void);
extern void init_type_functions(void);
extern void init_sym_functions(void);
extern void init_coll_functions(void);
extern void init_list_functions(void);
extern void init_str_functions(void);
extern void init_vec_functions(void);
extern void init_nlx_functions(void);
extern void init_misc_functions(void);
extern void init_def_functions(void);
extern void init_debug_functions(void);
extern void init_char_functions(void);
extern void init_num_functions(void);
extern void init_handler_functions(void);
extern void init_error_functions(void);
extern void init_instance_functions(void);
extern void init_func_functions(void);
extern void init_class_functions(void);
extern void init_print_functions(void);
extern void init_input_functions(void);
extern void init_thread_functions(void);
extern void init_table_functions(void);
extern void init_fd_functions(void);
extern void init_buffer_functions(void);
extern void init_weak_functions(void);
extern void init_c_functions(void);
extern void init_load_functions(void);
extern void init_gc_functions(void);
extern void init_bool_functions(void);
extern void init_module_functions(void);

extern void done_initializing_vars(void);

extern void init_driver(void);
extern void init_loader(void);
extern void init_interpreter(void);


void init(void)
{
    make_class_classes();
    make_obj_classes();
    make_type_classes();
    make_bool_classes();
    make_sym_classes();
    make_func_classes();
    make_coll_classes();
    make_str_classes();
    make_vec_classes();
    make_list_classes();
    make_char_classes();
    make_num_classes();
    make_interp_classes();
    make_thread_classes();
    make_module_classes();
    make_value_classes();
    make_nlx_classes();
    make_handler_classes();
    make_instance_classes();
    make_buffer_classes();
    make_weak_classes();
    make_table_classes();
    make_c_classes();

    init_nil();
    init_symbol_tables();
    init_modules();

    init_obj_classes();
    init_type_classes();
    init_class_classes();
    init_bool_classes();
    init_sym_classes();
    init_func_classes();
    init_coll_classes();
    init_str_classes();
    init_vec_classes();
    init_list_classes();
    init_char_classes();
    init_num_classes();
    init_interp_classes();
    init_thread_classes();
    init_module_classes();
    init_value_classes();
    init_nlx_classes();
    init_handler_classes();
    init_instance_classes();
    init_buffer_classes();
    init_weak_classes();
    init_table_classes();
    init_c_classes();

    init_obj_functions();
    init_type_functions();
    init_sym_functions();
    init_coll_functions();
    init_list_functions();
    init_str_functions();
    init_vec_functions();
    init_nlx_functions();
    init_misc_functions();
    init_def_functions();
    init_debug_functions();
    init_char_functions();
    init_num_functions();
    init_handler_functions();
    init_error_functions();
    init_instance_functions();
    init_func_functions();
    init_class_functions();
    init_print_functions();
    init_input_functions();
    init_thread_functions();
    init_table_functions();
    init_fd_functions();
    init_buffer_functions();
    init_weak_functions();
    init_c_functions();
    init_load_functions();
    init_gc_functions();
    init_bool_functions();
    init_module_functions();

    done_initializing_vars();

    init_driver();
    init_loader();
    init_interpreter();
}
