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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/init.c,v 1.2 1994/03/26 07:46:11 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/



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

    init_obj_functions();
    init_type_functions();
    init_sym_functions();
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

    done_initializing_vars();

    init_loader();
    init_interpreter();
}
