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


extern obj_t obj_FunctionClass;
extern obj_t obj_MethodInfoClass;
extern obj_t obj_MethodClass;
extern obj_t obj_ByteMethodClass;
extern obj_t obj_GFClass;
extern obj_t obj_CFunctionClass;

struct method_info {
    obj_t class;
    bool restp;
    obj_t keys;
    bool all_keys;
    obj_t component;
    int n_closure_vars;
};

#define METHOD_INFO(o) obj_ptr(struct method_info *, o)

extern obj_t make_raw_function(const char *debug_name, obj_t specializers,
                               bool restp, obj_t keywords, bool all_keys,
                               obj_t result_types, obj_t more_results_type,
                               void (*xep)(struct thread *thread, int nargs));

extern obj_t make_raw_method(const char *debug_name, obj_t specializers,
                             bool restp, obj_t keywords, bool all_keys,
                             obj_t result_types, obj_t more_results_type,
                             void (*iep)(obj_t self, struct thread *thread,
                                         obj_t *args));
extern void set_method_iep(obj_t method,
                           void (*iep)(obj_t self, struct thread *thread,
                                       obj_t *args));
extern obj_t make_builtin_method(const char *debug_name, obj_t specializers,
                                 bool restp, obj_t keys, bool all_keys,
                                 obj_t result_type, obj_t (*func)());
extern obj_t make_method_info(bool rest_p, obj_t keys, bool all_keys,
                              obj_t component, int n_closure_vars);
extern obj_t make_byte_method(obj_t method_info, obj_t specializers,
                              obj_t result_types, obj_t more_results_type,
                              obj_t *lexenv);
extern obj_t byte_method_component(obj_t byte_method);

extern obj_t make_accessor_method(obj_t debug_name, obj_t class, obj_t type,
                                  bool setter, obj_t datum,
                                  void (*iep)(obj_t self, struct thread *thread,
                                              obj_t *args));
extern obj_t accessor_method_datum(obj_t method);
extern void set_accessor_method_datum(obj_t method, obj_t datum);

extern obj_t make_c_function(obj_t debug_name, void *pointer);

extern obj_t make_generic_function(obj_t debug_name, obj_t specializers,
                                   bool restp, obj_t keys, bool all_keys,
                                   obj_t result_types, obj_t more_results_type);
extern obj_t make_default_generic_function(obj_t debug_name, obj_t method);
extern void set_gf_signature(obj_t gf, obj_t specializers, bool restp,
                             obj_t keys, bool all_keys, obj_t result_types,
                             obj_t more_results_type);

extern obj_t generic_function_methods(obj_t gf);
extern obj_t generic_function_methods_clock(obj_t gf);
extern obj_t add_method(obj_t gf, obj_t method);

extern void invoke(struct thread *thread, int nargs);
extern obj_t *push_linkage(struct thread *thread, obj_t *args);
extern void set_c_continuation(struct thread *thread,
                               void (*cont)(struct thread *thread, obj_t *vals));
extern obj_t *pop_linkage(struct thread *thread);
#ifdef MINDY_SLOW_LONGJMP
#define do_return do_return_setup
#else
extern void do_return(struct thread *thread, obj_t *old_sp, obj_t *vals);
#endif
extern void do_return_setup(struct thread *thread, obj_t *old_sp, obj_t *vals);

extern obj_t function_debug_name(obj_t func);
extern obj_t function_debug_name_or_self(obj_t func);
extern obj_t function_keywords(obj_t func);
extern bool function_all_keywords_p(obj_t func);

extern obj_t function_specializers(obj_t method);

extern obj_t invoke_simple_method(struct thread *thread, obj_t method, obj_t arg_list);
