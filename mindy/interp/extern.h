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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/extern.h,v 1.2 1994/11/30 16:18:14 rgs Exp $
*
* This file exports definitions for support of raw C pointers.
*
\**********************************************************************/


struct c_pointer {
    obj_t class;
    void *pointer;
};

#define C_PTR(o) obj_ptr(struct c_pointer *, o)

struct symtab {
    char *name;
    void *ptr;
};

struct foreign_file {
    obj_t class;
    obj_t file_name;		/* relocatable object file */
    int sym_count;
    int extra_size;
    struct symtab syms[1];
};

#define FOREIGN_FILE(o) obj_ptr(struct foreign_file *, o)

extern obj_t obj_CPointerClass;
extern obj_t /* <foreign-file> */ mindy_explicit_syms;
extern obj_t obj_ForeignFileClass;

extern obj_t make_c_pointer(obj_t /* <static-pointer-class> */ cls, void *ptr);
extern void *get_c_object(obj_t obj);
extern obj_t convert_c_object(obj_t class, void *obj, boolean miss_ok);
extern int scav_c_pointer(struct object *obj);
extern obj_t trans_c_pointer(obj_t cptr);
