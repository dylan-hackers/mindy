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
* $Header: /scm/cvs/src/mindy/interp/extern.c,v 1.1 1998/05/03 19:55:17 andreas Exp $
*
* This file provides support for manipulating native C pointers.
*
\**********************************************************************/

#include "../compat/std-c.h"

#include "mindy.h"
#include "gc.h"
#include "obj.h"
#include "bool.h"
#include "char.h"
#include "list.h"
#include "type.h"
#include "class.h"
#include "def.h"
#include "sym.h"
#include "module.h"
#include "error.h"
#include "thread.h"
#include "func.h"
#include "extern.h"
#include "num.h"
#include "str.h"
#include "print.h"
#include "coll.h"
#ifdef HAVE_LIBDLD
#   ifdef HAVE_LIBDLDELF
#      include "../compat/shl.h"
#   else
#      include <dl.h>
#   endif
#endif

obj_t obj_CPointerClass = NULL;	  /* all instances of StaticTypeClass are
				     subclasses of this one */
obj_t obj_ForeignFileClass = NULL;
obj_t obj_ArchivedFileClass = NULL;
obj_t obj_NullPointer = NULL;
obj_t /* <foreign-file> */ mindy_explicit_syms = NULL;

static obj_t /* <foreign-file> */ mindy_dynamic_syms = NULL;

obj_t make_c_pointer(obj_t /* <static-pointer-class> */ cls, void *ptr)
{
    obj_t res = alloc(cls, sizeof(struct c_pointer));

    C_PTR(res)->pointer = ptr;

    return res;
}

/* Dylan routines. */

#ifdef HAVE_LIBDLD

obj_t obj_SharedFileClass = NULL;

struct shared_file {
    obj_t class;
    obj_t file_name;		/* relocatable object file */
    int file_count;
    shl_t handles[1];
};

/* Links the named object files for dynamic loading, reads it in, and returns
   a "foreign_file" object which allows access to its symbols.  If
   names is a non-empty list (of byte-strings), then make ld "undefine"
   these names so that they will show up in the linked version. */
obj_t load_c_file(obj_t /* list */ c_files, obj_t /* list */ names)
{
    int i;
    obj_t retval = obj_False;
    struct shared_file *ret;
    shl_t handle;

    retval = alloc(obj_SharedFileClass,
		   sizeof(struct shared_file)
		     + ((length(c_files) - 1) * sizeof(shl_t)));
    ret = obj_ptr(struct shared_file *, retval);
    ret->file_count = length(c_files);
    for (i = 0; c_files != obj_Nil; i++, c_files = TAIL(c_files)) {
	handle = shl_load(string_chars(HEAD(c_files)), BIND_DEFERRED, 0);
	if (handle == NULL) {
	    error("Can't load shared library %s.", HEAD(c_files));
	};
	ret->handles[i] = handle;
	ret->file_name = HEAD(c_files);
    }
    return retval;
}

/* Reads the symtab (in some machine specific format) from the main program 
   and returns a "foreign_file" object which allows access to those symbols */
obj_t  load_program_file()
{
    obj_t retval;
    
    retval = alloc(obj_SharedFileClass, sizeof(struct shared_file));
    obj_ptr(struct shared_file *, retval)->file_name
	= make_byte_string(exec_file_name);
    obj_ptr(struct shared_file *, retval)->file_count = 1;
    obj_ptr(struct shared_file *, retval)->handles[0] = PROG_HANDLE;
    return retval;
}
#else

/* Reads the symtab (in some machine specific format) from the main program 
   and returns a "foreign_file" object which allows access to those symbols */
obj_t  load_program_file()
{
    error("Cannot do dynamic loading on this architecture -- \n"
	  "wanted to examine %s", make_byte_string(exec_file_name));
    return obj_False;
}

/* Links the named object files for dynamic loading, reads it in, and returns
   a "foreign_file" object which allows access to its symbols.  If
   names is a non-empty list (of byte-strings), then make ld "undefine"
   these names so that they will show up in the linked version. */
obj_t load_c_file(obj_t /* list */ c_files, obj_t /* list */ names)
{
    error("Cannot do dynamic loading on this architecture -- tried to load %=",
	  HEAD(c_files));
    return obj_False;
}

#endif /* HAVE_LIBDLD */

static void print_foreign_file(obj_t file)
{
    printf("{<foreign-file> %s}", string_chars(FOREIGN_FILE(file)->file_name));
}

#ifdef HAVE_LIBDLD
static void print_shared_foreign_file(obj_t file)
{
    printf("{<shared-file> %s}", string_chars(FOREIGN_FILE(file)->file_name));
}
#endif /* HAVE_LIBDLD */

static void print_c_pointer(obj_t ptr)
{
    obj_t class = C_PTR(ptr)->class;
    obj_t class_name = obj_ptr(struct class *, class)->debug_name;
    char *class_str;

    if (class_name != NULL && class_name != obj_False)
	class_str = sym_name(class_name);
    else
	class_str = "<c-pointer>";

    printf("{%s 0x%08lx}", class_str, (unsigned long)(C_PTR(ptr)->pointer));
}

/* Look for an object with the given name in the named file and return a
   callable "<c-function>" object for it. */
obj_t find_c_function(obj_t /* <string> */ symbol, obj_t lookup)
{
    char *string = string_chars(symbol);
    struct symtab *syms;
    int sym_count, i;
    obj_t retval = obj_False;

    if (lookup == obj_Unbound) {
	retval = find_c_function(symbol, mindy_explicit_syms);
	if (retval != obj_False) return retval;

	if (mindy_dynamic_syms == NULL)
	    mindy_dynamic_syms = load_program_file();
	return find_c_function(symbol, mindy_dynamic_syms);
    } else if (lookup == obj_False)
	return obj_False;
    else if (!instancep(lookup, obj_ForeignFileClass)) {
	error("Keyword file: is not a <foreign-file>: %=", lookup);
	return retval;		/* make lint happy */
#ifdef HAVE_LIBDLD
    } else if (instancep(lookup, obj_SharedFileClass)) {
	shl_t *files = obj_ptr(struct shared_file *, lookup)->handles;
	int file_count = obj_ptr(struct shared_file *, lookup)->file_count;
	void *ptr;

	for (i = 0; i < file_count; i++)
	    if (shl_findsym(&files[i], string, TYPE_PROCEDURE, &ptr) == 0)
		return(make_c_function(make_byte_string(string), ptr));
	return retval;
#endif
    } else {
	syms = FOREIGN_FILE(lookup)->syms;
	sym_count = FOREIGN_FILE(lookup)->sym_count;
	for (i = 0; i < sym_count; i++)
	    if (strcmp(syms[i].name, string) == 0) {
		retval = make_c_function(make_byte_string(string),
					 syms[i].ptr);
		break;
	    }
	return retval;
    }
}

/* Look for an object with the given name in the named file and return a
   "<c-pointer>" object for it. */
obj_t find_c_ptr(obj_t /* <string> */ symbol, obj_t lookup)
{
    char *string = string_chars(symbol);
    struct symtab *syms;
    int sym_count, i;
    obj_t retval = obj_False;

    if (lookup == obj_Unbound) {
	retval = find_c_ptr(symbol, mindy_explicit_syms);
	if (retval != obj_False) return retval;

	if (mindy_dynamic_syms == NULL)
	    mindy_dynamic_syms = load_program_file();
	return find_c_ptr(symbol, mindy_dynamic_syms);
    } else if (lookup == obj_False)
	return obj_False;
    else if (!instancep(lookup, obj_ForeignFileClass)) {
	error("Keyword file: is not a <foreign-file>: %=", lookup);
	return retval;		/* make lint happy */
#ifdef HAVE_LIBDLD
    } else if (instancep(lookup, obj_SharedFileClass)) {
	shl_t *files = obj_ptr(struct shared_file *, lookup)->handles;
	int file_count = obj_ptr(struct shared_file *, lookup)->file_count;
	void *ptr;

	for (i = 0; i < file_count; i++)
	    if (shl_findsym(&files[i], string, TYPE_UNDEFINED, &ptr) == 0)
		return(make_c_pointer(obj_CPointerClass, ptr));
	return retval;
#endif
    } else {
	syms = FOREIGN_FILE(lookup)->syms;
	sym_count = FOREIGN_FILE(lookup)->sym_count;
	for (i = 0; i < sym_count; i++)
	    if (strcmp(syms[i].name, string) == 0) {
		retval = make_c_pointer(obj_CPointerClass, syms[i].ptr);
		break;
	    }
	return retval;
    }
}

/* Tries to return a version of some Dylan object which will be
   meaningful to C.  This may include a pointer, an integer, or
   something else.  We assume that it can be freely cast to and from a
   pointer. */
void *get_c_object(obj_t obj)
{
    obj_t cls = object_class(obj);

    if (object_class(cls) == obj_StaticTypeClass)
	return C_PTR(obj)->pointer;
    else if (cls == obj_IntegerClass || cls == obj_FixnumClass)
	return (void *)fixnum_value(obj);
    else if (cls == obj_ByteStringClass)
	return (void *)string_chars(obj);
    else if (cls == obj_CharacterClass)
	return (void *)(int)char_int(obj);
    else if (cls == obj_BooleanClass)
	return (void *)(obj != obj_False);
    else
	return NULL;
}

/* Tries to convert a C return value back into a dylan object. */
obj_t convert_c_object(obj_t cls, void *obj, boolean miss_ok)
{
    if (cls == obj_ObjectClass)
	return make_c_pointer(obj_CPointerClass, obj);
    else if (object_class(cls) == obj_StaticTypeClass)
	return make_c_pointer(cls, obj);
    else if (cls == obj_CFunctionClass)
	return make_c_function(make_byte_string("(unknown)"), obj);
    else if (cls == obj_IntegerClass || cls == obj_FixnumClass)
	return make_fixnum((int) obj);
    else if (cls == obj_ByteStringClass || cls == obj_StringClass)
	return make_byte_string((char *)obj);
    else if (cls == obj_CharacterClass)
	return int_char((int)obj);
    else if (cls == obj_BooleanClass)
	return obj == NULL ? obj_False : obj_True;
    else if (miss_ok)
	return make_c_pointer(obj_CPointerClass, obj);
    else {
	error("Could not coerce c_pointer to class %=", cls);
	return obj_NullPointer;
    }
}

obj_t signed_byte_at(obj_t /* <statically-typed-pointer> */ pointer,
		     obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    return make_fixnum(*((char *)((int)ptr + true_offset)));
}

obj_t signed_byte_at_setter(obj_t /* <integer> */ value,
			    obj_t /* <statically-typed-pointer> */ pointer,
			    obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    *((char *)((int)ptr + true_offset)) = fixnum_value(value);
    return value;
}

obj_t unsigned_byte_at(obj_t /* <statically-typed-pointer> */ pointer,
		       obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    return make_fixnum(*((unsigned char *)((int)ptr + true_offset)));
}

obj_t unsigned_byte_at_setter(obj_t /* <integer> */ value,
			      obj_t /* <statically-typed-pointer> */ pointer,
			      obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    *((unsigned char *)((int)ptr + true_offset)) = fixnum_value(value);
    return value;
}

obj_t signed_short_at(obj_t /* <statically-typed-pointer> */ pointer,
		      obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    return make_fixnum(*((short *)((int)ptr + true_offset)));
}

obj_t signed_short_at_setter(obj_t /* <integer> */ value,
			     obj_t /* <statically-typed-pointer> */ pointer,
			     obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    *((short *)((int)ptr + true_offset)) = fixnum_value(value);
    return value;
}

obj_t unsigned_short_at(obj_t /* <statically-typed-pointer> */ pointer,
			obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    return make_fixnum(*((unsigned short *)((int)ptr + true_offset)));
}

obj_t unsigned_short_at_setter(obj_t /* <integer> */ value,
			       obj_t /* <statically-typed-pointer> */ pointer,
			       obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    *((unsigned short *)((int)ptr + true_offset)) = fixnum_value(value);
    return value;
}

obj_t signed_long_at(obj_t /* <statically-typed-pointer> */ pointer,
		     obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    return make_fixnum(*((long *)((int)ptr + true_offset)));
}

obj_t signed_long_at_setter(obj_t /* <integer> */ value,
			    obj_t /* <statically-typed-pointer> */ pointer,
			    obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    *((long *)((int)ptr + true_offset)) = fixnum_value(value);
    return value;
}

obj_t unsigned_long_at(obj_t /* <statically-typed-pointer> */ pointer,
		       obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    return make_fixnum(*((unsigned long *)((int)ptr + true_offset)));
}

obj_t unsigned_long_at_setter(obj_t /* <integer> */ value,
			      obj_t /* <statically-typed-pointer> */ pointer,
			      obj_t /* <integer> */ offset)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    *((unsigned long *)((int)ptr + true_offset)) = fixnum_value(value);
    return value;
}

obj_t pointer_at(obj_t /* <statically-typed-pointer> */ pointer,
		 obj_t /* <integer> */ offset,
		 obj_t /* <class> */ cls)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    if (!instancep(cls, obj_StaticTypeClass))
	error("class is not statically typed pointer: %=", cls);
    /* pointer size object -- dereference as (void **) */
    return convert_c_object(cls, *(void **)((char *)ptr + true_offset),
			    FALSE);
}

obj_t pointer_at_setter(obj_t /* <statically-typed-pointer> */ value,
			obj_t /* <statically-typed-pointer> */ pointer,
			obj_t /* <integer> */ offset,
			obj_t /* <class> */ cls)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);
    
    if (!obj_is_fixnum(offset))
	error("Offset is not fixnum: %=", offset);
    else if (ptr == 0)
	error("Attempt to dereference null <statically-typed-pointer>: %=");
    if (!instancep(cls, obj_StaticTypeClass))
	error("class is not statically typed pointer: %=", cls);
    if (!instancep(value, cls))
	error("supplied value is not an instance of class %=", cls);
    /* pointer size object -- dereference as (void **) */
    *((void **)((char *)ptr + true_offset)) = get_c_object(value);
    return value;
}

obj_t pointer_add(obj_t /* <statically-typed-pointer> */ pointer,
		  obj_t /* <integer> */ num)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(num);
    
    return make_c_pointer(object_class(pointer),
			  (void *)((int)ptr + true_offset));
}
    
obj_t pointer_subtract(obj_t /* <statically-typed-pointer> */ pointer1,
		       obj_t /* <statically-typed-pointer> */ pointer2)
{
    void *ptr1 = C_PTR(pointer1)->pointer;
    void *ptr2 = C_PTR(pointer2)->pointer;
    
    return make_fixnum((long int)ptr1 - (long int)ptr2);
}
    

/* Dereferences a "slot" in the "structure" pointed to by a <c-pointer>. */
obj_t c_pointer_field(obj_t pointer, obj_t offset, obj_t cls, obj_t deref)
{
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);

    if (deref == obj_False)
	/* Don't dereference -- just increment */
	return convert_c_object(cls, (void *)((int)ptr + true_offset), FALSE);
    else if (cls == obj_CharacterClass || cls == obj_BooleanClass)
	/* byte size object -- dereference as (char *) */
	return convert_c_object(cls,
				(void *)((long)*((char *)ptr + true_offset)),
				FALSE);
    else
	/* pointer size ofject -- dereference as (void **) */
	return convert_c_object(cls, *(void **)((char *)ptr + true_offset),
				FALSE);
}

/* Sets the value of a "slot" in the "structure" pointed to by a */
/* <c-pointer>. */
obj_t c_pointer_field_setter(obj_t value, obj_t /* <c-pointer> */ pointer,
			     obj_t /* <integer> */ offset)
{
    obj_t cls = object_class(value);
    void *ptr = C_PTR(pointer)->pointer;
    int true_offset = fixnum_value(offset);

    if (cls == obj_CharacterClass || cls == obj_BooleanClass)
	/* byte size object -- dereference as (char *) */
	*((char *)ptr + true_offset) = ((char)((long)get_c_object(value)));
    else
	/* pointer size ofject -- dereference as (void **) */
	*((void **)((char *)ptr + true_offset)) = get_c_object(value);
    return value;
}

obj_t c_pointer_as(obj_t /* <class> */ cls,
		   obj_t /* <statically-typed-pointer> */ object)
{
    if (instancep(object, cls))
	return object;
    else
	return make_c_pointer(cls, C_PTR(object)->pointer);
}

obj_t c_ptr_as_int(obj_t /* <class> */ cls,
		   obj_t /* <statically-typed-pointer> */ object)
{
    return make_fixnum((int) C_PTR(object)->pointer);
}

obj_t c_int_as_ptr(obj_t /* <class> */ cls,
		   obj_t /* <integer> */ object)
{
    if (instancep(object, cls))
	return object;
    else
	return make_c_pointer(cls, (void *)fixnum_value(object));
}

obj_t c_pointer_equal(obj_t left, obj_t right)
{
    if (C_PTR(left)->pointer == C_PTR(right)->pointer)
	return obj_True;
    else
	return obj_False;
}

/* GC routines. */

int scav_c_pointer(struct object *obj)
{
    return sizeof(struct c_pointer);
}

obj_t trans_c_pointer(obj_t cptr)
{
    return transport(cptr, sizeof(struct c_pointer), TRUE);
}

static int scav_foreign_file(struct object *obj)
{
    scavenge(&((struct foreign_file *)obj)->file_name);
    return sizeof(struct foreign_file)
	+ ((struct foreign_file *)obj)->extra_size;
}

static obj_t trans_foreign_file(obj_t cptr)
{
    return transport(cptr,
		     sizeof(struct foreign_file)
		       + FOREIGN_FILE(cptr)->extra_size,
		     TRUE);
}

#ifdef HAVE_LIBDLD
static int scav_shared_file(struct object *obj)
{
    scavenge(&((struct shared_file *)obj)->file_name);
    return sizeof(struct shared_file)
	+ ((((struct shared_file *)obj)->file_count - 1) * sizeof(shl_t));
}

static obj_t trans_shared_file(obj_t cptr)
{
    return transport(cptr,
		     sizeof(struct shared_file)
		     + ((obj_ptr(struct shared_file *,cptr)->file_count - 1)
			* sizeof(shl_t)),
		     TRUE);
}
#endif /* HAVE_LIBDLD */

void scavenge_c_roots(void)
{
    if (mindy_dynamic_syms != NULL)
	/* Let it be scavenged and we'll recreate it at need */
	mindy_dynamic_syms = NULL;
}


/* Init stuff. */

void make_c_classes(void)
{
    obj_CPointerClass
	= make_builtin_class(scav_c_pointer, trans_c_pointer);
    CLASS(obj_CPointerClass)->class = obj_StaticTypeClass;
    CLASS(obj_CPointerClass)->sealed_p = FALSE;
    obj_ForeignFileClass = make_abstract_class(0);
    obj_ArchivedFileClass
	= make_builtin_class(scav_foreign_file, trans_foreign_file);
#ifdef HAVE_LIBDLD
    obj_SharedFileClass 
	= make_builtin_class(scav_shared_file, trans_shared_file);
#endif /* HAVE_LIBDLD */

    add_constant_root(&obj_CPointerClass);
    add_constant_root(&obj_ForeignFileClass);
#ifdef HAVE_LIBDLD
    add_constant_root(&obj_SharedFileClass);
#endif /* HAVE_LIBDLD */
    add_constant_root(&obj_ArchivedFileClass);
}

void init_c_classes(void)
{
    init_builtin_class(obj_CPointerClass, "<statically-typed-pointer>",
		       obj_ObjectClass, NULL);
    def_printer(obj_CPointerClass, print_c_pointer);
    init_builtin_class(obj_ForeignFileClass, "<foreign-file>", obj_ObjectClass,
		       NULL);
    init_builtin_class(obj_ArchivedFileClass, "<archived-file>",
		       obj_ForeignFileClass, NULL);
    def_printer(obj_ArchivedFileClass, print_foreign_file);
#ifdef HAVE_LIBDLD
    init_builtin_class(obj_SharedFileClass, "<shared-file>",
		       obj_ForeignFileClass, NULL);
    def_printer(obj_ForeignFileClass, print_shared_foreign_file);
#endif /* HAVE_LIBDLD */
}

void init_c_functions(void)
{
    extern void build_explicit_syms(void);
    
    /* This is required by find_c_function and find_c_pointer */
    build_explicit_syms();
    
    define_method("find-c-function",
		  list1(obj_ByteStringClass), FALSE,
		  list1(pair(symbol("file"), obj_Unbound)),
		  FALSE, obj_ObjectClass, find_c_function);
    define_method("find-c-pointer",
		  list1(obj_ByteStringClass), FALSE,
		  list1(pair(symbol("file"), obj_Unbound)),
		  FALSE, obj_ObjectClass, find_c_ptr);
    define_method("load-object-file",
		  list1(obj_ListClass), FALSE,
		  list1(pair(symbol("include"), obj_Nil)), FALSE,
		  obj_ObjectClass, load_c_file);
    define_method("signed-byte-at", list1(obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, signed_byte_at);
    define_method("signed-byte-at-setter",
		  list2(obj_IntegerClass, obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, signed_byte_at_setter);
    define_method("unsigned-byte-at", list1(obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, unsigned_byte_at);
    define_method("unsigned-byte-at-setter",
		  list2(obj_IntegerClass, obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, unsigned_byte_at_setter);
    define_method("signed-short-at", list1(obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, signed_short_at);
    define_method("signed-short-at-setter",
		  list2(obj_IntegerClass, obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, signed_short_at_setter);
    define_method("unsigned-short-at", list1(obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, unsigned_short_at);
    define_method("unsigned-short-at-setter",
		  list2(obj_IntegerClass, obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, unsigned_short_at_setter);
    define_method("signed-long-at", list1(obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, signed_long_at);
    define_method("signed-long-at-setter",
		  list2(obj_IntegerClass, obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, signed_long_at_setter);
    define_method("unsigned-long-at", list1(obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, unsigned_long_at);
    define_method("unsigned-long-at-setter",
		  list2(obj_IntegerClass, obj_CPointerClass), FALSE,
		  list1(pair(symbol("offset"), make_fixnum(0))), FALSE,
		  obj_IntegerClass, unsigned_long_at_setter);
    define_method("pointer-at", list1(obj_CPointerClass), FALSE,
		  list2(pair(symbol("offset"), make_fixnum(0)),
			pair(symbol("class"), obj_CPointerClass)), FALSE,
		  obj_IntegerClass, pointer_at);
    define_method("pointer-at-setter",
		  list2(obj_CPointerClass, obj_CPointerClass), FALSE,
		  list2(pair(symbol("offset"), make_fixnum(0)),
			pair(symbol("class"), obj_CPointerClass)), FALSE,
		  obj_IntegerClass, pointer_at_setter);
    define_method("+", list2(obj_CPointerClass, obj_IntegerClass), FALSE,
		  obj_False, FALSE, obj_CPointerClass, pointer_add);
    define_method("-", list2(obj_CPointerClass, obj_CPointerClass), FALSE,
		  obj_False, FALSE, obj_IntegerClass, pointer_subtract);
    define_method("c-pointer-slot",
		  listn(4, obj_CPointerClass, obj_IntegerClass,
			obj_TypeClass, obj_ObjectClass),
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, c_pointer_field);
    define_method("c-pointer-slot-setter",
		  list3(obj_ObjectClass, obj_CPointerClass, obj_IntegerClass),
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, c_pointer_field_setter);
    define_method("as",
		  list2(obj_StaticTypeClass, obj_CPointerClass), 
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, c_pointer_as);
    define_method("as",
		  list2(singleton(obj_FixnumClass), obj_CPointerClass), FALSE,
		  obj_False, FALSE,
		  obj_ObjectClass, c_ptr_as_int);
    define_method("as",
		  list2(obj_StaticTypeClass, obj_IntegerClass), 
		  FALSE, obj_False, FALSE,
		  obj_ObjectClass, c_int_as_ptr);
    define_method("=", list2(obj_CPointerClass, obj_CPointerClass),
		  FALSE, obj_False, FALSE,
		  obj_BooleanClass, c_pointer_equal);
    obj_NullPointer = make_c_pointer(obj_CPointerClass, 0);
    add_constant_root(&obj_NullPointer);
    define_constant("null-pointer", obj_NullPointer);
    add_variable_root(&mindy_explicit_syms);
}
