#include <stdio.h>
#include <stdlib.h>
#include <runtime.h>

#include "../gc/gc.h"

#define KEY_OBJECT		'o'
#define KEY_HEAPPTR		'h'
#define KEY_BOOLEAN		'B'
#define KEY_LONG		'l'
#define KEY_INT			'i'
#define KEY_UINT		'u'
#define KEY_SHORT		's'
#define KEY_USHORT		't'
#define KEY_BYTE		'c'
#define KEY_UBYTE		'b'
#define KEY_PTR			'p'
#define KEY_FLOAT		'f'
#define KEY_DOUBLE		'd'
#define KEY_LONG_DOUBLE		'D'
#define KEY_VOID		'v'

/* A trampoline is a full lexical closure masquerading as a simple
 * function pointer.  When capturing a closure that will be used as a
 * callback, we dynamically build executable code that prepends the
 * closure information (closure.heapptr) as the first argument of the
 * function call and jumps to the callback-closure's entry point
 * (func).
 *
 * The signature argument points to an array of characters with length
 * nkeys, and no terminating \0 character.  The first character in the
 * array gives the return type of the desired callback entry point,
 * and the remaining keys give the type of the callback's arguments.
 *
 * The code to be generated can be derived by compiling variations of:
 *
 *	 int c2(int arg1, int arg2)
 *	 {
 *	   return u2((void *)0x, arg1, arg2);
 *	 }
 *
 * for various argument types and argument counts, simplifying and
 * generalizing, and determining the instruction encodings.  */

#ifdef __i386__
heapptr_t make_trampoline(void *func, descriptor_t closure,
			  int nkeys, char *signature)
{
  char *keyp;			/* pointer within signature */
  size_t code_bytes = 0;	/* trampoline byte count */
  size_t arg_bytes = 0;		/* number of bytes of arguments */
  void *code;			/* pointer to beginning of generated code */
  char *codep;			/* pointer to next available code byte */
  int i;

  /* make a first pass through the argument signature and determine
     how many bytes of code will be required, and how many bytes of
     arguments may be found on the stack. */
  for(keyp = signature + 1; keyp < signature + nkeys; ++keyp)
    {
      switch(*keyp)
	{
	case KEY_OBJECT:
	  /* general-rep uses a 2-word descriptor_t structure */
	  arg_bytes += 8;
	  code_bytes += 8;	/* two pushl x(%esp) instructions */
	  break;
	  
	case KEY_HEAPPTR:
	case KEY_PTR:
	case KEY_INT:
	case KEY_LONG:
	case KEY_UINT:
	case KEY_BOOLEAN:
	  
	case KEY_SHORT:
	case KEY_USHORT:
	case KEY_BYTE:
	case KEY_UBYTE:
	case KEY_FLOAT:
	  /* one word */
	  arg_bytes += 4;
	  code_bytes += 4;	/* one pushl x(%esp) instructions */
	  break;

	case KEY_DOUBLE:
	  /* two words */
	  arg_bytes += 8;
	  code_bytes += 8;	/* two pushl x(%esp) instructions */
	  break;
	  
	case KEY_LONG_DOUBLE:
	  /* three words */
	  arg_bytes += 12;
	  code_bytes += 12;	/* two pushl x(%esp) instructions */
	  break;
	  
	case KEY_VOID:
	default:
	  fprintf(stderr, "make_trampoline illegal argument key %c\n",
		  *keyp);
	  fflush(stderr);
	  abort();
	  break;
	}
    }

  code_bytes += 14;		/* remaining instructions */

  code_bytes = (code_bytes + 7) & ~3; /* space at end for closure ptr */

  codep = code = GC_malloc(code_bytes);

  for(i = 0; i < arg_bytes; i += 4)
    {
      *codep++ = 0xFF;		/* pushl <arg_bytes>(%esp) */
      *codep++ = 0x74;
      *codep++ = 0x24;
      *codep++ = arg_bytes;
    }

  *codep++ = 0x68;		/* pushl $<closure> (curried arg) */
  *((heapptr_t *)codep) = closure.heapptr;
  codep += 4;

  *codep++ = 0xE8;		/* call <func> */
  *((long *)codep) = (char *)func - (codep + 4);
  codep += 4;

  *codep++ = 0x83;		/* add $<arg_bytes+4>, %esp */
  *codep++ = 0xC4;
  *codep++ = arg_bytes + 4;

  *codep++ = 0xC3;		/* ret */

  /* At the end we place another pointer to the function closure
     object, word-aligned so the garbage collector can find it. */
  *((heapptr_t *)((char *)code + code_bytes - 4)) = closure.heapptr;

  /* x86 platforms don't seem to require us to flush the cache or
     call mprotect() or anything like that. */
  return code;
}
#else
heapptr_t make_trampoline(void *func, descriptor_t closure,
			  int nkeys, char *signature)
{
  fprintf(stderr, "make_trampoline not supported on this architecture\n");
  fflush(stderr);
  abort();
  return 0;			/* not reached */
}
#endif;
