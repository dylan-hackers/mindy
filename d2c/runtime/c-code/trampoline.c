#include <stdio.h>
#include <stdlib.h>
#include "runtime.h"
#ifdef __sgi__
#include <sys/cachectl.h>
#endif

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

#if 0
  fprintf(stderr,
	  "\n==> Making closure with arg bytes: %i, signature: %*s\n",
	  arg_bytes, nkeys, signature);
  fflush(stderr);
#endif

  code_bytes += 14;		/* remaining instructions */

  code_bytes = (code_bytes + 7) & ~3; /* space at end for closure ptr */

  codep = code = allocate(code_bytes);

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

#elif defined __powerpc__
/* On the PowerPC we will just rearrange the integer/pointer arguments within the
   integer registers, and then jump to the callback routine.  We don't need to make
   a stack frame, or return to ourselves for cleanup, and we can totally ignore all
   FP arguments, whether they are in registers or on the stack.  Effectively, we are
   just a prolog to the callback function.

   If there are too many integer arguments (more than seven explicit, plus our
   environment pointer) then we will just punt.  Coping with this would require
   building a stack frame, saving the return PC, saving the old stack pointer,
   copying and enlarging the stack-based argument area, moving one argument from
   r10 to the stack, shuffling r3 -> r10, and then calling the callback routine.
   On return from the callback routine, we'd need to reload the return address to
   the LR, reload the old stack pointer, and return to the calling routine.

   NB. This implementation is for the System V ABI, as used e.g. on LinuxPPC and MKLinux.
   It is *not* correct for the AIX/MacOS ABI.  For the current implementation, all that
   should be needed is to allow two extra registers (R11 and R12) for integer args, plus
   allowing for "func" being a pointer to the TOC entry rather than to the code for the
   callback function.  If this function is extended to cope with situations requiring a
   new stack frame then allowances will also have to be made for the different stack
   frame format.

   This implementation assumes that the PowerPC cache block size is 32 bytes.  This is
   true for the 601, 603(e), 604(e) and 750 processors.  It may not be true for future
   models, but hopefully it will only get bigger, not smaller, in which case the code
   sequences used here will be safe, if perhaps mildly non-optimal.
*/

#define BLOCKSIZE 32

heapptr_t make_trampoline(void *func, descriptor_t closure,
			  int nkeys, char *signature)
{
  char *keyp;			/* pointer within signature */
  size_t code_bytes = 0;	/* trampoline byte count */
  size_t arg_bytes = 0;		/* number of bytes of arguments */
  void *code;			/* pointer to beginning of generated code */
  long *codep;			/* pointer to next available code instruction */
  int i;
  long tmp;

  int fr = 0;                   /* FP register count */
  int gr = 0;                   /* general purpose reg count */
  int starg = 0;                /* address of first argument of stack */

  /* make a pass through the argument signature and determine
     how many bytes of code will be required, and how many bytes of
     arguments may be found on the stack. */
  for(keyp = signature + 1; keyp < signature + nkeys; ++keyp)
    {
      switch(*keyp)
	{
	case KEY_OBJECT:
	case KEY_LONG_DOUBLE:
	  /* the argument will be a pointer to a structure stored
	     elsewhere, so just fall through */

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

	  if( gr > 7) {
	    starg += 4;
	  } else {
	    gr++;
	  }
	  break;

	case KEY_FLOAT:
	case KEY_DOUBLE:
	  if( fr > 7) {
	    starg = (starg + 7) & (~0x7);
	    starg += 8;
	  } else {
	    fr++;
	  }
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

#if 0
  fprintf(stderr,
	  "gr: %i, fr: %i, stdarg: %i, signature: %*s\n",
	  gr, fr, starg, nkeys, signature);
  fflush(stderr);
#endif

  if (gr >= 7){
    fprintf(stderr,
	    "\n\nOnly seven non-FP args are allowed in the current make_trampoline\n"
	    "implementation on PPC.  The program attempted to make a trampoline\n"
	    "with the signature: %*s, which needs %d integer registers.\n",
	    nkeys, signature, gr);
    fflush(stderr);
    abort();
  }

  /* six instructions + #gr + ptr to closure */
  code_bytes = (gr + 7) * 4;
  codep = code = allocate(code_bytes);

  tmp = (long)func;                               /* load branch dest into ctr */
  *codep++ = (15 << 26) | ((tmp >> 16) & 0xFFFF); /* lis r0,hi(tmp) */
  *codep++ = (24 << 26) | (tmp & 0xFFFF);         /* ori r0,lo(tmp) */
  *codep++ = (31 << 26) | (467 << 1) | (9 << 16); /* mtctr r0  =>  mtspr 9,r0 */

  /* shuffle those integer args... */
  for(i = gr+2; i>2; --i){
    /* mr i+1,i => or i+1,i,i */
    *codep++ = (31 << 26) | (444 << 1) | ((i+1) << 16) | (i << 21) | (i << 11);
  }

  tmp = (long)closure.heapptr;                                    /* load closure env into r3 */
  *codep++ = (15 << 26) | (3 << 21) | ((tmp >> 16) & 0xFFFF);     /* lis r3,hi(tmp) */
  *codep++ = (24 << 26) | (3 << 21) | (3 << 16) | (tmp & 0xFFFF); /* ori r3,lo(tmp) */
  *codep++ = 0x4E800420;                                          /* bctr */

  /* At the end we place another pointer to the function closure
     object, word-aligned so the garbage collector can find it. */
  *((heapptr_t *)codep) = closure.heapptr;

  /* On PowerPC we need to make sure the new code gets flushed from the data
     cache and any old code in the same place gets flushed from the instruction
     cache...
     
  */

#ifdef __GNUC__

#define SYNC()   asm ("sync" : : )
#define ISYNC()  asm ("isync" : : )
#define DCBST(x) asm ("dcbst 0,%0" :  : "g" (x))
#define ICBI(x)  asm ("icbi 0,%0" :  : "g" (x))

#else
  /* I'm not going to cause compilation to fail, because everything will be fine
     if their programs don't actually create callback closures */
  fprintf(stderr,
	  "\n\nError: attempt to make a callback closure.  The implementation of\n"
	  "make_trampoline() does not know how to make self-modifying code safe\n"
	  "on the compiler used to produce your Dylan runtime.  Your program is\n"
	  "probably about to crash.  Sorry...\n");
  fflush(stderr);
#endif

  {
    /* flush new instructions from data cache */
    char *block = (char*) ((long)code & ~(BLOCKSIZE-1));
    do {
      DCBST(block); /* Data Cache Block Store */
      block += BLOCKSIZE;
    } while (block < ((char*)code + code_bytes));

    /* wait for stores to finish */
    SYNC();

    /* invalidate instruction cache */
    block = (char*) ((long)code & ~(BLOCKSIZE-1));
    do {
      ICBI(block); /* Instruction Cache Block Invalidate */
      block += BLOCKSIZE;
    } while (block < ((char*)code + code_bytes));

    /* This sync may or may not be necessary.  The 601 of course doesn't need any of
       this cache flushing at all.  The 603(e) manual and the PPC Programming Environments
       manual don't include this extra sync instruction in their discussion of self-
       modifying code.  The 604(e) and 750 manuals do include the extra sync.  At the
       worst it's harmless...
    */
    SYNC();

    /* invalidate any pre-fetched instructions (unlikely, but...) */
    ISYNC();
  }

  return code;
}
#undef BLOCKSIZE

#elif defined __mips__
/* MIPS support is currently limited to the n32 ABI.  This is mostly
 * like PPC (we're just a prologue to the function, accepting only
 * arguments that fit into the registers), with the following additional
 * details:
 *
 * With n32 we've got 64-bit registers for up to eight arguments.
 * 
 * The registers' nice size means that that every arguments fits into
 * exactly one register, so we just step through the signature,
 * shuffling registers where needed.
 *
 * We've got to shuffle fp registers, too, since argument number (say)
 * `two' will, if it happens to be a float, go into $f2 regardless of
 * whether argument number `one' was float.  I.e., adding any kind of
 * argument arguments shifts _all_ registers.
 */
heapptr_t
make_trampoline(void *func, descriptor_t closure, int n, char *sig)
{
    int size;                   /* size of code */
    void *code;                 /* ptr to beginning of code */
    unsigned *c;                /* ptr into code */
    unsigned tmpf, tmpc;

    /* if arguments don't fit into registers, give up */
    if (n > 8) {
	fprintf(stderr,
		"\nCallback trampoline wants wants %d arguments,"
		" exceeding the current\nmaximum of seven."
		"  (Signature is %*s).--dfl@gmx.de",
		n - 1, sig);
	fflush(stderr);
	abort();
    }

    /* one shuffling instruction per arg, plus five other, and one extra ptr */
    size = ((n - 1) + 7) << 2;
    c = code = allocate(size);

    /* step through args backwards, assembling code to shuffle regs */
    for (n--; n > 0; n--)
	switch(sig[n]) {
	case KEY_OBJECT:
	    /* This is an 8-byte structure, passed in a single 64-bit
	     * register, so just fall through */
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
	    /* daddu $an, $a{n-1}, $0 */
	    *c++ = 0x2d                 /* op=0, func */
		| (n + 3) << 21         /* rs, rt=0 */
		| (n + 4) << 11;        /* rd */
	    break;

	case KEY_LONG_DOUBLE:
	    /* `long double' is `double' for gcc, so fall through */
	case KEY_FLOAT:
	case KEY_DOUBLE:
	    /* mov.d $fn, $f{n-1} */
	    *c++ = 0x11 << 26 + 1 << 21 + 6     /* op, double, func */
		| (n + 11) << 11                /* fs */
		| (n + 12) << 6;                /* fd */
	    break;

	case KEY_VOID:
	default:
	    fprintf(stderr, "make_trampoline illegal argument key %c\n",
		    sig[n]);
	    fflush(stderr);
	    abort();
	    break;
	}

    tmpf = (unsigned) func;
    tmpc = (unsigned) closure.heapptr;
    *c++ = 0xf << 26 | 25 << 16 | tmpf >> 16;           /* lui $25, upper */
    *c++ = 0xf << 26 | 4 << 16 | tmpc >> 16;            /* lui $4, upper */
    *c++ = 9 << 26 | 25 << 21 | 25 << 16 | tmpf&0xffff; /* addiu $25,$25,low */
    *c++ = 9 << 26 | 4 << 21 | 4 << 16 | tmpc & 0xffff; /* addiu $4,$4,lower */
    *c++ = 25 << 21 | 8;                                /* jr $25 */
    *c++ = 0;
    
    /* pointer for gc */
    *c++ = (unsigned) closure.heapptr;

#ifdef __sgi__
    if (cacheflush(code, size, BCACHE) < 0) {
	fprintf(stderr, "\nmake_trampoline: Argh--couldn't flush cache.\n");
	fflush(stderr);
    }	
#else
    fprintf(stderr, "\nmake_trampoline: Oh, new platform?\n");
    fflush(stderr);
#endif

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
#endif
