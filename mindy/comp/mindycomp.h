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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/mindycomp.h,v 1.5 1994/07/26 18:36:03 hallgren Exp $
*
\**********************************************************************/

#ifdef sparc
#include <sys/stdtypes.h>
#endif

extern void *malloc(size_t len);
extern void *realloc(void *ptr, size_t len);
extern void free(void *ptr);

typedef int boolean;
#define TRUE 1
#define FALSE 0

extern char *current_file;
extern boolean ParseOnly;

extern void error(int line, char *msg, ...);
extern void warn(int line, char *msg, ...);

extern struct symbol *ModuleName;
extern struct symbol *LibraryName;

#ifdef sparc
extern int printf();
extern int fprintf();
extern int vfprintf();
extern int _flsbuf();
extern int _filbuf();
extern int fseek();
extern int fclose();
extern int fwrite();
extern void perror();
extern int fputs();
extern int tolower();
extern int fflush();
extern int ungetc();
#endif
