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
* $Header: /scm/cvs/src/mindy/comp/sym.h,v 1.1 1998/05/03 19:55:09 andreas Exp $
*
\**********************************************************************/


struct symbol {
    unsigned hash;
    struct symbol *next;
    int handle;
    unsigned char name[1];
};

extern struct symbol *symbol(char *name);
extern struct symbol *gensym(void);

extern void init_sym_table(void);

extern struct symbol *sym_DefineClass1;
extern struct symbol *sym_DefineClass2;
extern struct symbol *sym_DefineDomain;
extern struct symbol *sym_DefineGeneric;
extern struct symbol *sym_DefineMethod;
extern struct symbol *sym_DefineSlot;
extern struct symbol *sym_Or;
extern struct symbol *sym_Plus;
extern struct symbol *sym_Less;
extern struct symbol *sym_LessEqual;
extern struct symbol *sym_Object;
extern struct symbol *sym_Type;
extern struct symbol *sym_Eq;
extern struct symbol *sym_DylanUser;
extern struct symbol *sym_Apply;
extern struct symbol *sym_Aref;
extern struct symbol *sym_Catch;
extern struct symbol *sym_CheckType;
extern struct symbol *sym_Class;
extern struct symbol *sym_Do;
extern struct symbol *sym_Each_Subclass;
extern struct symbol *sym_Element;
extern struct symbol *sym_Error;
extern struct symbol *sym_FindVariable;
extern struct symbol *sym_ForwardIterationProtocol;
extern struct symbol *sym_Getter;
extern struct symbol *sym_InitVariable;
extern struct symbol *sym_Instance;
extern struct symbol *sym_List;
extern struct symbol *sym_MakeInherited;
extern struct symbol *sym_MakeInitarg;
extern struct symbol *sym_MakeNextMethodFunction;
extern struct symbol *sym_MakeSlot;
extern struct symbol *sym_Negative;
extern struct symbol *sym_NegativeP;
extern struct symbol *sym_NextMethod;
extern struct symbol *sym_Not;
extern struct symbol *sym_PopHandler;
extern struct symbol *sym_PushHandler;
extern struct symbol *sym_Setter;
extern struct symbol *sym_Singleton;
extern struct symbol *sym_Throw;
extern struct symbol *sym_Uwp;
extern struct symbol *sym_Values;
extern struct symbol *sym_Virtual;
