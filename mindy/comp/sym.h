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
* $Header: /home/housel/work/rcs/gd/src/mindy/comp/sym.h,v 1.4 1994/06/02 23:27:21 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/


struct symbol {
    unsigned hash;
    struct symbol *next;
    int handle;
    unsigned char name[0];
};

extern struct symbol *symbol(char *name);
extern struct symbol *gensym(void);

extern void init_sym_table(void);

extern struct symbol *sym_DefineClass1;
extern struct symbol *sym_DefineClass2;
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
extern struct symbol *sym_Constant;
extern struct symbol *sym_Do;
extern struct symbol *sym_Element;
extern struct symbol *sym_Error;
extern struct symbol *sym_FindVariable;
extern struct symbol *sym_ForwardIterationProtocol;
extern struct symbol *sym_Getter;
extern struct symbol *sym_InitVariable;
extern struct symbol *sym_Instance;
extern struct symbol *sym_List;
extern struct symbol *sym_MakeNextMethodFunction;
extern struct symbol *sym_MakeSlot;
extern struct symbol *sym_Negative;
extern struct symbol *sym_NegativeP;
extern struct symbol *sym_NextMethod;
extern struct symbol *sym_PopHandler;
extern struct symbol *sym_PushHandler;
extern struct symbol *sym_Setter;
extern struct symbol *sym_Singleton;
extern struct symbol *sym_Subclass;
extern struct symbol *sym_Throw;
extern struct symbol *sym_Uwp;
extern struct symbol *sym_Values;
extern struct symbol *sym_Virtual;
