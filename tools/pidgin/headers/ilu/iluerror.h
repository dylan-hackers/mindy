/*
 BeginILUCopyright
 
 Copyright (c) 1991-1998 Xerox Corporation.  All Rights Reserved.
 
 Unlimited use, reproduction, modification, and distribution of this
 software and modified versions thereof is permitted.  Permission is
 granted to make derivative works from this software or a modified
 version thereof.  Any copy of this software, a modified version
 thereof, or a derivative work must include both the above copyright
 notice of Xerox Corporation and this paragraph.  Any distribution of
 this software, a modified version thereof, or a derivative work must
 comply with all applicable United States export control laws.  This
 software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGES.
 
 EndILUCopyright
*/
/*
*/
/* $Id: iluerror.h,v 1.1 1999/04/05 16:44:46 emk Exp $ */
/* Last edited by Mike Spreitzer September 22, 1998 3:50 pm PDT */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _ILUERROR_H_
#define _ILUERROR_H_

/***********************************************************************
Error signalling in the ILU kernel.

Each routine that can raise errors does so by returning an ilu_Error,
by storing through an ilu_Error* parameter that is the last parameter.
Some values of type ilu_Error indicate success; a successful
invocation will return one of these; an exceptional invocation will
construct and return one of the others.

General issue: pointer or l-value arguments?  For procedures we take
pointer arguments.  Macros were treated inconsistently in the
previous draft of the error system, and this draft inherits those
inconsistent treatments.  The preferred treatment is: macros take
l-value arguments.  The only exceptions are the error-constructing
macros inherited from the previous draft.

The macro
	ILU_CLER(e)
expands to a true-valued expresseion that sets e, an ilu_Error l-value,
to one of the many success-indicating values of type ilu_Error;
"e" may be evaluated more than once by this macro.
Also, the macro
	ILU_INIT_NO_ERR
expands to a success-indicating initializer for an ilu_Error.  For
backward compatability (with an earlier version of this system),
the macro
	ILU_NO_ERR
expands to a (non-constant) expression that evaluates to one of the
many success-indicating values of type ilu_Error.

Each error is an instance of some `type' of errors.  An error carries
parameters, which are declared with the error's type and called the
"members" of that error (or error type).  Each type of error is
`declared' once, and all error type declarations appear in one file,
iluerrs.h.  An error type is declared using the following syntax:

	ILU_DECL_ERR(<id>) {
		<memberlist>
	}
	ILU_END_DECL_ERR;

Different error types must use different <id>s; there are no other
restrictions on the <id>s.  The <memberlist> is as in a structure
definition.  Due to C brain-damage, the <memberlist> cannot be empty
(but its one member can be otherwise ignored).  The invocation
	
	ILU_DECL_PARMLESS_ERR(<id>);

declares an error type <id> that has no significant members.

A procedure that can raise errors should declare in its signature the
set of possible error types it can raise.  This can be done by using a
macro invocation like
	ILU_ERRS((<id1>, <id2>, ... <idN>))
which is defined to expand to
	ilu_Error
Here is an example:
	
	extern char* substr(char* base, int start, int len,
			    ILU_ERRS((alloc, bad_index)) *err);
	
The form
	ILU_ERRMEMP_T(<id>)
expands to an identifier typedef'd to "pointer to a struct containing
the members of error type <id>".  This is needed only for the second
form of error decoding below.

Each error type is `defined' exactly once, using the following syntax:

	ILU_DEF_ERR(<id>, "<descriptive string>") {
		<statements>
	}

Here <id> identifies the error type.  The
<descriptive string> is an arbitrary C string (no embedded \000's)
that describes the error, and is used only for purposes of debugging
the error system itself.  The <statements> form the body of the
procedure used to free errors of this type; they free the members of
the error, but not the error structure itself.  These
statements assume the existence of a variable named `e' containing
a pointer to a struct containing the error's members.  When a member
is an "ilu_Error *", the procedure
	extern void ilu_FreeNestedErr(ilu_Error *e);
is used to free it.

Raising an error requires creating an appropriate error instance and
storing it somewhere (ie, either in a local variable that will be
returned or through the ilu_Error* parameter).  This is done like so:

	ILU_BIND_ERR(<id>, <ep>, <mp>)
		<statement>
	ILU_END_BIND_ERR;

Here <id> identifies the error type, <ep> is an expression that
evaluates to a pointer to where the ilu_Error is to be stored, and
<mp> is an identifier that will be bound in the scope of the
<statement> to a pointer to a struct containing the error's members.
<ep> may be evaluated more than once in the expansion of this
macrology.  It is a checked runtime error for <ep> to evaluate to a
null pointer.  The members of *<mp> are not guaranteed to be
initialized in any particular way.  The <statement> then sets the
structure's members to appropriate values.

The expansion of ILU_BIND_ERR ... ILU_END_BIND_ERR records the
file name and line number where they occur.  If that's too boring,
you can explicitly supply the file and line by calling

	ILU_FULLBIND_ERR(<id>, <ep>, <mp>, <file>, <line>)
		<statement>
	ILU_END_FULLBIND_ERR;


A common special case of construction is when the <statement> just
assigns to members of the error.  This special case is handled more
compactly by a family of macros, illustrated by the following member:

	ILU_ERR_CONS2(<id>, <ep>, <mn1>, <mv1>, <mn2>, <mv2>, <fv>)

This expands to an expression that assigns to *<ep> a fully-created
error instance, and then evaluates <fv>.  The type and value of the
expression are those of <fv>.  The side-effects of the expression are
equivalent to those of:

	ILU_BIND_ERR(<id>, <ep>, ev) {
		ev -> <mn1> = <mv1>;
		ev -> <mn2> = <mv2>;
		}
	ILU_END_BIND_ERR;
	<fv>;

except that no identifier "ev" is used.  Note that the member value
expressions are evaluated once, in the order given, and that <fv> is
evaluated at the end.  This family has macros for errors (error types)
with 0, 1, 2, 3, and 4 members.  There are also "FULLCONS*" variants
that take <file> and <line> parameters at end.  Here's an example usage:

	char* substr(char* s, int start, int len, ILU_ERRS((BadIndex)) *e)
	{int slen;
	slen = strlen(s);
	if (start<0 || start>slen)
		return ILU_ERR_CONS1(BadIndex, e, i, start, NIL);
	...}

(this example requires that NIL expands to ((void*)0), rather than 0:
"(expr, ..., expr, 0)" (which form the expansion must necessarily
take) is not required to coerce to a char*).

Finally, the erring procedure returns.

A returned error is decoded with the following syntax:

	ILU_ERR_SWITCH(<expr>) {
	  ILU_SUCCESS_CASE
	  	<statement-S>
	  ILU_ERR_CASE(<tid-1>, <vid-1>)
		<statement-1>
	  ILU_ERR_CASE(<tid-2>, <vid-2>)
		<statement-2>
	  ...
	  ILU_ERR_ELSE
		<statement-E>
	}
	ILU_ERR_ENDSWITCH;

where: <expr> evaluates to the ilu_Error that was returned;
<statement-S> is to be executed in the success case;
for each i>0, <statement-i> is to be executed in the extent of <vid-i>,
which will be a variable initialized to a pointer to a struct containing
the error members, iff the error is of the type identified by <tid-i>;
and <statement-E> is to be executed if none of the earlier cases applies.
The "ILU_ERR_ELSE <statement-E>" part can be omitted when the earlier
cases are expected to handle all posibilities.  It is a checked
runtime error for none of the actual cases to apply.  Multiple
types of errors can be handled by one arm using abbreviations like
	ILU_ERR_CASE2(<tid1>, <tid2>)
		<statement>
which handles errors of types <tid1> and <tid2> by the given <statement>;
this abbreviation is available for handling 2, 3, and 4 error types.

There is another convenience invocation
	ILU_MUST_BE_SUCCESS(<expr>);
that is equivalent to
	ILU_ERR_SWITCH(<expr>) { ILU_SUCCESS_CASE ; } ILU_ERR_ENDSWITCH;
This was useful in some places at the boundary of the kernel;
its use must now be eliminated.

Alternatively, the ilu_Error can be decoded by using
automatically-generated procedures that effectively implement the arms
of such type-switch statements.  The declaration of error type <id>
expands to, among other things, a declaration like

  extern ILU_ERRMEMP_T(<id>) ilu_ErrpQua_<id>(ilu_Error *e);

This procedure takes a non-null ilu_Error* (passing NIL is a checked
runtime error) and returns a pointer to its members iff the error is
of the identified type, or NIL if the error is not of the identified
type.  For a non-NIL result, the callee retains ownership, and
guarantees that the result will be valid until e is freed (see later).
An advantage of this decoding style is that the error handling code
has no macro invocations; the error type declaration and definition
suffice.

The macros
	ILU_ERROK(e)
	ILU_ERRNOK(e)
expand to a boolean expressions that are true iff ilu_Error e indicates
success and failure, respectively.

For some types of errors, it is desirable to supply a "nested" error
as one of the members.  This is done through a pointer to an
ilu_Error, not an ilu_Error directly.  The following procedure is used
to heap-allocate an ilu_Error and copy a given ilu_Error into the
heap:
	extern ilu_Error* ilu_ErrDup(ilu_Error *e);
Caller owns result.  NIL is returned if the needed memory can't be
allocated.

Every non-successful ilu_Error value includes the source file name and
line number where it was raised.  These can be extracted using the calls

	extern const char* ilu_ErrorFile(ilu_Error *e);
	extern       int   ilu_ErrorLine(ilu_Error *e);

These return NIL and 0 when called on success-indicating errors.

Temporarily, until the error system is pervasive, there is a need to
convert into some other error handling regime.  This typically
involves printing a message to stderr.  For this interim purpose only,
the the name and description of an ilu_Error value may be accessed
with the macros declared by the following pseudocode:

	const char *ILU_ERR_NAME(ilu_Error err);
	const char *ILU_ERR_DESCRIPTION(ilu_Error err);

After an error is handled, it must be freed by a call on the following routine or macro:

	extern void ilu_FreeErrp(ilu_Error *e);
	extern void ILU_HANDLED (ilu_Error  e);

which does not itself raise any errors.  While e will not point into
the heap, some members of e may; that's why this procedure must be
called.  This need not be called on success-indicating values of
ilu_Error.

C code that uses this error system in the way prescribed above ---
using only the defined macro syntax, not any knowledge about how the
macros expand --- effectively achieves type safety through runtime
typing of errors.  An error value can only be created by use of the
ILU_BIND_ERR macro, which always tags the error value consistently
with the member structure used.  Members of error structures are only
accessible within ILU_ERR_SWITCH constructs, which invoke
error-type-specific code only on errors tagged with that error type,
or through the use of ilu_ErrpQua_<id> procedures.

For non-C code (ie, non-C language-specific runtimes), achieving type
safety is a little harder.  Of course, we can't even consider it
unless the language's type system can describe the C data types used
in the errors; let's consider what to do when it can.

This error system affords two levels of safety.  The first is provided
by the ilu_ErrpQua_<id> procedures.  These procedure encapsulate the
run-time type checking.  If the non-C code accesses error parameters
using only these procedures, it will be type-safe --- provided the
ilu_ErrpQua_<id> procedures are declared correctly in the non-C
language.

The ilu_ErrpQua_<id> procedures need to be defined as well as declared.
To minimize kernel size in some configurations (eg, C LS runtime
only), we put those definitions in a separate file.  That is, we might
expect that purely C programs will not use the ilu_ErrpQua_<id> style of
decoding, and so need not include the code for the ilu_ErrpQua_<id>
procedures.  For each error type <id>, the macro

	ILU_QUADEF(<id>);

is invoked in that separate file, and expands into the definition
of ilu_ErrpQua_<id>.

The second, higher, level of type-safety is built on the first, by
automatically transliterating the C header (".h") files into the non-C
language of interest.  This guarantees that the ilu_ErrpQua_<id>
procedures are declared correctly.  Sadly, I don't know of any program
that does such automatic transliteration to the non-C languages of
interest to us.

Following is stuff you shouldn't depend on.

In iluerrs.h, we construct the definition of the type ilu_Error as a big tagged union of all the member structure types.  That's done like this:

struct ilu_Error_s {
	const char     *ilu_file;
	int             ilu_line;
	ilu_ErrorType   ilu_type;
	union {
		ILU_ERRMEM_T(<id>) ILU_ERRLBL(<id>);
		ILU_ERRMEM_T(<id>) ILU_ERRLBL(<id>);
		...
		ILU_ERRMEM_T(<id>) ILU_ERRLBL(<id>);
	} u;
};

for each <id> that names an error type.
*/

typedef struct ilu_Error_s ilu_Error;

#define ILU_ERRTYP(id)		  ilu_ET_##id
#define ILU_ERRTYPDET(id)	(&ilu_ETS_##id)

#define ILU_OKERRTYPE ILU_ERRTYP(success)

#define ILU_CLER(e) \
((e).ilu_type = ILU_OKERRTYPE, (e).ilu_file = (const char *) 0, ilu_TRUE)

#define ILU_INIT_NO_ERR { (const char *) 0, 0, ILU_OKERRTYPE, {{0}}}

#define ILU_NO_ERR ilu_success_err

typedef struct {
  char           *name;
  char           *description;
  void            (*freeproc) (ilu_Error *);
}               ilu_ErrorTypeDetailsStruct, *ilu_ErrorTypeDetails;

#define ILU_ERRMEM_T(x)		ilu_##x##_EVS
#define ILU_ERRMEMP_T(x)	ilu_##x##_EV
/*
 * The macro preprocessor of SunPro C++ for Solaris2.3 has a bug:
 * "x##y z" (x, y and z identifiers, y a macro parameter) expands as
 * if it were "x##y##z".  We avoid this bug.  Among other things,
 * this means we avoid expanding to "ilu_foo##x" any macro that may
 * be followed by an identifier.
 */

#define ILU_DECL_ERR(x) \
ILU_PUBLIC ilu_ErrorTypeDetailsStruct ilu_ETS_##x ; \
typedef struct ilu_##x##_EVSs ILU_ERRMEM_T(x), *ILU_ERRMEMP_T(x) ; \
ILU_PUBLIC ILU_ERRMEMP_T(x) ilu_ErrpQua_##x (ilu_Error * e); \
struct ilu_##x##_EVSs

#define ILU_END_DECL_ERR

#define ILU_DECL_PARMLESS_ERR(id) ILU_DECL_ERR(id) {int ignoreme;} ILU_END_DECL_ERR

#define ILU_ERRS(x) ilu_Error

#define ILU_ERRLBL(id)		ilu_erru_##id
#define ILU_ERRSEL(id, e)	((e).u.ILU_ERRLBL(id))

#define ILU_DEF_ERR(id,xdescrip) \
static void ilu_EF_##id (ilu_Error*); \
ilu_ErrorTypeDetailsStruct ilu_ETS_##id = { # id , xdescrip, ilu_EF_##id }; \
static void ilu_EF2_##id (ILU_ERRMEM_T(id) *e); \
static void ilu_EF_##id (ilu_Error *ep) \
{ ilu_EF2_##id (&ILU_ERRSEL(id, *ep)); } \
static void ilu_EF2_##id (ILU_ERRMEM_T(id) *e)

ILU_PUBLIC void 
_ilu_NoteRaise(int et, const char *file, int line);

/* The following assumes "_ilu_Assert(b,s)" is an expression. */

#define ILU_ERRSTART(id,ep,file,line) ( \
	_ilu_NoteRaise(ILU_ERRTYP(id), file, line), \
	_ilu_Assert((ep) != ILU_NIL, #ep " is null"), \
	(ep)->ilu_type = ILU_ERRTYP(id), \
	(ep)->ilu_file = file, \
	(ep)->ilu_line = line )

#define ILU_FULLBIND_ERR(id,ep,mp,file,line) \
do  {	ILU_ERRMEM_T(id) *mp; \
	ILU_ERRSTART(id,ep,file,line); \
	mp = &ILU_ERRSEL(id, *(ep));

#define ILU_END_FULLBIND_ERR } while (0)

#define ILU_BIND_ERR(id,ep,mp) ILU_FULLBIND_ERR(id,ep,mp,__FILE__,__LINE__)

#define ILU_END_BIND_ERR ILU_END_FULLBIND_ERR

#define ILU_ERR_FULLCONS0(id,ep,fv,file,line) ( \
	ILU_ERRSTART(id,ep,file,line), \
	fv )

#define ILU_ERR_FULLCONS1(id,ep,mn1,mv1,fv,file,line) ( \
	ILU_ERRSTART(id,ep,file,line), \
	ILU_ERRSEL(id, *ep).mn1 = mv1, \
	fv )

#define ILU_ERR_FULLCONS2(id,ep,mn1,mv1,mn2,mv2,fv,file,line) ( \
	ILU_ERRSTART(id,ep,file,line), \
	ILU_ERRSEL(id, *ep).mn1 = mv1, \
	ILU_ERRSEL(id, *ep).mn2 = mv2, \
	fv )

#define ILU_ERR_FULLCONS3(id,ep,mn1,mv1,mn2,mv2,mn3,mv3,fv,file,line) ( \
	ILU_ERRSTART(id,ep,file,line), \
	ILU_ERRSEL(id, *ep).mn1 = mv1, \
	ILU_ERRSEL(id, *ep).mn2 = mv2, \
	ILU_ERRSEL(id, *ep).mn3 = mv3, \
	fv )

#define ILU_ERR_FULLCONS4(id,ep,mn1,mv1,mn2,mv2,mn3,mv3,mn4,mv4,fv,file,line) ( \
	ILU_ERRSTART(id,ep,file,line), \
	ILU_ERRSEL(id, *ep).mn1 = mv1, \
	ILU_ERRSEL(id, *ep).mn2 = mv2, \
	ILU_ERRSEL(id, *ep).mn3 = mv3, \
	ILU_ERRSEL(id, *ep).mn4 = mv4, \
	fv )

#define     ILU_ERR_CONS0(id,ep,fv) \
	ILU_ERR_FULLCONS0(id,ep,fv,__FILE__,__LINE__)

#define     ILU_ERR_CONS1(id,ep,mn1,mv1,fv) \
	ILU_ERR_FULLCONS1(id,ep,mn1,mv1,fv, __FILE__,__LINE__)

#define     ILU_ERR_CONS2(id,ep,mn1,mv1,mn2,mv2,fv) \
	ILU_ERR_FULLCONS2(id,ep,mn1,mv1,mn2,mv2,fv, __FILE__,__LINE__)

#define     ILU_ERR_CONS3(id,ep,mn1,mv1,mn2,mv2,mn3,mv3,fv) \
	ILU_ERR_FULLCONS3(id,ep,mn1,mv1,mn2,mv2,mn3,mv3,fv, \
	                  __FILE__,__LINE__)

#define     ILU_ERR_CONS4(id,ep,mn1,mv1,mn2,mv2,mn3,mv3,mn4,mv4,fv) \
	ILU_ERR_FULLCONS4(id,ep,mn1,mv1,mn2,mv2,mn3,mv3,mn4,mv4,fv, \
	                  __FILE__,__LINE__)

#define ILU_ERR_SWITCH(x)	\
do  {	ilu_Error *__ilu_CurrentError = &(x); \
	if (0)

#define ILU_SUCCESS_CASE \
	} else if (ILU_ERROK(*__ilu_CurrentError)) {

#define ILU_ERR_CASE(tid,vid) \
	} else if (__ilu_CurrentError->ilu_type == ILU_ERRTYP(tid) ) { \
		ILU_ERRMEM_T(tid) * vid ; \
		vid = &ILU_ERRSEL(tid, *__ilu_CurrentError);

#define ILU_ERR_ELSE \
	} else if ( 1 ) {

#define ILU_ERR_ENDSWITCH \
	else {char __errbuf[1000]; \
	if (ILU_ERROK(*__ilu_CurrentError)) \
	  sprintf(__errbuf, "unhandled success"); \
	else \
	  sprintf(__errbuf, "unhandled error %s from line %d in file %s", \
	    ILU_ERR_NAME(*__ilu_CurrentError), \
	    ilu_ErrorLine(__ilu_CurrentError), \
	    ilu_ErrorFile(__ilu_CurrentError)); \
	(void) _ilu_Assert(0, __errbuf);} } while (0)

#define ILU_ERR_CASE2(t1, t2) \
	} else if (\
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t1) || \
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t2) ) {

#define ILU_ERR_CASE3(t1, t2, t3) \
	} else if (\
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t1) || \
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t2) || \
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t3) ) {

#define ILU_ERR_CASE4(t1, t2, t3, t4) \
	} else if (\
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t1) || \
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t2) || \
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t3) || \
	__ilu_CurrentError->ilu_type == ILU_ERRTYP(t4) ) {

#define ILU_MUST_BE_SUCCESS(x) ILU_ERR_SWITCH(x) { ILU_SUCCESS_CASE ; } ILU_ERR_ENDSWITCH;

#define ILU_ERROK(e)	((e).ilu_type == ILU_OKERRTYPE)
#define ILU_ERRNOK(e)	((e).ilu_type != ILU_OKERRTYPE)

ILU_PUBLIC ilu_Error *ilu_ErrDup(ilu_Error * e);
ILU_PUBLIC const char *ilu_ErrorFile(ilu_Error * e);
ILU_PUBLIC int      ilu_ErrorLine(ilu_Error * e);

ILU_PUBLIC ilu_ErrorTypeDetails ilu_GetErrorTypeDetails(int et);

#define ILU_ERR_NAME(err)	 (ilu_GetErrorTypeDetails((int)((err).ilu_type))->name)
#define ILU_ERR_DESCRIPTION(err) (ilu_GetErrorTypeDetails((int)((err).ilu_type))->description)

ILU_PUBLIC void     ilu_FreeNestedErr(ilu_Error * e);
ILU_PUBLIC void     ilu_FreeErrp(ilu_Error *);
#define ILU_HANDLED(e)	ilu_FreeErrp(&(e))

#define ILU_QUADEF(x) \
ILU_ERRMEM_T(x) *ilu_ErrpQua_##x (ilu_Error *e) \
{ \
  if (e->ilu_type == ILU_ERRTYP(x) ) \
	return &ILU_ERRSEL(x, *e); \
  else	return (ILU_NIL); \
} \
ILU_ERRMEM_T(x) *ilu_ErrpQua_##x (ilu_Error *e)




#endif /* ndef _ILUERROR_H_ */
#ifdef __cplusplus
}
#endif
