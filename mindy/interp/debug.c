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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/debug.c,v 1.2 1994/03/27 02:10:31 wlott Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>

#include "mindy.h"
#include "thread.h"
#include "driver.h"
#include "func.h"
#include "module.h"
#include "lexer.h"
#include "parser.h"
#include "str.h"
#include "list.h"
#include "vec.h"
#include "type.h"
#include "sym.h"
#include "num.h"
#include "obj.h"
#include "bool.h"
#include "print.h"

struct library *CurLibrary = NULL;
struct module *CurModule = NULL;

static struct thread *CurThread = NULL;
static obj_t *CurFP = NULL;
static boolean ThreadChanged = FALSE, FrameChanged = FALSE;
static boolean Continue;

static obj_t do_funcall_func;
static obj_t do_print_func;

static struct variable *debugger_print_var;
static struct variable *debugger_report_var;
static struct variable *debugger_abort_var;
static struct variable *debugger_restarts_var;
static struct variable *debugger_restart_var;
static struct variable *debugger_return_var;


/* Frame printing. */

static void print_frame(obj_t *fp)
{
    obj_t *ptr = obj_rawptr(fp[-3]);
    obj_t *end = fp - 4;

    printf("fp 0x%08lx: ", (unsigned long)fp);
    prin1(function_debug_name_or_self(*ptr++));
    putchar('(');
    if (ptr < end) {
	while (1) {
	    prin1(*ptr++);
	    if (ptr >= end)
		break;
	    printf(", ");
	}
    }
    printf(")\n");
}

static void print_thread(struct thread *thread)
{
    static char *status_chars = {"RSDBW"};
    printf("[%d] %c ", thread->id, status_chars[(int)thread->status]);
    if (thread->suspend_count != 0)
	printf("%d ", thread->suspend_count);
    else
	printf("  ");
    if (thread->debug_name != obj_False)
	print(thread->debug_name);
    else
	putchar('\n');
}

static void maybe_print_frame(void)
{
    if (ThreadChanged) {
	ThreadChanged = FALSE;

	if (CurThread == NULL) {
	    printf("no current thread\n");
	    CurFP = NULL;
	    FrameChanged = FALSE;
	}
	else {
	    printf("thread ");
	    print_thread(CurThread);
	    CurFP = CurThread->fp;
	    FrameChanged = TRUE;
	}
    }

    if (FrameChanged) {
	if (CurFP == NULL)
	    printf("No stack.\n");
	else
	    print_frame(CurFP);
	FrameChanged = FALSE;
    }
}


/* Thread control utilities. */

static void suspend_other_threads(struct thread *thread)
{
    struct thread_list *threads;

    for (threads = all_threads(); threads != NULL; threads = threads->next)
	if (threads->thread != thread)
	    thread_suspend(threads->thread);
}

static void restart_other_threads(struct thread *thread)
{
    struct thread_list *threads;

    for (threads = all_threads(); threads != NULL; threads = threads->next)
	if (threads->thread != thread)
	    thread_restart(threads->thread);
}


static void kill_me(struct thread *thread, obj_t *vals)
{
    thread_kill(thread);
    restart_other_threads(NULL);
    CurThread = NULL;
    ThreadChanged = TRUE;
    pause(pause_DebuggerCommandFinished);
}

static void debugger_cmd_finished(struct thread *thread, obj_t *vals)
{
    thread->sp = vals;
    thread_pop_escape(thread);
    pause(pause_DebuggerCommandFinished);
}





/* Command tables. */

struct cmd_entry {
    char *cmd;
    char *help;
    void (*fn)(void);
};

static struct cmd_entry *find_cmd(struct cmd_entry *table, char *what)
{
    struct cmd_entry *match = NULL;

    while (table->cmd) {
	if (strncmp(table->cmd, yytext, yyleng) == 0) {
	    if (strlen(table->cmd) == yyleng)
		return table;
	    else if (match) {
		printf("ambiguous %s, could be either ``%s'' or ``%s''\n",
		       what, match->cmd, table->cmd);
		return NULL;
	    }
	    else
		match = table;
	}
	table++;
    }

    if (match == NULL)
	printf("unknown %s\n", what);

    return match;
}


/* Generic Commands */

static void quit_cmd(void)
{
    exit(0);
}

static void continue_cmd(void)
{
    Continue = TRUE;
}

static void tron_cmd(void)
{
    extern boolean Tracing;
    Tracing = TRUE;
}

static void troff_cmd(void)
{
    extern boolean Tracing;
    Tracing = FALSE;
}

static void gc_cmd(void)
{
    collect_garbage();
}

/* help_cmd is defined later, because it needs to */
/* reference the cmd table. */
static void help_cmd(void);


/* Frame manipulation commands. */

static void down_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (CurFP == NULL)
	printf("The current thread has nothing on the stack.\n");
    else {
	obj_t *old_fp = obj_rawptr(CurFP[-4]);

	if (old_fp) {
	    CurFP = old_fp;
	    FrameChanged = TRUE;
	}
	else
	    printf("Already at the bottom of the stack.\n");
    }
}

static void up_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (CurFP == NULL)
	printf("The current thread has nothing on the stack.\n");
    else {
	obj_t *fp, *up_fp = NULL;

	for (fp = CurThread->fp;
	     fp != CurFP && fp != NULL;
	     fp = obj_rawptr(fp[-4]))
	    up_fp = fp;

	if (fp == NULL)
	    printf("Can't find the frame above this one.\n");
	else if (up_fp == NULL)
	    printf("Already at the top of the stack.\n");
	else {
	    CurFP = up_fp;
	    FrameChanged = TRUE;
	}
    }
}

static void frame_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (CurFP == NULL)
	printf("The current thread has nothing on the stack.\n");
    else {
	int tok = yylex();

	if (tok == tok_EOF)
	    print_frame(CurFP);
	else if (tok != tok_LITERAL || !obj_is_fixnum(yylval))
	    printf("Bogus frame number, should be an integer.\n");
	else {
	    int frame = fixnum_value(yylval);

	    if (frame < 0)
		printf("Bogus frame number, should be >= 0.\n");
	    else {
		obj_t *fp = CurThread->fp;
		int i;

		for (i = 0; fp != NULL && i < frame; i++)
		    fp = obj_rawptr(fp[-4]);

		if (fp == NULL)
		    printf("Frame number too large, should be < %d.\n", i);
		else {
		    CurFP = fp;
		    FrameChanged = TRUE;
		}
	    }
	}
    }
}

static void backtrace_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else {
	obj_t *fp;
    
	for (fp = CurThread->fp; fp != NULL; fp = obj_rawptr(fp[-4]))
	    print_frame(fp);
    }
}


/* Library/module/variable manipulation commands. */

static void library_cmd(void)
{
    struct library *lib;

    switch (yylex()) {
      case tok_EOF:
	list_libraries();
	putchar('\n');
	if (CurLibrary) {
	    printf("Current library is ");
	    print(library_name(CurLibrary));
	}
	else
	    printf("No library currently selected\n");
	break;
      case tok_SYMBOL:
	lib = find_library(yylval, FALSE);
	if (lib) {
	    CurLibrary = lib;
	    CurModule = NULL;
	}
	else {
	    printf("No library named ");
	    print(yylval);
	}
	break;
      default:
	printf("Syntax error.\n");
	break;
    }
}

static void module_cmd(void)
{
    struct module *module;

    if (CurLibrary == NULL) {
	printf("No library currently selected.\n");
	return;
    }

    switch (yylex()) {
      case tok_EOF:
	list_modules(CurLibrary);
	putchar('\n');
	if (CurModule) {
	    printf("The current module is ");
	    print(module_name(CurModule));
	}
	else
	    printf("No module currently selected.\n");
	break;
      case tok_SYMBOL:
	module = find_module(CurLibrary, yylval, FALSE, FALSE);
	if (module)
	    CurModule = module;
	else {
	    printf("No module named ");
	    print(yylval);
	}
	break;
      default:
	printf("Syntax error.\n");
	break;
    }
}


/* print command. */

static void eval_vars(obj_t expr, boolean *okay, boolean *simple)
{
    obj_t kind = HEAD(expr);

    if (kind == keyword("literal")) {
	/* Don't have to do anything for literals. */
    }
    else if (kind == keyword("variable")) {
	/* Variable reference. */
	obj_t name = TAIL(expr);
	if (CurModule == NULL) {
	    if (*okay) {
		printf("No module currently selected\n");
		*okay = FALSE;
	    }
	}
	else {
	    struct variable *var = find_variable(CurModule, name,
						 FALSE, FALSE);
	    if (var == NULL) {
		printf("no variable named %s in module %s\n",
		       sym_name(name),
		       sym_name(module_name(CurModule)));
		*okay = FALSE;
	    }
	    else {
		obj_t value = var->value;
		if (value == obj_Unbound) {
		    printf("variable %s in module %s is unbound\n",
			   sym_name(name), sym_name(module_name(CurModule)));
		    *okay = FALSE;
		}
		else {
		    HEAD(expr) = keyword("literal");
		    TAIL(expr) = value;
		}
	    }
	}
    }
    else if (kind == keyword("funcall")) {
	obj_t args;

	for (args = TAIL(expr); args != obj_Nil; args = TAIL(args))
	    eval_vars(HEAD(args), okay, simple);
    
	*simple = FALSE;
    }
    else
	lose("Parser returned something strange.");
}

static void do_funcall(struct thread *thread, obj_t args, int nargs);
static void do_more_prints(struct thread *thread, obj_t exprs);
static void do_more_calls(struct thread *thread, obj_t exprs);

static void funcall_return(struct thread *thread, obj_t *vals)
{
    do_return(thread, pop_linkage(thread), vals);
}

static void continue_funcall(struct thread *thread, obj_t *vals)
{
    obj_t args = vals[-2];
    int nargs = fixnum_value(vals[-1]);

    vals[-2] = vals[0];
    thread->sp = vals - 1;

    do_funcall(thread, args, nargs);
}

static void do_funcall(struct thread *thread, obj_t args, int nargs)
{
    while (args != obj_Nil) {
	obj_t arg = HEAD(args);
	obj_t kind = HEAD(arg);

	if (kind == keyword("literal")) {
	    *thread->sp++ = TAIL(arg);
	    nargs++;
	}
	else if (kind == keyword("funcall")) {
	    *thread->sp++ = TAIL(args);
	    *thread->sp++ = make_fixnum(nargs+1);
	    *thread->sp++ = do_funcall_func;
	    *thread->sp++ = TAIL(arg);
	    set_c_continuation(thread, continue_funcall);
	    invoke(thread, 1);
	}
	else
	    lose("Print command found a strange expression.");
	args = TAIL(args);
    }
    /* One of the ``args'' is the function. */
    check_type(thread->sp[-nargs], obj_FunctionClass);
    set_c_continuation(thread, funcall_return);
    invoke(thread, nargs-1);
}

static void do_funcall_start(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t funcall_args = args[0];

    assert(nargs == 1);

    push_linkage(thread, args);
    do_funcall(thread, funcall_args, 0);
}

static void do_print(struct thread *thread, obj_t *vals)
{
    obj_t *old_sp = vals - 1;
    obj_t exprs = *old_sp;

    if (vals == thread->sp)
	printf("[returned 0 values]\n");
    else {
	while (1) {
	    prin1(*vals++);
	    if (vals < thread->sp)
		printf(", ");
	    else {
		putchar('\n');
		break;
	    }
	}
    }

    thread->sp = old_sp;

    do_more_prints(thread, exprs);
}

static void do_more_prints(struct thread *thread, obj_t exprs)
{
    while (exprs != obj_Nil) {
	obj_t expr = HEAD(exprs);
	obj_t kind = HEAD(expr);

	if (kind == keyword("literal")) {
	    print(TAIL(expr));
	}
	else if (kind == keyword("funcall")) {
	    *thread->sp++ = TAIL(exprs);
	    *thread->sp++ = do_funcall_func;
	    *thread->sp++ = TAIL(expr);
	    set_c_continuation(thread, do_print);
	    invoke(thread, 1);
	}
	else
	    lose("Print command found a strange expression.");
    }

    {
	obj_t *old_sp = pop_linkage(thread);
	thread->sp = old_sp;
	do_return(thread, old_sp, old_sp);
    }
}

static void do_print_start(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;

    assert(nargs == 1);

    push_linkage(thread, args);
    do_more_prints(thread, args[0]);
}

static void call_or_print(boolean call)
{
    obj_t exprs = parse_exprs();
    boolean okay = TRUE;
    boolean simple = TRUE;
    obj_t expr;
    struct thread *thread;

    if (exprs == obj_False) {
	printf("Invalid expression.\n");
	return;
    }

    for (expr = exprs; expr != obj_Nil; expr = TAIL(expr))
	eval_vars(HEAD(expr), &okay, &simple);

    if (!okay)
	return;

    if (simple) {
	for (expr = exprs; expr != obj_Nil; expr = TAIL(expr))
	    print(TAIL(HEAD(expr)));
	return;
    }

    if (CurThread == NULL) {
	obj_t debug_name;
	if (call)
	    debug_name = make_string("debugger call command");
	else
	    debug_name = make_string("debugger print command");
	thread = thread_create(debug_name);
	set_c_continuation(thread, kill_me);
    }
    else {
	thread = CurThread;
	thread_push_escape(thread);
	set_c_continuation(thread, debugger_cmd_finished);
    }

    suspend_other_threads(thread);

    if (call || debugger_print_var == NULL
	  || debugger_print_var->value == obj_Unbound)
	*thread->sp++ = do_print_func;
    else
	*thread->sp++ = debugger_print_var->value;
    *thread->sp++ = exprs;

    thread_restart(thread);
    Continue = TRUE;
}


static void call_cmd(void)
{
    call_or_print(TRUE);
}

static void print_cmd(void)
{
    call_or_print(FALSE);
}


/* Restart commands. */

static void abort_cmd(void)
{
    struct thread *thread;

    if (debugger_abort_var == NULL
	  || debugger_abort_var->value == obj_Unbound) {
	printf("debugger-abort undefined.\n");
	return;
    }

    if ((thread = CurThread) == NULL) {
	printf("No current thread.\n");
	return;
    }

    thread_push_escape(thread);
    set_c_continuation(thread, debugger_cmd_finished);
    
    suspend_other_threads(thread);
    
    *thread->sp++ = debugger_abort_var->value;
    thread_restart(thread);
    Continue = TRUE;
}

static void describe_restarts(void)
{
    obj_t cond;
    struct thread *thread;

    if (debugger_restarts_var == NULL
	  || debugger_restarts_var->value == obj_Unbound) {
	printf("debugger-describe-restarts undefined.\n");
	return;
    }

    if ((thread = CurThread) == NULL) {
	printf("No current thread.\n");
	return;
    }

    if (thread->status == status_Debuggered)
	cond = thread->datum;
    else
	cond = obj_False;

    thread_push_escape(thread);
    set_c_continuation(thread, debugger_cmd_finished);
    
    suspend_other_threads(thread);
    
    *thread->sp++ = debugger_restarts_var->value;
    *thread->sp++ = cond;
    thread_restart(thread);
    Continue = TRUE;
}

static void maybe_return(struct thread *thread, obj_t *vals)
{
    if (vals[0] == obj_False)
	debugger_cmd_finished(thread, vals);
    else {
	obj_t value_vec = vals[1];
	int len = SOVEC(value_vec)->length;
	int i;
	obj_t *old_sp;

	thread->sp = vals;
	thread_pop_escape(thread);
	thread_buggered(thread);

	old_sp = pop_linkage(thread);

	thread->sp = old_sp + len;

	for (i = 0; i < len; i++)
	    old_sp[i] = SOVEC(value_vec)->contents[i];

	restart_other_threads(thread);

	do_return(thread, old_sp, old_sp);
    }
}

static void do_restart(obj_t restart)
{
    obj_t cond;
    struct thread *thread;

    if (debugger_restart_var == NULL
	  || debugger_restart_var->value == obj_Unbound) {
	printf("debugger-restart undefined.\n");
	return;
    }

    if ((thread = CurThread) == NULL) {
	printf("No current thread.\n");
	return;
    }

    if (thread->status == status_Debuggered)
	cond = thread->datum;
    else
	cond = obj_False;

    thread_push_escape(thread);
    set_c_continuation(thread, maybe_return);
    
    suspend_other_threads(thread);
    
    *thread->sp++ = debugger_restart_var->value;
    *thread->sp++ = cond;
    *thread->sp++ = restart;
    thread_restart(thread);
    Continue = TRUE;
}

static void restart_cmd(void)
{
    int tok = yylex();

    if (tok == tok_EOF)
	describe_restarts();
    else if (tok != tok_LITERAL || !obj_is_fixnum(yylval))
	printf("Bogus restart number, should be an integer.\n");
    else {
	int restart = fixnum_value(yylval);

	if (restart < 0)
	    printf("Bogus restart number, should be >= 0.\n");
	else
	    do_restart(yylval);
    }
}

static void return_cmd(void)
{
    obj_t cond;
    struct thread *thread;

    if (debugger_return_var == NULL
	  || debugger_return_var->value == obj_Unbound) {
	printf("debugger-return undefined.\n");
	return;
    }

    if ((thread = CurThread) == NULL) {
	printf("No current thread.\n");
	return;
    }

    if (thread->status == status_Debuggered)
	cond = thread->datum;
    else {
	printf("The current thread did not call invoke-debugger\n");
	return;
    }

    thread_push_escape(thread);
    set_c_continuation(thread, maybe_return);
    
    suspend_other_threads(thread);
    
    *thread->sp++ = debugger_return_var->value;
    *thread->sp++ = cond;
    thread_restart(thread);
    Continue = TRUE;
}
    

/* Thread commands. */

static struct thread *find_thread(void)
{
    int id;
    struct thread_list *threads;

    if (instancep(yylval, obj_IntegerClass))
	id = fixnum_value(yylval);
    else
	id = -1;

    for (threads = all_threads(); threads != NULL; threads = threads->next) {
	struct thread *thread = threads->thread;
	if (thread->debug_name == yylval || thread->id == id)
	    return thread;
    }

    printf("No thread named ");
    print(yylval);
    return NULL;
}
    

static void thread_cmd(void)
{
    struct thread_list *threads;
    struct thread *thread;

    switch (yylex()) {
      case tok_EOF:
	for (threads=all_threads(); threads != NULL; threads=threads->next) {
	    if (threads->thread == CurThread)
		printf("c ");
	    else
		printf("  ");
	    print_thread(threads->thread);
	}
	break;

      case tok_SYMBOL:
      case tok_LITERAL:
	thread = find_thread();
	if (thread != NULL) {
	    CurThread = thread;
	    ThreadChanged = TRUE;
	}
	break;

      default:
	printf("Bogus thread identifier: ");
	print(yylval);
	printf("should be either a symbol or integer.");
	break;
    }
}

static void kill_cmd(void)
{
    struct thread *thread;

    switch (yylex()) {
      case tok_EOF:
	thread = CurThread;
	if (thread == NULL) {
	    printf("No current thread selected.\n");
	    return;
	}
	break;

      case tok_SYMBOL:
      case tok_LITERAL:
	thread = find_thread();
	if (thread == NULL)
	    return;
	break;

      default:
	printf("Bogus thread identifier: ");
	print(yylval);
	printf("should be either a symbol or integer.");
	return;
    }

    thread_kill(thread);
    if (thread == CurThread) {
	printf("killed the current thread, hence it is no longer current.\n");
	CurThread = NULL;
	CurFP = NULL;
	ThreadChanged = FALSE;
	FrameChanged = FALSE;
    }
}
    
static void disable_cmd(void)
{
    struct thread *thread;

    switch (yylex()) {
      case tok_EOF:
	thread = CurThread;
	if (thread == NULL) {
	    printf("No current thread selected.\n");
	    return;
	}
	break;

      case tok_SYMBOL:
      case tok_LITERAL:
	thread = find_thread();
	if (thread == NULL)
	    return;
	break;

      default:
	printf("Bogus thread identifier: ");
	print(yylval);
	printf("should be either a symbol or integer.");
	return;
    }

    thread_suspend(thread);
    print_thread(thread);
}
    
static void enable_cmd(void)
{
    struct thread *thread;

    switch (yylex()) {
      case tok_EOF:
	thread = CurThread;
	if (thread == NULL) {
	    printf("No current thread selected.\n");
	    return;
	}
	break;

      case tok_SYMBOL:
      case tok_LITERAL:
	thread = find_thread();
	if (thread == NULL)
	    return;
	break;

      default:
	printf("Bogus thread identifier: ");
	print(yylval);
	printf("should be either a symbol or integer.");
	return;
    }

    if (thread->suspend_count == 0) {
	printf("thread ");
	prin1(yylval);
	printf(" isn't suspended\n");
	return;
    }

    while (thread->suspend_count > 0)
	thread_restart(thread);
    print_thread(thread);
}
    

/* Command table. */

static struct cmd_entry Cmds[] = {
    {"abort", "abort\t\tInvoke the first available <abort> restart.",
	 abort_cmd},
    {"backtrace",
	 "backtrace\tDisplay a stack backtrace for the current thread.",
	 backtrace_cmd},
    {"c", NULL, continue_cmd},
    {"call", "call expr...\tCall each expr in its own thread.", call_cmd},
    {"continue", "continue\tContinue execution.", continue_cmd},
    {"d", NULL, down_cmd},
    {"down", "down\t\tMove down one frame.", down_cmd},
    {"disable", "disable thread\tRestart the given thread.", disable_cmd},
    {"enable", "enable thread\tSuspend the given thread.", enable_cmd},
    {"frame", "frame num\tMove to the given frame.", frame_cmd},
    {"gc", "gc\t\tCollect garbage.", gc_cmd},
    {"help", "help [topic]\tDisplay help about some topic.", help_cmd},
    {"kill", "kill thread\tKill the given thread.", kill_cmd},
    {"library",
	 "library [lib]\tSwitch to given library or list all libraries.",
	 library_cmd},
    {"module",
 "module [module]\tSwitch to given module or list modules in current library.",
	 module_cmd},
    {"print", "print expr...\tPrint the values of the listed vars", print_cmd},
    {"restart", "restart [num]\tList or invoke one of the available restarts.",
	 restart_cmd},
    {"return",
	 "return\t\tReturn from this call to invoke-debugger (if allowed)",
	 return_cmd},
    {"thread", "thread [name]\tSwitch to given thread or list all threads.",
	 thread_cmd},
    {"troff", "troff\t\tTurn function/return tracing off.", troff_cmd},
    {"tron", "troff\t\tTurn function/return tracing on.", tron_cmd},
    {"up", "up\t\tMove up one frame.", up_cmd},
    {"quit", "quit\t\tQuit.", quit_cmd},
    {NULL, NULL, NULL}
};
    
static void do_cmd(void)
{
    struct cmd_entry *entry = find_cmd(Cmds, "command");

    if (entry)
	(*entry->fn)();
}

static void help_cmd(void)
{
    struct cmd_entry *ptr;

    for (ptr = Cmds; ptr->cmd != NULL; ptr++)
	printf("%s\n", ptr->help);
}


/* Stuff to explain the reason why we dropped into the debugger. */

static void explain_condition(struct thread *thread, obj_t condition)
{
    if (instancep(condition, obj_SimpleObjectVectorClass)) {
	char *fmt = string_chars(SOVEC(condition)->contents[0]);

	putchar('\n');
	vformat(fmt, SOVEC(condition)->contents+1);
	printf("\n\n");
    }
    else if (debugger_report_var == NULL
	     || debugger_report_var->value == obj_Unbound) {
	printf("\ninvoke-debugger called from dylan.  Condition = ");
	print(condition);
	putchar('\n');
    }
    else {
	thread_push_escape(thread);
	set_c_continuation(thread, debugger_cmd_finished);

	suspend_other_threads(thread);

	*thread->sp++ = debugger_report_var->value;
	*thread->sp++ = condition;
	thread_restart(thread);
	Continue = TRUE;
    }
}

static void explain_debugger_invocation(void)
{
    struct thread *thread = thread_current();

    CurThread = thread;
    ThreadChanged = TRUE;

    if (thread == NULL) {
	printf("Debugger explicitly invoked, but no current thread?\n");
	ThreadChanged = FALSE;
	FrameChanged = FALSE;
	return;
    }

    if (thread->status != status_Debuggered) {
	printf("Debugger explicitly invoked, but not by current thread?\n");
	return;
    }

    explain_condition(thread, thread->datum);
}

static void explain_reason(enum pause_reason reason)
{
    struct thread_list *threads;

    switch (reason) {
      case pause_NoReason:
	break;
      case pause_NothingToRun:
	printf("All threads exited.\n");
	CurThread = NULL;
	ThreadChanged = FALSE;
	FrameChanged = FALSE;
	break;
      case pause_Interrupted:
	printf("Interrupted\n");
	CurThread = thread_current();
	ThreadChanged = TRUE;
	break;
      case pause_DebuggerInvoked:
	explain_debugger_invocation();
	break;
      case pause_HitBreakpoint:
	printf("Breakpoint\n");
	CurThread = thread_current();
	ThreadChanged = TRUE;
	break;
      case pause_DebuggerCommandFinished:
	break;
    }

    if (CurThread != NULL) {
	for (threads = all_threads(); threads != NULL; threads = threads->next)
	    if (threads->thread == CurThread)
		break;
	if (threads == NULL) {
	    printf("Current thread no longer exists.\n");
	    CurThread = NULL;
	    ThreadChanged = FALSE;
	    FrameChanged = FALSE;
	}
    }
}


/* The main debugger loop */

void invoke_debugger(enum pause_reason reason)
{
    char line[256];

    Continue = FALSE;

    explain_reason(reason);

    while (Continue) {
	Continue = FALSE;
	reason = do_stuff();
	explain_reason(reason);
    }

    if (!isatty(fileno(stdin)))
	exit(1);

    lex_init();

    while (1) {
	thread_set_current(NULL);

	while (!Continue) {
	    maybe_print_frame();

	    printf("mindy> ");
	    fflush(stdout);

	    if (fgets(line, sizeof(line), stdin) == NULL) {
		putchar('\n');
		lex_setup("quit", 4);
	    }
	    else
		lex_setup(line, strlen(line));

	    switch (yylex()) {
	      case tok_EOF:
		break;
	      case tok_SYMBOL:
		do_cmd();
		break;
	      default:
		printf("Bogus command -- use ``help'' for help.\n");
		break;
	    }
	}
	Continue = FALSE;

	reason = do_stuff();
	explain_reason(reason);
    }
}


/* GC stuff. */

void scavenge_debug_roots(void)
{
    scavenge(&do_print_func);
    scavenge(&do_funcall_func);
}


/* Initialization stuff. */

void init_debug_functions(void)
{
    do_print_func = make_raw_function("debug-print", 1, FALSE, obj_False,
				      obj_Nil, obj_ObjectClass,
				      do_print_start);
    do_funcall_func = make_raw_function("debug-funcall", 1, FALSE, obj_False,
					obj_Nil, obj_ObjectClass,
					do_funcall_start);
    debugger_print_var = find_variable(module_BuiltinStuff,
				       symbol("debugger-print"),
				       FALSE, TRUE);
    debugger_report_var = find_variable(module_BuiltinStuff,
					symbol("debugger-report-condition"),
					FALSE, TRUE);
    debugger_abort_var = find_variable(module_BuiltinStuff,
				       symbol("debugger-abort"),
				       FALSE, TRUE);
    debugger_restarts_var = find_variable(module_BuiltinStuff,
					  symbol("debugger-describe-restarts"),
					  FALSE, TRUE);
    debugger_restart_var = find_variable(module_BuiltinStuff,
					 symbol("debugger-restart"),
					 FALSE, TRUE);
    debugger_return_var = find_variable(module_BuiltinStuff,
					symbol("debugger-return"),
					FALSE, TRUE);
}
