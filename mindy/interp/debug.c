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
* $Header: /home/housel/work/rcs/gd/src/mindy/interp/debug.c,v 1.25 1994/06/13 21:14:05 hallgren Exp $
*
* This file does whatever.
*
\**********************************************************************/

#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#ifdef MACH
extern int isatty(int fd);
#endif
#if defined(hpux) || defined(__osf__)
#define pause buttplug
#include <unistd.h>
#undef pause
#endif
#ifdef sgi
#include <stdlib.h>
#endif

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
#include "interp.h"
#include "value.h"
#include "error.h"
#include "gc.h"
#include "brkpt.h"
#include "../comp/byteops.h"

struct library *CurLibrary = NULL;
struct module *CurModule = NULL;

struct frame_info {
    struct frame_info *up;
    struct frame_info *down;
    obj_t *fp;
    obj_t component;
    int pc;
    obj_t source_file;
    obj_t mtime;
    int line;
    obj_t locals;
};

static jmp_buf BlowOffCmd;
static struct thread *CurThread = NULL;
static obj_t CurThreadObj = NULL;
static struct frame_info *CurFrame = NULL, *TopFrame = NULL;
static int PrevLine = -1;
static boolean ThreadChanged = FALSE, FrameChanged = FALSE;
static boolean Continue;

static obj_t do_eval_func;
static obj_t do_print_func;

static struct variable *debugger_eval_var;
static struct variable *debugger_flush_var;
static struct variable *debugger_call_var;
static struct variable *debugger_print_var;
static struct variable *debugger_report_var;
static struct variable *debugger_abort_var;
static struct variable *debugger_restarts_var;
static struct variable *debugger_restart_var;
static struct variable *debugger_return_var;


/* Frame utilities. */

static struct frame_info *make_frame(struct frame_info *up, obj_t *fp,
				     obj_t component, int pc)
{
    struct frame_info *res = malloc(sizeof(*res));
    obj_t debug_info;
    int len, i, n_const;

    res->up = up;
    res->down = NULL;
    res->fp = fp;
    res->component = component;
    res->pc = pc;
    res->source_file = obj_False;
    res->mtime = make_fixnum(0);
    res->line = 0;
    res->locals = obj_False;

    if (obj_is_fixnum(component))
	return res;

    res->source_file = COMPONENT(component)->source_file;
    res->mtime = COMPONENT(component)->mtime;

    debug_info = COMPONENT(component)->debug_info;
    if (debug_info == obj_False)
	return res;

    n_const = COMPONENT(component)->n_constants;
    pc -= (char *)(&COMPONENT(component)->constant[n_const])-(char *)component;

    if (pc < 0)
	return res;

    len = SOVEC(debug_info)->length;
    for (i = 0; i < len; i++) {
	obj_t entry = SOVEC(debug_info)->contents[i];
	pc -= fixnum_value(SOVEC(entry)->contents[1]);
	if (pc < 0) {
	    res->line = fixnum_value(SOVEC(entry)->contents[0]);
	    res->locals = SOVEC(entry)->contents[2];
	    break;
	}
    }

    return res;
}

static struct frame_info *top_frame(struct thread *thread)
{
    if (thread)
	return make_frame(NULL, thread->fp, thread->component, thread->pc);
    else
	return NULL;
}

static struct frame_info *frame_down(struct frame_info *frame)
{
    if (frame->down != NULL)
	return frame->down;
    else {
	obj_t *fp = frame->fp;
	obj_t *old_fp = obj_rawptr(fp[-4]);

	if (old_fp) {
	    frame->down = make_frame(frame, old_fp, fp[-2],
				     fixnum_value(fp[-1]));
	    return frame->down;
	}
	else
	    return NULL;
    }
}

static struct frame_info *frame_up(struct frame_info *frame)
{
    return frame->up;
}

static void free_frames(struct frame_info *frame)
{
    struct frame_info *next;

    while (frame) {
	next = frame->down;
	free(frame);
	if (frame == CurFrame)
	    CurFrame = NULL;
	frame = next;
    }
}

static void scav_frames(struct frame_info *frame)
{
    while (frame != NULL) {
	scavenge(&frame->component);
	scavenge(&frame->source_file);
	scavenge(&frame->mtime);
	scavenge(&frame->locals);
	frame = frame->down;
    }
}

static void print_frame(struct frame_info *frame, boolean print_line)
{
    obj_t *ptr = obj_rawptr(frame->fp[-3]);
    obj_t *end = frame->fp - 4;

    printf("fp 0x%08lx: ", (unsigned long)frame->fp);
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
    if (frame->source_file == obj_False || !print_line)
	printf(")\n");
    else {
	printf(") [%s", string_chars(frame->source_file));
	if (frame->line == 0)
	    printf("]\n");
	else
	    printf(", line %d]\n", frame->line);
    }
}

static void set_frame(struct frame_info *frame)
{
    if (CurFrame == NULL || frame == NULL || CurFrame->fp != frame->fp) {
	FrameChanged = TRUE;
	PrevLine = -1;
    }

    CurFrame = frame;
}



/* Thread utilities. */

static void set_thread(struct thread *thread)
{
    CurThread = thread;
    if (thread != NULL)
	CurThreadObj = thread->thread_obj;
    else
	CurThreadObj = NULL;
    ThreadChanged = TRUE;
    free_frames(TopFrame);
    TopFrame = top_frame(thread);
    set_frame(TopFrame);
}

static void print_thread(struct thread *thread)
{
    static char *status_chars = {"RSDBW"};
    printf("[%d] %c ", thread->id, status_chars[(int)thread->status]);
    if (thread->suspend_count != 0)
	printf("%d ", thread->suspend_count);
    else
	printf("  ");
    if (THREAD(thread->thread_obj)->debug_name != obj_False)
	print(THREAD(thread->thread_obj)->debug_name);
    else
	putchar('\n');
}

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
    set_thread(NULL);
    pause(pause_DebuggerCommandFinished);
}

static void debugger_cmd_finished(struct thread *thread, obj_t *vals)
{
    thread->sp = vals;
    thread_pop_escape(thread);
    restart_other_threads(thread);
    pause(pause_DebuggerCommandFinished);
}

static void validate_thread_and_frame()
{
    if (CurThread != NULL) {
	if (THREAD(CurThreadObj)->thread == NULL) {
	    printf("Current thread no longer exists.\n");
	    set_thread(NULL);
	    ThreadChanged = FALSE;
	    FrameChanged = FALSE;
	    return;
	}

	if (TopFrame == NULL) {
	    if (CurThread->fp != NULL) {
		TopFrame = top_frame(CurThread);
		set_frame(TopFrame);
	    }
	}
	else {
	    if (CurThread->fp != TopFrame->fp
		  || CurThread->component != TopFrame->component
		  || CurThread->pc != TopFrame->pc) {
		struct frame_info *old_top = TopFrame;
		TopFrame = top_frame(CurThread);
		set_frame(TopFrame);
		free_frames(old_top);
	    }
	}
    }
}


/* Source code stuff. */

static obj_t cur_source_file = NULL;
static time_t cur_mtime = 0;
static FILE *cur_source_stream = NULL;
static long *line_offsets = NULL;
static int lines_alloced = 0;
static int lines_found = 0;
static char **source_directories = NULL;

static FILE *find_source_line(obj_t file, obj_t mtime, int line)
{
    int c;

    if (file == obj_False)
	return NULL;

    if (file != cur_source_file) {
	char *name = string_chars(file);

	if (cur_source_stream != NULL) {
	    fclose(cur_source_stream);
	    cur_source_stream = NULL;
	}
	
	if (source_directories == NULL || name[0] == '/')
	    cur_source_stream = fopen(name, "r");
	else
	    cur_source_stream = NULL;

	cur_source_file = file;
	cur_mtime = 0;
	if (line_offsets == NULL) {
	    lines_alloced = 1000;
	    line_offsets = malloc(sizeof(long) * lines_alloced);
	    line_offsets[0] = 0;
	    line_offsets[1] = 0;
	}
	lines_found = 1;
    }

    if (cur_source_stream == NULL)
	return NULL;

    if (cur_mtime == 0) {
	struct stat buf;

	fstat(fileno(cur_source_stream), &buf);

	cur_mtime = buf.st_mtime;
    }

    if (cur_mtime != fixnum_value(mtime))
	printf("\nWarning: %s has changed", string_chars(file));

    if (line > lines_found) {
	fseek(cur_source_stream, line_offsets[lines_found], 0);
	while ((c = getc(cur_source_stream)) != EOF) {
	    if (c == '\n') {
		lines_found++;
		if (lines_found > lines_alloced) {
		    lines_alloced *= 2;
		    line_offsets = realloc(line_offsets,
					   sizeof(long) * lines_alloced);
		}
		line_offsets[lines_found] = ftell(cur_source_stream);
		if (lines_found == line)
		    break;
	    }
	}
    }
    else
	fseek(cur_source_stream, line_offsets[line], 0);

    return cur_source_stream;
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

    set_thread(thread);

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
    switch (reason) {
      case pause_NoReason:
      case pause_PickNewThread:
      case pause_DebuggerCommandFinished:
	validate_thread_and_frame();
	break;
      case pause_NothingToRun:
	printf("All threads exited.\n");
	set_thread(NULL);
	ThreadChanged = FALSE;
	FrameChanged = FALSE;
	break;
      case pause_Interrupted:
	printf("Interrupted\n");
	set_thread(thread_current());
	break;
      case pause_DebuggerInvoked:
	explain_debugger_invocation();
	break;
      case pause_HitBreakpoint:
	printf("Breakpoint\n");
	set_thread(thread_current());
	break;
    }
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

static void error_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (CurThread->status != status_Debuggered)
	printf("The current thread did not stop due to an error.\n");
    else
	explain_condition(CurThread, CurThread->datum);
}

/* help_cmd is defined later, because it needs to */
/* reference the cmd table. */
static void help_cmd(void);


/* Frame manipulation commands. */

static void down_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (CurFrame == NULL)
	printf("The current thread has nothing on the stack.\n");
    else {
	struct frame_info *down = frame_down(CurFrame);

	if (down != NULL)
	    set_frame(down);
	else
	    printf("Already at the bottom of the stack.\n");
    }
}

static void up_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (CurFrame == NULL)
	printf("The current thread has nothing on the stack.\n");
    else {
	struct frame_info *up = frame_up(CurFrame);

	if (up != NULL)
	    set_frame(up);
	else
	    printf("Already at the top of the stack.\n");
    }
}

static void frame_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else if (TopFrame == NULL)
	printf("The current thread has nothing on the stack.\n");
    else {
	int tok = yylex();

	if (tok == tok_EOF)
	    FrameChanged = TRUE;
	else if (tok != tok_LITERAL || !obj_is_fixnum(yylval))
	    printf("Bogus frame number, should be an integer.\n");
	else {
	    int num = fixnum_value(yylval);

	    if (num < 0)
		printf("Bogus frame number, should be >= 0.\n");
	    else {
		struct frame_info *frame = TopFrame;
		int i;

		for (i = 0; frame != NULL && i < num; i++)
		    frame = frame_down(frame);

		if (frame == NULL)
		    printf("Frame number too large, should be < %d.\n", i);
		else
		    set_frame(frame);
	    }
	}
    }
}

static boolean backtrace_punted;

static void punt_backtrace(void)
{
    backtrace_punted = TRUE;
}

static void backtrace_cmd(void)
{
    if (CurThread == NULL)
	printf("No current thread.\n");
    else {
	struct frame_info *frame;
    
	backtrace_punted = FALSE;
	set_interrupt_handler(punt_backtrace);

	for (frame = TopFrame; frame != NULL; frame = frame_down(frame)) {
	    if (backtrace_punted) {
		printf("interrupted\n");
		break;
	    }
	    print_frame(frame, TRUE);
	}

	clear_interrupt_handler();
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


/* Locals command. */

static void locals_cmd()
{
    obj_t locals;

    if (CurFrame == NULL) {
	printf("No current frame.\n");
	return;
    }

    locals = CurFrame->locals;
    if (locals == obj_False) {
	printf("No debug info for this frame.\n");
	return;
    }

    while (locals != obj_Nil) {
	obj_t vec = HEAD(locals);
	int len = SOVEC(vec)->length;
	int i;

	for (i = 0; i < len; i++) {
	    obj_t entry = SOVEC(vec)->contents[i];
	    obj_t symbol = SOVEC(entry)->contents[0];
	    int loc_info = fixnum_value(SOVEC(entry)->contents[1]);
	    boolean indirect = loc_info & 2;
	    boolean argument = loc_info & 1;
	    int offset = loc_info >> 2;
	    obj_t value;

	    if (argument)
		value = CurFrame->fp[-offset-5];
	    else
		value = CurFrame->fp[offset];
	    if (indirect)
		value = value_cell_ref(value);

	    prin1(symbol);
	    printf(": ");
	    print(value);
	}

	locals = TAIL(locals);
    }
}



/* Flush command. */

static void flush_cmd(void)
{
    struct thread *thread;

    if (debugger_flush_var == NULL
	  || debugger_flush_var->value == obj_Unbound) {
	printf("debugger-flush undefined.\n");
	return;
    }

    if ((thread = CurThread) == NULL) {
	thread = thread_create(make_string("debugger flush cmd"));
	set_c_continuation(thread, kill_me);
    }

    thread_push_escape(thread);
    set_c_continuation(thread, debugger_cmd_finished);
    
    suspend_other_threads(thread);
    
    *thread->sp++ = debugger_flush_var->value;
    thread_restart(thread);
    Continue = TRUE;
}



/* print command. */

static void eval_vars(obj_t expr, boolean *okay, boolean *simple)
{
    obj_t kind = HEAD(expr);

    if (kind == symbol("literal")) {
	/* Don't have to do anything for literals. */
    }
    else if (kind == symbol("variable")) {
	/* Variable reference. */
	obj_t name = TAIL(expr);
	if (CurFrame != NULL && CurFrame->locals != obj_False) {
	    obj_t list = CurFrame->locals;
	    while (list != obj_Nil) {
		obj_t vec = HEAD(list);
		int len = SOVEC(vec)->length;
		int i;
		for (i = 0; i < len; i++) {
		    obj_t entry = SOVEC(vec)->contents[i];
		    if (SOVEC(entry)->contents[0] == name) {
			int loc_info = fixnum_value(SOVEC(entry)->contents[1]);
			boolean indirect = loc_info & 2;
			boolean argument = loc_info & 1;
			int offset = loc_info >> 2;
			obj_t value;

			if (argument)
			    value = CurFrame->fp[-offset-5];
			else
			    value = CurFrame->fp[offset];
			if (indirect)
			    value = value_cell_ref(value);
			HEAD(expr) = symbol("literal");
			TAIL(expr) = value;
			return;
		    }
		}
		list = TAIL(list);
	    }
	}
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
		    HEAD(expr) = symbol("literal");
		    TAIL(expr) = value;
		}
	    }
	}
    }
    else if (kind == symbol("debug-var"))
	*simple = FALSE;
    else if (kind == symbol("arg")) {
	if (CurFrame == NULL)  {
	    printf("No current frame.\n");
	    *okay = FALSE;
	}
	else {
	    obj_t *fp = CurFrame->fp;
	    obj_t *args = ((obj_t *)obj_rawptr(fp[-3])) + 1;
	    int nargs = fp - args - 4;
	    int arg = fixnum_value(TAIL(expr));

	    if (arg >= nargs) {
		printf("%d too large -- Only %d argument%s\n", arg, nargs,
		       nargs == 1 ? "" : "s");
		*okay = FALSE;
	    }
	    else {
		HEAD(expr) = symbol("literal");
		TAIL(expr) = args[arg];
	    }
	}
    }
    else if (kind == symbol("funcall")) {
	obj_t args;

	for (args = TAIL(expr); args != obj_Nil; args = TAIL(args))
	    eval_vars(HEAD(args), okay, simple);
    
	*simple = FALSE;
    }
    else
	lose("Parser returned something strange.");
}

static void do_eval(struct thread *thread, obj_t args, int nargs);
static void do_more_prints(struct thread *thread, obj_t exprs);

static void eval_return(struct thread *thread, obj_t *vals)
{
    do_return(thread, pop_linkage(thread), vals);
}

static void continue_eval(struct thread *thread, obj_t *vals)
{
    obj_t args = vals[-2];
    int nargs = fixnum_value(vals[-1]);

    vals[-2] = vals[0];
    thread->sp = vals - 1;

    do_eval(thread, args, nargs);
}

static void do_eval(struct thread *thread, obj_t args, int nargs)
{
    while (args != obj_Nil) {
	obj_t arg = HEAD(args);
	obj_t kind = HEAD(arg);

	if (kind == symbol("literal")) {
	    *thread->sp++ = TAIL(arg);
	    nargs++;
	}
	else if (kind == symbol("funcall")) {
	    *thread->sp++ = TAIL(args);
	    *thread->sp++ = make_fixnum(nargs+1);
	    *thread->sp++ = do_eval_func;
	    *thread->sp++ = TAIL(arg);
	    set_c_continuation(thread, continue_eval);
	    invoke(thread, 1);
	    return;
	}
	else
	    lose("Print command found a strange expression.");
	args = TAIL(args);
    }
    /* One of the ``args'' is the function. */
    check_type(thread->sp[-nargs], obj_FunctionClass);
    set_c_continuation(thread, eval_return);
    invoke(thread, nargs-1);
}

static void do_eval_start(struct thread *thread, int nargs)
{
    obj_t *args = thread->sp - 1;
    obj_t eval_args = args[0];

    assert(nargs == 1);

    push_linkage(thread, args);
    do_eval(thread, eval_args, 0);
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

	if (kind == symbol("literal")) {
	    print(TAIL(expr));
	}
	else if (kind == symbol("funcall")) {
	    *thread->sp++ = TAIL(exprs);
	    *thread->sp++ = do_eval_func;
	    *thread->sp++ = TAIL(expr);
	    set_c_continuation(thread, do_print);
	    invoke(thread, 1);
	    return;
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

static void call_or_print(struct variable *var)
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
    if (exprs == obj_Nil) {
	printf("No expression.\n");
	return;
    }

    for (expr = exprs; expr != obj_Nil; expr = TAIL(expr))
	eval_vars(HEAD(expr), &okay, &simple);

    if (!okay)
	return;

    if (simple && (var == NULL || var->value == obj_Unbound)) {
	for (expr = exprs; expr != obj_Nil; expr = TAIL(expr))
	    print(TAIL(HEAD(expr)));
	return;
    }

    if (CurThread == NULL) {
	thread = thread_create(var ? var->name : obj_False);
	set_c_continuation(thread, kill_me);
    }
    else {
	thread = CurThread;
	thread_push_escape(thread);
	set_c_continuation(thread, debugger_cmd_finished);
    }

    suspend_other_threads(thread);

    if (var == NULL || var->value == obj_Unbound)
	*thread->sp++ = do_print_func;
    else
	*thread->sp++ = var->value;
    *thread->sp++ = exprs;

    thread_restart(thread);
    Continue = TRUE;
}


static void call_cmd(void)
{
    call_or_print(debugger_call_var);
}

static void print_cmd(void)
{
    call_or_print(debugger_print_var);
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
    if (vals[0] == obj_False) {
	thread->sp = vals;
	thread_pop_escape(thread);
	thread->sp--;
	pause(pause_DebuggerCommandFinished);
    }
    else {
	obj_t value_vec = vals[1];
	int len = SOVEC(value_vec)->length;
	int i;
	obj_t *old_sp;
	obj_t keep_going;

	thread->sp = vals;
	thread_pop_escape(thread);

	keep_going = *--thread->sp;

	thread_buggered(thread);

	old_sp = pop_linkage(thread);

	thread->sp = old_sp + len;

	for (i = 0; i < len; i++)
	    old_sp[i] = SOVEC(value_vec)->contents[i];

	restart_other_threads(thread);

	if (keep_going != obj_False)
	    do_return(thread, old_sp, old_sp);
	else {
	    do_return_setup(thread, old_sp, old_sp);
	    pause(pause_DebuggerCommandFinished);
	}
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

    *thread->sp++ = obj_True;

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
	if (THREAD(thread->thread_obj)->debug_name == yylval
	      || thread->id == id)
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
	if (thread != NULL)
	    set_thread(thread);
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
	set_thread(NULL);
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
    

/* Step/next commands */

static void step_cmd(void)
{
    struct thread *thread = CurThread;

    if (thread == NULL) {
	printf("No current thread.\n");
	return;
    }

    switch (thread->status) {
      case status_Running:
	{
	    enum pause_reason reason;
	    int prev_line = PrevLine;

	    do {
		reason = single_step(thread);
		explain_reason(reason);
	    } while (reason == pause_NoReason && CurThread == thread
		     && !FrameChanged && TopFrame->line == prev_line);
	}
	break;

      case status_Debuggered:
	if (debugger_return_var == NULL
	    || debugger_return_var->value == obj_Unbound) {
	    printf("debugger-return undefined.\n");
	    return;
	}
	else {
	    obj_t cond = thread->datum;

	    *thread->sp++ = obj_False;

	    thread_push_escape(thread);
	    set_c_continuation(thread, maybe_return);
    
	    suspend_other_threads(thread);
    
	    *thread->sp++ = debugger_return_var->value;
	    *thread->sp++ = cond;
	    thread_restart(thread);
	    Continue = TRUE;
	}

	break;

      default:
	printf("The current thread is not runnable.\n");
	return;
    }
}


/* Breakpoint commands */

static int find_pc_for_line(obj_t component, int line)
{
    obj_t debug_info = COMPONENT(component)->debug_info;
    int len = SOVEC(debug_info)->length;
    int n_const = COMPONENT(component)->n_constants;
    int pc = (char *)(&COMPONENT(component)->constant[n_const])
	- (char *)component;
    boolean prev_line_before = FALSE;
    int i;
    
    for (i = 0; i < len; i++) {
	obj_t entry = SOVEC(debug_info)->contents[i];
	int this_line = fixnum_value(SOVEC(entry)->contents[0]);
	if (prev_line_before ? line <= this_line : line == this_line)
	    return pc;
	pc += fixnum_value(SOVEC(entry)->contents[1]);
	prev_line_before = (this_line != 0 && this_line < line);
    }

    return -1;
}

static void install_breakpoint(obj_t func, obj_t thing, int line)
{
    if (instancep(thing, obj_ComponentClass)) {
	if (line == -1)
	    printf("Can't install function-start breakpoints directly into "
		   "components.\n");
	else if (COMPONENT(thing)->debug_info == obj_False) {
	    prin1(func);
	    printf(" has no debug-info.\n");
	}
	else {
	    int pc = find_pc_for_line(thing, line);

	    if (pc < -1) {
		prin1(func);
		printf(" does not span line number %d\n", line);
	    }
	    else {
		int id = install_byte_breakpoint(thing, pc);
		
		if (id < 0)
		    printf("couldn't install breakpoint in ");
		else
		    printf("breakpoint %d installed in ", id);
		prin1(func);
		printf(" at line %d (pc %d)\n", line, pc);
	    }
	}
    }
    else if (!instancep(thing, obj_FunctionClass)) {
	prin1(thing);
	printf(" isn't a function or component\n");
    }
    else if (line == -1) {
	printf("Can't install function start breakpoints."); /* ### */
    }
    else if (!instancep(thing, obj_ByteMethodClass)) {
	prin1(thing);
	printf(" isn't a byte method.\n");
    }
    else
	install_breakpoint(func, byte_method_component(thing), line);
}

static void breakpoint_cont(struct thread *thread, obj_t *vals)
{
    obj_t *old_sp;
    obj_t okay = vals[0];
    int line = fixnum_value(thread->fp[0]);

    if (okay != obj_False) {
	obj_t results = vals[1];
	obj_t thing = obj_False;

	if (SOVEC(results)->length != 0)
	    thing = SOVEC(results)->contents[0];

	install_breakpoint(thing, thing, line);
    }
    old_sp = pop_linkage(thread);
    do_return(thread, old_sp, old_sp);
}

static void breakpoint_cmd(void)
{
    obj_t exprs = parse_exprs();
    
    if (exprs == obj_False)
	printf("Invalid function expression.\n");
    else if (exprs == obj_Nil)
	list_breakpoints();
    else {
	boolean okay = TRUE;
	boolean func_simple = TRUE;
	obj_t line;

	eval_vars(HEAD(exprs), &okay, &func_simple);

	if (!okay)
	    return;

	if (TAIL(exprs) == obj_Nil)
	    line = make_fixnum(-1);
	else if (TAIL(TAIL(exprs)) == obj_Nil) {
	    boolean line_simple = TRUE;

	    eval_vars(HEAD(TAIL(exprs)), &okay, &line_simple);

	    if (!okay)
		return;

	    line = TAIL(HEAD(TAIL(exprs)));
	    if (!line_simple || !obj_is_fixnum(line)) {
		printf("Bogus line number.\n");
		return;
	    }
	}
	else {
	    printf("Too many arguments to breakpoint.\n");
	    return;
	}

	if (func_simple) {
	    obj_t func = TAIL(HEAD(exprs));
	    install_breakpoint(func, func, fixnum_value(line));
	}
	else if (debugger_eval_var == NULL
		 || debugger_eval_var->value == obj_Unbound)
	    printf("Can't eval expressions without debugger-eval "
		   "being defined.\n");
	else {
	    struct thread *thread = CurThread;

	    if (thread == NULL) {
		thread = thread_create(make_string("eval for disassemble"));
		set_c_continuation(thread, kill_me);
	    }
	    else {
		thread_push_escape(thread);
		set_c_continuation(thread, debugger_cmd_finished);
	    }

	    suspend_other_threads(thread);

	    *thread->sp++ = obj_False;
	    push_linkage(thread, thread->sp);
	    set_c_continuation(thread, breakpoint_cont);
	    *thread->sp++ = line;
	    thread->datum = obj_rawptr(thread->sp);
	    *thread->sp++ = debugger_eval_var->value;
	    *thread->sp++ = HEAD(exprs);

	    thread_restart(thread);

	    Continue = TRUE;
	}
    }
}

static void delete_cmd(void)
{
    if (yylex() != tok_LITERAL || !obj_is_fixnum(yylval))
	printf("Bogus breakpoint id\n");
    else
	remove_breakpoint(fixnum_value(yylval));
}


/* Disassemble command */

static struct byteop_info {
    int match;
    int mask;
    char *op;
} ByteOpInfos[] = {
    {op_TRAP, 0xff, "trap"},
    {op_BREAKPOINT, 0xff, "breakpoint"},
    {op_RETURN_SINGLE, 0xff, "return single"},
    {op_MAKE_VALUE_CELL, 0xff, "make-value-cell"},
    {op_VALUE_CELL_REF, 0xff, "value-cell-ref"},
    {op_VALUE_CELL_SET, 0xff, "value-cell-set"},
    {op_MAKE_METHOD, 0xff, "make-method"},
    {op_CHECK_TYPE, 0xff, "check-type"},
    {op_CHECK_TYPE_FUNCTION, 0xff, "check-type-function"},
    {op_CANONICALIZE_VALUE, 0xff, "canonicalize-value %r"},
    {op_PUSH_BYTE, 0xff, "push\t%b"},
    {op_PUSH_INT, 0xff, "push\t%i"},
    {op_CONDITIONAL_BRANCH, 0xff, "cbr\t%t"},
    {op_BRANCH, 0xff, "br\t%t"},
    {op_PUSH_NIL, 0xff, "push\t#()"},
    {op_PUSH_UNBOUND, 0xff, "push\t#unbound"},
    {op_PUSH_TRUE, 0xff, "push\t#t"},
    {op_PUSH_FALSE, 0xff, "push\t#f"},
    {op_DUP, 0xff, "dup"},
    {op_DOT_TAIL, 0xff, "dot\ttail"},
    {op_DOT_FOR_MANY, 0xff, "dot\tfor %r"},
    {op_DOT_FOR_SINGLE, 0xff, "dot\tfor single"},

    {op_PUSH_CONSTANT, 0xf0, "push\tconst(%c)\t"},
    {op_PUSH_ARG, 0xf0, "push\targ(%a)"},
    {op_POP_ARG, 0xf0, "pop\targ(%a)"},
    {op_PUSH_LOCAL, 0xf0, "push\tlocal(%a)"},
    {op_POP_LOCAL, 0xf0, "pop\tlocal(%a)"},
    {op_CALL_TAIL, 0xf0, "call\tnargs = %a, tail"},
    {op_CALL_FOR_MANY, 0xf0, "call\tnargs = %n, for %r"},
    {op_CALL_FOR_SINGLE, 0xf0, "call\tnargs = %n, for single"},
    {op_PUSH_VALUE, 0xf0, "push\tvalue %v"},
    {op_PUSH_FUNCTION, 0xf0, "push\tfunction %v"},
    {op_POP_VALUE, 0xf0, "pop\tvalue %v"},

    {op_PLUS, 0xff, "+"},
    {op_MINUS, 0xff, "-"},
    {op_LT, 0xff, "<"},
    {op_LE, 0xff, "<="},
    {op_EQ, 0xff, "="},
    {op_IDP, 0xff, "=="},
    {op_NE, 0xff, "~="},
    {op_GE, 0xff, ">="},
    {op_GT, 0xff, ">"},
    {0, 0, NULL}
};

static int disassem_int4(unsigned char *ptr)
{
    return ptr[0] | (ptr[1] << 8) | (ptr[2] << 16) | (ptr[3] << 24);
}

static unsigned char *disassemble_op(obj_t component, unsigned char *start)
{
    unsigned char *ptr = start;
    unsigned char byte = *ptr++;
    struct byteop_info *info;
    char buf[256], *fill = buf, *msg = "";
    int i, c;
    obj_t trailer = NULL;
    boolean extra = FALSE;

    for (info = ByteOpInfos; info->op != NULL; info++)
	if ((info->mask & byte) == info->match) {
	    msg = info->op;
	    break;
	}

    do {
	c = *msg++;
	if (c == '%') {
	    switch (*msg++) {
	      case 'a':
		i = byte & 0xf;
		if (i == 0xf) {
		    i = *ptr++;
		    if (i == 0xff) {
			i = disassem_int4(ptr);
			ptr += 4;
		    }
		}
		sprintf(fill, "%d", i);
		break;

	      case 'b':
		sprintf(fill, "%d", *(signed char *)(ptr++));
		break;

	      case 'c':
		i = byte & 0xf;
		if (i == 0xf) {
		    i = *ptr++;
		    if (i == 0xff) {
			i = disassem_int4(ptr);
			ptr += 4;
		    }
		}
		sprintf(fill, "%d", i);
		trailer = COMPONENT(component)->constant[i];
		break;

	      case 'i':
		sprintf(fill, "%d", disassem_int4(ptr));
		ptr += 4;
		break;

	      case 'n':
		i = byte & 0xf;
		if (i == 0xf) {
		    extra = TRUE;
		    i = *ptr++;
		    if (i == 0xff) {
			i = disassem_int4(ptr);
			ptr += 4;
		    }
		}
		sprintf(fill, "%d", i);
		break;

	      case 'r':
		i = *ptr++;
		if (i == 0xff) {
		    i = disassem_int4(ptr);
		    ptr += 4;
		}
		sprintf(fill, "%d", i>>1);
		if (i & 1)
		    strcat(fill, ", #rest");
		break;

	      case 't':
		ptr += 4;
		sprintf(fill, "%d",
			(int) (ptr - (unsigned char *)component
			       + disassem_int4(ptr-4)));
		break;

	      case 'v':
		i = byte & 0xf;
		if (i == 0xf) {
		    i = *ptr++;
		    if (i == 0xff) {
			i = disassem_int4(ptr);
			ptr += 4;
		    }
		}
		trailer = ((struct variable *)COMPONENT(component)
			   ->constant[i])
		    ->name;
		fill[0] = '\0';
		break;

	      default:
		fill[0] = '?';
		fill[1] = '?';
		fill[2] = '?';
		fill[3] = '\0';
	    }
	    fill = strchr(fill, '\0');
	}
	else
	    *fill++ = c;
    } while (c != '\0');

    if (extra)
	ptr++;

    for (i = 0; i < (ptr - start); i++)
	printf(" %02x", start[i]);
    while (i++ < 4)
	printf("   ");
    printf("\t%s", buf);
    if (trailer != NULL)
	prin1(trailer);

    return ptr;
}

static void disassemble_component(obj_t component)
{
    obj_t debug_name = COMPONENT(component)->debug_name;
    obj_t debug_info = COMPONENT(component)->debug_info;
    obj_t source_file = COMPONENT(component)->source_file;
    obj_t mtime = COMPONENT(component)->mtime;
    int nconst = COMPONENT(component)->n_constants;
    unsigned char *ptr
	= (unsigned char *)(COMPONENT(component)->constant + nconst);
    unsigned char *end
	= obj_ptr(unsigned char *, component) + COMPONENT(component)->length;
    int debug_index = 0;
    unsigned char *next_line;
    int i;

    if (debug_name == obj_False)
	printf("anonymous");
    else
	prin1(debug_name);
    printf(" component");
    if (source_file != obj_False) {
	printf(", from ");
	prin1(source_file);
    }

    if (debug_info == obj_False)
	next_line = end;
    else
	next_line = ptr;

    while (ptr < end) {
	while (ptr >= next_line) {
	    obj_t entry = SOVEC(debug_info)->contents[debug_index++];
	    int line = fixnum_value(SOVEC(entry)->contents[0]);
	    FILE *source = find_source_line(source_file, mtime, line);
	    if (source) {
		int c;
		printf("\n%d\t", line);
		while ((c = getc(source)) != EOF && c != '\n')
		    putchar(c);
	    }
	    else
		printf("\nline %d:", line);
	    next_line += fixnum_value(SOVEC(entry)->contents[1]);
	}
	printf("\n%6d:", (int)(ptr - (unsigned char *)component));
	ptr = disassemble_op(component, ptr);
    }
    putchar('\n');

    for (i = 0; i < nconst; i++) {
	obj_t c = COMPONENT(component)->constant[i];
	if (instancep(c, obj_MethodInfoClass)) {
	    putchar('\n');
	    prin1(c);
	    printf(", ");
	    disassemble_component(METHOD_INFO(c)->component);
	}
    }
}

static void disassemble_thing(obj_t thing)
{
    if (instancep(thing, obj_ComponentClass))
	disassemble_component(thing);
    else if (instancep(thing, obj_ByteMethodClass))
	disassemble_component(byte_method_component(thing));
    else if (instancep(thing, obj_FunctionClass)) {
	printf("don't know how to disassemble ");
	print(thing);
    }
    else {
	prin1(thing);
	printf(" isn't a function or component\n");
    }
}

static void disassemble_cont(struct thread *thread, obj_t *vals)
{
    obj_t *old_sp;
    obj_t okay = vals[0];

    if (okay != obj_False) {
	obj_t results = vals[1];
	if (SOVEC(results)->length == 0)
	    disassemble_thing(obj_False);
	else
	    disassemble_thing(SOVEC(results)->contents[0]);
    }
    old_sp = pop_linkage(thread);
    do_return(thread, old_sp, old_sp);
}

static void disassemble_cmd(void)
{
    obj_t exprs = parse_exprs();

    if (exprs == obj_False)
	printf("Invalid function expression.\n");
    else if (exprs == obj_Nil) {
	if (CurFrame == NULL)
	    printf("No current frame.\n");
	else if (obj_is_fixnum(CurFrame->component))
	    printf("Current frame is not in a byte method\n");
	else
	    disassemble_component(CurFrame->component);
    }
    else if (TAIL(exprs) != obj_Nil)
	printf("Too many expressions for disassemble\n");
    else {
	boolean okay = TRUE;
	boolean simple = TRUE;

	eval_vars(HEAD(exprs), &okay, &simple);

	if (!okay)
	    return;

	if (simple)
	    disassemble_thing(TAIL(HEAD(exprs)));
	else if (debugger_eval_var == NULL
		 || debugger_eval_var->value == obj_Unbound)
	    printf("Can't eval expressions without debugger-eval "
		   "being defined.\n");
	else {
	    struct thread *thread = CurThread;

	    if (thread == NULL) {
		thread = thread_create(make_string("eval for disassemble"));
		set_c_continuation(thread, kill_me);
	    }
	    else {
		thread_push_escape(thread);
		set_c_continuation(thread, debugger_cmd_finished);
	    }

	    suspend_other_threads(thread);

	    *thread->sp++ = obj_False;
	    push_linkage(thread, thread->sp);
	    set_c_continuation(thread, disassemble_cont);
	    thread->datum = obj_rawptr(thread->sp);
	    *thread->sp++ = debugger_eval_var->value;
	    *thread->sp++ = HEAD(exprs);

	    thread_restart(thread);

	    Continue = TRUE;
	}
    }
}


/* Command table. */

static struct cmd_entry Cmds[] = {
    {"abort", "abort\t\tInvoke the first available <abort> restart.",
	 abort_cmd},
    {"backtrace",
	 "backtrace\tDisplay a stack backtrace for the current thread.",
	 backtrace_cmd},
    {"breakpoint", "breakpoint\tInstall a breakpoint.", breakpoint_cmd},
    {"c", NULL, continue_cmd},
    {"call", "call expr...\tCall each expr, printing the results.", call_cmd},
    {"continue", "continue\tContinue execution.", continue_cmd},
    {"d", NULL, down_cmd},
    {"delete", "delete id\tDelete the given breakpoint.", delete_cmd},
    {"disable", "disable thread\tSuspend the given thread.", disable_cmd},
    {"disassemble",
	 "disassemble\tDisassemble the component for the current frame",
	 disassemble_cmd},
    {"down", "down\t\tMove down one frame.", down_cmd},
    {"enable", "enable thread\tRestart the given thread.", enable_cmd},
    {"error",
"error\t\tRedisplay the error that caused this thread to enter the debugger.",
	 error_cmd},
    {"flush", "flush\t\tFlush all debugger variables.", flush_cmd},
    {"frame", "frame num\tMove to the given frame.", frame_cmd},
    {"gc", "gc\t\tCollect garbage.", gc_cmd},
    {"help", "help [topic]\tDisplay help about some topic.", help_cmd},
    {"kill", "kill thread\tKill the given thread.", kill_cmd},
    {"l", NULL, locals_cmd},
    {"library",
	 "library [lib]\tSwitch to given library or list all libraries.",
	 library_cmd},
    {"locals", "locals\t\tDisplay all the locals in the current frame.",
	 locals_cmd},
    {"module",
 "module [module]\tSwitch to given module or list modules in current library.",
	 module_cmd},
    {"print", "print expr...\tPrint each expr, ignoring errors.", print_cmd},
    {"restart", "restart [num]\tList or invoke one of the available restarts.",
	 restart_cmd},
    {"return",
	 "return\t\tReturn from this call to invoke-debugger (if allowed)",
	 return_cmd},
    {"step", "step\t\tStep the current thread one byte-op.", step_cmd},
    {"thread", "thread [name]\tSwitch to given thread or list all threads.",
	 thread_cmd},
    {"troff", "troff\t\tTurn function/return tracing off.", troff_cmd},
    {"tron", "tron\t\tTurn function/return tracing on.", tron_cmd},
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
	if (ptr->help)
	    printf("%s\n", ptr->help);
}


/* The main debugger loop */

static void maybe_print_frame(void)
{
    if (ThreadChanged) {
	ThreadChanged = FALSE;

	if (CurThread == NULL) {
	    printf("no current thread\n");
	    FrameChanged = FALSE;
	}
	else {
	    printf("thread ");
	    print_thread(CurThread);
	    FrameChanged = TRUE;
	}
    }

    if (FrameChanged) {
	if (CurFrame == NULL)
	    printf("No stack.\n");
	else
	    print_frame(CurFrame, FALSE);
	FrameChanged = FALSE;
	PrevLine = -1;
    }

    if (CurFrame != NULL
	  && CurFrame->source_file != obj_False
	  && CurFrame->line != PrevLine) {
	printf("%s", string_chars(CurFrame->source_file));
	if (CurFrame->line != 0) {
	    int line = CurFrame->line;
	    FILE *source = find_source_line(CurFrame->source_file,
					    CurFrame->mtime,
					    line);
	    if (source) {
		int c;
		printf("\n%d\t", line);
		while ((c = getc(source)) != EOF && c <= ' ')
		    ;
		while (c != EOF && c != '\n') {
		    putchar(c);
		    c = getc(source);
		}
		putchar('\n');
	    }
	    else
		printf(", line %d.\n", line);
	}
	else
	    putchar('\n');
	PrevLine = CurFrame->line;
    }
}

static void blow_off_cmd(void)
{
    longjmp(BlowOffCmd, TRUE);
}

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

    if (!isatty(fileno(stdin))) {
	printf("STDIN is not a tty.  Cannot debug.\n");
	exit(1);
    }

    lex_init();

    while (1) {
	thread_set_current(NULL);

	while (!Continue) {

	    maybe_print_frame();

	    if (setjmp(BlowOffCmd))
		printf("\ninterrupted\n");
	    else
		set_interrupt_handler(blow_off_cmd);

	    printf("mindy> ");
	    fflush(stdout);

	    if (fgets(line, sizeof(line), stdin) == NULL) {
		putchar('\n');
		lex_setup("quit", 4);
	    }
	    else
		lex_setup(line, strlen(line));

	    clear_interrupt_handler();

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
    scavenge(&do_eval_func);
    scavenge(&cur_source_file);
    scav_frames(TopFrame);
    if (CurThreadObj)
	scavenge(&CurThreadObj);
}


/* Initialization stuff. */

void init_debug_functions(void)
{
    cur_source_file = obj_False;
    do_print_func = make_raw_function("debug-print", 1, FALSE, obj_False,
				      FALSE, obj_Nil, obj_ObjectClass,
				      do_print_start);
    do_eval_func = make_raw_function("debug-eval", 1, FALSE, obj_False,
				     FALSE, obj_Nil, obj_ObjectClass,
				     do_eval_start);
    debugger_eval_var = find_variable(module_BuiltinStuff,
				      symbol("debugger-eval"),
				      FALSE, TRUE);
    debugger_flush_var = find_variable(module_BuiltinStuff,
				      symbol("debugger-flush"),
				      FALSE, TRUE);
    debugger_call_var = find_variable(module_BuiltinStuff,
				      symbol("debugger-call"),
				      FALSE, TRUE);
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
