/* rcs-wrapper.c */

/* Author: Nick Kramer
   RCS-header: $Header: /scm/cvs/src/tools/win32-misc/rcs-wrapper.c,v 1.1 1998/05/03 19:55:55 andreas Exp $

   This is a replacement for rcs-wrapper.perl.  I've had to rewrite it
   in C in order to avoid having to play quoting games with the shell
   whenever I spawn a new process.  (I'm not sure whether it's Perl or
   the batch file wrapper which is at fault)

   This program tries to compensate for the lack of symbolic
   links in Windows/NT.  It looks through the arguments you're passing
   to the RCS utility being wrapped, and any argument that doesn't
   begin with a dash will have fake symlinks expanded into a real file
   name.  Fake symlinks are named "RCS", and the contents of them is
   the name of the directory they point at.  After expanding all fake
   symlinks, the wrapped rcs-command is invoked.

   This program knows which rcs-command it's supposed to wrap by
   examining it's own filename.  It then invokes the appropriate
   command in $GWYDIONDIR/rcs-bin.

   We play various games so that our wrapped RCS commands can be used
   both natively and by CVS.  What we do is, if a fake symlink is
   used, we add the "-V4" flag to the command line.  Otherwise, we
   assume the command is being called by CVS, and we don't add -V4.

   If you're trying to debug this program, try the -verbose-wrapper
   flag.  This magical flag is interpreted by the wrapper instead of
   the RCS command.  (It must be the first argument)

   The memory management in this program is terrible.  We can get away
   with it here because this is a small program, but be careful...  */

#include <stdio.h>
#include <windows.h>
#include <process.h>

typedef struct _cache_entry {
    struct _cache_entry *next;
    char *from_dir;
    char *to_dir;
} cache_entry;

static cache_entry *cache = NULL;

static void add_cache_entry (char *from_dir, char *to_dir)
{
    cache_entry *entry = malloc(sizeof(cache_entry));
    entry->next = cache;
    entry->from_dir = from_dir;
    entry->to_dir = to_dir;
    cache = entry;
}

static cache_entry *cache_lookup (char *from_dir)
{
    cache_entry *ptr;
    for (ptr = cache; ptr != NULL; ptr = ptr->next) {
	if (strcmp(ptr->from_dir, from_dir) == 0) {
	    return ptr;
	}
    }
    return NULL;
}


/* path will include a trailing / if it isn't empty.
 */
static void extract_path (char *arg, char **path, char **file)
{
    int i;
    *path = malloc(strlen(arg) + 1); /* more than big enough... */
    *file = malloc(strlen(arg) + 1);
    for (i = strlen(arg); i >= 0; i--) {
	if (arg[i] == '/' || arg[i] == '\\') {
	    memcpy(*path, arg, i + 1);
	    (*path)[i + 2] = 0;
	    memcpy(*file, arg + i + 1, strlen(arg) - i);
	    (*file)[strlen(arg) - i + 1] = 0;
	    return;
	}
    }
    strcpy(*path, "");
    strcpy(*file, arg);
    return;
}

static char *make_filename (char *path, char *file)
{
    char *res = malloc(strlen(path) + strlen(file) + 3);
    strcpy(res, path);
    strcat(res, file);
    strcat(res, ",v");
    return res;
}

static int already_a_dash_v4 = 0;

/* NT insists on reparsing the command line, so we're going to have to
   quote some things.  The main case is args with spaces in them.  We
   have code to handle embedded quotation marks, but apparently CVS
   strips the quotes out before handing the command line to us, so
   there's not much we can do about it. */
static char *maybe_quote (char *arg)
{
    char *ptr;
    if (strcmp(arg, "-V4") == 0) {
	already_a_dash_v4 = 1;
    }
    /* if arg has spaces, need to quote it */
    for (ptr=arg; *ptr != 0; ptr++) {
	if (*ptr == ' ') {
	    char *res = malloc(2*strlen(arg) + 3);
	    char *ptr2;
	    char short_string[2];
	    short_string[1] = 0;
	    strcpy(res, "\"");
	    for (ptr2=arg; *ptr2 != 0; ptr2++) { /* handle embedded quotes */
		if (*ptr2 == '"') {
		    strcat(res, "\\\"");  /* add \" to string */
		} else {
		    short_string[0] = *ptr2;
		    strcat(res, short_string);
		}
	    }
	    strcat(res, "\"");
	    return res;
	}
    }
    return arg;
}

static int use_dash_v4 = 0;

static char *do_fake_symlinks (char *arg)
{
    char *path, *file;
    cache_entry *entry;
    extract_path(arg, &path, &file);
    entry = cache_lookup(path);
    if (entry) {
	use_dash_v4 = 1;
	return make_filename(entry->to_dir, file);
    } else {
	char rcsdir_filename[1000];     /* foo/RCS */
	char *rcs_dir = malloc(1000);   /* ...../rcs/foo */
	FILE *rcs;
	strcpy(rcsdir_filename, path);
	strcat(rcsdir_filename, "RCS");
	rcs = fopen(rcsdir_filename, "r");
	if (!rcs) {
	    return arg; /* RCS dir not found */
	} else {
	    fscanf(rcs, "%s", rcs_dir);
	    fclose(rcs);
	    rcs_dir = strcat(rcs_dir, "/");
	    add_cache_entry(path, rcs_dir);
	    use_dash_v4 = 1;
	    return make_filename(rcs_dir, file);
	}
    }
}

static char *convert_one_arg (char *arg)
{
    if (strlen(arg) < 1 || arg[0] == '-') {
	return maybe_quote(arg);
    } else {
	return do_fake_symlinks(arg);
    }
}

static int verbose = 0;

/* Last element of argv[] is NULL
 */
static int run_program (char *cmd_name, char **argv)
{
    int res;
    /* For some reason, spawnv works but system() doesn't. */
    res = spawnv(P_WAIT, cmd_name, argv);
    if (verbose)
	fprintf(stderr, "[Return value: %d]\n", res);
    return res;
}

int main (int argc, char **argv)
{
    int i, src_index, dest_index;
    char executable[1000], last_four[5];
    char **new_argv = malloc(sizeof(char*) * (argc));
    char **third_argv = malloc(sizeof(char*) * (argc+10));
    char *gwydion_dir;

    /* Don't bother with argv[0], because that's the command name */
    for (i=1; i<argc; i++) {
	new_argv[i] = convert_one_arg(argv[i]);
    }

    /* Create the final argv by adding or subtracting any args necessary. */
    src_index = dest_index = 1;
    if (argv > 0 && stricmp(argv[1], "-verbose-wrapper") == 0) {
	verbose = 1;
	src_index++;
    }
    if (use_dash_v4 && !already_a_dash_v4) {
	third_argv[1] = "-V4";
	dest_index++;
    }
    for (; src_index < argc; src_index++, dest_index++) {
	third_argv[dest_index] = new_argv[src_index];
    }
    third_argv[dest_index] = NULL;

    gwydion_dir = getenv("GWYDIONDIR");
    if (!gwydion_dir) {
	fprintf(stderr, "GWYDIONDIR not set!\n");
	exit(1);
    }
    strcpy(executable, gwydion_dir);
    strcat(executable, "/rcs-bin/");
    strcat(executable, argv[0]);

    /* Add .exe if it isn't already there */
    strcpy(last_four, executable + strlen(executable) - 4);
    if (stricmp(last_four, ".exe") != 0) {
	strcat(executable, ".exe");
    }

    third_argv[0] = executable;
    /* ### I hope the return value of spawnv is the return value of
       the child process... */
    if (verbose) {
	for (i=0; third_argv[i] != NULL; i++) {
	    fprintf(stderr, "[%s] ", third_argv[i]);
	}
	fprintf(stderr, "\n");
    }
    return run_program(executable, third_argv);
}
