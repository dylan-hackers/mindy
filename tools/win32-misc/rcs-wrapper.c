/* rcs-wrapper.c */

/* Author: Nick Kramer
   RCS-header: $Header: /home/housel/work/rcs/gd/src/tools/win32-misc/rcs-wrapper.c,v 1.1 1997/03/04 16:54:36 nkramer Exp $

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

   The memory management in this program is terrible.  We can get away
   with it here because this is a small program, but be careful...  
   */

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

static int use_dash_v4 = 0;

static char *convert_one_arg (char *arg)
{
    if (strlen(arg) < 1 || arg[0] == '-') {
	return arg;
    } else {
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
}

static int verbose = 0;

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

    if (argv > 0 && stricmp(argv[1], "-verbose-wrapper") == 0) {
	verbose = 1;
    }
    
    /* Create the final argv by adding or subtracting any args necessary. */
    src_index = (verbose) ? 2 : 1;
    dest_index = (use_dash_v4) ? 2 : 1;
    if (use_dash_v4) 
	third_argv[1] = "-V4";
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
	for (i=0; i<dest_index; i++) {
	    fprintf(stderr, "[%s] ", third_argv[i]);
	}
	fprintf(stderr, "\n");
    }
    return spawnv(P_WAIT, executable, third_argv);
}
