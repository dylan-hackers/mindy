/* rcsupdate: utility to update a tree of RCS files.

$Header: /scm/cvs/src/tools/unix-misc/rcsupdate.c,v 1.1 1998/05/03 19:55:58 andreas Exp $

*/
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/wait.h>

static int quiet = 1;
static int force = 0;
static int just_tell = 0;
static char *rcs_init = NULL;

extern int errno;

#define CACHENAME ".rcsupdate-cache"

#define MAXRCSFILES 1000

static char lastcache[MAXPATHLEN] = "";
static struct cache_entry {
    char *file;
    time_t rcstime;
} cache[MAXRCSFILES], *last_entry = cache;
static int dirty = 0; 

void write_cache_if_dirty()
{
    FILE *file;
    char bak[MAXPATHLEN];
    struct cache_entry *entry;

    if (dirty && !just_tell) {
        strcpy(bak, lastcache);
        strcat(bak, ".BAK");
        rename(lastcache, bak);
        file = fopen(lastcache, "w");
        for (entry = cache; entry != last_entry; entry++) {
            fprintf(file, "%s %ld\n", entry->file, entry->rcstime);
            free(entry->file);
        }
        fclose(file);
    }
    dirty = 0;
}

void read_cache(char *dir)
{
    char cachefile[MAXPATHLEN];
    FILE *file;
    char line[MAXPATHLEN+80], name[MAXPATHLEN];
    time_t rcstime;
    struct cache_entry *entry;

    strcpy(cachefile, dir);
    strcat(cachefile, "/"CACHENAME);

    if (strcmp(cachefile, lastcache) == 0)
	return;

    write_cache_if_dirty();

    for (entry = cache; entry != last_entry; entry++)
	free(entry->file);

    entry = cache;
    if (!force && (file = fopen(cachefile, "r")) != NULL) {
        while (fgets(line, sizeof(line), file) != NULL) {
            sscanf(line, "%s %ld", name, &rcstime);
            entry->file = (char *)malloc(strlen(name)+1);
            strcpy(entry->file, name);
            entry->rcstime = rcstime;
            entry++;
        }
        fclose(file);
    }
    strcpy(lastcache, cachefile);
    last_entry = entry;
    dirty = 0;
}

struct cache_entry *find_entry(char *rcsfile)
{
    char *name;
    struct cache_entry *entry;

    name = rindex(rcsfile, '/');
    if (name == NULL)
        name = rcsfile;
    else
        name++;

    for (entry = cache; entry != last_entry; entry++)
        if (strcmp(name, entry->file) == 0)
            return entry;
    return NULL;
}

void make_entry(char *rcsfile, time_t rcstime)
{
    char *name;
    struct cache_entry *entry;

    name = rindex(rcsfile, '/');
    if (name == NULL)
        name = rcsfile;
    else
        name++;

    for (entry = cache; entry != last_entry; entry++)
	if (strcmp(name, entry->file) == 0)
	    goto found;

    last_entry++;
    entry->file = (char *)malloc(strlen(name)+1);
    strcpy(entry->file, name);

  found:
    entry->rcstime = rcstime;
    dirty = 1;
}

int co(char *localfile, char *rcsfile)
{
    char buf[MAXPATHLEN*2+40];
    int res;

    sprintf(buf, "co %s %s %s %s",
	    rcs_init, quiet ? "-q " : "", localfile, rcsfile);
    res = system(buf);

    if (WIFEXITED(res) && WEXITSTATUS(res) == 0)
	return 1;
    else if (WIFEXITED(res) && WEXITSTATUS(res) == 130) {
	fprintf(stderr, "interrupted.\n");
	exit(1);
    }
    else
	return 0;
}

int co_if_different(char *localfile, char *rcsfile)
{
    char buf[MAXPATHLEN*2+40];
    int res;

    sprintf(buf, "rcsdiff %s %s %s %s > /dev/null",
	    rcs_init, quiet ? "-q " : "", localfile, rcsfile);
    res = system(buf);

    if (WIFEXITED(res)) {
	if (WEXITSTATUS(res) == 0) {
	    if (!quiet)
		printf("identical\n");
	    return 1;
	}
	else if (WEXITSTATUS(res) == 130) {
	    fprintf(stderr, "interrupted.\n");
	    exit(1);
	}
	else {
	    if (quiet)
		printf(just_tell
		       ? "%s is different -- would check it out\n"
		       : "%s is different -- checking out\n",
		       localfile);
	    else
		printf("different\n");
	    if (just_tell || co(localfile, rcsfile))
		return 1;
	    else {
		printf("checkout failed.\n");
		return 0;
	    }
	}
    }
    else {
	printf("diff failed\n");
	return 0;
    }
}

void update(localfile, rcsfile)
char *localfile, *rcsfile;
{
    struct stat statbuf;
    long rcstime;
    struct cache_entry *entry;
    
    if (stat(rcsfile, &statbuf) < 0) {
	if (quiet)
	    perror(rcsfile);
	else
	    perror("problem w/ RCS file");
	return;
    }
    rcstime = statbuf.st_mtime;

    if (stat(localfile, &statbuf) < 0) {
        switch (errno) {
          case ENOENT:
	    if (quiet)
		if (just_tell)
		    printf("would check out %s\n", localfile);
		else
		    printf("checking out %s\n", localfile);
	    else
		printf("doesn't exist.\n");
	    if (just_tell || co(localfile, rcsfile))
		make_entry(rcsfile, rcstime);
	    else
		printf("co failed.\n");
            break;

          default:
	    if (quiet)
		perror(localfile);
	    else
		perror("oops");
	    break;
        }
    }
    else if ((statbuf.st_mode & (S_IWUSR|S_IWGRP|S_IWOTH)) != 0) {
	if (quiet)
	    printf("%s is ", localfile);
	printf("writable, hence ignoring.\n");
    }
    else {
	entry = find_entry(rcsfile);
	if (entry == NULL || entry->rcstime != rcstime) {
	    if (!quiet) {
		if (force)
		    printf("forcing diff\n");
		else
		    printf("rcs file has changed\n");
	    }

	    if (co_if_different(localfile, rcsfile)) {
		if (entry == NULL)
		    make_entry(rcsfile, rcstime);
		else {
		    entry->rcstime = rcstime;
		    dirty = 1;
		}
	    }
	}
	else if (!quiet)
	    printf("rcs file is the same.\n");
    }
}

void update_rcs_files(dirname)
     char *dirname;
{
    char localfile[MAXPATHLEN], rcsfile[MAXPATHLEN];
    char *localptr, *rcsptr;
    DIR *rcsdir;
    struct direct *entry;

    read_cache(dirname);

    strcpy(rcsfile, dirname);
    strcat(rcsfile, "/RCS");

    rcsdir = opendir(rcsfile);
    if (rcsdir == NULL)
        return;

    rcsptr = rcsfile + strlen(rcsfile);
    *rcsptr++ = '/';

    strcpy(localfile, dirname);
    localptr = localfile + strlen(localfile);
    *localptr++ = '/';

    while ((entry = readdir(rcsdir)) != NULL) {
        if (strcmp(entry->d_name + entry->d_namlen - 2, ",v") == 0) {
            strcpy(rcsptr, entry->d_name);
            strcpy(localptr, entry->d_name);
            localptr[entry->d_namlen - 2] = '\0';
	    if (!quiet)
		printf("%s:\t", localfile);
            update(localfile, rcsfile);
        }
    }

    closedir(rcsdir);
}

void update_directory(dirname)
     char *dirname;
{
    DIR *dir;
    struct stat buf;
    struct direct *entry;
    char subdir[MAXPATHLEN], *subptr;

    printf(just_tell ? "Checking %s:\n" : "Updating %s:\n", dirname);

    update_rcs_files(dirname);

    dir = opendir(dirname);
    if (dir == NULL) {
        fprintf(stderr, "couldn't open ``%s''?\n", dirname);
        return;
    }

    strcpy(subdir, dirname);
    subptr = subdir + strlen(dirname);
    *subptr++ = '/';

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, "RCS") != 0
            && strcmp(entry->d_name, "..") != 0
            && strcmp(entry->d_name, ".") != 0) {
            strcpy(subptr, entry->d_name);
            if (stat(subdir, &buf) != -1 && (buf.st_mode & S_IFMT) == S_IFDIR)
                update_directory(subdir);
        }
    }

    closedir(dir);
}

main(argc, argv)
int argc;
char *argv[];
{
    char dirname[MAXPATHLEN], localfile[MAXPATHLEN], rcsfile[MAXPATHLEN], *ptr;
    int len;
    struct stat buf;
    int did_anything = 0;

    rcs_init = getenv("RCSINIT");
    if (rcs_init == NULL)
	rcs_init = "";
    else
	putenv("RCSINIT=");

    while (*++argv != NULL) {
	if (strcmp(*argv, "-q") == 0)
	    quiet = 1;
	else if (strcmp(*argv, "-v") == 0)
	    quiet = 0;
	else if (strcmp(*argv, "-f") == 0)
	    force = 1;
	else if (strcmp(*argv, "-n") == 0)
	    just_tell = 1;
	else if (stat(*argv, &buf)!=-1 && (buf.st_mode&S_IFMT)==S_IFDIR) {
	    update_directory(*argv);
	    did_anything = 1;
	}
	else {
	    len = strlen(*argv);

	    /* Determine the two names. */
	    strcpy(rcsfile, *argv);
	    strcpy(localfile, *argv);
	    if (strcmp(",v", localfile + len-2) == 0) {
		localfile[len-2] = '\0';
		ptr = rindex(localfile, '/');
		if (ptr == NULL)
		    ptr = localfile;
		else
		    ptr -= 3;
		if (strncmp(ptr, "RCS/", 4) == 0)
		    strcpy(ptr, ptr+4);
	    } else {
		ptr = rindex(rcsfile, '/');
		if (ptr == NULL)
		    ptr = rcsfile;
		else
		    ptr++;
		strcpy(ptr, "RCS/");
		strcat(ptr, localfile + (ptr - rcsfile));
		strcat(ptr, ",v");
	    }
                
	    /* Read the cache */
	    strcpy(dirname, localfile);
	    ptr = rindex(dirname, '/');
	    if (ptr == NULL)
		strcpy(dirname, ".");
	    else
		*ptr = '\0';
	    read_cache(dirname);

	    /* Display the file we are working on: */
	    if (!quiet) {
		ptr = rindex(localfile, '/');
		if (ptr == NULL)
		    ptr = localfile;
		printf("%s:\t", ptr);
		fflush(stdout);
	    }
                
	    /* Update it. */
	    update(localfile, rcsfile);
	    did_anything = 1;
	}
    }
    if (!did_anything)
	update_directory(".");

    write_cache_if_dirty();

    exit(0);
}
