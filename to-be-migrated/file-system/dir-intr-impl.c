#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "dir-intr-impl.h"

void *gd_opendir(const char *filename)
{
  return opendir(filename);
}

void *gd_readdir(void *dir)
{
  return readdir((DIR *)dir);
}

char *gd_dirent_name(void *dirent)
{
  return ((struct dirent *)dirent)->d_name;
}

int gd_closedir(void *dir)
{
  return closedir((DIR *)dir);
}

int gd_is_dir(int mode)
{
  return (mode & S_IFMT) == S_IFDIR;
}

int gd_is_link(int mode)
{
  return (mode & S_IFMT) == S_IFLNK;
}

int gd_is_regular_file(int mode)
{
  return (mode & S_IFMT) == S_IFREG;
}

int gd_stat_mode(const char *file)
{
  struct stat st;
  if(lstat(file, &st) != 0)
    return 0;
  return st.st_mode;
}
