void *gd_opendir(const char *filename);
void *gd_readdir(void *dir);
char *gd_dirent_name(void *dirent);
int gd_closedir(void *dir);

int gd_is_dir(int mode);
int gd_is_link(int mode);
int gd_is_regular_file(int mode);
int gd_stat_mode(const char *file);
