#include <sys/types.h>
#include <sys/stat.h>

#define POSIX_FILE_EXISTS 1
#define POSIX_DIRECTORY_EXISTS 2

int file_exists_p(const char *path, int flags)
{
    int result = 1;                          /* assume failure */
    struct stat stat_buf;

    if (stat(path, &stat_buf) == 0) {
        switch (flags) {
        case POSIX_FILE_EXISTS:
            if (stat_buf.st_mode & S_IFREG)
                result = 0;
            break;
        case POSIX_DIRECTORY_EXISTS:
            if (stat_buf.st_mode & S_IFDIR)
                result = 0;
            break;
        default:
            result = 0;
            break;
        }
    }

    return result;
}
