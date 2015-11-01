#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    char *outname = *++argv;
    FILE *out = fopen(outname, "wb");
    while (*++argv != NULL) {
        FILE *f = fopen(*argv, "rb");
        int c;
        while ((c = fgetc(f)) != EOF) {
            fputc(c, out);
        }
        fclose(f);
    }
    fclose(out);
    exit(0);
}
