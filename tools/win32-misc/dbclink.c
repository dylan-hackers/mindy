/* Usage: dbclink outname in1 in2 in3 ....

   On Unix, we use cat.  But under NT, while cat might or might not
   work (depends on whose cat you use), the file redirection > thingy
   will screw you with newline translations.  Thus, this little excuse
   for a program...
 */

#include <stdio.h>

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
