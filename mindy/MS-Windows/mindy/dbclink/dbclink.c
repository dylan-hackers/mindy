/* Usage: dbclink outname in1 in2 in3 ....

   On Unix, we use cat.  But under NT, while cat might or might not
   work (depends on whose cat you use), the file redirection > thingy
   will screw you with newline translations.  Thus, this little excuse
   for a program...
 */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  char *outname = NULL;
  char *dbc_list = NULL;
  char *lib_file_names = NULL;
  FILE *out = NULL;

  // Check arguments
  if (argc < 3)
  {
	  printf("dbclink [library name] [dbc filenames]");
	  exit(1);
  }

  outname = *++argv;
  out = fopen(outname, "wb");
  
  while (dbc_list = *++argv)
  {
    int c = 0;
    FILE *f = NULL;

    f = fopen(dbc_list, "rb");
    if (f == NULL)
    {
       printf("Failed to open %s\r\n", dbc_list);
       exit(2);
    }

    printf("Processing library %s...\r\n", dbc_list);	  
    while ((c = fgetc(f)) != EOF) 
    {
       fputc(c, out);
    }
    fclose(f);
  }

  fclose(out);
  exit(0);
}
