/* Usage: dbclink outname in1 in2 in3 ....

   On Unix, we use cat.  But under NT, while cat might or might not
   work (depends on whose cat you use), the file redirection > thingy
   will screw you with newline translations.  Thus, this little excuse
   for a program...
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>

const char* delim = "\r\n";

int main(int argc, char *argv[])
{
  char *outname = NULL;
  char *dataname = NULL;
  char *lib_file_names = NULL;
  char *token;
  char *call = NULL; 

  FILE *out = NULL;
  FILE *data = NULL;

  int done = 0;
  int c;

  long data_size = 0;
  long items = 0;

  struct stat st_data;

  // Check arguments
  if (argc < 3)
  {
	  printf("dbclink [library name] [file of dbc filenames]");
	  exit(1);
  }

  outname = *++argv;
  dataname = *++argv;

  // Get size of data list
  if ( stat(dataname, &st_data) )
  {
	  printf("Error reading data file: %d", errno);
	  exit(-1);
  }

  // Open data file and library output file
  out = fopen(outname, "wb");
  data = fopen(dataname, "r");

  // Allocate our buffer
  data_size = st_data.st_size;
  lib_file_names = (char*)malloc(data_size * sizeof(char));
  if (lib_file_names == NULL)
  {
	  printf("Error allocating file memory: %d", errno);
	  exit(-2);
  }

  // Read data list
  items = fread(lib_file_names, sizeof(char), data_size, data);
  if (items == 0)
  {
	  printf("Error reading data file: %d", errno);
	  exit(-3);
  }
  lib_file_names[items] = EOF;

  fclose(data);

  //
  //  For each entry in the data file, read the library data
  //  and append it to the library file.
  //
  printf("Starting process...\r\n");
  call = &lib_file_names[0];
  while (token = strtok(call, delim))
  {
	  FILE *f = NULL;

	  if (token[0] == EOF)
		  break;

	  f = fopen(token, "rb");

	  printf("Processing library %s...\r\n", token);	  
	  while ((c = fgetc(f)) != EOF) 
	  {
		  fputc(c, out);
	  }
	  fclose(f);
	  call = NULL;
  }

  fclose(out);
  free((void*)lib_file_names);
  exit(0);
}
