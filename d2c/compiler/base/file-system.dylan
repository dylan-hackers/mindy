module: File-system
author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/file-system.dylan,v 1.1 1996/06/26 14:53:07 nkramer Exp $
copyright: Copyright (c) 1995, 1996  Carnegie Mellon University
	   All rights reserved.

// This code only works under Unix, and under systems that have Unix
// emulation commands (like Lisp-PC1)

define method delete-file (filename :: <string>) => ();
  let rm-command = concatenate("rm -f ", filename);
  if (system(rm-command) ~== 0)
    cerror("so what", "rm failed?");
  end if;
end method delete-file;

define method rename-file (old-filename :: <string>, new-filename :: <string>)
 => ();
  let mv-command = concatenate("mv -f ", old-filename, " ", new-filename);
  if (system(mv-command) ~== 0)
    cerror("so what", "mv failed?");
  end if;
end method rename-file;

// Returns false if one of the files isn't there
//
define method files-identical? (file1 :: <string>, file2 :: <string>)
 => answer :: <boolean>;
  let cmp-command = concatenate("cmp -s ", file1, " ", file2);
  // cmp will return non-zero if they are different, if a file's not
  // found, or if cmp somehow fails to execute.
  system(cmp-command) == 0;
end method files-identical?;
