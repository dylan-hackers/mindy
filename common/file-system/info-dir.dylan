module: directory-information
author: Douglas M. Auclair, dauclair@hotmail.com

// This module finds out information on the file-system
// It uses Section 8.5 from "System and I/O" of the
// Common-Dylan spec available at www.functionalobjects.com

// Most of this module doesn't work for cygnus, as well!

define function home-directory() => (ans :: <pathname>)
  let str* = #f;

  with-pointer(home* = "HOME")
    str* := getenv(home*);
  end with-pointer;

  if(as(<integer>,str*) = 0) file-signal("getenv", "HOME"); end if;
  convert-to-string(str*);
end function home-directory;

define function root-directories() => (roots :: <sequence>)
#if(compiled-for-cygnus)
  error("root-directories does not work on cygnus platforms!");
#else
  let roots = #();

  local method add-if-dir(root, name, type)
		if(type == #"directory") 
		  roots := append(roots, as-dir(concatenate(root, name)));
		end if;
	end;

  do-directory(add-if-dir, $path-separator);
  roots;
#endif
end function root-directories;

// I ask the os to provide the temp directory by creating a temp file
// name and removing the name
define function temp-directory() => tmp :: <pathname>;
  let file-name* = tmpnam();
  if(as(<integer>, file-name*) = 0) 
    file-signal("tmpnam", "no arguments");
  end if;

  filename-prefix(as(<byte-string>,file-name*));
end function temp-directory;

define function working-directory() => pwd :: <pathname>;
  get-current-directory();
end function working-directory;

// -------------------------------------------------------
// hand-generated code (stolen from melange to avoid conflicts with stdio.h)
// -------------------------------------------------------
define method getenv
    (arg1 :: <char*>)
 => (result :: <char*>);
  let result-value
    = call-out("getenv", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<char*>, pointer: result-value);
  values(result-value);
end method getenv;

define method tmpnam() => (result :: <c-string>);
  let tempy = as(<c-string>, make(<string>, size: 256));
  call-out("tmpnam", ptr:, ptr: tempy.raw-value);
  tempy;
end method tmpnam;

