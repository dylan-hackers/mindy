module: c-lexer

//=========================================================================
//  Include paths
//=========================================================================
//  Represent the method used to find headers. Each <c-include-path>
//  object knows how to find two kinds of headers:
//
//    #include <system-headers.h>
//    #include "user-headers.h"
//
//  We provide a <gcc-include-path> object which implements GCC's bizarre
//  and inexplicable behavior. Users of other compilers will want to
//  subclass <c-include-path> and implement their own searching algorithms.
//
//  XXX - Need to use locators!

define abstract open class <c-include-path> (<object>)
end class <c-include-path>;

define open generic find-in-include-path
    (name :: <string>,
     include-path :: <c-include-path>,
     #key system-header? :: <boolean>,
     current-header :: false-or(<string>))
 => (full-name :: false-or(<string>));

//=========================================================================
//  GCC include paths
//=========================================================================
//  GCC uses a fairly inexplicable set of rules when searching for headers.

define class <gcc-include-path> (<c-include-path>)
  // Equivalent to '-nostdinc'.
  keyword no-standard-includes?:, init-value: #f;

  // Equivalent to '-I-'.
  keyword no-relative-includes?:, init-value: #f;

  // Result of 'gcc --print-file-name=include'. A value of "include" is
  // synonymous with a value of #f. This is an extremely private GCC
  // directory that we'll access anyway.
  keyword hidden-include-directory:, init-value: #f;

  // The "default" search path for system headers.
  required keyword standard-include-directories:;

  // Directories specified by normal -I flags, in order.
  keyword extra-include-directories:, init-value: #();

  // Directories specified by '-I' flags before a '-I-' flag, in order.
  keyword extra-user-include-directories:, init-value: #();

  // Actual slots.
  slot system-include-path :: <list>;
  slot user-include-path :: <list>;
  slot relative-includes? :: <boolean>;
end class <gcc-include-path>;

define method initialize
    (include-path :: <gcc-include-path>,
     #next next-method,
     #rest keys,
     #key no-standard-includes? :: <boolean>,
     no-relative-includes? :: <boolean>,
     hidden-include-directory :: false-or(<string>),
     standard-include-directories :: <sequence>,
     extra-include-directories :: <sequence>,
     extra-user-include-directories :: <sequence>,
     #all-keys)

  // Convert our hidden-include-directory argument to a list for later
  // concatenation. If the directory name is "include", then we're dealing
  // with a version of GCC that doesn't have a hidden directory.
  let hidden-dir =
    if (hidden-include-directory & hidden-include-directory ~= "include")
      list(hidden-include-directory);
    else
      #();
    end if;

  // Build our list of system directories.
  let system-dirs = 
    if (no-standard-includes?)
      extra-include-directories;
    else
      concatenate(extra-include-directories,
		  standard-include-directories,
		  hidden-dir);
    end if;

  // Build our list of user directories.
  let user-dirs = concatenate(extra-user-include-directories, system-dirs);

  // Store everything.
  include-path.system-include-path :=
    map-as(<list>, identity, system-dirs);
  include-path.user-include-path :=
    map-as(<list>, identity, user-dirs);
  include-path.relative-includes? := ~no-relative-includes?;
end method initialize;

define method find-in-include-path
    (name :: <byte-string>,
     include-path :: <c-include-path>,
     #key system-header? :: <boolean>,
     current-header :: false-or(<string>))
 => (full-name :: false-or(<string>))

  // XXX - We don't have a real file-exists? function, so we fake one
  // locally by attempting to open the file.
  local method file-exists? (file-name :: <string>)
	  block ()
	    let stream = make(<file-stream>,
			      locator: file-name,
			      direction: #"input");
	    close(stream);
	    #t;
	  exception (<file-does-not-exist-error>)
	    #f;
	  end block;
	end method file-exists?;

  if (name[0] == '/')
    // An absolute file name--this is easy.
    file-exists?(name) & name;
  else
    // Assemble our search path.
    let dirs :: <list> =
      if (system-header?)
	include-path.system-include-path;
      else
	include-path.user-include-path;
      end if;
    if (~system-header? & include-path.relative-includes?)
      dirs :=
	pair(if (current-header)
	       // Get directory of current-header.
	       // XXX - should use locators
	       regexp-replace(current-header, "[^/]+$", "");
	     else
	       ".";
	     end if,
	     dirs);
    end if;
    
    // Look for our header in each directory.
    block (return)
      for (dir in dirs)
	let full-name = concatenate(dir, "/", name);
	if (file-exists?(full-name))
	  return(full-name);
	end if;
      end for;
      #f;
    end block;
  end if;
end method find-in-include-path;
