module:      manipulating-directories
author:      Douglas M. Auclair, dauclair@hotmail.com
copyright:   2000, LGPL

// Functions that work with directories ... creating, removing, and then
// there's the neat fn:  do-directory that is an interator over each
// element in the directory (except . and ..)

// Most of this module does not work on cygnus platforms, ah, well!
// Dylan will throw conditions when a cygnus user attempts to 
// create or to remove directories

define function create-directory(parent :: <pathname>, name :: <string>)
 => (directory :: <pathname>)
  let directory = build-dir(parent, name);
  try-call-out("mkdir", directory, int: #o755);
  directory;
end function create-directory;

define function delete-directory(directory :: <pathname>) => ()
  try-call-out("rmdir", directory);
end function delete-directory;

define function ensure-directories-exist(maybe-dir :: <pathname>)
 => (created? :: <boolean>)
  let honest-dir = as-dir(maybe-dir);
  if(file-exists?(honest-dir))
    #f;
  else
    create-down-to(honest-dir);
  end if;
end function ensure-directories-exist;

// as stated on io_198.htm, fn needs to be the following format:
// fn(dir, name, type :: <file-type>) => ()
// (This'd be a good place for Neel's limited(<function>) type)
define function do-directory(fn :: <function>, dir-name :: <pathname>) => ()
#if(compiled-for-cygnus)
  error("do-directory does not work on cygnus");
#else
  // ooOOoOOoh!  This gets weird quickly! -- melange to the rescue!
  let dir* = open-dir(dir-name);
  if(dir* = null-pointer) file-signal("opendir", dir-name); end if;
  block()
    while(#t)
      let (name, type) = dir-element(dir*, dir-name);
      fn(as-dir(dir-name), name, type);
    end while;
  exception(cond :: <file-system-error>)
    // nada
  end;
  gd-closedir(dir*);
#endif
end function do-directory;

// Working-directory-setter works fine on all platforms, so far
define function working-directory-setter(dir :: <pathname>) 
 => ans :: <pathname>;
  with-pointer(dir* = dir)
    if(chdir(dir*) ~= 0) file-signal("chdir", "dir") end if;
  end with-pointer;
  dir;
end function working-directory-setter;

// -------------------------------------------------------
// internal fns &c
// -------------------------------------------------------

define class <directory-end> (<file-system-error>)
// not really an error, but it's part of the file-system module
end class <directory-end>;

define function end-of-directory()
  signal(make(<directory-end>, format-string: "End of directory"));
end function end-of-directory;

define function dir-element(dir :: <machine-pointer>, path :: <pathname>)
 => (name :: <string>, type :: <file-type>)
  if(dir = null-pointer) end-of-directory(); end if;
  let file = gd-readdir(dir);
  if(file = null-pointer) end-of-directory(); end if;

  let name = convert-to-string(gd-dirent-name(file));
  let stat = with-pointer(path = concatenate(as-dir(path), name))
               gd-stat-mode(path);
             end;
  let type = case
	       gd-is-dir?(stat) => #"directory";
	       gd-is-link?(stat) => #"link";
	       otherwise => #"file";
	     end case;
  values(name, type);
end function dir-element;

define function create-down-to(dir :: <pathname>) => b :: <boolean>;
  let (#rest dirs) = split($path-separator, dir);
  create-dirs(if(relative-path?(dir)) #(".") else #("") end, 
		   as(<list>,dirs));
end function create-down-to;

define method create-dirs(parent :: <list>, rest :: <list>)
 => ans :: <boolean>;
  if(empty?(rest))
    #f;
  else
    let above = build-dir(parent);
    let sleepy = head(rest); // get it?
    let recurse = method() 
		      create-dirs(append(parent, sleepy), tail(rest))
		  end;

    if(file-exists?(build-dir(above, sleepy)))
      recurse()
    else
      create-directory(above, sleepy);
      recurse();
      #t;
    end if;
  end if;
end method create-dirs;
  
define function relative-path?(path :: <pathname>)
 => (relative? :: <boolean>)
  path.size > 0 & path[0] ~= $path-separator[0];
end function relative-path?;

define function build-dir(#rest names) => directory :: <pathname>;
  apply(concatenate, map(as-dir, names));
end function build-dir;

define function open-dir(path :: <string>)
  with-pointer(dir = path)
    gd-opendir(dir);
  end;
end function open-dir;

define method create-pointer(c :: <class>, s :: <string>)
  create-c-type(<char*>, string: s);
end method create-pointer;

define function create-c-type(c-type :: <class>, 
			      #key size :: <integer> = 0, 
			      string)
  let str* = export-value(<c-string>, if(size = 0) string 
				      else make(<string>, size: size)
				      end if);
  make(c-type, pointer: str*);
end function create-c-type;

//-------------------------------------------------------
// mini-melange definitions to avoid big header file includes
//-------------------------------------------------------

define method unlink
    (arg1 :: <char*>)
 => (result :: <integer>);
  let result-value
    = call-out("unlink", int:, ptr: (arg1).raw-value);
  values(result-value);
end method unlink;

define constant $R-OK = 4;
define constant $W-OK = 2;
define constant $X-OK = 1;
define constant $F-OK = 0;

define method access
    (arg1 :: <char*>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("access", int:, ptr: (arg1).raw-value, int: arg2);
  values(result-value);
end method access;

define method chdir
    (arg1 :: <char*>)
 => (result :: <integer>);
  let result-value
    = call-out("chdir", int:, ptr: (arg1).raw-value);
  values(result-value);
end method chdir;

