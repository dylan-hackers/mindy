module:      manipulating-directories
author:      Douglas M. Auclair, dauclair@hotmail.com
copyright:   2000, LGPL

// Functions that work with directories ... creating, removing, and then
// there's the neat fn:  do-directory that is an interator over each
// element in the directory (except . and ..)

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
  // ooOOoOOoh!  This gets weird quickly! -- melange to the rescue!
  let dir* = open-dir(dir-name);
  if(as(<integer>, dir*) = 0) file-signal(opendir, dir-name); end if;
  block()
    while(#t)
      let (name, type) = dir-element(dir*, dir-name);
      fn(as-dir(dir-name), name, type);
    end while;
  exception(cond :: <file-system-error>)
    // nada
  end;
  closedir(dir*);
end function do-directory;

// -------------------------------------------------------
// internal fns &c
// -------------------------------------------------------

define class <directory-end> (<file-system-error>)
// not really an error, but it's part of the file-system module
end class <directory-end>;

define function end-of-directory()
  signal(make(<directory-end>, format-string: "End of directory"));
end function end-of-directory;

define function dir-element(dir :: <DIR>, path :: <pathname>)
 => (name :: <string>, type :: <file-type>)
  if(as(<integer>, dir) = 0) end-of-directory(); end if;
  let file* = readdir(dir);
  if(as(<integer>, file*) = 0) end-of-directory(); end if;
  let file = make(<virtual-dirent>, c-type: file*);

  local method to-string(c :: <c-vector>) => s :: <string>;
	  let char :: <character> = '*';
	  let str = make(<deque>);
	  for(index from 0, until: char = '\0')
	    char := as(<character>, c[index]);
	    push-last(str, char);
	  end for;
	  pop-last(str);
	  as(<string>, str);
	end;

  let name = d-name(file).to-string;
  let stat = stat-mode(concatenate(as-dir(path), name));
  let type = case
	       is-dir?(stat) => #"directory";
	       is-link?(stat) => #"link";
	       otherwise => #"file";
	     end case;
  values(name, type);
end function dir-element;

define method is-dir?(file :: <pathname>) => (ans :: <boolean>)
  is-dir?(stat-mode(file));
end method is-dir?;

define method is-link?(file :: <pathname>) => (ans :: <boolean>)
  is-link?(stat-mode(file));
end method is-link?;

define method is-regular-file?(file :: <pathname>) => (ans :: <boolean>)
  is-regular-file?(stat-mode(file));
end method is-regular-file?;

define method is-dir?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, #xf000) = #x4000;
end method is-dir?;

define method is-link?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, #xf000) = #xa000;
end method is-link?;

define method is-regular-file?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, #xf000) = #x8000;
end method is-regular-file?;

define function value&destroy(fn :: <function>,
			      ptr* :: <statically-typed-pointer>)
  let ans = fn(ptr*);
  destroy(ptr*);
  ans;
end function value&destroy;

define function stat-mode(file :: <pathname>) => (bar :: <integer>)
  let stat* = create-stat();
  let str* = create-anon(file);
  lstat(str*, stat*);
  destroy(str*);
  value&destroy(stat$st-mode, stat*);
end function stat-mode;

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

define function as-dir(path :: <pathname>) => directory :: <pathname>;
  concatenate(path, $path-separator);
end function as-dir;

define function build-dir(#rest names) => directory :: <pathname>;
  apply(concatenate, map(as-dir, names));
end function build-dir;

define function append(a :: <list>, b :: <string>) => c :: <list>;
  reverse(pair(b, reverse(a)));
end function append;

define function open-dir(dir :: <string>)
  value&destroy(opendir, create-anon(dir));
end function open-dir;

define function create-anon(s :: <string>) => (ans :: <anonymous-9>)
  create-c-type(<anonymous-9>, string: s);
end function create-anon;

define function create-stat() => (ans :: <stat>)
  create-c-type(<stat>, size: content-size(<stat>));
end function create-stat;

define function create-c-type(c-type :: <class>, 
			      #key size :: <integer> = 0, 
			      string)
  let str* = export-value(<c-string>, if(size = 0) string 
				      else make(<string>, size: size)
				      end if);
  make(c-type, pointer: str*);
end function create-c-type;

