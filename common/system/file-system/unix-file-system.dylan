Module:	      system-internals
Author:       Gary Palter
Synopsis:     UNIX implementation of the File System library API
Copyright:    Original Code is Copyright (c) 1998-2001 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Handles expansion of "~" and "~USER" in a pathname
define method %expand-pathname 
    (path :: <posix-directory-locator>)
 => (expanded-path :: <locator>)
  block (return)
    if (locator-relative?(path))
      let elements = locator-path(path);
      if (size(elements) > 0)
	let first = elements[0];
	if (instance?(first, <string>)
	      & size(first) > 0
	      & first[0] = '~')
	  let name = if (first = "~")
		       login-name()
		     else
		       copy-sequence(first, start: 1)
		     end;
	  let passwd = %getpwnam(name);
	  if (passwd =~ null-pointer)
	    let homedir = as(<native-directory-locator>, pw-dir(passwd));
	    return(merge-locators(make(<native-directory-locator>,
				       path: copy-sequence(elements, start: 1),
				       relative?: #t),
				  homedir))
	  else
	    return(path)
	  end
	else
	  return(path)
	end
      else
	return(path)
      end
    else
      return(path)
    end
  end
end method %expand-pathname;

define method %expand-pathname 
    (path :: <posix-file-locator>)
 => (expanded-path :: <locator>)
  let directory = locator-directory(path);
  let expanded-directory = directory & %expand-pathname(directory);
  if (directory ~= expanded-directory)
    make(<native-file-locator>,
	 directory: expanded-directory,
	 base: locator-base(path),
	 extension: locator-extension(path))
  else
    path
  end
end method %expand-pathname;

define method %expand-pathname
    (path :: <posix-file-system-locator>) => (expanded-path :: <locator>)
  path
end method %expand-pathname;


///
define function %shorten-pathname 
    (path :: <posix-file-system-locator>)
 => (shortened-path :: <posix-file-system-locator>)
  path
end function %shorten-pathname;


///
define function %file-exists? 
    (file :: <posix-file-system-locator>) => (exists? :: <boolean>)
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  ~err?
end function %file-exists?;


///
define function %file-type 
    (file :: <posix-file-system-locator>, #key if-not-exists = #f)
 => (file-type :: <file-type>)
  let file = %expand-pathname(file);
  let (err?, st) = %lstat(as(<byte-string>, file));
  if (~err?)
    if (unix-last-error() = $ENOENT & if-not-exists)
      if-not-exists
    else
      unix-file-error("determine the type of", "%s", file)
    end
  elseif (logand(st-mode(st), $S_IFMT) = $S_IFDIR)
    #"directory"
  elseif (logand(st-mode(st), $S_IFMT) = $S_IFLNK)
    #"link"
  else // if (logand(st-mode(st), $S_IFMT) = $S_IFREG)
    #"file"
  end
end function %file-type;


///
define function %link-target
    (link :: <posix-file-system-locator>) => (target :: <posix-file-system-locator>)
  let link = %expand-pathname(link);
  while (%file-type(link, if-not-exists: #"file") == #"link")
    let link-path = as(<byte-string>, link);
    let buffer-size = %pathconf(link-path, $_PC_SYMLINK_MAX);
    let buffer = make(<c-string>, size: buffer-size, fill: '\0');
    let count = %readlink(link-path, buffer, buffer-size);
    if (count = -1)
      unless (unix-last-error() = $ENOENT | unix-last-error() = $EINVAL)
	unix-file-error("readlink", "%s", link)
      end
    else
      let target = as(<physical-locator>, copy-sequence(buffer, end: count));
      link := merge-locators(target, link)
    end
  end;
  link
end function %link-target;


///
define function %delete-file 
    (file :: <posix-file-system-locator>) => ()
  let file = %expand-pathname(file);
  if (%unlink(as(<byte-string>, file)))
    unix-file-error("delete", "%s", file)
  end
end function %delete-file;


/// Whoever heard of an OS that doesn't provide a primitive to copy files?
/// Why, the creators of UNIX, of course since it doesn't.  We have to resort
/// to invoking the cp (copy) command via RUN-APPLICATION.
define function %copy-file
    (source :: <posix-file-system-locator>, destination :: <posix-file-system-locator>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = %expand-pathname(source);
  let destination = %expand-pathname(destination);
  // UNIX strikes again!  The copy command will overwrite its target if
  // the user has write access and the only way to prevent it would
  // require the user to respond to a question!  So, we have to manually
  // check beforehand.  (Just another reason I'm a member of Unix-Haters)
  if (if-exists = #"signal" & file-exists?(destination))
    error(make(<file-system-error>,
	       format-string: "File exists: Can't copy %s to %s",
	       format-arguments: list(as(<string>, source), 
				      as(<string>, destination))))
  end;
  run-application
    (concatenate
       (if ($os-name = #"osf3") "cp -pf" else "cp -p" end,
        " '",
        as(<string>, source),
        "' '",
        as(<string>, destination),
        "'"))
end function %copy-file;


///
define function %rename-file
    (source :: <posix-file-system-locator>, destination :: <posix-file-system-locator>,
     #Key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = %expand-pathname(source);
  let destination = %expand-pathname(destination);
  // UNIX strikes again!  It's rename function always replaces the target.
  // So, if the caller doesn't want to overwrite an existing file, we have 
  // to manually check beforehand.  (Sigh)
  if (if-exists = #"signal" & file-exists?(destination))
    error(make(<file-system-error>,
	       format-string: "File exists: Can't rename %s to %s",
	       format-arguments: list(as(<string>, source),
				      as(<string>, destination))))
  end;
  if (%rename(as(<byte-string>, source), as(<byte-string>, destination)))
    unix-file-error("rename", "%s to %s", source, destination)
  end
end function %rename-file;


///
define function %file-properties
    (file :: <posix-file-system-locator>)
 => (properties :: <explicit-key-collection>)
  let file = %expand-pathname(file);
  let properties = make(<table>);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get attributes of", "%s", file)
  else
    properties[#"size"] := st-size(st);
    // ### st_ctime is the "Time of last status change", not creation-date
    // ### st_birthtime is available on FreeBSD 5, but not elsewhere
    properties[#"creation-date"] := make(<date>, native-clock: st-ctime(st));
    properties[#"access-date"] := make(<date>, native-clock: st-atime(st));
    properties[#"modification-date"] := make(<date>, native-clock: st-mtime(st))
  end;
  properties[#"author"] := %file-property(file, #"author");
  properties[#"readable?"] := %file-property(file, #"readable?");
  properties[#"writeable?"] := %file-property(file, #"writeable?");
  properties[#"executable?"] := %file-property(file, #"executable?");
  properties
end function %file-properties;


/// "Standard" properties not implemented on this platform:
///    ?

/// "Standard" properties not settable on this platform:
///    author, size, creation-date, access-date, modification-date

define method %file-property
    (file :: <posix-file-system-locator>, key == #"author")
 => (author :: false-or(<string>))
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get the author of", "%s", file)
  end;
  let passwd = %getpwuid(st-uid(st));
  if (passwd ~= null-pointer)
    as(<byte-string>, pw-name(passwd))
  else
    unix-file-error("get the author of", "%s", file)
  end
end method %file-property;

define method %file-property 
    (file :: <posix-file-system-locator>, key == #"size")
 => (file-size :: <abstract-integer>)
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get the size of", "%s", file)
  else
    st-size(st)
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"creation-date")
 => (creation-date :: <date>)
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get the creation date of", "%s", file)
  else
    make(<date>, native-clock: st-ctime(st))
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"access-date")
 => (access-date :: <date>)
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get the access date of", "%s", file)
  else
    make(<date>, native-clock: st-atime(st))
  end
end method %file-property;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"modification-date")
 => (modification-date :: <date>)
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get the modification date of", "%s", file)
  else
    make(<date>, native-clock: st-mtime(st))
  end
end method %file-property;

define function accessible? 
    (file :: <posix-file-system-locator>, mode :: <integer>)
 => (accessible? :: <boolean>)
  let file = %expand-pathname(file);
  if (%access(as(<byte-string>, file), mode))
    unless (unix-last-error() = $EACCES)
      unix-file-error("determine access to", "%s", file)
    end;
    #f
  else
    #t
  end  
end function accessible?;

define function accessible?-setter
    (new-mode :: <integer>, file :: <posix-file-system-locator>, on? :: <boolean>)
 => (new-mode :: <integer>)
  let file = %expand-pathname(file);
  let (err?, st) = %stat(as(<byte-string>, file));
  if (err?)
    unix-file-error("get permissions for", "%s", file)
  else
    let old-mode = st-mode(st);
    let mode = if (on?)
                 logior(old-mode, new-mode)
               else
                 logand(old-mode, lognot(new-mode))
               end;
    if (%chmod(as(<byte-string>, file), mode))
      unix-file-error("set permissions for", "%s", file)
    end;
  end;
  new-mode
end function accessible?-setter;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"readable?")
 => (readable? :: <boolean>)
  accessible?(file, $R_OK)
end method %file-property;

define method %file-property-setter 
    (new-readable? :: <boolean>, file :: <posix-file-system-locator>,
     key == #"readable?")
 => (new-readable? :: <boolean>)
  if (new-readable? ~= %file-property(file, #"readable?"))
    accessible?(file, new-readable?) := logior($S_IRUSR, $S_IRGRP, $S_IROTH)
  end;
  new-readable?
end method %file-property-setter;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"writeable?")
 => (writeable? :: <boolean>)
  accessible?(file, $W_OK)
end method %file-property;

define method %file-property-setter 
    (new-writeable? :: <boolean>, file :: <posix-file-system-locator>, 
     key == #"writeable?")
 => (new-writeable? :: <boolean>)
  if (new-writeable? ~= %file-property(file, #"writeable?"))
    accessible?(file, new-writeable?) := logior($S_IWUSR, $S_IWGRP, $S_IWOTH)
  end;
  new-writeable?
end method %file-property-setter;

define method %file-property
    (file :: <posix-file-system-locator>, key == #"executable?")
 => (executable? :: <boolean>)
  accessible?(file, $X_OK)
end method %file-property;

define method %file-property-setter 
    (new-executable? :: <boolean>, file :: <posix-file-system-locator>, 
     key == #"executable?")
 => (new-executable? :: <boolean>)
  if (new-executable? ~= %file-property(file, #"executable?"))
    accessible?(file, new-executable?) := logior($S_IXUSR, $S_IXGRP, $S_IXOTH)
  end;
  new-executable?
end method %file-property-setter;


///
define function %do-directory 
    (f :: <function>, directory :: <posix-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  let directory-fd :: <DIR*> = as(<DIR*>, null-pointer);
  block ()
    directory-fd := %opendir(as(<byte-string>, directory));
    if (directory-fd = null-pointer)
      unix-file-error("start listing of", "%s", directory)
    end;
    unix-last-error() := 0;
    let dirent = %readdir(directory-fd);
    while (dirent ~= null-pointer)
      let filename :: <byte-string> = dirent-name(dirent);
      let type :: <file-type>
	= %file-type(make(<posix-file-locator>,
			  directory: directory,
			  name: filename));
      unless (type == #"directory" & (filename = "." | filename = ".."))
	f(directory,
	  filename,
	  type)
      end;
      unix-last-error() := 0;
      dirent := %readdir(directory-fd);
    end;
    if (unix-last-error() ~= 0)
      unix-file-error("continue listing of", "%s", directory)
    end;
  cleanup
    if (directory-fd ~= null-pointer)
      %closedir(directory-fd);
    end
  end
end function %do-directory;


///
define function %create-directory 
    (directory :: <posix-directory-locator>)
 => (directory :: <posix-directory-locator>)
  let directory = %expand-pathname(directory);
  // Let the process' UMASK restrict access to the directory as desired
  if (%mkdir(as(<byte-string>, directory),
             logior($S_IRWXU, $S_IRWXG, $S_IRWXO)))
    unix-file-error("create the directory", "%s", directory)
  else
    directory
  end
end function %create-directory;


///
define function %delete-directory
    (directory :: <posix-directory-locator>) => ()
  let directory = %expand-pathname(directory);
  if (%rmdir(as(<byte-string>, directory)))
    unix-file-error("delete", "%s", directory)
  end
end function %delete-directory;


///---*** Is there an easier way?  (Look into it ...)
define function %directory-empty?
    (directory :: <posix-directory-locator>) => (empty? :: <boolean>)
  ~%file-exists?(directory)
    | block (return)
	%do-directory
	  (method (directory :: <posix-directory-locator>, name :: <string>, type :: <file-type>)
	     ignore(directory); ignore(name); ignore(type);
	     return(#f)
	   end,
	   directory);
	#t
      end
end function %directory-empty?;


///
define function %home-directory
    () => (home-directory :: false-or(<posix-directory-locator>))
  let path = environment-variable("HOME");
  path
    & as(<posix-directory-locator>, path)
end function %home-directory;


///
define function %working-directory
    () => (working-directory :: false-or(<posix-directory-locator>))
  let bufsiz :: <integer> = %pathconf(".", $_PC_PATH_MAX);
  let buffer :: <c-string> = make(<c-string>, size: bufsiz, fill: '\0');
  let result :: <c-string> = %getcwd(buffer, bufsiz);

  if (result ~= null-pointer)
    as(<directory-locator>, buffer);
  else
    // Arrive here iff we couldn't get the working directory
    unix-file-error("getcwd", #f)
  end
end function %working-directory;


///
define function %working-directory-setter
    (new-working-directory :: <posix-directory-locator>)
 => (new-working-directory :: <posix-directory-locator>)
  let directory = %expand-pathname(new-working-directory);
  if (%chdir(as(<byte-string>, directory)))
    unix-file-error("chdir", "%s", directory)
  end;
  directory
end function %working-directory-setter;


///
define variable *temp-directory* = #f;

define function %temp-directory
    () => (temp-directory :: false-or(<posix-directory-locator>))
  *temp-directory*
    | (*temp-directory*
         := as(<posix-directory-locator>,
               environment-variable("TMPDIR") | "/tmp"))
end function %temp-directory;


/// A UNIX system has exactly one root directory
define function %root-directories () => (roots :: <sequence>)
  vector(as(<posix-directory-locator>, "/"))
end function %root-directories;
