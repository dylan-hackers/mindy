module:      information
rcs-header:  $Header: /scm/cvs/src/common/file-system/information.dylan,v 1.3 2003/03/07 18:08:04 andreas Exp $
author:      Douglas M. Auclair, dauclair@hotmail.com

// Gets information files ... whether they exist and their properties
// These operations are described in
// http://www.functionalobjects.com/products/doc/io/io_200.htm

define method file-exists?(file :: <pathname>)
  find-file(pathless-filename(file), paths(file)); 
end method file-exists?;

// for copying and renaming we want one, and only one, path to search
define method paths(file :: <pathname>) => ans :: <sequence>;
  let ans = filename-prefix(file);
  if(size(ans) = 0)
    #("./")
  else
    list(ans)
  end if;
end method paths;

define method is-dir?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, #xf000) = #x4000;
end method is-dir?;

define method is-link?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, #xf000) = #xa000;
end method is-link?;

define function stat-mode(file :: <pathname>) => (bar :: <integer>)
  with-pointer(stat* = <stat>)
    with-pointer(str* = file)
      lstat(str*, stat*);
      stat$st-mode(stat*);
    end with-pointer;
  end with-pointer;
end function stat-mode;


define method file-type(file :: <pathname>) => (type :: <file-type>);
  let stat = stat-mode(file);
  let type = case
	       is-dir?(stat) => #"directory";
	       is-link?(stat) => #"link";
	       otherwise => #"file";
	     end case;
end method file-type;
       