module:      information
rcs-header:  $Header: /scm/cvs/src/common/file-system/information.dylan,v 1.4 2003/07/21 18:43:08 andreas Exp $
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

define method file-type(file :: <pathname>) => (type :: <file-type>);
  let stat = stat-mode(file);
  let type = case
	       is-dir?(stat) => #"directory";
	       is-link?(stat) => #"link";
	       otherwise => #"file";
	     end case;
end method file-type;
       