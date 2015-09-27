module:      manipulating-files
author:      Douglas M. Auclair, dauclair@hotmail.com

// Basic operations on files:  copying, renaming, deleting.  
// These operations are described in
// http://www.functionalobjects.com/products/doc/io/io_197.htm

define function delete-file(file :: <pathname>) => ()
  try(base-delete-file, file);
end function delete-file;

define function rename-file(old-file :: <pathname>, new-file :: <pathname>,
                            #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  do-file-operation(base-rename-file, old-file, new-file, if-exists);
end function rename-file;

define method copy-file(old-file :: <pathname>, 
			new-file :: <pathname>,
			#key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  local method copy-to(origin, destination)
	  with-open-file(from = origin)
//	    with-open-file(to = destination, direction: #"output");
// Why doesn't the above line work?
	    let to = make(<file-stream>, 
			  locator: destination, 
			  direction: #"output");
	    until(stream-at-end?(from))
	      write-element(to, read-element(from));
	    end until;
	    close(to);
	  end with-open-file;
	end;
	    
  do-file-operation(copy-to, old-file, new-file, if-exists);
end method copy-file;
