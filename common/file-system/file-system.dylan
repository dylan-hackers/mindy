module:      file-system
rcs-header:  $Header: /scm/cvs/src/common/file-system/Attic/file-system.dylan,v 1.1 1999/04/14 19:32:58 tree Exp $
author:      Tom Emerson, tree@tiac.net
copyright:   Copyright 1999 Thomas R. Emerson
synopsis:    Implementation of the Harlequin Dylan 1.2 file-system library
             for POXIS compatible operating systems.

/* DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER

   This is still in very early stages of implementation: use at your own
   risk.

   REGNAD REGNAD REGNAD REGNAD REGNAD REGNAD REGNAD REGNAD REGNAD REGNAD */
   


define constant <pathname> = <byte-string>;
define constant <copy/rename-disposition> = one-of(#"signal", #"replace");

define function relative-path?(path :: <pathname>)
 => (relative? :: <boolean>)
  if (path.size > 0 & path[0] = '/')
    #t;
  else
    #f;
  end if;
end function relative-path?;

define function file-exists?(file :: <pathname>)
 => (exists? :: <boolean>)
  native-file-exists?(file, flags: $POSIX_FILE_EXISTS);
end function file-exists?;

define function ensure-directories-exist(file :: <pathname>)
 => (created? :: <boolean>)
  #f;
end function ensure-directories-exist;

define function do-directory(function :: <function>, file :: <pathname>)
 => ()
end function do-directory;

define function delete-file(file :: <pathname>)
 => ()
end function delete-file;

define function delete-directory(directory :: <pathname>)
 => ()
end function delete-directory;

define function create-directory(parent :: <pathname>, name :: <string>)
 => (directory :: <pathname>)
  "foo";
end function create-directory;

define function copy-file(old-file :: <pathname>, new-file :: <pathname>,
                          #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
end function copy-file;
