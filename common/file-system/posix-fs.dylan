module:      file-system
rcs-header:  $Header: /scm/cvs/src/common/file-system/Attic/posix-fs.dylan,v 1.1 1999/04/14 19:33:00 tree Exp $
author:      Tom Emerson, tree@tiac.net
copyright:   Copyright 1999 Thomas R. Emerson
synopsis:    POSIX implementation of various 'low-level' file-system functions.

define constant $POSIX_FILE_EXISTS      = 1;
define constant $POSIX_DIRECTORY_EXISTS = 2;

define function native-file-exists?(file :: <pathname>, #key flags :: <integer> = 0)
 => (exists :: <boolean>)
  let res = call-out("file_exists_p", int:, ptr: (export-value(<c-string>, file)).raw-value, int: flags);
  if (res = 0)
    #t;
  else
    #f;
  end if;
end function;
