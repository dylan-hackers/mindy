module:      file-system
rcs-header:  $Header: /scm/cvs/src/common/file-system/Attic/posix-fs.dylan,v 1.3 1999/04/22 17:08:01 tree Exp $
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

define function native-delete-file(file :: <pathname>)
  => (result :: <integer>)
  let res = call-out("unlink", int:, ptr: (export-value(<c-string>, file)).raw-value);
  res;
end function;

define function native-delete-directory(directory :: <pathname>)
 => (result :: <integer>)
  let res = call-out("rmdir", int:, ptr: (export-value(<c-string>, directory)).raw-value);
  res;
end function;

define function native-create-directory(directory :: <pathname>)
  => (result :: <integer>)
  let res = call-out("mkdir", int:, ptr: (export-value(<c-string>, directory)).raw-value, int: #o755);
  res;
end function;

define function native-rename-file(old-name :: <pathname>, new-name :: <pathname>)
  => (res :: <integer>)
  let res = call-out("rename", int:,
                     ptr: (export-value(<c-string>, old-name).raw-value),
                     ptr: (export-value(<c-string>, new-name).raw-value));
  res;
end function;

