module:      dylan-user
rcs-header:  $Header: /scm/cvs/src/common/file-system/exports.dylan,v 1.4 2003/07/21 18:43:08 andreas Exp $
author:      Douglas M. Auclair, dauclair@hotmail.com

define library file-system
  use dylan;
  use melange-support;
  use regular-expressions;
  use streams;
  use format-out;
  use string-extensions;
  use base-file-system;

  export file-system;
end library;

// These definitions come from the Common-Dylan document "System and I/O",
// chapter 8, available from Functional Objects, inc
define module file-system
// section 8.2: types
  create <file-type>,
         <copy/rename-disposition>,
         <pathname>,
         <file-system-error>;

// section 8.3: Manipulating files
  create copy-file,
         rename-file,
         delete-file
	 // not yet: , file-property-setter;
	 ;

// section 8.4: Manipulating directories
  create create-directory,
         delete-directory,
         do-directory,
         ensure-directories-exist,
	 working-directory-setter;

// section 8.5: Finding out file system information
  create home-directory,
	 root-directories,
	 temp-directory,
	 working-directory;

// section 8.6: Finding out file information
  create file-exists?,
    file-type

  /** not yet: , file-properties,
	 file-property,
	 file-type
   **/ ;
end module file-system;

// -------------------------------------------------------
// Implementation modules
// -------------------------------------------------------

define module types
  use dylan;
  use extensions;

  use file-system, 
    import: { <file-type>,
	     <copy/rename-disposition>,
	     <pathname>,
	     <file-system-error> };
end module types;

define module helpers
  use dylan;
  use streams, export: all;
  use format-out;
  use standard-io;
  use melange-support;

  use file-system;

  export 
    try, try-call-out,
    file-signal,
    do-file-operation, 
    $path-separator,
    append, as-dir, create-pointer, \with-pointer, convert-to-string;
end module helpers;

define module dir-commands
  use dylan;
  use melange-support, export: all;
  use helpers;
  use file-system, import: { <pathname> };
  
  export 
    readdir, opendir, closedir, d-name,
    lstat, st-mode,
    <dir>, <stat>, <char*>,
    $S-IFMT, $S-IFLNK, $S-IFREG, $S-IFDIR,

    is-dir?, is-link?, is-regular-file?, stat-mode;
end module dir-commands;

define module information
  use dylan;
  use file-system;
  use dir-commands;
  use helpers;

  use base-file-system, exclude: { delete-file, rename-file };
end module information;

define module manipulating-files
  use dylan;
  use base-file-system, prefix: "base-";

  use file-system;

  use types;
  use information;
  use helpers;
end module manipulating-files;

define module manipulating-directories
  use dylan;
  use format-out;
  use regular-expressions;
  use standard-io;

  use file-system;

  use types;
  use information;
  use helpers;
  use dir-commands;
end module manipulating-directories;

define module directory-information
  use dylan;

  use file-system;

  use base-file-system, import: { filename-prefix, get-current-directory };
  use helpers;
  use manipulating-directories;
  use dir-commands;
end module directory-information;

