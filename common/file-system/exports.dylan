module:      dylan-user
rcs-header:  $Header: /scm/cvs/src/common/file-system/exports.dylan,v 1.1 2000/10/21 03:39:41 dauclair Exp $
author:      Douglas M. Auclair, dauclair@hotmail.com

define library file-system
  use dylan;
  use melange-support;
  use regular-expressions;
//  use compiler-base, import: { file-system }, prefix: "base-";
  use streams;
  use format-out;
  use string-extensions;

  export file-system;
  export base-file-system;
end library;

define module file-system
  create <file-type>,
         <copy/rename-disposition>,
         <pathname>,
         <file-system-error>;

  create create-directory,
         delete-directory,
         do-directory,
         ensure-directories-exist;

  create copy-file,
         rename-file,
         delete-file,
         file-exists?;
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

define module base-file-system
  use dylan;
  use extensions;
  use System, import: {system};
  use String-conversions;  // as(<string>, char)
  // We use the Streams library to see if a file exists
  use Streams, 
    import: {<stream>, <file-stream>, <file-does-not-exist-error>, close}; 

#if (mindy)
  use System, import: {getcwd};
#else
  use System, 
     import: {call-out, buffer-address, <buffer>};
#endif

  export
     <filename>,
     $search-path-separator,
     search-path-separator?,
     $a-path-separator,
     path-separator?,
     filename-prefix,
     filename-extension,
     base-filename,
     pathless-filename,
     extensionless-filename,
     
     get-current-directory,

     find-file,
     find-and-open-file,
     
     delete-file,
     rename-file,
     files-identical?;
end module base-file-system;

define module information
  use dylan;
  use file-system;

  use base-file-system, exclude: { delete-file, rename-file };
end module information;

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
    $path-separator;
end module helpers;

define module manipulating-files
  use dylan;
  use base-file-system, prefix: "base-";

  use file-system;

  use types;
  use information;
  use helpers;
end module manipulating-files;

define module dir-commands
  use dylan;
  use melange-support, export: all;
  
  export 
    readdir, opendir, closedir, d-name, // d-ino, d-off, d-reclen,
    // dd-fd, dd-loc, dd-size, 
    lstat, stat$st-mode,
    <dir>, <stat>, <anonymous-9>,
    <virtual-dir>, <virtual-dirent>;
end module dir-commands;

define module manipulating-directories
  use dylan;
  use format-out;
  use standard-io;
  use regular-expressions;

  use file-system;

  use types;
  use information;
  use helpers;
  use dir-commands;
end module manipulating-directories;
