module:      dylan-user
author:      Douglas M. Auclair, dauclair@hotmail.com

define library base-file-system
  use dylan;
  use regular-expressions;
  use streams;
  use string-extensions;

  export base-file-system;
end library;

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
