module: pidgin
synopsis: 
author: 
copyright: 

//==============================================================================
// GCC portability
//==============================================================================

define method construct-include-path
    (extra-includes)
 => (result :: <gcc-include-path>)
  let (to-gcc, from-gcc) = piped-exec("gcc --print-file-name=include");
  make(<gcc-include-path>,
    standard-include-directories:
    $i386-linux-platform.c-platform-default-include-path,
    hidden-include-directory: read-line(from-gcc),
    extra-include-directories: extra-includes,
    extra-user-include-directories: #());
end method;
