module: pidgin
synopsis: 
author: 
copyright: 

// This code is used by Pidgin's "--debug" command line option.

define variable debug :: <boolean> = #f;

define method debug-print (decl :: <c-declaration>) => ()
  format(*standard-output*, "%=\n%s\n", decl, format-c-declaration(decl));
  force-output(*standard-output*);
end;

define method debug-print (decl :: <c-file>) => ()
  format(*standard-output*, "#include %s\nTranslation: %s\n", c-file-name(decl), c-output(decl));
  force-output(*standard-output*);
end;
