module: c-file

define class <c-file> (<object>)
  slot c-file-name :: false-or(<byte-string>),
    init-keyword: name:,
    init-value: #f;
  slot c-file-contents :: <stretchy-vector>
    = make(<stretchy-vector>); // contains <c-file>s and <c-declaration>s
end class;

define method add-c-declaration!
    (file :: <c-file>, decl :: <c-declaration>)
 => ()
  add!(file.c-file-contents, decl);
end method;

define method add-c-file!
    (file :: <c-file>, subfile :: <c-file>) 
 => ()
  add!(file.c-file-contents, subfile);
end method;
