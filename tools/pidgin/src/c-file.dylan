module: c-file

define class <c-file> (<object>)
  slot c-file-name :: <byte-string>,
    required-init-keyword: name:;
  slot c-file-system-header? :: <boolean>,
    init-keyword: system-header?:,
    init-value: #f;

  slot c-file-included-files :: <stretchy-vector>
    = make(<stretchy-vector>); // contains <c-file>s
  slot c-file-declarations :: <stretchy-vector>
    = make(<stretchy-vector>); // contains <c-declaration>s
end class;

define method add-c-declaration!
    (file :: <c-file>, decl :: <c-declaration>)
 => ()
  add!(file.c-file-declarations, decl);
end method;

define method add-c-file!
    (file :: <c-file>, subfile :: <c-file>) 
 => ()
  add!(file.c-file-included-files, subfile);
end method;

