module: c-file

define class <c-file> (<object>)
  slot c-file-name :: <byte-string>,
    required-init-keyword: name:;
  slot c-file-system-header? :: <boolean>,
    init-keyword: system-header?:,
    init-value: #f;

  // Contains a mix of <c-declaration>s and <c-file>s to represent
  // declarations and #included files.
  slot c-file-contents :: <stretchy-vector>
    = make(<stretchy-vector>);
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

