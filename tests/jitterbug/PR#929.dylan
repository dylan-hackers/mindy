module: PR-929

define class <foo>(<object>)
  slot buba :: <integer> = 1;
end;


begin
  let f = make(<foo>);
  f.buba := 42;
end;