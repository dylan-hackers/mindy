module: operating-system

define function login-name()
 => (name :: false-or(<string>))
  #f;
end;

define function login-group()
 => (group :: false-or(<string>))
  #f;
end;

define function owner-name()
 => (group :: false-or(<string>))
  #f;
end;

define function owner-organization()
 => (group :: false-or(<string>))
  #f;
end;

define function split
    (character :: <character>, string :: <byte-string>)
 => (components :: <vector>)

  let count = 1;
  for (c in string)
    if (c = character)
      count := count + 1;
    end;
  end;

  let result = make(<vector>, size: count);

  // XXX - chop into bits

  result;
end;

define function tokenize-environment-variable
    (variable :: <byte-string>)
 => (components :: <vector>)
  // XXX - The return value is undocumented.
  split(':', variable);
end function;

define function tokenize-command-string
    (command-string :: <byte-string>)
 => (command :: <byte-string>, #rest arguments :: <byte-string>)
  // XXX - handle bash quoting and escape sequences, but not
  // expansion or interpolation.
end function;

    