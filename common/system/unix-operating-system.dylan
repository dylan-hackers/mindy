Module:       system-internals
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1998-2001 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $os-name = begin
                             let (result, utsname) = %uname();
                             as(<symbol>, utsname.sysname)
                           end;

define constant $os-variant = $os-name;
define constant $os-version = begin
                                let (result, utsname) = %uname();
                                utsname.release
                              end;

define constant $command-line-option-prefix = '-';

define function command-line-option-prefix
    () => (prefix :: <character>)
  $command-line-option-prefix
end function command-line-option-prefix;

define function login-name () => (name :: false-or(<string>))
  %getlogin();
end function login-name;

define function login-group () => (group :: false-or(<string>))
  let gid = %getgid();
  let gr-ent = %getgrgid(gid);
  if (gr-ent = null-pointer)
    #f
  else
    gr-ent.gr-name
  end;
end function login-group;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-name () => (name :: false-or(<string>))
  #f
end function owner-name;

///---*** NOTE: Provide a non-null implementation when time permits...
define function owner-organization () => (organization :: false-or(<string>))
  #f
end function owner-organization;

define constant $environment-variable-delimiter = ':';

define function environment-variable
    (name :: <byte-string>) => (value :: false-or(<byte-string>))
  let v = %getenv(name);
  if (v = null-pointer)
    #f
  else
    as(<byte-string>, v)
  end;
end function environment-variable;

define function environment-variable-setter
    (new-value :: false-or(<byte-string>), name :: <byte-string>)
 => (new-value :: false-or(<byte-string>))
  if(new-value)
    %putenv(concatenate(name, "=", new-value));
  else
    %unsetenv(name);
  end;
  new-value;
end function environment-variable-setter;

///---*** NOTE: Should change this to use exec so we can capture I/O, etc ...
define function run-application
    (command :: <string>, #key, #all-keys) => (status :: <integer>)
  %system(command)
end function run-application;


///---*** NOTE: The following functions need real implementations!

define function create-application-event
    (event :: <string>) => (event-object :: <machine-word>)
  as(<machine-word>, 0)
end function create-application-event;

define constant $INFINITE_TIMEOUT = -1;

define function wait-for-application-event
    (event-object :: <machine-word>, #key timeout :: <integer> = $INFINITE_TIMEOUT)
 => (success? :: <boolean>)
  #t
end function wait-for-application-event;

define function signal-application-event
    (event :: <string>) => (success? :: <boolean>)
  #t
end function signal-application-event;

define function load-library
    (name :: <string>) => (module)
  #f
end function load-library;
