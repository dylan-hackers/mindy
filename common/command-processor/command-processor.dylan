module: command-processor
synopsis: 
author: 
copyright: 

define constant <modifiers> = one-of(#"shift", #"control", #"alternate",
                                     #"meta", #"super", #"hyper");

define class <keystroke> (<object>)
  slot key;
  slot modifiers;
end class <keystroke>;

define method put-char(c)
  write-element(*standard-output*, c);
  force-output(*standard-output*);
end method put-char;

define method repaint-line()
  format-out("\r%s ", *command-line*);
  for(i from 0 below *command-line*.size - *buffer-pointer* + 1)
    write-element(*standard-output*, '\b');
  end for;
  force-output(*standard-output*);
end method repaint-line;

define method ring-bell()
  put-char(as(<character>, 7));
end method ring-bell;

define constant *command-table* = make(<stretchy-vector>);

define class <command> (<object>)
  slot name, init-keyword: name:;
  slot command, init-keyword: command:;
end class <command>;

define method make( cmd == <command>, #key, #all-keys) => (res :: <command>)
  let result = next-method();
  add!(*command-table*, result);
  result;
end method make;

make(<command>, 
     name: "Help", 
     command: method()
                  format-out("Help not yet available :).\r\n");
                  force-output(*standard-output*);
              end);

define variable *command-line* = "";
define variable *buffer-pointer* = 0;

define method self-insert-command(c)
  *command-line* := 
    concatenate(subsequence(*command-line*, end: *buffer-pointer*),
                vector(c),
                subsequence(*command-line*, start: *buffer-pointer*));
  *buffer-pointer* := *buffer-pointer* + 1;
  repaint-line();
end method self-insert-command;

define method run-command(c)
  format-out("\r\nThe command entered was: '%s'\r\n", *command-line*);
  force-output(*standard-output*);

  for(i in *command-table*)
    if(case-insensitive-equal(*command-line*, i.name))
      i.command();
    end if;
  end for;

  *command-line* := "";
  *buffer-pointer* := 0;
end method run-command;

define method forward-char-command(c)
  if(*buffer-pointer* < *command-line*.size)
    put-char(*command-line*[*buffer-pointer*]);
    *buffer-pointer* := *buffer-pointer* + 1;
  else
    ring-bell();
  end if;
end method forward-char-command;

define method backward-char-command(c)
  if(*buffer-pointer* > 0)
    put-char('\b');
    *buffer-pointer* := *buffer-pointer* - 1;
  else
    ring-bell();
  end if;
end method backward-char-command;

define method delete-char-backwards(c)
  if(*buffer-pointer* > 0)
    *command-line* := 
      concatenate(subsequence(*command-line*, end: *buffer-pointer* - 1),
                  subsequence(*command-line*, start: *buffer-pointer*));
    *buffer-pointer* := *buffer-pointer* - 1;
    repaint-line();
  else
    ring-bell();
  end if;
end method delete-char-backwards;

define method complete-command-aux()
    for(i in *command-table*)
    if(case-insensitive-equal(*command-line*, 
                              subsequence(i.name, end: *command-line*.size)))
      *command-line* := copy-sequence(i.name);
      *buffer-pointer* := i.name.size;
    end if;
  end for;
end method complete-command-aux;

define method complete-command(c)
  complete-command-aux();
  repaint-line();
end method complete-command;

define method complete-command-and-insert-space(c)
  complete-command-aux();
  self-insert-command(' ');
  repaint-line();
end method complete-command-and-insert-space;

define method beginning-of-line(c)
  *buffer-pointer* := 0;
  repaint-line();
end method beginning-of-line;

define method end-of-line(c)
  *buffer-pointer* := *command-line*.size;
  repaint-line();
end method end-of-line;

define method kill-to-end-of-line(c)
  for(i from 0 below *command-line*.size - *buffer-pointer*)
    put-char(' ');
  end for;
  *command-line* := copy-sequence(subsequence(*command-line*, 
                                              end: *buffer-pointer*));
  repaint-line();
end method kill-to-end-of-line;

define variable *key-bindings* = make(<simple-vector>, 
                                      size: 256,
                                      fill: self-insert-command);

*key-bindings*[as(<integer>, '\r')] := run-command;
*key-bindings*[2] := backward-char-command;
*key-bindings*[6] := forward-char-command;
*key-bindings*[8] := delete-char-backwards;
*key-bindings*[127] := delete-char-backwards;
*key-bindings*[9] := complete-command;
*key-bindings*[as(<integer>, ' ')] := complete-command-and-insert-space;
*key-bindings*[1] := beginning-of-line;
*key-bindings*[5] := end-of-line;
*key-bindings*[11] := kill-to-end-of-line;

begin
  let running = #t;
  make(<command>, 
       name: "Exit", 
       command: method()
                    running := #f;
                end);

  let old-termios = make(<termios>);
  tcgetattr(*standard-input*.file-descriptor, old-termios);

  let new-termios = make(<termios>);
  cfmakeraw(new-termios);
  tcsetattr(*standard-input*.file-descriptor, $TCSANOW, new-termios);

  while(running)
    let c = read-element(*standard-input*);
    force-output(*standard-output*);
    *key-bindings*[as(<integer>,c)](c);
  end while;

  tcsetattr(*standard-input*.file-descriptor, $TCSANOW, old-termios);
end;