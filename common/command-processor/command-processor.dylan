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
  format-out("\r%s%s ", *prompt*, *command-line*);
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
  slot name      :: <string>,       required-init-keyword: name:;
  slot command   :: <function>,     required-init-keyword: command:;
  slot summary   :: <string>,       required-init-keyword: summary:;
  slot full-help :: false-or(<string>) = #f, init-keyword: full-help:;
end class <command>;

define method make( cmd == <command>, #key, #all-keys) => (res :: <command>)
  let result = next-method();
  add!(*command-table*, result);
  result;
end method make;

define function n-spaces(n :: <integer>)
 => (string-of-n-spaces :: <string>)
  if(n < 0)
    ""
  else
    make(<string>, size: n, fill: ' ')
  end if;
end function n-spaces;

// find the command with command-name as the prefix of its name
define function find-command-by-prefix(command-name :: <string>)
  choose(method(x) 
             case-insensitive-equal(command-name,
                                    subsequence(x.name, end: command-name.size))
         end, *command-table*)
end function find-command-by-prefix;

// Find the command that forms the prefix of command-name. This
// is not the same as above!
define function find-prefixing-command(command-name :: <string>)
  choose(method(x) 
             case-insensitive-equal(subsequence(*command-line*, 
                                                end: x.name.size), x.name)
         end, *command-table*)
end function find-prefixing-command;

define function find-command(command-name :: <string>)
  choose(method(x) case-insensitive-equal(command-name, x.name) end, 
         *command-table*)
end function find-command;

make(<command>, 
     name: "Help", 
     summary:   "You have just found out what this does :).",
     full-help: 
       "Without an argument, lists all available commands.  When given\r\n"
       "an argument, displays help for the command named.\r\n",
     command: method(parameter)
                  if(parameter.size = 0)
                    for(c in *command-table*)
                      format-out("%s%s %s\r\n", 
                                 c.name,
                                 n-spaces(20 - c.name.size),
                                 c.summary);
                    end for;
                  else
                    let commands = find-command-by-prefix(parameter);
                    if(commands.size = 0)
                      format-out("This command is not known to the system.\r\n");
                    else
                      for(c in commands)
                        if(c.full-help)
                          format-out("%s:\r\n%s", c.name, c.full-help)
                        else
                          format-out("%s: %s\r\n", 
                                     c.name, c.summary)
                        end if;
                      end for;
                    end if;
                  end if;
              end);

make(<command>, name: "Set Prompt", 
     summary: "Sets the prompt to the argument given.",
     command: method(x) *prompt* := x end);

define variable *command-line* = "";
define variable *buffer-pointer* = 0;
define variable *prompt* = "gwydion> ";

define method self-insert-command(c)
  *command-line* := 
    concatenate(subsequence(*command-line*, end: *buffer-pointer*),
                vector(c),
                subsequence(*command-line*, start: *buffer-pointer*));
  *buffer-pointer* := *buffer-pointer* + 1;
  repaint-line();
end method self-insert-command;

define method run-command(c)
  complete-command(c);
  format-out("\r\n");
  let commands = find-prefixing-command(*command-line*);
  if(commands.size = 0)
    format-out("Unknown command. Try Help.\r\n");
  elseif(commands.size > 1)
    format-out("Ambiguous command. Try Help.\r\n");
  else
    to-cooked();
    commands[0].command(copy-sequence(*command-line*, 
                                      start: commands[0].name.size + 1));
    to-raw();
  end if;

  *command-line* := "";
  *buffer-pointer* := 0;
  repaint-line();
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

define variable to-raw    = identity;
define variable to-cooked = identity;


define function run-command-processor()
  let running = #t;
  make(<command>, 
       name: "Exit", 
       command: method(parameter)
                    running := #f;
                end,
       summary: "Exits the command loop.");

  let old-termios = make(<termios>);
  tcgetattr(*standard-input*.file-descriptor, old-termios);

  let new-termios = make(<termios>);
  cfmakeraw(new-termios);

  to-raw    := curry(tcsetattr, *standard-input*.file-descriptor, 
                     $TCSANOW, new-termios);
  to-cooked := curry(tcsetattr, *standard-input*.file-descriptor, 
                     $TCSANOW, old-termios);

  to-raw();

  repaint-line();
  while(running)
    let c = read-element(*standard-input*);
    *key-bindings*[as(<integer>,c)](c);
    force-output(*standard-output*);
  end while;

  to-cooked();
end;