library: getopttest
module: getopttest
author: Jeff Dubrule
copyright: GPL

define variable *options* :: <option-table> = make(<option-table>);

define method init-options()
                     // Doc:  val:   names:
  add-option(*options*, "Foo",    #f, "foo", "f");
  add-option(*options*, "Bar", "bar", "bar", "b");
end method;

define method main(progname :: <string>, #rest argv)
  init-options();
  format-out("Program name is: %s\n", progname);
  let extras = parse-options(*options*, argv);
  format-out("Current options values:\n");
  let option :: <option> = make(<option>);

  format-out("*options*[\"foo\"]: %=\n", *options*["foo"]);
  format-out("*options*['f']: %=\n", *options*['f']);
  format-out("*options*[\"bar\"]: %=\n", *options*["bar"]);
  format-out("*options*['b']: %=\n", *options*['b']);

  format-out("\nNon-option arguments: %=\n", extras);

  exit(exit-code: 0);
end method main;
