module: test

define library test
  use dylan;
  use streams;
  use stream-extensions;
  use format;
  use tk;
end library test;

define module test
  use dylan;
  use threads;
  use extensions;
  use streams;
  use standard-io;
  use eager-stream;
  use format;
  use tk;
  use tk-extension;
end module test;

// Test routine

define constant *tty* = make(<eager-stream>, stream: *standard-output*);

define method main(program-name :: <string>, #rest args)
  let widgets = #[];
  local method print-a-config (config)
	  if (config.size == 5)
	    let value = config.last;
	    format(*tty*, "  %-20s  %-55s\n",
		   copy-sequence(config.second, end: 20),
		   if (instance?(value, <string>))
		     copy-sequence(value, end: 55);
		   else
		     copy-sequence(format-to-string("%=", value), end: 60);
		   end if);
	  end if;
	end method print-a-config;
  local method print-config(index)
	  let widget = widgets[index];
	  format(*tty*, "Configuration for %s:\n", head(widget));
	  let configs = configuration(widget.tail);
	  do(print-a-config, configs);
	end method;
  local method print-a-binding(binding)
	  format(*tty*, "  %-20s  %-55s\n", binding.head, binding.tail);
	end method print-a-binding;
  local method print-bindings(index)
	  let widget = widgets[index];
	  format(*tty*, "Bindings for %s:\n", head(widget));
	  do(print-a-binding, get-bindings(tail(widget)));
	end method;

  let f2 = make(<frame>, fill: "both", expand: #t);

  let c1 = make(<canvas>, in: f2, fill: "both", expand: #t, side: "bottom");
  let item1 = create-line(c1, #(0, 0, 100, 100, 200, 0), smooth: #t);
  let item2 = create-rectangle(c1, "1c", "1c", "2c", "3c", fill: "red");
  let item3 = create-oval(c1, "1c", "1c", "2c", "3c", fill: "green");
  
  let l1 = make(<listbox>, in: f2, fill: "y");
  bind(l1, "<Double-Button-1>",
       method () do(print-config, current-selection(l1)) end method);
  bind(l1, "<Button-2>", "%W select from [%W nearest %y]");
  bind(l1, "<Double-Button-2>",
       method () do(print-bindings, current-selection(l1)) end method);
  let s1 = scroll(l1, in: f2, fill: "y", side: "right");
//  let s1 = make(<scrollbar>, before: l1, fill: "y", side: "right",
//		command: curry(yview, l1));
//  configure(l1, yscrollcommand: curry(set-units, s1));

  let s2 = scroll(l1, before: l1, orient: "horizontal",
		  side: "bottom", fill: "x");

  let f1 = make(<frame>, fill: "x", side: "top");
  let bmake = rcurry(curry(make, <button>), in: f1);

  let switch = make(<active-variable>, class: <boolean>, value: #t);
  let word = make(<active-variable>, value: "barbeque");

  let b1 = make(<menubutton>, in: f1, text: "File", underline: 0,
		expand: #f);
  let m3 = make(<menu>, in: b1);
  configure(b1, menu: m3);
  add-command(m3, label: "call 'test'", command: "test");
  add-command(m3, label: "exit", command: "exit");

  let b2 = make(<menubutton>, in: f1, text: "Print", underline: 0,
		expand: #f);
  let m4 = make(<menu>, in: b2);
  configure(b2, menu: m4);
  add-command(m4, label: "bindings for selection(s)",
	      command: method ()
			 do(print-bindings, current-selection(l1));
		       end method);
  add-command(m4, label: "configuration for selection(s)", 
	      command: method ()
			 do(print-config, current-selection(l1));
		       end method);
  add-separator(m4);
  add-command(m4, label: "variable status",
	      command: method ()
			 format(*tty*, "%s: Checkbutton is %s\n",
				word.value,
				if (switch.value) "on" else "off" end);
		       end method);

  let b3 = make(<menubutton>, in: f1, text: "Variables",
		     underline: 0);
  let m2 = make(<menu>, in: b3);
  configure(b3, menu: m2);
  add-radiobutton(m2, label: "foo", variable: word, value: "food chain");
  add-radiobutton(m2, label: "bar", variable: word, value: "barbeque");
  add-radiobutton(m2, label: "baz", variable: word, value: "bazeball strike");
  add-separator(m2);
  add-checkbutton(m2, label: "toggle menus", variable: switch,
		  command: method ()
			     let state
			       = if (switch.value) "normal"
				 else "disabled" end if;
			     configure(b1, state: state);
			     configure(b2, state: state);
			   end method);

  let b4 = make(<entry>, width: 20, textvariable: word,
		      in: f1, relief: "sunken",
		expand: #f, side: "right");

  widgets := vector(pair("menu frame", f1), pair("list frame", f2),
		    pair("list }box", l1), pair("scroll bar", s1),
		    pair("file menu", b1), pair("print menu", b2),
		    pair("variable menu", b3), pair("word entry", b4));
  apply(insert, l1, 0, map(head, widgets));

  make(<label>, text: "Select widgets to show bindings or configs",
       foreground: "White", background: "Black",
       expand: #f, fill: "x", side: "top");

  let m1 = make(<message>,
		text: "This is a demo application.  It does nothing "
		  "useful but employs a fascinating variety of widgets.  "
		  "Enjoy!",
		relief: "raised", fill: "x", side: "bottom");

  let s3 = make(<scale>, from: 1, to: 10, label: "aspect:",
		command: method (x) configure(m1, #"aspect",
					      100 * tk-as(<integer>, x));
			 end method,
		tickinterval: 1, fill: "x", orient: "horizontal", side: "top");
  format(*tty*, "Scale was at: %=\n", get-units(s3));
  set-units(s3, 4);

  add-command(m2, label: "reset aspect",
	      command: method ()
			 set-units(s3, 1);
			 set-units(s3, 3);
					do(print-a-config, configuration(m1));
		       end method);

  let c2 = make(<canvas>, before: m1, pack: #f);
  let t1 = make(<text>, before: m1, side: "top",
		relief: "sunken", fill: "both");
  create-window(c2, 0, 0, window: t1);
  let t1-insert = make(<text-mark>, in: t1, name: "insert");
  let string = read-as(<byte-string>, make(<file-stream>, name: "makefile"),
		       to-eof?: #t);
  insert(t1, t1-insert, string);

  map-window(*root-window*);

  block (return)
    while (#t)
      let input = read-line(*standard-input*);
      if (input = "quit")
	return();
      elseif (input = "break")
	break();
      elseif (input = "ps")
	let psfile
	  = make(<file-stream>, direction: #"output", name: "test.ps");
	write(postscript(c1), psfile);
	close(psfile);
      elseif (input = "test")
	format(*tty*, "|%s|\n", get-elements(b4, 2, end: 5));
	format(*tty*, "|%s|\n", get-elements(t1, 7, end: "12.end"));
	format(*tty*, "Rectangle coordinates: %=\n", item-coords(item2));
	for (i from 0 below 25)
	  move-item(item2, 2, 1);
	end for;
	for (i from 0 below 25)
	  move-item(item2, -1, -1);
	end for;
	format(*tty*, "New rectangle coordinates: %=\n", item-coords(item2));
      else
	put-tk-line(as(<string>, input))
      end if;
    end while;
  end block;
  put-tk-line("exit");
end method main;
