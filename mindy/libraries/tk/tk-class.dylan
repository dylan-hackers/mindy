module: tk
author: Russ Schaaf (rsbe@andrew.cmu.edu)

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// This file contains methods with operate on, or return, Dylan/Tk classes

// Convert a tk class to a Dylan class and vice-versa
//
define method get-class-from-name (name :: <string>) => (class :: <class>);
  select (name)
    "Frame" => <frame>;
    "Button" => <button>;
    "Checkbutton" => <checkbutton>;
    "Menubutton" => <menubutton>;
    "Radiobutton" => <radiobutton>;
    "Canvas" => <canvas>;
    "Entry" => <entry>;
    "Label" => <label>;
    "Listbox" => <listbox>;
    "Menu" => <menu>;
    "Message" => <message>;
    "Scale" => <scale>;
    "Scrollbar" => <scrollbar>;
    "Text" => <text>;
    "Toplevel" => <toplevel>;
  end select;
end method get-class-from-name;

define method get-name-from-class (class :: <class>) => (name :: <string>);
  select (class)
    <frame> => "Frame";
    <button> => "Button";
    <checkbutton> => "Checkbutton";
    <menubutton> => "Menubutton";
    <radiobutton> => "Radiobutton";
    <canvas> => "Canvas";
    <entry> => "Entry";
    <label> => "Label";
    <listbox> => "Listbox";
    <menu> => "Menu";
    <message> => "Message";
    <scale> => "Scale";
    <scrollbar> => "Scrollbar";
    <text> => "Text";
    <toplevel> => "Toplevel";
  end select;
end method get-name-from-class;

// Some methods on \=, for <window>s and <string>s
//
define method \= (a :: <window>, b :: <window>) => eq? :: <boolean>;
  a.path = b.path;
end method \=;

define method \= (window :: <window>, string :: <string>) => eq? :: <boolean>;
  window.path = string;
end method \=;

define method \= (string :: <string>, window :: <window>) => eq? :: <boolean>;
  string = window.path;
end method \=;

// Binding method for classes
//
define method bind (class :: <class>, event :: <string>, command) => ();
  put-tk-line("bind ", get-name-from-class(class), " ", event, " {",
	      command, "}");
end method bind;

define method get-binding (class :: <class>, event) => (result :: <string>);
  call-tk-function("bind ", get-name-from-class(class), " ", event);
end method get-binding;

define method get-bindings (class :: <class>) => (result :: <sequence>);
  map(method (event)
	pair(event, get-binding(get-name-from-class(class), event))
      end method,
      parse-tk-list(call-tk-function("bind ", get-name-from-class(class)),
		    depth: 1));
end method get-bindings;

// Methods to get a dylan class from a window name
//
define method tk-class (widget :: <window>) => (class :: <class>);
    get-class-from-name(call-tk-function("winfo class ", window));
end method tk-class;

define method tk-class (path :: <string>) => (class :: <class>);
    get-class-from-name(call-tk-function("winfo class ", path));
end method tk-class;