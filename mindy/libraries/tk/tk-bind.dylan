module: tk
author: Robert Stockton (rgs@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//
// This file contains support for binding events to windows (i.e. the wish
// "bind" command.
//
//======================================================================

define method bind
    (window :: <window>, event :: <string>, command)
 => (window :: <window>);
  put-tk-line("bind ", window, " ", event, " {", command, "}");
  window;
end method bind;

define method get-binding (window :: <window>, event) => (result :: <string>);
  call-tk-function("bind ", window, " ", event);
end method;

define method get-bindings (window :: <window>) => (result :: <sequence>);
  map(method (event) pair(event, get-binding(window, event)) end method,
      parse-tk-list(call-tk-function("bind ", window), depth: 1));
end method get-bindings;

// Binding methods for tags
//
define method bind (tag :: <string>, event :: <string>, command) => ();
  put-tk-line("bind ", tag, " ", event, " {", command, "}");
end method bind;

define method get-binding (tag :: <string>, event) => (result :: <string>);
  call-tk-function("bind ", tag, " ", event);
end method get-binding;

define method get-bindings (tag :: <string>) => (result :: <sequence>);
  map(method (event) pair(event, get-binding(tag, event)) end method,
      parse-tk-list(call-tk-function("bind ", tag), depth: 1));
end method get-bindings;

// Two utilities for the new binding call conventions

define method tk-continue () => ();
  put-tk-line("continue");
end method tk-continue;

define method tk-break () => ();
  put-tk-line("break");
end method tk-break;

// New stuff for the bindtags command
//

define method binding-call-order (window :: <window>) =>
    (result :: <sequence>);
  parse-tk-list(call-tk-function("bindtags ", window), depth: 1);
end method binding-call-order;

define method binding-call-order-setter (seq :: <sequence>, window :: <window>)
  => (seq :: <sequence>);
  let string-seq = map(curry(tk-as, <string>), seq);
  put-tk-line("bindtags ", window, " { ",
	      reduce1(method(x,y) concatenate(x, " ", y) end, string-seq),
	      " } ");
  seq;
end method binding-call-order-setter;

define method add-binding-tag
    (window :: <window>, tag :: <string>, #key position = 1)
 => (window :: <window>);
  let bindtags = window.binding-call-order;
  let front = copy-sequence(bindtags, end: position);
  let back = copy-sequence(bindtags, start: position);
  window.binding-call-order := concatenate(front, list(tag), back);
  window;
end method add-binding-tag;
  
