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
