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
// This file contains support for <scrollbar>s.
//
//======================================================================

define class <scrollbar> (<window>) end class;

define-widget(<scrollbar>, "scrollbar",
	      #"activebackground", #"command", #"orient", #"repeatdelay",
	      #"repeatinterval", #"width", #"troughcolor", #"jump",
	      #"activerelief", #"elementborderwidth");

define method scroll
    (widget :: <window>, #rest rest, #key orient = "vertical", #all-keys)
 => (result :: <scrollbar>);
  let result = apply(make, <scrollbar>, rest);
  let widget-string = tk-as(<string>, widget);
  let axis
    = select (as-lowercase(tk-as(<string>, orient)) by \=)
	"vertical" =>
	  "y";
	"horizontal" =>
	  "x";
	otherwise =>
	  error("Orient: must be either \"vertical\" or \"horizontal\"");
      end select;
  put-tk-line(result, " configure -command {", widget-string, " ",
	       axis, "view }");
  put-tk-line(widget-string, " configure -", axis,
	       "scrollcommand {", result.path, " set }");
  result;
end method scroll;

define method get-units (bar :: <scrollbar>) => (#rest units :: <integer>);
  apply(values, map(curry(tk-as, <integer>),
		    parse-tk-list(call-tk-function(bar, " get"), depth: 1)));
end method get-units;

define method set-units
    (bar :: <scrollbar>, #rest Units) => (bar :: <scrollbar>);
  put-tk-line(bar.path, " set ", apply(join-tk-args, Units));
  bar;
end method set-units;
