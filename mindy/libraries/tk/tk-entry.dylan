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
// This file contains support for <entry>s.
//
//======================================================================

// An <entry> is a window which can be used to enter or edit a short string.
//
define class <entry> (<window>, <editable>) end class;

define-widget(<entry>, "entry",
	      #"exportselection", #"font", #"insertbackground",
	      #"insertborderwidth", #"insertofftime", #"insertontime",
	      #"insertwidth", #"xscrollcommand", #"selectbackground",
	      #"selectborderwidth", #"selectforeground", #"state",
	      #"textvariable", #"width", #"show", #"justify");

define method icursor (entry :: <entry>, index) => (entry :: <entry>);
  put-tk-line(entry, " icursor ", index);
  entry;
end method icursor;

define method xview (entry :: <entry>, index) => (entry :: <entry>);
  put-tk-line(entry, " xview ", index);
  entry;
end method xview;
  
define method get-all (entry :: <entry>) => (result :: <string>);
  tk-unquote(call-tk-function(entry, " get"));
end method get-all;

define method get-elements
    (widget :: <entry>, index, #key end: last) => (result :: <string>);
  let real-index = tk-as(<integer>, index);
  let real-end
    = if (last) tk-as(<integer>, last) else real-index + 1 end if;

  copy-sequence(widget.get-all, start: real-index, end: real-end);
end method get-elements;

