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
// This file contains support for <listbox>s.
//
//======================================================================

define class <listbox> (<window>, <editable>) end class;

define-widget(<listbox>, "listbox",
	      #"exportselection", #"font", #"width", #"height",
	      #"selectbackground", #"selectborderwidth", #"selectforeground",
	      #"setgrid", #"xscrollcommand", #"yscrollcommand",
	      #"selectmode");

define method current-selection(listbox :: <listbox>, #rest rest) =>
    (indices :: <sequence>);
  map(curry(tk-as, <integer>),
      parse-tk-list(call-tk-function(listbox.path, " curselection")));
end method;

define method nearest(listbox :: <listbox>, y-coord) => index :: <integer>;
  tk-as(<integer>, call-tk-function(listbox.path, " nearest ",
					tk-as(<string>, y-coord)));
end method nearest;

define method size(listbox :: <listbox>) => result :: <integer>;
  tk-as(<integer>, call-tk-function(listbox.path, " size"));
end method size;

define method xview(listbox :: <listbox>, index) => listbox :: <listbox>;
  put-tk-line(listbox.path, " xview ", tk-as(<string>, index));
  listbox;
end method xview;

define method yview(listbox :: <listbox>, index) => listbox :: <listbox>;
  put-tk-line(listbox.path, " yview ", tk-as(<string>, index));
  listbox;
end method yview;

define method get-elements
    (widget :: <listbox>, index, #key end: last) => (result :: <string>);
  let real-index = tk-as(<integer>, index);
  let real-end
    = if (last) tk-as(<integer>, last) else real-index + 1 end if;

  let buffer :: <stream> = make(<byte-string-stream>);
  for (i from real-index below real-end,
       newline = #f then #t)
    if (newline) write(buffer, "\n") end if;
    write(buffer, tk-unquote(call-tk-function(widget, " get ", i)));
  end for;
  stream-contents(buffer);
end method get-elements;
  
define method get-all (listbox :: <listbox>) => (result :: <string>);
  get-elements(listbox, 0, end: listbox.size);
end method get-all;

define method see (listbox :: <listbox>, index :: <integer>);
  put-tk-line(listbox, " see ", index);
end method see;

define method selection-anchor-setter (listbox :: <listbox>, index)
  => listb :: <listbox>;
  put-tk-line(listbox, " selection anchor ", index);
  listbox;
end method selection-anchor-setter;

define method clear-selection (listbox :: <listbox>, first :: <integer>,
			       #rest last)
  put-tk-line(listbox, " selection clear ", apply(join-tk-args, first, last));
end method clear-selection;

define method selection-includes? (listbox :: <listbox>, index)
 => in-selection? :: <boolean>;
  tk-as(<boolean>, call-tk-function(listbox, " selection includes ", index));
end method selection-includes?;

define method set-selection (listbox :: <listbox>, first :: <integer>,
			     #rest last);
  put-tk-line(listbox, " selection set ", apply(join-tk-args, first, last));
end method set-selection;

