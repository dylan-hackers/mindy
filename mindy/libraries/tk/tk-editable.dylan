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
// This file provides general support for "editable" objects such as <text>,
// <listbox>, or <entry>.
//
//======================================================================

// Mixin for windows which contain one or more strings which can be
// inserted and modified.
//
define abstract class <editable> (<object>) end class;

// Hack -- to avoid an inconsistent CPL, <editable> is a subclass of
// <object> rather than <window>, and so this generic has a
// not-so-clean signature
//
define generic delete
    (widget :: type-union(<window>, <editable>), index, #key end:) 
 => (widget :: <window>);

define method delete
    (widget :: <editable>, index, #key end: last) => (widget :: <window>);
  put-tk-line(widget, " delete ", index, " ", if (last) last else "" end if);
  widget;
end method delete;

define generic get-all
    (widget :: <editable>) => (result :: <string>);

define generic get-elements
    (widget :: <editable>, index, #key end:) => (result :: <string>);

define generic insert
    (widget :: <editable>, index, #rest elements) => (widget :: <editable>);

define method insert
    (widget :: <editable>, index, #rest elements) => (widget :: <editable>);
  put-tk-line(widget, " insert ", tk-as(<string>, index),
	       apply(concatenate,
		     map(method (str)
			   concatenate(" \"", tk-quote(str), "\"")
			 end method, elements)));
  widget;
end method insert;

define generic scan-mark
    (widget :: type-union(<window>, <editable>), #rest coords) 
 => (widget :: <window>);

define method scan-mark
    (widget :: <editable>, #rest coords) => (widget :: <window>);
  put-tk-line(widget, " scan mark ", apply(join-tk-args, coords));
  widget;
end method scan-mark;

define generic scan-dragto
    (widget :: type-union(<window>, <editable>), #rest coords) 
 => (widget :: <window>);

define method scan-dragto
    (widget :: <editable>, #rest coords) => (widget :: <window>);
  put-tk-line(widget, " scan dragto ", apply(join-tk-args, coords));
  widget;
end method scan-dragto;

define generic select-adjust
    (widget :: type-union(<window>, <editable>), index) 
 => (widget :: <window>);

define method select-adjust
    (widget :: <editable>, index) => (widget :: <window>);
  put-tk-line(widget, " select adjust ", index);
  widget;
end method select-adjust;

define generic select-clear (widget :: <window>) => (widget :: <window>);

define method select-clear (widget :: <window>) => (widget :: <window>);
  put-tk-line(widget, " select clear");
  widget;
end method select-clear;

define generic select-from
    (widget :: type-union(<window>, <editable>), index) 
 => (widget :: <window>);

define method select-from
    (widget :: <editable>, index) => (widget :: <window>);
  put-tk-line(widget, " select from ", index);
  widget;
end method select-from;

define generic select-to
    (widget :: type-union(<window>, <editable>), index) 
 => (widget :: <window>);

define method select-to
    (widget :: <editable>, index) => (widget :: <window>);
  put-tk-line(widget, " select to ", index);
  widget;
end method select-to;

