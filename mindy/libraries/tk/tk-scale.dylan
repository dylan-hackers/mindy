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
// This file contains support for <scale>s.
//
//======================================================================

define class <scale> (<window>) end class;

define-widget(<scale>, "scale",
	      #"troughcolor", #"command", #"font", #"from", #"label",
	      #"length", #"orient", #"showvalue", #"activebackground",
	      #"sliderlength", #"state", #"tickinterval", #"to", #"width",
	      #"bigincrement", #"repeatinterval", #"repeatdelay",
	      #"digits", #"resolution", #"variable");

define method get-value (scale :: <scale>) => (value :: <integer>);
  tk-as(<integer>, call-tk-function(scale, " get"));
end method get-value;

define method set-value (scale :: <scale>, value) => (scale :: <scale>);
  put-tk-line(scale.path, " set ", value);
  scale;
end method set-value;

