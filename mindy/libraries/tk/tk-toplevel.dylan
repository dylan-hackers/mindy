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
// This file contains support for <toplevel>s.
//
//======================================================================

// A <toplevel> is a separate window which acts as a container for other
// windows.
//
define class <toplevel> (<window>) end class;

define-widget(<toplevel>, "toplevel",
	      #"width", #"height", #"screen", #"colormap", #"visual");

// <Toplevel>s are at the top level, so it is meaningless to pack them.
//
define method pack
    (window :: <toplevel>, #key, #all-keys) => (result :: <toplevel>);
  window;
end method pack;

define method tk-dialog (window :: <toplevel>, title :: <string>,
			 text :: <string>, bitmap :: <string>,
			 default :: <integer>, #rest buttons)
 => button :: <integer>;
  tk-as(<integer>, call-tk-function("tk_dialog ",
				    apply(join-tk-args, window, title,
					  text, bitmap, default, buttons)));
end method tk-dialog;