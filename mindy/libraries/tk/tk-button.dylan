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
// This file contains support for buttons and "buttonlike" objects such as
// menubuttons, checkbuttons, etc.
//
//======================================================================

define abstract class <buttonlike> (<window>) end class;

// Removed for Tk 4.0
define method activate (button :: <buttonlike>) => (button :: <buttonlike>);
  // put-tk-line(button, " activate");
  error("Activate no longer works in Tk 4.0.\nUse configure with the state keyword instead");
end method activate;

define method deactivate (button :: <buttonlike>) => (button :: <buttonlike>);
  // put-tk-line(button, " deactivate");
  error("Deactivate no linger works in Tk 4.0.\nUse configure with the state keyword instead");
end method deactivate;

define class <button> (<buttonlike>) end class;

define-widget(<button>, "button",
	      #"activebackground", #"activeforeground", #"bitmap",
	      #"command", #"disabledforeground", #"font", #"height",
	      #"state", #"text", #"textvariable", #"width", #"image",
	      #"justify", #"underline", #"wraplength");

define method flash (button :: <button>) => (button :: <button>);
  put-tk-line(button, " flash");
  button;
end method;

define method invoke (button :: <button>) => (button :: <button>);
  put-tk-line(button, " invoke");
  button;
end method;

define abstract class <valued-button> (<object>) end class;

define method select-value
    (button :: <valued-button>) => (button :: <valued-button>);
  put-tk-line(button, " select");
end method select-value;

define method deselect-value
    (button :: <valued-button>) => (button :: <valued-button>);
  put-tk-line(button, " deselect");
end method deselect-value;

define class <checkbutton> (<button>, <valued-button>)
  slot variable :: <active-variable>;
  required keyword variable:;
end class;

define-widget(<checkbutton>, "checkbutton",
	      #"activebackground", #"activeforeground", #"bitmap", #"command",
	      #"disabledforeground", #"font", #"height", #"offvalue",
	      #"onvalue", #"state", #"text", #"textvariable",
	      #"variable", #"width", #"selectcolor", #"indicatoron",
	      #"image", #"justify", #"wraplength", #"selectimage",
	      #"underline");

define method toggle-value
    (button :: <checkbutton>) => (button :: <checkbutton>);
  put-tk-line(button, " toggle");
end method toggle-value;

define class <radiobutton> (<button>, <valued-button>)
  slot variable :: <active-variable>;
  required keyword variable:;
  required keyword value:;
end class;

define-widget(<radiobutton>, "radiobutton",
	      #"activebackground", #"activeforeground", #"bitmap", #"command",
	      #"disabledforeground", #"font", #"height",  #"state", #"text",
	      #"textvariable", #"value", #"variable", #"width",
	      #"selectcolor", #"indicatoron", #"image", #"justify",
	      #"wraplength", #"underline", #"selectimage");

define class <menubutton> (<buttonlike>) end class;

define-widget(<menubutton>, "menubutton",
	      #"activebackground", #"activeforeground", #"bitmap",
	      #"disabledforeground", #"font", #"height", #"menu", #"state",
	      #"text", #"textvariable", #"underline", #"width", #"image",
	      #"justify", #"wraplength", #"indicatoron");
