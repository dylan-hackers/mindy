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
// This file contains support for <menu>s, and the entries that go in them. 
//
//======================================================================

define class <menu> (<window>) end class;

define-widget(<menu>, "menu",
	      #"activebackground", #"activeborderwidth", #"activeforeground",
	      #"disabledforeground", #"font", #"postcommand", #"selectcolor",
	      #"tearoff");

// <menu>s are independent windows, so it is meaningless to pack them.
//
define method pack
    (window :: <menu>, #key, #all-keys) => (result :: <menu>);
  window;
end method pack;

define method activate-entry
    (menu :: <menu>, index :: <object>) => (menu :: <menu>);
  put-tk-line(menu, " activate ", index);
  menu;
end method activate-entry;

define method delete
    (widget :: <menu>, index, #key end: last) => (widget :: <menu>);
  put-tk-line(widget, " delete ", index, " ", if (last) last else "" end if);
  widget;
end method delete;

// Removed for tk 4.0
define method disable-entry
    (menu :: <menu>, index :: <object>) => (menu :: <menu>);
/*  put-tk-line(menu, " disable ", index);
    menu;
*/
  error("Disable-entry has been removed from Tk 4.0.\n  Use configure with the state keyword instead");
end method disable-entry;

define method enable-entry
    (menu :: <menu>, index :: <object>) => (menu :: <menu>);
/*  put-tk-line(menu, " enable ", index);
    menu;
*/
  error("Enable-entry has been removed from Tk 4.0.\n Use configure with the state keyword instead.");
end method enable-entry;

define method configure-entry
    (menu :: <menu>, index :: <object>, #rest options) => (menu :: <menu>);
  apply(put-tk-line, menu, " entryconfigure ", index,
	std-options(#[#"accelerator", #"activebackground", #"label", #"state",
		      #"command", #"menu", #"onvalue", #"selectcolor",
		      #"offvalue", #"value", #"bitmap", #"image", #"variable",
		      #"indicatoron", #"selectimage", #"underline",
		      #"activeforeground", #"font"],
		    #f, options));
  menu;
end method configure-entry;

define method entry-configuration
    (menu :: <menu>, index :: <object>) => (result :: <sequence>);
  let string = call-tk-function(menu, " entryconfigure ", index);
  parse-tk-list(string, depth: 2);
end method entry-configuration;

define method invoke-entry
    (menu :: <menu>, index :: <object>) => (result :: <menu>);
  put-tk-line(menu, " invoke ", index);
  menu;
end method invoke-entry;

define method post (menu :: <menu>, x, y);
  put-tk-line(menu, " post ", x, " ", y);
end method post;

define method unpost (menu :: <menu>);
  put-tk-line(menu, " unpost ");
end method unpost;

define method yposition-entry
    (menu :: <menu>, index :: <object>) => (result :: <integer>);  
  tk-as(<integer>, call-tk-function(menu, " invoke ", index));
end method yposition-entry;

define method post-cascade (menu :: <menu>, index :: <integer>);
  put-tk-line(menu, " postcascade ", index);
end method post-cascade;

define method add-command
    (menu :: <menu>, #next next, #rest rest,
     #key state, command, label, #all-keys);
  apply(put-tk-line, menu, " add command ",
	std-options(#[#"accelerator", #"activebackground", #"label", #"state",
		      #"command", #"bitmap", #"image", #"underline",
		      #"activeforeground", #"font"],
		    #f, rest));
end method add-command;

define method add-checkbutton
    (menu :: <menu>, #next next, #rest rest,
     #key variable, label, #all-keys);
  if (~variable)
    error("You must specify a 'variable:' for menu checkbuttons");
  end if;
  apply(put-tk-line, menu, " add checkbutton ",
	std-options(#[#"accelerator", #"activebackground", #"variable",
			#"command", #"onvalue", #"offvalue", #"label",
		      #"bitmap", #"image", #"selectimage", #"state"],
		    #f, rest));
end method add-checkbutton;

define method add-radiobutton
    (menu :: <menu>, #next next, #rest rest,
     #key variable, value, label, #all-keys);
  if (~variable)
    error("You must specify a 'variable:' for menu radiobuttons");
  end if;
  apply(put-tk-line, menu, " add radiobutton ",
	std-options(#[#"accelerator", #"activebackground", #"command",
			#"variable", #"label", #"value"],
		    #f, rest));
end method add-radiobutton;

define method add-cascade
    (menu :: <menu>, #rest rest, #key menu: sub-menu, label, #all-keys);
  if (~menu)
    error("You must specify a 'menu:' for menu cascades");
  end if;
  apply(put-tk-line, menu, " add cascade ",
	std-options(#[#"accelerator", #"activebackground", #"label", #"menu"],
		    #f, rest));
end method add-cascade;

define method add-separator (menu :: <menu>, #rest rest, #key , #all-keys);
  apply(put-tk-line, menu, " add separator ",
	std-options(#[],
		    #f, rest));
end method add-separator;
