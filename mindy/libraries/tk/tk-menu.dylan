module: tk

define class <menu> (<window>) end class;

define-widget(<menu>, "menu",
	      #"activebackground", #"activeborderwidth", #"activeforeground",
	      #"disabledforeground", #"font", #"postcommand", #"selector");

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

define method disable-entry
    (menu :: <menu>, index :: <object>) => (menu :: <menu>);
  put-tk-line(menu, " disable ", index);
  menu;
end method disable-entry;

define method enable-entry
    (menu :: <menu>, index :: <object>) => (menu :: <menu>);
  put-tk-line(menu, " enable ", index);
  menu;
end method enable-entry;

define method configure-entry
    (menu :: <menu>, index :: <object>, #rest options) => (menu :: <menu>);
  apply(put-tk-line, menu, " entryconfigure ", index,
	std-options(#[#"accelerator", #"activebackground", #"label", #"state",
			#"command", #"label", #"menu", #"onvalue",
			#"offvalue", #"value"],
		    #f, options));
end method configure-entry;

define method entry-configuration
    (menu :: <menu>, index :: <object>) => (result :: <sequence>);
  let string = call-tk-function(menu, " entryconfigure ", index);
  parse-tk-list(string, depth: 2);
end method entry-configuration;

define method invoke-entry
    (menu :: <menu>, index :: <object>) => (result :: <menu>);
  put-tk-line(menu, " invoke ", index);
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

define method add-command
    (menu :: <menu>, #next next, #rest rest,
     #key state, command, label, #all-keys);
  apply(put-tk-line, menu, " add command ",
	std-options(#[#"accelerator", #"activebackground", #"label", #"state",
			#"command", #"label"],
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
			#"command", #"onvalue", #"offvalue", #"label"],
		    #f, rest));
end method add-checkbutton;

define method add-radiobutton
    (menu :: <menu>, #next next, #rest rest,
     #key variable, value, label, #all-keys);
  if (~variable)
    error("You must specify a 'variable:' for menu radiobuttons");
  end if;
  apply(put-tk-line, menu, " add radiobutton ",
	std-options(#[#"accelerator", #"activebackground", #"variable",
			#"label", #"value"],
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
