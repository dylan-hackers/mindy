module: tk

define abstract class <buttonlike> (<window>) end class;

define method activate (button :: <buttonlike>) => (button :: <buttonlike>);
  put-tk-line(button, " activate");
end method activate;

define method deactivate (button :: <buttonlike>) => (button :: <buttonlike>);
  put-tk-line(button, " deactivate");
end method deactivate;

define class <button> (<buttonlike>) end class;

define-widget(<button>, "button",
	      #"activebackground", #"activeforeground", #"bitmap",
	      #"command", #"disabledforeground", #"font", #"height",
	      #"state", #"text", #"textvariable", #"width");

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
	      #"onvalue", #"selector", #"state", #"text", #"textvariable",
	      #"variable", #"width");

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
	      #"disabledforeground", #"font", #"height", #"selector",
	      #"state", #"text", #"textvariable", #"value", #"variable",
	      #"width");

define class <menubutton> (<buttonlike>) end class;

define-widget(<menubutton>, "menubutton",
	      #"activebackground", #"activeforeground", #"bitmap",
	      #"disabledforeground", #"font", #"height", #"menu", #"state",
	      #"text", #"textvariable", #"underline", #"width");
