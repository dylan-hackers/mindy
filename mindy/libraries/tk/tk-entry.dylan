module: tk

// An <entry> is a window which can be used to enter or edit a short string.
//
define class <entry> (<window>, <editable>) end class;

define-widget(<entry>, "entry",
	      #"exportselection", #"font", #"insertbackground",
	      #"insertborderwidth", #"insertofftime", #"insertontime",
	      #"insertwidth", #"scrollcommand", #"selectbackground",
	      #"selectborderwidth", #"selectforeground", #"state",
	      #"textvariable", #"width");

define method icursor (entry :: <entry>, index) => (entry :: <entry>);
  put-tk-line(entry, " icursor ", index);
  entry;
end method icursor;

define method view (entry :: <entry>, index) => (entry :: <entry>);
  put-tk-line(entry, " view ", index);
  entry;
end method view;
  
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

