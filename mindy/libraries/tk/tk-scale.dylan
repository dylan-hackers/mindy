module: tk

define class <scale> (<window>) end class;

define-widget(<scale>, "scale",
	      #"activeforeground", #"command", #"font", #"from", #"label",
	      #"length", #"orient", #"showvalue", #"sliderforeground",
	      #"sliderlength", #"state", #"tickinterval", #"to", #"width");

define method get-units (scale :: <scale>) => (#rest units :: <integer>);
  apply(values, map(curry(tk-as, <integer>),
		    parse-tk-list(call-tk-function(scale, " get"),
				   depth: 1)));
end method get-units;

define method set-units
    (scale :: <scale>, #rest Units) => (scale :: <scale>);
  put-tk-line(scale.path, " set ", apply(join-tk-args, Units));
  scale;
end method set-units;

