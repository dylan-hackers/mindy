module: tk

define class <scale> (<window>) end class;

define-widget(<scale>, "scale",
	      #"activeforeground", #"command", #"font", #"from", #"label",
	      #"length", #"orient", #"showvalue", #"sliderforeground",
	      #"sliderlength", #"state", #"tickinterval", #"to", #"width");

define method get-value (scale :: <scale>) => (value :: <integer>);
  tk-as(<integer>, call-tk-function(scale, " get"));
end method get-value;

define method set-value (scale :: <scale>, value) => (scale :: <scale>);
  put-tk-line(scale.path, " set ", value);
  scale;
end method set-value;

