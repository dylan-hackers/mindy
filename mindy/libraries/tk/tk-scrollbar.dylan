module: tk

define class <scrollbar> (<window>) end class;

define-widget(<scrollbar>, "scrollbar",
	      #"activeforeground", #"command", #"orient", #"repeatdelay",
	      #"repeatinterval", #"width");

define method scroll
    (widget :: <window>, #rest rest, #key orient = "vertical", #all-keys)
 => (result :: <scrollbar>);
  let result = apply(make, <scrollbar>, rest);
  let widget-string = tk-as(<string>, widget);
  let axis
    = select (as-lowercase(tk-as(<string>, orient)) by \=)
	"vertical" =>
	  "y";
	"horizontal" =>
	  "x";
	otherwise =>
	  error("Orient: must be either \"vertical\" or \"horizontal\"");
      end select;
  put-tk-line(result, " configure -command {", widget-string, " ",
	       axis, "view }");
  put-tk-line(widget-string, " configure -", axis,
	       "scrollcommand {", result.path, " set }");
  result;
end method scroll;

define method get-units (bar :: <scrollbar>) => (#rest units :: <integer>);
  apply(values, map(curry(tk-as, <integer>),
		    parse-tk-list(call-tk-function(bar, " get"), depth: 1)));
end method get-units;

define method set-units
    (bar :: <scrollbar>, #rest Units) => (bar :: <scrollbar>);
  put-tk-line(bar.path, " set ", apply(join-tk-args, Units));
  bar;
end method set-units;
