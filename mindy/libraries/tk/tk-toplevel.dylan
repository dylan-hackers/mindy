module: tk

// A <toplevel> is a separate window which acts as a container for other
// windows.
//
define class <toplevel> (<window>) end class;

define-widget(<toplevel>, "toplevel",
	      #"geometry", #"screen");

// <Toplevel>s are at the top level, so it is meaningless to pack them.
//
define method pack
    (window :: <toplevel>, #key, #all-keys) => (result :: <toplevel>);
  window;
end method pack;
