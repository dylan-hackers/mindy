module: tk

// A <frame> is a window which acts as a container for other windows.
//
define class <frame> (<window>) end class;

define-widget(<frame>, "frame",
	      #"geometry", #"height", #"width");
