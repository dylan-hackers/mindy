module: tk

// A <label> is a simple window which contains a string or an icon.  The value
// for a 'bitmap:' keyword should be a filename preceded by an '@'.
//
define class <label> (<window>) end class;

define-widget(<label>, "label",
	      #"bitmap", #"font", #"height", #"text", #"textvariable",
	      #"width");
