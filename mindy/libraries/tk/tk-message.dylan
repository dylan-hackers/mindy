module: tk

// A <message> is a window which contains a lengthy string or an icon.
// It will break it into lines according to the percentage aspect ratio
// specified by #"aspect".
//
define class <message> (<window>) end class;

define-widget(<message>, "message",
	      #"aspect", #"font", #"justify", #"text", #"textvariable",
	      #"width");
