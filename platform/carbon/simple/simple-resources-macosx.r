/* Carbon Application */ 

#include"Carbon.r"

data 'carb' (0) {
};

/* Application Resources */

resource 'MBAR' (32000) {
	{	
		32000,
		32001,
		32002
	}
};

resource 'MENU' (32000) {
	32000,
	63,
	0x7FFFFFFD,
	enabled,
	apple,
	{	
		"About This Application…", noIcon, noKey, noMark, plain,
		"-", noIcon, noKey, noMark, plain
	}
};

resource 'MENU' (32001) {
	32001,
	63,
	0x7FFFFFFD,
	enabled,
	"File",
	{
		"Close", noIcon, "W", noMark, plain,
	}
};

resource 'MENU' (32002) {
	32002,
	63,
	0x7FFFFFFD,
	enabled,
	"Edit",
	{	
                "Undo", noicon, 			"Z", nomark, plain;
		"-", noicon,		 		nokey, nomark, plain;
		"Cut", noicon, 				"X", nomark, plain;
		"Copy", noicon, 			"C", nomark, plain;
		"Paste", noicon, 			"V", nomark, plain;
		"Clear", noicon, 			nokey, nomark, plain;
		"-", noicon,		 		nokey, nomark, plain;
		"Select All", noicon, 			nokey, nomark, plain;
	}
};
