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
		"About This ApplicationÉ", noIcon, noKey, noMark, plain,
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


/* Remove. Change to Standard Dialog. */

data 'ALRT' (32000) {
	$"0064 0064 00D2 0140 7D00 5555 300A"                 /* .d.d.Ò.@}.UU0Â */
};

data 'DITL' (32000) {
	$"0002 0000 0000 0050 0096 0064 00D0 0402"            /* .......P.–.d.Ð.. */
	$"4F4B 0000 0000 0014 001E 0024 00C5 8819"            /* OK.........$.Åˆ. */
	$"5369 6D70 6C65 2044 796C 616E 2041 7070"            /* Simple Dylan App */
	$"6C69 6361 7469 6F6E 2E00 0000 0000 0032"            /* lication.......2 */
	$"0032 0042 00AB 880E 4D61 6465 2075 7369"            /* .2.B.«ˆ.Made usi */
	$"6E67 2064 3263"                                     /* ng d2c */
};
