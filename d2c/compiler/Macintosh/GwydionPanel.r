// File: GwydionPanel.r
// Purpose: define necessary resources for CW preference panel
// Author: Gabor Greif <gabor@mac.com>

//======================================================================
//
// Copyright (c) 2000 - 2005  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

#include "Types.r"
#include "CWPlugins.r"
#include "AEWideUserTermTypes.r"
#include "GwydionVersion.h"

#define NameForIDE "Dylan Compiler"
#define NameForUSER "Dylan Language"

#define VERSION7API 7

resource 'vers' (1, NameForIDE" Vers")
{
	CodebaseMajor,
	CodebaseMinor,
	DevelopmentStage,
	DevelopmentVersion,
	verUS,
	CodebaseVersion DevelopmentState,
	"v" CodebaseVersion DevelopmentState ", a Gwydion Dylanª component"
};

resource 'vers' (2, NameForIDE" Prod")
{
	CodebaseMajor,
	CodebaseMinor,
	DevelopmentStage,
	DevelopmentVersion,
	verUS,
	CodebaseVersion DevelopmentState,
	"© " CopyrightYears ", Gwydion Dylan maintainers"
};

resource 'Flag' (128, NameForIDE)
{
	Panel {
			VERSION7API,
			usesStrictAPI,
			doesntSupportByteSwapping,
			supportsTextSettings,
			doesntUseCrossPlatformAPI,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			FrontEnd,
			newestAPIVersion,
			1,	// preference data version
			panelScopeTarget
		}
};

resource 'STR ' (128, NameForIDE" Name")
{
	NameForUSER
};


type 'pref'(128)
{
	integer	version;

	// some language settings
	boolean	implicitNextMethod, explicitNextMethod;
	align word;

	// some plugin settings
	boolean	defaultGen, groupForGen;
	boolean	noDebug, emitDebug;
	boolean	noCommandLine, emitCommandLine;
	align word;
	
	// lid overrides
	boolean	onlyLid, overrideLid;
	align word;

	// overridden values
	unsigned longint noIDs = 0xFFFFFFFF;
	
	integer currentRelativePathVersion = 1;
	byte AbsolutePath, ProjectPath, CompilerPath, SystemPath, RootPath; //origin,
	byte GeneralFormat, MacFormat, WindozeFormat, UnixFormat; //format,
	cstring[256];
	cstring[512];

	pstring;	// generatedGroup
	align word;
};

resource 'pref' (128, NameForIDE)
{
	1,
	implicitNextMethod,
	
	defaultGen,
	noDebug,
	emitCommandLine,
	
	onlyLid,
	noIDs,
	
	currentRelativePathVersion, ProjectPath, MacFormat,
	"",
	":",
	
	"Generated"
};

resource 'aete' (0, "English") {
	0x1,
	0x0,
	english,
	roman,
	{	/* array Suites: 1 elements */
		/* [1] */
		"Dylan Language Suite",
		"Settings supported by the Gwydion Dylan PPC compiler.",
		'DYLª',
		1,
		1,
		{	/* array Events: 0 elements */
		},
		{	/* array Classes: 1 elements */
			/* [1] */
			"Dylan Compiler",
			'DYLª',
			"Dylan Compiler options",
			{	/* array Properties: 8 elements */
				/* [1] */
				"implicit next method",
				'DY01',
				typeBoolean,
				"Implicitly define the next-method parameter.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [2] */
				"override lid",
				'DY02',
				typeBoolean,
				"Override .lid file options.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [3] */
				"unique id base",
				'DY03',
				typeLongInteger,
				"Base value for generating unique IDs.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [4] */
				"output in group",
				'DY11',
				typeBoolean,
				"Place generated .c files in specific project group.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [5] */
				"output group path",
				'DY12',
				typeChar,
				"Colon-separated path to project group where to place .c files.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [6] */
				"debug",
				'DY13',
				typeBoolean,
				"Emit debug code.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [7] */
				"command line dialog",
				'DY14',
				typeBoolean,
				"The main entry point will present a command-line arguments dialog.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readWrite, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				},
				/* [8] */
				"output in folder",
				'DY15',
				'RlPt',
				"Place generated .c files in this folder.",
				{	/* array: 1 elements */
					/* [1] */
					reserved, singleItem, notEnumerated, readOnly, enumsAreConstants, enumListCanRepeat, propertyIsValue, reserved, reserved, reserved, reserved, reserved, noApostrophe, notFeminine, notMasculine, singular
				}
			},
			{	/* array Elements: 0 elements */
			}
		},
		{	/* array ComparisonOps: 0 elements */
		},
		{	/* array Enumerations: 0 elements */
		}
	}
};



// Here come the PowerPlant-related resources
// they are in binary format, because they are
// better edited with a visual tool anyway

data 'RidL' (703, "Panel Items") {
       $"000A 0000 0001 0000 0004 0000 0002 0000 0005 0000 0006 6F74 7074 0000 000B 0000"                    /* .?....................otpt...... */
       $"000C 0000 0007 0000 0008"                                                                           /* .......... */
};

data 'PPob' (703, "Panel Items") {
       $"0002 6F62 6A64 0000 003C 7669 6577 0000 2710 01B8 0122 0101 0000 0000 0000 0000"                    /* ..objd...<view..'..?.".......... */
       $"0000 0000 0000 0000 FFFF FFFF 0000 01B8 0000 0122 0000 0000 0000 0000 0000 0010"                    /* ........????...?..."............ */
       $"0000 0010 0000 6265 6773 646F 706C 5042 6B67 6F62 6A64 0000 000A 6174 6368 0000"                    /* ......begsdoplPBkgobjd...?atch.. */
       $"032B 0101 6F62 6A64 0000 0060 7467 6278 0000 0000 01A5 0032 0101 0000 0000 0000"                    /* .+..objd...`tgbx.....?.2........ */
       $"0007 0000 0008 0000 0000 FFFF FFFF 0000 0000 0000 0000 0000 0000 0000 0000 0000"                    /* ..........????.................. */
       $"0001 0000 0001 0000 0000 0000 0000 0000 0000 0000 0000 0000 00A0 0086 0F44 524D"                    /* .........................?.?.DRM */
       $"2063 6F6E 666F 726D 616E 6365 6265 6773 6F62 6A64 0000 004F 426C 6C6E FFFF FFFE"                    /*  conformancebegsobjd...OBlln???? */
       $"0101 0042 596F 7520 6361 6E20 7370 6563 6966 7920 686F 7720 636C 6F73 656C 7920"                    /* ...BYou can specify how closely  */
       $"7468 6520 4479 6C61 6E20 5265 6665 7265 6E63 6520 4D61 6E75 616C 2069 7320 7472"                    /* the Dylan Reference Manual is tr */
       $"6163 6B65 642E 006F 626A 6400 0000 5463 6862 7800 0000 0100 B700 1201 0101 0100"                    /* acked..objd...Tchbx.....?....... */
       $"0000 0000 0600 0000 1700 0000 00FF FFFF FF00 0000 0000 0000 0100 0000 0000 0000"                    /* .............????............... */
       $"0201 7100 8F1D 696D 706C 6963 6974 6C79 2064 6566 696E 6520 6E65 7874 2D6D 6574"                    /* ..q.?.implicitly define next-met */
       $"686F 6462 6567 736F 626A 6400 0000 CA42 6C6C 6EFF FFFF FE01 0182 596F 7520 6361"                    /* hodbegsobjd...?Blln????..?You ca */
       $"6E20 7265 6665 7220 746F 206E 6578 742D 6D65 7468 6F64 2077 6974 686F 7574 206D"                    /* n refer to next-method without m */
       $"656E 7469 6F6E 696E 6720 6974 2061 7320 6120 236E 6578 7420 6172 6775 6D65 6E74"                    /* entioning it as a #next argument */
       $"2069 6E20 4746 206D 6574 686F 6473 2E20 546F 2072 6571 7569 7265 2065 7870 6C69"                    /*  in GF methods. To require expli */
       $"6369 7420 6D65 6E74 696F 6E2C 2063 6C69 636B 2074 6869 7320 626F 782E 3B54 6F20"                    /* cit mention, click this box.;To  */
       $"696D 706C 6963 6974 6C79 2064 6566 696E 6520 6E65 7874 2D6D 6574 686F 6420 7061"                    /* implicitly define next-method pa */
       $"7261 6D65 7465 722C 2063 6C69 636B 2074 6869 7320 626F 782E 0065 6E64 7365 6E64"                    /* rameter, click this box..endsend */
       $"736F 626A 6400 0000 6274 6762 7800 0000 0001 A500 8C01 0100 0000 0000 0000 0700"                    /* sobjd...btgbx.....?.?........... */
       $"0000 4100 0000 00FF FFFF FF00 0000 0000 0000 0000 0000 0000 0000 0000 0000 0100"                    /* ..A....????..................... */
       $"0000 0100 0000 0000 0000 0000 0000 0000 0000 0000 0000 A000 8611 5472 616E 736C"                    /* ......................?.?.Transl */
       $"6174 6F72 206F 7574 7075 7462 6567 736F 626A 6400 0000 3C42 6C6C 6EFF FFFF FE01"                    /* ator outputbegsobjd...<Blln????. */
       $"0100 2F59 6F75 2063 616E 2073 7065 6369 6679 206F 7074 696F 6E73 2066 6F72 2067"                    /* ../You can specify options for g */
       $"656E 6572 6174 6564 202E 6320 6669 6C65 732E 006F 626A 6400 0000 3B65 7478 7400"                    /* enerated .c files..objd...;etxt. */
       $"0000 0400 D600 1801 0100 0000 0000 0000 C400 0000 1100 0000 00FF FFFF FF00 0000"                    /* ....?...........?........????... */
       $"0000 0000 0000 0000 0000 0000 0001 1000 8F00 00FF 2003 6265 6773 6F62 6A64 0000"                    /* ................?..? .begsobjd.. */
       $"008D 426C 6C6E FFFF FFFE 0101 0037 436F 6C6F 6E2D 7365 7061 7261 7465 6420 6675"                    /* .?Blln????...7Colon-separated fu */
       $"6C6C 2070 6174 6820 6E61 6D65 2074 6F20 6375 7374 6F6D 2070 726F 6A65 6374 2067"                    /* ll path name to custom project g */
       $"726F 7570 2E49 4765 6E65 7261 7465 6420 2E63 2066 696C 6573 2077 696C 6C20 6265"                    /* roup.IGenerated .c files will be */
       $"2069 6E73 6572 7465 6420 696E 746F 2074 6865 2070 726F 6A65 6374 2069 6E20 6465"                    /*  inserted into the project in de */
       $"6661 756C 7420 6C6F 6361 7469 6F6E 2E65 6E64 736F 626A 6400 0000 5B63 6862 7800"                    /* fault location.endsobjd...[chbx. */
       $"0000 0200 B700 1201 0100 0101 0000 0000 0600 0000 1300 0000 00FF FFFF FF00 0000"                    /* ....?....................????... */
       $"0000 0000 0000 0000 0000 0000 0201 7100 8F24 506C 6163 6520 2E63 2066 696C 6573"                    /* ..............q.?$Place .c files */
       $"2069 6E20 6465 6661 756C 7420 6772 6F75 702C 206F 723A 6265 6773 6F62 6A64 0000"                    /*  in default group, or:begsobjd.. */
       $"00EB 426C 6C6E FFFF FFFE 0101 8246 7265 7368 6C79 2067 656E 6572 6174 6564 202E"                    /* .?Blln????..?Freshly generated . */
       $"6320 6669 6C65 7320 7769 6C6C 2062 6520 706C 6163 6564 2069 6E20 7468 6520 7361"                    /* c files will be placed in the sa */
       $"6D65 2070 726F 6A65 6374 2067 726F 7570 2061 7320 7468 6520 736F 7572 6365 2066"                    /* me project group as the source f */
       $"696C 652E 2054 6F20 7370 6563 6966 7920 6120 6375 7374 6F6D 2067 726F 7570 2C20"                    /* ile. To specify a custom group,  */
       $"636C 6963 6B20 7468 6973 2062 6F78 2E5C 546F 2070 6C61 6365 2066 7265 7368 6C79"                    /* click this box.\To place freshly */
       $"2067 656E 6572 6174 6564 202E 6320 6669 6C65 7320 696E 2074 6865 2073 616D 6520"                    /*  generated .c files in the same  */
       $"7072 6F6A 6563 7420 6772 6F75 7020 6173 2074 6865 2073 6F75 7263 652C 2063 6C69"                    /* project group as the source, cli */
       $"636B 2074 6869 7320 626F 782E 0065 6E64 736F 626A 6400 0000 4663 6862 7800 0000"                    /* ck this box..endsobjd...Fchbx... */
       $"0500 8600 1201 0100 0000 0000 0000 0600 0000 2F00 0000 00FF FFFF FF00 0000 0000"                    /* ..?.............../....????..... */
       $"0000 0000 0000 0000 0000 0201 7100 8F0F 456D 6974 2064 6562 7567 2063 6F64 6562"                    /* ............q.?.Emit debug codeb */
       $"6567 736F 626A 6400 0000 9E42 6C6C 6EFF FFFF FE01 016A 5468 6520 6765 6E65 7261"                    /* egsobjd...?Blln????..jThe genera */
       $"7465 6420 2E63 2066 696C 6573 2077 696C 6C20 6265 2069 6E73 7472 756D 656E 7465"                    /* ted .c files will be instrumente */
       $"6420 7769 7468 2064 6562 7567 6769 6E67 2063 6865 636B 732E 2054 6F20 6F6D 6974"                    /* d with debugging checks. To omit */
       $"2064 6562 7567 6769 6E67 2063 6F64 652C 2063 6C69 636B 2074 6869 7320 626F 782E"                    /*  debugging code, click this box. */
       $"2754 6F20 656D 6974 2064 6562 7567 6769 6E67 2063 6F64 652C 2063 6C69 636B 2074"                    /* 'To emit debugging code, click t */
       $"6869 7320 626F 782E 0065 6E64 736F 626A 6400 0000 5263 6862 7800 0000 0600 C800"                    /* his box..endsobjd...Rchbx.....?. */
       $"1201 0100 0000 0000 0000 C500 0000 3000 0000 00FF FFFF FF00 0000 0000 0000 0100"                    /* ..........?...0....????......... */
       $"0000 0000 0000 0201 7100 8F1B 5072 6573 656E 7420 636F 6D6D 616E 642D 6C69 6E65"                    /* ........q.?.Present command-line */
       $"2064 6961 6C6F 6762 6567 736F 626A 6400 0000 EE42 6C6C 6EFF FFFF FE01 0196 5468"                    /*  dialogbegsobjd...?Blln????..?Th */
       $"6520 6765 6E65 7261 7465 6420 6170 706C 6963 6174 696F 6E20 7769 6C6C 2070 7265"                    /* e generated application will pre */
       $"7365 6E74 2061 2064 6961 6C6F 6720 626F 7820 7072 6F6D 7074 696E 6720 666F 7220"                    /* sent a dialog box prompting for  */
       $"636F 6D6D 616E 642D 6C69 6E65 2061 7267 756D 656E 7473 2E20 546F 206F 6D69 7420"                    /* command-line arguments. To omit  */
       $"6765 6E65 7261 7469 6E67 2063 6F64 6520 7072 6573 656E 7469 6E67 2061 2064 6961"                    /* generating code presenting a dia */
       $"6C6F 672C 2063 6C69 636B 2074 6869 7320 626F 782E 4B54 6F20 656D 6974 2063 6F64"                    /* log, click this box.KTo emit cod */
       $"6520 7468 6174 2070 7265 7365 6E74 7320 6120 636F 6D6D 616E 642D 6C69 6E65 2061"                    /* e that presents a command-line a */
       $"7267 756D 656E 7473 2064 6961 6C6F 672C 2063 6C69 636B 2074 6869 7320 626F 782E"                    /* rguments dialog, click this box. */
       $"0065 6E64 736F 626A 6400 0000 5774 6762 786F 7470 7401 9C00 4101 0101 0101 0000"                    /* .endsobjd...Wtgbxotpt.?.A....... */
       $"0000 0500 0000 4600 0000 00FF FFFF FF00 0000 0000 0000 0000 0000 0000 0000 0000"                    /* ......F....????................. */
       $"0000 0100 0000 0100 0000 0000 0000 0000 0000 0000 0000 0000 0000 A000 8606 466F"                    /* ..........................?.?.Fo */
       $"6C64 6572 6265 6773 6F62 6A64 0000 0087 426C 6C6E FFFF FFFE 0101 007A 5468 6973"                    /* lderbegsobjd...?Blln????...zThis */
       $"2069 7320 7468 6520 6C6F 6361 7469 6F6E 2074 6861 7420 7468 6520 6F75 7470 7574"                    /*  is the location that the output */
       $"2062 696E 6172 7920 7769 6C6C 2062 6520 6372 6561 7465 642E 2020 546F 2063 6861"                    /*  binary will be created.  To cha */
       $"6E67 6520 7468 6520 6F75 7470 7574 206C 6F63 6174 696F 6E2C 2063 6C69 636B 2074"                    /* nge the output location, click t */
       $"6865 2022 4368 6F6F 7365 2E2E 2E22 2062 7574 746F 6E2E 006F 626A 6400 0000 5174"                    /* he "Choose..." button..objd...Qt */
       $"6762 7800 0000 0001 4A00 1201 0101 0101 0000 0000 0800 0000 1D00 0000 00FF FFFF"                    /* gbx.....J....................??? */
       $"FF00 0000 0000 0000 0000 0000 0000 0000 0000 0000 0100 0000 0100 0000 0000 0000"                    /* ?............................... */
       $"0000 0000 0000 0000 0000 0000 A400 0000 6265 6773 6F62 6A64 0000 0025 6361 7074"                    /* ............?...begsobjd...%capt */
       $"0000 000A 0146 0010 0101 0101 0101 0000 0002 0000 0001 0000 0000 FFFF FFFF 0000"                    /* ...?.F....................????.. */
       $"8F65 6E64 736F 626A 6400 0000 3F70 7573 6800 0000 0B00 4200 1201 0100 0101 0000"                    /* ?endsobjd...?push.....B......... */
       $"0001 5600 0000 1200 0000 00FF FFFF FF00 0000 0000 0000 0000 0000 0000 0000 0001"                    /* ..V........????................. */
       $"7000 8507 4368 6F6F 7365 C900 6F62 6A64 0000 003D 7075 7368 0000 000C 0042 0012"                    /* p.?.Choose?.objd...=push.....B.. */
       $"0101 0001 0100 0000 0156 0000 0028 0000 0000 FFFF FFFF 0000 0000 0000 0000 0000"                    /* .........V...(....????.......... */
       $"0000 0000 0000 0170 0085 0543 6C65 6172 0065 6E64 7365 6E64 736F 626A 6400 0000"                    /* .......p.?.Clear.endsendsobjd... */
       $"5E63 6267 6200 0000 0701 A500 4801 0100 0000 0000 0000 0700 0000 D000 0000 00FF"                    /* ^cbgb.....?.H.............?....? */
       $"FFFF FF00 0000 0000 0000 0000 0000 0000 0000 0000 0000 0100 0000 0100 0000 0000"                    /* ???............................. */
       $"0000 0000 0000 0000 0000 0000 0100 A100 860D 4C69 6420 6F76 6572 7269 6465 7362"                    /* ..............?.?.Lid overridesb */
       $"6567 736F 626A 6400 0000 E142 6C6C 6EFF FFFF FE01 018C 4F70 7469 6F6E 7320 7370"                    /* egsobjd...?Blln????..?Options sp */
       $"6563 6966 6965 6420 6865 7265 2074 616B 6520 7072 6563 6564 656E 6365 206F 7665"                    /* ecified here take precedence ove */
       $"7220 7468 6520 2E6C 6964 2066 696C 6520 2877 6869 6368 2063 616E 2062 6520 6D69"                    /* r the .lid file (which can be mi */
       $"7373 696E 6729 2E20 546F 2065 7863 6C75 7369 7665 6C79 2075 7365 206F 7074 696F"                    /* ssing). To exclusively use optio */
       $"6E73 2066 726F 6D20 2E6C 6964 2066 696C 652C 2063 6C69 636B 2074 6869 7320 626F"                    /* ns from .lid file, click this bo */
       $"782E 4854 6F20 7370 6563 6966 7920 6F70 7469 6F6E 7320 7468 6174 206F 7665 7272"                    /* x.HTo specify options that overr */
       $"6964 6520 7468 6F73 6520 696E 2074 6865 202E 6C69 6420 6669 6C65 2C20 636C 6963"                    /* ide those in the .lid file, clic */
       $"6B20 7468 6973 2062 6F78 2E00 6F62 6A64 0000 0034 6361 7074 0000 0000 004D 0014"                    /* k this box..objd...4capt.....M.. */
       $"0101 0000 0000 0000 000A 0000 001A 0000 0000 FFFF FFFF 0F55 6E69 7175 6520 4944"                    /* .........?........????.Unique ID */
       $"2062 6173 653A 008F 6F62 6A64 0000 003B 6574 7874 0000 0008 004B 0018 0101 0000"                    /*  base:.?objd...;etxt.....K...... */
       $"0000 0000 005B 0000 0016 0000 0000 FFFF FFFF 0000 0000 0000 0000 0000 0000 0000"                    /* .....[........????.............. */
       $"0000 0110 008F 0000 0A20 0162 6567 736F 626A 6400 0000 9B42 6C6C 6EFF FFFF FE01"                    /* .....?..? .begsobjd...?Blln????. */
       $"0100 6159 6F75 2063 616E 2073 7065 6369 6679 2061 2062 6173 6520 7661 6C75 6520"                    /* ..aYou can specify a base value  */
       $"6865 7265 2074 6861 7420 7769 6C6C 2062 6520 7573 6564 2066 6F72 2067 656E 6572"                    /* here that will be used for gener */
       $"6174 696E 6720 756E 6971 7565 2049 4473 2066 6F72 2073 6561 6C65 6420 636C 6173"                    /* ating unique IDs for sealed clas */
       $"7365 732E 2D54 6865 2062 6173 6520 4944 2077 696C 6C20 6265 2074 616B 656E 2066"                    /* ses.-The base ID will be taken f */
       $"726F 6D20 7468 6520 2E6C 6964 2066 696C 652E 656E 6473 656E 6473 656E 6473 656E"                    /* rom the .lid file.endsendsendsen */
       $"642E"                                                                                               /* d. */
};

data 'CTYP' (5151, "LBalloon") {
       $"0001 6F62 6A64 0000 001F 7670 7465 084C 4261 6C6C 6F6F 6E00 0000 426C 6C6E 6174"                    /* ..objd....vpte.LBalloon...Bllnat */
       $"6368 004B 004B 0000 0062 6567 736F 626A 6400 0000 1F70 7374 720B 4261 6C6C 6F6E"                    /* ch.K.K...begsobjd....pstr.Ballon */
       $"2074 6578 7400 0000 0001 0150 506F 6200 00FF 0000 6F62 6A64 0000 002B 7073 7472"                    /*  text......PPob..?..objd...+pstr */
       $"1742 616C 6C6F 6F6E 2074 6578 7420 6966 2063 6865 636B 6564 0000 0000 0101 5050"                    /* .Balloon text if checked......PP */
       $"6F62 0000 FF00 006F 626A 6400 0000 2970 7374 7215 4261 6C6C 6F6F 6E20 666F 7220"                    /* ob..?..objd...)pstr.Balloon for  */
       $"7465 7874 2069 7465 6D00 0000 0001 0150 506F 6200 00FF 0000 656E 6473 656E 642E"                    /* text item......PPob..?..endsend. */
};

