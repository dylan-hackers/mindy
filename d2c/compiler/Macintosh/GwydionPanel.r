// File: GwydionPanel.r
// Purpose: define necessary resources for panel
// Author: Gabor Greif
// Copyright: © 2000 Gabor Greif, all rights reserved.

#include "Types.r"
#include "CWPlugins.r"
#include "AEWideUserTermTypes.r"
#include "GwydionVersion.h"

#define NameForIDE "Dylan Compiler"
#define NameForUSER "Dylan Language"

resource 'vers' (1, NameForIDE" Vers")
{
	2,
	0x33,
	DevelopmentStage,
	DevelopmentVersion,
	verUS,
	"2.3.3" DevelopmentState,
	"v2.3.3" DevelopmentState ", a Gwydion Dylanª component"
};

resource 'vers' (2, NameForIDE" Prod")
{
	2,
	0x33,
	DevelopmentStage,
	DevelopmentVersion,
	verUS,
	"2.3.3" DevelopmentState,
	"© 2000, Gwydion Dylan maintainers"
};

resource 'Flag' (128, NameForIDE)
{
	kCurrentResourceVersion,
	Panel {
		VERSION7API {
			usesStrictAPI,
			doesntSupportByteSwapping,
			supportsReadWrite,
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
			reserved,
			FrontEnd,
			Pro5,
			1,	// preference data version
			target
		}
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

