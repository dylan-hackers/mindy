// File: GwydionPanel.r
// Purpose: define necessary resources for CW preference panel
// RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/GwydionPanel.r,v 1.2 2002/04/03 23:11:55 gabor Exp $
// Author: Gabor Greif <gabor@mac.com>

//======================================================================
//
// Copyright (c) 2000, 2001, 2002  Gwydion Dylan Maintainers
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

