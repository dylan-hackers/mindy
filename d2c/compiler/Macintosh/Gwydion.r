// File: Gwydion.r
// RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/Gwydion.r,v 1.2 2002/03/16 23:52:28 gabor Exp $
// Purpose: define necessary resources for plugin
// Author: Gabor Greif <gabor@mac.com>
// Status: This version is based on the Pro6 CW API

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
#include "GwydionVersion.h"

#define PluginFileName "Gwydion Dylan PPC"
#define PluginResName PluginFileName
#define VERSION10API 10


resource 'vers' (1, PluginResName" Vers")
{
	CodebaseMajor,
	CodebaseMinor,
	DevelopmentStage,
	DevelopmentVersion,
	verUS,
	CodebaseVersion DevelopmentState,
	"v" CodebaseVersion DevelopmentState ", a Gwydion Dylanª component"
};

resource 'vers' (2, PluginResName" Prod")
{
	CodebaseMajor,
	CodebaseMinor,
	DevelopmentStage,
	DevelopmentVersion,
	verUS,
	CodebaseVersion DevelopmentState,
	"© 2000-2002, Gwydion Dylan maintainers"
};

resource 'Flag' (128, PluginResName) {
	Compiler {
		VERSION10API,
		generatesCode,
		doesntGenerateResources,
		cantPreprocess,
		cantPrecompile,
		isntPascal,
		cantImport,
		cantDisassemble,
		isntPersistent,
		dontAllowDuplicateFileNames,
		isntMultipleTargetAware,
		isntMultiprocessingAware,
		doesntUseTargetStorage,
		doesntHaveCompSpecificBrSymbols,
		alwaysReload,	// dontAlwaysReload, when fixed the cleanup issue
		doesntWantBuildStartedRequest,
		doesntWantTargetBuildStartedRequest,
		doesntWantSubprojectBuildStartedRequest,
		doesntWantFileListBuildStartedRequest,
		isntReentrant,
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
		UnknownLanguage,
		newestAPIVersion,
		reserved,
		reserved,
	}
};

resource 'Targ' (128, PluginResName) {
	kCurrentResourceVersion,
	{	/* array CPUs: 1 elements */
		/* [1] */
		PowerPC
	},
	{	/* array OperatingSystems: 1 elements */
		/* [1] */
		MacOS
	}
};

#define RESERVEDS 		reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved

resource 'EMap' (128, PluginResName) {
	kCurrentResourceVersion,
	{	/* array Mappings: 1 elements */
		/* [1] */
		text,
		".lid",
		doPrecompile,
		notLaunchable,
		notResourceFile,
		handledByMake,
		RESERVEDS,

		/* [2] */
		text,
		".dylan",
		dontPrecompile,
		notLaunchable,
		notResourceFile,
		ignoredByMake,
		RESERVEDS,

		/* [3] */
		text,
		".dyl",
		dontPrecompile,
		notLaunchable,
		notResourceFile,
		ignoredByMake,
		RESERVEDS,

		/* [4] */
		'BINA',
		".du",
		dontPrecompile,
		notLaunchable,
		notResourceFile,
		ignoredByMake,
		RESERVEDS
	}
};

resource 'STR ' (128, PluginFileName " Name", purgeable) {
	"Gwydion Dylan Compiler"
};

resource 'STR ' (129, PluginFileName " CPU", purgeable) {
	"PowerPC"
};

resource 'STR#' (128, PluginFileName " Panels", purgeable) {
	{
		"Dylan Compiler"
	}
};
