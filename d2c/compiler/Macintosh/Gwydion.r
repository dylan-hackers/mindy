// File: Gwydion.r
// RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/Gwydion.r,v 1.4 2004/04/13 20:48:47 gabor Exp $
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

#undef reserved

#define PluginFileName "Gwydion Dylan PPC"
#define PluginResName PluginFileName
#define VERSION11API 11


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
	"© " CopyrightYears ", Gwydion Dylan maintainers"
};

/*
type 'Flag'(128) {
	// resource version
	integer	= 2;
	
	switch 
	{
	case Compiler:
		key literal	longint = 'Comp';
		
		// earliest supported API version
		integer		earliestCompatibleVersion;
		
		// 1: compiler generates code ?
		boolean		doesntGenerateCode, generatesCode;
		// 2: compiler generates resources ?
		boolean		doesntGenerateResources, generatesResources;
		// 3: compiler supports preprocessing ?
		boolean		cantPreprocess, canPreprocess;
		// 4: compiler supports precompiling ?
		boolean		cantPrecompile, canPrecompile;
		// 5: this is for Metrowerks Pascal only
		boolean		isntPascal, isPascal;
		// 6: this is for library importers only
		boolean		cantImport, canImport;
		// 7: does the plugin handle the reqCompDisassemble request?
		boolean		cantDisassemble, canDisassemble;
		// 8: keep the compiler resident except on context switches?
		boolean		isntPersistent, isPersistent;
		// 9: allow multiple project files with the same name
		boolean		dontAllowDuplicateFileNames, allowDuplicateFileNames;
		// 10: the compiler can be used with multiple targets
		boolean		isntMultipleTargetAware, isMultipleTargetAware;
		// 11: the compiler can be run in an MP thread
		boolean		isntMultiprocessingAware, isMultiprocessingAware;
		// 12: the compiler uses per-target storage
		boolean		doesntUseTargetStorage, usesTargetStorage;
		// 13: the compiler generates compiler-specific browser symbols
		boolean		doesntHaveCompSpecificBrSymbols, hasCompSpecificBrSymbols;
		// 14: reload the compiler for every compile
		boolean		dontAlwaysReload, alwaysReload;
		// 15: should we make build started request to linker?
		boolean		doesntWantBuildStartedRequest, wantsBuildStartedRequest;
		// 16: should we make target build started request to linker?
		boolean		doesntWantTargetBuildStartedRequest, wantsTargetBuildStartedRequest;
		// 17: should we make sub-project build started request to linker?
		boolean		doesntWantSubprojectBuildStartedRequest, wantsSubprojectBuildStartedRequest;
		// 18: should we make file list build started request to linker?
		boolean		doesntWantFileListBuildStartedRequest, wantsFileListBuildStartedRequest;
		// 19: compiler supports reentrant requests
		boolean		isntReentrant, isReentrant;
		boolean		reserved;	// 20
		boolean		reserved;	// 21
		boolean		reserved;	// 22
		boolean		reserved;	// 23
		boolean		reserved;	// 24
		boolean		reserved;	// 25
		boolean		reserved;	// 26
		boolean		reserved;	// 27
		boolean		reserved;	// 28
		boolean		reserved;	// 29
		boolean		reserved;	// 30
		boolean		reserved;	// 31
		boolean		reserved;	// 32
		
		// language type
		literal longint	CPPLanguage		= 'c++ ', 
						PascalLanguage	= 'pasc', 
						RezLanguage		= 'rez ', 
						JavaLanguage	= 'java', 
						UnknownLanguage	= '????';
		// latest API version supported by the compiler
		integer		newestAPIVersion = 10;
		integer		reserved;
		integer		reserved;
	};
};*/

type 'Flag' (128) {
	// resource version
	integer	= 3;
	
	switch 
	{
	case Compiler:
		key literal	longint = 'Comp';
		
		// earliest supported API version
		integer		earliestCompatibleVersion;
		
		// 1: compiler generates code ?
		boolean		doesntGenerateCode, generatesCode;
		// 2: compiler generates resources ?
		boolean		doesntGenerateResources, generatesResources;
		// 3: compiler supports preprocessing ?
		boolean		cantPreprocess, canPreprocess;
		// 4: compiler supports precompiling ?
		boolean		cantPrecompile, canPrecompile;
		// 5: this is for Metrowerks Pascal only
		boolean		isntPascal, isPascal;
		// 6: this is for library importers only
		boolean		cantImport, canImport;
		// 7: does the plugin handle the reqCompDisassemble request?
		boolean		cantDisassemble, canDisassemble;
		// 8: keep the compiler resident except on context switches?
		boolean		isntPersistent, isPersistent;
		// 9: allow multiple project files with the same name
		boolean		dontAllowDuplicateFileNames, allowDuplicateFileNames;
		// 10: the compiler can be used with multiple targets
		boolean		isntMultipleTargetAware, isMultipleTargetAware;
		// 11: the compiler can be run in an MP thread
		boolean		isntMultiprocessingAware, isMultiprocessingAware;
		// 12: the compiler uses per-target storage
		boolean		doesntUseTargetStorage, usesTargetStorage;
		// 13: the compiler generates compiler-specific browser symbols
		boolean		doesntHaveCompSpecificBrSymbols, hasCompSpecificBrSymbols;
		// 14: reload the compiler for every compile
		boolean		dontAlwaysReload, alwaysReload;
		// 15: should we make build started request to linker?
		boolean		doesntWantBuildStartedRequest, wantsBuildStartedRequest;
		// 16: should we make target build started request to linker?
		boolean		doesntWantTargetBuildStartedRequest, wantsTargetBuildStartedRequest;
		// 17: should we make sub-project build started request to linker?
		boolean		doesntWantSubprojectBuildStartedRequest, wantsSubprojectBuildStartedRequest;
		// 18: should we make file list build started request to linker?
		boolean		doesntWantFileListBuildStartedRequest, wantsFileListBuildStartedRequest;
		// 19: compiler supports reentrant requests
		boolean		isntReentrant, isReentrant;
		boolean		reserved;	// 20
		boolean		reserved;	// 21
		boolean		reserved;	// 22
		boolean		reserved;	// 23
		boolean		reserved;	// 24
		boolean		reserved;	// 25
		boolean		reserved;	// 26
		boolean		reserved;	// 27
		boolean		reserved;	// 28
		boolean		reserved;	// 29
		boolean		reserved;	// 30
		boolean		reserved;	// 31
		boolean		reserved;	// 32
		
		// language type
		literal longint	CPPLanguage		= 'c++ ', 
						PascalLanguage	= 'pasc', 
						RezLanguage		= 'rez ', 
						JavaLanguage	= 'java', 
						UnknownLanguage	= '????';
		// latest API version supported by the compiler
		integer		newestAPIVersion = 13;
		integer		reserved;
		integer		reserved;
		
	};
};


resource 'Flag' (128, PluginResName) {
	Compiler {
		13,//VERSION11API,
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
