/*
	mindy_compiler.cpp
	
	Metrowerks Codewarrior Mindy compiler plugin.
	
	by Patrick C. Beard.
 */

/* standard headers */
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <new.h>
#include <setjmp.h>

/* system headers */
#include <Files.h>
#include <Errors.h>
#include <Strings.h>

#include "FullPath.h"
#include "MoreFilesExtras.h"

/* compiler headers */
#include "DropInCompilerLinker.h"
#include "CompilerMapping.h"
#include "CWPluginErrors.h"

/* local headers. */
#include "mindy_compiler.h"
#include "mindy_panel.h"
#include "mac_console.h"
#include "mac_strings.h"

/* prototypes of local functions */
static CWResult	Compile(CWPluginContext context);
static CWResult	Disassemble(CWPluginContext context);
static CWResult	LocateFile(CWPluginContext context, const char* filename, FSSpec& file);

/* global variables */
extern CWPluginContext gPluginContext;

extern "C" {
pascal short mindy_compiler(CWPluginContext context);
FILE * FSp_fopen(ConstFSSpecPtr spec, const char * open_mode);
int MindyComp(int argc, char *argv[], FILE * newStderr);
}

pascal short mindy_compiler(CWPluginContext context)
{
	long request;
	if (CWGetPluginRequest(context, &request) != cwNoErr)
		return cwErrRequestFailed;
	
	gPluginContext = context;
	short result = cwNoErr;
	
	/* dispatch on compiler request */
	switch (request) {
	case reqInitCompiler:
		/* compiler has just been loaded into memory */
		break;
		
	case reqTermCompiler:
		/* compiler is about to be unloaded from memory */
		break;
		
	case reqCompile:
		/* compile a source file */
		result = Compile(context);
		break;
	
	case reqCompDisassemble:
		/* disassemble a source file */
		result = Disassemble(context);
		break;
	
	default:
		result = cwErrRequestFailed;
		break;
	}
	
	/* is this necessary? */
	CWDonePluginRequest(context, result);
	
	/* return result code */
	return (result);
}

static char* full_path_to(const FSSpec& file)
{
	short len = 0;
	Handle fullPath = NULL;
	OSErr err = FSpGetFullPath(&file, &len, &fullPath);
	if (fullPath != NULL) {
		char* path = new char[1 + len];
		if (path != NULL) {
			BlockMoveData(*fullPath, path, len);
			path[len] = '\0';
		}
		DisposeHandle(fullPath);
		return path;
	}
	return NULL;
}

static CWResult GetSettings(CWPluginContext context, MindySettings& settings)
{
	CWMemHandle	settingsHand;
	CWResult err = CWGetNamedPreferences(context, kMindyPanelName, &settingsHand);
	if (!CWSUCCESS(err))
		return (err);
	
	MindySettings* settingsPtr = NULL;
	err = CWLockMemHandle(context, settingsHand, false, (void**)&settingsPtr);
	if (!CWSUCCESS(err))
		return (err);
	
	settings = *settingsPtr;
	
	err = CWUnlockMemHandle(context, settingsHand);
	if (!CWSUCCESS(err))
		return (err);

	return noErr;
}

static CWResult	Compile(CWPluginContext context)
{
	// get source file's location.
	FSSpec sourceFile;
	CWResult err = CWGetMainFileSpec(context, &sourceFile);
	if (!CWSUCCESS(err))
		return (err);

	long fileNum;
	err = CWGetMainFileNumber(context, &fileNum);
	if (!CWSUCCESS(err))
		return (err);

	// get the name of the source file to compile.
	char* sourcePath = full_path_to(sourceFile);
	if (sourcePath == NULL)
		return cwErrOutOfMemory;
	
	// build an argument list and call the compiler.
	MindySettings settings = { kMindySettingsVersion, kMindyModeApplication, false, false, "\p", "\pm.out" };
	GetSettings(context, settings);

	// always generate the output file into the project target's data directory.
	FSSpec outputFile;
	err = CWGetSuggestedObjectFileSpec(context, fileNum, &outputFile);
	if (!CWSUCCESS(err))
		return (err);
	
	// construct a full path to the output file.
	char* outputName = (char*) outputFile.name + 1;
	outputName[outputFile.name[0]] = '\0';
	char* dot = strchr(outputName, '.');
	if (dot == NULL)
		return cwErrRequestFailed;
	strcpy(dot + 1, "dbc");
	outputFile.name[0] = strlen(outputName);
	char* outputPath = full_path_to(outputFile);
	
	// build the argument list.
	int argc = 4;
	char* argv[] = { "mindy", sourcePath, "-o", outputPath,
					 NULL, /* -d */
					 NULL, /* -q */
					 NULL, NULL, /* -l libraryName */
					 NULL };
	
	if (settings.dumpTrees)
		argv[argc++] = "-d";
	if (settings.noWarnings)
		argv[argc++] = "-q";
	if (settings.mode == kMindyModeLibrary) {
		argv[argc++] = "-l";
		argv[argc++] = p2c_strdup(settings.library);
	}
	
	// run the mindy compiler.
	if (MindyComp(argc, argv, stderr) != 0)
		err = cwErrRequestFailed;

	delete[] sourcePath;
	sourcePath = NULL;
	delete[] outputPath;
	outputPath = NULL;

	// if the compilation succeeded, tell CodeWarrior about the output file.
	// this ensures several things:  1. if the output file is deleted by the user,
	// then the IDE will know to recompile it, which is good for dirty builds,
	// where the output files may be hand deleted; 2. if the user elects to remove
	// objects, the output files are deleted. Thanks to robv@metrowerks.com for
	// pointing this new CWPro4 API out.
	if (err == cwNoErr) {
		CWObjectData objectData;
		BlockZero(&objectData, sizeof(objectData));
		
		// for fun, show how large the output file is in the data area.
		long dataSize, rsrcSize;
		if (FSpGetFileSize(&outputFile, &dataSize, &rsrcSize) == noErr)
			objectData.idatasize = dataSize;
		
		// tell the IDE that this file was generated by the compiler.
		objectData.objectfile = &outputFile;
		
		err = CWStoreObjectData(context, fileNum, &objectData);
	}

	// did the compiler emit any error messages?
	if (mac_console_handle != NULL) {
		// display the error messages in a fresh text window.
		CWNewTextDocumentInfo info = {
			NULL,
			mac_console_handle,
			false
		};
		CWResizeMemHandle(context, mac_console_handle, mac_console_count);
		err = CWCreateNewTextDocument(context, &info);
	}
	
	return (err);
}

static CWResult	Disassemble(CWPluginContext context)
{
	// the disassembly code has moved to the linker.
	return noErr;
}

static CWResult	LocateFile(CWPluginContext context, const char* filename, FSSpec& file)
{
	/* prefill the CWFileInfo struct */
	CWFileInfo fileinfo;
	BlockZero(&fileinfo, sizeof(fileinfo));
	// memset(&fileinfo, 0, sizeof(fileinfo));
	fileinfo.fullsearch = true;
	fileinfo.suppressload = true;
	fileinfo.dependencyType = cwNormalDependency;
	fileinfo.isdependentoffile = kCurrentCompiledFile;

	/* locate the file name using the project's access paths */
	CWResult err = CWFindAndLoadFile(context, filename, &fileinfo);
	if (err == cwNoErr) {
		file = fileinfo.filespec;
	} else if (err == cwErrFileNotFound) {
		char errmsg[200];
		sprintf(errmsg, "Can't locate file \"%s\".", filename);
		CWResult callbackResult = CWReportMessage(context, 0, errmsg, 0, messagetypeError, 0);
	}
	
	return (err);
}

/**
 * Returns the length of a file, assuming it is always located in the
 * project's output directory.
 */
size_t mac_get_file_length(const char* filename)
{
	long dataSize= 0, rsrcSize = 0;
	FSSpec filespec;
	if (CWGetOutputFileDirectory(gPluginContext, &filespec) != noErr)
		return 0;
	c2p_strcpy(filespec.name, filename);
	if (FSpGetFileSize(&filespec, &dataSize, &rsrcSize) != noErr)
		return 0;
	return dataSize;
}

void mac_warning(const char* warning_message)
{
	CWReportMessage(gPluginContext, 0, warning_message, 0, messagetypeError, 0);
}

void mac_error(const char* error_message)
{
	CWReportMessage(gPluginContext, 0, error_message, 0, messagetypeError, 0);
}

// plugin compiler exports.

#if CW_USE_PRAGMA_EXPORT
#pragma export on
#endif

CWPLUGIN_ENTRY(CWPlugin_GetDropInFlags)(const DropInFlags** flags, long* flagsSize)
{
	static const DropInFlags sFlags = {
		kCurrentDropInFlagsVersion,
		CWDROPINCOMPILERTYPE,
		DROPINCOMPILERLINKERAPIVERSION,
		(kGeneratescode | /* kCandisassemble | */ kCompMultiTargAware | kCompAlwaysReload),
		Lang_MISC,
		DROPINCOMPILERLINKERAPIVERSION
	};
	
	*flags = &sFlags;
	*flagsSize = sizeof(sFlags);
	
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetDropInName)(const char** dropinName)
{
	static const char* sDropInName = "mindy";
	
	*dropinName = sDropInName;
	
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetDisplayName)(const char** displayName)
{
	static const char* sDisplayName = "mindy";
	
	*displayName = sDisplayName;
	
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetPanelList)(const CWPanelList** panelList)
{
	static const char* sPanelName = kMindyPanelName;
	static CWPanelList sPanelList = { kCurrentCWPanelListVersion, 1, &sPanelName };
	
	*panelList = &sPanelList;
	
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetTargetList)(const CWTargetList** targetList)
{
	static CWDataType sCPU = '****';
	static CWDataType sOS = '****';
	static CWTargetList sTargetList = { kCurrentCWTargetListVersion, 1, &sCPU, 1, &sOS };
	
	*targetList = &sTargetList;
	
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetDefaultMappingList)(const CWExtMapList** defaultMappingList)
{
	static CWExtensionMapping sExtension = { 'TEXT', ".dylan", 0 };
	static CWExtMapList sExtensionMapList = { kCurrentCWExtMapListVersion, 1, &sExtension };
	
	*defaultMappingList = &sExtensionMapList;
	
	return cwNoErr;
}

#if CW_USE_PRAGMA_EXPORT
#pragma export off
#endif
