/*
	mindy_linker.cpp
	
	CodeWarrior plugin linker, links together multiple .dbc files.
	
	by Patrick C. Beard.
 */

/* standard headers */
#include <stdio.h>
#include <string.h>
#include <setjmp.h>

/* system headers */
#include <Files.h>
#include <Strings.h>
#include <Aliases.h>
#include <Resources.h>

/* compiler headers */
#include "DropInCompilerLinker.h"
#include "CompilerMapping.h"
#include "CWPluginErrors.h"

/* project headers */
#include "mindy_panel.h"
#include "mac_console.h"
#include "mac_strings.h"
#include "FullPath.h"
#include "MoreFilesExtras.h"

/* use standard CodeWarrior debugger */
#define kDebuggerCreator	'MWDB'

/* prototypes of local functions */
static CWResult	Link(CWPluginContext context);
static CWResult	Disassemble(CWPluginContext context);
static CWResult	GetTargetInfo(CWPluginContext context);

extern "C" {
pascal short mindy_linker(CWPluginContext context);

size_t mac_get_file_length(const char* filename);
FILE * FSp_fopen(ConstFSSpecPtr spec, const char * open_mode);
}

/* global variables */
CWPluginContext gPluginContext;

/* local variables */
static CWFileSpec gOutputDirectory;
static CWFileSpec gObjectCodeDirectory;

/*
 *	mindy_linker	-	main entry-point for linker plugin
 *
 */
pascal short mindy_linker(CWPluginContext context)
{
	long request;
	if (CWGetPluginRequest(context, &request) != cwNoErr)
		return cwErrRequestFailed;
	
	gPluginContext = context;
	short result = cwNoErr;
		
	/* dispatch on linker request */
	switch (request) {
	case reqInitLinker:
		/* linker has just been loaded into memory */
		break;
		
	case reqTermLinker:
		/* linker is about to be unloaded from memory */
		break;
		
	case reqLink:
		/* build the final executable */
		result = Link(context);
		break;
		
	case reqDisassemble:
		/* disassemble object code for a given project file */
		result = Disassemble(context);
		break;
	
	case reqTargetInfo:
		/* return info describing target characteristics */
		result = GetTargetInfo(context);
		break;
		
	default:
		result = cwErrRequestFailed;
		break;
	}
	
	result = CWDonePluginRequest(context, result);
	
	/* return result code */
	return result;
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

/**
 * Provides the full path name to a given directory.
 */
static char* full_path_to(short vRefNum, long dirID)
{
	long parID;
	if (GetParentID(vRefNum, dirID, NULL, &parID) == noErr) {
		FSSpec dirSpec = { vRefNum, parID };
		if (GetDirName(vRefNum, dirID, dirSpec.name) == noErr) {
			return full_path_to(dirSpec);
		}
	}
	return NULL;
}

/**
 * Returns the length of a file, assuming it is always located in the
 * project's object code directory.
 */
size_t mac_get_file_length(const char* filename)
{
	FSSpec fileSpec = { gObjectCodeDirectory.vRefNum, gObjectCodeDirectory.parID };
	c2p_strcpy(fileSpec.name, filename);
	long dataSize, rsrcSize;
	if (FSpGetFileSize(&fileSpec, &dataSize, &rsrcSize) != noErr)
		dataSize = 0;
	return dataSize;
}

static CWResult GetSettings(CWPluginContext context, MindySettings& settings)
{
	CWMemHandle	settingsHand;
	CWResult err = CWGetNamedPreferences(context, kMindyPanelName, &settingsHand);
	if (!CWSUCCESS(err))
		return err;
	
	MindySettings* settingsPtr = NULL;
	err = CWLockMemHandle(context, settingsHand, false, (void**)&settingsPtr);
	if (!CWSUCCESS(err))
		return err;
	
	settings = *settingsPtr;
	
	err = CWUnlockMemHandle(context, settingsHand);
	if (!CWSUCCESS(err))
		return err;

	return cwNoErr;
}

static CWResult LinkByteCodes(CWPluginContext context, MindySettings& settings)
{
	// find out how many files there are to link.
	long fileCount = 0;
	CWResult err = CWGetProjectFileCount(context, &fileCount);
	if (err != cwNoErr || fileCount == 0)
		return err;

	// open the output file.
	FSSpec outputSpec;
	err = CWGetOutputFileDirectory(context, &outputSpec);
	if (!CWSUCCESS(err))
		return err;
	BlockMoveData(settings.output, outputSpec.name, 1 + settings.output[0]);
	FILE* outputFile = FSp_fopen(&outputSpec, "wb");
	if (outputFile == NULL)
		return cwErrOutOfMemory;
	
	// enumerate all of the output bytecode files, .
	for (long index = 0; (err == cwNoErr) && (index < fileCount); index++) {
		// get the name of each output file.
		CWFileSpec bytecodeSpec;
		err = CWGetStoredObjectFileSpec(context, index, &bytecodeSpec);
		if (err != cwNoErr)
			break;
			
		FILE* bytecodeFile = FSp_fopen(&bytecodeSpec, "rb");
		if (bytecodeFile == NULL) {
			err = cwErrFileNotFound;
			break;
		}
		
		char buffer[BUFSIZ];
		size_t count;
		
		while ((count = fread(buffer, 1, sizeof(buffer), bytecodeFile)) > 0) {
			if (fwrite(buffer, 1, count, outputFile) != count) {
				err = cwErrRequestFailed;
				break;
			}
		}
		
		fclose(bytecodeFile);
	}
	
	fclose(outputFile);
	
	return err;
}

static CWResult	Link(CWPluginContext context)
{
	// load the relevant prefs.
	MindySettings settings = { kMindySettingsVersion, kMindyModeApplication, false, false, "\p", "\pm.out" };
	CWResult err = GetSettings(context, settings);
	if (err != cwNoErr)
		return err;

	switch (settings.mode) {
	case kMindyModeApplication:
	case kMindyModeLibrary:
		return LinkByteCodes(context, settings);
	default:
		return cwNoErr;
	}
}

static CWResult	Disassemble(CWPluginContext context)
{
	CWResult err = noErr;

#if 0
	// cache the project's output directory.
	err = CWGetOutputFileDirectory(gPluginContext, &gOutputDirectory);
	if (!CWSUCCESS(err))
		return err;

	long fileNum;
	err = CWGetMainFileNumber(context, &fileNum);
	if (!CWSUCCESS(err))
		return err;

	// get the output file's location from the stored object data.
	err = CWGetStoredObjectFileSpec(context, fileNum, &gObjectCodeDirectory);
	if (!CWSUCCESS(err))
		return err;
	
	char* outputName = p2c_strdup(gObjectCodeDirectory.name);
	if (outputName == NULL)
		return cwErrOutOfMemory;

	MindySettings settings = { kMindySettingsVersion, kMindyModeApplication, false, false, "\p", "\pm.out" };
	GetSettings(context, settings);

	// build an argument list and call xpt_dump.
	int argc = 1;
	char* argv[] = { "xpt_dump", NULL, NULL, NULL };
	if (settings.verbose) argv[argc++] = "-v";
	argv[argc++] = outputName;
	
	// trap calls to exit, which longjmp back to here.
	if (setjmp(exit_jump) == 0) {
		if (xptdump_main(argc, argv) != 0)
			err = cwErrRequestFailed;
	} else {
		// evidently the good old exit function got called.
		if (exit_status != 0)
			err = cwErrRequestFailed;
	}

	delete[] outputName;

	if (err == noErr) {
		// display the disassembly in its own fresh text window.
		CWNewTextDocumentInfo info = {
			NULL,
			mac_console_handle,
			false
		};
		CWResizeMemHandle(context, mac_console_handle, mac_console_count);
		err = CWCreateNewTextDocument(context, &info);
	}
#endif

	return err;
}

static CWResult	GetTargetInfo(CWPluginContext context)
{
	CWTargetInfo targ;
	memset(&targ, 0, sizeof(targ));
	
	CWResult err = CWGetOutputFileDirectory(context, &targ.outfile);
	targ.outputType = linkOutputFile;
	targ.symfile = targ.outfile;	/* location of SYM file */
	targ.linkType = exelinkageFlat;
	targ.targetCPU = '****';
	targ.targetOS = '****';
	
	// load the relevant settings.
	MindySettings settings = { kMindySettingsVersion, kMindyModeApplication, false, false, "\p", "\pm.out" };
	err = GetSettings(context, settings);
	if (err != cwNoErr)
		return err;
	
#if CWPLUGIN_HOST == CWPLUGIN_HOST_MACOS
	// tell the IDE about the output file.
	targ.outfileCreator		= 'MMCH';
	targ.outfileType		= 'CWIE';
	targ.debuggerCreator	= kDebuggerCreator;	/* so IDE can locate our debugger	*/

	BlockMoveData(settings.output, targ.outfile.name, 1 + settings.output[0]);
	targ.symfile.name[0] = 0;
#endif

#if CWPLUGIN_HOST == CWPLUGIN_HOST_WIN32
	targ.debugHelperIsRegKey = true;
	*(long*)targ.debugHelperName = kDebuggerCreator;
	targ.debugHelperName[4] = 0;
	strcat(targ.outfile.path, "\\");
	strcat(targ.outfile.path, prefsData.outfile);
	strcpy(targ.symfile.path, targ.outfile.path);
	strcat(targ.symfile.path, ".SYM");
#endif

	targ.runfile			= targ.outfile;
	targ.linkAgainstFile	= targ.outfile;

	/* we can only run applications */
	// targ.canRun = (prefsData.projtype == kProjTypeApplication);
	
	/* we can only debug if we have a SYM file */
	// targ.canDebug = prefsData.linksym;	
	
	err = CWSetTargetInfo(context, &targ);
	
	return err;
}

#if 0

#if CW_USE_PRAGMA_EXPORT
#pragma export on
#endif

CWPLUGIN_ENTRY(CWPlugin_GetDropInFlags)(const DropInFlags** flags, long* flagsSize)
{
	static const DropInFlags sFlags = {
		kCurrentDropInFlagsVersion,
		CWDROPINLINKERTYPE,
		DROPINCOMPILERLINKERAPIVERSION_7,
		(linkMultiTargAware | linkAlwaysReload),
		0,
		DROPINCOMPILERLINKERAPIVERSION
	};
	
	*flags = &sFlags;
	*flagsSize = sizeof(sFlags);
	
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetDropInName)(const char** dropinName)
{
	static const char* sDropInName = "mindy Linker";
	*dropinName = sDropInName;
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetDisplayName)(const char** displayName)
{
	static const char* sDisplayName = "mindy Linker";
	*displayName = sDisplayName;
	return cwNoErr;
}

CWPLUGIN_ENTRY(CWPlugin_GetPanelList)(const CWPanelList** panelList)
{
	// +++Turn this on when the sample panel has been converted!
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
	static CWExtensionMapping sExtension = { 'MMCH', ".dbc", 0 };
	static CWExtMapList sExtensionMapList = { kCurrentCWExtMapListVersion, 1, &sExtension };
	
	*defaultMappingList = &sExtensionMapList;
	
	return cwNoErr;
}

CWPLUGIN_ENTRY (CWPlugin_GetFamilyList)(const CWFamilyList** familyList)
{
	static CWFamily sFamily = { 'XIDL', "mindy Settings" };
	static CWFamilyList sFamilyList = { kCurrentCWFamilyListVersion, 0, &sFamily };
	
	*familyList = &sFamilyList;
	
	return cwNoErr;
}

#if CW_USE_PRAGMA_EXPORT
#pragma export off
#endif

#endif
