/*
	mindy_panel.cpp
	
	Implements the Mindy settings panel.
	
	by Patrick C. Beard.
 */

#define CW_STRICT_DIALOGS 1

/* standard headers */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

/* system headers */
#include <AERegistry.h>
#include <Drag.h>
#include <Palettes.h>
#include <Resources.h>
#include <Scrap.h>
#include <TextUtils.h>
#include <Sound.h>

/* compiler headers */
#include <A4Stuff.h>
#include <SetUpA4.h>
#include <DropInPanel.h>

/* project headers */
#include "mindy_panel.h"

enum {
	kFactoryPrefsID = 128,
	kCW7ItemListID = 128,
	kCW8ItemListID = 129,

	kMindyModeItem = 1,
	kMindyDumpTreesItem,
	kMindyNoWarningsItem,
	kMindyLibraryNameItem,
	kMindyLinkerOutputItem
};


/* local variables */
static RgnHandle	sDragRgn;
static Boolean		sHighlightOn;


/* prototypes of local functions */
static short	InitDialog(PanelParameterBlock *pb);
static void		TermDialog(PanelParameterBlock *pb);
static void		PutData(PanelParameterBlock *pb, Handle options);
static short	GetData(PanelParameterBlock *pb, Handle options, Boolean noisy);
static void		ByteSwapData(MindySettingsHandle options);
static short	Filter(PanelParameterBlock *pb, EventRecord *event, short *itemHit);
static void		ItemHit(PanelParameterBlock *pb);
static void		Validate(Handle original, Handle current, Boolean *recompile, Boolean *relink, Boolean *reset);
static short	GetPref(AEKeyword keyword, AEDesc *prefsDesc, Handle settings);
static short	SetPref(AEKeyword keyword, const AEDesc *prefsDesc, Handle settings);
static short	GetFactory(Handle settings);
static short	UpdatePref(Handle settings);
static Boolean	ComparePrefs(Handle prefsHand1, Handle prefsHand2);
static Boolean	ComparePrefs(MindySettings& prefs1, MindySettings& prefs2);
static void		OutlineRect(const Rect* focusRect, Boolean outlineOn);
static OSErr	DragEnter(PanelParameterBlock *pb);
static void		DragWithin(PanelParameterBlock *pb);
static void		DragExit(PanelParameterBlock *pb);
static void		DragDrop(PanelParameterBlock *pb);

extern "C" {

pascal short	mindy_panel(PanelParameterBlock *pb);

}

/*
 *	main	-	entry-point for Drop-In Preferences Panel
 *
 */
 
pascal short mindy_panel(PanelParameterBlock *pb)
{
	short	result;
	
	EnterCodeResource();
	PrepareCallback();
	
	result = noErr;
			
	switch (pb->request) {
	case reqInitPanel:
		/* panel has just been loaded into memory */
		break;

	case reqTermPanel:
		/* panel is about to be unloaded from memory */
		break;

	case reqFirstLoad:
		/* first time panel was loaded. */
		break;

	case reqInitDialog:
		/* hook our dialog item list into the preferences dialog */
		result = InitDialog(pb);
		break;
	
	case reqTermDialog:
		/* unhook our dialog item list from the preferences dialog */
		TermDialog(pb);
		break;
	
	case reqPutData:
		/* put the data in the given handle into our dialog items */
		PutData(pb, pb->currentPrefs);
		break;

	case reqGetData:
		/* fill in the given handle with our dialog items */
		result = GetData(pb, pb->currentPrefs, true);
		break;

	case reqByteSwapData:
		/* byte swap the data in the handle */
		ByteSwapData((MindySettingsHandle)pb->currentPrefs);
		break;
		
	case reqFilter:
		/* filter an event in the dialog */
		result = Filter(pb, pb->event, &pb->itemHit);
		break;
		
	case reqItemHit:
		/* handle a hit on one of our dialog items */
		ItemHit(pb);
		break;
		
	case reqDrawCustomItem:
		/* handle a request to draw one of our user items (CW/8 and later) */
		break;
		
	case reqActivateItem:
		break;
		
	case reqDeactivateItem:
		break;
		
	case reqHandleKey:
		break;
		
	case reqHandleClick:
		break;
		
	case reqFindStatus:
		break;
		
	case reqObeyCommand:
		break;
		
	case reqAEGetPref:
		/* return one item in the given handle as an Apple Event descriptor */
		result = GetPref(pb->prefsKeyword, &pb->prefsDesc, pb->currentPrefs);
		break;

	case reqAESetPref:
		/* change one item in the given handle according to the given Apple Event descriptor */
		result = SetPref(pb->prefsKeyword, &pb->prefsDesc, pb->currentPrefs);
		break;

	case reqValidate:
		/* determine if we need to reset paths, recompile, or relink */
		Validate(pb->originalPrefs, pb->currentPrefs, &pb->recompile, &pb->relink, &pb->reset);
		break;

	case reqGetFactory:
		/* return our factory settings */
		result = GetFactory(pb->factoryPrefs);
		break;

	case reqUpdatePref:
		/* update the given handle to use the current format for our prefs data */
		result = UpdatePref(pb->currentPrefs);
		break;
		
	case reqDragEnter:
		/* determine if we can accept the drag and, if so, start tracking */
		result = DragEnter(pb);
		break;
	
	case reqDragWithin:
		/* continue tracking */
		DragWithin(pb);
		break;
	
	case reqDragExit:
		/* stop tracking */
		DragExit(pb);
		break;
	
	case reqDragDrop:
		/* the user has dropped in our panel */
		DragDrop(pb);
		break;
	
	default:
		result = paramErr;
		break;
	}
	
	ExitCodeResource();
	
	return (result);
}

/*
 *	InitDialog  -	initialize Dialog Box items for this panel
 *
 */

static short InitDialog(PanelParameterBlock *pb)
{
	OSErr	err;
	
	// The library function will call the IDE to append the dialog items 
	// if possible;  else it will call AppendDITL itself.  This way, you 
	// don't have to worry about it.
	
	err = CWPanlAppendItems(pb, kCW8ItemListID);
	if (err != noErr)
		return (err);
	
	sDragRgn = NewRgn();
	
	return (err);
}

/*
 *	TermDialog	-	destroy Dialog Box items for this panel
 *
 */

static void TermDialog(PanelParameterBlock *pb)
{
	DisposeRgn(sDragRgn);
}

inline Boolean hasLinkerOutput(short mode)
{
	return (mode == kMindyModeApplication || mode == kMindyModeLibrary);
}

/*
 *	PutData		-	copy the options data from the handle to the screen
 *
 */

static void PutData(PanelParameterBlock *pb, Handle options)
{
	// make sure the options are the right size.
	UpdatePref(options);
	
	MindySettings prefsData = **(MindySettingsHandle) options;

	CWPanlSetItemValue(pb, kMindyModeItem, prefsData.mode);
	CWPanlSetItemValue(pb, kMindyDumpTreesItem, prefsData.dumpTrees);
	CWPanlSetItemValue(pb, kMindyNoWarningsItem, prefsData.noWarnings);

	CWPanlEnableItem(pb, kMindyLibraryNameItem, prefsData.mode == kMindyModeLibrary);
	CWPanlSetItemText(pb, kMindyLibraryNameItem, prefsData.library);

	CWPanlEnableItem(pb, kMindyLinkerOutputItem, true);
	CWPanlSetItemText(pb, kMindyLinkerOutputItem, prefsData.output);
}

/*
 *	GetData		-	copy the options data from screen to the handle
 *
 */

static short GetData(PanelParameterBlock *pb, Handle options, Boolean noisy)
{
	MindySettings prefsData	= **(MindySettingsHandle) options;
	long mode, dumpTrees, noWarnings;
	
	CWPanlGetItemValue(pb, kMindyModeItem, &mode);
	CWPanlGetItemValue(pb, kMindyDumpTreesItem, &dumpTrees);
	CWPanlGetItemValue(pb, kMindyNoWarningsItem, &noWarnings);
	
	prefsData.mode = (short) mode;
	prefsData.dumpTrees = (Boolean) dumpTrees;
	prefsData.noWarnings = (Boolean) noWarnings;
	
	CWPanlGetItemText(pb, kMindyLibraryNameItem, prefsData.library, sizeof(Str32));
	CWPanlGetItemText(pb, kMindyLinkerOutputItem, prefsData.output, sizeof(Str32));
	
	** (MindySettingsHandle) options = prefsData;
	
	return (noErr);
}

static void ByteSwapShort(short* x)
{
	union {
		short	s;
		char	c[2];
	}	from,to;

	from.s=*x;
	to.c[0]=from.c[1];
	to.c[1]=from.c[0];
	*x = to.s;
}

/*
 *	ByteSwapData		-	byte-swap the options data
 *
 */

static void ByteSwapData(MindySettingsHandle options)
{
	ByteSwapShort(&(**options).version);
	ByteSwapShort(&(**options).mode);
}

/*
 *	Filter		-	filter an event for the Preferences panel
 *
 */
static short Filter(PanelParameterBlock *pb, EventRecord *event, short *itemHit)
{
#pragma unused(pb, event, itemHit)
	
	return (noErr);
}

/*
 *	ItemHit		-	handle an itemHit in a Preferences panel
 *
 */

static void ItemHit(PanelParameterBlock *pb)
{
	short	theItem	= pb->itemHit - pb->baseItems;
	long	oldValue;
	
	switch (theItem) {
	case kMindyModeItem:
		CWPanlGetItemValue(pb, theItem, &oldValue);
		CWPanlEnableItem(pb, kMindyLibraryNameItem, oldValue == kMindyModeLibrary);
		break;
		
	case kMindyDumpTreesItem:
	case kMindyNoWarningsItem:
		CWPanlGetItemValue(pb, theItem, &oldValue);
		break;
	}
	
	GetData(pb, pb->currentPrefs, false);

	pb->canRevert	= !ComparePrefs(pb->originalPrefs, pb->currentPrefs);
	pb->canFactory	= !ComparePrefs(pb->factoryPrefs,  pb->currentPrefs);
}

/*
 *	Validate	-	check if panel's changes require a recompile or relink
 *
 */

static void Validate(Handle original, Handle current, Boolean *recompile, Boolean *relink, Boolean *reset)
{
	MindySettings& origSettings = **(MindySettingsHandle) original;
	MindySettings& currentSettings = **(MindySettingsHandle) current;
	
	*recompile	= !EqualString(origSettings.library, currentSettings.library, true, true);
	*relink		= *recompile && hasLinkerOutput(currentSettings.mode);
	*reset		= false;
}

/*
 *	GetPref		-	get a specified Preference setting for an AppleEvent request
 *
 */
static short GetPref(AEKeyword keyword, AEDesc *prefsDesc, Handle settings)
{
	MindySettings	prefsData	= ** (MindySettingsHandle) settings;
	DescType	anEnum;
	OSErr		err;

	switch (keyword)  {
#if 0	
	case prefsLN_GenerateSymFile:
		err = AECreateDesc(typeBoolean, &prefsData.linksym, sizeof(Boolean), prefsDesc);
		break;
		
	case prefsPR_ProjectType:
		switch (prefsData.projtype)
		{
		case kProjTypeApplication:	anEnum = enum_Project_Application;		break;
		case kProjTypeLibrary:		anEnum = enum_Project_Library;			break;
		case kProjTypeSharedLib:	anEnum = enum_Project_SharedLibrary;	break;
		case kProjTypeCodeResource:	anEnum = enum_Project_CodeResource;		break;
		case kProjTypeMPWTool:		anEnum = enum_Project_MPWTool;			break;
		default:					return (paramErr);
		}
		err = AECreateDesc(typeEnumeration, &anEnum, sizeof(anEnum), prefsDesc);
		break;
		
	case prefsPR_FileName:
		err = AECreateDesc(typeChar, prefsData.outfile+1, StrLength(prefsData.outfile), prefsDesc);
		break;
#endif

	default:
		err = errAECantHandleClass;
		break;
	}
	
	return (err);
}

/*
 *	SetPref		-	set a specified Preference setting from an AppleEvent request
 *
 */

static short SetPref(AEKeyword keyword, const AEDesc *prefsDesc, Handle settings)
{
	MindySettings	prefsData	= ** (MindySettingsHandle) settings;
	AEDesc			toDesc	= { typeNull, NULL };
	OSErr			err		= noErr;
	Handle			dataHand;
	Size			textLength;
	DescType		anEnum;
	
	switch (keyword)
	{
#if 0
	case prefsLN_GenerateSymFile:
		if (prefsDesc->descriptorType == typeBoolean)
		{
			dataHand = prefsDesc->dataHandle;
		}
		else
		{
			err = AECoerceDesc(prefsDesc, typeBoolean, &toDesc);
			if (err == noErr)
				dataHand = toDesc.dataHandle;
		}
		if (err == noErr)
		{
			prefsData.linksym = ** (Boolean **) dataHand;
		}
		break;
		
	case prefsPR_ProjectType:
		if (prefsDesc->descriptorType != typeEnumeration)
		{
			err = errAETypeError;
			break;
		}

		anEnum = ** (DescType **) prefsDesc->dataHandle;
		
		switch (anEnum)
		{
		case enum_Project_Application:		prefsData.projtype = kProjTypeApplication;	break;
		case enum_Project_Library:			prefsData.projtype = kProjTypeLibrary;		break;
		case enum_Project_SharedLibrary:	prefsData.projtype = kProjTypeSharedLib;	break;
		case enum_Project_CodeResource:		prefsData.projtype = kProjTypeCodeResource;	break;
		case enum_Project_MPWTool:			prefsData.projtype = kProjTypeMPWTool;		break;
		default:							return (errAECoercionFail);
		}
		break;
		
	case prefsPR_FileName:
		if (prefsDesc->descriptorType == typeChar)
		{
			dataHand = prefsDesc->dataHandle;
		}
		else
		{
			err = AECoerceDesc(prefsDesc, typeChar, &toDesc);
			if (err == noErr)
				dataHand = toDesc.dataHandle;
		}
		if (err == noErr)
		{
			textLength = GetHandleSize(dataHand);
			if (textLength > sizeof(prefsData.outfile) - 1)
				textLength = sizeof(prefsData.outfile) - 1;
			BlockMoveData(*dataHand, prefsData.outfile+1, textLength);
			prefsData.outfile[0] = textLength;
		}
		break;
#endif

	default:
		err = errAECantHandleClass;
		break;
	}
	
	if (err == noErr)
	{
		** (MindySettingsHandle) settings = prefsData;
	}
	
	AEDisposeDesc(&toDesc);
	
	return (err);
}

/*
 *	GetFactory	-	retrieve factory settings
 *
 */

static short GetFactory(Handle settings)
{
	Handle	factory;
	Size	size;
	OSErr	err;
	
	factory = Get1Resource('pref', kFactoryPrefsID);
	if (factory == NULL) {
		err = ResError();
		if (err == noErr)
			err = resNotFound;
		return (err);
	}
	
	size = GetHandleSize(factory);
	SetHandleSize(settings, size);
	err = MemError();
	
	if (err == noErr) {
		BlockMoveData(*factory, *settings, size);
	}
	
	return (err);
}

/*
 *	UpdatePref	-	"upgrade" a pref to the current version
 */
static short UpdatePref(Handle settings)
{
	if (GetHandleSize(settings) != sizeof(MindySettings))
		GetFactory(settings);

	return (noErr);
}

/*
 *	ComparePrefs
 *
 */
static Boolean ComparePrefs(Handle prefsHand1, Handle prefsHand2)
{
	MindySettings& prefs1	= **(MindySettingsHandle) prefsHand1;
	MindySettings& prefs2	= **(MindySettingsHandle) prefsHand2;
	
	return ((prefs1.mode  == prefs2.mode) && 
			(prefs1.dumpTrees == prefs2.dumpTrees) && 
			(prefs1.noWarnings == prefs2.noWarnings) &&
			(EqualString(prefs1.library, prefs2.library, true, true)) &&
			(EqualString(prefs1.output, prefs2.output, true, true)));
}

static Boolean ComparePrefs(MindySettings& prefs1, MindySettings& prefs2)
{
	return ((prefs1.mode  == prefs2.mode) && 
			(prefs1.dumpTrees == prefs2.dumpTrees) && 
			(prefs1.noWarnings == prefs2.noWarnings) &&
			(EqualString(prefs1.library, prefs2.library, true, true)) &&
			(EqualString(prefs1.output, prefs2.output, true, true)));
}

/*
 *	OutlineRect
 *
 */
static void	OutlineRect(const Rect* focusRect, Boolean outlineOn)
{
	ColorSpec	savedForeColor, backColor;
	PenState	savedPen;
	
	GetPenState(&savedPen);
	PenNormal();
	
	if (!outlineOn)
	{
		SaveFore(&savedForeColor);
		SaveBack(&backColor);
		RestoreFore(&backColor);
	}
	
	PenSize(2, 2);
	FrameRect(focusRect);
	
	SetPenState(&savedPen);
	
	if (!outlineOn)
	{
		RestoreFore(&savedForeColor);
	}
}

/*
 *	DragEnter
 *
 */
static OSErr	DragEnter(PanelParameterBlock *pb)
{
	short			theItem	= pb->itemHit - pb->baseItems;
	unsigned short	itemCount;
	Rect			itemRect;
	OSErr			err;
	
	/* Return paramErr if the user is on a item that can't be dropped on */
	return (paramErr);
}

/*
 *	DragWithin
 *
 */
static void	DragWithin(PanelParameterBlock *pb)
{
#pragma unused(pb)

	/* there's nothing to do */
	
///	SysBreakStr("\preqDragWithin");
}

/*
 *	DragExit
 *
 */
static void	DragExit(PanelParameterBlock *pb)
{
	OSErr	err;
	
///	SysBreakStr("\preqDragExit");
	
	if (sHighlightOn) {
		err = HideDragHilite(pb->dragref);
		if (err == noErr)
			sHighlightOn = false;
	}
}

/*
 *	DragDrop
 *
 */
static void	DragDrop(PanelParameterBlock *pb)
{
	Rect	itemRect;
	
///	SysBreakStr("\preqDragDrop");
	
	DragExit(pb);
}
