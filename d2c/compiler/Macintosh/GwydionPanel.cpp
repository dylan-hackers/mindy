// File: GwydionPanel.cpp
// RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/GwydionPanel.cpp,v 1.2 2002/04/03 23:17:33 gabor Exp $
// Purpose: implement the programmatic part of CW preference panel
// Author: Gabor Greif <gabor@mac.com>
// Todo:	AEErr and CWErr checks (throws)
// 				HLock(curr) i not yet...
// Status: needs some cleanup, some messages not yet supported

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


#define CW_STRICT_DIALOGS 1

/* current version number for our prefs data */
#define PANELVERSION		1


#include <Resources.h>
#include <Sound.h>
#include <DropInPanel.h>
#include <CWPluginErrors.h>
#include <cstring>
#include <cstdio>

const short kItemListID = 703;

struct _
{
	_(OSErr err) throw(OSErr) { if (err != noErr) throw err; }
};

inline OSErr InitDialog(PanelParameterBlock* context)
try
{
	(_)CWPanlAppendItems(context, kItemListID);
	return noErr;
}
catch (OSErr problem)
{
	return problem;
}

inline void TermDialog(PanelParameterBlock*)
{
}

#pragma options align=mac68k

struct PluginPrefBase
{
	short	version;
	short	explicitNextMethod:1;
	short	reserved1_1:1;
	short	reserved1_2:1;
	short	reserved1_3:1;
	short	reserved1_4:1;
	short	reserved1_5:1;
	short	reserved1_6:1;
	short	reserved1_7:1;
	short	reserved1_8:1;
	short	reserved1_9:1;
	short	reserved1_10:1;
	short	reserved1_11:1;
	short	reserved1_12:1;
	short	reserved1_13:1;
	short	reserved1_14:1;
	short	reserved1_15:1;

	short	groupForGen:1;
	short	emitDebug:1;
	short	emitCommandLine:1;
	short	reserved2_3:1;
	short	reserved2_4:1;
	short	reserved2_5:1;
	short	reserved2_6:1;
	short	reserved2_7:1;
	short	reserved2_8:1;
	short	reserved2_9:1;
	short	reserved2_10:1;
	short	reserved2_11:1;
	short	reserved2_12:1;
	short	reserved2_13:1;
	short	reserved2_14:1;
	short	reserved2_15:1;

	short	overrideLid:1;
	short	reserved3_1:1;
	short	reserved3_2:1;
	short	reserved3_3:1;
	short	reserved3_4:1;
	short	reserved3_5:1;
	short	reserved3_6:1;
	short	reserved3_7:1;
	short	reserved3_8:1;
	short	reserved3_9:1;
	short	reserved3_10:1;
	short	reserved3_11:1;
	short	reserved3_12:1;
	short	reserved3_13:1;
	short	reserved3_14:1;
	short	reserved3_15:1;
	
	unsigned long	baseID;
	static const unsigned long noIDSpecified = 0xFFFFFFFF;
	CWRelativePath	outputFolder;
};

struct PluginPref : PluginPrefBase
{
	unsigned char	generatedGroup[1];
	Size size(void) const { return sizeof(PluginPrefBase) + *generatedGroup + 1; }
	Size padded_size(void) const { return (size() + 1) & ~1UL; }
};

#pragma options align=reset

typedef PluginPref *PluginPrefPtr, **PluginPrefHandle;

struct Locked
{
	Handle&	h;
	Locked(PluginPrefHandle& _h) : h(reinterpret_cast<Handle&>(_h)) { HLock(h); }
	Locked(Handle& _h) : h(_h) { HLock(_h); }
	~Locked(void) { HUnlock(h); }
	PluginPrefPtr operator -> (void) { return reinterpret_cast<PluginPrefPtr>(*h); }
	PluginPref& operator * (void) { return *reinterpret_cast<PluginPrefPtr>(*h); }
	Size size(void) const { return GetHandleSize(h); }
	operator PluginPrefHandle(void) { return reinterpret_cast<PluginPrefHandle>(h); }
};

//######## GetFactory -- fill up handle with factory settings
inline OSErr GetFactory(Handle settings)
{
	OSErr result(noErr);

	Handle	res = Get1Resource('pref', 128);
	if (res)
	{
		Size size(GetHandleSize(res));
		SetHandleSize(settings, size);
		OSErr	err(MemError());
		
		if (err == noErr)
			BlockMoveData(*res, *settings, size);

		return err;
	}
	else
		return ResError() == noErr ? resNotFound : ResError();
}


/*
 *	PutData		-	copy the options data from the handle to the screen
 *
 */
 
 enum { explicitNextMethodCheck = 1,
 				groupForGenCheck,
 				dummy,
 				generatedGroupText,
 				emitDebugCheck,
 				emitCommandLineCheck,
 				overrideLidCheck,
 				baseIDText,
 				
 				outputDirText = 10,
 				outputDirButton,
 				outputDirClearButton
 			};

static OSErr CWPanlGetItemInteger(PanelParamBlkPtr context, long whichItem, unsigned long& val)
{
	unsigned char	buf[20];
	OSErr		err = CWPanlGetItemText(context, whichItem, buf, 19);
	if (err != noErr)
		return err;

	char*	read((char*)(buf + 1));
	*(read + *buf) = 0;
	if (std::sscanf(read, "%u", &val) != 0)
		return noErr;
	else return cwErrSilent;	// good error? see api ref for CWPanlGetItemText
}


static OSErr CWPanlSetItemInteger(PanelParamBlkPtr context, long whichItem, unsigned long val)
{
	unsigned char	buf[20];
	char*	write((char*)(buf + 1));
	std::sprintf(write, "%u", val);
	*buf = std::strlen(write);

	return CWPanlSetItemText(context, whichItem, buf);
}

static OSErr CWPanlSetItemPath(PanelParamBlkPtr context, long whichItem, CWRelativePath path)
{
	unsigned char pathString[1000];
	long maxLength = sizeof pathString - 1;
	OSErr	err(CWPanlGetRelativePathString(context, &path, (char*)pathString + 1, &maxLength));
	if (err != noErr)
		return err;
	*pathString = std::strlen((char*)pathString + 1);
	return CWPanlSetItemText(context, whichItem, pathString);
}

inline void PutData(PanelParameterBlock *context, const PluginPref& options)
{
	OSErr		err = CWPanlSetItemText(context, generatedGroupText, options.generatedGroup);
	err = CWPanlSetItemValue(context, explicitNextMethodCheck, !options.explicitNextMethod);
	err = CWPanlSetItemValue(context, groupForGenCheck, !options.groupForGen);
	err = CWPanlEnableItem(context, generatedGroupText, options.groupForGen);
	err = CWPanlSetItemValue(context, emitDebugCheck, bool(options.emitDebug));
	err = CWPanlSetItemValue(context, emitCommandLineCheck, bool(options.emitCommandLine));
	err = CWPanlSetItemValue(context, overrideLidCheck, bool(options.overrideLid));

	if (options.baseID != PluginPref::noIDSpecified)
		err = CWPanlSetItemInteger(context, baseIDText, options.baseID);
	else
		err = CWPanlSetItemText(context, baseIDText, "\p");

	err = CWPanlSetItemPath(context, outputDirText, options.outputFolder);
}

/*
 *	GetData		-	copy the options data from screen to the handle
 *
 */

static void Beep(PanelParameterBlock* context, long item) throw(OSErr)
{
	SysBeep(10);
	CWPanlActivateItem(context, item);
	throw OSErr(userCanceledErr);
}


template <int ID>
inline bool GetData(PanelParameterBlock* context, bool noisy) throw(OSErr)
{
	long	value(0);
	OSErr	err(CWPanlGetItemValue(context, ID, &value));
	if (err != noErr && noisy)
		::Beep(context, ID);
	return value;
}

template <int ID, int SIZE>
inline void GetData(PanelParameterBlock* context, unsigned char* fill, bool noisy) throw(OSErr)
{
	OSErr	err(CWPanlGetItemText(context, ID, fill, SIZE));
	if (err != noErr && noisy)
		::Beep(context, ID);
}

template <int ID>
inline void GetData(PanelParameterBlock* context, unsigned long& i, bool noisy) throw(OSErr)
{
	OSErr	err(::CWPanlGetItemInteger(context, ID, i));
	if (err != noErr && noisy)
		::Beep(context, ID);
}

struct Unlocked
{
	Unlocked(Handle _h) { HUnlock(_h); }
};

template <Size SIZE, bool increase>
struct Resized : Unlocked
{
	Resized(Handle _h, Size by = SIZE) : Unlocked(_h)
	{
		Size curr(increase ? GetHandleSize(_h) : 0);
		SetHandleSize(_h, curr + by);
	}
};

template <Size SIZE>
struct BackSizer : Resized<SIZE, true>, Locked
{
	BackSizer(PluginPrefHandle& _h) : Resized<SIZE, true>(reinterpret_cast<Handle&>(_h)), Locked(_h) { }
	~BackSizer(void) { Unlocked(this->h); SetHandleSize(h, (**this).padded_size()); }
};

static OSErr GetData(PanelParameterBlock* context, PluginPrefHandle options, bool noisy)
try
{
	BackSizer<256>	lock(options);

	PluginPref&	opts(**options);
	
	GetData<generatedGroupText, 255>(context, opts.generatedGroup, noisy);
	opts.explicitNextMethod = !GetData<explicitNextMethodCheck>(context, noisy);
	opts.groupForGen = !GetData<groupForGenCheck>(context, noisy);
	opts.emitDebug = GetData<emitDebugCheck>(context, noisy);
	opts.emitCommandLine = GetData<emitCommandLineCheck>(context, noisy);
	opts.overrideLid = GetData<overrideLidCheck>(context, noisy);
	GetData<baseIDText>(context, opts.baseID = PluginPref::noIDSpecified, noisy);

	return noErr;
}
catch(OSErr problem)
{
	return problem;
}


/*
 *	ComparePrefs
 *
 */

static bool	operator == (const PluginPref& lhs, const PluginPref& rhs)
{
	return /*lhs.size() == rhs.size() && */0 == std::memcmp(&lhs, &rhs, lhs.size());
}

inline static bool	operator != (const PluginPref& lhs, const PluginPref& rhs)
{
	return !operator == (lhs, rhs);
}

//######## Validate -- determine if we need to reset paths, recompile, or relink
inline OSErr Validate(PanelParameterBlock *context)
{
	Locked	curr(context->currentPrefs);
	Locked	orig(context->originalPrefs);
	
	context->recompile = *curr != *orig;
	return noErr;
}


/*struct WithoutPP
{
	PanelParameterBlock* const	context;
	WithoutPP(PanelParameterBlock* c) : context(c) { CWPreDialog(c); }
	~WithoutPP(void) { CWPostDialog(context); }
};
*/

template <bool DEL = false, bool CREAT = false>
struct reco
{
	AERecord&	rec;
	reco(AERecord& r) : rec(r) { if (CREAT) AECreateList(NULL, 0, true, &r); }
	~reco(void) { if (DEL) AEDisposeDesc(&rec); }
};

template <DescType CODE>
struct enu
{
	typedef DescType data;
	void operator () (AERecord& r, DescType k) const { DescType d(CODE); AEPutKeyPtr(&r, k, typeEnumerated, &d, sizeof d); }
};

template <AEKeyword KEY, typename DATA>
struct keyed : DATA
{
	keyed(void) { }
	keyed(const typename DATA::data& d) : DATA(d) { }
	void operator () (AERecord& r) const { DATA::operator() (r, KEY); }
};

template <typename DATA, AEKeyword KEY, bool DEL, bool CREAT>
const reco<DEL, CREAT>& operator << (const reco<DEL, CREAT>& rec, const keyed<KEY, DATA>& data)
{
	data(rec.rec);
	return rec;
}

template <AEKeyword KEY, typename DATA>
keyed<KEY, DATA> key(void)
{
	return keyed<KEY, DATA>();
}


/*
 *	ItemHit		-	handle an itemHit in a Preferences panel
 *
 */

static void ItemHit(PanelParameterBlock *context)
{
	short	theItem	= context->itemHit - context->baseItems;
	Locked	lock(context->currentPrefs);

	GetData(context, lock, false);

	switch (theItem)
	{
		case groupForGenCheck:
		{
			CWPanlEnableItem(context, generatedGroupText, lock->groupForGen);
			break;
		}
/*		case 3:
		{
			break;
		}*/
		case outputDirClearButton:
		{
			lock->outputFolder = PluginPrefPtr(*context->factoryPrefs)->outputFolder;
			/*err = */CWPanlSetItemPath(context, outputDirText, lock->outputFolder);
			break;
		}
		case outputDirButton:
		{
			CWRelativePath	path;
			std::memset(&path, 0, sizeof path);
			path.version = kCurrentCWRelativePathVersion;
			path.pathFormat = format_Mac;
			path.pathType = type_Project;
			std::strcpy(path.pathString, ":");
//			OSType	dummy('TEXT');
			OSErr err(CWPanlChooseRelativePath(context, &path, true, 0, NULL, "Folder for generated .c files"));
			if (err == noErr)
			{
				lock->outputFolder = path;
				err = CWPanlSetItemPath(context, outputDirText, lock->outputFolder);
			}

/*			char hehe[1000];
			unsigned char pathString[1000];
			std::memset(hehe, 0, sizeof hehe);
			hehe[0] = 1;
//			hehe[1] = 0;
			hehe[2] = 3;	// 1: Project, 2: Compiler, 3: System
//			hehe[3] = 1;	// 2: ?
			hehe[4] = ':';
//			hehe[256+4] = ':';
//			hehe[256+5] = 'g';
			std::strcpy(hehe+256+4, ":Preferences");
//			Debugger();
//			CWPanlSetRelativePathValue(context, "{Project}:", (CWRelativePath*) hehe);
			long maxLength = 999;
			CWPanlGetRelativePathString(context, (CWRelativePath*) hehe, (char*)pathString + 1, &maxLength);
			*pathString = std::strlen((char*)pathString + 1);
			CWPanlSetItemText(context, outputDirText, pathString);
//			Debugger();
			AEDesc	desc = { 'null', 0 };*/
/*			
			reco
			key: pnamTEXT
			key: Frmt enum PthF: PFGn | PFMc | PFWn | PFUx;
			key: OrigPPrm enum type PPrm: Abso | PRel | SRel | YRel | RRel;
			key: RootTEXT
			//key: Recubool
*/
/*			CWPanlWriteRelativePathAEDesc(context, (CWRelativePath*) hehe, desc);
			
			reco<>(desc)
						<< key<'Frmt', enu<'PFMc'> >()
						<< key<'Orig', enu<'PRel'> >();

			CWPanlReadRelativePathAEDesc(context, (CWRelativePath*) hehe, desc);
			
			OSType	dummy('TEXT');
			CWPanlChooseRelativePath(context, (CWRelativePath*) hehe, false, 1, &dummy, "" / *"Folder for generated .c files"* /);
			*/
		}
//		default: break;
	}
	
	context->canRevert	= **PluginPrefHandle(context->originalPrefs) != *lock;
	context->canFactory	= **PluginPrefHandle(context->factoryPrefs) != *lock;
}

/*
 *	UpdatePref	-	"upgrade" a pref to the current version
 *
 */

static OSErr UpdatePref(PluginPrefHandle settings)
{
	OSErr	err(noErr);
	
	Locked	lsettings(settings);
	
	switch (lsettings->version)
	{
		case PANELVERSION:
		break;
		
		default:
			return kBadPrefVersion;
	}
	
	return err;
}

enum	// make sync with dialog IDs
{
	implicitNextMethodProp = 'DY01',
	overrideLidProp,
	baseIDProp,
	groupForGenProp = 'DY11',
	generatedGroupProp,
	emitDebugProp,
	emitCommandLineProp,
	outputDirProp
};

static OSErr GetAEData(PanelParameterBlock* context, const PluginPref& curr, AEKeyword keyword, AEDesc& desc)
try
{
	switch (keyword)
	{
		case implicitNextMethodProp:
			desc.descriptorType = curr.explicitNextMethod ? typeFalse : typeTrue;
			break;
		case groupForGenProp:
			desc.descriptorType = curr.groupForGen ? typeTrue : typeFalse;
			break;
		case generatedGroupProp:
			return AECreateDesc(typeChar, curr.generatedGroup + 1, *curr.generatedGroup, &desc);
		case emitDebugProp:
			desc.descriptorType = curr.emitDebug ? typeTrue : typeFalse;
			break;
		case emitCommandLineProp:
			desc.descriptorType = curr.emitCommandLine ? typeTrue : typeFalse;
			break;
		case overrideLidProp:
			desc.descriptorType = curr.overrideLid ? typeTrue : typeFalse;
			break;
		case baseIDProp:
			return AECreateDesc(typeLongInteger, &curr.baseID, sizeof curr.baseID, &desc);
		case outputDirProp:
			return CWPanlWriteRelativePathAEDesc(context, &curr.outputFolder, &desc);
	}
	return noErr;
}
catch (OSErr err)
{
	return err;
}

template <DescType TO, bool DEL = true>
struct AsDesc : AEDesc
{
	AsDesc(const AEDesc& d) { AECoerceDesc(&d, TO, this); }
	~AsDesc(void) { if (DEL) AEDisposeDesc(this); }
	
	template <typename AS>
	AS as(void) { return *reinterpret_cast<AS*>(*dataHandle); }
};


template <typename AS>
struct AETraits;

template <> struct AETraits<bool> { enum { is = typeBoolean }; };
template <> struct AETraits<unsigned long> { enum { is = typeLongInteger }; };

template <typename AS>
AS GetData(const AEDesc& desc)
{
	return AsDesc<AETraits<AS>::is>(desc).template as<AS>();
}
/*
template <long SIZE>
void GetStringData(const AEDesc& desc, char (*arr)[SIZE])
{
	AsDesc<typeChar>	coer(desc);
	Size mi(min(SIZE - 1, GetHandleSize));
	BlockMoveData(*coer.dataHandle, arr, mi);
	arr[mi] = 0;
}

template <long SIZE>
void GetStringData(const AEDesc& desc, unsigned char (*arr)[SIZE])
{
	AsDesc<typeChar>	coer(desc);
	Size mi(min(SIZE - 1, GetHandleSize(coer.dataHandle)));
	BlockMoveData(*coer.dataHandle, *arr + 1, mi);
	arr[mi] = 0;
}
*/	// CW cannot do this!

//template <long SIZE>
static void GetStringData(const AEDesc& desc, unsigned char (*arr)[1])
{
	AsDesc<typeChar>	coer(desc);
//	Size mi(min(SIZE - 1, GetHandleSize(coer.dataHandle)));
	const Size mi(GetHandleSize(coer.dataHandle));
	BlockMoveData(*coer.dataHandle, *arr + 1, mi);
	(*arr)[0] = mi;
}

static OSErr SetAEData(PanelParameterBlock* context, AEKeyword keyword, const AEDesc& desc, PluginPref& curr)
try
{
	switch (keyword)
	{
		case implicitNextMethodProp:
			curr.explicitNextMethod = !GetData<bool>(desc);
			break;
		case groupForGenProp:
			curr.groupForGen = GetData<bool>(desc);
			break;
		case generatedGroupProp:
		{
			BackSizer<256>	lock(*(PluginPrefHandle*)(&context->currentPrefs));
			GetStringData/*<1>*/(desc, &curr.generatedGroup);
			break;
		}
		case emitDebugProp:
			curr.emitDebug = GetData<bool>(desc);
			break;
		case emitCommandLineProp:
			curr.emitCommandLine = GetData<bool>(desc);
			break;
		case overrideLidProp:
			curr.overrideLid = GetData<bool>(desc);
			break;
		case baseIDProp:
			curr.baseID = GetData<unsigned long>(desc);
			break;
		case outputDirProp:
			return CWPanlReadRelativePathAEDesc(context, &curr.outputFolder, &desc);
	}
	return noErr;
}
catch (OSErr err)
{
	return err;
}


static OSErr WriteXMLData(PanelParameterBlock* context, const PluginPref& curr)
try
{
	(_)CWPanlWriteBooleanSetting(context, "gwydion-dylan--always-define-next-method", !curr.explicitNextMethod);

	(_)CWPanlWriteBooleanSetting(context, "gwydion-dylan--generate-in-group", curr.groupForGen);
	char generatedGroup[256];
	BlockMoveData(curr.generatedGroup + 1, generatedGroup, *curr.generatedGroup);
	generatedGroup[*curr.generatedGroup] = 0;
	(_)CWPanlWriteStringSetting(context, "gwydion-dylan--group-name", generatedGroup);
	(_)CWPanlWriteBooleanSetting(context, "gwydion-dylan--emit-debug", curr.emitDebug);
	(_)CWPanlWriteBooleanSetting(context, "gwydion-dylan--emit-command-line", curr.emitCommandLine);
	(_)CWPanlWriteRelativePathSetting(context, "gwydion-dylan--output-folder", &curr.outputFolder);

	(_)CWPanlWriteBooleanSetting(context, "gwydion-dylan--emit-override-lid", curr.overrideLid);
	(_)CWPanlWriteIntegerSetting(context, "gwydion-dylan--unique-ID", curr.baseID);
	return noErr;
}
catch (OSErr err)
{
	return err;
}

static OSErr ReadXMLData(PanelParameterBlock* context, PluginPref& curr)
try
{
//	DebugStr("\pat this point we have factoryPrefs");

	Boolean	b;
	OSErr	result(CWPanlReadBooleanSetting(context, "gwydion-dylan--always-define-next-method", &b));
	result == noErr && (curr.explicitNextMethod = !b);

	result = CWPanlReadBooleanSetting(context, "gwydion-dylan--generate-in-group", &b);
	result == noErr && (curr.groupForGen = b);

	const char*	s;
	result = CWPanlReadStringSetting(context, "gwydion-dylan--group-name", &s);
	if (result == noErr)
	{
		*curr.generatedGroup = std::strlen(s);
		BlockMoveData(s, curr.generatedGroup + 1, *curr.generatedGroup);
	}

	result = CWPanlReadBooleanSetting(context, "gwydion-dylan--emit-debug", &b);
	result == noErr && (curr.emitDebug = b);
	result = CWPanlReadBooleanSetting(context, "gwydion-dylan--emit-command-line", &b);
	result == noErr && (curr.emitCommandLine = b);
	CWRelativePath	r;
	std::memset(&r, 0, sizeof r);
	result = CWPanlReadRelativePathSetting(context, "gwydion-dylan--output-folder", &r);
	if (result == noErr)
		BlockMoveData(&r, &curr.outputFolder, sizeof r);
/*	else
		BlockMoveData(&PluginPrefPtr(*context->factoryPrefs)->outputFolder, &curr.outputFolder, sizeof r);
*/

	result = CWPanlReadBooleanSetting(context, "gwydion-dylan--emit-override-lid", &b);
	result == noErr && (curr.overrideLid = b);
	long	i;
	result = CWPanlReadIntegerSetting(context, "gwydion-dylan--unique-ID", &i);
	result == noErr && (curr.baseID = i);
	
	return noErr;
}
catch (OSErr err)
{
	return err;
}

#pragma mark main

CWPLUGIN_ENTRY(main) (PanelParameterBlock* context)
{
	OSErr	result(noErr);
//Debugger();
	
	// Dispatch on compiler request
	switch (context->request)
	{
		case reqInitialize:
			// panel has just been loaded into memory
			break;

		case reqTerminate:
			// panel is about to be unloaded from memory
			break;

		case reqInitDialog:
			// hook our dialog item list into the preferences dialog
			result = InitDialog(context);
			break;
			
		case reqTermDialog:
			// unhook our dialog item list from the preferences dialog
			TermDialog(context);
			break;
		
		case reqPutData:
			// put the data in the given handle into our dialog items */
			{
				Locked	lock(context->currentPrefs);
				PutData(context, **PluginPrefHandle(context->currentPrefs));
			}
			break;

		case reqGetData:
			// fill in the given handle with our dialog items
			result = GetData(context, PluginPrefHandle(context->currentPrefs), true);
			break;

		case reqAEGetPref:			/* get preference setting for AppleEvent request		*/
			result = GetAEData(context, **PluginPrefHandle(context->currentPrefs), context->prefsKeyword, context->prefsDesc);
			break;

		case reqAESetPref:			/* set preference setting from AppleEvent request		*/
			result = SetAEData(context, context->prefsKeyword, context->prefsDesc, **PluginPrefHandle(context->currentPrefs));
			break;

		case reqWriteSettings:			/* get preference setting for XML request		*/
		{
			Locked	lock(context->currentPrefs);
			result = WriteXMLData(context, *lock);
		}
		break;

		case reqFirstLoad:
		{
			result = noErr;
			break;
		}

		case reqReadSettings:			/* set preference setting for XML request		*/
		{
			result = GetFactory(context->currentPrefs);
			if (result == cwNoErr)
			{
				BackSizer<sizeof(PluginPref) + 256>	lock(*(PluginPrefHandle*)(&context->currentPrefs));
				result = ReadXMLData(context, *lock);
			}
		}
		break;

		case reqByteSwapData:
			// byte swap the data in the handle
//			result = ByteSwapData(context);
			break;
		
		case reqFilter:
			// filter an event in the dialog
			result = cwErrRequestFailed;
			break;
			
		case reqItemHit:
			// handle a hit on one of our dialog items
			ItemHit(context);
			break;
			
		case reqValidate:
			// determine if we need to reset paths, recompile, or relink
			result = Validate(context);
			break;

		case reqGetFactory:
			// return our factory settings
			result = GetFactory(context->factoryPrefs);
			break;

		case reqUpdatePref:
			// update the given handle to use the current format for our prefs data
			result = UpdatePref(PluginPrefHandle(context->currentPrefs));
			break;
			
		default:
//			Debugger();
			result = cwErrRequestFailed;
			break;
	}

	// Return result code
	return result;
	
} // main


#ifdef OLDSTYLE

pascal short main(PanelParameterBlock *pb)
{
	switch (pb->request)
	{
	case reqInitPanel:
		/* panel has just been loaded into memory */
		break;

	case reqTermPanel:
		/* panel is about to be unloaded from memory */
		break;

	case reqInitDialog:
		/* hook our dialog item list into the preferences dialog */
		return InitDialog(pb);
		
	case reqTermDialog:
		/* unhook our dialog item list from the preferences dialog */
		TermDialog(pb);
		break;
	
	case reqPutData:
		/* put the data in the given handle into our dialog items */
//		PutData(pb, pb->currentPrefs);
		break;

	case reqGetData:
		/* fill in the given handle with our dialog items */
//		result = GetData(pb, pb->currentPrefs, true);
		break;

	case reqFilter:
		/* filter an event in the dialog */
//		result = Filter(pb, pb->event, &pb->itemHit);
		break;
		
	case reqItemHit:
		/* handle a hit on one of our dialog items */
//		ItemHit(pb);
		break;
		
	case reqDrawCustomItem:
		/* handle a request to draw one of our user items (CW/8 and later) */
/*		theItem = pb->itemHit - pb->baseItems;
		switch (theItem)
		{
			case kBoxItem1:
				PanelDrawBox(pb, theItem, kBoxItem1Str);
			break;
			case kBoxItem2:
				PanelDrawBox(pb, theItem, kBoxItem2Str);
			break;
			case kBoxItem3:
				PanelDrawBox(pb, theItem, kBoxItem3Str);
			break;
			case kBoxItem5:
			case kBoxItem6:
				PanelDrawBox(pb, theItem, 0);
			break;
		}*/
		break;
		
	case reqActivateItem:
//		theItem = pb->itemHit - pb->baseItems;
		break;
		
	case reqDeactivateItem:
//		theItem = pb->itemHit - pb->baseItems;
		break;
		
	case reqHandleKey:
//		theItem = pb->itemHit - pb->baseItems;
//		HandleKey(pb, pb->event);
		break;
		
	case reqHandleClick:
//		theItem = pb->itemHit - pb->baseItems;
//		HandleClick(pb, pb->event);
		break;
		
	case reqFindStatus:
//		FindStatus(pb);
		break;
		
	case reqObeyCommand:
//		ObeyCommand(pb);
		break;
		
	case reqAEGetPref:
		/* return one item in the given handle as an Apple Event descriptor */
//		result = GetPref(pb->prefsKeyword, &pb->prefsDesc, pb->currentPrefs);
		break;

	case reqAESetPref:
		/* change one item in the given handle according to the given Apple Event descriptor */
//		result = SetPref(pb->prefsKeyword, &pb->prefsDesc, pb->currentPrefs);
		break;

	case reqValidate:
		/* determine if we need to reset paths, recompile, or relink */
//		Validate(pb->originalPrefs, pb->currentPrefs, &pb->recompile, &pb->relink, &pb->reset);
		break;

	case reqGetFactory:
		/* return our factory settings */
		result = GetFactory(pb->factoryPrefs);
		break;

	case reqUpdatePref:
		/* update the given handle to use the current format for our prefs data */
//		result = UpdatePref(pb->currentPrefs);
		break;
		
	case reqDragEnter:
		/* determine if we can accept the drag and, if so, start tracking */
//		result = DragEnter(pb);
		break;
	
	case reqDragWithin:
		/* continue tracking */
//		DragWithin(pb);
		break;
	
	case reqDragExit:
		/* stop tracking */
//		DragExit(pb);
		break;
	
	case reqDragDrop:
		/* the user has dropped in our panel */
//		DragDrop(pb);
		break;
	
	default:
		return paramErr;
	}
	
	return noErr;
}

#endif