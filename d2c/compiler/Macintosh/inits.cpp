// File: inits.cpp
// RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/inits.cpp,v 1.5 2004/04/15 23:51:43 gabor Exp $
// Purpose: present the correct interface to be a CW plugin
// Author: Gabor Greif <gabor@mac.com>
// Status: This version is is based on the Pro6 CW API, but sorely needs cleanup

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


// MacOS headers
#include <Files.h>
#include <Gestalt.h>
#include <Threads.h>

// CW headers
#define CWPLUGIN_LONG_FILENAME_SUPPORT 1
#include <CWPluginErrors.h>
#include <DropInCompilerLinker.h>

// MSL headers
#include <pool_alloc.h>
#include <string.h>
#include <setjmp.h>
#include <cstdlib>
#include <cstdio>

// GC headers
#include "gc.h"



// override stuff in mac-fds.dylan
// later change these methods to be CW friendly:
/*
Link Error   : undefined 'fd_open' (code)
Referenced from 'streamsZstreamsZstream_contents_METH_2' in d2c-runtime.lib
Referenced from 'streamsZstreamsZinitialize_METH_5' in d2c-runtime.lib

Link Error   : undefined 'fd_close' (code)
Referenced from 'streamsZfile_descriptorsZfd_close_METH' in d2c-runtime.lib
Referenced from 'streamsZstreamsZstream_contents_METH_2' in d2c-runtime.lib
Referenced from 'streamsZstreamsZclose_METH_3' in d2c-runtime.lib

Link Error   : undefined 'fd_read' (code)
Referenced from 'streamsZfile_descriptorsZfd_read_METH' in d2c-runtime.lib
Referenced from 'streamsZstreamsZstream_contents_METH_2' in d2c-runtime.lib
Referenced from 'streamsZstreamsZfill_input_buffer_METH' in d2c-runtime.lib
*/
extern "C" int fd_input_available(int fd)
{
    return false;
}

extern "C" int fd_read (int fd, char *buffer, int max_chars)
{
    return 0;
}

extern "C" void fd_exec(char *command, int *toprog, int *fromprog)
{
}

extern "C" void streams_fd_init (void) {
    /* Does nothing */
}

//define sealed method fd_open ( filename :: <string>, flags :: <integer>, mode :: <integer> ) => ( result :: <integer> );
extern "C" int fd_open(char *filename, int flags, int mode)
{
	return false;
}


//define sealed method fd_close ( fd :: <integer> ) => ( result :: <integer> );
extern "C" int fd_close(int fd)
{
	return true;
}

/* done now by linking unistd.mac.c
#define _POSIX
#define __inline
#include <unistd.h>
//extern "C" long _lseek(int a, long b, int c);
extern "C" long lseek(int a, long b, int c)
{
	return _lseek(a, b, c);
}

//extern "C" int _write(int , const void*, __std(size_t));
extern "C" int write(int a, const void* b, __std(size_t) c)
{
	return _write(a, b, c);
}
*/
// override stuff in MactextFileIO.c
// later change these methods to be CW friendly:
/*
Link Error   : undefined 'MacWrite' (code)
Referenced from 'streamsZstreamsZdo_get_output_buffer_METH' in d2c-runtime.lib
Referenced from 'streamsZstreamsZdo_next_input_buffer_METH_2' in d2c-runtime.lib
Referenced from 'streamsZstreamsZdo_next_output_buffer_METH' in d2c-runtime.lib
Referenced from 'streamsZstreamsZdo_force_output_buffers_METH' in d2c-runtime.lib
Referenced from 'streamsZstreamsZdo_get_input_buffer_METH_2' in d2c-runtime.lib
*/
extern "C" int MacWrite(int fd, const char *buf, int count)
{
    return count;
}


extern "C"
{
#define _MSL_CERRNO
#pragma cplusplus off
#	include <runtime.h>
#pragma cplusplus reset
}

#undef dylanZdylan_visceraZtype_error_FUN

extern "C"
void  dylanZdylan_visceraZtype_error_FUN(descriptor_t *orig_sp, descriptor_t A0, heapptr_t A1);

extern "C"
void  dylanZdylan_visceraZtype_error_METH(descriptor_t *orig_sp, descriptor_t A0, heapptr_t A1)
{
	dylanZdylan_visceraZtype_error_FUN(orig_sp, A0, A1);
}


int type__error__LINE__ = 0;
const char* type__error__FILE__ = 0;


extern "C" descriptor_t* allocate_stack(void)
{
	static descriptor_t* always(NULL);
	const size_t dylan_stack(64*1024);

	if (always)
		memset(always, 0, dylan_stack);

	return always ? always : (always = static_cast<descriptor_t*>(GC_malloc_ignore_off_page(dylan_stack)));
}

extern "C" void not_reached(void)
{
	DebugStr("\pShould never come here!; sc");
}
/*
extern "C" void c2pstrcpy(Str255 dst, const char* src)	// in Carbon?еее
{
	Size l = strlen(src);
	*dst = l > 255 ? 255 : l;
	BlockMoveData(src, dst + 1, *dst);
}
*/


static long main_result(0);

extern "C" CWPluginContext plugincontext;
/*static*/ CWPluginContext plugincontext(NULL);


struct mv_result_1 {
    heapptr_t R0;
    heapptr_t R1;
};

/* find-and-load-file-glue{<machine-pointer>, <byte-string>, <sequence>} */
extern "C" struct mv_result_1 warriorZplugin_apiZfind_and_load_file_glue_FUN(descriptor_t *orig_sp, void * A0 /* cb */, heapptr_t A1 /* file-name */, descriptor_t A2);


//define method find-and-open-file
//    (file-name :: <byte-string>, search-paths :: <sequence>)
//    => (stream :: false-or(<stream>), found-loc :: false-or(<byte-string>));

//extern "C" mv_result_1 compiler_baseZfile_systemZfind_and_open_file_METH(descriptor_t *orig_sp, heapptr_t A0, descriptor_t A1)
extern "C" mv_result_1 base_file_systemZbase_file_systemZfind_and_open_file_METH(descriptor_t *orig_sp, heapptr_t A0, descriptor_t A1)
{
	return warriorZplugin_apiZfind_and_load_file_glue_FUN(orig_sp, plugincontext, A0, A1);
}


/// Memory subsystem
//	GC calls NewPtrClear to grab a new block
//	We allocate them by asking the IDE and storing
//	then in a linked list for destruction
//	if cannot satisfy request, return NULL
//	CallbackMemExhausted gets called by GC if all
//	measures do not help

static jmp_buf	GCAllocExit;

extern "C" void abort(void)	// called by invoke-debugger
{
	DebugStr("\pinvoke-debugger called me! -- type g to exit plugin");
	longjmp(GCAllocExit, cwErrRequestFailed);
}

static void* CallbackMemExhausted(size_t bytes_requested)
{
//	DebugStr("\pCallbackMemExhausted called by GC subsystem");
	longjmp(GCAllocExit, cwErrOutOfMemory);
	return NULL;
}

struct GCBlockBase
{
	GCBlockBase*	next;
	
	GCBlockBase(GCBlockBase* nex = NULL) throw() : next(nex) { }

	virtual ~GCBlockBase(void) { delete next; }
	void* operator new(size_t sizme, size_t extra)
	{
		void* block(NULL);
		size_t siz(sizme + extra);
		CWResult	res(CWAllocateMemory(plugincontext, siz, true, &block));
		switch (res)
		{
			case cwNoErr:
				memset(block, 0, siz);
				break;
			case cwErrOutOfMemory:
//				DebugStr("\pcwErrOutOfMemory allocating GC block");
//				longjmp(GCAllocExit, res);
				return NULL;
			default:
//				DebugStr("\pother error allocating GC block");
				longjmp(GCAllocExit, res);
		}
		return block;
	}

	void operator delete(void* block, size_t extra)
	{
		operator delete(block);
	}

	void operator delete(void* block)
	{
		CWResult	res(CWFreeMemory(plugincontext, block, true));
	}

}	GCBlockChain;


struct GCBlock : GCBlockBase
{
	Size	size;
	GCBlock(Size siz) throw() : GCBlockBase(GCBlockChain.next), size(siz) { GCBlockChain.next = this; }
	virtual ~GCBlock(void) { memset(this + 1, 0, size); }
	operator Ptr(void) { return this == NULL ? NULL : Ptr(this + 1); }
};

bool ThisRequest(long desired)
{
	long	req(0);
	return	plugincontext
					&& cwNoErr == CWGetPluginRequest(plugincontext, &req)
					&& req == desired;
}

EXTERN_API( Ptr ) NewPtrClear (Size s)
{
	if (plugincontext)
	{
		if (ThisRequest(reqCompile))
		{
			CWResult res(CWUserBreak(plugincontext));
			if (res != cwNoErr)
				longjmp(GCAllocExit, res);
		}
		return *new(s) GCBlock(s);
	}
	else
		return NULL;
//		return ( Ptr )std::calloc(1, s);
}

void throw_long(long l)
{
	main_result = l;	// globals are ugly!
}

extern "C" void inits(descriptor_t *sp, int argc, char *argv[]);
extern "C" void primary_inits(descriptor_t *sp, int argc, char *argv[]);
extern "C" long warriorZwarriorZplugin_entry_FUN(descriptor_t *orig_sp, void * A0 /* arg */);

extern "C" void primary_inits(descriptor_t *sp, int argc, char *argv[])
{
	GC_set_max_heap_size(55*(1 << 20));
	GC_max_retries = 4;
	GC_oom_fn = CallbackMemExhausted;
	GC_free_space_divisor = 10;
	if (ThisRequest(reqInitialize))
		inits(sp, argc, argv);

	GC_free_space_divisor = 3;
	throw_long( warriorZwarriorZplugin_entry_FUN(sp, plugincontext /* arg */));
}

extern "C" void real_main(int argc, char *argv[]);

void real_main(int argc, char *argv[])
{
    /* Run all the top level initializations. */
    primary_inits(allocate_stack(), argc, argv);
}


template <OSType SELECTOR>
class Gestalter
{
 public:
	Gestalter(void) : result(0), problem(Gestalt(SELECTOR, &result)) { }
	operator long(void) { return result; }
	long	result;
	OSErr	problem;
};

Gestalter<gestaltSystemVersion> systemVersion;
enum { macOSXversion = 10 << 8 };

const bool qStackProfiling(false), qGuardPage(qStackProfiling);

CW_CALLBACK plugin_main(CWPluginContext context)
{//Debugger();
	GC_stackbottom = (char*)&context;
	GC_quiet = true;
	unsigned long	freeStack;
	char*	stackTop;
	unsigned char macsbug[100];
	char*	dest((char*)macsbug + 1);
	
	// stack requirements
	//	initialize: 10 kB
	//	compile: 600 kB
	//	terminate: 5 kB
	// otherwise exit with memError and errormessage

	plugincontext = context;
	
	// Of course on MacOS X we have huge stack space...
	if (systemVersion < macOSXversion && noErr == ThreadCurrentStackSpace(kCurrentThreadID, &freeStack))
	{
		if (ThisRequest(reqCompile))
		{
			if (freeStack < 700000UL)
			{
				CWMessageRef	mess;
				memset(&mess, 0, sizeof mess);

				std::sprintf(dest, "Gwydion Dylan PPC requires 0.7 MB stack space (have %lu bytes)", freeStack);

				CWReportMessage(context, &mess,
												dest,
												"Please assign more compiler stack in \"Build Settings\" IDE preference panel",
												messagetypeError, 0);
				return cwErrSilent;
			}

			if (qStackProfiling)
			{
				stackTop = GC_stackbottom - freeStack;
				memset(stackTop + 128, 'm', freeStack - 2 * 1024);
				
				if (qGuardPage)
				{
					std::sprintf(dest, "Setting guard page; WP #%ld #%ld; g", stackTop + 256, stackTop + 512);
					*macsbug = strlen(dest);
					DebugStr(macsbug);
				}
			}
		}
	}
	
	main_result = cwNoErr;
	char* dummy_argv[]	= { "" };
	int unnormal(setjmp(GCAllocExit));

	CWMessageRef	mess;
	memset(&mess, 0, sizeof mess);

	switch (unnormal)
	{
		case cwNoErr:
			real_main(1, dummy_argv);
			break;
		case cwErrUserCanceled:
			throw_long(cwErrUserCanceled);
			break;
		case cwErrOutOfMemory:
//			DebugStr("\pCATCHING cwErrOutOfMemory");
			CWReportMessage(context, &mess,
											"Out of memory",
											"please assign a larger memory partition to the IDE",
											messagetypeError, 0);
			throw_long(cwErrSilent);
			break;
		default:
//			DebugStr("\pCATCHING unnormal");
			CWReportMessage(context, &mess,
											"An error occurred while allocating GC block",
											"assigning a larger partition to the IDE may help",
											messagetypeError, 0);
			throw_long(unnormal);
			break;
	}

	if (ThisRequest(reqTerminate))
		__malloc_free_all();	// warning! destructors may run after this###

	if (qStackProfiling && ThisRequest(reqCompile))
	{
		if (qGuardPage)
			DebugStr("\pClearing guard page; WPC; g");

		stackTop += 128;
		while (*stackTop++ == 'm') { }
		std::sprintf(dest, "Used stack space: #%ld; g", GC_stackbottom - stackTop);
		*macsbug = strlen(dest);
		DebugStr(macsbug);
	}

	return main_result;
}


extern "C" FSSpec fsSpec(const CWFileSpec& spec);

FSSpec fsSpec(const CWFileSpec& ref)
{
   FSRef newRef;

  FSMakeFSRefUnicode(&ref.parentDirRef,
  ref.filename.length,
  ref.filename.unicode,
  kTextEncodingUnknown,
  &newRef);
  
  
	FSSpec spec;

FSGetCatalogInfo(&newRef, kFSCatInfoNone, NULL, NULL, &spec, NULL);
return spec;

/*
	UInt8 path[1024];
	UInt8* path2;
	FSSpec res;
	int len, count;

	FSRefMakePath(
		&spec.parentDirRef,
		path + 1,
		1024 - 1);

	*path = strlen((const char*)path + 1);

	for(path2 = path + *path + 1, count = 0; count < spec.filename.length; ++count, ++path2)
	{
		*path2 = spec.filename.unicode[count];
	}

	*path2 = 0;
	*path = path2 - path;
	FSMakeFSSpec(0, 0, path, &res);
	return res;*/
}


extern "C" CWFileSpec fileRef(const FSSpec& spec);

CWFileSpec fileRef(const FSSpec& spe)
{
	FSSpec spec(spe);
	int i=0;
	CWFileSpec ref;
	memset(&ref, 0, sizeof(ref));
	ref.filename.length = spec.name[0];
	for(i=0; i < ref.filename.length; ++i)
	{
		ref.filename.unicode[i] = spec.name[i + 1];
	}

	spec.name[0] = 1;
	spec.name[1] = ':';
	FSpMakeFSRef(&spec, &ref.parentDirRef);
	return ref;
}

