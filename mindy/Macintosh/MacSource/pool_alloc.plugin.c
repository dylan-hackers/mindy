/*
 *	pool_alloc.plugin.c
 *
 *	A replacement for pool_alloc.mac.c in the MetroWerks Standard Library.
 *
 *	Goal:
 *		Produce a library that is more amenable to porting Un*x code into 
 *		a CodeWarrior IDE plugin. In particular, we want to
 *		modify malloc() and it's friends so that two things can happen.
 *			1) The Un*x code will malloc from temporary memory.
 *			2) All of the memory can be be recovered when the Un*x source
 *				believes that it has quit.
 *		
 *	by James Jennings
 *	July 21, 1996
 *		
 *	This is a nearly complete rewrite of pool_alloc.mac.c as it appears in:
 *		Metrowerks Standard Library  Version 1.2  07-May-96
 */

#include <assert.h>
#include <Memory.h>
#include "pool_alloc.h"

// New functionality interface. (Functions to export.)
void	free_all_alloc_pools();
void	alloc_uses_temporary_memory( Boolean useTemp );

// defined in "custom alloc.c"
void	uninitialize_memory_pool(); 

typedef void ** PoolHandle;
typedef PoolHandle *PoolHandlePtr;
typedef PoolHandle PoolHandleList[];
typedef PoolHandle **PoolListHandle;

static Boolean use_temporary_memory = false;

static PoolHandlePtr NewPoolListSlot();
static PoolHandlePtr FindPoolListSlot(void *ptr);
static PoolListHandle	poolList = 0;	// poolList is a Handle containing a list of Handles

void * __sys_alloc(mem_size size, struct mem_pool_obj * )
{
	PoolHandlePtr slot;
	OSErr err;
	
	slot = NewPoolListSlot();
	assert ( slot != 0 );
	
	// (slot is dereferenced from poolList, and we dont want it to move)
	HLock( (Handle)poolList );
	
	// Allocate a new handle.
	// Use temporary memory if asked to.
	// Use application heap if temporary memory fails.
	if (use_temporary_memory)
		*slot = (PoolHandle)TempNewHandle( size, &err );
	if ( !use_temporary_memory || *slot == nil || err!=noErr )
		*slot = (PoolHandle)NewHandle( size );
		
	assert( *slot != 0 );
	HUnlock( (Handle)poolList );
	
	HLock( *slot );
	
	return(**slot);
}

void __sys_free(void *ptr, struct mem_pool_obj *)
{
	PoolHandlePtr slot;
	assert( poolList != 0 );
	
	// find the pointer in the poolList
	slot = FindPoolListSlot(ptr);
	assert( slot != 0 && *slot != 0 && **slot != 0 );
	
	// free the handle
	// (slot is dereferenced from poolList, and we dont want it to move)
	HLock( (Handle)poolList );
	
	HUnlock( (Handle)*slot );
	DisposeHandle( (Handle)*slot );
	*slot = 0;
	
	HUnlock( (Handle)poolList );
	
}

static PoolHandlePtr NewPoolListSlot()
{	// Find or make an empty slot in the list.
	// WARNING: returns a pointer to data in an unlocked handle.
	PoolHandlePtr p, q;
	long count;
	const int kInitialSlots = 4;
	const int kIncreaseBySlots = 4;
	
	// Initialize the pool list if necessary
	if ( poolList == 0 ) {
		poolList = (PoolListHandle)NewHandleClear(kInitialSlots*sizeof(Handle));
		assert( poolList != 0 );
	}
	
	// Find an empty slot in the poolList (if there is one)
	count = GetHandleSize( (Handle)poolList )/sizeof(PoolHandle);
	
	p = *poolList;
	q = p + count;
	
	while (p<q) {
		if ( *p == 0 )
			return p;
		p++;
	}
	
	// Couldn't find and empty slot. Make some.
	SetHandleSize( (Handle)poolList, sizeof(PoolHandle) * ( count + kIncreaseBySlots) );
	assert( MemError() == noErr );
	
	// Note: poolList might have moved, so we *must* rebuild p and q.
	p = *poolList + count;
	q = p + kIncreaseBySlots;
	
	while ( p<q ) {
		*(p++) = 0;
	}
	
	return *poolList + count;
}

static PoolHandlePtr FindPoolListSlot(void *ptr)
{	// Find the slot used by a pointer.
	// WARNING: returns a pointer to data in an unlocked handle.
	PoolHandlePtr p, q;
	long count;
	
	assert( poolList != 0 && *poolList != 0 );
	
	// find the slot in the pool list that refers to ptr
	p = *poolList;
	count = GetHandleSize( (Handle)poolList )/sizeof(PoolHandle);
	
	q = p + count;
	
	while (p<q) {
		if ( **p == ptr )
			return p;
		p++;
	}
	
	// Not found.
	return 0;
	
}

void	free_all_alloc_pools()
{
	PoolHandlePtr p, q;
	long count;
	
	if ( poolList == 0 ) return;
	assert( *poolList != 0 );
	
	// loop through the allocated blocks and free them
	p = *poolList;
	count = GetHandleSize( (Handle)poolList )/sizeof(PoolHandle);
	
	q = p + count;
	
	HLock( (Handle)poolList );
	while (p<q) {
		if ( *p != 0 )
			DisposeHandle( (Handle)*p );
		*p = 0;
		p++;
	}
	HUnlock( (Handle)poolList );
	
	DisposeHandle( (Handle)poolList );
	poolList = 0;
	
	// reset the alloc mechanism
	uninitialize_memory_pool();
}

void	alloc_uses_temporary_memory( Boolean useTemp )
{
	use_temporary_memory = useTemp;
}
