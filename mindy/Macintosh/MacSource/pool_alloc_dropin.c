/*
 *	pool_alloc_dropin.c
 *
 *	A replacement for pool_alloc.mac.c in the MetroWerks Standard Library.
 *
 *  Uses the CodeWarrior plugin memory allocation API, which simplifies
 *  memory management significantly, since all allocations are tracked
 *  by the CodeWarrior IDE.
 *
 *  Adapted by Patrick C. Beard from pool_alloc.plugin.c by James Jennings.
 *
 *	This is a complete rewrite of pool_alloc.mac.c as it appears in:
 *		Metrowerks Standard Library  Version 1.2  07-May-96
 */

#include <assert.h>
#include <Memory.h>
#include "pool_alloc.h"

#include <Files.h>
#include <Memory.h>
#include "DropInCompilerLinker.h"
#include "CompilerMapping.h"
#include "CWPluginErrors.h"

// Globally accessible CW plugin context.
CWPluginContext gPluginContext;

const Boolean kTemporaryAllocation = false;

// New functionality interface. (Functions to export.)
void	free_all_alloc_pools();
void	alloc_uses_temporary_memory( Boolean useTemp );

void * __sys_alloc(mem_size size, struct mem_pool_obj * )
{
	void* ptr = NULL;
	if (CWAllocateMemory(gPluginContext, size, kTemporaryAllocation, &ptr) == cwNoErr)
		return ptr;
	return NULL;
}

void __sys_free(void *ptr, struct mem_pool_obj *)
{
	if (ptr != NULL)
		CWFreeMemory(gPluginContext, ptr, kTemporaryAllocation);
}

void free_all_alloc_pools()
{
	// this is a no-op, because CW will automatically free all allocated memory.
}

void	alloc_uses_temporary_memory( Boolean useTemp )
{
	// this is a no-op, because CW will automatically use temporary memory.
}
