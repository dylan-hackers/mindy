/*
	Added an accessor for the initialized flag so that we can reset the 
	memory pool.
*/


/*/  Metrowerks Standard Library  Version 1.2  07-May-96  /*/

/*
 *	alloc.c
 *	
 *		Copyright © 1995-1996 Metrowerks, Inc.
 *		All rights reserved.
 *	
 *	Routines
 *	--------
 *		malloc
 *		calloc
 *		realloc
 *		free
 *	
 *	Modification History
 *	--------------------
 *
 *	20-Mar-95 JFH  First code release.
 *  09-Jun-95 JFH  Changed malloc_pool to __malloc_pool as per standard naming conventions.
 */

#include "critical_regions.h"
#include <stdlib.h>
#include "pool_alloc.h"

			 mem_pool_obj	__malloc_pool;
static int					initialized = 0;
/******************** Reset the memory pool ******************/
void	uninitialize_memory_pool();
void	uninitialize_memory_pool()
{
	initialized = 0;
}
/********************** (end of changes) *********************/


void * malloc(size_t size)
{
	void *	block;
	
	__begin_critical_region(malloc_pool_access);
	
	if (!initialized)
	{
		__init_pool_obj(&__malloc_pool);
		initialized = 1;
	}
	
	block = __pool_alloc(&__malloc_pool, size);
	
	__end_critical_region(malloc_pool_access);
	
	return(block);
}

void * calloc(size_t nmemb, size_t size)
{
	void *	block;
	
	__begin_critical_region(malloc_pool_access);
	
	if (!initialized)
	{
		__init_pool_obj(&__malloc_pool);
		initialized = 1;
	}
	
	block = __pool_alloc_clear(&__malloc_pool, nmemb*size);
	
	__end_critical_region(malloc_pool_access);
	
	return(block);
}

void * realloc(void * ptr, size_t size)
{
	void *	block;
	
	__begin_critical_region(malloc_pool_access);
	
	if (!initialized)
	{
		__init_pool_obj(&__malloc_pool);
		initialized = 1;
	}
	
	block = __pool_realloc(&__malloc_pool, ptr, size);
	
	__end_critical_region(malloc_pool_access);
	
	return(block);
}

void free(void * ptr)
{
	if (!initialized)
		return;
	
	__begin_critical_region(malloc_pool_access);
	
	__pool_free(&__malloc_pool, ptr);
	
	__end_critical_region(malloc_pool_access);
}
