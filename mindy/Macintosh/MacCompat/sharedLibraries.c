#include "shl.h"
#include<string.h>

#include<CodeFragments.h>
#include<MacTypes.h>
#include<Memory.h>

shl_t shl_load ( const char *path, int flags, long address)
{
    OSErr 				err;
    
    Str63 				libraryName;
    OSType				architecture = kPowerPCCFragArch;		// PPC code only to avoid UPPs
    CFragLoadOptions	findFlags = kLoadCFrag;
    CFragConnectionID 	connID;
    Ptr 				mainAddr; 
    Str255 				errName;
    
    int 				nameLength = strlen( path );  
    if( nameLength > 63 )								// Convert to pascal string
    	nameLength = 63;
    BlockMove( path, &(libraryName[1]), nameLength );
    libraryName[0] = nameLength;

    err = GetSharedLibrary( libraryName, architecture, 	// Get the library
                            findFlags, &connID, 
                            &mainAddr, errName);
                                 
    return connID;										// Return connection
}

int shl_findsym ( shl_t *handle, const char *sym, short type, void *value)
{
    OSErr 				err;
    
    Str255 				symName;
    CFragSymbolClass	symClass;
    
    int 				nameLength = strlen( sym );  
    if( nameLength > 255 )							// Convert name to pascal string
    	nameLength = 255;
    BlockMove( sym, &(symName[1]), nameLength );
    symName[0] = nameLength;
    
    err = FindSymbol( *handle, symName, value, &symClass);	// Get the symbol
    
    return err == noErr ? 0 : -1;					// Return error code
}
