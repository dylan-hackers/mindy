/* MindyCompLib.c */

/* Make sure this gets linked before the standard library!!!! */

/* includes */

#include<setjmp.h>
#include<stdio.h>
#include<stdlib.h>

#include<ansi_files.h>

// #defines

#define MESSAGE_BUFFER_SIZE		4096

// typedefs

typedef void (*ShowCompilerMessagePtr)( char * message );


/* prototypes */

extern void	free_all_alloc_pools( void );	/* from the PCCTS malloc lib */
extern int main( int argc, char *argv[] );

int MindyComp( int argc, char *argv[], FILE * newStderr );
void InitializeAnsi( FILE * newStderr );
void FinalizeAnsi( void );



/* globals */

jmp_buf		gJmp;

FILE * gSTDERR;


/* functions */

/* MindyComp */

int MindyComp( int argc, char *argv[], FILE * newStderr )
{
	int result;						/* For the setjmp result */

	InitializeAnsi( newStderr );
	
	result = setjmp( gJmp );		/* Set the longjump point */
	if( result )					/* If we've longjumped */
		goto bail;
	
	result = main( argc, argv );	/* Call main with argC and argV */

	bail:

	FinalizeAnsi();
	return result;
}

/* 	exit */
/* 	We do not want people to exit our main app, so we longjmp
	back to where we setjmp in main, from where we can exit to
	the calling app.
*/

void exit( int code )
{
	longjmp( gJmp, 1 );
}

void InitializeAnsi( FILE * newStderr )
{
	*gSTDERR = *stderr;
	*stderr = *newStderr;
}

void FinalizeAnsi( void )
{
	*stderr = *gSTDERR;
	__close_all();
	
	free_all_alloc_pools();
}
