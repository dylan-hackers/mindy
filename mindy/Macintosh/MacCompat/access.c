#include<string.h>
#include<Files.h>
#include<MacTypes.h>
#include<Processes.h>

static int dirid = 0;

int access( char * file, int rights )
{
	CInfoPBRec cipbr;
	HFileInfo *fpb = (HFileInfo*) &cipbr;
	DirInfo *dpb = (DirInfo*) &cipbr;
	FSSpec spec;
	Str255 pname;
	short err;

	/* Make a temp copy of the name and pascalize. */
	strcpy ((char *) pname, file);
	c2pstr (pname);

	cipbr.dirInfo.ioDrDirID = dirid;
	cipbr.hFileInfo.ioNamePtr = pname;
	cipbr.hFileInfo.ioVRefNum = 0;
	cipbr.hFileInfo.ioFDirIndex = 0;
	cipbr.hFileInfo.ioFVersNum = 0;
	
	err = PBGetCatInfo (&cipbr, 0);
	if (err != noErr)
	{
	  return -1;
	}
	
	return 0;
}