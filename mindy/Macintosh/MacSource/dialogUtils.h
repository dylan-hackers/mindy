// dialogs.h

#include<MacTypes.h>
#include<MacWindows.h>
#include<Controls.h>
#include<Dialogs.h>

// Prototypes

short HandleSimpleModalDialog( DialogPtr dialog, short ok, short cancel );
void SetRadioButtons( DialogPtr dialog, short selected, short first, short last );
void HandleRadioButton( DialogPtr dialog, short selected);
void HandleCheckBox( DialogPtr dialog, short box );
void HandleDialogItem( DialogPtr dialog, short item );
void GetDialogText( DialogPtr dialog, short item, Str255 text );
void SetDialogText( DialogPtr dialog, short item, Str255 text );
Boolean GetRadioState( DialogPtr dialog, short item );
void SetCheckState( DialogPtr dialog, short item, Boolean state );
Boolean GetCheckState( DialogPtr dialog, short item );
void GetPopString( DialogPtr dialog, short menuRsrc, short item, Str255 text );

void AlertNote( Str255 title, Str255 message );
void AlertCaution( Str255 title, Str255 message );
void AlertFatal( Str255 title, Str255 message );
void AlertAbout( void );
