#include "dialogUtils.h"

// Alert resource IDs

#define kAlert 		32760
#define kAlertAbout 32761

#ifndef TRUE
	#define TRUE	1
	#define FALSE 	0
#endif


// HandleSimpleModalDialog

short HandleSimpleModalDialog( DialogPtr dialog, short ok, short cancel )
{
	OSErr 		err;
	short		itemHit;
	
	err = SetDialogDefaultItem( dialog, ok );			// Set the ok( enter ) item 
	err = SetDialogCancelItem( dialog, cancel );		// Set the cancel( command-. ) item
	
	ShowWindow( dialog );								// Now we've intialised, show the dialog
	
	do													// Loop
	{
		ModalDialog( NULL, &itemHit );					// With the system drawing and processing the dialog
		HandleDialogItem( dialog, itemHit );
	} 
	while( 	itemHit != ok && 				// Until the user ok's or cancels
			itemHit != cancel );
			
	HideWindow( dialog );								// Hide the window
	
	return itemHit;										// Retrun the number of the item the user hit
}

// SetRadioButtons

void SetRadioButtons( DialogPtr dialog, short selected, short first, short last )
{
	int 			i;
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	for( i = first; i <= last; i++ )	// Turn all the radio buttons off
	{
		GetDialogItem( dialog, i, &itemType, (Handle*)&itemHandle, &itemRect );
		SetControlValue( itemHandle, FALSE );
	}
	
	// Set the selected one
	
	GetDialogItem( dialog, selected, &itemType, (Handle*)&itemHandle, &itemRect );
	SetControlValue( itemHandle, TRUE );
}

// HandleRadioButton

void HandleRadioButton( DialogPtr dialog, short selected)
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	short numItems;

	short first;
	short last; 
	
	numItems = CountDITL( dialog );		// Find the number of items in the dialog
	
	// Lop down away from the radio button
	for( first = selected; first > 0; first-- )
	{
		GetDialogItem( dialog, first, &itemType, (Handle*)&itemHandle, &itemRect );
		if( itemType != ctrlItem + radCtrl )	// When it's no longer a radio button
		{										// The one after it is the first radio button
			first--;							// So reflect this
			break;
		}
	}
	
	for( last = selected; last <= numItems; last++ )
	{
		GetDialogItem( dialog, last, &itemType, (Handle*)&itemHandle, &itemRect );
		if( itemType != ctrlItem + radCtrl )	// When it's no longer a radio button
		{										// The one after it is the last radio button
			last--;								// So reflect this					
			break;	
		}
	}
	
	SetRadioButtons( dialog, selected, first, last );

}

// HandleCheckBox

void HandleCheckBox( DialogPtr dialog, short box )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	Boolean			state;
	
	GetDialogItem( dialog, box, &itemType, (Handle*)&itemHandle, &itemRect );
	state = GetControlValue( itemHandle );		// Get whether the box is checked or not
	state = ! state;							// Toggle it
	SetControlValue( itemHandle, state );		// Set the toggled state
}

// HandleDialogItem

void HandleDialogItem( DialogPtr dialog, short item )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	
	switch( itemType )
	{
		case ctrlItem + radCtrl:
			HandleRadioButton( dialog, item );
			break;
		case ctrlItem + chkCtrl:
			HandleCheckBox( dialog, item );
		default:
			return;
	}
}

// GetDialogText

void GetDialogText( DialogPtr dialog, short item, Str255 text )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	GetDialogItemText( (Handle)itemHandle, text );
}

// SetDialogText

void SetDialogText( DialogPtr dialog, short item, Str255 text )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	SetDialogItemText( (Handle)itemHandle, text );
}

// GetRadioState

Boolean GetRadioState( DialogPtr dialog, short item )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	return GetControlValue( itemHandle );
}

// SetCheckState

void SetCheckState( DialogPtr dialog, short item, Boolean state )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	SetControlValue( itemHandle, state );
}

// GetCheckState

Boolean GetCheckState( DialogPtr dialog, short item )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	return GetControlValue( itemHandle );
}

// GetPopString

void GetPopString( DialogPtr dialog, short menuRsrc, short item, Str255 text )
{
	short 			itemType;
	ControlHandle 	itemHandle;		// We don't need to dispose of this, it belongs to the OS
	Rect			itemRect;
	
	short			itemSelected;
	short 			menuNum;
	MenuHandle 		menu;

	GetDialogItem( dialog, item, &itemType, (Handle*)&itemHandle, &itemRect );
	
	itemSelected = GetControlValue( itemHandle );
	
	menu = GetMenu( menuRsrc );
	
	GetMenuItemText( menu, itemSelected, text );
	
	DeleteMenu( menuNum );
	ReleaseResource( (Handle)menu );
}

// AlertNote

void AlertNote( Str255 title, Str255 message )
{
	ParamText( title, message, NULL, NULL  );
	NoteAlert( kAlert, NULL );	// Just show an alert then continue when enter is pressed
}

// AlertCaution

void AlertCaution( Str255 title, Str255 message )
{
	ParamText( title, message, NULL, NULL  );
	CautionAlert( kAlert, NULL );	// Just show an alert then continue when enter is pressed
}

// AlertFatal

void AlertFatal( Str255 title, Str255 message )
{
	ParamText( title, message, NULL, NULL  );
	StopAlert( kAlert, NULL );	// Show an alert
	ExitToShell();
}

// AlertAbout

void AlertAbout( void )
{
	Alert( kAlertAbout, NULL );	// Show an alert
}
