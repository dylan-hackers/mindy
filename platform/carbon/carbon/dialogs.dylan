module: carbon

/*
	Dialog Manager
*/


/*
	Includes
*/

c-include( "Carbon.h" );

/*
	Constants
*/

define constant $kControlDialogItem	:: <integer>		= 4;
define constant $kButtonDialogItem	:: <integer>		= 4;
define constant $kCheckBoxDialogItem :: <integer>		= 5;
define constant $kRadioButtonDialogItem	:: <integer>		= 6;
define constant $kResourceControlDialogItem	:: <integer>	= 7;
define constant $kStaticTextDialogItem	:: <integer>		= 8;
define constant $kEditTextDialogItem	:: <integer>		= 16;
define constant $kIconDialogItem	:: <integer>		= 32;
define constant $kPictureDialogItem	:: <integer>		= 64;
define constant $kUserDialogItem	:: <integer>		= 0;
define constant $kItemDisableBit	:: <integer>		= 128;

define constant $kAlertStopAlert	:: <integer> = 0;
define constant $kAlertNoteAlert	:: <integer> = 1;
define constant $kAlertCautionAlert	:: <integer> = 2;
define constant $kAlertPlainAlert	:: <integer> = 3;


/*
	Dialog Types
*/

define functional Class <DialogRef> ( <Ptr> )
end class <DialogRef>;

define constant <ModalFilterUPP> = <UniversalProcPtr>;

define constant $uppModalFilterProcInfo	:: <integer> = 4048;

define constant <AlertType> = <integer>;

define functional Class <AlertStdAlertParam> ( <Ptr> )
end class <AlertStdAlertParam>;


/*
	Alert
*/

define method Alert( alrtID :: <integer>, #key filter = make( <ModalFilterUPP>, pointer: 0 ) )
=> ( result :: <integer> )
	call-out("Alert", int:, short: alrtID, ptr: filter.raw-value );
end method alert;


/*
	GetNewDialog
*/

define method GetNewDialog( dialogID :: <integer> )
=> ( result :: <DialogRef> )

	let ptr = call-out( "GetNewDialog", ptr:, short: dialogID, 
						ptr: $NULL.raw-value, ptr: $NULL.raw-value );

	//- Should throw if couldn't get!!!
        
        make( <DialogRef>, pointer: ptr );
        
end method GetNewDialog;


/*
	SetDialogDefaultItem
*/

define method SetDialogDefaultItem( dialog :: <DialogRef>, item :: <integer> )
=> ( result :: <OSErr> )

	call-out( "SetDialogDefaultItem", int:, ptr: dialog.raw-value, short: item );

end method SetDialogDefaultItem;


/*
	SetDialogCancelItem
*/

define method SetDialogCancelItem( dialog :: <DialogRef>, item :: <integer> )
=> ( result :: <OSErr> )

	call-out( "SetDialogCancelItem", int:, ptr: dialog.raw-value, short: item );

end method SetDialogCancelItem;	


/*
	DisposeDialog
*/

define method DisposeDialog( dialog :: <DialogRef> )
=> ()

	call-out( "DisposeDialog", void:, ptr: dialog.raw-value );

	values();

end method DisposeDialog;


/*
	IsDialogEvent
*/

define method IsDialogEvent( event :: <EventRecord*> )
=> ( result :: <boolean> )
	if( call-out( "IsDialogEvent", int:, ptr: event.raw-value ) = 1 ) #t else #f end if;
end method IsDialogEvent;


/*
	DialogSelect
*/

define method DialogSelect( event :: <EventRecord*> )
=> ( result :: <boolean>, dialog :: <DialogRef>, itemHit :: <integer> )

	let dialog :: <handle> = make( <handle> ); // Mis-use a 4-byte storage space....

	let itemHit = make( <Handle> ); // Mis-use a 4-byte storage space....
	let selected = call-out( "DialogSelect", unsigned-char:, ptr: event.raw-value, ptr: dialog.raw-value, ptr: itemHit.raw-value );
	
	let result = if( selected = 1 ) #t else #f end if;

	values( result, pointer-at( dialog, offset: 0, class: <DialogRef> ), signed-short-at( itemHit ) );

end method DialogSelect;


/*
	GetDialogItem
*/

define method GetDialogItem( theDialog :: <DialogRef>, itemNo :: <integer> )
=> ( itemType :: <integer>, itemHandle :: <ControlHandle>, itemBounds :: <Rect*> )

	let type :: <Handle> = make( <Handle> );	// Handy 4-byte storage
	let handle :: <ControlHandle> = make( <ControlHandle> );
	let bounds :: <Rect*> = make( <Rect*> );

	call-out( "GetDialogItem", void:, ptr: theDialog.raw-value, short: itemNo,
				 ptr: type.raw-value, ptr: handle.raw-value, ptr: bounds.raw-value );

	values( signed-short-at( type ) , pointer-at( handle, offset: 0, class: <ControlHandle> ), bounds );

end method GetDialogItem;


/*
	GetDialogItemText
*/

define method GetDialogItemText( item :: <ControlHandle>, text :: <pascal-string> )
=> ()
	
	call-out( "GetDialogItemText", void:, ptr: item.raw-value, ptr: text.raw-value );
	
	values();
	
end method GetDialogItemText;


/*
	SetDialogItemText
*/

define method SetDialogItemText( item :: <ControlHandle>, text :: <pascal-string> )
=> ()
	
	call-out( "SetDialogItemText", void:, ptr: item.raw-value, ptr: text.raw-value );
	
	values();

end method SetDialogItemText;


/*
	CountDITL
*/

define method CountDITL( dialog :: <DialogRef> )
=> ( result :: <integer> )

	call-out( "CountDITL", short:, ptr: dialog.raw-value );

end method CountDITL;


/*
    Carbon Accessors
*/

/*
    GetDialogWindow
*/

define method GetDialogWindow( dialog :: <DialogRef> )
=> ( result :: <WindowRef> )
    let ptr = call-out( "GetDialogWindow", ptr:, ptr: dialog.raw-value );
    make( <WindowRef>, pointer: ptr );
end method GetDialogWindow;


/*
    GetDialogPort
*/

define method GetDialogPort( dialog :: <DialogRef> )
=> ( result :: <CGrafPtr> )
    let ptr = call-out( "GetDialogPort", ptr:, ptr: dialog.raw-value );
    make( <CGrafPtr>, pointer: ptr );
end method GetDialogPort;


/*
    APPEARANCE
*/

/*
    StandardAlert
*/

define method StandardAlert(	inAlertType :: <AlertType>, inError :: <string>,
                                inExplanation :: <string>, inAlertParam :: <AlertStdAlertParam> )
=>( result :: <integer>, error :: <OSErr> )
    let temp = make( <Handle> ); // 4 bytes of scratch space
    let result = call-out( "StandardAlert", int:, int: inAlertType, ptr: as( <pascal-string>, inError ).raw-value,
                ptr: as( <pascal-string>, inExplanation ).raw-value, ptr: inAlertParam.raw-value, ptr: temp.raw-value);

    values( unsigned-short-at( temp, offset: 0 ), as( <OSErr>, result ) );
end method StandardAlert
