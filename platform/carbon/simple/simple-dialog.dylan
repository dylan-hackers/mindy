module: simple

/*
	NOTES.
	
	-Why are dialogs created from resources when windows are configured in the initializer?
	-It's easier to use a screen pointer to make a complex bit of UI like a dialog. ResEdit
	or another resource editor fits this role.  
	Windows have been kept code-configurable to allow them to be hacked up easily.
	
	-What do I do?
	-make() a dialog using the resource id you've used, setting the default and cancel
	items if desired. 
	When subclassing, add the action code for your dialog to an implementation of the
	dialog-selection GF, which will be called when the user clicks on a button in the 
	dialog. 
	If you need to handle user actions before this, implement the handle-dialog-item method
	for your subclass.

	-How are Radio Buttons handled?
	-Radio buttons are handled automatically if a few simple rules are followed.
	Groups of radio buttons must each be form a group of continuously numbered dialog items
	(3,4,5, not 3,4,8), and must be seperated from other groups of radio buttons by a 
	non radio button dialog item (1,2,3 and 4,5,6 being radio buttons, 1,2,3 and 5,6,7
	with 4 as a static text item will).
	
	-Why do I have to set radio buttons and checkboxes by hand?
	-There is no magic way to know which options should be set, and dialogs should reflect
	current state. It's easy to set radio buttons and checkboxes: call handle-radio-button
	and set-checkbox-state. You'll need to use <simple-dialog>.dialogRef, which is a little
	ugly. Maybe wrappers would be good.

*/

/*
	<simple-dialog>
	A subclass of simple window.
*/

define open class <simple-dialog> ( <simple-window> )

	slot dialogRef :: <DialogRef>, init-value: make( <DialogRef> );	// Dummy

	slot default-item :: <integer>, init-value: -1;
	slot cancel-item :: <integer>, init-value: -1;
	
	slot focus-item-number :: <integer>, init-value: -1;
	slot focus-item-handle :: <ControlHandle>, init-value: make( <ControlHandle> );
	
	slot selected-item :: <integer>, init-value: -1;

end class <simple-dialog>;


/*
	dialog-to-object
*/

define method dialog-to-object( dialog :: <DialogRef> )
=> ( result :: type-union( <simple-dialog>, <boolean> ) ) //- #f

	window-to-object( GetDialogWindow( dialog ) );

end method dialog-to-object;


/*
	initialize <simple-dialog>
*/

define method initialize( dialog :: <simple-dialog>,   
							#key is-modal :: <boolean> = #f,  
					   		can-resize :: <boolean> = #f, 
					   		DLOG :: <integer> = 128, 
							default :: <integer> = -1, 
							cancel :: <integer> = -1 )
=>( result :: <simple-dialog> )

	dialog.modal := is-modal;
	dialog.resizeable := can-resize;

	dialog.dialogRef := GetNewDialog( DLOG );
	
	if( dialog.dialogRef ~= $NULL )
	
		dialog.windowRef := GetDialogWindow( dialog.dialogRef );

		dialog.grafPort := GetDialogPort( dialog.dialogRef ); 
		
		*objects-by-windowRef*[ as( <integer>, dialog.windowRef.raw-value ) ] := dialog;

		dialog.default-item := default;
		dialog.cancel-item := cancel;
		
		if( default ~= -1 )
			SetDialogDefaultItem( dialog.dialogRef, default );		// Set the ok( enter ) item
		end if;
		
		if( cancel ~= -1)	 
			SetDialogCancelItem( dialog.dialogRef, cancel );		// Set the cancel( command-. ) item
		end if;
		
		
		SetWindowKind( dialog.windowRef, 2000 );

		SelectWindow( dialog.windowRef );

		dialog;
		
	else
		
		#f;	//- Handle properly 
		
	end if;
	
end method initialize;


/*
	dialog-selection
	The user has clicked a button
*/

define open generic dialog-selection( dialog :: <simple-dialog> ) => ();

define method dialog-selection( dialog :: <simple-dialog> )
 => ()
	
	HideWindow( dialog.windowRef );
	
	values();
	
end method dialog-selection;


/*
	window-idle
*/

define method window-idle( dialog :: <simple-dialog>, event :: <EventRecord*> )
=> ()

	SetWindowKind( dialog.windowRef, $dialogKind );
	DialogSelect( event );	// Blink the caret if relevent.
	SetWindowKind( dialog.windowRef, 2000 );

	values();

end method window-idle;

/*
	update
*/

define method update( dialog :: <simple-dialog>, event :: <EventRecord*>  )
=> ()

	//next-method(); Would call draw, which we shouldn't need. I'm open to debate on this.

	SetWindowKind( dialog.windowRef, $dialogKind );
	DialogSelect( event );	// Blink the caret if relevent.
	SetWindowKind( dialog.windowRef, 2000 );

	values();

end method update;


/*
	window-click
*/


define method window-click( dialog :: <simple-dialog>, event :: <EventRecord*>, localPoint :: <Point*> )
=> ()

	let( didSelect, dialogRef, itemHit ) = DialogSelect( event );
	if( didSelect )
		handle-dialog-item( dialogRef, itemHit );	// dialog.dialogRef?
	end if;

	values();

end method window-click;


/*
	window-key
*/

define method window-key( dialog :: <simple-dialog>, event :: <EventRecord*>, charCode :: <character>, keyCode :: <integer>  )
=> ()

	SetWindowKind( dialog.windowRef, $dialogKind );
	DialogSelect( event );	
	SetWindowKind( dialog.windowRef, 2000 );

	values();

end method window-key;

/*
	activate
*/

define method activate( dialog :: <simple-dialog>, event :: <EventRecord*>, activating :: <boolean> )
=> ()

	SetWindowKind( dialog.windowRef, $dialogKind );
	DialogSelect( event );	
	SetWindowKind( dialog.windowRef, 2000 );
	
	values();

end method activate;


/*
	set-radio-buttons
*/

define method set-radio-buttons( dialog :: <DialogRef>, selected :: <integer>,
									first :: <integer>, last :: <integer> )
=> ()
	
	for( item :: <integer> from first to last )	// Turn all the radio buttons off <= last!
		let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
		SetControlValue( itemHandle, 0 );	// 0 = false = off
	end for;
		
	// Set the selected one
	
	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, selected );
	SetControlValue( itemHandle, 1 );	// 1 = true = on
		
	values();
	
end method set-radio-buttons;


/*
	handle-radio-button
*/

define method handle-radio-button( dialog :: <DialogRef>, selected :: <integer> )
=> ()
	
	let numItems :: <integer> = CountDITL( dialog );		// Find the number of items in the dialog
	let first-radio :: <integer> = 1;
	let last-radio :: <integer> = numItems;
	// Lop down away from the radio button
	block( finish )
		for( first :: <integer> from selected to 1 by -1 )
			let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, first );
			if( itemType ~= $kRadioButtonDialogItem )	// When it's no longer a radio button
				// The one after it is the first radio button
				first-radio := first + 1;							// So reflect this
				finish();
			end if;
		end for;
	end block;
	
	block( finish )
		for( last :: <integer> from selected to numItems ) // <= numItems
			let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, last );
			if( itemType ~= $kRadioButtonDialogItem )	// When it's no longer a radio button
				// The one before it is the last radio button
				last-radio := last - 1;					// So reflect this					
				finish();	
			end if;
		end for;
	end block;
	
	set-radio-buttons( dialog, selected, first-radio, last-radio );

end method handle-radio-button;


/*
	handle-checkbox
*/

define method handle-checkbox( dialog :: <DialogRef>, box :: <integer> )
=> ()
	
	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, box );
	let state = GetControlValue( itemHandle );		// Get whether the box is checked or not
	let flipped-state =	if( state = 1 )								// Toggle it
							0;
						else
							1;
						end if;	
	SetControlValue( itemHandle, flipped-state );		// Set the toggled state
	
	values();
	
end method handle-checkbox;

/*
	handle-dialog-item
*/

define open generic handle-dialog-item( dialog :: <DialogRef>, item :: <integer> ) => ( result :: <boolean> );

define method handle-dialog-item( dialog :: <DialogRef>, item :: <integer> )
=> ( result :: <boolean> )

	let dialog-object = dialog-to-object( dialog );

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
	
	dialog-object.focus-item-number := item;
	dialog-object.focus-item-handle := itemhandle;
	
	select( itemType )

		$kRadioButtonDialogItem =>	handle-radio-button( dialog, item );
									#t;
		
		$kCheckBoxDialogItem => handle-checkbox( dialog, item );
								#t;
		
		$kEditTextDialogItem => #f;
		
		$kButtonDialogItem =>	dialog-selection( dialog-object );
								close-window( dialog-to-object( dialog ) ); 
								#t;
						
		otherwise =>	#f;
	
	end select;

end method handle-dialog-item;

/*
	get-dialog-item-text
*/

define method get-dialog-item-text( dialog :: <DialogRef>, item :: <integer> )
=> ( result :: <pascal-string> )

	let text :: <pascal-string> = make( <pascal-string> );

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
	GetDialogItemText( itemHandle, text );

	text;

end method get-dialog-item-text;


/*
	set-dialog-item-text
*/

define method set-dialog-item-text( dialog :: <DialogRef>, item :: <integer>, text :: <pascal-string> )
=> ()

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
	SetDialogItemText( itemHandle, text );

	values();
	
end method set-dialog-item-text;


/*
	get-radio-button-state
*/

define method get-radio-button-state( dialog :: <DialogRef>, item :: <integer> )
=> ( result :: <boolean> )

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
	
	let result = GetControlValue( itemHandle );

	if( result = 1 )
		#t;
	else
		#f;
	end if;

end method get-radio-button-state;


/*
	set-checkbox-state
*/

define method set-checkbox-state( dialog :: <DialogRef> , item :: <integer>, state :: <boolean> )
=> ()

	let state-value :: <integer> =	if( state = #t ) 
										1; 
									else 
										0; 
									end if;

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
	SetControlValue( itemHandle, state-value );
	
	values();
	
end method set-checkbox-state;


/*
	get-checkbox-state
*/

define method get-checkbox-state( dialog :: <DialogRef>, item :: <integer> )
=> ( result :: <boolean> )

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );

	let value = GetControlValue( itemHandle );
	
	if( value = 1 ) 
		#t 
	else 
		#f 
	end if;
	
end method get-checkbox-state;


/*
	get-popup-string
*/

define method get-popup-string( dialog :: <DialogRef>, menuRsrc :: <integer>, item :: <integer> )
=> ( result :: <pascal-string> )
	
	let itemSelected :: <integer> = 0;
	let menuNum :: <integer> = 0;
	let menu :: <MenuHandle> = make( <MenuHandle> );

	let text :: <pascal-string> = make( <pascal-string> );

	let( itemType :: <integer>, itemHandle :: <ControlHandle>, itemRect :: <Rect*> ) = GetDialogItem( dialog, item );
	
	itemSelected = GetControlValue( itemHandle );
	
	menu = GetMenuHandle( menuRsrc );
	
	GetMenuItemText( menu, itemSelected, text );
	
	DeleteMenu( menuNum );
	ReleaseResource( menu );
	
	text;
end method get-popup-string;
