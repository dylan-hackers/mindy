module: controls

/*
	Mac Control Manager
*/

/*
	Includes
*/

c-include( "MacTypes.h" );
c-include( "Controls.h" );
c-include( "ControlDefinitions.h" );

/*
	Constants
	Incomplete.
*/  


define constant $pushButProc :: <integer>				= 0;
define constant $checkBoxProc :: <integer>				= 1;
define constant $radioButProc :: <integer>				= 2;
define constant $scrollBarProc :: <integer>				= 16;
define constant $popupMenuProc :: <integer>				= 1008;


define constant $kControlEditTextProc :: <integer>				= 272; /* Appearance 1.0 or higher */
define constant $kControlEditTextPasswordProc :: <integer> 		= 274; /* Appearance 1.0 or higher */
define constant $kControlEditTextInlineInputProc :: <integer> 	= 276; /* Appearance 1.1 Can't combine with the other variants*/

define constant $kControlStaticTextProc :: <integer>		= 288; /* Appearance 1.0 or higher */ 

define constant $kControlPictureProc :: <integer>			= 304; /* Appearance 1.0 or higher */
define constant $kControlPictureNoTrackProc :: <integer>	= 305; /* Appearance 1.0 or higher */			


define constant $kControlLabelPart :: <integer>			= 1;
define constant $kControlMenuPart :: <integer>			= 2;
define constant $kControlTrianglePart :: <integer>		= 4;
define constant $kControlEditTextPart :: <integer>		= 5;						/* Appearance 1.0 and later*/
define constant $kControlPicturePart :: <integer>			= 6;							/* Appearance 1.0 and later*/
define constant $kControlIconPart :: <integer>			= 7;							/* Appearance 1.0 and later*/
define constant $kControlClockPart :: <integer>			= 8;						/* Appearance 1.0 and later*/
define constant $kControlListBoxPart :: <integer>			= 24;							/* Appearance 1.0 and later*/
define constant $kControlListBoxDoubleClickPart :: <integer> = 25;						/* Appearance 1.0 and later*/
define constant $kControlImageWellPart :: <integer>		= 26;							/* Appearance 1.0 and later*/
define constant $kControlRadioGroupPart :: <integer>		= 27;							/* Appearance 1.0.2 and later*/
define constant $kControlButtonPart :: <integer>			= 10;
define constant $kControlCheckBoxPart :: <integer>		= 11;
define constant $kControlRadioButtonPart :: <integer>		= 11;
define constant $kControlUpButtonPart :: <integer>		= 20;
define constant $kControlDownButtonPart :: <integer>		= 21;
define constant $kControlPageUpPart :: <integer>			= 22;
define constant $kControlPageDownPart :: <integer>		= 23;
define constant $kControlClockHourDayPart :: <integer>	= 9;							/* Appearance 1.1 and later*/
define constant $kControlClockMinuteMonthPart :: <integer> = 10;						/* Appearance 1.1 and later*/
define constant $kControlClockSecondYearPart :: <integer>	= 11;							/* Appearance 1.1 and later*/
define constant $kControlClockAMPMPart :: <integer>		= 12;						/* Appearance 1.1 and later*/
define constant $kControlDataBrowserPart :: <integer>		= 24;							/* CarbonLib 1.0 and later*/
define constant $kControlDataBrowserDraggedPart :: <integer> = 25;							/* CarbonLib 1.0 and later*/



/*
	types
*/

define functional class <ControlHandle> ( <Handle> )
end class <ControlHandle>;

define constant <ControlActionUPP> = <UniversalProcPtr>;


/*
	DrawControls
*/

define method DrawControls( window :: <WindowRef> )
=> ()

	call-out( "DrawControls", void:, ptr: window.raw-value );

	values();

end method DrawControls;


/*
	NewControl
*/

define method NewControl( window :: <WindowRef>, bounds :: <Rect>, title :: <pascal-string>,
						 	visible :: <boolean>, initialValue :: <integer>, minimumValue :: <integer>,
							maximumValue :: <integer>, procID :: <integer>, controlReference :: <integer> )
=> ( result :: <ControlHandle> )

	let visBool = if( visible ) 1 else 0 end if;

	make( <Handle>, pointer: 
		call-out( "NewControl", ptr:, ptr: window.raw-value, ptr: bounds.raw-value, 
					ptr: title.raw-value, unsigned-char: visBool, 
					short: initialValue, short: minimumValue, short: maximumValue,
					short: procID, long: controlReference ) );

end method NewControl;
								 
								 	
/*
	DisposeControl
*/

define method DisposeControl( control :: <ControlHandle> )
=> ()
	
	call-out( "DisposeControl", void:, ptr: control.raw-value );
	
	values();
	
end method DisposeControl;		

/*
	KillControls
*/

define method KillControls( window :: <WindowRef> )	
=> ()
	
	call-out( "KillControls", void:, ptr: window.raw-value );
	
	values();

end method KillControls;		

/*
	HiliteControl
*/

define method HiliteControl( control :: <ControlHandle>, part :: <integer> )
=> ()
	
	call-out( "HiliteControl", void:, ptr: control.raw-value, short: part );
	
	values();
	
end method HiliteControl;	


/*
	ShowControl
*/

define method ShowControl( control :: <ControlHandle> )
=> ()
	
	call-out( "ShowControl", void:, ptr: control.raw-value );
	
	values();
	
end method ShowControl;	


/*
	HideControl
*/

define method HideControl( control :: <ControlHandle> )
=> ()
	
	call-out( "HideControl", void:, ptr: control.raw-value );
	
	values();
	
end method HideControl;


/*
	GetControlValue
*/

define method GetControlValue( control :: <ControlHandle> )
=> ( result :: <integer> )
	
	call-out( "GetControlValue", short:, ptr: control.raw-value );
	
end method GetControlValue;


/*
	SetControlValue
*/

define method SetControlValue( control :: <ControlHandle>, value :: <integer> )
=> ()
	
	call-out( "SetControlValue", void:, ptr: control.raw-value, short: value );
	
	values();

end method SetControlValue;


/*
	MoveControl
*/

define method MoveControl( control :: <ControlHandle>, h :: <integer>, v :: <integer> )
=> ()
	
	call-out( "MoveControl", void:, ptr: control.raw-value, short: h, short: v );
	
	values();

end method MoveControl;


/*
	SizeControl
*/

define method SizeControl( control :: <ControlHandle>, h :: <integer>, v :: <integer> )
=> ()
	
	call-out( "SizeControl", void:, ptr: control.raw-value, short: h, short: v );
	
	values();

end method SizeControl;


/*
	SetControlTitle
*/

define method SetControlTitle( item :: <ControlHandle>, text :: <pascal-string> )
=> ()
	
	call-out( "SetControlTitle", void:, ptr: item.raw-value, ptr: text.raw-value );
	
	values();

end method SetControlTitle;


/*
	GetControlTitle
*/

define method GetControlTitle( item :: <ControlHandle>, text :: <pascal-string> )
=> ()
	
	call-out( "GetControlTitle", void:, ptr: item.raw-value, ptr: text.raw-value );
	
	values();

end method GetControlTitle;


/*
	DragControl
*/

define method DragControl( control :: <ControlHandle> , start :: <Point>, limitRect :: <Rect>, slopRect :: <Rect>, axis :: <integer>)
=> ()

	call-out( "dragcontrol", void:, ptr: control.raw-value, ptr: start.raw-value, 
				ptr: limitRect.raw-value, ptr: slopRect.raw-value, short: axis );

	values();

end method DragControl;


/*
	FindControl
*/

define method FindControl( point :: <Point>, window :: <WindowRef>, control :: <ControlHandle> )
=> ( result :: <integer> )

	call-out( "findcontrol", short:, ptr: point.raw-value, ptr: window.raw-value, ptr: control.raw-value );

end method FindControl;


/*
	TrackControl
*/

define method TrackControl( control :: <ControlHandle>, point :: <Point>, proc :: <ControlActionUPP> )
=> ( result :: <integer> )

	call-out( "trackcontrol", short:, ptr: control.raw-value, ptr: point.raw-value, ptr: proc.raw-value );

end method TrackControl;

/*
	TestControl
*/

define method TestControl( control :: <ControlHandle>, point :: <Point> )
=> ( result :: <integer> )

	call-out( "testcontrol", short:, ptr: control.raw-value, ptr: point.raw-value );

end method TestControl;

