module: carbon

/*
	Mac Control Manager
*/

/*
	Includes
*/

c-include( "Carbon.h" );
c-include( "struct.h" );

/*
	Constants
	Incomplete.
*/  


define constant $pushButProc :: <integer>				= 0;
define constant $checkBoxProc :: <integer>				= 1;
define constant $radioButProc :: <integer>				= 2;
define constant $scrollBarProc :: <integer>				= 16;
define constant $popupMenuProc :: <integer>				= 1008;


define constant $kControlUserPaneProc	:: <integer> = c-expr(int: "kControlUserPaneProc");
define constant $kControlEditTextProc :: <integer>				= 272; /* Appearance 1.0 or higher */
define constant $kControlEditTextPasswordProc :: <integer> 		= 274; /* Appearance 1.0 or higher */
define constant $kControlEditTextInlineInputProc :: <integer> 	= 276; /* Appearance 1.1 Can't combine with the other variants*/

define constant $kControlStaticTextProc :: <integer>		= 288; /* Appearance 1.0 or higher */ 

define constant $kControlPictureProc :: <integer>			= 304; /* Appearance 1.0 or higher */
define constant $kControlPictureNoTrackProc :: <integer>	= 305; /* Appearance 1.0 or higher */		

define constant $kControlSeparatorLineProc	:: <integer> = c-expr(int: "kControlSeparatorLineProc");
define constant $kControlGroupBoxTextTitleProc	:: <integer> = c-expr(int: "kControlGroupBoxTextTitleProc");
define constant $kControlGroupBoxSecondaryTextTitleProc	:: <integer> = c-expr(int: "kControlGroupBoxSecondaryTextTitleProc");
define constant $kControlSliderProc	:: <integer> = c-expr(int: "kControlSliderProc");
define constant $kControlSliderLiveFeedback	:: <integer> = c-expr(int: "kControlSliderLiveFeedback");
define constant $kControlSliderHasTickMarks	:: <integer> = c-expr(int: "kControlSliderHasTickMarks");
define constant $kControlSliderReverseDirection	:: <integer> = c-expr(int: "kControlSliderReverseDirection");
define constant $kControlSliderNonDirectional	:: <integer> = c-expr(int: "kControlSliderNonDirectional");
define constant $kControlSliderPointsDownOrRight	:: <integer> = c-expr(int: "kControlSliderPointsDownOrRight");
define constant $kControlSliderPointsUpOrLeft	:: <integer> = c-expr(int: "kControlSliderPointsUpOrLeft");
define constant $kControlSliderDoesNotPoint	:: <integer> = c-expr(int: "kControlSliderDoesNotPoint");
define constant $kControlListBoxProc	:: <integer> = c-expr(int: "kControlListBoxProc");
define constant $kControlListBoxAutoSizeProc	:: <integer> = c-expr(int: "kControlListBoxAutoSizeProc");
define constant $kControlScrollBarProc	:: <integer> = c-expr(int: "kControlScrollBarProc");
define constant $kControlScrollBarLiveProc	:: <integer> = c-expr(int: "kControlScrollBarLiveProc");

define constant $kControlPushButtonProc	:: <integer> = c-expr(int: "kControlPushButtonProc");
define constant $kControlRadioButtonProc :: <integer> = c-expr(int: "kControlRadioButtonProc");
define constant $kControlCheckBoxProc	:: <integer> = c-expr(int: "kControlCheckBoxProc");

define constant $kControlLabelPart :: <integer>			= 1;
define constant $kControlMenuPart :: <integer>			= 2;
define constant $kControlTrianglePart :: <integer>		= 4;
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
define constant $kControlEntireControl :: <integer> = c-expr(int: "kControlEntireControl");
define constant $kControlNoPart :: <integer> = c-expr(int: "kControlNoPart");
define constant $kControlEditTextPart :: <integer> = c-expr(int: "kControlEditTextPart");

define constant $kControlKindBevelButton :: <integer> = c-expr(int: "kControlKindBevelButton");
define constant $kControlKindChasingArrows :: <integer> = c-expr(int: "kControlKindChasingArrows");	
define constant $kControlKindClock :: <integer> = c-expr(int: "kControlKindClock");	
define constant $kControlKindDataBrowser :: <integer> = c-expr(int: "kControlKindDataBrowser");	
define constant $kControlKindDisclosureButton :: <integer> = c-expr(int: "kControlKindDisclosureButton");	
define constant $kControlKindDisclosureTriangle :: <integer> = c-expr(int: "kControlKindDisclosureTriangle");	
define constant $kControlKindEditText :: <integer> = c-expr(int: "kControlKindEditText");	
define constant $kControlKindGroupBox :: <integer> = c-expr(int: "kControlKindGroupBox");	
define constant $kControlKindIcon :: <integer> = c-expr(int: "kControlKindIcon");	
define constant $kControlKindImageWell :: <integer> = c-expr(int: "kControlKindImageWell");	
define constant $kControlKindListBox :: <integer> = c-expr(int: "kControlKindListBox");	
define constant $kControlKindLittleArrows :: <integer> = c-expr(int: "kControlKindLittleArrows");	
define constant $kControlKindPicture :: <integer> = c-expr(int: "kControlKindPicture");	
define constant $kControlKindPlacard :: <integer> = c-expr(int: "kControlKindPlacard");	
define constant $kControlKindPopupArrow :: <integer> = c-expr(int: "kControlKindPopupArrow");	
define constant $kControlKindPopupButton :: <integer> = c-expr(int: "kControlKindPopupButton");	
define constant $kControlKindProgressBar :: <integer> = c-expr(int: "kControlKindProgressBar");	
define constant $kControlKindPushButton :: <integer> = c-expr(int: "kControlKindPushButton");	
define constant $kControlKindRadioGroup :: <integer> = c-expr(int: "kControlKindRadioGroup");	
define constant $kControlKindRoundButton :: <integer> = c-expr(int: "kControlKindRoundButton");	
define constant $kControlKindScrollBar :: <integer> = c-expr(int: "kControlKindScrollBar");	
define constant $kControlKindScrollingTextBox :: <integer> = c-expr(int: "kControlKindScrollingTextBox");	
define constant $kControlKindSeparator :: <integer> = c-expr(int: "kControlKindSeparator");	
define constant $kControlKindSignatureApple :: <integer> = c-expr(int: "kControlKindSignatureApple");	
define constant $kControlKindSlider :: <integer> = c-expr(int: "kControlKindSlider");	
define constant $kControlKindStaticText :: <integer> = c-expr(int: "kControlKindStaticText");	
define constant $kControlKindTabs :: <integer> = c-expr(int: "kControlKindTabs");	
define constant $kControlKindUserPane :: <integer> = c-expr(int: "kControlKindUserPane");	
define constant $kControlKindWindowHeader :: <integer> = c-expr(int: "kControlKindWindowHeader");

// Tags

define constant $kControlUserPaneDrawProcTag :: <integer> = c-expr(int: "kControlUserPaneDrawProcTag");
define constant $kControlStaticTextTextTag :: <integer> = c-expr(int: "kControlStaticTextTextTag");
define constant $kControlEditTextSelectionTag :: <integer> = c-expr(int: "kControlEditTextSelectionTag");
define constant $kControlEditTextPasswordTag :: <integer> = c-expr(int: "kControlEditTextPasswordTag"); 
define constant $kControlEditTextTextTag :: <integer> = c-expr(int: "kControlEditTextTextTag");
define constant $kControlListBoxListHandleTag :: <integer> = c-expr(int: "kControlListBoxListHandleTag");
define constant $kControlEditTextValidationProcTag :: <integer> = c-expr(int: "kControlEditTextValidationProcTag");
define constant $kControlEditTextKeyFilterTag :: <integer> = c-expr(int: "kControlEditTextKeyFilterTag");

// etc.

define constant $kControlSupportsEmbedding :: <integer> = c-expr(int: "kControlSupportsEmbedding");

// Fonts

define constant $kControlFontBigSystemFont :: <integer> = c-expr(int: "kControlFontBigSystemFont");
define constant $kControlFontSmallSystemFont :: <integer> = c-expr(int: "kControlFontSmallSystemFont");
define constant $kControlFontSmallBoldSystemFont :: <integer> = c-expr(int: "kControlFontSmallBoldSystemFont");
define constant $kControlFontViewSystemFont :: <integer> = c-expr(int: "kControlFontViewSystemFont");

// Content

define constant $kControlContentTextOnly :: <integer> = c-expr(int: "kControlContentTextOnly");

// Sort Order

define constant $kDataBrowserOrderUndefined :: <integer>    = 0;    /* Not currently supported */
define constant $kDataBrowserOrderIncreasing :: <integer>   = 1;
define constant $kDataBrowserOrderDecreasing :: <integer>   = 2;

define constant $kDataBrowserListViewDefaultColumnFlags :: <integer> = c-expr(int: "kDataBrowserListViewDefaultColumnFlags");
define constant $kControlUseFontMask :: <integer> = c-expr(int: "kControlUseFontMask");
define constant $kControlUseJustMask :: <integer> = c-expr(int: "kControlUseJustMask");

define constant $errDataBrowserPropertyNotSupported :: <integer> = c-expr(int: "errDataBrowserPropertyNotSupported");

// Etc.

define constant $kDataBrowserItemIsActiveProperty :: <integer> = c-expr(int: "kDataBrowserItemIsActiveProperty");
define constant $kDataBrowserItemDoubleClicked :: <integer> = c-expr(int: "kDataBrowserItemDoubleClicked");
define constant $kDataBrowserItemDeselected :: <integer> = c-expr(int: "kDataBrowserItemDeselected");
define constant $kDataBrowserItemSelected :: <integer> = c-expr(int: "kDataBrowserItemSelected");


/*
	types
*/

define functional class <ControlHandle> ( <Handle> )
end class <ControlHandle>;

define constant <ControlRef> = <ControlHandle>;

define constant <ControlActionUPP> = <UniversalProcPtr>;
define constant <ControlEditTextValidationUPP> = <UniversalProcPtr>;
define constant <ControlKeyFilterUPP> = <UniversalProcPtr>;
define constant <DataBrowserItemDataUPP> = <UniversalProcPtr>;
define constant <DataBrowserItemNotificationUPP> = <UniversalProcPtr>;
define constant <DataBrowserItemCompareUPP> = <UniversalProcPtr>;

define constant <ControlPartCode> = <SInt16>;
define constant <ControlFocusPart> = <SInt16>;

define functional class <ControlEditTextSelectionRec*> 
  (<statically-typed-pointer>)
end class <ControlEditTextSelectionRec*>;

define method content-size
  (thing == <ControlEditTextSelectionRec*>)
=>(result :: <integer>)
  c-expr(int: "sizeof(ControlEditTextSelectionRec)");
end method content-size;

define method selStart-value
  (rec :: <ControlEditTextSelectionRec*>)
=>(result :: <integer>)
  signed-short-at(rec, offset: 0);
end method selStart-value;

define method selEnd-value
  (rec :: <ControlEditTextSelectionRec*>)
=>(result :: <integer>)
  signed-short-at(rec, offset: 0);
end method selEnd-value;

define method selStart-value-setter
  (new-value :: <integer>, rec :: <ControlEditTextSelectionRec*>)
=>(result :: <integer>)
  signed-short-at(rec, offset: 0) := new-value;
end method selStart-value-setter;

define method selEnd-value-setter
  (new-value :: <integer>, rec :: <ControlEditTextSelectionRec*>)
=>(result :: <integer>)
  signed-short-at(rec, offset: 0) := new-value;
end method selEnd-value-setter;

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

define method NewControl( window :: <WindowRef>, bounds :: <Rect*>, title :: <pascal-string>,
						 	visible :: <boolean>, initialValue :: <integer>, minimumValue :: <integer>,
							maximumValue :: <integer>, procID :: <integer>, controlReference :: <integer> )
=> ( result :: <ControlRef> )

	let visBool = if( visible ) 1 else 0 end if;

	make( <ControlRef>, pointer: 
		call-out( "NewControl", ptr:, ptr: window.raw-value, ptr: bounds.raw-value, 
					ptr: title.raw-value, unsigned-char: visBool, 
					short: initialValue, short: minimumValue, short: maximumValue,
					short: procID, long: controlReference ) );

end method NewControl;
								 
								 	
/*
	DisposeControl
*/

define method DisposeControl( control :: <ControlRef> )
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
	GetControlBounds
*/

define method GetControlBounds( control :: <ControlRef>, r :: <Rect*> )
=> (r :: <Rect*>)
	
	call-out( "GetControlBounds", void:, ptr: control.raw-value, ptr: r.raw-value );
	
	r;
	
end method GetControlBounds;		



/*
	HiliteControl
*/

define method HiliteControl( control :: <ControlRef>, part :: <integer> )
=> ()
	
	call-out( "HiliteControl", void:, ptr: control.raw-value, short: part );
	
	values();
	
end method HiliteControl;	


/*
	ShowControl
*/

define method ShowControl( control :: <ControlRef> )
=> ()
	
	call-out( "ShowControl", void:, ptr: control.raw-value );
	
	values();
	
end method ShowControl;	


/*
	HideControl
*/

define method HideControl( control :: <ControlRef> )
=> ()
	
	call-out( "HideControl", void:, ptr: control.raw-value );
	
	values();
	
end method HideControl;


/*
	GetControlValue
*/

define method GetControlValue( control :: <ControlRef> )
=> ( result :: <integer> )
	
	call-out( "GetControlValue", short:, ptr: control.raw-value );
	
end method GetControlValue;


/*
	SetControlValue
*/

define method SetControlValue( control :: <ControlRef>, value :: <integer> )
=> ()
	
	call-out( "SetControlValue", void:, ptr: control.raw-value, short: value );
	
	values();

end method SetControlValue;


/*
	MoveControl
*/

define method MoveControl( control :: <ControlRef>, h :: <integer>, v :: <integer> )
=> ()
	
	call-out( "MoveControl", void:, ptr: control.raw-value, short: h, short: v );
	
	values();

end method MoveControl;


/*
	SizeControl
*/

define method SizeControl( control :: <ControlRef>, h :: <integer>, v :: <integer> )
=> ()
	
	call-out( "SizeControl", void:, ptr: control.raw-value, short: h, short: v );
	
	values();

end method SizeControl;


/*
	SetControlTitle
*/

define method SetControlTitle( item :: <ControlRef>, text :: <pascal-string> )
=> ()
	
	call-out( "SetControlTitle", void:, ptr: item.raw-value, ptr: text.raw-value );
	
	values();

end method SetControlTitle;


/*
	GetControlTitle
*/

define method GetControlTitle( item :: <ControlRef>, text :: <pascal-string> )
=> ()
	
	call-out( "GetControlTitle", void:, ptr: item.raw-value, ptr: text.raw-value );
	
	values();

end method GetControlTitle;


/*
	DragControl
*/

define method DragControl( control :: <ControlRef> , start :: <Point*>, limit<Rect*> :: <Rect*>, slop<Rect*> :: <Rect*>, axis :: <integer>)
=> ()

	call-out( "dragcontrol", void:, ptr: control.raw-value, ptr: start.raw-value, 
				ptr: limit<Rect*>.raw-value, ptr: slop<Rect*>.raw-value, short: axis );

	values();

end method DragControl;


/*
	FindControl
*/

define method FindControl( point :: <Point*>, window :: <WindowRef> )
=> ( control :: <ControlRef>, result :: <ControlPartCode> )

    let temp = make( <Handle> ); // used as a pointer parameter
    let result = call-out( "findcontrol", short:, ptr: point.raw-value, ptr: window.raw-value, ptr: temp.raw-value );
    values( pointer-at( temp, offset: 0, class: <ControlRef> ), as( <ControlPartCode>, result ) );

end method FindControl;


/*
	HandleControlClick
*/

define method HandleControlClick( inControl :: <ControlRef>, inWhere :: <Point*>, 
                                    inModifiers :: <EventModifiers>, inAction :: <ControlActionUPP> )
=> ( result :: <ControlPartCode> )

	call-out( "handlecontrolclick", short:, ptr: inControl.raw-value, ptr: inWhere.raw-value, 
                    short: inModifiers, ptr: inAction.raw-value );

end method HandleControlClick;

/*
	HandleControlKey
*/

define method HandleControlKey( control :: <ControlRef>, inKeyCode :: <SInt16>, 
                                inCharCode :: <SInt16>, inModifiers :: <EventModifiers> )
=> ( result :: <SInt16> )

	call-out( "HandleControlKey", short:, ptr: control.raw-value, short: inKeyCode,
                    short: inCharCode, int: inModifiers );

end method HandleControlKey;


/*
	IdleControls
*/

define method IdleControls( window :: <WindowRef> )
=> ()

    call-out( "IdleControls", void:, ptr: window.raw-value, );
    values()

end method IdleControls;


/*
	TrackControl
*/

define method TrackControl( control :: <ControlRef>, point :: <Point*>, proc :: <ControlActionUPP> )
=> ( result :: <integer> )

	call-out( "trackcontrol", short:, ptr: control.raw-value, ptr: point.raw-value, ptr: proc.raw-value );

end method TrackControl;

/*
	TestControl
*/

define method TestControl( control :: <ControlRef>, point :: <Point*> )
=> ( result :: <integer> )

	call-out( "testcontrol", short:, ptr: control.raw-value, ptr: point.raw-value );

end method TestControl;

/*
    Keyboard Focus. Appearance 1.0+
*/

/*
    AdvanceKeyboardFocus
*/

define method AdvanceKeyboardFocus( inWindow :: <WindowRef> )
=> ( result :: <OSErr> )

	call-out( "AdvanceKeyboardFocus", int:, ptr: inWindow.raw-value );

end method AdvanceKeyboardFocus;

/*
    ClearKeyboardFocus
*/

define method ClearKeyboardFocus( inWindow :: <WindowRef> )
=> ( result :: <OSErr> )

	call-out( "ClearKeyboardFocus", int:, ptr: inWindow.raw-value );

end method ClearKeyboardFocus;

/*
    GetKeyboardFocus
*/

define method GetKeyboardFocus( inWindow :: <WindowRef> )
=> ( control :: <ControlRef>, error :: <OSErr> )

        let temp = make( <Handle> );	// Used as a ControlHandle * parameter
	let error = call-out( "GetKeyboardFocus", int:, ptr: inWindow.raw-value, ptr: temp.raw-value );
        values( pointer-at( temp, offset: 0, class: <ControlRef> ), as( <OSErr>, error) );

end method GetKeyboardFocus;


/*
    ReverseKeyboardFocus
*/

define method ReverseKeyboardFocus( inWindow :: <WindowRef> )
=> ( result :: <OSErr> )

	call-out( "ReverseKeyboardFocus", int:, ptr: inWindow.raw-value );

end method ReverseKeyboardFocus;


/*
    SetKeyboardFocus
*/

define method SetKeyboardFocus( inWindow :: <WindowRef>, inControl :: <ControlRef>,
                                inPart :: <ControlFocusPart> )
=> ( result :: <OSErr> )

	call-out( "SetKeyboardFocus", int:, ptr: inWindow.raw-value, ptr: inControl.raw-value,
                    short: inPart );

end method SetKeyboardFocus;

/*
    IsControlVisible
*/

define method IsControlVisible( inControl :: <ControlRef> )
=> ( result :: <OSErr> )

	if( call-out( "IsControlVisible", int:, ptr: inControl.raw-value ))
    #t;
  else
    #f;
  end;

end method IsControlVisible;

/*
    CreateRootControl
*/

define method CreateRootControl( for-window :: <WindowRef> )
=> ( result :: <OSErr>, root-control :: <ControlRef> )

  let temp :: <Handle> = make(<Handle>);
	let err = call-out( "CreateRootControl", int:, ptr: for-window.raw-value, ptr: temp.raw-value );
  values(as(<OSErr>, err), pointer-at(temp, class: <ControlRef>, offset: 0));

end method CreateRootControl;

/*
  SetControlData
*/

define method SetControlData( inControl :: <ControlRef>, inPart :: <integer>, inTagName :: <integer>,
                        inSize :: <integer>, inData :: <statically-typed-pointer>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetControlData", int:, ptr: inControl.raw-value, short: inPart, int: inTagName,
                        int: inSize, ptr: inData.raw-value));
end method SetControlData;

define method SetControlData( inControl :: <ControlRef>, inPart :: <integer>, inTagName :: <integer>,
                        inSize :: <integer>, inData :: <raw-pointer>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetControlData", int:, ptr: inControl.raw-value, short: inPart, int: inTagName,
                        int: inSize, ptr: inData));
end method SetControlData;

/*
  GetControlData
*/

define method GetControlData( inControl :: <ControlRef>, inPart :: <integer>, inTagName :: <integer>,
                        inSize :: <integer>, inData :: <statically-typed-pointer>)
=> (result :: <OSErr>, outSize :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetControlData", int:, ptr: inControl.raw-value, short: inPart, int: inTagName,
                        int: inSize, ptr: inData.raw-value, ptr: temp.raw-value)),
  signed-long-at(temp, offset: 0));	// Yes, signed
end method GetControlData;

define method GetControlData( inControl :: <ControlRef>, inPart :: <integer>, inTagName :: <integer>,
                        inSize :: <integer>, inData :: <raw-pointer>)
=> (result :: <OSErr>, outSize :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetControlData", int:, ptr: inControl.raw-value, short: inPart, int: inTagName,
                        int: inSize, ptr: inData, ptr: temp.raw-value)),
  signed-long-at(temp, offset: 0));	// Yes, signed
end method GetControlData;

/*
  GetControlDataSize
*/

define method GetControlDataSize( inControl :: <ControlRef>, inPart :: <integer>, inTagName :: <integer>)
=> (result :: <OSErr>, outSize :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetControlDataSize", int:, ptr: inControl.raw-value, short: inPart, int: inTagName,
                        ptr: temp.raw-value)),
  signed-long-at(temp, offset: 0));	// Yes, signed
end method GetControlDataSize;


/*
		NewControlUserPaneDrawUPP
*/

define method NewControlUserPaneDrawUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlUserPaneDrawUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlUserPaneDrawUPP;

define method NewControlUserPaneDrawUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlUserPaneDrawUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlUserPaneDrawUPP;

/*
		NewControlEditTextValidationUPP
*/

define method NewControlEditTextValidationUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlEditTextValidationUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlEditTextValidationUPP;

define method NewControlEditTextValidationUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlEditTextValidationUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlEditTextValidationUPP;

/*
		NewControlKeyFilterUPP
*/

define method NewControlKeyFilterUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlKeyFilterUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlKeyFilterUPP;

define method NewControlKeyFilterUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlKeyFilterUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlKeyFilterUPP;


/*
		NewControlActionUPP
*/

define method NewControlActionUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlActionUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlActionUPP;

define method NewControlActionUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewControlActionUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewControlActionUPP;


/*
		NewDataBrowserItemDataUPP
*/

define method NewDataBrowserItemDataUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewDataBrowserItemDataUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewDataBrowserItemDataUPP;

define method NewDataBrowserItemDataUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewDataBrowserItemDataUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewDataBrowserItemDataUPP;


/*
		NewDataBrowserItemNotificationUPP
*/

define method NewDataBrowserItemNotificationUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewDataBrowserItemNotificationUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewDataBrowserItemNotificationUPP;

define method NewDataBrowserItemNotificationUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewDataBrowserItemNotificationUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewDataBrowserItemNotificationUPP;

/*
		NewDataBrowserItemCompareUPP
*/

define method NewDataBrowserItemCompareUPP( userRoutine ) //:: <callback-function> )
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewDataBrowserItemCompareUPP", ptr:, ptr: userRoutine.callback-entry );
	make( <UniversalProcPtr>, pointer: result );
end method NewDataBrowserItemCompareUPP;

define method NewDataBrowserItemCompareUPP( userRoutine :: <function-pointer> )	//  :: <callback-function>
=> ( UPP :: <UniversalProcPtr> )
	let result = call-out( "NewDataBrowserItemCompareUPP", ptr:, ptr: userRoutine.raw-value );
	make( <UniversalProcPtr>, pointer: result );
end method NewDataBrowserItemCompareUPP;



/*
  EmbedControl
*/

define method EmbedControl( inControl :: <ControlRef>, inContainer :: <ControlRef>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("EmbedControl", int:, ptr: inControl.raw-value, 
                        ptr: inContainer.raw-value));
end method EmbedControl;


define method GetBestControlRect(inControl :: <ControlRef>, 
                                  outRect :: <Rect*>)
=> (result :: <OSErr>, baselineOffset :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetBestControlRect", int:, ptr: inControl.raw-value, 
                        ptr: outRect.raw-value, ptr: temp.raw-value)),
     signed-short-at(temp, offset: 0));
end method GetBestControlRect;

define method ActivateControl( inControl :: <ControlRef>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("ActivateControl", int:, ptr: inControl.raw-value));
end method ActivateControl;

define method DeactivateControl( inControl :: <ControlRef>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("DeactivateControl", int:, ptr: inControl.raw-value));
end method DeactivateControl;


// 32 Bit

define method SetControl32BitValue
    (inControl :: <ControlRef>, value :: <integer>) => ()
  call-out("SetControl32BitValue", void:, ptr: inControl.raw-value, int: value);
end method SetControl32BitValue;

define method SetControl32BitMinimum
    (inControl :: <ControlRef>, value :: <integer>) => ()
  call-out("SetControl32BitMinimum", void:, ptr: inControl.raw-value, int: value);
end method SetControl32BitMinimum;

define method SetControl32BitMaximum
    (inControl :: <ControlRef>, value :: <integer>) => ()
  call-out("SetControl32BitMaximum", void:, ptr: inControl.raw-value, int: value);
end method SetControl32BitMaximum;

define method SetControlViewSize
    (inControl :: <ControlRef>, value :: <integer>) => ()
  call-out("SetControlViewSize", void:, ptr: inControl.raw-value, int: value);
end method SetControlViewSize;


define method GetControl32BitValue(inControl :: <ControlRef>)
=> (value :: <integer>)
  call-out("GetControl32BitValue", int:, ptr: inControl.raw-value);
end method GetControl32BitValue;

define method GetControl32BitMinimum(inControl :: <ControlRef>)
=> (value :: <integer>)
  call-out("GetControl32BitMinimum", int:, ptr: inControl.raw-value);
end method GetControl32BitMinimum;

define method GetControl32BitMaximum(inControl :: <ControlRef>)
=> (value :: <integer>)
  call-out("GetControl32BitMaximum", int:, ptr: inControl.raw-value);
end method GetControl32BitMaximum;

define method GetControlViewSize(inControl :: <ControlRef>)
=> (value :: <integer>)
  call-out("GetControlViewSize", int:, ptr: inControl.raw-value);
end method GetControlViewSize;

// Actions

define method SetControlAction
    (inControl :: <ControlRef>, action :: <UniversalProcPtr>) => ()
  call-out("SetControlAction", void:, ptr: inControl.raw-value, ptr: action.raw-value);
end method SetControlAction;

// Slider creation
// Only way to set number of ticks?

define method CreateSliderControl
    (window :: <WindowRef>, bounds :: <Rect*>, value :: <integer>, min :: <integer>, max :: <integer>,
     orientation :: <integer>, numTickMarks :: <UInt16>, liveTracking :: <boolean>, 
     liveProc :: <ControlActionUPP>)
 => (result :: <OSStatus>, control :: <ControlRef>)
  let temp :: <Handle> = make(<handle>);
  let live? :: <integer> = if(liveTracking) 1 else 0 end;
  values(call-out("CreateSliderControl", int:, ptr: window.raw-value, ptr: bounds.raw-value,
      int: value, int: min, int: max, int: orientation, int: numTickMarks, int: live?, 
      ptr: liveProc.raw-value, ptr: temp.raw-value),
    pointer-at(temp, class: <ControlRef>, offset: 0));
end method CreateSliderControl;

// That ListView Thing

define constant $kDataBrowserListView :: <integer> = c-expr(int: "kDataBrowserListView");
define constant $kDataBrowserColumnView :: <integer> = c-expr(int: "kDataBrowserColumnView");

define constant $kDataBrowserDragSelect :: <integer> = c-expr(int: "kDataBrowserDragSelect");
define constant $kDataBrowserSelectOnlyOne :: <integer> = c-expr(int: "kDataBrowserSelectOnlyOne");
define constant $kDataBrowserResetSelection :: <integer> = c-expr(int: "kDataBrowserResetSelection");

define constant $kDataBrowserNoItem :: <integer> = c-expr(int: "kDataBrowserNoItem");

define constant $kDataBrowserItemNoState :: <integer> = c-expr(int: "kDataBrowserItemNoState");
define constant $kDataBrowserItemAnyState :: <integer> = c-expr(int: "kDataBrowserItemAnyState");
define constant $kDataBrowserItemIsSelected :: <integer> = c-expr(int: "kDataBrowserItemIsSelected");

define constant $kDataBrowserItemsAdd :: <integer> = c-expr(int: "kDataBrowserItemsAdd");
define constant $kDataBrowserItemsAssign :: <integer> = c-expr(int: "kDataBrowserItemsAssign");
define constant $kDataBrowserItemsToggle :: <integer> = c-expr(int: "kDataBrowserItemsToggle");
define constant $kDataBrowserItemsRemove :: <integer> = c-expr(int: "kDataBrowserItemsRemove");

define constant $kDataBrowserItemNoProperty :: <integer> = c-expr(int: "kDataBrowserItemNoProperty");

define constant $kDataBrowserCmdTogglesSelection :: <integer> = c-expr(int: "kDataBrowserCmdTogglesSelection");

define constant $kDataBrowserDefaultPropertyFlags :: <integer> = c-expr(int: "kDataBrowserDefaultPropertyFlags");

define constant $kDataBrowserListViewLatestHeaderDesc :: <integer> = c-expr(int: "kDataBrowserListViewLatestHeaderDesc");

define constant $kDataBrowserIconType :: <integer> = c-expr(int: "kDataBrowserIconType");
define constant $kDataBrowserTextType :: <integer> = c-expr(int: "kDataBrowserTextType");
define constant $kDataBrowserIconAndTextType :: <integer> = c-expr(int: "kDataBrowserIconAndTextType");

define method CreateDataBrowserControl
    (window :: <WindowRef>, bounds :: <Rect*>, style :: <integer>)
 => (result :: <OSStatus>, control :: <ControlRef>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("CreateDataBrowserControl", int:, ptr: window.raw-value, 
          ptr: bounds.raw-value, int: style, ptr: temp.raw-value)),
         pointer-at(temp, class: <ControlRef>, offset: 0));
end method CreateDataBrowserControl; 

define functional class <DataBrowserListViewColumnDesc*>
    (<statically-typed-pointer>)
end class <DataBrowserListViewColumnDesc*>;

define method content-size
  (thing == <DataBrowserListViewColumnDesc*>)
=>(result :: <integer>)
  c-expr(int: "sizeof(DataBrowserListViewColumnDesc)");
end method content-size;

define method propertyDesc-value
    (desc :: <DataBrowserListViewColumnDesc*>)
 => (result :: <DataBrowserTableViewColumnDesc*>)    
  make(<DataBrowserTableViewColumnDesc*>, 
  	pointer: desc.raw-value + c-expr(int: "fldoff(DataBrowserListViewColumnDesc, propertyDesc)"));
end method propertyDesc-value;

define method headerBtnDesc-value
    (desc :: <DataBrowserListViewColumnDesc*>)
 => (result :: <DataBrowserListViewHeaderDesc*>)    
  make(<DataBrowserListViewHeaderDesc*>, 
  	pointer: desc.raw-value + c-expr(int: "fldoff(DataBrowserListViewColumnDesc, headerBtnDesc)"));
end method headerBtnDesc-value;

define functional class <DataBrowserPropertyDesc*>
    (<statically-typed-pointer>)
end class <DataBrowserPropertyDesc*>;

define method content-size
  (thing == <DataBrowserPropertyDesc*>)
=>(result :: <integer>)
  c-expr(int: "sizeof(DataBrowserPropertyDesc)");
end method content-size;

define method propertyID-value
    (desc :: <DataBrowserPropertyDesc*>)
 => (result :: <integer>)    
  signed-long-at(desc, 
  	offset: c-expr(int: "fldoff(DataBrowserPropertyDesc, propertyID)"));
end method propertyID-value;

define method propertyType-value
    (desc :: <DataBrowserPropertyDesc*>)
 => (result :: <integer>)    
  signed-long-at(desc, 
  	offset: c-expr(int: "fldoff(DataBrowserPropertyDesc, propertyType)"));
end method propertyType-value;

define method propertyFlags-value
    (desc :: <DataBrowserPropertyDesc*>)
 => (result :: <integer>)    
  signed-long-at(desc, 
  	offset: c-expr(int: "fldoff(DataBrowserPropertyDesc, propertyFlags)"));
end method propertyFlags-value;

define method propertyID-value-setter
    (value :: <integer>, desc :: <DataBrowserPropertyDesc*>)
 => (result :: <integer>)    
  signed-long-at(desc, 
  	offset: c-expr(int: "fldoff(DataBrowserPropertyDesc, propertyID)")) := value;
end method propertyID-value-setter;

define method propertyType-value-setter
    (value :: <integer>, desc :: <DataBrowserPropertyDesc*>)
 => (result :: <integer>)    
  signed-long-at(desc, 
  	offset: c-expr(int: "fldoff(DataBrowserPropertyDesc, propertyType)")) := value;
end method propertyType-value-setter;

define method propertyFlags-value-setter
    (value :: <integer>, desc :: <DataBrowserPropertyDesc*>)
 => (result :: <integer>)    
  signed-long-at(desc, 
  	offset: c-expr(int: "fldoff(DataBrowserPropertyDesc, propertyFlags)")) := value;
end method propertyFlags-value-setter;

define constant <DataBrowserTableViewColumnDesc*> = <DataBrowserPropertyDesc*>;

define functional class <ControlButtonContentInfo*>
    (<statically-typed-pointer>)
end class <ControlButtonContentInfo*>;

// We don't handle the second, union, field

define method content-size
    (cls == <ControlButtonContentInfo*>)
 => (result :: <integer>)
  c-expr(int: "sizeof(ControlButtonContentInfo)");
end method content-size;

define method contentType-value
    (target :: <ControlButtonContentInfo*>)
 => (result :: <integer>)
  signed-short-at(target, 
  	offset: c-expr(int: "fldoff(ControlButtonContentInfo, contentType)"));
end method contentType-value;

define method contentType-value-setter
    (value :: <integer>, target :: <ControlButtonContentInfo*>)
 => (result :: <integer>)
  signed-short-at(target, 
  	offset: c-expr(int: "fldoff(ControlButtonContentInfo, contentType)")) := value;
end method contentType-value-setter;

/*
typedef SInt16                          ControlContentType;
struct ControlButtonContentInfo {
  ControlContentType  contentType;
  union {
    SInt16              resID;
    CIconHandle         cIconHandle;
    Handle              iconSuite;
    IconRef             iconRef;
    PicHandle           picture;
    Handle              ICONHandle;
  }                       u;
};
*/

define functional class <ControlFontStyleRec*>
    (<statically-typed-pointer>)
end class <ControlFontStyleRec*>;

define method content-size
    (cls == <ControlFontStyleRec*>)
 => (result :: <integer>)
  c-expr(int: "sizeof(ControlFontStyleRec)");
end method content-size;

define method flags-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, flags)"));
end method flags-value;

define method flags-value-setter
    (value :: <integer>, target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, flags)")) := value;
end method flags-value-setter;

define method font-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, font)"));
end method font-value;

define method font-value-setter
    (value :: <integer>, target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, font)")) := value;
end method font-value-setter;

define method size-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, size)"));
end method size-value;

define method size-value-setter
    (value :: <integer>, target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, size)")) := value;
end method size-value-setter;

define method style-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, style)"));
end method style-value;

define method style-value-setter
    (value :: <integer>, target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, style)")) := value;
end method style-value-setter;

define method mode-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, mode)"));
end method mode-value;

define method mode-value-setter
    (value :: <integer>, target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, mode)")) := value;
end method mode-value-setter;

define method just-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, just)"));
end method just-value;

define method just-value-setter
    (value :: <integer>, target :: <ControlFontStyleRec*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, just)")) := value;
end method just-value-setter;

define method foreColor-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <RGBColor*>)
  make(<RGBColor*>, 
    pointer: as(<raw-pointer>, target.raw-value + 
      c-expr(int: "fldoff(ControlFontStyleRec, foreColor)")));
end method foreColor-value;

define method foreColor-value-setter
    (value :: <RGBColor*>, target :: <ControlFontStyleRec*>)
 => (result :: <RGBColor*>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, foreColor)")) := value.red-value;
  unsigned-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, foreColor)") + 2) := value.green-value;
  unsigned-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, foreColor)") + 4) := value.blue-value;
  value;
end method foreColor-value-setter;

define method backColor-value
    (target :: <ControlFontStyleRec*>)
 => (result :: <RGBColor*>)
  make(<RGBColor*>, 
    pointer: as(<raw-pointer>, target.raw-value + 
      c-expr(int: "fldoff(ControlFontStyleRec, backColor)")));
end method backColor-value;

define method backColor-value-setter
    (value :: <RGBColor*>, target :: <ControlFontStyleRec*>)
 => (result :: <RGBColor*>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, backColor)")) := value.red-value;
  unsigned-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, backColor)") + 2) := value.green-value;
  unsigned-short-at(target, offset: c-expr(int: "fldoff(ControlFontStyleRec, backColor)") + 4) := value.blue-value;
  value;
end method backColor-value-setter;

define functional class <DataBrowserListViewHeaderDesc*>
    (<statically-typed-pointer>)
end class <DataBrowserListViewHeaderDesc*>;

define method content-size
    (cls == <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  c-expr(int: "sizeof(DataBrowserListViewHeaderDesc)");
end method content-size;

define method version-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  signed-short-at(target, 
  	offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, version)"));
end method version-value;

define method version-value-setter
    (value :: <integer>, target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  signed-short-at(target, 
  	offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, version)")) := value;
end method version-value-setter;

define method minimumWidth-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, minimumWidth)"));
end method minimumWidth-value;

define method minimumWidth-value-setter
    (value :: <integer>, target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, minimumWidth)")) := value;
end method minimumWidth-value-setter;

define method maximumWidth-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, maximumWidth)"));
end method maximumWidth-value;

define method maximumWidth-value-setter
    (value :: <integer>, target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, maximumWidth)")) := value;
end method maximumWidth-value-setter;

define method titleOffset-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, titleOffset)"));
end method titleOffset-value;

define method titleOffset-value-setter
    (value :: <integer>, target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  signed-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, titleOffset)")) := value;
end method titleOffset-value-setter;

define method titleString-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <CFStringRef>)
  pointer-at(target, class: <CFStringRef>, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, titleString)"));
end method titleString-value;

define method titleString-value-setter
    (value :: <CFStringRef>, target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <CFStringRef>)
  pointer-at(target, class: <CFStringRef>, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, titleString)")) := value;
end method titleString-value-setter;

define method initialOrder-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, initialOrder)"));
end method initialOrder-value;

define method initialOrder-value-setter
    (value :: <integer>, target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <integer>)
  unsigned-short-at(target, offset: c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, initialOrder)")) := value;
end method initialOrder-value-setter;

define method btnFontStyle-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <ControlFontStyleRec*>)
  make(<ControlFontStyleRec*>, 
    pointer: as(<raw-pointer>, target.raw-value + 
      c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, btnFontStyle)")));
end method btnFontStyle-value;

define method btnContentInfo-value
    (target :: <DataBrowserListViewHeaderDesc*>)
 => (result :: <ControlButtonContentInfo*>)
  make(<ControlButtonContentInfo*>, 
    pointer: as(<raw-pointer>, target.raw-value + 
      c-expr(int: "fldoff(DataBrowserListViewHeaderDesc, btnContentInfo)")));
end method btnContentInfo-value;
        
define method AddDataBrowserListViewColumn
    (browser :: <ControlRef>, desc :: <DataBrowserListViewColumnDesc*>, position :: <integer>)
 => (result :: <OSStatus>)
        as(<OSErr>, call-out("AddDataBrowserListViewColumn", int:, ptr: browser.raw-value, 
          ptr: desc.raw-value, int: position));
end method AddDataBrowserListViewColumn; 
        
define method AddDataBrowserItems
    (browser :: <ControlRef>, container :: <integer>, numItems :: <integer>,
     items :: <statically-typed-pointer>, preSortProperty :: <integer>)
 => (result :: <OSStatus>)
        as(<OSErr>, call-out("AddDataBrowserItems", int:, ptr: browser.raw-value,
          int: container, int: numItems, ptr: items.raw-value, int: preSortProperty));
end method AddDataBrowserItems; 
        
define method RemoveDataBrowserItems
    (browser :: <ControlRef>, container :: <integer>, numItems :: <integer>,
     items :: <statically-typed-pointer>, preSortProperty :: <integer>)
 => (result :: <OSStatus>)
        as(<OSErr>, call-out("RemoveDataBrowserItems", int:, ptr: browser.raw-value,
          int: container, int: numItems, ptr: items.raw-value, int: preSortProperty));
end method RemoveDataBrowserItems; 
        
define method GetDataBrowserItemState
    (browser :: <ControlRef>, item :: <integer>)
 => (result :: <OSStatus>, state :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetDataBrowserItemState", int:, ptr: browser.raw-value,
    int: item, ptr: temp.raw-value)), signed-long-at(temp));
end method GetDataBrowserItemState; 
  
define method GetDataBrowserItemCount
    (browser :: <ControlRef>, container :: <integer>, recurse :: <boolean>,
     state :: <integer>)
 => (status :: <OSStatus>, numItems :: <integer>)
  let recurse? :: <integer> = if(recurse) 1 else 0 end;
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSStatus>, call-out("GetDataBrowserItemCount", int:, ptr: browser.raw-value, 
      int: container, int: recurse?, int: state, ptr: temp.raw-value)),
    unsigned-long-at(temp, offset: 0));
end method GetDataBrowserItemCount;
  
define method SetDataBrowserSelectionFlags
    (browser :: <ControlRef>, flags :: <integer>)
 => (status :: <OSStatus>)
  as(<OSStatus>, call-out("SetDataBrowserSelectionFlags", int:, ptr: browser.raw-value, int: flags));
end method SetDataBrowserSelectionFlags;

define functional class <DataBrowserCallbacks*>
    (<statically-typed-pointer>)
end class <DataBrowserCallbacks*>;

define method content-size
  (thing == <DataBrowserCallbacks*>)
=>(result :: <integer>)
  c-expr(int: "sizeof(DataBrowserCallbacks)");
end method content-size;

define method version-value-setter
    (version :: <integer>, callbacks :: <DataBrowserCallbacks*>)
 => (version :: <integer>)
  unsigned-long-at(callbacks, offset: 0) := version;
end method version-value-setter;

define method u-value
    (callbacks :: <DataBrowserCallbacks*>)
 => (u :: <DataBrowserCallbacks-u*>)
  make(<DataBrowserCallbacks-u*>, pointer: callbacks.raw-value + 4);
end method u-value;

define functional class <DataBrowserCallbacks-u*>
    (<statically-typed-pointer>)
end class <DataBrowserCallbacks-u*>;

define method content-size
  (thing == <DataBrowserCallbacks-u*>)
=>(result :: <integer>)
  c-expr(int: "sizeof(DataBrowserCallbacks)") - 4;
end method content-size;

define method v1-value
    (callbacks :: <DataBrowserCallbacks-u*>)
 => (v1 :: <DataBrowserCallbacks-u-v1*>)
  make(<DataBrowserCallbacks-u-v1*>, pointer: callbacks.raw-value);
end method v1-value;

define functional class <DataBrowserCallbacks-u-v1*>
    (<statically-typed-pointer>)
end class <DataBrowserCallbacks-u-v1*>;

define method content-size
  (thing == <DataBrowserCallbacks-u-v1*>)
=>(result :: <integer>)
  content-size(<DataBrowserCallbacks-u*>);
end method content-size;

define method itemDataCallback-value-setter
    (callback :: <DataBrowserItemDataUPP>, callbacks :: <DataBrowserCallbacks-u-v1*>)
 => (callback :: <DataBrowserItemDataUPP>)
  pointer-at(callbacks, offset: 0) := callback;
end method itemDataCallback-value-setter;

define method itemCompareCallback-value-setter
    (callback :: <DataBrowserItemCompareUPP>, callbacks :: <DataBrowserCallbacks-u-v1*>)
 => (callback :: <DataBrowserItemCompareUPP>)
  pointer-at(callbacks, offset: 4) := callback;
end method itemCompareCallback-value-setter;

define method itemNotificationCallback-value-setter
    (callback :: <DataBrowserItemCompareUPP>, callbacks :: <DataBrowserCallbacks-u-v1*>)
 => (callback :: <DataBrowserItemCompareUPP>)
  pointer-at(callbacks, offset: 8) := callback;
end method itemNotificationCallback-value-setter;

define constant $kDataBrowserLatestCallbacks = c-expr(int: "kDataBrowserLatestCallbacks");
        
define method InitDataBrowserCallbacks
    (callbacks :: <DataBrowserCallbacks*>)
 => (result :: <OSStatus>)
        as(<OSErr>, call-out("InitDataBrowserCallbacks", int:, ptr: callbacks.raw-value));
end method InitDataBrowserCallbacks;        
        
define method SetDataBrowserCallbacks
    (control :: <ControlRef>, callbacks :: <DataBrowserCallbacks*>)
 => (result :: <OSStatus>)
        as(<OSErr>, call-out("SetDataBrowserCallbacks", int:, ptr: control.raw-value, 
          ptr: callbacks.raw-value));
end method SetDataBrowserCallbacks;       

define functional class <DataBrowserItemDataRef>
		(<statically-typed-pointer>)
end class <DataBrowserItemDataRef>;

define method SetDataBrowserItemDataText
		(itemData :: <DataBrowserItemDataRef>, theData :: <CFStringRef>)
 => (status :: <OSStatus>)
 	as(<OSStatus>, call-out("SetDataBrowserItemDataText", int:, ptr: itemData.raw-value, ptr: theData.raw-value));
 end method SetDataBrowserItemDataText;

define method SetDataBrowserHasScrollBars
		(browser :: <ControlRef>, horiz :: <boolean>, vert :: <boolean>)
 => (status :: <OSStatus>)
	let horiz? = if(horiz) 1 else 0 end;
	let vert? = if(vert) 1 else 0 end;
		as(<OSStatus>, call-out("SetDataBrowserHasScrollBars", int:, ptr: browser.raw-value, int: horiz?, int: vert?));
end method SetDataBrowserHasScrollBars;

define method SetDataBrowserListViewHeaderBtnHeight
		(browser :: <ControlRef>, height :: <integer>)
 => (status :: <OSStatus>)
		as(<OSStatus>, call-out("SetDataBrowserListViewHeaderBtnHeight", int:, ptr: browser.raw-value, int: height));
end method SetDataBrowserListViewHeaderBtnHeight;

// Tabs