module: dylan-user

/*
	controls
*/

define module controls

	use Dylan;
	//use Extensions;						// <extended-integer>
	use events;
	use melange-support;
	use mac-types;
	use memory;
	use quickdraw;
	use windows;
	
	export	// Control manager.
		$pushButProc, $checkBoxProc, $radioButProc, $scrollBarProc, $popupMenuProc,
		
		$kControlEditTextProc, $kControlEditTextPasswordProc, $kControlEditTextInlineInputProc, 
		$kControlStaticTextProc, $kControlPictureProc, $kControlPictureNoTrackProc,

		$kControlLabelPart, $kControlMenuPart, $kControlTrianglePart, $kControlEditTextPart,
		$kControlPicturePart, $kControlIconPart, $kControlClockPart, $kControlListBoxPart,
		$kControlListBoxDoubleClickPart, $kControlImageWellPart, $kControlRadioGroupPart,
		$kControlButtonPart, $kControlCheckBoxPart, $kControlRadioButtonPart, $kControlUpButtonPart,
		$kControlDownButtonPart, $kControlPageUpPart, $kControlPageDownPart,
		$kControlClockHourDayPart, $kControlClockMinuteMonthPart, $kControlClockSecondYearPart,
		$kControlClockAMPMPart, $kControlDataBrowserPart, $kControlDataBrowserDraggedPart,	
                
                $kControlKindBevelButton, $kControlKindChasingArrows, $kControlKindClock, 
                $kControlKindDataBrowser, $kControlKindDisclosureButton, $kControlKindDisclosureTriangle,	
                $kControlKindEditText, $kControlKindGroupBox, $kControlKindIcon, $kControlKindImageWell,	
                $kControlKindListBox, $kControlKindLittleArrows, $kControlKindPicture, $kControlKindPlacard,	
                $kControlKindPopupArrow, $kControlKindPopupButton, $kControlKindProgressBar,	
                $kControlKindPushButton, $kControlKindRadioGroup, $kControlKindRoundButton, 
                $kControlKindScrollBar,	$kControlKindScrollingTextBox,$kControlKindSeparator,
                $kControlKindSignatureApple, $kControlKindSlider, $kControlKindStaticText,$kControlKindTabs,	
                $kControlKindUserPane, $kControlKindWindowHeader,
			
		<ControlHandle>, <ControlActionUPP>,
		NewControl, DisposeControl, KillControls,
		HiliteControl, ShowControl, HideControl,
		GetControlValue, SetControlValue, MoveControl, SizeControl,
		SetControlTitle, GetControlTitle, 
		DragControl, FindControl, HandleControlClick, HandleControlKey, IdleControls, 
                TrackControl, TestControl,
                AdvanceKeyboardFocus, ClearKeyboardFocus, GetKeyboardFocus, ReverseKeyboardFocus, SetKeyboardFocus,
		DrawControls;
		
end module controls;





