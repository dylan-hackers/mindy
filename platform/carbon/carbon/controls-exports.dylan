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
			
		<ControlHandle>, <ControlActionUPP>,
		NewControl, DisposeControl, KillControls,
		HiliteControl, ShowControl, HideControl,
		GetControlValue, SetControlValue, MoveControl, SizeControl,
		SetControlTitle, GetControlTitle, 
		DragControl, FindControl, TrackControl, TestControl,
		DrawControls;
		
end module controls;





