module: dylan-user

/*
	dialogs
*/

define module dialogs

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use controls;
	use events;
	use memory;
	use quickdraw;
	use windows;
	
	export	// Dialog Manager.
		<DialogRef>, <ModalFilterUPP>, $uppModalFilterProcInfo,
		$kControlDialogItem, $kButtonDialogItem, $kCheckBoxDialogItem,
		$kRadioButtonDialogItem, $kResourceControlDialogItem, $kStaticTextDialogItem,
		$kEditTextDialogItem, $kIconDialogItem, $kPictureDialogItem,
		$kUserDialogItem, $kItemDisableBit,
		Alert,
		GetNewDialog, DisposeDialog,
		SetDialogDefaultItem, SetDialogCancelItem,
		IsDialogEvent, DialogSelect,
		GetDialogItem, GetDialogItemText, SetDialogItemText,
		CountDITL,
                GetDialogWindow, GetDialogPort,
                
                // Appearance
                
                <AlertStdAlertParam>, <AlertType>,
                $kAlertStopAlert, $kAlertNoteAlert, $kAlertCautionAlert, $kAlertPlainAlert,
                StandardAlert;
		
end module dialogs;






