module: dylan-user

/*
	gestalt
*/

define module gestalt

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
		
	export	// Gestalt
		Gestalt,
		$gestaltQuickdrawVersion, $gestaltOriginalQD, $gestalt8BitQD, $gestalt32BitQD,
		$gestalt32BitQD11, $gestalt32BitQD12, $gestalt32BitQD13, $gestaltAllegroQD;
		
end module gestalt;







