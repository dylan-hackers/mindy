module: dylan-user

/*
	mac-types
*/

define module mac-types

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	
	export	// utilities
			<OSErr>, <OSStatus>, <OSType>, os-type,
			Debugger, DebugStr,
			
			// pascal string	
			<Pascal-string>;
			
end module mac-types;