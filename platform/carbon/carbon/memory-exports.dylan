module: dylan-user

/*
	memory
*/

define module memory

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	
	export	// Memory Manager
			$nil, $NULL,
			<Ptr>, 	NewPtr, DisposePtr, 
			<Handle>, NewHandle, DisposeHandle;
			
end module memory;
