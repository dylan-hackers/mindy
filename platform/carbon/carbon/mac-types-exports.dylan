module: dylan-user

/*
	mac-types
*/

define module mac-types

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	
	export	
                // Basic Types
                        <UInt8>, <SInt8>, <UInt16>, <SInt16>, <UInt32>,
                        <SInt32>,  <OSErr>, <OSStatus>, <FourCharCode>, 
                        <OSType>, <ResType>,
        
                // Utilities
			os-type,
			Debugger, DebugStr,
                        
                // Error Codes        
                        $noErr,
                        
                // pascal string	
			<pascal-string>;
			
end module mac-types;
