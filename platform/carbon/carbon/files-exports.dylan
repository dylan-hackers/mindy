module: dylan-user

/*
	files
*/

define module files

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	
	export	// Files
			$fsCurPerm, $fsRdPerm, $fsWrPerm, $fsRdWrPerm, 
			$fsRdWrShPerm, $fsRdDenyPerm, $fsWrDenyPerm, 
			<FSSpec>, 
			vRefNum, vrefNum-setter, parID, parID-setter, FSSpec-name, FSSpec-name-setter,
			<FSRef>,
			FSClose, FSRead, FSWrite, Allocate, GetEOF, SetEOF, GetFPos, SetFPos,
			GetVRefNum, FSpOpenRF, FSpOpenDF, FSpCreate, FSpDirCreate, FSpDelete,
			FSpRename, FSpCatMove, FSpExchangeFiles, 
			FSpMakeFSRef;
			
end module files;

