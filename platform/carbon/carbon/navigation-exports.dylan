module: dylan-user

/*
	navigation
*/

define module navigation

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	use events;
	use quickdraw;

	export // Navigation services
		<NavEventUPP>, <NavPreviewUPP>, <NavObjectFilterUPP>,
		<NavTypeListHandle>, <NavMenuItemSpecArrayHandle>, <FileTranslationSpecArrayHandle>,
		$kNavDefaultNavDlogOptions, $kNavNoTypePopup, $kNavDontAutoTranslate,
		$kNavDontAddTranslateItems, $kNavAllFilesInPopup, $kNavAllowStationery,
		$kNavAllowPreviews, $kNavAllowMultipleFiles, $kNavAllowInvisibleFiles,
		$kNavDontResolveAliases, $kNavSelectDefaultLocation, $kNavSelectAllReadableItem,
		$kNavSupportPackages, $kNavAllowOpenPackages,$kNavDontAddRecents,
		$kNavDontUseCustomFrame, 
		<NavDialogOptions>, navDialog-version, navDialog-version-setter, 
		dialogOptionFlags, dialogOptionFlags-setter,
		navDialog-location, navDialog-location-setter, clientName, clientName-setter, 
		windowTitle, windowTitle-setter, actionButtonLabel, actionButtonLabel-setter,
		cancelButtonLabel, cancelButtonLabel-setter, savedFilename, savedFileName-setter,
		navDialog-message, navDialog-message-setter, 
		<NavReplyRecord>, navReply-version, navReply-version-setter, 
		validRecord, validRecord-setter, navReply-replacing, navReply-replacing-setter,
		isStationary, isStationary-setter, translationNeeded, translationNeeded-setter,
		navReply-selection, navReply-selection-setter, 
		NavLoad, NavUnload, NavLibraryVersion, NavGetDefaultDialogOptions,
		NavGetFile, NavPutFile, NavChooseFile, NavChooseFolder, NavDisposeReply,
		NavServicesCanRun;

end module navigation;











