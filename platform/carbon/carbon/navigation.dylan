module: navigation


/*
        c-includes
*/

c-include( "MacTypes.h" );
c-include( "Navigation.h" );

/*
        Constants
*/

define constant <NavEventUPP> = <UniversalProcPtr>;
define constant <NavPreviewUPP> = <UniversalProcPtr>;
define constant <NavObjectFilterUPP> = <UniversalProcPtr>;

define constant <NavTypeListHandle> = <UniversalProcPtr>;    //- FIXME: !!! Define class later
define constant <NavMenuItemSpecArrayHandle> = <UniversalProcPtr>;   //- FIXME: !!! Define class later 
define constant <FileTranslationSpecArrayHandle> = <UniversalProcPtr>;       //- FIXME: !!! Define class later

define constant $kNavDefaultNavDlogOptions   = #x000000E4;                   /* use defaults for all the options */
define constant $kNavNoTypePopup             = #x00000001;                   /* don't show file type/extension popup on Open/Save */
define constant $kNavDontAutoTranslate       = #x00000002;                   /* don't automatically translate on Open */
define constant $kNavDontAddTranslateItems   = #x00000004;                   /* don't add translation choices on Open/Save */
define constant $kNavAllFilesInPopup         = #x00000010;                   /* "All Files" menu item in the type popup on Open */
define constant $kNavAllowStationery         = #x00000020;                   /* allow saving of stationery files */
define constant $kNavAllowPreviews           = #x00000040;                   /* allow preview to show */
define constant $kNavAllowMultipleFiles      = #x00000080;                   /* allow multiple items to be selected */
define constant $kNavAllowInvisibleFiles     = #x00000100;                   /* allow invisible items to be shown */
define constant $kNavDontResolveAliases      = #x00000200;                   /* don't resolve aliases */
define constant $kNavSelectDefaultLocation   = #x00000400;                   /* make the default location the browser selection */
define constant $kNavSelectAllReadableItem   = #x00000800;                   /* make the dialog select "All Readable Documents" on open */
define constant $kNavSupportPackages         = #x00001000;                   /* recognize file system packages, v2.0 or greater */
define constant $kNavAllowOpenPackages       = #x00002000;                   /* allow opening of packages, v2.0 or greater */
define constant $kNavDontAddRecents          = #x00004000;                   /* don't add chosen objects to the recents list, v2.0 or greater */
define constant $kNavDontUseCustomFrame      = #x00008000;                    /* don't add the bevelled custom frame, v2.0 or greater */


/*
	<NavDialogOptions>
	
	Un-implemented fields, excluding resreved fields
		UInt32                          preferenceKey;    
		NavMenuItemSpecArrayHandle      popupExtension;  
*/

define functional class <NavDialogOptions>      (<statically-typed-pointer>)
end class <NavDialogOptions>;


/*
	content-size <NavDialogOptions>
*/

define method content-size( class == <NavDialogOptions> )
=> ( result :: <integer> )

        c-expr( int:, "sizeof( NavDialogOptions )" );
        
end method content-size;



/*
	version
*/

define method navDialog-version( options :: <NavDialogOptions> )
=>( result :: <integer> )

	unsigned-short-at( options, offset: 0 );

end method navDialog-version;


/*
	version-setter
*/

define method navDialog-version-setter( options :: <NavDialogOptions>, value :: <integer> )
=>()

	unsigned-short-at( options, offset: 0 ) := value;
	
	values();

end method navDialog-version-setter;


/*
	dialogOptionFlags
*/

define method dialogOptionFlags( options :: <NavDialogOptions> )
=>( result :: <integer> )

	unsigned-long-at( options, offset: 2 );

end method dialogOptionFlags;


/*
	dialogOptionFlags-setter
*/

define method dialogOptionFlags-setter( options :: <NavDialogOptions>, value :: <integer> )
=>()

	unsigned-long-at( options, offset: 2 ) := value;
	
	values();

end method dialogOptionFlags-setter;


/*
	location
*/

define method navDialog-location( options :: <NavDialogOptions> )
=>( result :: <point> )

	make( <Point>,	v: unsigned-short-at( options, offset: 6 ),
					h: unsigned-short-at( options, offset: 8 ) );

end method navDialog-location;


/*
	location-setter
*/

define method navDialog-location-setter( options :: <NavDialogOptions>, value :: <Point> )
=>()

	unsigned-short-at( options, offset: 6 ) := value.point-v;
	unsigned-short-at( options, offset: 8 ) := value.point-h;
	
	values();

end method navDialog-location-setter;


/*
	clientName
*/

define method clientName( options :: <NavDialogOptions> )
=>( result :: <pascal-string> )

	pointer-at( options, offset: 10, class: <pascal-string> );

end method clientName;


/*
	clientName-setter
*/

define method clientName-setter( options :: <NavDialogOptions>, value :: <string> )
=>()

	pointer-at( options, offset: 10, class: <pascal-string> ) := value;
	
	values();

end method clientName-setter;


/*
	windowTitle
*/

define method windowTitle( options :: <NavDialogOptions> )
=>( result :: <pascal-string> )

	pointer-at( options, offset: 266, class: <pascal-string> );

end method windowTitle;

/*
	windowTitle-setter
*/

define method windowTitle-setter( options :: <NavDialogOptions>, value :: <string> )
=>()

	pointer-at( options, offset: 266, class: <pascal-string> ) := value;
	
	values();

end method windowTitle-setter;


/*
	actionButtonLabel
*/

define method actionButtonLabel( options :: <NavDialogOptions> )
=>( result :: <pascal-string> )

	pointer-at( options, offset: 522, class: <pascal-string> );

end method actionButtonLabel;

/*
	actionButtonLabel-setter
*/

define method actionButtonLabel-setter( options :: <NavDialogOptions>, value :: <string> )
=>()

	pointer-at( options, offset: 522, class: <pascal-string> ) := value;
	
	values();

end method actionButtonLabel-setter;


/*
	cancelButtonLabel
*/

define method cancelButtonLabel( options :: <NavDialogOptions> )
=>( result :: <pascal-string> )

	pointer-at( options, offset: 778, class: <pascal-string> );

end method cancelButtonLabel;


/*
	cancelButtonLabel-setter
*/

define method cancelButtonLabel-setter( options :: <NavDialogOptions>, value :: <string> )
=>()

	pointer-at( options, offset: 778, class: <pascal-string> ) := value;
	
	values();

end method cancelButtonLabel-setter;


/*
	savedFileName
*/

define method savedFileName( options :: <NavDialogOptions> )
=>( result :: <pascal-string> )

	pointer-at( options, offset: 1034, class: <pascal-string> );

end method savedFileName;


/*
	savedFileName-setter
*/

define method savedFileName-setter( options :: <NavDialogOptions>, value :: <string> )
=>()

	pointer-at( options, offset: 1034, class: <pascal-string> ) := value;
	
	values();

end method savedFileName-setter;


/*
	message
*/

define method navDialog-message( options :: <NavDialogOptions> )
=>( result :: <pascal-string> )

	pointer-at( options, offset: 1290, class: <pascal-string> );

end method navDialog-message;


/*
	message-setter
*/

define method navDialog-message-setter( options :: <NavDialogOptions>, value :: <string> )
=>()

	pointer-at( options, offset: 1290, class: <pascal-string> ) := value;
	
	values();

end method navDialog-message-setter;


/*
	<NavReplyRecord>
	
	Unimplemented fields, excluding reserved fields
		ScriptCode                      keyScript;             
		FileTranslationSpecArrayHandle  fileTranslation; 

*/

define functional class <NavReplyRecord>        (<statically-typed-pointer>)
end class <NavReplyRecord>;


/*
	content-size <NavReplyRecord>
*/

define method content-size( class == <NavReplyRecord> )
=> ( result :: <integer> )

        c-expr( int:, "sizeof( NavDialogOptions )" );
        
end method content-size;


/*
	version
*/


define method navReply-version( record :: <NavReplyRecord> )
=>( result :: <integer> )

	unsigned-short-at( record, offset: 0 );

end method navReply-version;

/*
	version-setter
*/

define method navReply-version-setter( record :: <NavReplyRecord>, value :: <integer> )
=>()

	unsigned-short-at( record, offset: 0 ) := value;
	
	values();

end method navReply-version-setter;


/*
	valid-record
*/

define method validRecord( record :: <NavReplyRecord> )
=>( result :: <integer> )

	unsigned-byte-at( record, offset: 2 );

end method validRecord;


/*
	valid-record-setter
*/

define method validRecord-setter( record :: <NavReplyRecord>, value :: <integer> )
=>()

	unsigned-byte-at( record, offset: 2 ) := value;
	
	values();

end method validRecord-setter;


/*
	replacing
*/

define method navReply-replacing( record :: <NavReplyRecord> )
=>( result :: <integer> )

	unsigned-byte-at( record, offset: 4 );

end method navReply-replacing;

/*

	replacing-setter
*/

define method navReply-replacing-setter( record :: <NavReplyRecord>, value :: <integer> )
=>()

	unsigned-byte-at( record, offset: 4 ) := value;
	
	values();

end method navReply-replacing-setter;


/*
	isStationary
*/

define method isStationary( record :: <NavReplyRecord> )
=>( result :: <integer> )

	unsigned-byte-at( record, offset: 6 );

end method isStationary;


/*
	isStationary-setter
*/

define method isStationary-setter( record :: <NavReplyRecord>, value :: <integer> )
=>()

	unsigned-byte-at( record, offset: 6 ) := value;
	
	values();

end method isStationary-setter;


/*
	translationNeeded
*/

define method translationNeeded( record :: <NavReplyRecord> )
=>( result :: <integer> )

	unsigned-byte-at( record, offset: 8 );

end method translationNeeded;


/*
	translationNeeded-setter
*/

define method translationNeeded-setter( record :: <NavReplyRecord>, value :: <integer> )
=>()

	unsigned-byte-at( record, offset: 8 ) := value;
	
	values();

end method translationNeeded-setter;


/*
	selection
*/

define method navReply-selection( record :: <NavReplyRecord> )
=>( result :: <integer> )

	pointer-at( record, offset: 10, class: <AEDEscList> );

end method navReply-selection;


/*
	selection-setter
*/

define method navReply-selection-setter( record :: <NavReplyRecord>, value :: <AEDescList> )
=>()

	pointer-at( record, offset: 10 ) := value;
	
	values();

end method navReply-selection-setter;


/*
	NavLoad
*/

define method NavLoad()
=> ( result :: <OSErr> )

        let result = call-out( "NavLoad", unsigned-int: );
        
        as( <OSErr>, result );

end method NavLoad;

/*
	NavUnload
*/

define method NavUnload()
=> ( result :: <OSErr> )

        let result = call-out( "NavUnload", unsigned-int: );
        
        as( <OSErr>, result );

end method NavUnload;


/*
	NavLibraryVersion
*/

define method NavLibraryVersion()
=> ( result :: <integer> )

        call-out( "NavLibraryVersion", unsigned-int: );

end method NavLibraryVersion;


/*
	NavGetDefaultOptions
*/

define method NavGetDefaultDialogOptions()
=> ( result :: <OSErr>, options :: <NavDialogOptions> )

        let options = make( <NavDialogOptions> );

        let result = call-out( "NavGetDefaultDialogOptions", unsigned-int:, ptr: options.raw-value );
        
        values( as( <OSErr>, result ), options );

end method NavGetDefaultDialogOptions;


/*
	NavGetFile
	
*/	
	
define method NavGetFile(	defaultLocation :: <AEDesc>, /* can be NULL */
							reply :: <NavReplyRecord>,
							dialogOptions :: <integer>,
							eventProc :: <NavEventUPP>, /* can be NULL */
							previewProc :: <NavPreviewUPP>, /* can be NULL */
							filterProc :: <NavObjectFilterUPP>, /* can be NULL */
							typeList :: <NavTypeListHandle>, /* can be NULL */
							callBackUD :: <Ptr> ) /* can be NULL */
=> ( result :: <OSErr> )

	let result = call-out(	"NavGetFile", int:, 
							ptr: defaultLocation.raw-value, 
							ptr: reply.raw-value,
							unsigned-int: dialogOptions,
							ptr: eventProc.raw-value,
							ptr: previewProc.raw-value,
							ptr: filterProc.raw-value,
							ptr: typeList.raw-value,
							ptr: callBackUD.raw-value );
	
	as( <OSErr>, result );

end method NavGetFile;


/*
	NavPutFile
	
*/	
	
define method NavPutFile(	defaultLocation :: <AEDesc>, /* can be NULL */
							reply :: <NavReplyRecord>,
							dialogOptions :: <integer>,
							eventProc :: <NavEventUPP>, /* can be NULL */
							fileType :: <OSType>,
							fileCreator :: <OSType>,
							callBackUD :: <Ptr> ) /* can be NULL */
=> ( result :: <OSErr> )

	let result = call-out(	"NavPutFile", int:, 
							ptr: defaultLocation.raw-value, 
							ptr: reply.raw-value,
							unsigned-int: dialogOptions,
							ptr: eventProc.raw-value,
							unsigned-int: fileType,
							unsigned-int: fileCreator,
							ptr: callBackUD.raw-value );
	
	as( <OSErr>, result );

end method NavPutFile;
                          
/*
	NavChooseFile
	
*/	
	
define method NavChooseFile(	defaultLocation :: <AEDesc>, /* can be NULL */
								reply :: <NavReplyRecord>,
								dialogOptions :: <integer>,
								eventProc :: <NavEventUPP>, /* can be NULL */
								previewProc :: <NavPreviewUPP>, /* can be NULL */
								filterProc :: <NavObjectFilterUPP>, /* can be NULL */
								typeList :: <NavTypeListHandle>, /* can be NULL */
								callBackUD :: <Ptr> ) /* can be NULL */
=> ( result :: <OSErr> )

	let result = call-out(	"NavChooseFile", int:, 
							ptr: defaultLocation.raw-value, 
							ptr: reply.raw-value,
							unsigned-int: dialogOptions,
							ptr: eventProc.raw-value,
							ptr: previewProc.raw-value,
							ptr: filterProc.raw-value,
							ptr: typeList.raw-value,
							ptr: callBackUD.raw-value );
	
	as( <OSErr>, result );

end method NavChooseFile;

/*
	NavChooseFolder
	
*/	
	
define method NavChooseFolder(	defaultLocation :: <AEDesc>, /* can be NULL */
							reply :: <NavReplyRecord>,
							dialogOptions :: <integer>,
							eventProc :: <NavEventUPP>, /* can be NULL */
							filterProc :: <NavObjectFilterUpp>, /* can be NULL */
							callBackUD :: <Ptr> ) /* can be NULL */
=> ( result :: <OSErr> )

	let result = call-out(	"NavChooseFolder", int:, 
							ptr: defaultLocation.raw-value, 
							ptr: reply.raw-value,
							unsigned-int: dialogOptions,
							ptr: eventProc.raw-value,
							ptr: filterProc.raw-value,
							ptr: callBackUD.raw-value );
	
	as( <OSErr>, result );

end method NavChooseFolder;

/*
	NavChooseVolume
	
*/	
	
define method NavChooseVolume(	defaultLocation :: <AEDesc>, /* can be NULL */
							reply :: <NavReplyRecord>,
							dialogOptions :: <integer>,
							eventProc :: <NavEventUPP>, /* can be NULL */
							filterProc :: <NavObjectFilterUpp>, /* can be NULL */
							callBackUD :: <Ptr> ) /* can be NULL */
=> ( result :: <OSErr> )

	let result = call-out(	"NavChooseVolume", int:, 
							ptr: defaultLocation.raw-value, 
							ptr: reply.raw-value,
							unsigned-int: dialogOptions,
							ptr: eventProc.raw-value,
							ptr: filterProc.raw-value,
							ptr: callBackUD.raw-value );
	
	as( <OSErr>, result );

end method NavChooseVolume;


/*
	NavDisposeReply
*/

define method NavDisposeReply( reply :: <NavReplyRecord> )
=> ( result :: <OSErr> )

	let result = call-out( "NavDisposeReply", int:, ptr: reply.raw-value );

end method NavDisposeReply;


/*
	NavServicesCanRun
*/

define method NavServicesCanRun()
=> ( result :: <OSErr> )

        let result = call-out( "NavServicesCanRun", unsigned-int: );
        
        if( result = 1 ) 
                #t 
        else 
                #f 
        end if;

end method NavServicesCanRun;