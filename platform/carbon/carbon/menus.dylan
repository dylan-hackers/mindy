module: menus

/* 
	Mac Menu Manager.
*/


/*
	Includes
*/

c-include("Carbon/Carbon.h");


/*
	Menu Handles.
*/

define functional class <MenuBarHandle> ( <Handle> )
end class <MenuBarHandle>;


define functional class <MenuHandle> ( <Handle> )
end class <MenuHandle>;


/*
	GetNewMBar
*/

define method GetNewMBar( menuID :: <integer> )
=> ( result :: <MenuBarHandle> ) 
	let mbarH = call-out( "GetNewMBar", ptr:, short: menuID );
	make( <MenuBarHandle>, pointer: mBarH );
end method GetNewMBar;


/*
	SetMenuBar
*/

define method SetMenuBar( mBar :: <MenuBarHandle> )
=> ()
	call-out( "SetMenuBar", void:, ptr: mBar.raw-value );
	values();
end method SetMenuBar;


/*
	DrawMenuBar
*/	

define method DrawMenuBar()
=> ()
	call-out( "DrawMenuBar", void: );
	values();
end method DrawMenuBar;
											

/*
	HiliteMenu
*/
											
define method HiliteMenu( hilite :: <boolean> )
=> ()
	let on :: <integer> = if( hilite ) 1 else 0 end if;
	call-out("HiliteMenu", void:, unsigned-char: on );
	values();
end method HiliteMenu;


/*
	MenuSelect
	Deviates from the Mac version for ease of use and to avoid <extended-integer>
	outside of itself
*/

define method MenuSelect( startPoint :: <Point> )
=> ( menu :: <integer>, item :: <integer> )
	let result = call-out("menuselect", int:, ptr: startPoint.raw-value );
	floor/(result, 65536);
end method MenuSelect;


/*
	MenuKey
	Deviates from the Mac version for ease of use and to avoid <extended-integer>
	outside of itself
*/

define method MenuKey( keyCode :: type-union( <integer>, <character> ) )
=> ( menu :: <integer>, item :: <integer> )
	let param = as( <integer>, keyCode );
	let result = call-out( "MenuKey", int:, short: param );
	floor/(result, 65536);
end method MenuKey;


/*
	GetMenuHandle
*/

define method GetMenuHandle( menuID :: <integer> )
=> ( result :: <MenuHandle> )
	let mH = call-out( "GetMenuHandle", ptr:, short: menuID );
	make( <MenuHandle>, pointer: mH );
end method GetMenuHandle;


/*
	GetMenuItemText
*/

define method GetMenuItemText( menu :: <MenuHandle>, which :: <integer>, text :: <pascal-string> )
=> () 
	call-out("GetMenuItemText", void:, ptr: menu.raw-value, short: which, ptr: text.raw-value );
	values();
end method GetMenuItemText;


/*
	EnableMenuItem
*/

define method EnableMenuItem( menu :: <MenuHandle>, which :: <integer> )
=> () 
	call-out("EnableMenuItem", void:, ptr: menu.raw-value, short: which );
	values();
end method EnableMenuItem;


/*
	DisableMenuItem
*/

define method DisableMenuItem( menu :: <MenuHandle>, which :: <integer> )
=> () 
	call-out("DisableMenuItem", void:, ptr: menu.raw-value, short: which );
	values();
end method DisableMenuItem;


/*
	AppendResMenu
*/

define method AppendResMenu( to :: <MenuHandle>, which :: <OSType> )
=> ()
	call-out("AppendResMenu", void:, ptr: to.raw-value, unsigned-int: which );
	values();
end method AppendResMenu;

/*
	CountMenuItems
*/

define method CountMenuItems( menu :: <MenuHandle> )
=> ( result :: <integer> )

	call-out( "CountMenuItems", short:, ptr: menu.raw-value );

end method CountMenuItems;


/*
	DeleteMenu
*/

define method DeleteMenu( id :: <integer> )
=> ()

	call-out( "MacDeleteMenu", void:, short: id );

	values();

end method DeleteMenu;


/*
	Desk Accessory Support.
*/

/*
	OpenDeskAcc
*/

/*define method OpenDeskAcc( name :: <pascal-string> )
=> ( result :: <integer> ) 
	call-out( "OpenDeskAcc", short:, ptr: name.raw-value );
end method OpenDeskAcc;
*/
