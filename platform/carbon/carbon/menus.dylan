module: carbon

/* 
	Mac Menu Manager.
*/


/*
	Includes
*/

c-include("Carbon.h");

/*
  Constants
*/

// Other Marks are as per Fonts

define constant $noMark :: <integer> = c-expr(int: "noMark");

// Get/SetMenuItemModifiers

define constant $kMenuNoModifiers :: <integer> = c-expr(int: "kMenuNoModifiers");
define constant $kMenuShiftModifier :: <integer> = c-expr(int: "kMenuShiftModifier");
define constant $kMenuOptionModifier :: <integer> = c-expr(int: "kMenuOptionModifier");
define constant $kMenuControlModifier :: <integer> = c-expr(int: "kMenuControlModifier");
define constant $kMenuNoCommandModifier :: <integer> = c-expr(int: "kMenuNoCommandModifier");

/*
	Menu Handles.
*/

define functional class <MenuBarHandle> ( <Handle> )
end class <MenuBarHandle>;


define functional class <MenuHandle> ( <Handle> )
end class <MenuHandle>;


define constant <MenuRef> :: <type> = <MenuHandle>;


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

define method MenuSelect( start<Point*> :: <Point*> )
=> ( menu :: <integer>, item :: <integer> )
	let result = call-out("menuselect", int:, ptr: start<Point*>.raw-value );
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

/*
	GetMBarHeight
*/

define method GetMBarHeight()
=> (result ::<integer>)
  call-out("GetMBarHeight", int:);
end method GetMBarHeight;


define method AcquireRootMenu()
=> (menu :: <MenuRef>)
  make(<MenuRef>, pointer: call-out("AcquireRootMenu", ptr:));
end method AcquireRootMenu;

define method SetRootMenu(menu :: <MenuRef>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetRootMenu", int:, ptr: menu.raw-value));
end method SetRootMenu;

define method CheckMenuItem(menu :: <MenuRef>, 
                             item :: <integer>,
                             checked :: <boolean>)
=> ()
  call-out("CheckMenuItem", void:, ptr: menu.raw-value, 
           int: item, int: if(checked) 1 else 0 end);
end method CheckMenuItem;

define method SetMenuItemText(menu :: <MenuRef>,
                             item :: <integer>, 
                             string :: <pascal-string>)
=> ()
  call-out("SetMenuItemText", void:, ptr: menu.raw-value, 
           int: item, ptr: string.raw-value);
end method SetMenuItemText;
  
define method GetItemMark(menu :: <MenuRef>, item :: <integer>)
=> (modifiers :: <character>)
  let temp :: <Handle> = make(<Handle>);
  call-out("GetItemMark", void:, 
                              ptr: menu.raw-value, 
                              int: item, ptr: temp.raw-value);
  signed-byte-at(temp, offset: 0);
end method GetItemMark;

define method SetItemMark(menu :: <MenuRef>, item :: <integer>,
                                   modifiers :: <character>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetItemMark", int:, ptr: menu.raw-value, 
                       int: item, char: modifiers));
end method SetItemMark;
  
define method GetItemCmd(menu :: <MenuRef>, item :: <integer>)
=> (modifiers :: <character>)
  let temp :: <Handle> = make(<Handle>);
  call-out("GetItemCmd", void:, 
                              ptr: menu.raw-value, 
                              int: item, ptr: temp.raw-value);
         signed-byte-at(temp, offset: 0);
end method GetItemCmd;

define method SetItemCmd(menu :: <MenuRef>, item :: <integer>,
                                   modifiers :: <character>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetItemCmd", int:, ptr: menu.raw-value, 
                       int: item, char: modifiers));
end method SetItemCmd;  

define method GetItemStyle(menu :: <MenuRef>, item :: <integer>)
=> (command :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  call-out("GetItemStyle", void:, 
                              ptr: menu.raw-value, 
                              int: item, ptr: temp.raw-value);
         signed-long-at(temp, offset: 0);
end method GetItemStyle;

define method SetItemStyle(menu :: <MenuRef>, item :: <integer>,
                                   command :: <integer>)
=> ()
  call-out("SetItemStyle", void:, ptr: menu.raw-value, 
                       int: item, int: command);
end method SetItemStyle;

define method GetMenuItemCommandID(menu :: <MenuRef>, item :: <integer>)
=> (result :: <OSErr>, command :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetMenuItemCommandID", int:, 
                              ptr: menu.raw-value, 
                              int: item, ptr: temp.raw-value)),
         signed-long-at(temp, offset: 0));
end method GetMenuItemCommandID;

define method SetMenuItemCommandID(menu :: <MenuRef>, item :: <integer>,
                                   command :: <integer>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetMenuItemCommandID", int:, ptr: menu.raw-value, 
                       int: item, int: command));
end method SetMenuItemCommandID;

define method GetMenuItemModifiers(menu :: <MenuRef>, item :: <integer>)
=> (result :: <OSErr>, modifiers :: <character>)
  let temp :: <Handle> = make(<Handle>);
  values(as(<OSErr>, call-out("GetMenuItemModifiers", int:, 
                              ptr: menu.raw-value, 
                              int: item, ptr: temp.raw-value)),
         signed-byte-at(temp, offset: 0));
end method GetMenuItemModifiers;

define method SetMenuItemModifiers(menu :: <MenuRef>, item :: <integer>,
                                   modifiers :: <character>)
=> (result :: <OSErr>)
  as(<OSErr>, call-out("SetMenuItemModifiers", int:, ptr: menu.raw-value, 
                       int: item, char: modifiers));
end method SetMenuItemModifiers;

define method NewMenu(menuID :: <integer>, string :: <pascal-string>)
=> (menu :: <MenuRef>)
  make(<MenuRef>, pointer: call-out("NewMenu", ptr:, 
                                    int: menuID, 
                                    ptr: string.raw-value));
end method NewMenu;

define method InsertMenuItem(menu :: <MenuRef>, 
                             string :: <pascal-string>,
                             item :: <integer>)
=> ()
  call-out("InsertMenuItem", void:, ptr: menu.raw-value, 
           ptr: string.raw-value, int: item);
end method InsertMenuItem;

define method DeleteMenuItem(menu :: <MenuRef>, item :: <integer>)
=> ()
  call-out("DeleteMenuItem", void:, ptr: menu.raw-value, int: item);
end method DeleteMenuItem;

define method InsertMenu(menu :: <MenuRef>, menuID :: <integer>)
=> ()
  call-out("InsertMenu", void:, ptr: menu.raw-value, int: menuID);
end method InsertMenu;

define method ClearMenuBar()
=> ()
  call-out("ClearMenuBar", void:);
end method ClearMenuBar;

define method IsValidMenu(inMenu :: <MenuRef>)
=> (result :: <boolean>)
  if(call-out("IsValidMenu", int:, ptr: inMenu.raw-value))
    #t;
  else
    #f;
  end if;
end method IsValidMenu;



