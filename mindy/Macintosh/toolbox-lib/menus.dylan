module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Menu Manager.

define class <MenuBarHandle> (<Handle>) end class;
define class <MenuHandle> (<Handle>) end class;

define constant GetNewMBar = get-c-function("GetNewMBar", args: list(<integer>),
											result: <MenuBarHandle>, file: *InterfaceLib*);
define constant SetMenuBar = get-c-function("SetMenuBar", args: list(<MenuBarHandle>),
											result: #(), file: *InterfaceLib*);
define constant DrawMenuBar = get-c-function("DrawMenuBar", args: #(),
											result: #(), file: *InterfaceLib*);
define constant HiliteMenu = get-c-function("HiliteMenu", args: list(<integer>),
											result: #(), file: *InterfaceLib*);

// Note:  the following use <extended-integer> because all 32-bits of the result are significant.

define constant MenuSelect =
begin
	let func = get-c-function("MenuSelect", args: list(<integer>),
											result: <extended-integer>, file: *InterfaceLib*);
	method (clickPt :: <Point>) => (menu :: <integer>, item :: <integer>);
		let result = func(as(<integer>, clickPt));
		floor/(result, 65536);
//		let (menu, item) = floor/(result, 65536);
//		values(menu, item);
//		values(as(<fixed-integer>, menu), as(<fixed-integer>, item));
	end method;
end;

define constant MenuKey =
begin
	let func = get-c-function("MenuKey", args: list(<integer>),
											result: list(<extended-integer>), file: *InterfaceLib*);
	method (ch :: <character>) => (menu :: <integer>, item :: <integer>);
		let result = func(as(<integer>, ch));
		floor/(result, 65536);
//		let (menu, item) = floor/(result, 65536);
//		values(menu, item);
//		values(as(<fixed-integer>, menu), as(<fixed-integer>, item));
	end method;
end;

define constant GetMenuHandle = get-c-function("GetMenuHandle", args: list(<integer>),
											result: <MenuHandle>, file: *InterfaceLib*);
define constant CountMItems = get-c-function("CountMItems", args: list(<MenuHandle>),
											result: <integer>, file: *InterfaceLib*);
define constant GetMenuItemText = get-c-function("GetMenuItemText",
											args: list(<MenuHandle>, <integer>, <Pascal-string>),
											result: <integer>, file: *InterfaceLib*);
define constant EnableItem = get-c-function("EnableItem", args: list(<MenuHandle>, <integer>),
											result: #(), file: *InterfaceLib*);
define constant DisableItem = get-c-function("DisableItem", args: list(<MenuHandle>, <integer>),
											result: #(), file: *InterfaceLib*);

// adding resource types to menus.

define constant AppendResMenu = get-c-function("AppendResMenu", args: list(<MenuHandle>, <OSType>),
											result: #(), file: *InterfaceLib*);

// Desk Accessory Support.

define constant OpenDeskAcc = get-c-function("OpenDeskAcc",
											args: list(<Pascal-string>),
											result: <integer>, file: *InterfaceLib*);
