Module:       carbon-duim


// XXX - Do we need to store the menu ref in the object???


// NOTE: Mac menu items are indexed starting from 1


// Constants

define constant $menu-command-id-base :: <integer> = 1;


// Variables

define variable *popup-menu-id* :: <integer> = -128;
define variable *menu-id* :: <integer> = 128;


// Mapping menus to mirrors and sheets

define constant $mirror-MenuRef-table  :: <object-table> = make(<table>);

define sealed method menu-mirror(handle :: <MenuRef>) 
=> (mirror :: false-or(<menu-mirror>))
  element($mirror-ControlHandle-table, pointer-address(handle), default: #f);
end method menu-mirror;

define sealed method menu-mirror-setter(mirror :: <menu-mirror>, handle :: <MenuRef>)
 => (mirror :: <menu-mirror>)
  element($mirror-ControlHandle-table, pointer-address(handle)) := mirror;
end method menu-mirror-setter;

define sealed method menu-mirror-setter(mirror :: singleton(#f), handle :: <MenuRef>) 
=> (mirror :: singleton(#f))
  remove-key!($mirror-ControlHandle-table, pointer-address(handle));
  #f;
end method menu-mirror-setter;

define sealed method MenuRef-sheet(handle :: <MenuRef>)
 => (sheet :: false-or(<mirrored-sheet-mixin>))
  let mirror = menu-mirror(handle);
  mirror & mirror-sheet(mirror);
end method MenuRef-sheet;


// Utilities

define method make-popup-menu
		()
 => (menu :: <MenuRef>)
  let menu :: <MenuRef> = NewMenu(*popup-menu-id*, $empty-pascal-string);
	*popup-menu-id* := *popup-menu-id* - 1;
	menu;
end method make-popup-menu;

define method make-menu
		(title :: <string>)
 => (menu :: <MenuRef>)
  let menu :: false-or(<MenuRef>) = #f;
  with-pascal-string(pstr = title)
  	menu := NewMenu(*menu-id*, pstr);
  end with-pascal-string;
	*menu-id* := *menu-id* + 1;
	menu;
end method make-menu;
 
define method delete-menu-contents
		(menu :: <MenuRef>)
 => (menu :: <MenuRef>)
	let items :: <integer> = CountMenuItems(menu);
	for(i :: <integer> from 1 to items)
		DeleteMenuItem(menu, i);
	end for;
	menu;
end method delete-menu-contents;

define method add-menu-contents
		(menu :: <MenuRef>, collection-gadget :: <collection-gadget-mixin>)
 => (menu :: <MenuRef>)
  let items = gadget-items(collection-gadget);
	for(i :: <integer> from 0 below items.size)
		with-pascal-string(pstr = items[i])
			InsertMenuItem(menu, pstr, i + 1);
			SetMenuItemCommandID(menu, i + 1, $menu-command-id-base + i);
		end with-pascal-string;
	end for;
end method add-menu-contents;


// Mirror Classes

// Mac Menus are NOT controls, so we cannot treat them as such
// They ARE event targets, so we can attach event handlers to them

define sealed class <menu-mirror> (<carbon-mirror>)
  sealed slot menu-ref :: false-or(<MenuRef>),
     required-init-keyword: MenuRef:;
end class <menu-mirror>;

define sealed domain make (singleton(<menu-mirror>));
define sealed domain initialize (<menu-mirror>);

define method initialize
    (mirror :: <menu-mirror>, #key) => ()
  next-method();
  menu-mirror(mirror.menu-ref) := mirror;
end method initialize;

define sealed method destroy-mirror 
    (_port :: <carbon-port>, sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  next-method();
  let ref :: false-or(<MenuRef>) = menu-ref(mirror);
  when(ref)  
    check-result("DisposeMenu", DisposeMenu(ref))
  end;
  note-mirror-destroyed(sheet, mirror);
end method destroy-mirror;

define sealed method note-mirror-destroyed
    (sheet :: <sheet>, mirror :: <menu-mirror>) => ()
  let ref :: <MenuRef> = menu-ref(mirror);
  menu-mirror(ref) := #f;
  menu-ref(mirror) := #f;
end method note-mirror-destroyed;