module: dylan-user

/*
	carbon
*/

define library carbon

	use Dylan;
	use melange-support;
	export	mac-types, dialogs, events, gestalt, memory, files, menus, os-utils, 
			quickdraw, resources, sound, windows, controls,
                        appearance, carbon-events;

end library carbon;

define module carbon

    // Export all the Carbon Toolbox APIs in one easily imported module

    use mac-types,
        export: all;
    use dialogs, 
        export: all;
    use events, 
        export: all;
    use gestalt, 
        export: all;
    use memory, 
        export: all;
    use files, 
        export: all;
    use menus, 
        export: all;
    use os-utils, 
        export: all;
    use quickdraw, 
        export: all;
    use resources, 
        export: all;
    use sound, 
        export: all;
    use windows, 
        export: all;
    use controls,
        export: all;
    use appearance, 
        export: all;
    use carbon-events,
        export: all;

end module carbon;