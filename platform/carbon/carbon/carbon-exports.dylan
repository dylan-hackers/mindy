module: dylan-user

/*
	carbon
*/

define library carbon

	use Dylan;
	use melange-support;
	export	mac-types, dialogs, events, gestalt, memory, files, menus, os-utils, 
			quickdraw, resources, sound, windows, controls,
                        carbon-events;

end library carbon;