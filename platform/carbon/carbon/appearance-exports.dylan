module: dylan-user

/*
        appearance
*/

define module appearance
        use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
        
	export // appearance
        
        // Types and classes
        <ThemeBrush>, <ThemeBackgroundKind>, <ThemeDrawingState>,	
        
        // Methods
        RegisterAppearanceClient, UnregisterAppearanceClient,
        GetThemeDrawingState, SetThemeDrawingState;
            
end module appearance;











