module: dylan-user

/*
	quickdraw
*/

define module quickdraw

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	use memory;
	
	export	// QuickDraw.
                <Point>, point-v, point-v-setter, point-h, point-h-setter,
		point,
		
		<Rect>, top, top-setter, left, left-setter,
				bottom, bottom-setter, right, right-setter,
		SetRect, PtInRect, InsetRect,
				
		<BitMap>, bounds, //<QDGlobals>, screenBits, qd,
		<RgnHandle>, NewRgn, DisposeRgn, SetEmptyRgn, SetRectRgn, RectRgn,
		
		<CGrafPtr>, SetPort, GetPort,
		MoveTo, LineTo, DrawString, TextFont,
		PenMode, $patOr, $patCopy, $patXor, 
		SetOrigin,

		EraseRect, FrameRect, InvertRect, PaintRect,
		EraseOval, FrameOval, InvertOval, PaintOval,
		
		ClipRect, 
		
		InitCursor, HideCursor, ShowCursor,
		ObscureCursor,
		
		TextSize, TextFont, TextMode,
		// GetFNum,
		
		<RGBColor>,
		RGBForeColor, RGBBackColor, InvertColor,
		red, blue, green, red-setter, green-setter, blue-setter,
		
		<GDHandle>, <PixMapHandle>, <GWorldPtr>,
		NewGWorld, DisposeGWorld, GetGWorld, GetGWorldPixMap,
		LockPixels, UnlockPixels, GetGWorldDevice, PixMap32Bit,
                
                GetPortBounds, SetPortBounds, GetPortVisibleRegion, SetPortVisibleRegion;
		
end module quickdraw;


