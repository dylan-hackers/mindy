module: quickdraw

/*
	Includes.
*/

c-include("Carbon/Carbon.h");


/*
	Rob Myers replaced Patrick Beard's Mindy stack-alloc with non-threadsafe globals
*/


// QuickDraw Thread Safety from Patrick's Simple.dylan

//define constant $qd-lock = make(<multilock>);
//
//define method acquire-quickdraw (port :: <CGrafPtr>)
//    grab-lock($qd-lock);
//    SetPort(port);
//end method acquire-quickdraw;
//
//define method release-quickdraw ()
//    release-lock($qd-lock);
//end method release-quickdraw;

/*
	<Points>
	A QuickDraw Point.
*/

define functional class <Point> (<Ptr>)
end class;

/*
	content-size
	The size of object a <Point> contains
*/

define method content-size( cls == <Point> )
=>( result :: <integer> )
	c-expr( int: "sizeof(Point)" );
end method content-size;


/*
	initialize <Point>
*/

define method initialize( pt :: <Point>, #key v = 0, h = 0)
=> ( result :: <Point> )

  point-v( pt ) := v;
  point-h( pt ) := h;
  
  pt;

end method initialize;


/*
	Accessors for the h and v components of a <Point>
*/

define method point-v (pt :: <Point>) => (v :: <integer>);
	signed-short-at(pt, offset: 0);
end method point-v;


define method point-v-setter (value :: <integer>, pt :: <Point>) => (value :: <integer>);
	signed-short-at(pt, offset: 0) := value;
end method point-v-setter;


define method point-h (pt :: <Point>) => (h :: <integer>);
	signed-short-at(pt, offset: 2);
end method point-h;


define method point-h-setter (value :: <integer>, pt :: <Point>) => (value :: <integer>);
	signed-short-at(pt, offset: 2) := value;
end method point-h-setter;


/*
	point
	makes a <Point>
*/

define method point (x :: <integer>, y :: <integer>)
=> (pt :: <Point>);
	let pt = make ( <Point> );
	pt.point-v := y;
	pt.point-h := x;
	pt;
end method point;

/*
define method as (cls == <integer>, pt :: <Point>) => (result :: <integer>);
	//as(<extended-integer>, signed-long-at(pt));
	as(<integer>, signed-long-at(pt));
end method as;
*/


/*
	<Rect>
	A Mac Rect.
*/

define functional class <Rect> (<Ptr>) 
end class;


/*
	content-size
	The size of object a <Rect> contains
*/

define method content-size( cls == <Rect> )
=>( result :: <integer> )
	c-expr( int: "sizeof(Rect)" );
end method content-size;


/*
	Accessors for the top, left, right and bottom components of the <Rect>
*/

define method top (rect :: <Rect>) => (top :: <integer>);
	signed-short-at(rect, offset: 0);
end method top;


define method top-setter (value :: <integer>, rect :: <Rect>) => (top :: <integer>);
	signed-short-at(rect, offset: 0) := value;
end method top-setter;


define method left (rect :: <Rect>) => (left :: <integer>);
	signed-short-at(rect, offset: 2);
end method left;


define method left-setter (value :: <integer>, rect :: <Rect>) => (left :: <integer>);
	signed-short-at(rect, offset: 2) := value;
end method left-setter;


define method bottom (rect :: <Rect>) => (bottom :: <integer>);
	signed-short-at(rect, offset: 4);
end method bottom;


define method bottom-setter (value :: <integer>, rect :: <Rect>) => (bottom :: <integer>);
	signed-short-at(rect, offset: 4) := value;
end method bottom-setter;


define method right (rect :: <Rect>) => (right :: <integer>);
	signed-short-at(rect, offset: 6);
end method right;


define method right-setter (value :: <integer>, rect :: <Rect>) => (right :: <integer>);
	signed-short-at(rect, offset: 6) := value;
end method right-setter;


/*
	initialize <Rect>
*/

define method initialize( rect :: <Rect>, #key left: l = 0, top: t = 0,
						right: r = 0, bottom: b = 0 )
=> (result :: <Rect>)
  rect.top := t;
  rect.left := l;
  rect.bottom := b;
  rect.right := r;
  rect;
end method initialize;


/*
	SetRect
	Checks whether a <Point> is within a <Rect>
*/

define method SetRect(	rect :: <Rect>, l :: <integer>, t :: <integer>,
						r :: <integer>, b :: <integer> )
=> ()
	rect.top := t;
	rect.left := l;
	rect.bottom := b;
	rect.right := r;
	values();
end method SetRect;


// this one's harder to express using Toolbox interface.

/*
	PtInRect
	Checks whether a <Point> is within a <Rect>
*/

define method PtInRect(pt :: <Point>, rect :: <Rect>)
=> (result :: <Boolean>)
	 (pt.point-v >= rect.top &
	 pt.point-h >= rect.left &
	 pt.point-v <= rect.bottom &
	 pt.point-h <= rect.right);
end method;

/*
define constant PtInRect = 
begin
	call-out( "" );

	let func = get-c-function("PtInRect", args: list(<integer>, <Rect>),
											result: <boolean>, file: *InterfaceLib*);
	method (pt :: <Point>, rect :: <Rect>) => (result :: <boolean>);
		func(as(<integer>, pt), rect);
	end method;
end;
*/


/*
	<BitMap>
*/

define functional class <BitMap> (<Ptr>) 
end class;


/*
	bounds
	Gets the bounding <Rect> of a bitmap
*/

define method bounds (bitmap :: <BitMap>) 
=> (result :: <Rect>);
	let r :: <Rect> = make( <Rect> );
        
	call-out( "GetPixBounds", ptr:, ptr: bitmap.raw-value, ptr: r.raw-value );
        r;
end method bounds;


/*
	<QDGlobals>
*/

//define functional class <QDGlobals> (<statically-typed-pointer>) 
//end class;


/*
	content-size
	The size of object a <QDGlobals> contains
*/

/*define method content-size( cls == <QDGlobals> )
=>( result :: <integer> )
	c-expr( int: "sizeof(QDGlobals)" );
end method content-size;*/


//define constant qd :: <QDGlobals> = make( <QDGlobals>, pointer: c-expr( ptr: "&qd" ) );


/*
	screenBits
*/

/*define method screenBits (qdg :: <QDGlobals>)
=> (result :: <BitMap>);
	make( <BitMap>, pointer: qdg + 80);
end method;*/


/*
	<RgnHandle>
*/

define functional class <RgnHandle> (<Handle>) 
end class;


/*
	Region manipulators.
*/

define method NewRgn()
=> ( result :: <RgnHandle> )
	let result = call-out( "NewRgn", ptr: );
	make( <RgnHandle>, pointer: result );
end method NewRgn;


define method DisposeRgn( rgn :: <RgnHandle> )
=> ()
	call-out( "DisposeRgn", void:, ptr: rgn.raw-value );
	values();
end method DisposeRgn;


define method SetEmptyRgn( rgn :: <RgnHandle> )
=> ()
	call-out( "SetEmptyRgn", void: , ptr: rgn.raw-value );
	values();
end method SetEmptyRgn;

											
define method SetRectRgn( rgn :: <RgnHandle>, top :: <integer>, left :: <integer>, bottom :: <integer>, right :: <integer> )
=> ()
	call-out( "SetRectRgn", void:, ptr: rgn.raw-value, short: top, short: left, short: bottom, short: right );
	values();
end method SetRectRgn;


define method RectRgn( rgn :: <RgnHandle>, rect :: <Rect> )
=> ()
	call-out( "RectRgn", void:, ptr: rgn.raw-value, ptr: rect.raw-value );
	values();
end method RectRgn;


/*
	GrafPorts
*/

/*
	<CGrafPtr>
	A Mac GrafPtr.
*/

define functional class <CGrafPtr> (<Ptr>) 
end class;


/*
	content-size <CGrafPtr>
*/

define method content-size (value :: subclass(<CGrafPtr>))
=> (result :: <integer>);
  1;
end method content-size;


/*
	SetPort <CGrafPtr>
*/

define method SetPort( port :: <CGrafPtr> )
=> ()
	call-out( "SetPort", void:, ptr: port.raw-value );
	values										
end method SetPort;											
 
 
/*
	GetPort
*/
											
define method GetPort( port :: <CGrafPtr> )
=> ()
	call-out( "GetPort", void:, ptr: port.raw-value );
	values();
end;


/*
	Drawing.
*/

define method MoveTo( h :: <integer>, v :: <integer> )
=> ()
	call-out( "MoveTo", void:, short: h, short: v );
	values();											
end method MoveTo;											
											
											
define method LineTo( h :: <integer>, v :: <integer> )
=> ()
	call-out( "LineTo", void:, short: h, short: v );
	values();	
end method LineTo;											
											
											
define method DrawString( str :: <pascal-string> )
=> ()
	call-out( "DrawString", void:, ptr: str.raw-value );
	values();	
end method DrawString;												
											

define method PenMode( mode :: <integer> )
=> ()
	call-out( "PenMode", void:, short: mode );
	values();	
end method PenMode;											
											

define constant $patCopy = 8;
define constant $patOr = 9;
define constant $patXor = 10;


define method EraseRect( rect :: <rect> )
=> ()
	call-out("EraseRect", void:, ptr: rect.raw-value );
	values();
end method EraseRect;											
											
											
define method FrameRect( rect :: <rect> )
=> ()
	call-out("FrameRect", void:, ptr: rect.raw-value );
	values();
end method FrameRect;


define method InvertRect( rect :: <rect> )
=> ()
	call-out("InvertRect", void:, ptr: rect.raw-value );
	values();
end method InvertRect;
											
											
define method PaintRect( rect :: <rect> )
=> ()
	call-out("PaintRect", void:, ptr: rect.raw-value );
	values();
end method PaintRect;


define method EraseOval( rect :: <rect> )
=> ()
	call-out("EraseOval", void:, ptr: rect.raw-value );
	values();
end method EraseOval;


define method PaintOval( rect :: <rect> )
=> ()
	call-out("PaintOval", void:, ptr: rect.raw-value );
	values();
end method PaintOval;

define method InvertOval( rect :: <rect> )
=> ()
	call-out("InvertOval", void:, ptr: rect.raw-value );
	values();
end method InvertOval;


define method FrameOval( rect :: <rect> )
=> ()
	call-out("FrameOval", void:, ptr: rect.raw-value );
	values();
end method FrameOval;


/*
	Cursors
*/
								
define method InitCursor()
=> ()
	call-out("InitCursor", void: );
	values();
end method initCursor;
											
											
define method HideCursor()
=> ()
	call-out( "HideCursor", void: );
	values();
end method HideCursor;
	
	
define method ShowCursor()
=> ()
	call-out( "ShowCursor", void: );
	values();
end method ShowCursor;



/*
	Fonts
*/


/*
	GetFNum
*/
/*
define method GetFNum()
=> ( result :: <integer> )
begin

	let func = get-c-function("GetFNum", args: list(<Pascal-string>, <Ptr>),
								result: #(), file: *InterfaceLib*);
	method(fontName :: <Pascal-string>) => (fontNumber :: <integer>);
		//let fontNumPtr = stack-alloc(<Ptr>, 2);	// sizeof(short).
		func(fontName, $fontNumPtr);
		signed-short-at($fontNumPtr);
	end method;
end method GetFNum;
*/

/*
	TextSize
*/

define method TextSize( size :: <integer> )
=> ()
	call-out("TextSize", void:, short: size );
	values();
end method TextSize;


/*
	TextFont
*/

define method TextFont( size :: <integer> )
=> ()
	call-out("TextFont", void:, short: size );
	values();
end method TextFont;


/*
	TextMode
*/

define method TextMode( size :: <integer> )
=> ()
	call-out("TextMode", void:, short: size );
	values();
end method TextMode;

/*
	From Gareth Baker's more-toolbox
*/

define method InsetRect( rect :: <Rect>, h :: <integer>, v :: <integer> )
=> () 
	call-out("InsetRect", void:, ptr: rect.raw-value, short: h, short: v );
end method InsetRect;


/*
	RGBColor
*/

define functional class <RGBColor> ( <statically-typed-pointer> ) 
end class <RGBColor>;


/*
	content-size
	The size of object a <RGBColor> contains
*/

define method content-size( cls == <RGBColor> )
=>( result :: <integer> )
	c-expr( int: "sizeof(RGBColor)" );
end method content-size;


/*
	initialize <RGBColor>
*/

define method initialize( pt :: <RGBColor>, #rest init-args, #key red = 0, green = 0, blue = 0 )
=> ( result :: <RGBColor> )
  red( pt ) := red;
  green( pt ) := green;
  blue( pt ) := blue;
  pt;
end method initialize;

/*
	<RGBColor> accessors
*/

define method red( col :: <RGBColor> )
=> (result :: <integer>);
	unsigned-short-at(col, offset: 0);
end method;


define method red-setter (val :: <integer>, col :: <RGBColor>);
	unsigned-short-at(col, offset: 0) := val;
end method;


define method green (col :: <RGBColor>) => (result :: <integer>);
	unsigned-short-at(col, offset: 2);
end method;


define method green-setter (val :: <integer>, col :: <RGBColor>);
	unsigned-short-at(col, offset: 2) := val;
end method;


define method blue (col :: <RGBColor>) => (result :: <integer>);
	unsigned-short-at(col, offset: 4);
end method;


define method blue-setter (val :: <integer>, col :: <RGBColor>);
	unsigned-short-at(col, offset: 4) := val;
end method;


/*
	RGBForeColor
*/

define method RGBForeColor( col :: <RGBColor> )
=> ()
	call-out( "RGBForeColor", void:, ptr: col.raw-value );
	values();
end method RGBForeColor;


/*
	RGBBackColor
*/

define method RGBBackColor( col :: <RGBColor> )
=> ()
	call-out( "RGBBackColor", void:, ptr: col.raw-value );
	values();
end method RGBBackColor;


/*
	InvertColor
*/

define method InvertColor( col :: <RGBColor> )
=> ()
	call-out( "InvertColor", void:, ptr: col.raw-value );
	values();
end method InvertColor;


/*
	ClipRect
*/

define Method ClipRect( r :: <Rect> )
=> ()
	call-out( "ClipRect", void:, ptr: r.raw-value );
	values();
end method ClipRect;


/*
	ObscureCursor
*/

define method ObscureCursor()
=> ()
	call-out( "ObscureCursor", void: );
	values();
end method ObscureCursor;


/*
	SetOrigin
*/

define method SetOrigin( h :: <integer>, v :: <integer> )
=> ()
	call-out( "SetOrigin", void:, short: h, short: v );
	values();
end method SetOrigin;

/*
    Carbon GrafPort Accessors
    All Carbon ports are GrafPorts
*/

/*
    GetPortBounds
*/

define method GetPortBounds( port :: <CGrafPtr> )
=>( result :: <Rect> )
    let r = make( <Rect> );
    call-out( "GetPortBounds", ptr:, ptr: port.raw-value, ptr: r.raw-value );
    r;
end method GetPortBounds;

/*
    SetPortBounds
*/

define method SetPortBounds( port :: <CGrafPtr>, r :: <Rect> )
=>()
    call-out( "SetPortBounds", ptr:, ptr: port.raw-value, ptr: r.raw-value );
    values();
end method SetPortBounds;

/*
    GetPortVisibleRegion
*/

define method GetPortVisibleRegion( port :: <CGrafPtr>, visRgn :: <RgnHandle> )
=> ( result :: <RgnHandle> )
    call-out( "GetPortVisibleRegion", ptr:, ptr: port.raw-value, ptr: visRgn.raw-value );
    visRgn
end method GetPortVisibleRegion;

/*
    SetPortVisibleRegion
*/

define method SetPortVisibleRegion( port :: <CGrafPtr>, visRgn :: <RgnHandle> )
=> ()
    call-out( "SetPortVisibleRegion", void:, ptr: port.raw-value, ptr: visRgn.raw-value );
    values();
end method SetPortVisibleRegion;

/* in Carbon use GetPortBitMapForCopyBits or IsPortColor*/
//    RgnHandle                       visRgn;                     /* in Carbon use Get/SetPortVisibleRegion*/
//    RgnHandle                       clipRgn;                    /* in Carbon use Get/SetPortClipRegion*/
//    Point                           pnLoc;                      /* in Carbon use Get/SetPortPenLocation*/
//    Point                           pnSize;                     /* in Carbon use Get/SetPortPenSize*/
//    short                           pnMode;                     /* in Carbon use Get/SetPortPenMode*/
//    short                           pnVis;                      /* in Carbon use GetPortPenVisibility or Show/HidePen*/
//    short                           txFont;                     /* in Carbon use GetPortTextFont or TextFont*/
//    StyleField                      txFace;                     /* in Carbon use GetPortTextFace or TextFace*/
                                                                /*StyleField occupies 16-bits, but only first 8-bits are used*/
//    short                           txMode;                     /* in Carbon use GetPortTextMode or TextMode*/
//    short                           txSize;                     /* in Carbon use GetPortTextSize or TextSize*/
//    Fixed                           spExtra;                    /* in Carbon use GetPortSpExtra or SpaceExtra*/
//    Handle                          picSave;                    /* in Carbon use IsPortPictureBeingDefined*/
