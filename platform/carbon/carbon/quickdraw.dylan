module: carbon

/*
	Includes.
*/

c-include("Carbon.h");


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

define functional class <Point*> (<Ptr>)
end class;

/*
	content-size
	The size of object a <Point*> contains
*/

define method content-size( cls == <Point*> )
=>( result :: <integer> )
	c-expr( int: "sizeof(Point)" );
end method content-size;


/*
	initialize <Point*>
*/

define method initialize( pt :: <Point*>, #key v = 0, h = 0)
=> ( result :: <Point*> )

  v-value( pt ) := v;
  h-value( pt ) := h;
  
  pt;

end method initialize;


/*
	Accessors for the h and v components of a <Point*>
*/

define method v-value (pt :: <Point*>) => (v :: <integer>);
	signed-short-at(pt, offset: 0);
end method v-value;


define method v-value-setter (value :: <integer>, pt :: <Point*>) => (value :: <integer>);
	signed-short-at(pt, offset: 0) := value;
end method v-value-setter;


define method h-value (pt :: <Point*>) => (h :: <integer>);
	signed-short-at(pt, offset: 2);
end method h-value;


define method h-value-setter (value :: <integer>, pt :: <Point*>) => (value :: <integer>);
	signed-short-at(pt, offset: 2) := value;
end method h-value-setter;


/*
	point
	makes a <Point*>
*/

define method point (x :: <integer>, y :: <integer>)
=> (pt :: <Point*>);
	let pt = make ( <Point*> );
	pt.v-value := y;
	pt.h-value := x;
	pt;
end method point;

/*
define method as (cls == <integer>, pt :: <Point*>) => (result :: <integer>);
	//as(<extended-integer>, signed-long-at(pt));
	as(<integer>, signed-long-at(pt));
end method as;
*/


define method GlobalToLocal( pt :: <Point*> )
=> ()
	call-out( "GlobalToLocal", void:, ptr: pt.raw-value );
	values;
end method GlobalToLocal;


define method LocalToGlobal( pt :: <Point*> )
=> ()
	call-out( "LocalToGlobal", void:, ptr: pt.raw-value );
	values;
end method LocalToGlobal;


/*
	<Rect*>
	A Mac Rect.
*/

define functional class <Rect*> (<Ptr>) 
end class;


/*
	content-size
	The size of object a <Rect*> contains
*/

define method content-size( cls == <Rect*> )
=>( result :: <integer> )
	c-expr( int: "sizeof(Rect)" );
end method content-size;


/*
	Accessors for the top, left, right and bottom components of the <Rect*>
*/

define method top-value (rect :: <Rect*>) => (top :: <integer>);
	signed-short-at(rect, offset: 0);
end method top-value;


define method top-value-setter (value :: <integer>, rect :: <Rect*>) => (top :: <integer>);
	signed-short-at(rect, offset: 0) := value;
end method top-value-setter;


define method left-value (rect :: <Rect*>) => (left :: <integer>);
	signed-short-at(rect, offset: 2);
end method left-value;


define method left-value-setter (value :: <integer>, rect :: <Rect*>) => (left :: <integer>);
	signed-short-at(rect, offset: 2) := value;
end method left-value-setter;


define method bottom-value (rect :: <Rect*>) => (bottom :: <integer>);
	signed-short-at(rect, offset: 4);
end method bottom-value;


define method bottom-value-setter (value :: <integer>, rect :: <Rect*>) => (bottom :: <integer>);
	signed-short-at(rect, offset: 4) := value;
end method bottom-value-setter;


define method right-value (rect :: <Rect*>) => (right :: <integer>);
	signed-short-at(rect, offset: 6);
end method right-value;


define method right-value-setter (value :: <integer>, rect :: <Rect*>) => (right :: <integer>);
	signed-short-at(rect, offset: 6) := value;
end method right-value-setter;


/*
	initialize <Rect*>
*/

define method initialize( rect :: <Rect*>, #key left: l = 0, top: t = 0,
						right: r = 0, bottom: b = 0 )
=> (result :: <Rect*>)
  rect.top-value := t;
  rect.left-value := l;
  rect.bottom-value := b;
  rect.right-value := r;
  rect;
end method initialize;


/*
	SetRect
*/

define method SetRect(	rect :: <Rect*>, l :: <integer>, t :: <integer>,
						r :: <integer>, b :: <integer> )
=> ()
	rect.top-value := t;
	rect.left-value := l;
	rect.bottom-value := b;
	rect.right-value := r;
	values();
end method SetRect;


// this one's harder to express using Toolbox interface.

/*
	PtInRect
	Checks whether a <Point*> is within a <Rect*>
*/

define method PtInRect(pt :: <Point*>, rect :: <Rect*>)
=> (result :: <Boolean>)
	 (pt.v-value >= rect.top-value &
	 pt.h-value >= rect.left-value &
	 pt.v-value <= rect.bottom-value &
	 pt.h-value <= rect.right-value);
end method;

/*
define constant PtInRect = 
begin
	call-out( "" );

	let func = get-c-function("PtInRect", args: list(<integer>, <Rect*>),
											result: <boolean>, file: *InterfaceLib*);
	method (pt :: <Point*>, rect :: <Rect*>) => (result :: <boolean>);
		func(as(<integer>, pt), rect);
	end method;
end;
*/


/*
	<BitMap*> 
*/

define functional class <BitMap*> (<statically-typed-pointer>) 
end class;

define method content-size( cls == <BitMap*> )
=>( result :: <integer> )
	c-expr( int: "sizeof(BitMap)" );
end method content-size;

define method bounds-value(bitmap :: <BitMap*>)
=>(result :: <Rect*>)
  let r :: <Rect*> = make(<Rect*>);
  r.top-value := signed-short-at(bitmap, offset: 6); // 68k packing, after a Ptr and an SInt16
  r.left-value := signed-short-at(bitmap, offset: 8);
  r.bottom-value := signed-short-at(bitmap, offset: 10);
  r.right-value := signed-short-at(bitmap, offset: 12);
  r;
end method bounds-value;


/*
  <PixMapHandle>
*/

define functional class <PixMapHandle> (<Handle>) 
end class;


/*
	GetPixBounds
	Gets the bounding <Rect*> of a bitmap/pixmap
*/

define method GetPixBounds (pixmap :: <PixMapHandle>) 
=> (result :: <Rect*>);
	let r :: <Rect*> = make( <Rect*> );
	call-out( "GetPixBounds", ptr:, ptr: pixmap.raw-value, ptr: r.raw-value );
  r;
end method GetPixBounds;


/*
	screenBits
*/

define method GetQDGlobalsScreenBits()
=> (screenBits :: <BitMap*>)
  let bits :: <BitMap*> = make(<BitMap*>);
  call-out("GetQDGlobalsScreenBits", ptr:, ptr: bits.raw-value);
  bits;
end method GetQDGlobalsScreenBits;


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
											
define method GetPort()
=> ( port :: <CGrafPtr> )
  let temp :: <handle> = make(<Handle>);
	call-out( "GetPort", void:, ptr: temp.raw-value );
	pointer-at(temp, class: <CGrafPtr>, offset: 0);
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


define method EraseRect( rect :: <Rect*> )
=> ()
	call-out("EraseRect", void:, ptr: rect.raw-value );
	values();
end method EraseRect;											
											
											
define method FrameRect( rect :: <Rect*> )
=> ()
	call-out("FrameRect", void:, ptr: rect.raw-value );
	values();
end method FrameRect;


define method InvertRect( rect :: <Rect*> )
=> ()
	call-out("InvertRect", void:, ptr: rect.raw-value );
	values();
end method InvertRect;
											
											
define method PaintRect( rect :: <Rect*> )
=> ()
	call-out("PaintRect", void:, ptr: rect.raw-value );
	values();
end method PaintRect;
											
											
define method FillRect( rect :: <Rect*>, pat :: <Pattern> )
=> ()
	call-out("FillRect", void:, ptr: rect.raw-value, ptr: pat.raw-value );
	values();
end method FillRect;


define method EraseOval( rect :: <Rect*> )
=> ()
	call-out("EraseOval", void:, ptr: rect.raw-value );
	values();
end method EraseOval;


define method PaintOval( rect :: <Rect*> )
=> ()
	call-out("PaintOval", void:, ptr: rect.raw-value );
	values();
end method PaintOval;

define method InvertOval( rect :: <Rect*> )
=> ()
	call-out("InvertOval", void:, ptr: rect.raw-value );
	values();
end method InvertOval;


define method FrameOval( rect :: <Rect*> )
=> ()
	call-out("FrameOval", void:, ptr: rect.raw-value );
	values();
end method FrameOval;
											
											
define method FillOval( rect :: <Rect*>, pat :: <Pattern> )
=> ()
	call-out("FillOval", void:, ptr: rect.raw-value, ptr: pat.raw-value );
	values();
end method FillOval;


define method EraseRoundRect( rect :: <Rect*>, ovalWidth :: <integer>, ovalHeight :: <integer> )
=> ()
	call-out("EraseRoundRect", void:, ptr: rect.raw-value, int: ovalWidth, int: ovalHeight );
	values();
end method EraseRoundRect;											
											
											
define method FrameRoundRect( rect :: <Rect*>, ovalWidth :: <integer>, ovalHeight :: <integer> )
=> ()
	call-out("FrameRoundRect", void:, ptr: rect.raw-value, int: ovalWidth, int: ovalHeight );
	values();
end method FrameRoundRect;


define method InvertRoundRect( rect :: <Rect*>, ovalWidth :: <integer>, ovalHeight :: <integer> )
=> ()
	call-out("InvertRoundRect", void:, ptr: rect.raw-value, int: ovalWidth, int: ovalHeight );
	values();
end method InvertRoundRect;
											
											
define method PaintRoundRect( rect :: <Rect*>, ovalWidth :: <integer>, ovalHeight :: <integer> )
=> ()
	call-out("PaintRoundRect", void:, ptr: rect.raw-value, int: ovalWidth, int: ovalHeight );
	values();
end method PaintRoundRect;
											
											
define method FillRoundRect( rect :: <Rect*>, ovalWidth :: <integer>, ovalHeight :: <integer>, pat :: <Pattern> )
=> ()
	call-out("FillRoundRect", void:, ptr: rect.raw-value, int: ovalWidth, int: ovalHeight, ptr: pat.raw-value );
	values();
end method FillRoundRect;


define method EraseArc( rect :: <Rect*>, startAngle :: <integer>, arcAngle :: <integer> )
=> ()
	call-out("EraseArc", void:, ptr: rect.raw-value, int: startAngle, int: arcAngle );
	values();
end method EraseArc;											
											
											
define method FrameArc( rect :: <Rect*>, startAngle :: <integer>, arcAngle :: <integer> )
=> ()
	call-out("FrameArc", void:, ptr: rect.raw-value, int: startAngle, int: arcAngle );
	values();
end method FrameArc;


define method InvertArc( rect :: <Rect*>, startAngle :: <integer>, arcAngle :: <integer> )
=> ()
	call-out("InvertArc", void:, ptr: rect.raw-value, int: startAngle, int: arcAngle );
	values();
end method InvertArc;
											
											
define method PaintArc( rect :: <Rect*>, startAngle :: <integer>, arcAngle :: <integer> )
=> ()
	call-out("PaintArc", void:, ptr: rect.raw-value, int: startAngle, int: arcAngle );
	values();
end method PaintArc;
											
											
define method FillArc( rect :: <Rect*>, startAngle :: <integer>, arcAngle :: <integer>, pat :: <Pattern> )
=> ()
	call-out("FillArc", void:, ptr: rect.raw-value, int: startAngle, int: arcAngle, ptr: pat.raw-value );
	values();
end method FillArc;


/*
  GetCPixel
*/

define method GetCPixel(h :: <integer>, v :: <integer>, cPix :: <RGBColor*>)
=> ()
   call-out( "GetCPixel", void:, int: h, int: v, ptr: cPix.raw-value );
   values();
end method GetCPixel;

/*
  SetCPixel
*/

define method SetCPixel(h :: <integer>, v :: <integer>, cPix :: <RGBColor*>)
=> ()
   call-out( "SetCPixel", void:, int: h, int: v, ptr: cPix.raw-value );
   values();
end method SetCPixel;

/*
  Patterns
*/

define functional class <Pattern> (<Ptr>) 
end class;

/*
  Polys
*/

define functional class <PolyHandle> (<Handle>) 
end class;

define method OpenPoly()
=> ( port :: <PolyHandle> )
	let temp = call-out("OpenPoly", ptr:);
	make(<PolyHandle>, pointer: temp);
end method OpenPoly;

define method ClosePoly()
=> ()
	call-out("ClosePoly", void:);
end method ClosePoly;

define method KillPoly(poly :: <PolyHandle>)
=> ()
	call-out("KillPoly", void:, ptr: poly.raw-value);
end method KillPoly;

define method OffsetPoly(poly :: <PolyHandle>, dh :: <integer>, dv :: <integer>)
=> ()
	call-out("OffsetPoly", void:, ptr: poly.raw-value, int: dh, int: dv);
end method OffsetPoly;

define method FramePoly(poly :: <PolyHandle>)
=> ()
	call-out("FramePoly", void:, ptr: poly.raw-value);
end method FramePoly;

define method PaintPoly(poly :: <PolyHandle>)
=> ()
	call-out("PaintPoly", void:, ptr: poly.raw-value);
end method PaintPoly;

define method ErasePoly(poly :: <PolyHandle>)
=> ()
	call-out("ErasePoly", void:, ptr: poly.raw-value);
end method ErasePoly;

define method InvertPoly(poly :: <PolyHandle>)
=> ()
	call-out("InvertPoly", void:, ptr: poly.raw-value);
end method InvertPoly;

define method FillPoly(poly :: <PolyHandle>, pat :: <Pattern>)
=> ()
	call-out("FillPoly", void:, ptr: poly.raw-value, ptr: pat.raw-value);
end method FillPoly;

/*
  Regions
*/

define functional class <RgnHandle> (<Handle>) 
end class;

define method NewRgn()
=> ( port :: <RgnHandle> )
	let temp = call-out("NewRgn", ptr:);
	make(<RgnHandle>, pointer: temp);
end method NewRgn;

define method OpenRgn()
=> ()
	call-out("OpenRgn", void:);
end method OpenRgn;

define method CloseRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("CloseRgn", void:, ptr: Rgn.raw-value);
end method CloseRgn;

define method DisposeRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("DisposeRgn", void:, ptr: Rgn.raw-value);
end method DisposeRgn;

define method SetEmptyRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("SetEmptyRgn", void:, ptr: Rgn.raw-value);
end method SetEmptyRgn;

define method EmptyRgn(Rgn :: <RgnHandle>)
=> (result :: <boolean>)
	if(call-out("EmptyRgn", int:, ptr: Rgn.raw-value))
    #t;
  else
    #f;
  end if;
end method EmptyRgn;

define method SetRectRgn( rgn :: <RgnHandle>, top :: <integer>, left :: <integer>, bottom :: <integer>, right :: <integer> )
=> ()
	call-out( "SetRectRgn", void:, ptr: rgn.raw-value, short: top, short: left, short: bottom, short: right );
	values();
end method SetRectRgn;

define method RectRgn( rgn :: <RgnHandle>, rect :: <Rect*> )
=> ()
	call-out( "RectRgn", void:, ptr: rgn.raw-value, ptr: rect.raw-value );
	values();
end method RectRgn;

define method FrameRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("FrameRgn", void:, ptr: Rgn.raw-value);
end method FrameRgn;

define method PaintRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("PaintRgn", void:, ptr: Rgn.raw-value);
end method PaintRgn;

define method EraseRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("EraseRgn", void:, ptr: Rgn.raw-value);
end method EraseRgn;

define method InvertRgn(Rgn :: <RgnHandle>)
=> ()
	call-out("InvertRgn", void:, ptr: Rgn.raw-value);
end method InvertRgn;

define method FillRgn(Rgn :: <RgnHandle>, pat :: <Pattern>)
=> ()
	call-out("FillRgn", void:, ptr: Rgn.raw-value, ptr: pat.raw-value);
end method FillRgn;

define method OffsetRgn(rgn :: <RgnHandle>, dh :: <integer>, dv :: <integer>)
=> ()
	call-out("OffsetRgn", void:, ptr: rgn.raw-value, int: dh, int: dv);
end method OffsetRgn;

define method InsetRgn(rgn :: <RgnHandle>, dh :: <integer>, dv :: <integer>)
=> ()
	call-out("InsetRgn", void:, ptr: rgn.raw-value, int: dh, int: dv);
end method InsetRgn;

define method SectRgn(a :: <RgnHandle>, b :: <RgnHandle>, dest :: <RgnHandle>)
=> ()
	call-out("SectRgn", void:, ptr: a.raw-value, ptr: b.raw-value, ptr: dest.raw-value);
end method SectRgn;

define method UnionRgn(a :: <RgnHandle>, b :: <RgnHandle>, dest :: <RgnHandle>)
=> ()
	call-out("UnionRgn", void:, ptr: a.raw-value, ptr: b.raw-value, ptr: dest.raw-value);
end method UnionRgn;

define method DiffRgn(a :: <RgnHandle>, b :: <RgnHandle>, dest :: <RgnHandle>)
=> ()
	call-out("DiffRgn", void:, ptr: a.raw-value, ptr: b.raw-value, ptr: dest.raw-value);
end method DiffRgn;

define method XorRgn(a :: <RgnHandle>, b :: <RgnHandle>, dest :: <RgnHandle>)
=> ()
	call-out("XorRgn", void:, ptr: a.raw-value, ptr: b.raw-value, ptr: dest.raw-value);
end method XorRgn;

define method CopyRgn(a :: <RgnHandle>, b :: <RgnHandle>)
=> ()
	call-out("CopyRgn", void:, ptr: a.raw-value, ptr: b.raw-value);
end method CopyRgn;

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


define method GetCursor( resource-number :: <integer> )
=> ( result :: <Handle> )
	let result = call-out( "GetCursor", ptr:, int: resource-number );
	make( <Handle>, pointer: result ); 
end method GetCursor;


define method SetCursor( cursor-handle :: <Handle> )
=> ()
	call-out( "SetCursor", void:, ptr: cursor-handle.raw-value );
end method SetCursor;


/*
	From Gareth Baker's more-toolbox
*/

define method InsetRect( rect :: <Rect*>, h :: <integer>, v :: <integer> )
=> () 
	call-out("InsetRect", void:, ptr: rect.raw-value, short: h, short: v );
end method InsetRect;


/*
	RGBColor
*/

define functional class <RGBColor*> ( <statically-typed-pointer> ) 
end class <RGBColor*>;


/*
	content-size
	The size of object a <RGBColor*> contains
*/

define method content-size( cls == <RGBColor*> )
=>( result :: <integer> )
	c-expr( int: "sizeof(RGBColor)" );
end method content-size;


/*
	initialize <RGBColor*>
*/

define method initialize( pt :: <RGBColor*>, #rest init-args, #key red = 0, green = 0, blue = 0 )
=> ( result :: <RGBColor*> )
  pt.red-value := red;
  pt.green-value := green;
  pt.blue-value := blue;
  pt;
end method initialize;

/*
	<RGBColor*> accessors
*/

define method red-value( col :: <RGBColor*> )
=> (result :: <integer>);
	unsigned-short-at(col, offset: 0);
end method;


define method red-value-setter (val :: <integer>, col :: <RGBColor*>);
	unsigned-short-at(col, offset: 0) := val;
end method;


define method green-value (col :: <RGBColor*>) => (result :: <integer>);
	unsigned-short-at(col, offset: 2);
end method;


define method green-value-setter (val :: <integer>, col :: <RGBColor*>);
	unsigned-short-at(col, offset: 2) := val;
end method;


define method blue-value (col :: <RGBColor*>) => (result :: <integer>);
	unsigned-short-at(col, offset: 4);
end method;


define method blue-value-setter (val :: <integer>, col :: <RGBColor*>);
	unsigned-short-at(col, offset: 4) := val;
end method;


/*
	RGBForeColor
*/

define method RGBForeColor( col :: <RGBColor*> )
=> ()
	call-out( "RGBForeColor", void:, ptr: col.raw-value );
	values();
end method RGBForeColor;


/*
	RGBBackColor
*/

define method RGBBackColor( col :: <RGBColor*> )
=> ()
	call-out( "RGBBackColor", void:, ptr: col.raw-value );
	values();
end method RGBBackColor;


/*
	InvertColor
*/

define method InvertColor( col :: <RGBColor*> )
=> ()
	call-out( "InvertColor", void:, ptr: col.raw-value );
	values();
end method InvertColor;


/*
	ClipRect
*/

define Method ClipRect( r :: <Rect*> )
=> ()
	call-out( "ClipRect", void:, ptr: r.raw-value );
	values();
end method ClipRect;

/*
	SetClip
*/

define Method SetClip( r :: <RgnHandle> )
=> ()
	call-out( "SetClip", void:, ptr: r.raw-value );
	values();
end method SetClip;

/*
	GetClip
  // Yes, this does just use a handle rather than a pointer to a handle
*/

define Method GetClip( r :: <RgnHandle> )
=> ()
	call-out( "GetClip", void:, ptr: r.raw-value );
	values();
end method GetClip;


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
	QDError
*/

define method QDError()
=> ( result :: <OSErr> )
	as( <OSErr>, call-out( "QDError", int: ) );
end method QDError;


/*
    Carbon GrafPort Accessors
    All Carbon ports are GrafPorts
*/

/*
    GetPortBounds
*/

define method GetPortBounds( port :: <CGrafPtr> )
=>( result :: <Rect*> )
    let r = make( <Rect*> );
    call-out( "GetPortBounds", ptr:, ptr: port.raw-value, ptr: r.raw-value );
    r;
end method GetPortBounds;

/*
    SetPortBounds
*/

define method SetPortBounds( port :: <CGrafPtr>, r :: <Rect*> )
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

define method GetPortTextFont(port :: <CGrafPtr>)
=> (result :: <integer>)
  call-out("GetPortTextFont", int:, ptr: port.raw-value);
end method GetPortTextFont;

define method GetPortTextFace(port :: <CGrafPtr>)
=> (result :: <integer>)
  call-out("GetPortTextFace", int:, ptr: port.raw-value);
end method GetPortTextFace;

define method GetPortTextMode(port :: <CGrafPtr>)
=> (result :: <integer>)
  call-out("GetPortTextMode", int:, ptr: port.raw-value);
end method GetPortTextMode;

define method GetPortTextSize(port :: <CGrafPtr>)
=> (result :: <integer>)
  call-out("GetPortTextSize", int:, ptr: port.raw-value);
end method GetPortTextSize;

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


/*
    MacOS X Buffering Routines
*/

/*
    QDIsPortBuffered
*/

define method QDIsPortBuffered( port :: <CGrafPtr> )
=> ( result :: <boolean> )
   let result = call-out( "QDIsPortBuffered", int:, ptr: port.raw-value );
    if( result = 0 )
        #f;
    else
        #t;
    end if;
end method QDIsPortBuffered;

/*
    QDIsPortBufferDirty
*/

define method QDIsPortBufferDirty( port :: <CGrafPtr> )
=> ( result :: <boolean> )
   let result = call-out( "QDIsPortBufferDirty", int:, ptr: port.raw-value );
    if( result = 0 )
        #f;
    else
        #t;
    end if;
end method QDIsPortBufferDirty;

/*
    QDFlushPortBuffer
*/

define method QDFlushPortBuffer( port :: <CGrafPtr>, region :: <RgnHandle> )
=> ()
   call-out( "QDFlushPortBuffer", void:, ptr: port.raw-value, ptr: region.raw-value );
   values();
end method QDFlushPortBuffer;

/*
    QDGetDirtyRegion
*/

define method QDGetDirtyRegion( port :: <CGrafPtr>, region :: <RgnHandle> )
=> ( result :: <OSStatus> )
   let status :: <integer> = call-out( "QDGetDirtyRegion", void:, ptr: port.raw-value, ptr: region.raw-value );
   as( <OSStatus>, status );
end method QDGetDirtyRegion;

/*
    QDSetDirtyRegion
*/

define method QDSetDirtyRegion( port :: <CGrafPtr>, region :: <RgnHandle> )
=> ( result :: <OSStatus> )
   let status :: <integer> = call-out( "QDSetDirtyRegion", void:, ptr: port.raw-value, ptr: region.raw-value );
   as( <OSStatus>, status );
end method QDSetDirtyRegion;

