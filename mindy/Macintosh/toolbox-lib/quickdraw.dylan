module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: see below

define constant $port-ptr = NewPtr( 4 );
define constant $fontNumPtr = NewPtr( 2 );

// QuickDraw Thread Safety from Patrick's Simple.dylan

//define constant $qd-lock = make(<multilock>);
//
//define method acquire-quickdraw (port :: <GrafPtr>)
//    grab-lock($qd-lock);
//    SetPort(port);
//end method acquire-quickdraw;
//
//define method release-quickdraw ()
//    release-lock($qd-lock);
//end method release-quickdraw;

// Points.

define class <Point> (<Ptr>) end class;

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

define method point (x :: <integer>, y :: <integer>) => (pt :: <Point>);
	let pt = as (<Point>, NewPtr(4));
	pt.point-v := y;
	pt.point-h := x;
	pt;
end method point;

define method as (cls == <integer>, pt :: <Point>) => (result :: <integer>);
	as(<extended-integer>, signed-long-at(pt));
//	as(<integer>, signed-long-at(pt));
end method as;

define method make(cls == <Point>, #key v = 0, h = 0)
=> (result :: <Point>)
  let pt = as(<Point>, NewPtr(4));
  point-v(pt) := v;
  point-h(pt) := h;
  pt;
end method make;


// Rectangles.

define class <Rect> (<Ptr>) end class;

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

define method make(cls == <Rect>, #key left: l = 0, top: t = 0,
						right: r = 0, bottom: b = 0 )
=> (result :: <Rect>)
  let rect = as(<Rect>, NewPtr(8));
  rect.top := t;
  rect.left := l;
  rect.bottom := b;
  rect.right := r;
  rect;
end method make;

// this one's harder to express using Toolbox interface.

/*
define constant PtInRect = method (pt :: <Point>, rect :: <Rect>) => (result :: <Boolean>);
	(pt.point-v >= rect.top &
	 pt.point-h >= rect.left &
	 pt.point-v <= rect.bottom &
	 pt.point-h <= rect.right);
end method;
*/

define constant PtInRect = 
begin
	let func = get-c-function("PtInRect", args: list(<integer>, <Rect>),
											result: <boolean>, file: *InterfaceLib*);
	method (pt :: <Point>, rect :: <Rect>) => (result :: <boolean>);
		func(as(<integer>, pt), rect);
	end method;
end;

// QuickDraw.

define class <BitMap> (<statically-typed-pointer>) end class;

define method bounds (bitmap :: <BitMap>) => (result :: <Rect>);
	as(<Rect>, bitmap + 6);
end method;

define class <QDGlobals> (<statically-typed-pointer>) end class;

define method screenBits (qdg :: <QDGlobals>) => (result :: <BitMap>);
	as(<BitMap>, qdg + 80);
end method;

define constant qd = find-c-pointer( "qd" );	//as(<QDGlobals>, find-c-pointer("qd"));

define class <RgnHandle> (<Handle>) end class;

define constant NewRgn = get-c-function("NewRgn", args: #(),
											result: <RgnHandle>, file: *InterfaceLib*);
define constant DisposeRgn = get-c-function("DisposeRgn", args: list(<RgnHandle>),
											result: #(), file: *InterfaceLib*);
define constant SetEmptyRgn = get-c-function("SetEmptyRgn", args: list(<RgnHandle>),
											result: #(), file: *InterfaceLib*);
define constant SetRectRgn = get-c-function("SetRectRgn", args: list(<RgnHandle>, <integer>, <integer>, <integer>),
											result: #(), file: *InterfaceLib*);
define constant RectRgn = get-c-function("RectRgn", args: list(<RgnHandle>, <Rect>),
											result: #(), file: *InterfaceLib*);

define class <GrafPtr> (<Ptr>) end class;

define method portRect (port :: <GrafPtr>)
	as(<Rect>, port + 16);
end method;

define constant SetPort = get-c-function("SetPort", args: list(<GrafPtr>),
											result: #(), file: *InterfaceLib*);
define constant GetPort =
begin
	let func = get-c-function("GetPort", args: list(<Ptr>),
								result: #(), file: *InterfaceLib*);
	method() => (port :: <GrafPtr>);
		//let port-ptr = stack-alloc(<Ptr>, 4);
		func($port-ptr);
		pointer-at($port-ptr, class: <GrafPtr>);
		$port-ptr;
	end method;
end;

define constant MoveTo = get-c-function("MoveTo", args: list(<integer>, <integer>),
											result: #(), file: *InterfaceLib*);
define constant LineTo = get-c-function("LineTo", args: list(<integer>, <integer>),
											result: #(), file: *InterfaceLib*);
define constant DrawString = get-c-function("DrawString", args: list(<string>),
											result: #(), file: *InterfaceLib*);
define constant TextFont = get-c-function("TextFont", args: list(<integer>),
											result: #(), file: *InterfaceLib*);

define constant PenMode = get-c-function("PenMode", args: list(<integer>),
											result: #(), file: *InterfaceLib*);

define constant $patCopy = 8;
define constant $patOr = 9;
define constant $patXor = 10;

define constant EraseRect = get-c-function("EraseRect", args: list(<Rect>),
											result: #(), file: *InterfaceLib*);
define constant FrameRect = get-c-function("FrameRect", args: list(<Rect>),
											result: #(), file: *InterfaceLib*);
define constant InvertRect = get-c-function("InvertRect", args: list(<Rect>),
											result: #(), file: *InterfaceLib*);
define constant PaintRect = get-c-function("PaintRect", args: list(<Rect>),
											result: #(), file: *InterfaceLib*);

// Cursors.
								
define constant InitCursor = get-c-function("InitCursor", args: #(),
											result: #(), file: *InterfaceLib*);
define constant HideCursor = get-c-function("HideCursor", args: #(),
											result: #(), file: *InterfaceLib*);
define constant ShowCursor = get-c-function("ShowCursor", args: #(),
											result: #(), file: *InterfaceLib*);

// Fonts.

define constant GetFNum =
begin
	let func = get-c-function("GetFNum", args: list(<Pascal-string>, <Ptr>),
								result: #(), file: *InterfaceLib*);
	method(fontName :: <Pascal-string>) => (fontNumber :: <integer>);
		//let fontNumPtr = stack-alloc(<Ptr>, 2);	// sizeof(short).
		func(fontName, $fontNumPtr);
		signed-short-at($fontNumPtr);
	end method;
end;
