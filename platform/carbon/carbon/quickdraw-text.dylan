module: carbon

/*
	Includes.
*/

c-include("Carbon.h");

// These should be in TextEdit

define constant $teFlushDefault  :: <integer> = c-expr(int: "teFlushDefault ");
define constant $teCenter :: <integer> = c-expr(int: "teCenter");
define constant $teFlushRight :: <integer> = c-expr(int: "teFlushRight");
define constant $teFlushLeft :: <integer> = c-expr(int: "teFlushLeft");

/*
  FontInfo
*/

define functional class <FontInfo*> (<statically-typed-pointer>)
end class <FontInfo*>;

define method content-size(type == <FontInfo*>)
=> (result :: <integer>)
  c-expr(int:, "sizeof(FontInfo)");
end method content-size;

define method ascent-value(info :: <FontInfo*>)
=>(result :: <integer>)
  signed-short-at(info, offset: 0);
end method ascent-value;

define method descent-value(info :: <FontInfo*>)
=>(result :: <integer>)
  signed-short-at(info, offset: 2);
end method descent-value;

define method widMax-value(info :: <FontInfo*>)
=>(result :: <integer>)
  signed-short-at(info, offset: 4);
end method widMax-value;

define method leading-value(info :: <FontInfo*>)
=>(result :: <integer>)
  signed-short-at(info, offset: 4);
end method leading-value;

define method ascent-value-setter(info :: <FontInfo*>, new-value :: <integer>)
=>(result :: <integer>)
  signed-short-at(info, offset: 0) := new-value;
end method ascent-value-setter;

define method descent-value-setter(info :: <FontInfo*>, new-value :: <integer>)
=>(result :: <integer>)
  signed-short-at(info, offset: 2) := new-value;
end method descent-value-setter;

define method widMax-value-setter(info :: <FontInfo*>, new-value :: <integer>)
=>(result :: <integer>)
  signed-short-at(info, offset: 4) := new-value;
end method widMax-value-setter;

define method leading-value-setter(info :: <FontInfo*>, new-value :: <integer>)
=>(result :: <integer>)
  signed-short-at(info, offset: 4) := new-value;
end method leading-value-setter;

/*
	Port Text Property setters (see quickdraw.dylan for the getters).
*/

define method TextFont(font :: <integer>)
=> ()
  call-out("TextFont", void:, int: font);
end method TextFont;  

define method TextFace(face :: <integer>)
=> ()
  call-out("TextFace", void:, int: face);
end method TextFace;  

define method TextMode(mode :: <integer>)
=> ()
  call-out("TextMode", void:, int: mode);
end method TextMode; 

define method TextSize(point-size :: <integer>)
=> ()
  call-out("TextSize", void:, int: point-size);
end method TextSize;  

/*
	Drawing and measuring text.
*/

/*define method GetFNum()
=> ( result :: <integer> )
  let temp :: <handle> = 
	let func = get-c-function("GetFNum", args: list(<Pascal-string>, <Ptr>),
								result: #(), file: *InterfaceLib*);
	method(fontName :: <Pascal-string>) => (fontNumber :: <integer>);
		//let fontNumPtr = stack-alloc(<Ptr>, 2);	// sizeof(short).
		func(fontName, $fontNumPtr);
		signed-short-at($fontNumPtr);
	end method;
end method GetFNum;*/

define method DrawChar(char :: <character>)
=> ()
  call-out("DrawChar", void:, unsigned-char: char);
end method DrawChar;  

define method DrawText(string :: <C-string>, first-byte :: <integer>, byte-count :: <integer>)
=>()
  call-out("DrawText", void:, ptr: string.raw-value, short: first-byte, short: byte-count);
end method DrawText;

define method CharWidth(char :: <character>)
=>(result :: <integer>)
  call-out("CharWidth", short:, unsigned-char: char);
end method CharWidth;

define method TextWidth(string :: <C-string>, first-byte :: <integer>, byte-count :: <integer>)
=>(result :: <integer>)
  call-out("TextWidth", short:, ptr: string.raw-value, short: first-byte, short: byte-count);
end method TextWidth;

define method GetFontInfo(info :: <FontInfo*>)
=> ()
  call-out("GetFontInfo", void:, ptr: info.raw-value);
end method GetFontInfo;  