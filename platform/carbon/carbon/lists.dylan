module: carbon

c-include( "Carbon.h" );

// types

// <ListRef>

define functional class <ListRef>
  (<Handle>)
end class <ListRef>;

define constant <Cell> = <Point*>;

define constant <ListBounds> = <Rect*>;

// ListRec.listFlags bits

define constant	$lDoVAutoscrollBit :: <integer> = 1;
define constant	$lDoHAutoscrollBit :: <integer>           = 0;

// ListRec.listFlags masks

define constant	$lDoVAutoscroll :: <integer>              = 2;
define constant	$lDoHAutoscroll :: <integer>              = 1;

// ListRec.selFlags bits

define constant	$lOnlyOneBit :: <integer>                 = 7;
define constant	$lExtendDragBit :: <integer>              = 6;
define constant	$lNoDisjointBit :: <integer>              = 5;
define constant	$lNoExtendBit :: <integer>                = 4;
define constant	$lNoRectBit :: <integer>                  = 3;
define constant	$lUseSenseBit :: <integer>                = 2;
define constant	$lNoNilHiliteBit :: <integer>             = 1;

// ListRec.selFlags masks

define constant	$lOnlyOne :: <integer>                    = -128;
define constant	$lExtendDrag :: <integer>                 = 64;
define constant	$lNoDisjoint :: <integer>                 = 32;
define constant	$lNoExtend :: <integer>                   = 16;
define constant	$lNoRect :: <integer>                     = 8;
define constant	$lUseSense :: <integer>                   = 4;
define constant	$lNoNilHilite :: <integer>                = 2;

define constant	$kListDefUserProcType :: <integer>        = 0;
define constant	$kListDefStandardTextType :: <integer>    = 1;
define constant	$kListDefStandardIconType :: <integer>    = 2;

define method LNew
    (rView :: <Rect*>, dataBounds :: <Point*>, cSize :: <Point*>,
     theProc :: <integer>, theWindow :: <WindowRef>, drawIt :: <boolean>,
     hasGrow :: <boolean>, scrollHoriz :: <boolean>, scrollVert :: <boolean>)
 => ( result :: <ListRef> )
  make(<ListRef>, ptr: call-out("lnew", ptr:,
    ptr: rView, ptr: dataBounds, ptr: cSize, int: theProc, ptr: theWindow, int: if(drawIt) 1 else 0 end,
    int: if(hasGrow) 1 else 0 end, int: if(scrollHoriz) 1 else 0 end, int: if(scrollVert) 1 else 0 end));
end method LNew;

define method LDispose                        
    (lHandle :: <ListRef>)                           
 => ()
  call-out("ldispose", void:, ptr: lHandle.raw-value)
end method LDispose;

define method LSetDrawingMode                        
    (lHandle :: <ListRef>, drawIt :: <boolean>)                           
 => ()
  call-out("lsetdrawingmode", void:, ptr: lHandle.raw-value, int: if(drawIt) 1 else 0 end)
end method LSetDrawingMode;

define method LUpdate                        
    (theRgn :: <RgnHandle>, lHandle :: <ListRef>)                           
 => ()
  call-out("LUpdate", void:, ptr: theRgn, ptr: lHandle.raw-value)
end method LUpdate;

define method LActivate                        
    (act :: <boolean>, lHandle :: <ListRef>)                           
 => ()
  call-out("LActivate", void:, int: if(act) 1 else 0 end, ptr: lHandle.raw-value)
end method LActivate;

define method LClick                        
    (pt :: <Point*>, modifiers :: <integer>, lHandle :: <ListRef>)                           
 => (result :: <boolean>)
  if(call-out("lclick", int:, ptr: pt, int: modifiers, ptr: lHandle.raw-value))
    #t;
  else
    #f;
  end if;
end method LClick;

define method LAddRow                        
    (count :: <integer>, rowNum :: <integer>, lHandle :: <ListRef>)                           
 => (result :: <integer>)
  call-out("LAddRow", int:, int: count, int: rowNum, ptr: lHandle.raw-value);
end method LAddRow;

define method LDelRow                        
    (count :: <integer>, rowNum :: <integer>, lHandle :: <ListRef>)                           
 => (result :: <integer>)
  call-out("LDelRow", int:, int: count, int: rowNum, ptr: lHandle.raw-value);
end method LDelRow;

define method LGetSelect
    (next :: <boolean>, theCell :: <Point*>, lHandle :: <ListRef>)
 => ()
  call-out("lgetselect", void:, int: if(next) 1 else 0 end, ptr: theCell.raw-value, ptr: lHandle.raw-value);
end method LGetSelect;    

define method LSize                       
    (listWidth :: <integer>, listHeight :: <integer>, lHandle :: <ListRef>)                           
 => ()
  call-out("LSize", void:, int: listWidth, int: listHeight, ptr: lHandle.raw-value);
end method LSize;                 

define method LScroll                      
    (cols :: <integer>, rows :: <integer>, lHandle :: <ListRef>)                           
 => ()
  call-out("LSize", void:, int: cols, int: rows, ptr: lHandle.raw-value);
end method LScroll;

define method LGetCell
    (dataPtr, dataLen :: <integer>, theCell :: <Point*>, lHandle :: <ListRef>)
 => (dataLen :: <integer>)
  let temp :: <Handle> = make(<Handle>);
  signed-long-at(temp, offset: 0) := dataLen;
  call-out("lgetcell", void:, ptr: dataPtr.raw-value, ptr: theCell.raw-value, ptr: lHandle.raw-value);
  signed-long-at(temp, offset: 0);
end method LGetCell;    

define method LSetCell
    (dataPtr, dataLen :: <integer>, theCell :: <Point*>, lHandle :: <ListRef>)
 => ()
  call-out("lSetcell", void:, ptr: dataPtr.raw-value, int: dataLen, ptr: theCell.raw-value, ptr: lHandle.raw-value);
end method LSetCell;    

define method LSetSelect
    (next :: <boolean>, theCell :: <Point*>, lHandle :: <ListRef>)
 => ()
  call-out("lsetselect", void:, int: if(next) 1 else 0 end, ptr: theCell.raw-value, ptr: lHandle.raw-value);
end method LSetSelect;                                     
                                 
define method SetListFlags                      
    (lHandle :: <ListRef>, flags :: <integer>)                           
 => ()
  call-out("SetListFlags", void:, ptr: lHandle.raw-value, int: flags);
end method SetListFlags;
                                 
define method SetListSelectionFlags                      
    (lHandle :: <ListRef>, flags :: <integer>)                           
 => ()
  call-out("SetListSelectionFlags", void:, ptr: lHandle.raw-value, int: flags);
end method SetListSelectionFlags;

define method GetListDataBounds
    (lHandle :: <ListRef>, bounds :: <Rect*>)
 => (bounds :: <Rect*>)
  call-out("GetListDataBounds", ptr:, ptr: lHandle.raw-value, ptr: bounds.raw-value);
  bounds;
end method GetListDataBounds;       
