module: lists

typedef Point                           Cell;
typedef Rect                            ListBounds;
typedef char                            DataArray[32001];
typedef char *                          DataPtr;
typedef DataPtr *                       DataHandle;

    Rect                            rView;                      /* in Carbon use Get/SetListViewBounds*/
    GrafPtr                         port;                       /* in Carbon use Get/SetListPort*/
    Point                           indent;                     /* in Carbon use Get/SetListCellIndent*/
    Point                           cellSize;                   /* in Carbon use Get/SetListCellSize*/
    ListBounds                      visible;                    /* in Carbon use GetListVisibleCells*/
    ControlRef                      vScroll;                    /* in Carbon use GetListVerticalScrollBar*/
    ControlRef                      hScroll;                    /* in Carbon use GetListHorizontalScrollBar*/
    SInt8                           selFlags;                   /* in Carbon use Get/SetListSelectionFlags*/
    Boolean                         lActive;                    /* in Carbon use LActivate, GetListActive*/
    SInt8                           lReserved;                  /* not supported in Carbon */
    SInt8                           listFlags;                  /* in Carbon use Get/SetListFlags */
    long                            clikTime;                   /* in Carbon use Get/SetListClickTime*/
    Point                           clikLoc;                    /* in Carbon use GetListClickLocation*/
    Point                           mouseLoc;                   /* in Carbon use GetListMouseLocation*/
    ListClickLoopUPP                lClickLoop;                 /* in Carbon use Get/SetListClickLoop*/
    Cell                            lastClick;                  /* in Carbon use SetListLastClick*/
    long                            refCon;                     /* in Carbon use Get/SetListRefCon*/
    Handle                          listDefProc;                /* not supported in Carbon */
    Handle                          userHandle;                 /* in Carbon use Get/SetListUserHandle*/
    ListBounds                      dataBounds;                 /* in Carbon use GetListDataBounds*/
    DataHandle                      cells;                      /* in Carbon use LGet/SetCell*/
    short                           maxIndex;                   /* in Carbon use LGet/SetCell*/
    short                           cellArray[1];               /* in Carbon use LGet/SetCell*/
};
typedef struct ListRec                  ListRec;

typedef ListRec *                       ListPtr;
typedef ListPtr *                       ListHandle;
/* ListRef is obsolete.  Use ListHandle. */
typedef ListHandle                      ListRef;


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



// LDEF messages

define constant	$lInitMsg :: <integer>                    = 0;
define constant	$lDrawMsg :: <integer>                    = 1;
define constant	$lHiliteMsg :: <integer>                  = 2;
define constant	$lCloseMsg :: <integer>                   = 3;


define constant	$kListDefUserProcType :: <integer>        = 0;
define constant	$kListDefStandardTextType :: <integer>    = 1;
define constant	$kListDefStandardIconType :: <integer>    = 2;

typedef UInt32                          ListDefType;

struct ListDefSpec {
    ListDefType                     defType;
    union {
        ListDefUPP                      userProc;
    }                                 u;
};
typedef struct ListDefSpec              ListDefSpec;
EXTERN_API( OSStatus )
CreateCustomList                (const Rect *           rView,
                                 const ListBounds *     dataBounds,
                                 Point                  cellSize,
                                 const ListDefSpec *    theSpec,
                                 WindowRef              theWindow,
                                 Boolean                drawIt,
                                 Boolean                hasGrow,
                                 Boolean                scrollHoriz,
                                 Boolean                scrollVert,
                                 ListHandle *           outList);



EXTERN_API( ListHandle )
LNew                            (const Rect *           rView,
                                 const ListBounds *     dataBounds,
                                 Point                  cSize,
                                 short                  theProc,
                                 WindowRef              theWindow,
                                 Boolean                drawIt,
                                 Boolean                hasGrow,
                                 Boolean                scrollHoriz,
                                 Boolean                scrollVert)                         THREEWORDINLINE(0x3F3C, 0x0044, 0xA9E7);

EXTERN_API( void )
LDispose                        (ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0028, 0xA9E7);

EXTERN_API( short )
LAddColumn                      (short                  count,
                                 short                  colNum,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0004, 0xA9E7);

EXTERN_API( short )
LAddRow                         (short                  count,
                                 short                  rowNum,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0008, 0xA9E7);

EXTERN_API( void )
LDelColumn                      (short                  count,
                                 short                  colNum,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0020, 0xA9E7);

EXTERN_API( void )
LDelRow                         (short                  count,
                                 short                  rowNum,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0024, 0xA9E7);

EXTERN_API( Boolean )
LGetSelect                      (Boolean                next,
                                 Cell *                 theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x003C, 0xA9E7);

EXTERN_API( Cell )
LLastClick                      (ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0040, 0xA9E7);

EXTERN_API( Boolean )
LNextCell                       (Boolean                hNext,
                                 Boolean                vNext,
                                 Cell *                 theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0048, 0xA9E7);

EXTERN_API( Boolean )
LSearch                         (const void *           dataPtr,
                                 short                  dataLen,
                                 ListSearchUPP          searchProc,
                                 Cell *                 theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0054, 0xA9E7);

EXTERN_API( void )
LSize                           (short                  listWidth,
                                 short                  listHeight,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0060, 0xA9E7);

EXTERN_API( void )
LSetDrawingMode                 (Boolean                drawIt,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x002C, 0xA9E7);

EXTERN_API( void )
LScroll                         (short                  dCols,
                                 short                  dRows,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0050, 0xA9E7);

EXTERN_API( void )
LAutoScroll                     (ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0010, 0xA9E7);

EXTERN_API( void )
LUpdate                         (RgnHandle              theRgn,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0064, 0xA9E7);

EXTERN_API( void )
LActivate                       (Boolean                act,
                                 ListHandle             lHandle)                            TWOWORDINLINE(0x4267, 0xA9E7);

EXTERN_API( void )
LCellSize                       (Point                  cSize,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0014, 0xA9E7);

EXTERN_API( Boolean )
LClick                          (Point                  pt,
                                 short                  modifiers,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0018, 0xA9E7);

EXTERN_API( void )
LAddToCell                      (const void *           dataPtr,
                                 short                  dataLen,
                                 Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x000C, 0xA9E7);

EXTERN_API( void )
LClrCell                        (Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x001C, 0xA9E7);

EXTERN_API( void )
LGetCell                        (void *                 dataPtr,
                                 short *                dataLen,
                                 Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0038, 0xA9E7);

EXTERN_API( void )
LRect                           (Rect *                 cellRect,
                                 Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x004C, 0xA9E7);

EXTERN_API( void )
LSetCell                        (const void *           dataPtr,
                                 short                  dataLen,
                                 Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0058, 0xA9E7);

EXTERN_API( void )
LSetSelect                      (Boolean                setIt,
                                 Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x005C, 0xA9E7);

EXTERN_API( void )
LDraw                           (Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0030, 0xA9E7);

EXTERN_API( void )
LGetCellDataLocation            (short *                offset,
                                 short *                len,
                                 Cell                   theCell,
                                 ListHandle             lHandle)                            THREEWORDINLINE(0x3F3C, 0x0034, 0xA9E7);


EXTERN_API( Rect *)
GetListViewBounds               (ListRef                list,
                                 Rect *                 view);

EXTERN_API( CGrafPtr )
GetListPort                     (ListRef                list);

EXTERN_API( Point *)
GetListCellIndent               (ListRef                list,
                                 Point *                indent);

EXTERN_API( Point *)
GetListCellSize                 (ListRef                list,
                                 Point *                size);

EXTERN_API( ListBounds *)
GetListVisibleCells             (ListRef                list,
                                 ListBounds *           visible);

EXTERN_API( ControlRef )
GetListVerticalScrollBar        (ListRef                list);

EXTERN_API( ControlRef )
GetListHorizontalScrollBar      (ListRef                list);

EXTERN_API( Boolean )
GetListActive                   (ListRef                list);

EXTERN_API( SInt32 )
GetListClickTime                (ListRef                list);

EXTERN_API( Point *)
GetListClickLocation            (ListRef                list,
                                 Point *                click);

EXTERN_API( Point *)
GetListMouseLocation            (ListRef                list,
                                 Point *                mouse);

EXTERN_API( ListClickLoopUPP )
GetListClickLoop                (ListRef                list);

EXTERN_API( SInt32 )
GetListRefCon                   (ListRef                list);

EXTERN_API( Handle )
GetListDefinition               (ListRef                list);

EXTERN_API( Handle )
GetListUserHandle               (ListRef                list);

EXTERN_API( ListBounds *)
GetListDataBounds               (ListRef                list,
                                 ListBounds *           bounds);

EXTERN_API( DataHandle )
GetListDataHandle               (ListRef                list);

EXTERN_API( OptionBits )
GetListFlags                    (ListRef                list);

EXTERN_API( OptionBits )
GetListSelectionFlags           (ListRef                list);

/* Setters */
EXTERN_API( void )
SetListViewBounds               (ListRef                list,
                                 const Rect *           view);

EXTERN_API( void )
SetListPort                     (ListRef                list,
                                 CGrafPtr               port);

EXTERN_API( void )
SetListCellIndent               (ListRef                list,
                                 Point *                indent);

EXTERN_API( void )
SetListClickTime                (ListRef                list,
                                 SInt32                 time);

EXTERN_API( void )
SetListClickLoop                (ListRef                list,
                                 ListClickLoopUPP       clickLoop);

EXTERN_API( void )
SetListLastClick                (ListRef                list,
                                 Cell *                 lastClick);

EXTERN_API( void )
SetListRefCon                   (ListRef                list,
                                 SInt32                 refCon);

EXTERN_API( void )
SetListUserHandle               (ListRef                list,
                                 Handle                 userHandle);

EXTERN_API( void )
SetListFlags                    (ListRef                list,
                                 OptionBits             listFlags);

EXTERN_API( void )
SetListSelectionFlags           (ListRef                list,
                                 OptionBits             selectionFlags);
