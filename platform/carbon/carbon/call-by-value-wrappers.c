/*
** Extra wrapper functions that sanitize functions that use call-by-value Points
*/

#include <Carbon/Carbon.h>

/*
	Menu Wrappers
*/

EXTERN_API_C( long )
menuselect(const Point *          startPt)
{
	return MenuSelect(*startPt);
}

/*
	Window Wrappers
*/

EXTERN_API_C( void )
dragwindow                      (WindowRef              window,
                                 Point *                startPt,
                                 const Rect *           boundsRect)
{
	DragWindow(window, *startPt, boundsRect);
}

EXTERN_API_C( short )
findwindow                      (Point *                thePoint,
                                 WindowRef *            window)
{
	return FindWindow( *thePoint,  window);
}

EXTERN_API_C( long )
growwindow						(WindowRef 				window,
								 Point *				startPt,
								 const Rect *			bBox)
{
	GrowWindow( window, *startPt, bBox );
}

/*
	Control Wrappers
*/

EXTERN_API_C( Boolean )
trackbox                        (WindowRef              window,
                                 Point *                thePt,
                                 short                  partCode)
{
	return TrackBox(window, *thePt, partCode);
}

EXTERN_API_C( void )
dragcontrol						(ControlRef 			theControl,
								 Point *				startPt,
								 const Rect *			limitRect,
								 const Rect *			slopRect,
								 short 					axis)
{
	DragControl( theControl, *startPt, limitRect, slopRect, axis );
}
								 
EXTERN_API_C( short )
findcontrol						(Point *				thePoint,
								 WindowPtr 				theWindow,
								 ControlRef *			theControl)
{
	FindControl( *thePoint, theWindow, theControl );
}
								 
EXTERN_API_C( short )
trackcontrol					(ControlRef 			theControl,
								 Point *				thePoint,
								 ControlActionUPP 		actionProc)
{
	TrackControl( theControl, *thePoint, actionProc );
}
								 
EXTERN_API_C( short )
testcontrol						(ControlRef 			theControl,
								 Point *				thePt)
{
	TestControl( theControl, *thePt );
}
								 
EXTERN_API_C( Boolean )
trackgoaway						(WindowRef 				window,
								 Point *				thePt)
{
	TrackGoAway( window, *thePt );
}

EXTERN_API_C (ControlPartCode )
handlecontrolclick		(ControlRef inControl, 
                        Point * inWhere, 
                        EventModifiers inModifiers, 
                        ControlActionUPP inAction)
{
    HandleControlClick( inControl, *inWhere, inModifiers, inAction );
}

EXTERN_API( ListRef )
lnew                            (const Rect *           rView,
                                 const ListBounds *     dataBounds,
                                 Point     *            cSize,
                                 short                  theProc,
                                 WindowRef              theWindow,
                                 Boolean                drawIt,
                                 Boolean                hasGrow,
                                 Boolean                scrollHoriz,
                                 Boolean                scrollVert)  
{
  return LNew(rView, dataBounds, *cSize, theProc, theWindow, drawIt, hasGrow, scrollHoriz, scrollVert);
}        

EXTERN_API( Boolean )
lclick               (Point            *     pt,
                      short                  modifiers,
                      ListRef             lHandle)
{
  return LClick(*pt, modifiers, lHandle);
}

EXTERN_API( void )
lgetcell                        (void *                 dataPtr,
                                 short *                dataLen,
                                 Cell   *               theCell,
                                 ListRef             lHandle) 
{
  LGetCell(dataPtr, dataLen, *theCell, lHandle);
}                                 

EXTERN_API( void )
lsetcell                        (const void *           dataPtr,
                                 short                  dataLen,
                                 Cell      *            theCell,
                                 ListRef             lHandle)
{
  LSetCell(dataPtr, dataLen, *theCell, lHandle);
}

EXTERN_API( void )
lsetselect                      (Boolean                setIt,
                                 Cell         *         theCell,
                                 ListRef             lHandle) 
{
  LSetSelect(setIt, *theCell, lHandle);
}