module: carbon

/*
	Gestalt
*/

/*
	Includes
*/

c-include( "Carbon.h" );


/*
	Gestalt
*/

define method Gestalt( selector :: <OSType> )
=> ( result :: <OSErr>, response :: <integer> )
	let responseScratchSpace = make( <Handle> );
    let err :: <OSErr> = call-out("Gestalt", int:, int: selector, ptr: responseScratchSpace.raw-value );
    let response :: <integer> = signed-long-at( responseScratchSpace );
    values( err, response );
end method Gestalt;
	

// Quickdraw

//define constant $gestaltQuickdrawVersion	= os-type("qd  ");				//{  quickdraw version  }
define constant $gestaltQuickdrawVersion    = as(<OSType>, #x71642020);		//{  quickdraw version  }
define constant $gestaltOriginalQD			= #x0000;						/* original 1-bit QD */
define constant $gestalt8BitQD				= #x0100;						/* 8-bit color QD */
define constant $gestalt32BitQD				= #x0200;						/* 32-bit color QD */
define constant $gestalt32BitQD11			= #x0201;						/* 32-bit color QDv1.1 */
define constant $gestalt32BitQD12			= #x0220;						/* 32-bit color QDv1.2 */
define constant $gestalt32BitQD13			= #x0230;						/* 32-bit color QDv1.3 */
define constant $gestaltAllegroQD			= #x0250;						/* Allegro QD OS 8.5 */

// OS Type

define constant $gestaltSystemVersion = c-expr(int: "gestaltSystemVersion");

