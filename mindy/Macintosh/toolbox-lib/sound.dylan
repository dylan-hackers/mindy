module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: see below
// Sound Manager.

define constant SysBeep = get-c-function("SysBeep", args: list(<integer>),
											result: #(), file: *InterfaceLib*);

define class <SndChannel> (<Ptr>) end class;

define constant SndPlay = get-c-function("SndPlay", args: list(<SndChannel>, <Handle>, <boolean>),
											result: <OSErr>, file: *InterfaceLib*);