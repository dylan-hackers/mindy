module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: see below

define constant GetResource = get-c-function("GetResource", args: list(<OSType>, <integer>),
											result: <Handle>, file: *InterfaceLib*);
define constant ReleaseResource = get-c-function("ReleaseResource", args: list(<Handle>),
											result: #(), file: *InterfaceLib*);
