module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Dialogs.

define constant <DialogPtr> = <GrafPtr>;
define constant <ModalFilterUPP> = <UniversalProcPtr>;
define constant $uppModalFilterProcInfo = 4048;

define constant Alert =
begin
	let func = get-c-function("Alert", args: list(<integer>, <ModalFilterUPP>),
								result: <integer>, file: *InterfaceLib*);
	method (id :: <integer>, #key filter: flt = #f)
		if (~flt)
			flt := as(<ModalFilterUPP>, 0);
		end if;
		func(id, flt);
	end method;
end;
