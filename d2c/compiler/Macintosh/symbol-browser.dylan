module: symbol-browser
rcs-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/symbol-browser.dylan,v 1.3 2004/04/13 23:47:13 gabor Exp $
file: gwydion.dylan
author: gabor@mac.com
copyright: see below

//======================================================================
//
// Copyright (c) 2000 - 2004  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================


// TODO: use CWGetBrowseOptions
// inherited slots
// take source-loc from tlf, not from defn! (care with multiple vars/consts!)
// possibly take bases from defn, not ctv! (care with implicit <object>!)
// virtual setters!
// how to deal with renamed GFs that are virtual slots?
// use <method-definition>'s signature instead of <ct-func>'s
// clean up imports
// if virtual slot defined before declaration, I cannot connect them


define macro using
	{ using (?:name = ?:expression) ?:body end }
		=> { let ?name = ?expression; if (?name) ?body; end; ?name }
end macro using;

define abstract class <browse-offset-mixin>(<object>)
	slot start-offset :: <integer>, init-value: -1;
	slot end-offset :: <integer>, init-value: -1;
end class <browse-offset-mixin>;

define abstract class <browse-record>(<browse-offset-mixin>)
	slot contributing-file :: <integer>, init-value: 0;
	slot browse-source-file :: <integer>, init-value: 0;
	slot simple-name :: <byte-string>, required-init-keyword: name:;
	slot qualified-name :: <byte-string>, init-value: "";
end class <browse-record>;

define class <browse-several>(<browse-record>)	// just a placeholder for multiple browse-infos
	constant slot severals :: <sequence>, required-init-keyword: severals:;
end class <browse-several>;

define class <browse-macro>(<browse-record>)
end class <browse-macro>;

define class <browse-enum>(<browse-record>)
end class <browse-enum>;

define class <browse-constant>(<browse-record>)
end class <browse-constant>;

define class <browse-typedef>(<browse-record>)
end class <browse-typedef>;

define class <browse-package>(<browse-record>)
end class <browse-package>;

define abstract class <static-mixin>(<object>)
	slot static? :: <boolean>, init-value: #f;
end class <static-mixin>;

define abstract class <abstract-mixin>(<object>)
	slot abstract? :: <boolean>, init-value: #f;
end class <abstract-mixin>;

define class <browse-function>(<browse-record>, <static-mixin>)
	slot member-func? :: <boolean>, init-value: #f;
	slot inline? :: <boolean>, init-value: #f;
	slot pascal? :: <boolean>, init-value: #f;
	slot asm? :: <boolean>, init-value: #f;
end class <browse-function>;

define class <browse-member-function>(<browse-function>)
	slot member-id :: <integer>, init-value: 0;
end class <browse-member-function>;

define class <browse-class>(<browse-record>, <abstract-mixin>)
	slot final? :: <boolean>, init-value: #f;
	slot java-interface? :: <boolean>, init-value: #f;
	slot java-public? :: <boolean>, init-value: #f;
	slot bases :: <sequence>, init-value: #();
	slot members :: <sequence>, init-value: #();
end class <browse-class>;

define abstract class <access-flags-mixin>(<object>)
	// better? one-of(private:, protected:, public:)
	slot private? :: <boolean>, init-value: #f;
	slot protected? :: <boolean>, init-value: #t;
	slot public? :: <boolean>, init-value: #f;
end class <access-flags-mixin>;

define class <browse-class-base>(<access-flags-mixin>)
	slot virtual? :: <boolean>, required-init-keyword: virtual:;
	slot base-class-name :: <byte-string>, required-init-keyword: name:;
end class <browse-class-base>;

define abstract class <browse-class-member>(<access-flags-mixin>, <static-mixin>)
	slot final? :: <boolean>;
end class <browse-class-member>;

define class <browse-class-member-function>(<browse-class-member>, <browse-offset-mixin>, <abstract-mixin>)
	slot virtual? :: <boolean>, init-value: #t;
	slot ctor? :: <boolean>, init-value: #f;
	slot dtor? :: <boolean>, init-value: #f;
	slot java-native? :: <boolean>, init-value: #f;
	slot java-synchronized? :: <boolean>, init-value: #f;
	slot browse-function-id :: <integer>, required-init-keyword: id:;
end class <browse-class-member-function>;

define class <browse-class-data-member>(<browse-class-member>, <browse-offset-mixin>)
	slot java-transient? :: <boolean>, init-value: #f;
	slot java-volatile? :: <boolean>, init-value: #f;
	slot member-name :: <byte-string>, required-init-keyword: name:;
end class <browse-class-data-member>;

define class <browse-template>(<browse-record>)
	slot template-type :: one-of(#"class", #"function");
end class <browse-template>;

define class <browse-global>(<browse-record>, <static-mixin>)
end class <browse-global>;


define /*type-preserving*/ function suggest(what :: <browse-offset-mixin>) => result :: <browse-offset-mixin>.false-or;
	what.start-offset >= 0 & what
end function suggest;

// browser-info: return a CodeWarrior <browse-record> for a relevant top level form
//	or #f if no browser relevant info can (or must) be generated.
//
define generic browser-info(tlf :: <top-level-form>, file-id-callback :: <function>) => info :: <browse-record>.false-or;

define method browser-info(tlf :: <top-level-form>, file-id-callback :: <function>) => info :: #f.singleton;
end;


define generic get-symbol-names(name :: <name>)
	=> (simple-name :: <byte-string>, qualified-name :: <byte-string>.false-or);

define inline method get-symbol-names(name :: <basic-name>)
	=> (simple-name :: <byte-string>, qualified-name :: #f.singleton);

	as(<byte-string>, name.name-symbol)
end;

define method get-symbol-names(name :: <method-name>)
	=> (simple-name :: <byte-string>, qualified-name :: <byte-string>);

	values(name.method-name-generic-function.get-symbol-names, format-to-string("%s", name))
end;

define function function-info(defn :: <function-definition>,
		file-id-callback :: <function>,
		info-class :: <class>)
	=> info :: <browse-function>.false-or;

	let (simple-name, qualified-name)
		= defn.defn-name.get-symbol-names;

	let info = make(info-class /*<browse-function>*/, name: simple-name);
	
	stuff-location(defn, info, file-id-callback);
	
	if (qualified-name)
		info.qualified-name := qualified-name;	// use keyword!!!¥¥¥
	end if;
	
	info.suggest
end;


define inline function get-single-specializer(ctv :: <ct-function>) => specializer :: <ctype>.false-or;
	let specializers = ctv.ct-function-signature.specializers;
	specializers.size == 1 & specializers.first
end;

define method browser-info(tlf :: <define-method-tlf>, file-id-callback :: <function>) => info :: <browse-function>.false-or;
	// maybe it should be <browse-member-function>? <-- for virtual slots
	
	let defn = tlf.tlf-defn;
	let ctv :: <ct-function>.false-or = defn.ct-value;
	let generic-defn = defn.method-defn-of;
	let generic-ctv :: <ct-function>.false-or = generic-defn & generic-defn.ct-value;
	let spec = defn.method-defn-congruent? & ctv & ctv.get-single-specializer;
	let sofar :: <list> = element($seen-virtual-slots, spec, default: #());
	let found = find-key(sofar, curry(\==, generic-ctv));
	if (found)
		using (it = function-info(defn, file-id-callback, <browse-member-function>))
			it.member-id := sofar.size - found;
		end
/*		let info = function-info(defn, file-id-callback, <browse-member-function>);
		info & (info.member-id := sofar.size - found);
		info*/
	else
		function-info(defn, file-id-callback, <browse-function>)
	end if;
end;


define generic browse-option(info :: <browse-record>) => option :: <symbol>;

define method browse-option(info :: <browse-class>) => option :: <symbol>;
	#"class"
end method browse-option;

define method browse-option(info :: <browse-function>) => option :: <symbol>;
	#"function"
end method browse-option;

define method browse-option(info :: <browse-macro>) => option :: <symbol>;
	#"macro"
end method browse-option;

define method browse-option(info :: <browse-global>) => option :: <symbol>;
	#"variable"
end method browse-option;

define method browse-option(info :: <browse-constant>) => option :: <symbol>;
	#"constant"
end method browse-option;


define function get-file-location(loc)
	=> (file-name :: <byte-string>.false-or, start :: <integer>, stop :: <integer>);

	select (loc by instance?)
		<source-location-mixin> =>
			loc.source-location.get-file-location;

/*
		<compound-macro-source-location> =>	// probably not needed any more...
			let simple-loc = loc.simplify-source-location;
			if (simple-loc == loc)
				let (file-name1, start :: <integer>)
					= simple-loc.macro-srcloc-first.get-file-location;
				let (file-name2, start2 :: <integer>, stop :: <integer>)
					= simple-loc.macro-srcloc-last.get-file-location;
				if (file-name1 & file-name2)
					values(file-name1, start, stop)
				elseif(file-name1)
					values(file-name1, start, start)
				elseif(file-name2)
					values(file-name2, stop, stop)
				else
					values(#f, 0, 0)
				end if
			else
				simple-loc.get-file-location
			end if;
		<macro-source-location> =>
			loc.simplify-source-location.get-file-location;

*/
		<known-source-location> =>
			values(loc.source.full-file-name, loc.start-posn, loc.end-posn);
		otherwise =>
			values(#f, 0, 0);
	end select
end function get-file-location;

define function stuff-location(loc, info :: <browse-record>, file-id-callback :: <function>) => ();
	let (file-name, start :: <integer>, stop :: <integer>)
		= loc.get-file-location;

	if (file-name)
		let id = file-id-callback(info.browse-option, file-name);
		if (id)
			info.contributing-file := id;
			info.browse-source-file := id;
			info.start-offset := start;
			info.end-offset := stop;
		end if
	end if
end function stuff-location;

define function stuff-location-only(loc, info :: <browse-offset-mixin>) => ();
	let (file-name, start :: <integer>, stop :: <integer>)
		= loc.get-file-location;
	if (file-name)
		info.start-offset := start;
		info.end-offset := stop;
	end if
end function stuff-location-only;

define generic member-info-aux(slot :: <slot-defn>, allocation :: <slot-allocation>, enclosing-loc :: <integer>) => info :: <browse-class-member>.false-or;


define method member-info-aux(slot :: <slot-defn>, allocation :: <slot-allocation>, enclosing-loc :: <integer>) => info :: <browse-class-member>.false-or;
	make(<browse-class-data-member>, name: slot.slot-defn-getter-name.get-symbol-names)
end;

define method member-info-aux(slot :: <slot-defn>,
		allocation :: one-of(#"class", #"each-subclass"),
		enclosing-loc :: <integer>,
		#next next-method)
	=> info :: <browse-class-member>.false-or;
	let info = next-method();
	info.static? := #t;	// keyword?
	info
end;



define constant $seen-virtual-slots = <table>.make;
// this is a hack, it should be passed in to the browse-info GF ¥¥¥
// organized thus:
//	$seen-virtual-slots[<cclass>] == list(pair(<variable>, meth-index))

define method member-info-aux(slot :: <slot-defn>, allocation == #"virtual", enclosing-loc :: <integer>) => info :: <browse-class-member>.false-or;
	let slot-info = slot.slot-defn-info;
	let slot-var = slot-info.slot-getter;
	if (slot-var)
		let introducer = slot-info.slot-introduced-by;
		let sofar :: <list> = element($seen-virtual-slots, introducer, default: #());
		make(<browse-class-member-function>,
				name: slot.slot-defn-getter-name.get-symbol-names,
				id: size($seen-virtual-slots[introducer] := pair(slot-var.variable-definition.ct-value, sofar)))
	end if;
	
	
	//¥¥¥slot-info.slot-read-only?
end;



define method member-info(slot :: <slot-defn>, enclosing-loc :: <integer>) => info :: <browse-class-member>;
	let info = member-info-aux(slot, slot.slot-defn-allocation, enclosing-loc);
	stuff-location-only(slot, info);
	info.start-offset := info.start-offset - enclosing-loc;
	info.end-offset := info.end-offset - enclosing-loc;
	info
end method member-info;


define method browser-info(tlf :: <define-class-tlf>, file-id-callback :: <function>) => info :: <browse-class>.false-or;
	let defn :: <class-definition> = tlf.tlf-defn;
	let info = make(<browse-class>, name: as(<byte-string>, defn.defn-name.name-symbol));
	
	stuff-location(defn, info, file-id-callback);
	
	// better use <local-class-definition>::class-defn-supers?¥¥
	
	let ctv = defn.ct-value;

	if (ctv)
		info.bases := map(method(super :: <cclass>)
									make(<browse-class-base>,
										name: as(<byte-string>, super.cclass-name.name-symbol),
										virtual: #t) end method,
						ctv.direct-superclasses);
	end if;
	
	instance?(defn, <local-class-definition>) & info.start-offset >= 0
	&	(info.members := map(rcurry(member-info, info.start-offset /* and a closure for id generation!!!*/), defn.class-defn-slots));
	
	info.suggest
end;


define method browser-info(tlf :: <define-macro-tlf>, file-id-callback :: <function>) => info :: <browse-macro>.false-or;
	let defn :: <macro-definition> = tlf.tlf-defn;
	let info = make(<browse-macro>, name: as(<byte-string>, defn.defn-name.name-symbol));

	stuff-location(defn, info, file-id-callback);
	
	info.suggest
end method browser-info;


define function parameter-info(the-var :: <parameter>, info-class :: <class>, file-id-callback :: <function>)
 => info :: <browse-record>.false-or;

	let info = make(info-class, name: as(<byte-string>, the-var.param-name.token-symbol));
	stuff-location(the-var.param-name, info, file-id-callback);
	info.suggest
end function parameter-info;


define function binding-info(tlf :: <define-bindings-tlf>, file-id-callback :: <function>, info-class :: <class>)
	=> info :: <browse-record>.false-or;

	let tlf-variables = tlf.tlf-variables;
	let (varlist-fixed, varlist-rest) = values(tlf-variables.varlist-fixed, tlf-variables.varlist-rest);
	let vars-defined = varlist-fixed.size + (varlist-rest & 1 | 0);
	
	if (vars-defined == 1)
		let the-var :: <parameter> = varlist-rest & varlist-rest | varlist-fixed.first;
		let info = make(info-class, name: as(<byte-string>, the-var.param-name.token-symbol));
		stuff-location(tlf, info, file-id-callback);
		info.suggest
	else
		let the-vars = varlist-rest & concatenate(varlist-fixed, vector(varlist-rest)) | varlist-fixed;
		make(<browse-several>,
					severals: choose(not-false, map(rcurry(parameter-info, info-class, file-id-callback), the-vars)),
					name: "" /* overload slot? */);
	end if
end function binding-info;

define method browser-info(tlf :: <define-variable-tlf>, file-id-callback :: <function>) => info :: <browse-record>.false-or;
	binding-info(tlf, file-id-callback, <browse-global>)
end method browser-info;


define method browser-info(tlf :: <define-constant-tlf>, file-id-callback :: <function>) => info :: <browse-record>.false-or;
	binding-info(tlf, file-id-callback, <browse-constant>)
end;

define method browser-info(tlf :: <define-constant-method-tlf>, file-id-callback :: <function>) => info :: <browse-record>.false-or;
	function-info(tlf.tlf-required-defns.first, file-id-callback, <browse-function>)
end;


define constant not-false :: <function> = curry(\~==, #f);

define function gather-browser-infos(tlfs :: <sequence>, file-id-callback :: <function>) => infos :: <sequence>;
	choose(not-false, map(rcurry(browser-info, file-id-callback), tlfs))
end function gather-browser-infos;


// helper functions writing basic datatypes to a <cw-vector>
//
//	write-1: write a byte
define method write-1(byte :: <integer>, vec :: <cw-vector>, #key pos :: <integer> = vec.size, capacity :: <integer> = pos)
	=> (pos :: <integer>, capacity :: <integer>);
	
	local method put() => ();
			starting-address(vec);
			unsigned-byte-at-setter(byte, vec.dereferenced, offset: pos)
		end;

	let behind :: <integer> = pos + 1;

	if (behind > capacity)
		vec.size := behind;
		put();
		values(behind, behind);
	else
		put();
		values(behind, capacity);
	end if;
end;

//	write-2: write a short
define method write-2(short :: <integer>, vec :: <cw-vector>, #key pos :: <integer> = vec.size, capacity :: <integer> = pos)
	=> (pos :: <integer>, capacity :: <integer>);
	
	local method put() => ();
			starting-address(vec);
			unsigned-short-at-setter(short, vec.dereferenced, offset: pos)
		end;

	let behind :: <integer> = pos + 2;

	if (behind > capacity)
		vec.size := behind;
		put();
		values(behind, behind);
	else
		put();
		values(behind, capacity);
	end if;
end;

//	write-4: write a long
define method write-4(long :: <integer>, vec :: <cw-vector>, #key pos :: <integer> = vec.size, capacity :: <integer> = pos)
	=> (pos :: <integer>, capacity :: <integer>);
	
	local method put() => ();
			starting-address(vec);
			unsigned-long-at-setter(long, vec.dereferenced, offset: pos)
		end;

	let behind :: <integer> = pos + 4;

	if (behind > capacity)
		vec.size := behind;
		put();
		values(behind, behind);
	else
		put();
		values(behind, capacity);
	end if;
end;

//	write-raw-str: write a string without length in front
define method write-raw-str(str :: <byte-string>, vec :: <cw-vector>, #key pos :: <integer> = vec.size, capacity :: <integer> = pos, zero :: <boolean> = #t)
	=> (pos :: <integer>, capacity :: <integer>);
	
	let siz = str.size;
	let need-cap = pos + siz + (zero & 1 | 0);
	let new-cap = need-cap > capacity & (vec.size := need-cap) | capacity;

	starting-address(vec);

	for (i from 0 below siz)
		unsigned-byte-at(vec.dereferenced, offset: pos + i) := as(<integer>, str[i]);
	end for;

	zero & write-1(0, vec, pos: need-cap - 1, capacity: new-cap);
	
	values(need-cap, new-cap);
end;

//	write-str: write a string
define method write-str(str :: <byte-string>, vec :: <cw-vector>, #key pos :: <integer> = vec.size, capacity :: <integer> = pos)
	=> (pos :: <integer>, capacity :: <integer>);
	
	let siz = str.size;
	let need-cap = pos + siz + 3;
	let new-cap = need-cap > capacity & (vec.size := need-cap) | capacity;

	write-raw-str(str, vec, pos: write-2(siz, vec, pos: pos, capacity: new-cap), capacity: new-cap);
/*	starting-address(vec);

	for (i from 0 below siz)
		unsigned-byte-at(vec.dereferenced, offset: pos + 2 + i) := as(<integer>, str[i]);
	end for;

	write-1(0, vec, pos: need-cap - 1, capacity: new-cap);*/
	
	values(need-cap, new-cap);
end;

//	write-str-or-empty: write a string that can be empty
define method write-str-or-empty(str :: <byte-string>, vec :: <cw-vector>, #key pos :: <integer> = vec.size, capacity :: <integer> = pos)
	str.empty?
		& write-2(0, vec, pos: pos, capacity: capacity)
		| write-str(str, vec, pos: pos, capacity: capacity)
end;

c-system-include("MWBrowse.h");
c-system-include("MWLangDefs.h");

define function write-browse-header(vec :: <cw-vector>) => vec :: <cw-vector>;
	write-4(c-int-expr("BROWSE_HEADER"), vec);
	write-4(c-int-expr("BROWSE_VERSION_V2"), vec);
	write-2(c-int-expr("langUnknown"), vec);
	write-2(0, vec);	// uses_name_table
	write-4(c-int-expr("BROWSE_EARLIEST_COMPATIBLE_VERSION_V2"), vec);

	for (reserved from 0 below 15)
		write-4(0, vec);
	end for;

	vec
end function write-browse-header;


define function write-browse-end(vec :: <cw-vector>) => vec :: <cw-vector>;
	write-1($browse-end, vec);

	vec
end function write-browse-end;

// dumping routine: for a sequence of <browse-record>s
//
define function dump-browser(infos :: <sequence>, vec :: <cw-vector>) => vec :: <cw-vector>;
	do(rcurry(dump-browser-info, vec), infos);
	vec
end function dump-browser;

// dumping routines: for a single of <browse-record>s
//
define generic dump-browser-info(info :: <browse-record>, vec :: <cw-vector>) => ();

define method dump-browser-info(info :: <browse-record>, vec :: <cw-vector>) => ();
	write-1(info.browse-type, vec);
	write-2(info.contributing-file, vec);
	write-2(info.browse-source-file, vec);
	write-4(info.start-offset, vec);
	write-4(info.end-offset, vec);
	write-4(0 /*reserved*/, vec);
	write-str(info.simple-name, vec);
	write-str-or-empty(info.qualified-name, vec);
end;

define c-enumeration
	$static = 2,	/* Static routine */
	$member = 8,	/* Class member function */
	$inline = #x80,	/* In line routine */
	$pascal = #x100,/* Pascal routine */
	$asm = #x200	/* Assembly routine */
end c-enumeration;

define method dump-browser-info(info :: <browse-function>, vec :: <cw-vector>, #next next-method) => ();
	next-method();
	write-4(	(info.static? & $static | 0)
			+	(info.member-func? & $member | 0)
			+	(info.inline? & $inline | 0)
			+	(info.pascal? & $pascal | 0)
			+	(info.asm? & $asm | 0),
			vec);
	write-4(info.member-id, vec);
end;


define c-enumeration
	$abstract	= 1,
	$final		= 4,
	$java-interface	= #x80,
	$java-public	= #x100
end c-enumeration;

define c-enumeration
	$none-access,
	$private-access,
	$protected-access,
	$public-access	= 4,
	$all-access = $private-access + $protected-access + $public-access
end c-enumeration;


define function dump-base-info(info :: <browse-class-base>, vec :: <cw-vector>, #next next-method) => ();
	write-1(	(info.private? & $private-access | $none-access)
				+(info.protected? & $protected-access | $none-access)
				+(info.public? & $public-access | $none-access), vec);
	write-1(info.virtual? & 1 | 0, vec);
	write-str(info.base-class-name, vec);
end function dump-base-info;

define generic dump-member-info(info :: <browse-class-member>, vec :: <cw-vector>, #next next-method) => ();

define c-enumeration
	$member-function,			// member function/method
	$member-data,				// data member/field	
	$member-end = #xFF
end c-enumeration;

define function write-access(prot :: <access-flags-mixin>, vec :: <cw-vector>) => ();
	write-1(	(prot.private? & $private-access | $none-access)
				+	(prot.protected? & $protected-access | $none-access)
				+	(prot.public? & $public-access | $none-access), vec);
end;

define method dump-member-info(info :: <browse-class-data-member>, vec :: <cw-vector>, #next next-method) => ();
	write-1($member-data, vec);
	write-access(info, vec);
	
	write-4(0, vec);	// ¥¥¥ flags
	
	write-4(info.start-offset, vec);	// ¥¥factor out!!!
	write-4(info.end-offset, vec);
	
	write-str(info.member-name, vec);
end method dump-member-info;


define method dump-member-info(info :: <browse-class-member-function>, vec :: <cw-vector>, #next next-method) => ();
	write-1($member-function, vec);
	write-access(info, vec);

	write-4(0, vec);	// ¥¥¥ flags
	write-4(info.browse-function-id, vec);
	
	write-4(info.start-offset, vec);	// ¥¥factor out!!!
	write-4(info.end-offset, vec);
end method dump-member-info;

define method dump-browser-info(info :: <browse-class>, vec :: <cw-vector>, #next next-method) => ();
	next-method();
	write-4(	(info.abstract? & $abstract | 0)
			+	(info.final? & $final | 0)
			+	(info.java-interface? & $java-interface | 0)
			+	(info.java-public? & $java-public | 0),
			vec);
	write-1(info.bases.size, vec);

	do(rcurry(dump-base-info, vec), info.bases);

	do(rcurry(dump-member-info, vec), info.members);
	
	write-1($member-end, vec);
end;

define method dump-browser-info(info :: <browse-global>, vec :: <cw-vector>, #next next-method) => ();
	next-method();
	write-4(info.static? & $static | 0, vec);	// flags
end;

define method dump-browser-info(info :: <browse-several>, vec :: <cw-vector>) => ();
	do(rcurry(dump-browser-info, vec), info.severals);
end;

define generic member-id(info :: <browse-function>) => id :: <integer>;

define method member-id(info :: <browse-function>) => id :: <integer>;
	0
end method member-id;

define c-enumeration
	$browse-function,	// function, procedure, or method
	$browse-global,		// global variable
	$browse-class,		// class, struct, or union
	$browse-macro,		// macro
	$browse-enum,			// enum, enumerated type member
	$browse-typedef,	// user-defined type other than class
	$browse-constant,	// constant value
	$browse-template,	// C++ template
	$browse-package,	// Java package
	$browse-comp-symbol-start = #x70,
	$browse-end = #xFF	// used to denote end-of-list
end c-enumeration;

define generic browse-type(info :: <browse-record>) => type :: <integer>;

define method browse-type(info :: <browse-function>) => type :: <integer>;
	$browse-function
end;

define method browse-type(info :: <browse-class>) => type :: <integer>;
	$browse-class
end;

define method browse-type(info :: <browse-macro>) => type :: <integer>;
	$browse-macro
end;

define method browse-type(info :: <browse-constant>) => type :: <integer>;
	$browse-constant
end;

define method browse-type(info :: <browse-typedef>) => type :: <integer>;
	$browse-typedef
end;

define method browse-type(info :: <browse-global>) => type :: <integer>;
	$browse-global
end;

// <maker-function-definition>, <init-function-definition> ==> constructor!


standard-seals-for(<browse-class>);
standard-seals-for(<browse-macro>);
standard-seals-for(<browse-constant>);
standard-seals-for(<browse-global>);
standard-seals-for(<browse-enum>);
standard-seals-for(<browse-function>);
standard-seals-for(<browse-member-function>);
standard-seals-for(<browse-typedef>);
