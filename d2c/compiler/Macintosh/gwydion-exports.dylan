module: dylan-user
rcs-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/gwydion-exports.dylan,v 1.2.10.1 2004/10/09 07:00:13 gabor Exp $
file: gwydion-exports.dylan
author: gabor@mac.com
status: still much of experimental
copyright: see below

//======================================================================
//
// Copyright (c) 2000, 2001, 2002  Gwydion Dylan Maintainers
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


define library warrior
  use Dylan;
  use melange-support;
  use string-extensions;
  use streams;
  use format;
  use standard-io;
  use random;
  use format-out;
  use compiler-base;
  use compiler-parser;
  use compiler-front;
  use compiler-fer-transform;
  use compiler-optimize;
  use compiler-cback;
  use compiler-convert;
end library;


define module plugin-api
  use dylan;
  use melange-support;
  use system, import: {c-system-include /* can be rmoved */, <raw-pointer>, buffer-address};
  use extensions;
  use streams;
  use heap;	// hack!еее
  export
  	standard-seals-for,
  	<plugin-callback>,
  	get-plugin-request, done-plugin-request,
 
  	<new-text-document-info>, make-text-document-info, create-new-text-document,
  	get-api-version,
  	<plugin-error>, <plugin-fnf-error>, <plugin-duplicate-file-error>, cw-error,
  	$cw-no-error, $cw-user-canceled-error, $cw-request-failed-error, $cw-file-not-found-error,
  	$cw-err-invalid-parameter, $cw-err-invalid-callback, $cw-err-invalid-mp-callback, $cw-err-os-error,
  	$cw-err-out-of-memory, $cw-err-unknown-file, $cw-err-silent, $cw-err-cant-set-attribute,
  	$cw-err-last-common-error, $cw-err-unknown-segment, $cw-err-sbm-not-found,
  	$cw-err-object-file-not-stored, $cw-err-license-check-failed, $cw-err-last-compiler-linker-error,

  	get-target-name, user-break,
  	<mem-handle>, opaque-handle, new-mem-handle,
  	alloc-mem-handle, free-mem-handle,
  	lock-mem-handle, unlock-mem-handle, resize-mem-handle, get-mem-handle-size,
  	get-main-file-text, release-file-text,
  	<file-spec>, spec-file-name, spec-file-name-setter, volume-ref, volume-ref-setter, directory-ref, directory-ref-setter,
  	get-project-file, get-main-file-spec,

  	store-plugin-data, alert, macos-err-to-cw-result, display-lines, get-callback-os-error,
  	precompiling?, get-main-file-number,

  	<cw-vector>, vector-plugin, starting-address, free-vector-storage, dereferenced,

  	report-message, $message-type-info, $message-type-warning, $message-type-error,

  	cw-find-and-load-file, $no-dependency, $normal-dependency, $interface-dependency,
  	$cw-file-type-unknown, $cw-file-type-text, $cw-file-type-precompiled-header,
  	cw-find-file, cw-maybe-find-file,

  	<cw-file-stream>,
  	
  	add-project-entry, $default-link-position, get-project-file-count,
  	<project-file-info>, get-file-info, set-mod-date,
  	
  	project-file-spec, project-file-mod-date, project-file-segment, project-file-has-object-code?,
  	project-file-has-resources?, project-file-resourcefile?, project-file-weak-import?, project-file-init-before?,
  	project-file-gen-debug?, project-file-obj-mod-date, project-file-dropin-name,
  	project-file-id, project-file-record-browse-info?, project-file-type, project-file-creator,
  	project-file-has-unit-data?, project-file-merge-into-output?, project-file-unit-data-dependency-tag,
  	
  	show-status, store-plugin-data, get-plugin-data, $target-global-plugin-data,
  	
  	<cw-new-project-entry-info>,

  	create-file, write-file,
  	pre-file-action, post-file-action,

  	$cw-req-initialize, $cw-req-terminate,
  	$cw-req-compile, $cw-req-make-parse, $cw-req-comp-disassemble, $cw-req-check-syntax,
  	$cw-req-link, $cw-req-disassemble, $cw-req-target-info, $cw-req-pre-run,
  	$cw-req-idle, $cw-req-about, $cw-req-prefs-change,
	
  	$cw-req-target-link-ended, $cw-req-target-link-started,
  	$cw-req-file-list-build-ended, $cw-req-file-list-build-started,
  	$cw-req-sub-project-build-ended, $cw-req-sub-project-build-started,
  	$cw-req-target-build-ended, $cw-req-target-build-started,
  	$cw-req-project-build-ended, $cw-req-project-build-started,
  	$cw-req-target-loaded, $cw-req-target-prefs-changed, $cw-req-target-unloaded,
	
  	begin-sub-compile, end-sub-compile, get-named-preferences,
  	resolve-relative-path, relative-path-from,
	
  	store-object-data, <object-data>, <ppc-object-data>,
  	browse-data, browse-data-setter,
  	dependencies, dependencies-setter,
  	fill-object-data-header,
  	get-browse-options,
	
/*	<browse-record>, <browse-function>, <browse-class>,
	start-offset, start-offset-setter,
	end-offset, end-offset-setter,*/
	
  	c-enumeration-definer, c-int-expr, c-ptr-expr, c-void-expr;


  create write-1, write-2, write-4, write-raw-str, write-str;	// hack...
end module;

define module cw-output-streams
  use dylan;
  use streams;
  use plugin-api;
  use melange-support;
  use extensions;
  use system, import: {call-out, <buffer>, copy-bytes, buffer-address};
  use format, import: {format-to-string};
  use format-out;	// testing only...
  use standard-io;	// testing only...
  export
  	<cw-binary-output-stream>, <cw-text-output-stream>,
  	determine-file-name,
  	get-gwydion-prefs;
end module;

define module symbol-browser
  use dylan, exclude: {direct-superclasses, direct-subclasses};
  use melange-support;
  use system, import: {c-system-include}; // can be rmoved
  use format, import: {format-to-string};
  use extensions;
  use tokens;
  use source;
/*  use source-utilities, import: {	<macro-source-location>, simplify-source-location,
  																<compound-macro-source-location>, macro-srcloc-first,
  																macro-srcloc-last};*/
  use names;
  use variables, import: {variable-definition};
  use signature-interface, import: {<signature>, specializers};
  use ctype, import: {<ctype>};	// needed???
  use compile-time-functions, import: {<ct-function>, ct-function-signature};
  use parse-tree;
  use definitions;
  use define-classes;
  use define-constants-and-variables;
  use classes, rename: {abstract? => class-abstract?};
  use function-definitions, import: {method-defn-of, method-defn-congruent?};
  use top-level-forms;
  use macros;
  use define-macros;
  use plugin-api;
  export
  	write-browse-header, write-browse-end, gather-browser-infos, dump-browser /*,
  	write-1, write-2, write-4, write-raw-str, write-str */ ;
end module;

define module warrior
  use dylan;
  use streams;
  use extensions;	// For Main, false-or
  use melange-support;
  use system, import: {c-system-include /* can be rmoved */, c-expr, call-out, <buffer>,
		       copy-bytes, buffer-address};
  use format;
  use standard-io;
  use format-out;
  use random;
  use string-conversions, import: {string-to-integer};
  use %Hash-Tables;	/*until loader hashtable goes in od-format*/
  use plugin-api;
  use cw-output-streams;
  use character-type, import: {whitespace?};
  
  use platform;
  use platform-constants;
  use source;
  use header;
  use utils, import: {stringify};
  use variables;
  use tokens;
  use tokenize;
  use lexer;
  use top-level-forms;
  use parser;
  use errors;
  use od-format, import: {find-data-unit, $library-summary-unit-type, add-make-dumper,
  							dump-od, begin-dumping, end-dumping
  							
  							/* just experimenting*/ ,obj-resolved? /*, *lookup-loader* */};
  use definitions;
  use compile-time-values, import: {*compiler-dispatcher*};
  use signature-interface, import: {<signature>};
  use classes,	import: {inherit-slots, inherit-overrides, layout-instance-slots,
  						calculate-type-inclusion-matrix, assign-unique-ids,
//  						*all-classes*, $class-for-id, loaded?,
  						subclasses, direct-subclasses, <cclass>,
  						all-slot-infos /* , reset-slot*/ },
  				rename: {direct-subclasses => cclass-direct-subclasses};
  use names, import: {<anonymous-name>, <basic-name>};
  use c-representation;
  use front, import: {<fer-component>};
  use abstract-optimizer, import: {optimize-component, *current-optimizer*};
  use cheese, import: {<cmu-optimizer>};
  use function-definitions, import: {dump-queued-methods};
  use flow, import: {<empty-region>};
  use builder-interface, import: {end-body, make-function-literal, build-function-body,
  		build-region, build-return, build-function-body, builder-result, make-builder,
  		make-local-var, make-unknown-call, build-assignment, build-defn-ref};
  use ctype, import: {make-values-ctype, specifier-type};
  use compile-time-functions, import: {<ct-function>, <ct-entry-point>};
  use cback;
  use heap;
  use policy;
  use file-system;
  use define-functions, import: {*implicitly-define-next-method*};
  use symbol-browser;
end module;
