module: plugin-api
file: plugin-api.dylan
author: gabor@mac.com
synopsis: CW plugin interface.
RCS-header: $Header: /scm/cvs/src/d2c/compiler/Macintosh/plugin-api.dylan,v 1.1 2004/04/13 21:01:46 gabor Exp $

c-system-include("Files.h");
c-system-include("TextUtils.h");
c-system-include("DropInCompilerLinker.h");

define macro standard-seals-for
	{ standard-seals-for(?:name) }
		=>
	{ define sealed domain make (?name.singleton);
		define sealed domain initialize (?name)
	}
end macro;

define macro c-ptr-expr
	{ c-ptr-expr(?:expression) } => { c-expr(ptr:, ?expression) }
end macro c-ptr-expr;

define macro c-int-expr
	{ c-int-expr(?:expression) } => { c-expr(int:, ?expression) }
end macro c-int-expr;

define macro c-void-expr
	{ c-void-expr(?:expression) } => { c-expr(void:, ?expression) }
end macro c-void-expr;

define macro c-int-literal
//    { c-int-literal (?decl:expression) } => { %%primitive(\c-literal, #"int", ?decl) }
    { c-int-literal (?decl:expression) } => { c-literal(#"int", ?decl) }
end;

define macro c-zeroed-local
	{ c-zeroed-local(?c-type:expression, ?c-name:expression) }
	=>
	{
		c-local-decl(?c-type " " ?c-name ";");
		c-void-expr("memset(&" ?c-name ", 0, sizeof(" ?c-name "))");
	}
end macro c-zeroed-local;

define macro with-c-variable
	{ with-c-variable(?c-type:expression, ?c-name:name, #key ?type:expression = #"int") ?:body end }
	=>
	{
		with-c-variable(?c-type, ?"c-name")
/// useless			let ?c-name = c-expr(?type, ?"c-name");
			?body
		end
	}

	{ with-c-variable(?c-type:expression, ?c-name:expression) ?:body end }
	=>
	{
		c-local-decl(?c-type " " ?c-name ";");
		?body;
		c-int-expr(?c-name)
	}
end macro with-c-variable;

define macro with-c-variable-and-ptr
	{
		with-c-variable-and-ptr
			(?c-type:expression, ?c-name:name, ?c-ptr-name:name,
				#key ?type:expression = #"int",
			 	?ptr-type:expression = #"ptr")
			?:body
		end
	}
	=>
	{
		with-c-variable(?c-type, ?c-name, type: ?type)
			let ?c-ptr-name = c-ptr-expr("&" ?"c-name");
			?body
		end
	}
end macro with-c-variable-and-ptr;


define class <plugin-callback>(<object>)
	constant slot opaque-pointer :: <machine-pointer>, required-init-keyword: opaque:;
end class <plugin-callback>;

standard-seals-for(<plugin-callback>);

define class <plugin-error>(<error>)
	constant slot cw-error :: <integer>, required-init-keyword: err:;
end class <plugin-error>;

standard-seals-for(<plugin-error>);

// Plan: move this CWError machinery in an extra source file in library where <file-does-not-exist-error> is defined.
// this way <file-does-not-exist-error> is sealed
define class <plugin-fnf-error>( /* <file-does-not-exist-error> , */ <plugin-error>)
end class <plugin-fnf-error>;

standard-seals-for(<plugin-fnf-error>);

define class <plugin-os-error>(<plugin-error>)
	constant slot os-error :: <integer>, required-init-keyword: os-err:;
end class <plugin-os-error>;

standard-seals-for(<plugin-os-error>);

define class <plugin-duplicate-file-error>(<plugin-os-error>)
end class <plugin-duplicate-file-error>;

standard-seals-for(<plugin-duplicate-file-error>);

//CW_CALLBACK CWGetPluginRequest(CWPluginContext context, long* request);
define function get-plugin-request(plug :: <plugin-callback>) => request :: <integer>;
	with-c-variable("long", "req")
		check(call-out("CWGetPluginRequest", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&req")));
	end
end get-plugin-request;

//CW_CALLBACK CWDonePluginRequest(CWPluginContext, CWResult resultCode);
define function done-plugin-request(plug :: <plugin-callback>, result :: <integer>) => ();
	check(call-out("CWDonePluginRequest", int:, ptr: plug.opaque-pointer.raw-value, int: result));
end done-plugin-request;

//CW_CALLBACK	CWGetAPIVersion(CWPluginContext context, long* version);
define function get-api-version(plug :: <plugin-callback>) => version :: <integer>;
	with-c-variable("long", api)
		check(call-out("CWGetAPIVersion", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&api")));
	end
end get-api-version;

// CW_CALLBACK CWGetProjectFileCount(CWPluginContext context, long* count);
define function get-project-file-count(plug :: <plugin-callback>) => version :: <integer>;
	c-local-decl("long count;");
	check(call-out("CWGetProjectFileCount", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&count")));
	c-int-expr("count")
end get-project-file-count;

define constant <file-time> = <integer>;
define constant <os-type> = <integer>;	// move into System еее -> mac-os-type.dylan %%% primitive!

// CWProjectFileInfo
define class <project-file-info>(<object>)
	constant slot project-file-spec :: <file-spec>.false-or, required-init-keyword: spec:;
	constant slot project-file-mod-date :: <file-time>, required-init-keyword: mod-date:;
	constant slot project-file-segment :: <integer>, required-init-keyword: segment:;
	constant slot project-file-has-object-code? :: <boolean>, required-init-keyword: object-code?:;
	constant slot project-file-has-resources? :: <boolean>, required-init-keyword: resources?:;
	constant slot project-file-resourcefile? :: <boolean>, required-init-keyword: resourcefile?:;
	constant slot project-file-weak-import? :: <boolean>, required-init-keyword: weak?:;
	constant slot project-file-init-before? :: <boolean>, required-init-keyword: init-before?:;
	constant slot project-file-gen-debug? :: <boolean>, required-init-keyword: debug?:;
	constant slot project-file-obj-mod-date :: <file-time>, required-init-keyword: obj-mod-date:;
	constant slot project-file-dropin-name :: <byte-string>, required-init-keyword: dropin-name:;
	constant slot project-file-id :: <integer>, required-init-keyword: id:;
	constant slot project-file-record-browse-info? :: <boolean>, required-init-keyword: browse-info?:;
	constant slot project-file-type :: <os-type>, required-init-keyword: type:;
	constant slot project-file-creator :: <os-type>, required-init-keyword: creator:;
	constant slot project-file-has-unit-data? :: <boolean>, required-init-keyword: unit-data?:;
	constant slot project-file-merge-into-output? :: <boolean>, required-init-keyword: merge?:;
	constant slot project-file-unit-data-dependency-tag :: <integer>, required-init-keyword: unit-data-dependency-tag:;
end class <project-file-info>;

standard-seals-for(<project-file-info>);

// CW_CALLBACK	CWGetFileInfo(CWPluginContext context, long whichfile, Boolean checkFileLocation, CWProjectFileInfo* fileinfo);
define function get-file-info(plug :: <plugin-callback>, file-number :: <integer>, check-loc :: <boolean>) => info :: <project-file-info>;

	local method as-number(b :: <boolean>) => i :: <integer>;
						b & 1 | 0
					end;

	local method as-bool(i :: <integer>) => b :: <boolean>;
						i ~== 0
					end;

	c-local-decl("CWProjectFileInfo fileinfo;");
	c-void-expr("memset(&fileinfo, 0, sizeof(fileinfo))");
	check(call-out("CWGetFileInfo", int:, ptr: plug.opaque-pointer.raw-value, int: file-number, int: check-loc.as-number, ptr: c-ptr-expr("&fileinfo")));

	check-loc & call-out("p2cstrcpy", void:, ptr: c-ptr-expr("fileinfo.filespec.name"), ptr: c-ptr-expr("fileinfo.filespec.name"));

	let spec = check-loc & make(<file-spec>,
													vol: c-int-expr("fileinfo.filespec.vRefNum"),
													dir: c-int-expr("fileinfo.filespec.parID"),
													name: as(<byte-string>, make(<c-string>, pointer: c-ptr-expr("fileinfo.filespec.name"))));

	make(<project-file-info>,
		spec: spec,
		mod-date: c-int-expr("fileinfo.moddate"),
		segment: c-int-expr("fileinfo.segment"),
		object-code?: c-int-expr("fileinfo.hasobjectcode").as-bool,
		resources?: c-int-expr("fileinfo.hasresources").as-bool,
		resourcefile?: c-int-expr("fileinfo.isresourcefile").as-bool,
		weak?: c-int-expr("fileinfo.weakimport").as-bool,
		init-before?: c-int-expr("fileinfo.initbefore").as-bool,
		debug?: c-int-expr("fileinfo.gendebug").as-bool,
		obj-mod-date: c-int-expr("fileinfo.objmoddate"),
		dropin-name: as(<byte-string>, make(<c-string>, pointer: c-ptr-expr("fileinfo.dropinname"))),
		id: c-int-expr("fileinfo.fileID"),
		browse-info?: c-int-expr("fileinfo.recordbrowseinfo").as-bool,
		type: c-int-expr("fileinfo.filetype"),
		creator: c-int-expr("fileinfo.filecreator"),
		unit-data?: c-int-expr("fileinfo.hasunitdata").as-bool,
		merge?: c-int-expr("fileinfo.mergeintooutput").as-bool,
		unit-data-dependency-tag: c-int-expr("fileinfo.unitdatadependencytag"));
end function get-file-info;


//#####################
//######## check ########
//####################
define generic check(result :: <integer>) => ();

define method check(result == $cw-no-error) => ();
end method check;

define method check(result :: <integer>) => ();
	make(<plugin-error>, err: result).signal;
end method check;

define method check(result == $cw-file-not-found-error) => ();
	make(<plugin-fnf-error>, err: result).signal;
end method check;

/*define method check(result == $cw-err-os-error) => ();
//	select (get-callback-os-error) have no plug here... waiting for <question>
	make(<plugin-duplicate-file-error>, err: result, os-err: -48).signal;	// this is of course big shit! еее
end method check;*/

define method check(result == $cw-request-failed-error) => ();
//	select (get-callback-os-error) have no plug here... waiting for <question>
	make(<plugin-duplicate-file-error>, err: result, os-err: -48).signal;	// this is of course big shit! еее
end method check;

// CW_CALLBACK CWGetCallbackOSError(CWPluginContext context, CWOSResult* error);
define function get-callback-os-error(plug :: <plugin-callback>) => mac-err :: <integer>;
	c-local-decl("CWOSResult error;");
	check(call-out("CWGetCallbackOSError", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&error")));
	c-int-expr("error")
end function get-callback-os-error;

// CW_CALLBACK CWSetPluginOSError(CWPluginContext context, CWOSResult);
define function set-plugin-os-error(plug :: <plugin-callback>, mac-err :: <integer>) => ();
	check(call-out("CWSetPluginOSError", int:, ptr: plug.opaque-pointer.raw-value, int: mac-err));
end function set-plugin-os-error;

// CW_CALLBACK CWGetTargetName(CWPluginContext context, char* name, short maxLength);
define function get-target-name(plug :: <plugin-callback>) => name :: <byte-string>;
	c-local-decl("char name[1024];");
	check(call-out("CWGetTargetName", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("name"), int: 1023));
	as(<byte-string>, make(<c-string>, pointer: c-ptr-expr("name")));
end;


define macro c-enumeration-definer
	{ define c-enumeration ?:name end }
		=> { define constant ?name :: <integer> = 0 }

	{ define c-enumeration ?:name = ?:expression end }
		=> { define constant ?name :: <integer> = ?expression }

	{ define c-enumeration ?:name, ?n2:name, ?rest:* end }
		=> { define constant ?name :: <integer> = 0; define c-enumeration ?n2 = ?name + 1, ?rest end }

	{ define c-enumeration ?:name = ?:expression, ?n2:name, ?rest:* end }
		=> { define constant ?name :: <integer> = ?expression; define c-enumeration ?n2 = ?name + 1, ?rest end }

	{ define c-enumeration ?:name, ?n2:name end }
		=> { define constant ?name :: <integer> = 0; define constant ?n2 = ?name + 1 }

	{ define c-enumeration ?:name = ?:expression, ?n2:name end }
		=> { define constant ?name :: <integer> = ?expression; define constant ?n2 = ?name + 1 }

	{ define c-enumeration ?:name, ?rest:* end }
		=> { define constant ?name :: <integer> = 0; define c-enumeration ?rest end }

	{ define c-enumeration ?:name = ?:expression, ?rest:* end }
		=> { define constant ?name :: <integer> = ?expression; define c-enumeration ?rest end }
end macro c-enumeration-definer;


define c-enumeration
	$cw-no-error,
	$cw-user-canceled-error,
	$cw-request-failed-error,
	$cw-err-invalid-parameter,
	$cw-err-invalid-callback,
	$cw-err-invalid-mp-callback,
	$cw-err-os-error,
	$cw-err-out-of-memory,
	$cw-file-not-found-error,
	$cw-err-unknown-file,
	$cw-err-silent,
	$cw-err-cant-set-attribute,
	$cw-err-last-common-error = 512,
	$cw-err-unknown-segment,
	$cw-err-sbm-not-found,
	$cw-err-object-file-not-stored,
	$cw-err-license-check-failed,
	$cw-err-last-compiler-linker-error = 1024
end;

define class <mem-handle>(<object>)
	slot opaque-handle :: <machine-pointer>, required-init-keyword: opaque:;
end;

standard-seals-for(<mem-handle>);

//CW_CALLBACK	CWAllocMemHandle(CWPluginContext context, long size, Boolean useTempMemory, CWMemHandle* handle);
define function alloc-mem-handle(plug :: <plugin-callback>, size :: <integer>, temp-memory? :: <boolean>) => new-hand :: <machine-pointer>;
	c-local-decl("CWMemHandle handle;");
	let temp-memory :: <integer> = temp-memory? & 1 | 0;
	check(call-out("CWAllocMemHandle", int:, ptr: plug.opaque-pointer.raw-value, int: size, int: temp-memory, ptr: c-ptr-expr("&handle")));
	let raw-result = c-ptr-expr("handle");

	as(<machine-pointer>, raw-result)
end function alloc-mem-handle;

define function new-mem-handle(plug :: <plugin-callback>, #key size :: <integer> = 0, temp-memory? :: <boolean> = #t)
	=> (new-handle :: <mem-handle>, destructor :: <function>);
	let hand = make(<cw-vector>, opaque: alloc-mem-handle(plug, size, temp-memory?), plugin: plug);
	values(hand, curry(free-mem-handle, plug, hand))
end;

//CW_CALLBACK	CWFreeMemHandle(CWPluginContext context, CWMemHandle handle);
define function free-mem-handle(plug :: <plugin-callback>, used-handle :: <mem-handle>) => ()
	check(call-out("CWFreeMemHandle", int:, ptr: plug.opaque-pointer.raw-value, ptr: used-handle.opaque-handle.raw-value));
end;


define class <new-text-document-info>(<object>)
	slot document-name :: false-or(<byte-string>), required-init-keyword: name:;
	slot initial-text :: <mem-handle>, required-init-keyword: text:;
	slot mark-dirty? :: <boolean>, required-init-keyword: dirty:;
end;

standard-seals-for(<new-text-document-info>);

define function make-text-document-info(initial-text :: <mem-handle>, document-name :: false-or(<byte-string>), #key dirty? :: <boolean>) => new :: <new-text-document-info>;
	make(<new-text-document-info>, name: document-name, text: initial-text, dirty: dirty?)
end;

//CW_CALLBACK CWCreateNewTextDocument(CWPluginContext, const CWNewTextDocumentInfo* docinfo);
define function create-new-text-document(plug :: <plugin-callback>, info :: <new-text-document-info>) => ();
	c-local-decl("CWNewTextDocumentInfo docinfo;");
	let doc-name :: <c-string> = as(<c-string>, info.document-name);

	c-void-expr("memset(&docinfo, 0, sizeof(docinfo))");
	call-out("docinfo.documentname = ", void:, ptr: doc-name.raw-value);
	call-out("docinfo.text = ", void:, ptr: info.initial-text.opaque-handle.raw-value);
//	c-assign("docinfo.text", ptr: info.initial-text.opaque-handle.raw-value);
	let mark-dirty = mark-dirty? & 1 | 0;
//	let mark-dirty :: <integer> = if (mark-dirty?) 1 else 0 end if;
	call-out("docinfo.markDirty = ", void:, int: mark-dirty);
	check(call-out("CWCreateNewTextDocument", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&docinfo")));
end function create-new-text-document;

define class <file-spec>(<object>)
	slot volume-ref :: <integer>, required-init-keyword: vol:;
	slot directory-ref :: <integer>, required-init-keyword: dir:;
	slot spec-file-name :: <byte-string>, required-init-keyword: name:;
end class <file-spec>;

standard-seals-for(<file-spec>);

//CW_CALLBACK	CWGetProjectFile(CWPluginContext context, CWFileSpec* projectSpec);
define function get-project-file(plug :: <plugin-callback>) => project-spec :: <file-spec>;
	c-local-decl("CWFileSpec spec;");
	let i = call-out("CWGetProjectFile", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&spec"));
	check(i);
	make(<file-spec>,
		vol: c-int-expr("spec.vRefNum"),
		dir: c-int-expr("spec.parID"),
		name: as(<byte-string>, as(<c-string>, c-ptr-expr("spec.name"))));
end;

//CW_CALLBACK	CWReleaseFileText(CWPluginContext context, const char* text);
define function release-file-text(plug :: <plugin-callback>, file-text :: <machine-pointer>) => ();
	check(call-out("CWReleaseFileText", int:, ptr: plug.opaque-pointer.raw-value, ptr: file-text.raw-value));
end;

//CW_CALLBACK	CWUserBreak(CWPluginContext context);
define function user-break(plug :: <plugin-callback>) => ();
	check(call-out("CWUserBreak", int:, ptr: plug.opaque-pointer.raw-value));
end;

//CW_CALLBACK	CWAlert(CWPluginContext context, const char* msg1, const char* msg2, const char* msg3, const char* msg4);
define function alert(plug :: <plugin-callback>, msg1 :: <byte-string>, msg2 :: <byte-string>, msg3 :: <byte-string>, msg4 :: <byte-string>) => ();
//еее	check(call-out("CWAlert", int:, ptr: plug.opaque-pointer.raw-value, ptr: msg1.raw-value, ptr: msg2.raw-value, ptr: msg3.raw-value, ptr: msg4.raw-value));
end;

//CW_CALLBACK	CWUnlockMemHandle(CWPluginContext context, CWMemHandle handle);
define function unlock-mem-handle(plug :: <plugin-callback>, handle :: <mem-handle>) => ();
	check(call-out("CWUnlockMemHandle", int:, ptr: plug.opaque-pointer.raw-value, ptr: handle.opaque-handle.raw-value));
end;

//CW_CALLBACK	CWResizeMemHandle(CWPluginContext context, CWMemHandle handle, long newSize);
define function resize-mem-handle(plug :: <plugin-callback>, handle :: <mem-handle>, new-size :: <integer>) => ();
	check(call-out("CWResizeMemHandle", int:, ptr: plug.opaque-pointer.raw-value, ptr: handle.opaque-handle.raw-value, int: new-size));
end;

//CW_CALLBACK CWMacOSErrToCWResult(CWPluginContext context, OSErr err);
define function macos-err-to-cw-result(plug :: <plugin-callback>, mac-err :: <integer>) => cw-result :: <integer>;
	call-out("CWMacOSErrToCWResult", int:, ptr: plug.opaque-pointer.raw-value, short: mac-err);
end;

//CW_CALLBACK	CWLockMemHandle(CWPluginContext context, CWMemHandle handle, Boolean moveHi, void** ptr);
define function lock-mem-handle(plug :: <plugin-callback>, handle :: <mem-handle>, #key move-hi :: <boolean>) => locked :: <machine-pointer>;
	let stor = call-out("__alloca", ptr:, int: c-int-expr("sizeof(void*)"));
	check(call-out("CWLockMemHandle", int:, ptr: plug.opaque-pointer.raw-value, ptr: handle.opaque-handle.raw-value, int: 0/*еее*/, ptr: stor));
	pointer-at(as(<machine-pointer>, stor), class: <machine-pointer>)
end;

//CW_CALLBACK	CWGetMemHandleSize(CWPluginContext context, CWMemHandle handle, long* size);
define function get-mem-handle-size(plug :: <plugin-callback>, handle :: <mem-handle>) => current-size :: <integer>;
	with-c-variable-and-ptr("Size", currSize, currSize-ptr)
		check(call-out("CWGetMemHandleSize", int:, ptr: plug.opaque-pointer.raw-value, ptr: handle.opaque-handle.raw-value, ptr: currSize-ptr));
	end
end;

define class <cw-vector>(<stretchy-vector>, <string>, <mem-handle>)
	slot dereferenced :: false-or(<machine-pointer>) = #f;
	slot vector-plugin :: <plugin-callback>, required-init-keyword: plugin:;
end;

standard-seals-for(<cw-vector>);

/* I get a strange error if initialize defined in conjuncton with "init-keyword: opaque:;" еее
define method initialize(class == <cw-vector>, #next next-method, #rest rest, #key size = 0, opaque = #f, plugin, #all-keys) => (#rest objects);
	apply(next-method, size: size, opaque: opaque | alloc-mem-handle(plugin, size, #f), plugin: plugin, rest);

// fill not yet supported!еее
end method initialize;
*/

define method size (vec :: <cw-vector>) => size :: <integer>;
	vec.dereferenced & (unlock-mem-handle(vec.vector-plugin, vec) | (vec.dereferenced := #f));
	get-mem-handle-size(vec.vector-plugin, vec);
end method size;

define method size-setter(new-size :: <integer>, vec :: <cw-vector>) => new-size :: <integer>;
	vec.dereferenced & (unlock-mem-handle(vec.vector-plugin, vec) | (vec.dereferenced := #f));
	resize-mem-handle(vec.vector-plugin, vec, new-size);
	new-size
end size-setter;

define function element-error(coll :: <sequence>, index :: <integer>)	// from vector.dylan --- make it exported?
	=> res :: <never-returns>;
	error("No element %d in %=", index, coll);
end function;

define method element(vec :: <cw-vector>, index :: <integer>, #key default = #f) => element :: <object>; // because of default!  was: <byte-character>;
	case
		index < 0 | index >= get-mem-handle-size(vec.vector-plugin, vec) => default | element-error(vec, index);
		otherwise =>
			vec.dereferenced | (vec.dereferenced := lock-mem-handle(vec.vector-plugin, vec));
			as(<byte-character>, unsigned-byte-at(vec.dereferenced, offset: index))
	end case
end element;

define method element-setter(new-value :: <byte-character>, vec :: <cw-vector>, index :: <integer>) => new-value :: <byte-character>;
	vec.dereferenced | (vec.dereferenced := lock-mem-handle(vec.vector-plugin, vec));
	unsigned-byte-at-setter(as(<integer>, new-value), vec.dereferenced, offset: index);
	new-value
end method element-setter;

define method type-for-copy (vec :: <cw-vector>) => type :: <cw-vector>.singleton;
  <cw-vector>	// this will not work because there are unsupplied keyword args to make...
end method type-for-copy;

define method add!(vec :: <cw-vector>, new-elt :: <byte-character>) => changed-vec :: <cw-vector>;
	vec.size := vec.size + 1;
	vec.last := new-elt;
	vec
end method add!;

define method as(clas == <byte-string>, vec :: <cw-vector>) => string :: <byte-string>;
	map-as(<byte-string>, identity, vec)	// use memcpy?еее
end;

define method starting-address (vec :: <cw-vector>) => start :: <raw-pointer>;
	vec.dereferenced | (vec.dereferenced := lock-mem-handle(vec.vector-plugin, vec));
	vec.dereferenced.raw-value
end method;

define function free-vector-storage(vec :: <cw-vector>) => ();
	free-mem-handle(vec.vector-plugin, vec);
end function free-vector-storage;


/// Compiler API

//CW_CALLBACK	CWDisplayLines(CWPluginContext context, long nlines);
define function display-lines(plug :: <plugin-callback>, count :: <integer>) => ();
	check(call-out("CWDisplayLines", int:, ptr: plug.opaque-pointer.raw-value, int: count));
end;

//CW_CALLBACK CWIsPrecompiling(CWPluginContext context, Boolean* isPrecompiling);
define function precompiling?(plug :: <plugin-callback>) => precompiling? :: <boolean>;
	c-local-decl("Boolean isPrecompiling;");
	check(call-out("CWIsPrecompiling", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&isPrecompiling")));
	c-int-expr("isPrecompiling") ~== 0
end function precompiling?;

// CW_CALLBACK CWIsAutoPrecompiling(CWPluginContext context, Boolean* isAutoPrecompiling);
define function auto-precompiling?(plug :: <plugin-callback>) => auto-precompiling? :: <boolean>;
	c-void-expr("{ Boolean isAutoPrecompiling;");
	check(call-out("CWIsAutoPrecompiling", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&isAutoPrecompiling")));
	c-int-expr("isAutoPrecompiling; }") ~== 0
end function auto-precompiling?;

// CW_CALLBACK CWIsPreprocessing(CWPluginContext context, Boolean* isPreprocessing);
define function preprocessing?(plug :: <plugin-callback>) => preprocessing? :: <boolean>;
	c-void-expr("{ Boolean isPreprocessing;");
	check(call-out("CWIsPreprocessing", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&isPreprocessing")));
	c-int-expr("isPreprocessing; }") ~== 0
end function preprocessing?;

// CW_CALLBACK CWIsGeneratingDebugInfo(CWPluginContext context, Boolean* isGenerating);
define function generating-debug-info?(plug :: <plugin-callback>) => generating-debug-info? :: <boolean>;
	c-void-expr("{ Boolean isGenerating;");
	check(call-out("CWIsGeneratingDebugInfo", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&isGenerating")));
	c-int-expr("isGenerating; }") ~== 0
end function generating-debug-info?;

// CW_CALLBACK CWIsCachingPrecompiledHeaders(CWPluginContext context, Boolean* isCaching);
define function caching-precompiled-headers?(plug :: <plugin-callback>) => caching-precompiled-headers? :: <boolean>;
	c-void-expr("{ Boolean isCaching;");
	check(call-out("CWIsCachingPrecompiledHeaders", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&isCaching")));
	c-int-expr("isCaching; }") ~== 0
end function caching-precompiled-headers?;

//CW_CALLBACK CWGetMainFileNumber(CWPluginContext context, long* fileNumber);
define function get-main-file-number(plug :: <plugin-callback>) => main-file-number :: <integer>;
	with-c-variable-and-ptr("long", fileNumber, fileNumber-ptr)
		check(call-out("CWGetMainFileNumber", int:, ptr: plug.opaque-pointer.raw-value, ptr: fileNumber-ptr));
	end	// comment this out and report nice ICEеее
end function get-main-file-number;

//CW_CALLBACK CWGetMainFileID(CWPluginContext context, short* fileID);
define function get-main-file-id(plug :: <plugin-callback>) => main-file-number :: <integer>;
	with-c-variable-and-ptr("short", fileID, fileID-ptr)
		check(call-out("CWGetMainFileID", int:, ptr: plug.opaque-pointer.raw-value, ptr: fileID-ptr));
	end
end function get-main-file-id;

//CW_CALLBACK CWGetMainFileSpec(CWPluginContext context, CWFileSpec* fileSpec);
define function get-main-file-spec(plug :: <plugin-callback>) => spec :: <file-spec>;
	c-local-decl("CWFileSpec spec;");
	check(call-out("CWGetMainFileSpec", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&spec")));
	c-void-expr("p2cstrcpy((char*)spec.name, spec.name)");
	make(<file-spec>,
		vol: c-int-expr("spec.vRefNum"),
		dir: c-int-expr("spec.parID"),
		name: as(<byte-string>, as(<c-string>, c-ptr-expr("spec.name"))));
end function get-main-file-spec;

//CW_CALLBACK CWGetMainFileText(CWPluginContext context, const char** text, long* textLength);
define function get-main-file-text(plug :: <plugin-callback>) => (text :: <machine-pointer>, len :: <integer>);
	c-local-decl("char* text;");
	c-local-decl("long textLength;");
	check(call-out("CWGetMainFileText", int:, ptr: plug.opaque-pointer.raw-value, ptr: c-ptr-expr("&text"), ptr: c-ptr-expr("&textLength")));
	values(as(<machine-pointer>, c-ptr-expr("text")), c-int-expr("textLength"));
end;


define c-enumeration
	$message-type-info,
	$message-type-warning,
	$message-type-error
end c-enumeration;


//CW_CALLBACK	CWReportMessage(CWPluginContext context, const CWMessageRef* msgRef, const char *line1, const char *line2, short errorlevel, long errorNumber);
define function report-message(plug :: <plugin-callback>, in-file :: <file-spec>.false-or, error-string :: <byte-string>, line-number :: <integer>, error-level :: <integer>, error-number :: <integer>, #key line-2 :: <byte-string> = "") => ();
	c-zeroed-local("CWMessageRef", "msgRef");

	if (in-file)
		call-out("msgRef.sourcefile.vRefNum = ", void:, int: in-file.volume-ref);
		call-out("msgRef.sourcefile.parID = ", void:, int: in-file.directory-ref);
		call-out("c2pstrcpy", void:, ptr: c-ptr-expr("&msgRef.sourcefile.name"), ptr: as(<c-string>, in-file.spec-file-name).raw-value);
	end if;
	
	call-out("msgRef.linenumber = ", void:, int: line-number);
	check(call-out("CWReportMessage", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: c-ptr-expr("&msgRef"),
					ptr: as(<c-string>, error-string).raw-value,
					ptr: as(<c-string>, line-2).raw-value,
					int: error-level,
					int: error-number));
end function report-message;

define c-enumeration
	$no-dependency,
	$normal-dependency,
	$interface-dependency
end;


define function cw-find-file(plug :: <plugin-callback>, file-name :: <byte-string>, #key	full-search :: <boolean>) => file-spec :: <file-spec>;

	let (	file-data :: <machine-pointer>,
		file-data-length :: <integer>,
		file-data-type :: <integer>,
		file-id :: <integer>,
		file-spec :: <file-spec>)
	= cw-find-and-load-file(plug, file-name, full-search: full-search, suppress-load: #t, want-file-spec: #t);
	
	file-spec
end function cw-find-file;

define function cw-maybe-find-file(plug :: <plugin-callback>, file-name :: <byte-string>, #key	full-search: full-search? :: <boolean>) => file-spec-or-false :: <file-spec>.false-or;
	block ()
		cw-find-file(plug, file-name, full-search: full-search?)
	exception(<plugin-fnf-error>)
	end block;
end function cw-maybe-find-file;


define constant $current-compiled-file = -1;

//CW_CALLBACK	CWFindAndLoadFile(CWPluginContext context, const char* filename, CWFileInfo *fileinfo);
define function cw-find-and-load-file
		(	plug :: <plugin-callback>,
			file-name :: <byte-string>,
			#key
				full-search :: <boolean>,
				dependency-type :: <integer> = $no-dependency,
				is-dependent-of-file :: <integer> = $current-compiled-file,
				suppress-load :: <boolean>,
				want-file-spec :: <boolean>)
			=> (	file-data :: <machine-pointer>,
					file-data-length :: <integer>,
					file-data-type :: <integer>,
					file-id :: <integer>,
					file-spec :: <file-spec>.false-or,
					already-included :: <boolean>,
					record-browse-info :: <boolean>);

	c-zeroed-local("CWFileInfo", "fileinfo");

	local method as-number(b :: <boolean>) => i :: <integer>;
						b & 1 | 0
					end;

	call-out("fileinfo.fullsearch = ", void:, int: full-search.as-number);
	call-out("fileinfo.dependencyType = ", void:, int: dependency-type);
	call-out("fileinfo.isdependentoffile = ", void:, int: is-dependent-of-file);
	call-out("fileinfo.suppressload = ", void:, int: suppress-load.as-number);

	check(call-out("CWFindAndLoadFile", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: as(<c-string>, file-name).raw-value,
					ptr: c-ptr-expr("&fileinfo")));

	want-file-spec & call-out("p2cstrcpy", void:, ptr: c-ptr-expr("fileinfo.filespec.name"), ptr: c-ptr-expr("fileinfo.filespec.name"));

/*	let (	file-data :: <machine-pointer>,			// еее see get-file-info where this workaround is not needed for some reason...
			file-data-length :: <integer>,
			file-data-type :: <integer>,
			file-id :: <integer>,
			file-spec :: <file-spec>.false-or,
			already-included :: <boolean>,
			record-browse-info :: <boolean>)
		= */
		
		values(as(<machine-pointer>, c-ptr-expr("(void*)fileinfo.filedata")),
				c-int-expr("fileinfo.filedatalength"),
				c-int-expr("fileinfo.filedatatype"),
				c-int-expr("fileinfo.fileID"),
				want-file-spec & make(<file-spec>,
														vol: c-int-expr("fileinfo.filespec.vRefNum"),
														dir: c-int-expr("fileinfo.filespec.parID"),
														name: as(<byte-string>, as(<c-string>, c-ptr-expr("fileinfo.filespec.name")))),
				c-int-expr("fileinfo.alreadyincluded") ~== 0,
				c-int-expr("fileinfo.recordbrowseinfo;") ~== 0);

//	values(	file-data, file-data-length, file-data-type, file-id, file-spec, already-included, record-browse-info)
end function cw-find-and-load-file;



/*
use define method export-string (string :: <byte-string>)
 => ptr :: <raw-pointer>;
from dylan-viscera??? (system.dylan)
*/


define class <cw-file-stream>(<sequence-stream>, <buffered-stream>)
	constant slot buffer :: <buffer>, required-init-keyword: buffer:;
	constant slot stream-element-type :: <type> = <byte-character>, init-keyword: element-type:;
end class <cw-file-stream>;

standard-seals-for(<cw-file-stream>);

// made it a <buffered-stream> because of these three methods:

define sealed method do-get-input-buffer(stream :: <cw-file-stream>, #key wait? :: <boolean> = #t, bytes :: false-or(<integer>))
		=> buffer :: false-or(<buffer>);
	stream.buffer;
end method do-get-input-buffer;

define sealed method do-release-input-buffer(stream :: <cw-file-stream>) => ();
end method do-release-input-buffer;

define sealed method close(stream :: <cw-file-stream>, #key, #all-keys)  => ();
  // Get the buffer to make sure no one is using it.
  get-input-buffer(stream, bytes: 0);
  release-input-buffer(stream);
end method;

// Still better, define a <positionable-stream> that lives on the file-data
// does not copy around, and calls release-file-text on close-stream!
// but this will not work since there must be a buffer!

// also define a <cw-plugin-mixin>


define method stream-contents(stream :: <cw-file-stream>, #key clear-contents? :: <boolean>) => contents :: <sequence>;
	clear-contents? & "immutable stream".error;
	stream.buffer;
end method stream-contents;

// factor out the filling up of __alloca storage with the <file-spec>!

define function check-mac-os(plug :: <plugin-callback>, mac-err :: <integer>) => ();
	set-plugin-os-error(plug, mac-err);
	macos-err-to-cw-result(plug, mac-err).check
end function check-mac-os;

define function load-file-in-buffer(plug :: <plugin-callback>, spec :: <file-spec>) => filled-buffer :: <buffer>;
	c-zeroed-local("CWFileSpec", "spec");
	c-local-decl("short refNum;");
	c-local-decl("long logEOF;");

	call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec.parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: as(<c-string>, spec.spec-file-name).raw-value);

	check-mac-os(plug, call-out("FSpOpenDF", int:, ptr: c-ptr-expr("&spec"), int: c-int-expr("fsRdPerm"), ptr: c-ptr-expr("&refNum")));
	let err :: <integer> = call-out("GetEOF", int:, int: c-int-expr("refNum"), ptr: c-ptr-expr("&logEOF"));
	let b = #f;
	if (err == 0)
		b := make(<buffer>, size: c-int-expr("logEOF"), end: c-int-expr("logEOF"));
		err := call-out("FSRead", int:, int: c-int-expr("refNum"), ptr: c-ptr-expr("&logEOF"), ptr: b.buffer-address);
	end if;
	let err2 :: <integer> = call-out("FSClose", int:, int: c-int-expr("refNum"));
	
	check-mac-os(plug, err);
	check-mac-os(plug, err2);
	b
end;

define c-enumeration
	$cw-file-type-unknown,
	$cw-file-type-text,
	$cw-file-type-precompiled-header
end c-enumeration;

define function find-and-load-file-glue(cb :: <machine-pointer>, file-name :: <byte-string>, search-paths :: <sequence>)
		=> (stream :: false-or(<stream>), found-loc :: false-or(<byte-string>));

	let plugin = make(<plugin-callback>, opaque: cb);

	block()	// this must be made *much* more effective еее
		let (	file-data :: <machine-pointer>,
				file-data-length :: <integer>,
				file-data-type :: <integer>,
				file-id :: <integer>,
				file-spec :: <file-spec>.false-or,
				already-included :: <boolean>,
				record-browse-info :: <boolean>)
			= cw-find-and-load-file(plugin, file-name, want-file-spec: #t, dependency-type: $normal-dependency);
		
		let b =
			select (file-data-type)
			$cw-file-type-unknown => load-file-in-buffer(plugin, file-spec);
			otherwise =>
	//		let bv = as(<byte-vector>, file-data);
				let b = make(<buffer>, size: file-data-length, end: file-data-length);
				call-out("BlockMoveData", void:, ptr: file-data.raw-value, ptr: b.buffer-address, int: file-data-length);
	//		copy-into-buffer!();
	//		copy-bytes(); // as(<byte-vector>)?
				b
			end select;

		show-status(plugin, concatenate("Loading unit: ", file-name));
		plugin.user-break;

		values(make(<cw-file-stream>, buffer: b), file-spec.spec-file-name);
	//cleanup
		// еее release-file-text(plugin, file-data);	// only if not NULL
	exception ( /* should be: <file-does-not-exist-error> */ <plugin-fnf-error>)
		report-message(plugin, #f /* in-file */, concatenate("Unit file '", file-name, "' not found"), /* line-number :: <integer> */ 0, $message-type-error, /* error-number */ 0 /*, #key line-2 :: <byte-string> = ""*/)
	end block;

end function find-and-load-file-glue;



define constant $default-link-position :: <integer> = -1;

define class <cw-new-project-entry-info>(<object>)
	constant slot entry-position :: <integer> = $default-link-position, init-keyword: position:;
	constant slot entry-segment :: <integer> = $default-link-position, init-keyword: segment:;
	constant slot entry-overlay-group :: <integer> = $default-link-position, init-keyword: overlay-group:;
	constant slot entry-overlay :: <integer> = $default-link-position, init-keyword: overlay:;
	constant slot group-path :: <byte-string> = "", init-keyword: group-path:;
	constant slot merge-into-output :: <boolean> = #f, init-keyword: merge:;
	constant slot weak-import :: <boolean> = #f, init-keyword: weak:;
	constant slot init-before :: <boolean> = #f, init-keyword: init-before:;
end class <cw-new-project-entry-info>;

standard-seals-for(<cw-new-project-entry-info>);

// CW_CALLBACK CWAddProjectEntry(CWPluginContext context, const CWFileSpec* fileSpec, Boolean isGenerated, const CWNewProjectEntryInfo* projectEntryInfo, long* whichfile);
define function add-project-entry
	(	plug :: <plugin-callback>,
		spec :: <file-spec>,
		is-generated :: <boolean>,
		info :: <cw-new-project-entry-info>.false-or)
	=> which-file :: <integer>;

	c-zeroed-local("CWFileSpec", "spec");
	c-zeroed-local("CWNewProjectEntryInfo", "info");

	let (spec-file-name :: <c-string>, group-path :: <c-string>.false-or)
		= values(as(<c-string>, spec.spec-file-name), info & as(<c-string>, info.group-path));

	with-c-variable("long", "whichfile")
		call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
		call-out("spec.parID = ", void:, int: spec.directory-ref);
		call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: spec-file-name.raw-value);
		
		local method as-number(b :: <boolean>) => i :: <integer>;
							b & 1 | 0
						end;

		if (info)
			call-out("info.position = ", void:, int: info.entry-position);
			call-out("info.segment = ", void:, int: info.entry-segment);
			call-out("info.overlayGroup = ", void:, int: info.entry-overlay-group);
			call-out("info.overlay = ", void:, int: info.entry-overlay);
			call-out("info.groupPath = ", void:, ptr: group-path.raw-value);
			call-out("info.mergeintooutput = ", void:, int: info.merge-into-output.as-number);
			call-out("info.weakimport = ", void:, int: info.weak-import.as-number);
			call-out("info.initbefore = ", void:, int: info.init-before.as-number);
		else
			call-out("info.position = ", void:, int: $default-link-position);
			call-out("info.segment = ", void:, int: $default-link-position);
			call-out("info.overlayGroup = ", void:, int: $default-link-position);
			call-out("info.overlay = ", void:, int: $default-link-position);
		end if;
		
		check(call-out("CWAddProjectEntry", int:,
						ptr: plug.opaque-pointer.raw-value,
						ptr: c-ptr-expr("&spec"),
						int: is-generated.as-number,
						ptr: if (info) c-ptr-expr("&info") else c-ptr-expr("0") end if,
						ptr: c-ptr-expr("&whichfile")));
	end with-c-variable
end function add-project-entry;


// CW_CALLBACK	CWShowStatus(CWPluginContext context, const char *line1, const char *line2);
define function show-status(plug :: <plugin-callback>, line-1 :: <byte-string>, #key line-2 :: <byte-string> = "") => ();
	let (line-1 :: <c-string>, line-2 :: <c-string>) = values(as(<c-string>, line-1), as(<c-string>, line-2));

	check(call-out("CWShowStatus", int:,
							ptr: plug.opaque-pointer.raw-value,
							ptr: line-1.raw-value,
							ptr: line-2.raw-value));
end function show-status;

// used to signal non-file specific data in CWStore/GetPluginData
define constant $target-global-plugin-data :: <integer> = -1;

//CW_CALLBACK	CWStorePluginData(CWPluginContext context, long whichfile, CWDataType type, CWMemHandle prefsdata);
define function store-plugin-data(plug :: <plugin-callback>, which-file :: <integer>, type :: <integer>, prefs-data :: <mem-handle>) => ();
	check(call-out("CWStorePluginData", int:,
							ptr: plug.opaque-pointer.raw-value,
							int: which-file,
							int: type,
							ptr: prefs-data.opaque-handle.raw-value));
end function store-plugin-data;


//CW_CALLBACK	CWGetPluginData(CWPluginContext context, long whichfile, CWDataType type, CWMemHandle* prefsdata);
define function get-plugin-data(plug :: <plugin-callback>, which-file :: <integer>, type :: <integer>) => prefs-data :: <mem-handle>;
	c-void-expr("{ CWMemHandle prefsdata");
	check(call-out("CWGetPluginData", int:,
							ptr: plug.opaque-pointer.raw-value,
							int: which-file,
							int: type,
							ptr: c-ptr-expr("&prefsdata")));
	let raw-result :: <raw-pointer> = c-ptr-expr("prefsdata");
	c-void-expr("}");

	make(<mem-handle>, opaque: as(<machine-pointer>, raw-result));
end function get-plugin-data;


define function create-file(contents :: <cw-vector>, spec :: <file-spec>, #key binary? :: <boolean>) => ();
	c-zeroed-local("CWFileSpec", "spec");
	c-local-decl("short refNum;");
	c-local-decl("long count;");

	let vec-size :: <integer> = contents.size;
	call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec.parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: as(<c-string>, spec.spec-file-name).raw-value);

	let plug :: <plugin-callback> = contents.vector-plugin;
	let file-type :: <integer> = binary? & c-int-expr(" 'BINA' ") | c-int-expr(" 'TEXT' ");
//	let file-type :: <integer> = binary? & c-int-literal("BINA") | c-int-literal("TEXT");

//	check-mac-os(plug, call-out("FSpCreate", int:, ptr: c-ptr-expr("&spec"), int: c-int-literal("CWIE"), int: file-type, int: c-int-expr("smRoman")));
	check-mac-os(plug, call-out("FSpCreate", int:, ptr: c-ptr-expr("&spec"), int: c-int-expr(" 'CWIE' "), int: file-type, int: c-int-expr("smRoman")));
	check-mac-os(plug, call-out("FSpOpenDF", int:, ptr: c-ptr-expr("&spec"), int: c-int-expr("fsWrPerm"), ptr: c-ptr-expr("&refNum")));
	call-out("count = ", void:, int: vec-size);
	let err  :: <integer> = call-out("FSWrite", int:, int: c-int-expr("refNum"), ptr: c-ptr-expr("&count"), ptr: contents.starting-address);
	err.zero? & (err := call-out("SetEOF", int:, int: c-int-expr("refNum"), int: c-int-expr("count")));

	let err2 :: <integer> = call-out("FSClose", int:, int: c-int-expr("refNum"));
	
	check-mac-os(plug, err);
	check-mac-os(plug, err2);
end function create-file;

// We need to check if there are 'ckid' resources inside

define function write-file(contents :: <cw-vector>, spec :: <file-spec>) => mod-date :: <file-time>;
	let vec-size :: <integer> = contents.size;
// factor out common stuff -- see also create-file еее
	c-zeroed-local("CWFileSpec", "spec");
	c-local-decl("short refNum;");
	c-local-decl("long count;");

	call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec.parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: as(<c-string>, spec.spec-file-name).raw-value);

	let plug :: <plugin-callback> = contents.vector-plugin;

	check-mac-os(plug, call-out("FSpOpenDF", int:, ptr: c-ptr-expr("&spec"), int: c-int-expr("fsWrPerm"), ptr: c-ptr-expr("&refNum")));
	call-out("count = ", void:, int: vec-size);
	let err  :: <integer> = call-out("FSWrite", int:, int: c-int-expr("refNum"), ptr: c-ptr-expr("&count"), ptr: contents.starting-address);
	err.zero? & (err := call-out("SetEOF", int:, int: c-int-expr("refNum"), int: c-int-expr("count")));
	let err2 :: <integer> = call-out("FSClose", int:, int: c-int-expr("refNum"));
	
	check-mac-os(plug, err);
	check-mac-os(plug, err2);

	c-zeroed-local("HFileParam", "pb");
	call-out("pb.ioVRefNum = ", void:, int: spec.volume-ref);
	call-out("pb.ioDirID = ", void:, int: spec.directory-ref);
	call-out("pb.ioNamePtr = ", void:, ptr: c-ptr-expr("spec.name"));
	check-mac-os(plug, call-out("PBHGetFInfoSync", int:, ptr: c-ptr-expr("&pb")));
	c-int-expr("pb.ioFlMdDat");
end function write-file;

// CW_CALLBACK	CWSetModDate(CWPluginContext context, const CWFileSpec* filespec, CWFileTime* moddate, Boolean isGenerated);
define function set-mod-date(plug :: <plugin-callback>, spec :: <file-spec>, mod-date :: <file-time>, is-generated :: <boolean>) => ();
	c-zeroed-local("CWFileSpec", "spec");
	c-local-decl("CWFileTime moddate;");

	let spec-file-name :: <c-string> = as(<c-string>, spec.spec-file-name);

	call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec.parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: spec-file-name.raw-value);
	call-out("moddate = ", void:, int: mod-date);
	
	local method as-number(b :: <boolean>) => i :: <integer>;
						b & 1 | 0
					end;
	
	check(call-out("CWSetModDate", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: c-ptr-expr("&spec"),
					ptr: c-ptr-expr("&moddate"),
					int: is-generated.as-number));
	
//	mod-date	// does the result interest us?е

end function set-mod-date;

// CW_CALLBACK CWPreFileAction(CWPluginContext context, const CWFileSpec *theFile);
define function pre-file-action(plug :: <plugin-callback>, spec :: <file-spec>) => ();
	c-zeroed-local("CWFileSpec", "spec");
	let spec-file-name :: <c-string> = as(<c-string>, spec.spec-file-name);
	call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec.parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: spec-file-name.raw-value);

	check(call-out("CWPreFileAction", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: c-ptr-expr("&spec")));
end function pre-file-action;


// CW_CALLBACK CWPostFileAction(CWPluginContext context, const CWFileSpec *theFile);
define function post-file-action(plug :: <plugin-callback>, spec :: <file-spec>) => ();
	c-zeroed-local("CWFileSpec", "spec");

	let spec-file-name :: <c-string> = as(<c-string>, spec.spec-file-name);
	call-out("spec.vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec.parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: spec-file-name.raw-value);

	check(call-out("CWPostFileAction", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: c-ptr-expr("&spec")));

/*	cw-call-out("CWPostFileAction", plug,	еее			--- see below... ICE must be reported
							ptr: c-ptr-expr("&spec"));	*/
end function post-file-action;

define macro cw-call-out
	{ cw-call-out(?routine:expression, ?plug:expression, ?stuff:*) } => { check(call-out(routine, int:, ptr:, ?plug.opaque-pointer.raw-value, ?stuff)) }
end macro cw-call-out;


/*
causes this ICE: еее

No applicable methods in call of {GF build-defn-ref} when given arguments:
  #[{<internal-fer-builder> instance},
    {<policy> 0x01619cb0,
       speed: 1.0s0,
       space: 1.0s0,
       safety: 1.0s0,
       brevity: 1.0s0,
       debug: 1.0s0},
    {<file-source-location> 0x03084760,
       source-file: {<source-file> 0x0285d5e0, name: "plugin-api.dylan"},
       start-line: 923,
       start-column: 1,
       end-line: 923,
       end-column: 12},
    {<function-macro-definition> 0x030ebd80,
       name:
         {<basic-name> 0x0282ca38,
            symbol: #"cw-call-out",
            module: {<module> 0x02ee1a00, name: #"plugin-api"}}}]
*/

define c-enumeration
	$cw-req-initialize = -2,
	$cw-req-terminate,
	
	$cw-req-compile = 0,
	$cw-req-make-parse,
	$cw-req-comp-disassemble,
	$cw-req-check-syntax,
	
	$cw-req-link = 0,
	$cw-req-disassemble,
	$cw-req-target-info,
	$cw-req-pre-run,
	
	$cw-req-idle = -100,
	$cw-req-about = -101,
	$cw-req-prefs-change = -102,
	
	$cw-req-target-link-ended = -15,
	$cw-req-target-link-started,
	$cw-req-file-list-build-ended,
	$cw-req-file-list-build-started,
	$cw-req-sub-project-build-ended,
	$cw-req-sub-project-build-started,
	$cw-req-target-build-ended,
	$cw-req-target-build-started,
	$cw-req-project-build-ended,
	$cw-req-project-build-started,
	$cw-req-target-loaded,
	$cw-req-target-prefs-changed,
	$cw-req-target-unloaded
end c-enumeration;


/// design note:
// <object-data> should be a sequence of hunks
// and should be able to compute a name table

define class <object-data>(<object>)	// bad design! use keyword parameters instead!
	slot object-data :: <mem-handle>, required-init-keyword: object-data:;
	slot browse-data :: <mem-handle>.false-or = #f;
	slot uninitialized-data-size :: <integer> = 0;
	slot initialized-data-size :: <integer> = 0;
	slot compiled-lines :: <integer> = 0;
	slot interface-changed :: <boolean> = #f;
	slot dependencies :: <sequence>.false-or = #f;
end;

standard-seals-for(<object-data>);

define c-enumeration
	obj-is-shared-library = #x0001,
	obj-is-library = #x0002,
	obj-is-pascal = #x0004,
	obj-is-weak = #x0008,
	obj-is-init-before = #x0010
end;

define c-enumeration // obsolete... see gwydion.dylan
	hunk-start = #x4567,
	hunk-end
end;

define class <ppc-object-data>(<object-data>)
	slot ppc-object-data-flags :: <integer> = 0;
	slot old-def-version :: <integer> = 0;
	slot old-imp-version :: <integer> = 0;
	slot current-version :: <integer> = 0;
end;

standard-seals-for(<ppc-object-data>);

define function fill-object-data-header
  (plug :: <plugin-callback>,
   data :: <ppc-object-data>,
   obj-provider :: <function>.false-or) => ()
	c-decl(	"typedef struct ObjHeader {"
				"SInt32 magic_word;"
				"SInt16 version;"
				"SInt16 flags;"
				"SInt32 obj_size;"
				"SInt32 nametable_offset;"
				"SInt32 nametable_names;"
				"SInt32 symtable_offset;"
				"SInt32 symtable_size;"
				"SInt32 code_size;"
				"SInt32 udata_size;"
				"SInt32 idata_size;"
				"SInt32 toc_size;"
				"SInt32 old_def_version;"
				"SInt32 old_imp_version;"
				"SInt32 current_version;"
				"SInt32 reserved[13];"
//				"SInt16 HUNKstart;"		// my hack here, until I support hunks
//				"SInt16 HUNKpad1;"	
//				"SInt16 HUNKend;"
//				"SInt16 HUNKpad2;"
				"} ObjHeader;");

//	resize-mem-handle(plug, data.object-data, c-int-expr("sizeof(ObjHeader)"));
	c-zeroed-local("ObjHeader", "header");

/*	call-out("header.HUNKstart = ", void:, int: hunk-start);		// my hack here, until I support hunks
	call-out("header.HUNKpad1 = ", void:, int: 0);
	call-out("header.HUNKend = ", void:, int: hunk-end);
	call-out("header.HUNKpad2 = ", void:, int: 0);*/

	let data-vector = make(<cw-vector>, opaque: data.object-data.opaque-handle, plugin: plug);
	data-vector.size := c-int-expr("sizeof(ObjHeader)");
	
	let (obj-size :: <integer>,
		nametable-offset :: <integer>,
		nametable-names :: <integer>,
		code-size :: <integer>,
		udata-size :: <integer>,
		idata-size :: <integer>)
	  = if (obj-provider)
		obj-provider(data-vector)
		else
			write-2(hunk-start, data-vector);
			write-2(0, data-vector);	// pad
			write-2(hunk-end, data-vector);
			write-2(0, data-vector);	// pad
			values(0, c-int-expr("sizeof(ObjHeader)"), 0, 0, 0, 0)
		  end if;

	data-vector.dereferenced & (unlock-mem-handle(data-vector.vector-plugin, data-vector) | (data-vector.dereferenced := #f));

	call-out("header.magic_word = ", void:, int: c-int-expr("'POWR'"));
	//call-out("header.magic_word = ", void:, int: c-int-literal("POWR")); еее
	call-out("header.version = ", void:, int: c-int-expr("0 /*OBJ_VERSION*/"));
//	call-out("header.nametable_offset = ", void:, int: c-int-expr("sizeof(ObjHeader)"));
	call-out("header.nametable_offset = ", void:, int: nametable-offset);
	call-out("header.nametable_names = ", void:, int: nametable-names + 1);


	let locked = lock-mem-handle(plug, data.object-data);
	call-out("BlockMoveData", void:, ptr: c-ptr-expr("&header"), ptr: locked.raw-value, int: c-int-expr("sizeof(header)"));
	unlock-mem-handle(plug, data.object-data);
end function fill-object-data-header;


// basically this should be a getter: еее still needed???
define open generic code-size(plug :: <plugin-callback>, data :: <object-data>) => size :: <integer>;

define method code-size(plug :: <plugin-callback>, data :: <ppc-object-data>) => size :: <integer>;
	0	// for now...
end method code-size;


define function export-fss(fss :: <file-spec>, raw :: <raw-pointer>) => ();
	c-local-decl("FSSpec* fss;");
	call-out("fss = ", void:, ptr: raw);
	call-out("fss->vRefNum = ", void:, int: fss.volume-ref);
	call-out("fss->parID = ", void:, int: fss.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("fss->name"), ptr: as(<c-string>, fss.spec-file-name).raw-value);
end function export-fss;

define c-enumeration
	$cw-access-absolute,
	$cw-access-path-relative,
	$cw-access-file-name,
	$cw-access-file-relative
end c-enumeration;

// CW_CALLBACK	CWStoreObjectData(CWPluginContext context, long whichfile, CWObjectData* object);
define function store-object-data(plug :: <plugin-callback>, which-file :: <integer>, object :: <object-data>) => ();
	c-zeroed-local("CWObjectData", "data");
	call-out("data.objectdata = ", void:, ptr: object.object-data.opaque-handle.raw-value);
	object.browse-data & call-out("data.browsedata = ", void:, ptr: object.browse-data.opaque-handle.raw-value);
	call-out("data.codesize = ", void:, int: code-size(plug, object));
	call-out("data.udatasize = ", void:, int: object.uninitialized-data-size);
	call-out("data.idatasize = ", void:, int: object.initialized-data-size);
	call-out("data.compiledlines = ", void:, int: object.compiled-lines);
	call-out("data.interfaceChanged = ", void:, int: object.interface-changed & 1 | 0);
	if (object.dependencies)
		call-out("data.dependencyCount = ", void:, int: object.dependencies.size);
		let size :: <integer> = c-int-expr("sizeof(CWDependencyInfo)") * c-int-expr("data.dependencyCount");
		call-out("data.dependencies = ", void:, ptr: call-out("__alloca", ptr:, int: size));
		call-out("memset", void:, ptr: c-ptr-expr("data.dependencies"), int: 0, int: size);

		c-void-expr("{ CWDependencyInfo* dep = data.dependencies");
		for (dep :: <file-spec> in object.dependencies)
			call-out("dep->fileIndex = ", void:, int: -1);	// use fileSpec
			export-fss(dep, c-ptr-expr("&dep->fileSpec"));
			call-out("dep->fileSpecAccessType = ", void:, int: $cw-access-file-relative);	// this should be tweakable!
			call-out("dep->dependencyType = ", void:, int: $normal-dependency);	// this should be tweakable!
			c-void-expr("dep++");
		end for;
		c-void-expr("}");
	end if;

	check(call-out("CWStoreObjectData", int:,
					ptr: plug.opaque-pointer.raw-value,
					int: which-file,
					ptr: c-ptr-expr("&data")));
end function store-object-data;

//CW_CALLBACK	CWGetNamedPreferences(CWPluginContext context, const char* prefsname, CWMemHandle* prefsdata);
define function get-named-preferences(plug :: <plugin-callback>, panel-name :: <byte-string>) => prefs :: <mem-handle>;
	let c-name :: <c-string> = as(<c-string>, panel-name);
	c-void-expr("{ CWMemHandle prefsdata = NULL");
	check(call-out("CWGetNamedPreferences", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: c-name.raw-value,
					ptr: c-ptr-expr("&prefsdata")));
	make(<mem-handle>, opaque: as(<machine-pointer>, c-ptr-expr("prefsdata; }")))
end function get-named-preferences;

// CW_CALLBACK CWGetBrowseOptions(CWPluginContext context, CWBrowseOptions* browseOptions);
define function get-browse-options(plug :: <plugin-callback>) => options :: <table>;
	c-zeroed-local("CWBrowseOptions", "browseOptions");
	check(call-out("CWGetBrowseOptions", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: c-ptr-expr("&browseOptions")));
	let options :: <table> = <table>.make;
	options[#"function"] := #t;	// always!
	options[#"variable"] := #t;	// always!
	options[#"class"] := (c-int-expr("browseOptions.recordClasses") ~== 0);
	options[#"enum"] := (c-int-expr("browseOptions.recordEnums") ~== 0);
	options[#"macro"] := (c-int-expr("browseOptions.recordMacros") ~== 0);
	options[#"typedef"] := (c-int-expr("browseOptions.recordTypedefs") ~== 0);
	options[#"constant"] := (c-int-expr("browseOptions.recordConstants") ~== 0);
	options[#"template"] := (c-int-expr("browseOptions.recordTemplates") ~== 0);
	options[#"undefined-function"] := (c-int-expr("browseOptions.recordUndefinedFunctions") ~== 0);
	options
end function get-browse-options;

// CW_CALLBACK	CWGetAccessPathListInfo(CWPluginContext context, CWAccessPathListInfo* pathListInfo);
// CW_CALLBACK	CWGetAccessPathInfo(CWPluginContext context, CWAccessPathType pathType, long whichPath, CWAccessPathInfo* pathInfo);
// CW_CALLBACK	CWGetAccessPathSubdirectory(CWPluginContext context, CWAccessPathType pathType, long whichPath, long whichSubdirectory, CWFileSpec* subdirectory);

// CW_CALLBACK CWGetFileText(CWPluginContext context, const CWFileSpec* filespec, const char** text, long* textLength, short* filedatatype);
// CW_CALLBACK	CWCachePrecompiledHeader(CWPluginContext context, const CWFileSpec* filespec, CWMemHandle pchhandle);


// CW_CALLBACK CWGetBuildSequenceNumber(CWPluginContext context, long* sequenceNumber);
// CW_CALLBACK CWGetTargetInfo(CWPluginContext context, CWTargetInfo* targetInfo);
// CW_CALLBACK CWSetTargetInfo(CWPluginContext context, CWTargetInfo* targetInfo);

// CW_CALLBACK	CWLoadObjectData(CWPluginContext context, long whichfile, CWMemHandle* objectdata);
// CW_CALLBACK	CWFreeObjectData(CWPluginContext context, long whichfile, CWMemHandle objectdata);
// CW_CALLBACK	CWGetSuggestedObjectFileSpec(CWPluginContext context, long whichfile, CWFileSpec* fileSpec);
// CW_CALLBACK	CWGetStoredObjectFileSpec(CWPluginContext context, long whichfile, CWFileSpec* fileSpec);

// CW_CALLBACK CWGetModifiedFiles(CWPluginContext context, long* modifiedFileCount, const long** modifiedFiles);

// CW_CALLBACK	CWGetPrecompiledHeaderSpec(CWPluginContext context, CWFileSpec *pchspec, const char *target);
// CW_CALLBACK	CWGetResourceFile(CWPluginContext context, CWFileSpec* filespec);
// CW_CALLBACK	CWPutResourceFile(CWPluginContext context, const char* prompt, const char* name, CWFileSpec* filespec);

// CW_CALLBACK	CWGetTargetStorage(CWPluginContext context, void** storage);
// CW_CALLBACK	CWSetTargetStorage(CWPluginContext context, void* storage);

/* Metrowerks Pascal support */
// CW_CALLBACK	CWLookUpUnit(CWPluginContext context, const char* name, Boolean isdependency, const void** unitdata, long* unitdatalength);
// CW_CALLBACK	CWSBMfiles(CWPluginContext context, short libref);
// CW_CALLBACK CWStoreUnit(CWPluginContext context, const char* unitname, CWMemHandle unitdata, CWDependencyTag dependencytag);
// CW_CALLBACK CWReleaseUnit(CWPluginContext context, void* unitdata);
// CW_CALLBACK	CWUnitNameToFileName(CWPluginContext context, const char* unitname, char* filename);

define macro c-assign
	{ c-assign(?str:token, ptr ?:expression) } => { call-out(?str, void:, ptr: ?expression) }
end macro c-assign;


define function play-ground(p)
//	c-assign("docinfo.text = ", ptr p.raw-value);
end;

// CW_CALLBACK CWResolveRelativePath(CWPluginContext context, const CWRelativePath* relativePath, CWFileSpec* fileSpec, Boolean create);
define function resolve-relative-path(plug :: <plugin-callback>, relative-path :: <machine-pointer>, #key create :: <boolean> = #t) => file-spec :: <file-spec>;
	c-void-expr("{ CWFileSpec spec; memset(&spec, 0, sizeof(spec))");
	check(call-out("CWResolveRelativePath", int:,
								ptr: plug.opaque-pointer.raw-value,
								ptr: relative-path.raw-value,
								ptr: c-ptr-expr("&spec"),
								int: create & 1 | 0));

	call-out("p2cstrcpy", void:, ptr: c-ptr-expr("spec.name"), ptr: c-ptr-expr("spec.name"));

	make(<file-spec>,
				vol: c-int-expr("spec.vRefNum"),
				dir: c-int-expr("spec.parID"),
				name: as(<byte-string>, as(<c-string>, c-ptr-expr("spec.name; }"))))

end function resolve-relative-path;


define function fill-fs-spec(spec :: <file-spec>, into :: <raw-pointer>, #key extra :: <byte-string> = "") => ();
	let spec-file-name :: <c-string> = as(<c-string>, concatenate(":", spec.spec-file-name, extra));
	call-out("{ CWFileSpec* spec = ", void:, ptr: into);
	call-out("spec->vRefNum = ", void:, int: spec.volume-ref);
	call-out("spec->parID = ", void:, int: spec.directory-ref);
	call-out("c2pstrcpy", void:, ptr: c-ptr-expr("spec->name"), ptr: spec-file-name.raw-value);
	c-void-expr("}");
end function fill-fs-spec;

define function relative-path-from(plug :: <plugin-callback>, root-spec :: <file-spec>, relative-path :: <byte-string>, #key check: check? :: <boolean>) => file-spec :: <file-spec>;
	c-void-expr("{ CWFileSpec spec, dest; memset(&spec, 0, sizeof(spec)); memset(&dest, 0, sizeof(dest))");
	fill-fs-spec(root-spec, c-ptr-expr("&spec"), extra: relative-path);
	
	local method check-if-needed(result :: <integer>) => result :: <integer>;
				check? & result | $cw-no-error
			end;

	check-mac-os(plug, check-if-needed(call-out("FSMakeFSSpec", int:, int: c-int-expr("spec.vRefNum"), int: c-int-expr("spec.parID"), ptr: c-ptr-expr("spec.name"), ptr: c-ptr-expr("&dest"))));
	
	make(<file-spec>,
				vol: c-int-expr("dest.vRefNum"),
				dir: c-int-expr("dest.parID"),
				name: as(<byte-string>, as(<c-string>, c-ptr-expr("dest.name; }"))))

end function relative-path-from;


define function begin-sub-compile(plug :: <plugin-callback>, p1 :: <integer>, p2 :: <integer>) => ();
/*	c-decl("CW_CALLBACK CWBeginSubCompile(CWPluginContext context, void* p1, long p2);");

	check(call-out("CWBeginSubCompile", int:,
					ptr: plug.opaque-pointer.raw-value,
					ptr: 0,	//ееееее
					int: 0));*/
end function begin-sub-compile;


define function end-sub-compile(plug :: <plugin-callback>) => ();
/*	c-decl("CW_CALLBACK CWEndSubCompile(CWPluginContext context);");
	check(call-out("CWEndSubCompile", int:,
					ptr: plug.opaque-pointer.raw-value));*/
end function end-sub-compile;

/*
Interesting ones:
	    9: CWSetCommandStatus
	   12: CWBeginSubCompile
	   21: CWEndSubCompile
	   16: CWVCSStateChanged
	   17: CWGetCommandStatus
	   19: CWGetCOMProjectInterface
	   22: CWGetProjectFileSpecifier
	   30: CWAllocateMemory
	   34: CWGetTargetDataDirectory	// put .c files here
	   36: CWIsRecursiveRequest
	   37: CWGetComment
	   39: CWCompletionRatio
	   42: CWPanlGetPanelPrefs
	   46: CWFileStateChanged
	   60: CWIsCommandSupportedRequest
	   68: CWFreeMemory
	   77: CWSetCommandDescription
	   80: CWGetItemValue
	   81: CWGetCOMApplicationInterface
	   91: CWSetItemText
	   94: CWDoVisualDifference
	  103: CWPanlChooseRelativePath
	    4: CWGetEnvironmentVariableCount
	  110: CWGetEnvironmentVariable
	  112: CWOSErrorMessage
	  114: CWGetIDEInfo
	  117: CWIsAdvancedRequest
	  130: CWGetOutputFileDirectory
	  131: CWResolveRelativePath
	  171: CWSetItemValue
	  151: CWGetDatabaseConnectionInfo
	  142: CWGetWorkingDirectory


	    1: PluginLib4
	    7: CWPanlEnableItem
	    8: CWPanlGetRelativePathString
	   10: CWPanlGetFloatingPointValue
	   18: CWGetOverlay1FileInfo
	   23: CWPanlGetArraySettingElement
	   24: CWPanlGetMacPort
	   25: CWPanlRemoveUserItem
	   27: CWPanlSetRelativePathValue
	   28: CWGetOverlay1GroupInfo
	   32: CWPanlGetItemControl
	   35: CWPanlDrawPanelBox
	   40: CWSetVCSItem
	   41: CWGetVCSItemCount
	   47: CWPanlInstallUserItem
	   48: CWPanlGetItemTextHandle
	   50: CWPanlGetItemRect
	   51: CWPanlGetItemMaxLength
	   55: CWTerminateDialog
	   57: CWPanlGetItemValue
	   58: CWCheckoutLicense
	   61: CWGetVCSPointerStorage
	   62: CWPanlActivateItem
	   63: CWPanlWriteRelativePathAEDesc
	   64: CWPanlDrawUserItemBox
	   65: CWPanlSetItemMaxLength
	   70: CWPanlWriteFloatingPointSetting
	   71: CWSetupDialog
	   72: CWPanlReadRelativePathAEDesc
	   73: CWPanlWriteRelativePathSetting
	   74: CWPanlWriteStringSetting
	   78: CWOSAlert
	   87: CWPanlGetItemText
	   89: CWGetCommandLineArgs
	   92: CWPanlGetArraySettingSize
	   95: CWPanlSetItemData
	   96: CWSBMfiles
	   97: CWPanlSetItemTextHandle
	   98: CWPanlGetIntegerValue
	   99: CWPanlSetBooleanValue
	  100: CWGetOverlay1GroupsCount
	  102: CWPanlInvalItem
	  104: CWPanlWriteBooleanSetting
	  105: CWGetOverlay1Info
	  111: CWPanlReadStringSetting
	  113: CWPanlSetFloatingPointValue
	  115: CWPostDialog
	  120: CWCheckinLicense
	  121: CWPanlGetNamedSetting
	  122: CWPanlGetStructureSettingField
	  123: CWGetSegmentInfo
	  134: CWPanlSetItemText
	  135: CWPanlSetIntegerValue
	  136: CWPanlGetBooleanValue
	  137: CWGetCOMTargetInterface
	  138: CWGetItemText
	  139: CWPanlReadRelativePathSetting
	  140: CWGetTargetName
	  143: CWPanlSetItemValue
	  144: CWPanlGetItemData
	  145: CWPanlWriteIntegerSetting
	  146: CWGetCOMDesignInterface
	  147: CWPanlReadBooleanSetting
	  149: CWPreDialog
	  150: CWProcessDialog
	  152: CWAllowV1Compatibility
	  153: CWPanlReadIntegerSetting
	  154: CWGetVCSItem
	  155: CWPanlGetStringValue
	  156: CWGetBuildSequenceNumber
	  158: CWPanlShowItem
	  159: CWPanlSetStringValue
	  161: CWPanlValidItem
	  162: CWSetTargetInfo
	  164: CWPanlGetRelativePathValue
	  166: CWPanlReadFloatingPointSetting
	  168: CWPanlAppendItems
	  170: CWSetVCSPointerStorage




offsets/selectors in jump table:

IsPrecompiling		0xD0
IsAutoPrecompiling		0xD1

BASE 260 Jump Table::::

Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETFILEINFO"(16)  Size=348
00000146: 2050               movea.l   (a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFINDANDLOADFILE"(19)  Size=416
00000188: 2068 0004          movea.l   4(a0),a0
Hunk:	Kind=HUNK_LOCAL_CODE  Name="EnsureCachedAccessPaths__FP22CWPluginPrivateContext"(25)  Size=62
0000001A: 2068 008C          movea.l   140(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETFILETEXT"(20)  Size=678
00000290: 2068 0008          movea.l   8(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWRELEASEFILETEXT"(38)  Size=266
000000F4: 2068 000C          movea.l   12(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETSEGMENTINFO"(40)  Size=204
000000B6: 2068 0010          movea.l   16(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETOVERLAY1GROUPINFO"(43)  Size=164
0000008E: 2068 0014          movea.l   20(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETOVERLAY1INFO"(44)  Size=168
00000092: 2068 0018          movea.l   24(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETOVERLAY1FILEINFO"(45)  Size=172
00000096: 2068 001C          movea.l   28(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWREPORTMESSAGE"(46)  Size=416
0000018A: 2068 0020          movea.l   32(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWALERT"(51)  Size=272
000000FA: 2068 0024          movea.l   36(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSHOWSTATUS"(54)  Size=186
000000A4: 2068 0028          movea.l   40(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWUSERBREAK"(56)  Size=176
0000009A: 2068 002C          movea.l   44(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETNAMEDPREFERENCES"(58)  Size=326
00000130: 2068 0030          movea.l   48(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSTOREPLUGINDATA"(60)  Size=154
00000084: 2068 0034          movea.l   52(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETPLUGINDATA"(61)  Size=154
00000084: 2068 0038          movea.l   56(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSETMODDATE"(62)  Size=214
000000C0: 2068 003C          movea.l   60(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWADDPROJECTENTRY"(64)  Size=190
000000A8: 2068 0040          movea.l   64(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWCREATENEWTEXTDOCUMENT"(65)  Size=318
00000128: 2068 0044          movea.l   68(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWALLOCATEMEMORY"(66)  Size=182
000000A0: 2068 0048          movea.l   72(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFREEMEMORY"(67)  Size=174
00000098: 2068 004C          movea.l   76(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWALLOCMEMHANDLE"(34)  Size=286
00000108: 2068 0050          movea.l   80(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFREEMEMHANDLE"(68)  Size=160
0000008A: 2068 0054          movea.l   84(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETMEMHANDLESIZE"(69)  Size=250
000000E4: 2068 0058          movea.l   88(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWRESIZEMEMHANDLE"(70)  Size=216
000000C2: 2068 005C          movea.l   92(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWLOCKMEMHANDLE"(71)  Size=232
000000D2: 2068 0060          movea.l   96(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWUNLOCKMEMHANDLE"(72)  Size=178
0000009C: 2068 0064          movea.l   100(a0),a0

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"CWGETTARGETNAME"
00000086: 2068 0088          movea.l   136(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWPREDIALOG"(74)  Size=132
0000006E: 2068 0090          movea.l   144(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWPOSTDIALOG"(75)  Size=132
0000006E: 2068 0094          movea.l   148(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWPREFILEACTION"(76)  Size=122
00000064: 2068 0098          movea.l   152(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWPOSTFILEACTION"(77)  Size=122
00000064: 2068 009C          movea.l   156(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWCHECKOUTLICENSE"(78)  Size=108
00000058: 2068 00A0          movea.l   160(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWCHECKINLICENSE"(79)  Size=92
00000048: 2068 00A4          movea.l   164(a0),a0

BASE 734 callbacks are all _V6 callbacks
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSELECTPROJECTENTRY_V6"(30)  Size=44

Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSTOREUNIT_V6"(31)  Size=44
0000001A: 2068 0078          movea.l   120(a0),a0

Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFINDANDLOADFILE_V6"(1)  Size=42
0000001A: 2050               movea.l   (a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETPREFERENCES_V6"(2)  Size=44
0000001A: 2068 0004          movea.l   4(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETFILEINFO_V6"(3)  Size=44
0000001A: 2068 0008          movea.l   8(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETSEGMENTINFO_V6"(4)  Size=44
0000001A: 2068 000C          movea.l   12(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWLOADOBJECTDATA_V6"(5)  Size=44
0000001A: 2068 0010          movea.l   16(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSTOREOBJECTDATA_V6"(6)  Size=44
00000012: 206E 0010          movea.l   16(a6),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFREEOBJECTDATA_V6"(7)  Size=44
0000001A: 2068 0018          movea.l   24(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWREPORTERROR_V6"(8)  Size=48
0000001E: 2068 001C          movea.l   28(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWREPORTWARNING_V6"(9)  Size=48
0000001E: 2068 0020          movea.l   32(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWERRORREFMESSAGE_V6"(10)  Size=52
00000022: 2068 0024          movea.l   36(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWERRORMESSAGE_V6"(11)  Size=44
0000001A: 2068 0028          movea.l   40(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWOSERRORMESSAGE_V6"(12)  Size=48
0000001E: 2068 002C          movea.l   44(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWALERT_V6"(13)  Size=52
00000022: 2068 0030          movea.l   48(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWOSALERT_V6"(14)  Size=44
0000001A: 2068 0034          movea.l   52(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWUSERBREAK_V6"(15)  Size=36
00000012: 2068 0038          movea.l   56(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSHOWSTATUS_V6"(16)  Size=44
0000001A: 2068 003C          movea.l   60(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWDISPLAYLINES_V6"(17)  Size=40
00000016: 2068 0040          movea.l   64(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETRESOURCEFILE_V6"(18)  Size=40
00000016: 2068 0044          movea.l   68(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWPUTRESOURCEFILE_V6"(19)  Size=48
0000001E: 2068 0048          movea.l   72(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWCACHEINCLUDEFILE_V6"(20)  Size=44
0000001A: 2068 004C          movea.l   76(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETCACHEDINCLUDEFILE_V6"(21)  Size=44
0000001A: 2068 0050          movea.l   80(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFREEINCLUDEFILE_V6"(22)  Size=40
00000016: 2068 0054          movea.l   84(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWCACHEPRECOMPILEDHEADER_V6"(23)  Size=44
0000001A: 2068 0058          movea.l   88(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETCACHEDPRECOMPILEDHEADER_V6"(24)  Size=44
0000001A: 2068 005C          movea.l   92(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFIXTEXTFILEHANDLE_V6"(25)  Size=44
0000001A: 2068 0060          movea.l   96(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETPRECOMPILEDHEADERSPEC_V6"(26)  Size=44
0000001A: 2068 0064          movea.l   100(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSETMODDATE_V6"(27)  Size=44
0000001A: 2068 0068          movea.l   104(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWLOOKUPUNIT_V6"(28)  Size=52
00000022: 2068 006C          movea.l   108(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSBMFILES_V6"(29)  Size=40
00000016: 2068 0070          movea.l   112(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSELECTPROJECTENTRY_V6"(30)  Size=44
0000001A: 2068 0074          movea.l   116(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSTOREUNIT_V6"(31)  Size=44
0000001A: 2068 0078          movea.l   120(a0),a0


BASE 250 routines:
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWREPORTMESSAGE"(46)  Size=416
00000068: 4E90               jsr       (a0)


Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWALERT"(51)  Size=272

BASE 32 routines:
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWREPORTMESSAGE"(46)  Size=416
0000018A: 2068 0020          movea.l   32(a0),a0
0000018E: 4E90               jsr       (a0)
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETRESOURCEFILE"(30)  Size=130
00000070: 4E90               jsr       (a0)
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWREPORTWARNING_V6"(9)  Size=48
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSETITEMVALUE"(13)  Size=154


BASE 614:

Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWLOADOBJECTDATA"(19)  Size=138
00000074: 2068 0004          movea.l   4(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSTOREOBJECTDATA"(22)  Size=244
000000DE: 2068 0008          movea.l   8(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWFREEOBJECTDATA"(24)  Size=126
00000068: 2068 000C          movea.l   12(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETSUGGESTEDOBJECTFILESPEC"(26)  Size=138
00000072: 2068 0048          movea.l   72(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETSTOREDOBJECTFILESPEC"(27)  Size=138
00000072: 2068 004C          movea.l   76(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWDISPLAYLINES"(28)  Size=118
00000060: 2068 0010          movea.l   16(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETRESOURCEFILE"(30)  Size=130
0000006C: 2068 0020          movea.l   32(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWPUTRESOURCEFILE"(32)  Size=212
000000BE: 2068 0024          movea.l   36(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWCACHEPRECOMPILEDHEADER"(35)  Size=152
00000084: 2050               movea.l   (a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETPRECOMPILEDHEADERSPEC"(37)  Size=138
00000074: 2068 001C          movea.l   28(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWBEGINSUBCOMPILE"(39)  Size=100
0000004E: 2068 0014          movea.l   20(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWENDSUBCOMPILE"(40)  Size=62
0000002A: 2068 0018          movea.l   24(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWLOOKUPUNIT"(41)  Size=188
000000A6: 2068 0028          movea.l   40(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSBMFILES"(43)  Size=118
00000060: 2068 002C          movea.l   44(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWSTOREUNIT"(45)  Size=176
0000009A: 2068 0030          movea.l   48(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWRELEASEUNIT"(47)  Size=64
0000002C: 2068 0034          movea.l   52(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWUNITNAMETOFILENAME"(48)  Size=100
00000050: 2068 0038          movea.l   56(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWOSERRORMESSAGE"(51)  Size=156
00000086: 2068 003C          movea.l   60(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWOSALERT"(54)  Size=154
00000084: 2068 0040          movea.l   64(a0),a0
Hunk:	Kind=HUNK_GLOBAL_CODE  Name="CWGETMODIFIEDFILES"(56)  Size=152
00000082: 2068 0044          movea.l   68(a0),a0
*/