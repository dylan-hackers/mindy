module: misc-dump
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/misc-dump.dylan,v 1.6 1996/02/09 01:37:43 rgs Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


add-make-dumper(#"use", *compiler-dispatcher*,
  <use>, 
  list(
       name-used, #"name", #f,
       imports, #"imports", #f,
       prefix, #"prefix", #f,
       excludes, #"excludes", #f,
       renamings, #"renamings", #f,
       exports, #"exports", #f
   ));

add-make-dumper(#"renaming", *compiler-dispatcher*,
  <renaming>, 
  list(
       orig-name, #"orig-name", #f,
       new-name, #"new-name", #f
   ));


define method dump-od (obj :: <module>, buf :: <dump-state>) => ();
  if (maybe-dump-reference(obj, buf))
    dump-simple-object(#"module", buf,
		       obj.module-home.library-name,
		       obj.module-name);
  end if;
end;

add-od-loader(*compiler-dispatcher*, #"module",
  method (state :: <load-state>) => res :: <module>;
    let lib-name = load-object-dispatch(state);
    let mod-name = load-object-dispatch(state);
    assert-end-object(state);
    find-module(find-library(lib-name), mod-name, create: #t);
  end method
);


define method dump-od (obj :: <library>, buf :: <dump-state>) => ();
  dump-simple-object(#"library", buf, obj.library-name);
end;

add-od-loader(*compiler-dispatcher*, #"library",
  method (state :: <load-state>) => res :: <library>;
    find-library(load-sole-subobject(state));
  end method
);


define method dump-od (obj :: <basic-name>, buf :: <dump-state>) => ();
  dump-simple-object(#"basic-name", buf,
		     obj.name-module,
		     obj.name-symbol);
end method;

define constant load-basic-name
    = method (state :: <load-state>) => res :: <basic-name>;
	let modu = load-object-dispatch(state);
	let obj-name = load-object-dispatch(state);
	assert-end-object(state);
	make(<basic-name>, symbol: obj-name, module: modu);
      end method;

add-od-loader(*compiler-dispatcher*, #"basic-name", load-basic-name);


define method dump-od (obj :: <variable>, buf :: <dump-state>) => ();
  dump-simple-object(#"module-variable", buf,
		     obj.variable-home,
		     obj.variable-name);
end method;

add-od-loader(*compiler-dispatcher*, #"module-variable", 
  method (state :: <load-state>) => res :: <variable>;
    find-variable(load-basic-name(state), create: #t);
  end method
);
