module: misc-dump
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/misc-dump.dylan,v 1.2 1995/11/13 14:55:13 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define method dump-od (obj :: <module>, buf :: <dump-state>) => ();
  dump-simple-object(#"module", buf,
		     obj.module-home.library-name,
		     obj.module-name);
end;

add-od-loader(*compiler-dispatcher*, #"module",
  method (state :: <load-state>) => res :: <module>;
    let lib-name = load-object-dispatch(state);
    let mod-name = load-object-dispatch(state);
    assert-end-object(state);
    find-module(find-library(lib-name, create: #t), mod-name,
		create: #t);
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
