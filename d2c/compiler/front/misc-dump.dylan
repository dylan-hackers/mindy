module: misc-dump
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/misc-dump.dylan,v 1.1 1995/10/13 15:07:15 ram Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


define method dump-od (obj :: <basic-name>, buf :: <dump-state>) => ();
  let modu = obj.name-module;
  dump-simple-object(#"basic-name", buf,
  		     modu.module-home.library-name,
		     modu.module-name,
		     obj.name-symbol);
end method;

define method load-basic-name (state :: <load-state>) => res :: <basic-name>;
  let lib-name = load-object-dispatch(state);
  let mod-name = load-object-dispatch(state);
  let obj-name = load-object-dispatch(state);
  assert-end-object(state);
  make(<basic-name>, symbol: obj-name,
       module: find-module(find-library(lib-name, create: #t), mod-name,
			   create: #t));
end method;

add-od-loader(*compiler-dispatcher*, #"basic-name", 
  method (state :: <load-state>) => res :: <basic-name>;
    load-basic-name(state);
  end method
);


define method dump-od (obj :: <variable>, buf :: <dump-state>) => ();
  let modu = obj.variable-home;
  dump-simple-object(#"module-variable", buf,
  		     modu.module-home.library-name,
		     modu.module-name,
		     obj.variable-name);
end method;

add-od-loader(*compiler-dispatcher*, #"module-variable", 
  method (state :: <load-state>) => res :: <variable>;
    find-variable(load-basic-name(state), create: #t);
  end method
);
