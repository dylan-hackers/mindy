module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/optimize/optimize-exports.dylan,v 1.1 1996/03/17 00:40:59 wlott Exp $
copyright: Copyright (c) 1996  Carnegie Mellon University
	   All rights reserved.


define library compiler-optimize
  use Dylan;
  use compiler-base;
  use compiler-front;

  export
    cheese;

end library compiler-optimize;


define module cheese
  use common;
  use utils;
  use errors;
  use compile-time-values;
  use names;
  use definitions;
  use variables, exclude: {<renaming>};
  use flow;
  use front;
  use ctype;
  use classes;
  use signature-interface;
  use source;
  use builder-interface;
  use policy;
  use primitives;
  use transformers;
  use compile-time-functions;
  use function-definitions;

  export
    *optimize-ncalls*;
end;
