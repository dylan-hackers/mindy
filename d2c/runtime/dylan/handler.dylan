rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/handler.dylan,v 1.3 1995/11/13 23:09:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define class <handler> (<object>)
  //
  // The type of condition this is a handler for.
  slot handler-type :: <type>,
    required-init-keyword: type:;

  // The handler function.
  slot handler-function :: <function>,
    required-init-keyword: function:;

  // The guard function, or #f if it isn't guarded.
  slot handler-test :: false-or(<function>),
    required-init-keyword: test:;

  // The init-arguments to pass make when interactivly invoking this handler.
  slot handler-init-arguments :: <sequence>,
    required-init-keyword: init-arguments:;

  // The previous handler in the chain of handers.
  slot handler-prev :: union(<handler>, <false>),
    required-init-keyword: prev:;
end;

seal generic make (singleton(<handler>));
seal generic initialize (<handler>);


define method push-handler
    (type :: <type>, function :: <function>,
     #key test :: false-or(<function>), init-arguments :: <sequence> = #())
    => ();
  let thread = this-thread();
  thread.cur-handler := make(<handler>,
			     type: type,
			     function: function,
			     test: test,
			     init-arguments: init-arguments,
			     prev: thread.cur-handler);
end;

define method pop-handler () => ();
  let thread = this-thread();
  thread.cur-handler := thread.cur-handler.handler-prev;
end;
