rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/thread.dylan,v 1.3 1995/11/13 23:09:07 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define class <thread> (<object>)
  slot cur-uwp :: false-or(<unwind-protect>), init-value: #f;
  slot cur-handler :: false-or(<handler>), init-value: #f;
end;

seal generic make (singleton(<thread>));
seal generic initialize (<thread>);

define constant $the-one-and-only-thread :: <thread> = make(<thread>);

define method this-thread () => res :: <thread>;
  $the-one-and-only-thread;
end;
