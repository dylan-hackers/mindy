rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/runtime/dylan/thread.dylan,v 1.4 1996/03/17 00:11:23 wlott Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.
module: dylan-viscera

define class <thread> (<object>)
  slot cur-uwp :: false-or(<unwind-protect>), init-value: #f;
  slot cur-handler :: false-or(<handler>), init-value: #f;
end;

define sealed domain make (singleton(<thread>));
define sealed domain initialize (<thread>);

define constant $the-one-and-only-thread :: <thread> = make(<thread>);

define method this-thread () => res :: <thread>;
  $the-one-and-only-thread;
end;
