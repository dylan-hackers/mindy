module: dylan

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
