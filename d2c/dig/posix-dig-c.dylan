module: d2c-gnu

#if (mindy)
define constant anonymous-1
  = constrain-c-function(find-c-function("delegate_gdb_signals"), #(), #t, list(<integer>));
define method delegate-gdb-signals
    ()
 => (result :: <integer>);
  let result-value
    = anonymous-1();
  values(result-value);
end method delegate-gdb-signals;

#else
c-include(".//posix-dig-support.h");

define method delegate-gdb-signals
    ()
 => (result :: <integer>);
  let result-value
    = call-out("delegate_gdb_signals", int:);
  values(result-value);
end method delegate-gdb-signals;

#endif
