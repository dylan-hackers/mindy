module: dylan

define class <unwind-protect> (<object>)
  slot saved-stack :: <raw-pointer>,
    required-init-keyword: saved-stack:;
  slot prev-uwp :: false-or(<unwind-protect>),
    required-init-keyword: prev-uwp:;
  slot cleanup :: <function>,
    required-init-keyword: cleanup:;
end;

seal generic make (singleton(<unwind-protect>));
seal generic initialize (<unwind-protect>);

define method push-unwind-protect (cleanup :: <function>) => ();
  let thread = this-thread();
  thread.cur-uwp := make(<unwind-protect>,
			 saved-stack: %%primitive current-sp(),
			 prev-uwp: thread.cur-uwp,
			 cleanup: cleanup);
end;

define method pop-unwind-protect () => ();
  let thread = this-thread();
  thread.cur-uwp := thread.cur-uwp.prev-uwp;
end;

define class <catcher> (<object>)
  slot disabled :: <boolean>, init-value: #f;
  slot thread :: <thread>, required-init-keyword: thread:;
  slot uwp :: false-or(<unwind-protect>), required-init-keyword: uwp:;
  slot saved-stack :: <raw-pointer>, required-init-keyword: saved-stack:;
  slot saved-state :: <raw-pointer>, required-init-keyword: saved-state:;
end;

seal generic make (singleton(<catcher>));
seal generic initialize (<catcher>);

define method make-catcher (saved-state :: <raw-pointer>) => res :: <catcher>;
  let thread = this-thread();
  make(<catcher>, thread: thread, uwp: thread.cur-uwp,
       saved-stack: %%primitive current-sp(), saved-state: saved-state);
end;

define method make-exit-function (catcher :: <catcher>) => res :: <function>;
  method (#rest args)
    throw(catcher, args);
  end;
end;

define method disable-catcher (catcher :: <catcher>) => ();
  catcher.disabled := #t;
end;


define constant catch
  = method (saved-state :: <raw-pointer>, thunk :: <function>)
      thunk(saved-state);
    end;

define method throw (catcher :: <catcher>, values :: <simple-object-vector>)
    => res :: type-or();
  if (catcher.disabled)
    error("Can't exit to a block that has already been exited from.");
  end;
  let this-thread = this-thread();
  unless (catcher.thread == this-thread)
    error("Can't exit from a block set up by some other thread.");
  end;
  let target-uwp = catcher.uwp;
  let uwp = this-thread.cur-uwp;
  until (uwp == target-uwp)
    %%primitive unwind-stack(uwp.saved-stack);
    let prev = uwp.prev-uwp;
    this-thread.cur-uwp := prev;
    uwp.cleanup();
    uwp := prev;
  end;
  catcher.disabled := #t;
  %%primitive unwind-stack(catcher.saved-stack);
  // Note: the values-sequence has to happen after the unwind-stack.
  %%primitive throw(catcher.saved-state, values-sequence(values));
end;
