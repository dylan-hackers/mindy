Module:       carbon-duim

// XXX - DUMMY

define sealed method do-notify-user
    (framem :: <carbon-frame-manager>, owner :: <sheet>,
     message :: <string>, style :: <notification-style>,
     #key title :: false-or(<string>), documentation :: false-or(<string>), name, 
	  exit-style :: false-or(<notification-exit-style>) = #f,
     #all-keys)
 => (ok? :: <boolean>, exit-type)
  debug-message(message);
 	#t;
 end method do-notify-user;