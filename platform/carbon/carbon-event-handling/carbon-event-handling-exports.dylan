module: dylan-user

define library carbon-event-handling
  use common-dylan;
  use io;
	use melange-support;

  use carbon;
end library;

define module carbon-event-handling
  use common-dylan;
  use System;
  use format-out;
	use melange-support;

  use mac-types;
  use memory;
  use quickdraw;
  use windows;
  use carbon-events; 
end module;
