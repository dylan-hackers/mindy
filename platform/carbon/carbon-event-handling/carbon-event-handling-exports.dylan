module: dylan-user

define library carbon-event-handling
  use dylan; // for system
  use common-dylan;
  use io;
	use melange-support;

  use carbon;
end library;

define module carbon-event-handling
  use common-dylan;
  use system;
  use format-out;
	use melange-support;

  use carbon;
end module;
