Module: dylan-user

define library hello-world
  use Dylan;
end library;

define module hello-world
  use dylan;
  use cheap-io;		// For puts
  use extensions;	// For Main
end module;
