module: dylan-user
linker: -framework Cocoa

define library cocoa-test
  use common-dylan;
  use io;
  
  use cocoa;
  
  use dylan;
  use melange-support;
end library;

define module cocoa-test
  use common-dylan;
  use format-out;

  use cocoa,
		rename: { size => NSsize, size-setter => NSsize-setter,
				initialize => NSinitialize, format => NSformat,
				pop => NSpop, push => NSpush };
				
  use system;
  use melange-support;
end module;
