Module:       Dylan-User
Synopsis:     Carbon back-end
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module carbon-duim
  use dylan;
  
  use duim-imports;
  use duim-internals;
  use duim-gadget-panes-internals;	//---*** until we've got all native gadgets in
  
  //use C-FFI;
  use melange-support, exclude:{subclass};
  use system;
  use format-out;
  use carbon;

  // Basic classes
  export <carbon-port>,
         <carbon-medium>,
         <carbon-frame-manager>;

  // Fonts
  export <carbon-font>;
  
  //---*** Export the useful set of carbon gadget classes, etc.
end module carbon-duim;
