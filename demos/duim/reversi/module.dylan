Module:       dylan-user
Author:       Andy Armstrong
Synopsis:     Reversi game
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module reversi
  use common-dylan, exclude: {format-to-string};
  use format;			// exported from functional-dylan
  use format-out;
  use random;			// exported from functional-dylan
  use operating-system;		// exported from system
  use file-system;		// exported from system
  use streams;			// exported from io
  use duim;

  export <reversi-frame>,
         play-reversi;
end module reversi;
