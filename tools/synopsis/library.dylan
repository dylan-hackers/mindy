module: dylan-user
author: Nick Kramer
rcs-header: $Header: /home/housel/work/rcs/gd/src/tools/synopsis/library.dylan,v 1.2 1997/02/13 13:06:35 nkramer Exp $

// Copyright JPRC 1996, all rights reserved.

define library synopsis
  use dylan;
  use streams;
  use standard-io;
  use format;
  use regular-expressions;
  use table-extensions;
end library synopsis;

define module synopsis
  use dylan;
  use extensions;
#if (~mindy)
  use system; // for %main
#endif
  use streams;
  use standard-io;
  use format;
  use regular-expressions;
  use table-extensions, import: { case-insensitive-equal };
end module synopsis;

