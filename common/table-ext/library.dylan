module: Dylan-User
author: Nick Kramer (nkramer@cs.cmu.edu), David Watson (dwatson@cmu.edu)
synopsis: Definition of the Table-Extensions library.
rcs-header: $Header: /scm/cvs/src/common/table-ext/library.dylan,v 1.3 2004/08/24 07:38:23 bruce Exp $

//======================================================================
//
// Copyright (c) 1996  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

define library Table-Extensions
  use Dylan;
  export Table-Extensions;
end library Table-Extensions;

define module Table-Extensions
  use Dylan;
  use %Hash-Tables,
    export: {remove-all-keys!, <hash-state>, collection-hash,
	     sequence-hash, string-hash,
	     <equal-table>, equal-hash, <value-table>, value-hash};
  
  export
    <string-table>, <byte-string-table>,
    <case-insensitive-string-table>, <case-insensitive-byte-string-table>,
    values-hash,
    case-insensitive-string-hash,
    case-insensitive-equal;
end module Table-Extensions;
