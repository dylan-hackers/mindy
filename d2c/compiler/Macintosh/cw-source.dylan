module: source
author: gabor@mac.com
copyright: see below

//======================================================================
//
// Copyright (c) 2000, 2001, 2002  Gwydion Dylan Maintainers
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
//    the contribution of the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  The Gwydion Dylan Maintainers
// make any warranty about the software, its performance, or its conformity
// to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// <cw-source-file> -- exported.
// 
// <source-file> derivation for special use in the CodeWarrior IDE and similar
// weakly file-based environments
//
define class <cw-source-file> (<source-file>)
  //
  // The contents, already read in. Slot overrides GF contents.
  constant slot contents :: <file-contents>, required-init-keyword: buffer:;
end;

// when loading we want it to come back as a <source-file>
add-make-dumper(#"source-file", *compiler-dispatcher*, <cw-source-file>,
		list(full-file-name, name:, #f), dumper-only: #t);
