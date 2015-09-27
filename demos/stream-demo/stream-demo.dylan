module: stream-demo

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

define method main (foo, #rest stuff)
  
  write(*standard-output*, "File to type: ");
  force-output(*standard-output*);
  let res = read-line(*standard-input*);
  write(*standard-output*, "You said: ");
  write-line(*standard-output*, res);
  force-output(*standard-output*);
  let stream = make(<file-stream>, locator: res, direction: #"input");
  block (punt)
    for (while: #t)
      let wot = read-line(stream, on-end-of-stream: #f);
      if (wot)
        write-line(*standard-output*, wot);
      else
        punt();
      end if;
    end for;
  end block;
  close(stream);
  force-output(*standard-output*);

end method;
