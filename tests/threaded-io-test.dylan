module:     Threaded-io-test
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Test out the thread features of Mindy by getting I/O on 
            multiple streams.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /scm/cvs/src/tests/threaded-io-test.dylan,v 1.1 1998/05/03 19:54:57 andreas Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// This is a simple test of threads in the presence of streams.  It is
// meant to test the strength of the C select() function in
// interp/driver.c (select is provided by the operating system, but on
// some non-Unix platforms is not very good).

// This program reads from three streams simultaneously and echos them
// all to standard output, interlacing the results character by
// character.  One file is called "asterixes.txt" and consists of
// nothing but asterixes, the second is "dashes.txt" and is nothing
// but dashes, and the third file is standard input.  When you're
// tired of running it, hit control-C and quit out of the debugger
// with "quit".

// What you should see: Until you type something at the keyboard, you
// should see asterixes and dashes in approximately equal proportions.
// When you type something, before you hit return it'll appear in the
// input stream in places dictated by your typing speed.  When you hit
// return, this program will read it in and output it amoung the
// asterixes and dashes.

// Sample output:
//
// -*-*-*-*-*-*-*A-*A-A*-*-*-*-*-*-*\n-A-A-A*\n-*-*-*-*-*-
//               ^^^^^^             ^
//   Here I quickly typed in       Here I hit return
//   three A's

//   (\n will be replaced by a newline character)

// If the output is not appearing continuously, it means that Mindy
// has blocked on one of the input streams (probably
// *standard-input*), which means that select() is faulty.

define library Threaded-io-test
  use dylan;
  use streams;
  use standard-io;
//  use format;
end library Threaded-io-test;

define module threaded-io-test
  use dylan;
  use extensions, import: { main };
  use threads;
  use streams;
  use standard-io;
//  use format;
end module threaded-io-test;

define method echo-input (stream :: <stream>) => ();
  block (done) 
    while (#t)
      let char = read-element(stream, on-end-of-stream: #"eos");
      if (char == #"eos")  done()  end if;
      write-element(*standard-output*, as(<character>, char));
      force-output(*standard-output*);
    end while;
  end block;
end method echo-input;

define method echo-file (filename :: <string>) => ();
  while (#t)
    let file = make(<file-stream>, locator: filename, direction: #"input");
    echo-input(file);
    close(file);
  end while;
end method echo-file;

define constant asterixes = curry(echo-file, "asterixes.txt");
define constant dashes = curry(echo-file, "dashes.txt");

define method main (argv0, #rest ignored)
  let asterix-thread = spawn-thread("Asterixes", asterixes);
  let dash-thread = spawn-thread("Dashes", dashes);
  echo-input(*standard-input*);
  new-line(*standard-output*);
  write-line(*standard-output*, "Done");
  kill-thread(asterix-thread);
  kill-thread(dash-thread);
end method main;
