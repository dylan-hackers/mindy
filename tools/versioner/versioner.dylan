module: versioner
library: versioner
author: Nick Kramer (nkramer@cs.cmu.edu)

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

// Program that slurps up a whole bunch of rlog outputs, and decides
// which revisions go together to form a conceptual "version".  The
// heuristic used is that any two revisions by the same author that
// were checked in within 20 minutes of each other must go together.

// Output goes to standard output; status messages and other
// informational stuff goes to standard error.

// This program is relatively straightforward, but on Windows/NT it
// has to do a lot of obnoxious things to deal with the utilities
// being used.  Specifically, the command "find" is a Windows command;
// I instead use my own custom perl script.  Also, we have to use the
// original rlog, not the rlog I implemented fake-symlinks for.


// Constants that seem likely to change

define constant $gwydion-prefix 
    #if (compiled-for-win32)
       = "n:/gwydion/"; 
    #else
       = "/afs/cs/project/gwydion/";
    #endif

define constant $rlog-command
  #if (compiled-for-win32)
     = "/gwydion/rcs-bin/rlog.exe";
  #else
       = "rlog";
  #endif

define constant $find-command
  #if (compiled-for-win32)
     = "perl /gwydion/bin/unix-find.perl %s -name \"*,v\" -print";
  #else
     = "find %s -follow -name *,v -print";
  #endif

define constant $rlog-entry-divider :: <byte-string>
  = "----------------------------";
define constant $rlog-file-divider :: <byte-string>
  = "=============================================================================";


// If you don't give the current time zone to encode-time, you'll be
// screwed when you go to decode it.
define constant $current-time-zone = get-decoded-time().timezone;


// Revision stuff.  Revisions are what RCS deals with.  Versions is
// what we're converting them into.

define class <revision> (<object>)
  slot filename :: <byte-string>, required-init-keyword: #"filename";
  slot author :: <byte-string>, required-init-keyword: #"author";
  slot rev-number :: <integer>, required-init-keyword: #"revision";
  slot date-time :: <universal-time>, required-init-keyword: #"date-time";
  slot description :: <sequence>, required-init-keyword: #"description";
          // Sequence of strings
end class <revision>;

define sealed domain make (singleton(<revision>));
define sealed domain initialize (<revision>);

define inline method \< (rev1 :: <revision>, rev2 :: <revision>)
 => answer :: <boolean>;
  rev1.date-time < rev2.date-time;
end method;

define inline method chop (string :: <byte-string>) 
 => new-string :: <byte-string>;
  copy-sequence(string, end: string.size - 1);
end method chop;
  
define method rlog-file (file-name :: <byte-string>, stream :: <stream>) 
 => log :: <stretchy-vector>;
  let clean-filename
    = regexp-replace(file-name, concatenate(",v|/rcs|", $gwydion-prefix), "");
  printe(".");
  // Skip all the header crap
  block (break)
    while (#t)
      let line :: <byte-string> = read-line(stream);
      if (line = $rlog-entry-divider)
	break();
      end if;
    end while;
  end block;
  
  let log = make(<stretchy-vector>);
  block (break)
    while (#t)
      let first-line = read-line(stream);
      let (crap, something) = split("\\.", first-line);
      let rev = split("\\s", something);
      let second-line = read-line(stream);
      let date-time-string 
	= copy-sequence(second-line, start: 6, end: 23);

      let decoded-time 
	= parse-time(make(<string-stream>, contents: date-time-string),
		     "%y/%m/%d %H:%M:%S");
      // In order to encode the time, we have to make up a timezone
      // and somehow force it in there
      let time = encode-time(make(<decoded-time>, default-from: decoded-time, 
				  timezone: $current-time-zone));
// ### Debug by printf...
//      printe("\nEncoded %= as %d ", date-time-string, time);
//      printe("\ndecoded-time = %=\n", decoded-time);
//      format-time(*standard-error*, "(Internally %y/%m/%d %H:%M:%S)\n",
//		  decoded-time);
//      force-output(*standard-error*);
//      format-time(*standard-error*, "Decoded again as %y/%m/%d %H:%M:%S\n",
//		  decode-time(time, timezone: $current-time-zone));
//      force-output(*standard-error*);

      let (crap, crap2, crap3, crap4, rev-author) = split(" ", second-line);
      let rauthor :: <byte-string> = chop(rev-author);
      let descr = make(<stretchy-vector>);
      for (line :: <byte-string> = read-line(stream) then read-line(stream), 
	   until: line = $rlog-entry-divider)
	if (line = $rlog-file-divider)
	  add!(log, make(<revision>, filename: clean-filename,
			 author: rauthor,
			 date-time: time,
			 revision: string-to-integer(rev),
			 description: descr));
	  break();
	end if;
	add!(descr, line);
      end for;
      add!(log, make(<revision>, filename: clean-filename, 
		     author: rauthor,
		     date-time: time,
		     revision: string-to-integer(rev),
		     description: descr));
    end while;
  exception (<end-of-stream-error>)
    break();
  end block;
  
  log;
end method rlog-file;

define method do-all-rlogs (filenames :: <sequence>, rlog-stream :: <stream>) 
 => unified-log :: <sequence>;
  let list-of-logs = make(<stretchy-vector>);
  for (filename in filenames)
    add!(list-of-logs, rlog-file(filename, rlog-stream));
  end for;
  apply(concatenate, list-of-logs);
end method do-all-rlogs;

// Takes a collection, outputs a collection of collections
//
define method partition-by-author (revs :: <collection>) 
 => partioned-revs :: <collection>;
  let table = make(<string-table>);
  for (rev in revs)
    if (key-exists?(table, rev.author))
      table[rev.author] := add!(table[rev.author], rev);
    else
      table[rev.author] := list(rev);
    end if;
  end for;
  table;
end method partition-by-author;

// Date-time is any date-time that's close
// revisions is a sequence of the component revisions
//
define class <version> (<object>)
  slot date-time :: <universal-time>, required-init-keyword: #"date-time";
  slot author :: <byte-string>, required-init-keyword: #"author";
  slot revisions :: <sequence>, required-init-keyword: #"revisions";
end class <version>;

define sealed domain make (singleton(<version>));
define sealed domain initialize (<version>);

define constant $magic-time-interval :: <universal-time>
  = as(<universal-time>, 20 * 60);  // 20 minutes * 60 secs/minute
// We can't use encode-time because it wants all sorts of irrelevant
// crap like a day, month, and year.

// sequence of <revision>s to a sequence of <version>s.  These
// sequences probably aren't the same size; that's the whole point,
// after all.
//
define method log-to-versions (unsorted-log :: <sequence>) 
 => versions :: <sequence>;
  let log = sort(unsorted-log);  // Sorted by date-time
  let version-list = make(<stretchy-vector>);
  let rev-list :: <list> = #();  // Order of adding is important
  for (rev :: <revision> in log)
    if (rev-list == #())
      rev-list := list(rev);
    elseif (rev.date-time - rev-list.head.date-time < $magic-time-interval)
      rev-list := add!(rev-list, rev);
    else
      add!(version-list, make(<version>, revisions: rev-list,
			      date-time: rev-list.head.date-time,
			      author: rev-list.head.author));
      rev-list := list(rev);
    end if;
  end for;
  add!(version-list, make(<version>, revisions: rev-list,
			  date-time: rev-list.head.date-time,
			  author: rev-list.head.author));
  for (ver in version-list)
    check-type(ver, <version>);
  end for;
  version-list;
end method log-to-versions;  

// There's no < for sequences, so this'll have to do
//
define inline method sequence-less-than
    (seq1 :: <sequence>, seq2 :: <sequence>)
 => answer :: <boolean>;
  block (return)
    for (elt1 in seq1, elt2 in seq2)
      if (elt1 < elt2)
	return(#t);
      elseif (elt1 > elt2)
	return(#f);
      end if;
    end for;
    return(#f);
  end block;
end method sequence-less-than;

// stolen from the compiler.
//
define function path-separator? (c :: <character>) => answer :: <boolean>;
  #if (compiled-for-win32)
     (c == '/') | (c == '\\') | (c == ':');
  #else
     c == '/';
  #endif
end function path-separator?;

define function end-of-prefix (filename :: <string>) 
 => index :: <integer>;
  block (return)
    for (index from filename.size - 1 to 0 by -1)
      if (filename[index].path-separator?)
	return(index + 1);
      end if;
    end for;
    0;
  end block;
end function end-of-prefix;

define function filename-prefix (filename :: <string>)
 => prefix :: <string>;
  copy-sequence(filename, end: filename.end-of-prefix);
end function filename-prefix;

define function pathless-filename (filename :: <string>)
 => pathless-filename :: <string>;
  copy-sequence(filename, start: filename.end-of-prefix);
end function pathless-filename;

define method print-object (ver :: <version>, stream :: <stream>) => ();
  local method compare-by-descr (rev1 :: <revision>, rev2 :: <revision>) 
	 => answer :: <boolean>;
	  if (rev1.description = rev2.description)
	    rev1.filename < rev2.filename;
	  else
	    sequence-less-than(rev1.description, rev2.description);
	  end if;
	end method compare-by-descr;
  let any-revision = ver.revisions.first;
  format(stream, "%s at about ", any-revision.author);
  
  // We specify timezone: $current-time-zone because despite what the
  // documentation may claim, it doesn't actually seem to default to
  // that.
  format-time(stream, "%y/%m/%d %H:%M", 
	      decode-time(any-revision.date-time, 
			  timezone: $current-time-zone));
  new-line(stream);
  let sorted-revisions = sort(ver.revisions, test: compare-by-descr);

  // This is quite convoluted because we want to output as little as
  // possible.
  let previous-rev-description :: false-or(<sequence>) = #f;
  let filenames-and-versions :: <stretchy-vector> = make(<stretchy-vector>);
  let previous-file-prefix :: false-or(<string>) = #f;
  local method print-description () => ();
	  pprint-logical-block
	    (stream,
	     body: method (stream)
		     write(stream, "  ");
		     pprint-indent(#"current", 0, stream);
		     for (i from 0 below filenames-and-versions.size - 1)
		       let string = filenames-and-versions[i];
		       format(stream, "%s, ", string);
		       pprint-newline(#"fill", stream);
		     end for;
		     write(stream, filenames-and-versions.last);
		   end method);
	  new-line(stream);
	  pprint-logical-block
	    (stream,
	     body: method (stream)
		     write(stream, "    ");
		     pprint-indent(#"current", 0, stream);
		     for (line in previous-rev-description)
		       let (#rest words) = split("\\s", line);
		       for (i from 0 below words.size)
			 let word = words[i];
			 write(stream, word);
			 write(stream, " ");
			 pprint-newline(#"fill", stream);
		       end for;
//		       write(stream, words.last);
//		       pprint-newline(#"mandatory", stream);
		     end for;
		   end method);
	  new-line(stream);
	  previous-file-prefix := #f;
	  filenames-and-versions := make(<stretchy-vector>);	  
	end method print-description;

  for (rev in sorted-revisions)
    if (rev.description ~= previous-rev-description)
      if (previous-rev-description ~== #f) print-description(); end if;
      previous-rev-description := rev.description;
    end if;
    let prefix = if (rev.filename.filename-prefix ~= previous-file-prefix)
		   previous-file-prefix := rev.filename.filename-prefix;
		 else 
		   "";
		 end if;
    add!(filenames-and-versions,
	 format-to-string("%s%s 1.%d", prefix, 
			  rev.filename.pathless-filename, rev.rev-number));
  end for;
  print-description();
end method print-object;

define method print-versions (versions :: <sequence>) => ();
  *default-pretty?* := #t;
  *default-line-length* := 78; // defaults to 80
  for (ver in versions)
    printf("================\n%=", ver);  
    // print-object for versions outputs a newline
  end for;
end method print-versions;

define inline method printf (#rest args) => ();
  apply(format, *standard-output*, args);
//  force-output(*standard-output*);
end method printf;

define inline method printe (#rest args) => ();
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end method printe;

define method \< (ver1 :: <version>, ver2 :: <version>)
 => answer :: <boolean>;
  ver1.date-time < ver2.date-time;
end method;

// Takes a sequence of strings, an rlog stream, and a start date
// (which is ignored)
//
define method do-versioning
    (filenames :: <sequence>, rlog-stream :: <stream>, 
     start-date :: <universal-time>) 
 => ();
  printe("Reading input files");
  let unpartitioned-logs = do-all-rlogs(filenames, rlog-stream);
  printe("\nConverting to versions\n");
  let logs = partition-by-author(unpartitioned-logs);
  
  // versions-by-author is a table mapping authors to their versions
  let versions-by-author = map(log-to-versions, logs);

  // We need to convert versions-by-author into a <sequence>, 
  // and this is a convenient way to do that
  let empty-vector = make(<vector>, size: versions-by-author.size);
  for (thing :: <sequence> in versions-by-author, i from 0)
    printe("%s made %d versions\n", thing.first.author, thing.size);
    empty-vector[i] := thing;
  end for;
  let concated-versions = apply(concatenate, empty-vector);
  let all-versions = sort!(concated-versions, test: \>);
  print-versions(all-versions);
end method do-versioning;

define method show-usage () => ();
  printe("Usage:\n");
  printe("    versioner {all|mindy|compiler|libraries|tools|-dir dir}+\n");
  exit(exit-code: 1);
end method show-usage;

// Returns a sequence of strings, each string being a line in the file
// (without the newline)
//
define method slurp-stream (stream :: <stream>) => slurped :: <sequence>;
  let slurped = make(<stretchy-vector>);
  block ()
    while (#t)
      let line = read-line(stream);
      add!(slurped, line);
    end while;
  exception (<end-of-stream-error>)
    #f;
  end block;
  slurped;
end method slurp-stream;

define function make-rlog-stream (filenames :: <sequence>) 
 => stream :: <stream>;
  let index = 0;   // close over index
  local method get-next-stream () => stream :: false-or(<stream>);
	  if (index < filenames.size)
	    let (writable-pipe, readable-pipe)
	      = piped-exec(concatenate($rlog-command, " ", filenames[index]));
	    close(writable-pipe);  // we don't need it for anything...
	    index := index + 1;
	    readable-pipe;
	  else
	    #f;
	  end if;
	end method get-next-stream;

  make(<concatenated-stream>, get-next-stream: get-next-stream);
end function make-rlog-stream;

define function gwydionize (path :: <string>) => full-path :: <string>;
  concatenate($gwydion-prefix, path);
end function gwydionize;

define method main (ignored :: <byte-string>, #rest argv-sequence)
  let targets = make(<stretchy-vector>);
  let start-date = as(<universal-time>, 0);  // ### Start-date never fully worked...
  let argv = as(<deque>, argv-sequence);
  while (~argv.empty?)
    let word = pop(argv);
    select (word by \=)
      "all" =>
	add!(targets, gwydionize("mindy/rcs"));
	add!(targets, gwydionize("libraries/old-mindy-rcs"));
	add!(targets, gwydionize("compiler/rcs"));
	add!(targets, gwydionize("libraries/old-compiler-rcs"));
	add!(targets, gwydionize("tools/rcs"));
      "mindy" => 
	add!(targets, gwydionize("mindy/rcs"));
	add!(targets, gwydionize("libraries/old-mindy-rcs"));
      "compiler" =>
	add!(targets, gwydionize("compiler/rcs"));
	add!(targets, gwydionize("libraries/old-compiler-rcs"));
      "libraries" =>
	add!(targets, gwydionize("libraries/rcs"));
	add!(targets, gwydionize("libraries/old-mindy-rcs"));
	add!(targets, gwydionize("libraries/old-compiler-rcs"));
      "tools" => 
	add!(targets, gwydionize("tools/rcs"));
      "-dir" =>
	if (argv.empty?)
	  show-usage();
	else
	  add!(targets, pop(argv));
	end if;
      otherwise =>
	printe("Unrecognized argument %s", word);
	show-usage();
    end select;
  end while;

  if (targets.empty?)
    show-usage();
  end if;

  printe("Finding rcs files\n");
  let rcs-files = #[];
  for (dir in targets)
    let cmd-string = format-to-string($find-command, dir);
    let (to-find, from-find) = piped-exec(cmd-string);
    rcs-files := concatenate(rcs-files, slurp-stream(from-find));
    close(to-find);
    close(from-find);
  end for;

  let rlog-stream = make-rlog-stream(rcs-files);
  do-versioning(rcs-files, rlog-stream, start-date);
  close(rlog-stream);
  force-output(*standard-output*);
end method main;
