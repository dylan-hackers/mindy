module: versioner
library: versioner
author: Nick Kramer (nkramer@cs.cmu.edu)
rcs-header: $Header: /home/housel/work/rcs/gd/src/tools/versioner/versioner.dylan,v 1.5 1996/09/16 10:06:41 nkramer Exp $

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

// This program contains a <date-time>...  We now have a Time library;
// right now I'm too lazy to update this program to use it.


// Constants that seem likely to change

define constant $gwydion-prefix 
    #if (compiled-for-x86-win32)
       = "n:/gwydion/"; 
    #else
       = "/afs/cs/project/gwydion/";
    #endif

define constant $rlog-command
  #if (compiled-for-x86-win32)
     = "/tools/rcs-bin/rlog.exe";
  #else
       = "rlog";
  #endif

define constant $find-command
  #if (compiled-for-x86-win32)
     = "perl /tools/bin/unix-find.perl %s -name \"*,v\" -print";
  #else
     = "find %s -follow -name *,v -print";
  #endif

define constant $rlog-entry-divider :: <byte-string>
  = "----------------------------";
define constant $rlog-file-divider :: <byte-string>
  = "=============================================================================";


// Date-time stuff

define class <date-time> (<object>)
  slot year :: <integer>, required-init-keyword: #"year";
  slot month :: <integer>, required-init-keyword: #"month";
  slot day :: <integer>, required-init-keyword: #"day";
  slot hour :: <integer>, required-init-keyword: #"hour";
  slot minute :: <integer>, required-init-keyword: #"minute";
  slot sec :: <integer>, required-init-keyword: #"second";
end class <date-time>;

define sealed domain make (singleton(<date-time>));
define sealed domain initialize (<date-time>);

define method \< (date1 :: <date-time>, date2 :: <date-time>)
 => answer :: <boolean>;
  let d1 = vector(date1.year, date1.month, date1.day, 
		  date1.hour, date1.minute, date1.sec);
  let d2 = vector(date2.year, date2.month, date2.day, 
		  date2.hour, date2.minute, date2.sec);
  block (return)
    for (number1 in d1, number2 in d2)
      if (number1 ~== number2)
	return(number1 < number2);
      end if;
    finally
      #f;
    end for;
  end block;
end method;

define constant $days-in-month :: <vector>
  = #[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
//    Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

// Doesn't handle leap-years
//
define method days-to-month (days :: <integer>)
 => (month :: <integer>, days-left :: <integer>);
  let month = 0;
  let days-left = days;
  while (~ (days-left < $days-in-month[month]))
    days-left := days-left - $days-in-month[month];
    month := month + 1;
  end while;
  values(month, days-left);
end method days-to-month;

define inline method month-to-days (month :: <integer>) => days :: <integer>;
  reduce1(\+, copy-sequence($days-in-month, end: month));
end method month-to-days;

define inline method \- (date1 :: <date-time>, date2 :: <date-time>)
 => result :: <date-time>;
  let years = date1.year - date2.year;
  let days = month-to-days(date1.month) - month-to-days(date2.month)
               + date1.day - date2.day;
  let hours = date1.hour - date2.hour;
  let minutes = date1.minute - date2.minute;
  let seconds = date1.sec - date2.sec;
  if (seconds < 0)
    seconds := seconds + 60;
    minutes := minutes - 1;
  end if;
  if (minutes < 0)
    minutes := minutes + 60;
    hours := hours - 1;
  end if;
  if (hours < 0)
    hours := hours + 24;
    days := days - 1;
  end if;
  if (days < 0)
    days := days + 365;
    years := years - 1;
  end if;
  let (months, real-days) = days-to-month(days);
  make(<date-time>, year: years, month: months, day: real-days,
       hour: hours, minute: minutes, second: seconds);
end method;
	       
// RCS format date and time: "yy/mm/dd hh:mm:ss"
//
define method parse-date-time (string :: <byte-string>) 
 => date-time :: <date-time>;
  let (date, time) = split(" ", string);
  let (y-string, mon-string, d-string) = split("/", date);
  let (h-string, min-string, s-string) = split(":", time);
  make(<date-time>, 
       year: string-to-integer(y-string),
       month: string-to-integer(mon-string), 
       day: string-to-integer(d-string),
       hour: string-to-integer(h-string), 
       minute: string-to-integer(min-string), 
       second: string-to-integer(s-string));
end method parse-date-time;


// Revision stuff.  Revisions are what RCS deals with.  Versions is
// what we're converting them into.

define class <revision> (<object>)
  slot filename :: <byte-string>, required-init-keyword: #"filename";
  slot author :: <byte-string>, required-init-keyword: #"author";
  slot rev-number :: <integer>, required-init-keyword: #"revision";
  slot date-time :: <date-time>, required-init-keyword: #"date-time";
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
      let (crap, crap2, crap3, crap4, rev-author) = split(" ", second-line);
      let rauthor :: <byte-string> = chop(rev-author);
      let descr = make(<stretchy-vector>);
      for (line :: <byte-string> = read-line(stream) then read-line(stream), 
	   until: line = $rlog-entry-divider)
	if (line = $rlog-file-divider)
	  add!(log, make(<revision>, filename: clean-filename,
			 author: rauthor,
			 date-time: parse-date-time(date-time-string),
			 revision: string-to-integer(rev),
			 description: descr));
	  break();
	end if;
	add!(descr, line);
      end for;
      add!(log, make(<revision>, filename: clean-filename, 
		     author: rauthor,
		     date-time: parse-date-time(date-time-string),
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
  slot date-time :: <date-time>, required-init-keyword: #"date-time";
  slot author :: <byte-string>, required-init-keyword: #"author";
  slot revisions :: <sequence>, required-init-keyword: #"revisions";
end class <version>;

define sealed domain make (singleton(<version>));
define sealed domain initialize (<version>);

define constant $magic-time-interval :: <date-time>
  = make(<date-time>, year: 0, month: 0, 
	 day: 0, hour: 0, minute: 20, second: 0);

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
  #if (compiled-for-x86-win32)
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
  format(stream, "%s at about %d/%d/%d %d:%d\n",
	 any-revision.author, any-revision.date-time.year,
	 any-revision.date-time.month, any-revision.date-time.day, 
	 any-revision.date-time.hour, any-revision.date-time.minute);
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
     start-date :: <date-time>) 
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

  make(<combination-stream>, get-next-stream: get-next-stream);
end function make-rlog-stream;

define function gwydionize (path :: <string>) => full-path :: <string>;
  concatenate($gwydion-prefix, path);
end function gwydionize;

define method main (ignored :: <byte-string>, #rest argv-sequence)
  let targets = make(<stretchy-vector>);
  let start-date = parse-date-time("00/00/00 00:00:00");
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

  printe("Running rlog and slurping its output\n");
  let rlog-stream = make-rlog-stream(rcs-files);
  do-versioning(rcs-files, rlog-stream, start-date);
  close(rlog-stream);
  force-output(*standard-output*);
end method main;

#if (~mindy)
define method %main (argc :: <integer>, argv :: <raw-pointer>) => ();
  let args = make(<vector>, size: argc);
  for (index :: <integer> from 0 below argc)
    let argptr = pointer-deref(#"ptr", argv,
			       index * c-expr(#"int", "sizeof(void *)"));
    args[index] := import-string(argptr);
  end for;
  apply(main, args);
end method %main;
#endif
