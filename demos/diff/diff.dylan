module: Diff-Program
author: Nick Kramer (nkramer@cs.cmu.edu)

// This file implements an algorithm that accomplishes something
// similar to the Unix diff utility.  

// Usage:
//      mindy -f diff.dbc file1 file2
define constant <script> = <sequence>;

define method slurp-file (filename :: <string>) => lines :: <sequence>;
  let stream = make(<file-stream>, locator: filename, direction: #"input");
  let lines = make(<stretchy-vector>);
  for (i from 0, 
       line = read-line(stream, on-end-of-stream: #f) 
	 then read-line(stream, on-end-of-stream: #f),
       while: line ~= #f)
      lines[i] := line;
  end for;
  lines;
end method slurp-file;

define method print-lines (prefix :: <string>, seq :: <sequence>, 
			   start :: <integer>, count :: <integer>) => ();
  for (i from start below start + count)
    if (i < seq.size)  // Kluge to avoid some unknown fencepost error
      format(*standard-output*, "%s%s\n", prefix, seq[i]);
    end if;
  end for;
end method print-lines;

// Doesn't print the second line if it's the same as the first
// Also adds 1 to all line numbers -- Dylan counts sequences
// starting from 0, but people count line numbers starting with 1.
//
define method print-line-nums (line1 :: <integer>, line2 :: <integer>) => ();
  if (line1 = line2)
    format(*standard-output*, "%d", line1 + 1);
  else
    format(*standard-output*, "%d,%d", line1 + 1, line2 + 1);
  end if;
end method print-line-nums;

define method print-entry
    (entry :: <insert-entry>, file1 :: <sequence>, file2 :: <sequence>) => ();
  format(*standard-output*, "%da", entry.dest-index + 1);
  print-line-nums(entry.source-index, entry.source-index + entry.element-count - 1);
  format(*standard-output*, "\n");
  print-lines("> ", file2, entry.source-index, entry.element-count);
end method print-entry;

define method print-entry
    (entry :: <delete-entry>, file1 :: <sequence>, file2 :: <sequence>) => ();
  print-line-nums(entry.dest-index, entry.dest-index + entry.element-count - 1);
  format(*standard-output*, "d\n");
  print-lines("< ", file1, entry.dest-index, entry.element-count);
end method print-entry;

define method print-diffs 
    (diffs :: <script>, file1 :: <sequence>, file2 :: <sequence>) => ();
  for (pointer = diffs then pointer.tail, while: pointer ~= #())
    let entry = pointer.head;

    // If two consecutive entries could be considered a "change" (ie,
    // a delete with a corresponding insert), treat them specially.
    if (pointer.tail ~= #() & instance?(entry, <delete-entry>) 
	  & instance?(pointer.tail.head, <insert-entry>)
	  & (entry.dest-index + entry.element-count - 1
	       = pointer.tail.head.dest-index))
      let entry2 = pointer.tail.head;
      print-line-nums(entry.dest-index, entry.dest-index + entry.element-count - 1);
      format(*standard-output*, "c");
      print-line-nums(entry2.source-index, 
		      entry2.source-index + entry.element-count - 1);
      format(*standard-output*, "\n");
      print-lines("< ", file1, entry.dest-index, entry.element-count);
      format(*standard-output*, "---\n");
      print-lines("> ", file2, entry2.source-index, entry2.element-count);
      pointer := pointer.tail;
    else
      print-entry(entry, file1, file2);
    end if;
    force-output(*standard-output*);
  end for;
end method print-diffs;

define method main (argv0 :: <byte-string>, #rest filenames)
  let file1 = slurp-file(filenames[0]);
  let file2 = slurp-file(filenames[1]);
  print-diffs(sequence-diff(file1, file2), file1, file2);
end method main;
