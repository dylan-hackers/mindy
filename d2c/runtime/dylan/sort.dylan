module: dylan-viscera
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/sort.dylan,v 1.1 1998/05/03 19:55:40 andreas Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
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
//
//  This file contains definitions for sorting utilities for Dylan
//  sequences.
//
//  These are the default methods for sorting sequences.  The way they work
//  is to coerce the sequence to be sorted to a vector.  This allows easier
//  access to the elements of the sequence and the use of typical sorting
//  algorithms.  When the sorting is complete, the vector is coerced back
//  to the class for copy of the original sequence.
//
//  Several simple sorting functions are defined first.  These implement
//  selection sort and insertion sort.  These two algorithms work well for
//  small sequences, but are too inefficient for large tasks.  Two more
//  efficient algorithms are also implemented: merge sort and quick sort.
//  The more efficient sorts can use the simple sorting algorithms for
//  small subsequences.  (This is controlled by the $SMALL-SORT-SIZE$
//  constant.)
//
//  One common feature of the sort functions which sort in place is the
//  keyword arguments START and END.  These keywords tell the sort function
//  which portion of the vector to operate upon.  Thus recursive calls or
//  calls to other sort functions can sort different segments of the same
//  vector through use of keys.  The START key is always an inclusive bound
//  for the beginning of the subsequence; the END key is always an
//  exclusive bound for the end of the subsequence.
//
//  Written by David Pierce
//


//// Simple Sorting Algorithms

// swap-elements! -- internal
//
// Swaps two elements in a vector.
//
define method swap-elements! (vector :: <simple-object-vector>, key1 :: <integer>,
			      key2 :: <integer>)
  let element1 = vector[key1];
  let element2 = vector[key2];
  vector[key1] := element2;
  vector[key2] := element1;
end method swap-elements!;

// selection-sort! -- internal
//
// Selection sort sorts the vector from the beginning to the end.  At any
// point the vector is sorted up to a certain position.  From this
// position the remainder of the vector is searched for the next largest
// element, and this element is moved to the position.  After this has
// been done for each position in the vector, the vector is sorted.
//
// SELECTION-SORT! takes a TEST key to specify the ascending order or
// elements, a START key to specify where in the vector to begin the sort,
// and an END key to specify where to end the sort.  (As usual in Dylan,
// START is an inclusive bound while END is an exclusive bound.)
//
// Selection sort is NOT stable, but it does sort in place.
//
define method selection-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
          end: last :: <integer> = vector.size)
  for (current-key :: <integer> from first below last)
    for (search-key :: <integer> from current-key + 1 below last,
	 select-key :: <integer> = current-key
	   then if (test(vector[search-key], vector[select-key])) search-key
		else select-key
		end if)
    finally
      swap-elements!(vector, current-key, select-key)
    end for;
  end for;
  vector;
end method selection-sort!;

// selection-sort -- internal
//
// This version of selection sort does not modify the original vector.  It
// calls the destructive version on copy of the vector.
//
define method selection-sort
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
          end: last :: <integer> = vector.size)
  let sort-vector = copy-sequence(vector, start: first, end: last);
  selection-sort!(sort-vector, test: test);
end method selection-sort;

// insertion-sort! -- internal
//
// Insertion sort also maintains the invariant that the vector is sorted
// up to a current position.  The next element after this position is
// inserted into the sorted part of the vector, pushing larger elements up
// if necessary.
//
// INSERTION-SORT! accepts the same keys as SELECTION-SORT!  Insertion
// sort is stable, and this method sorts the vector in place.
//
define method insertion-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
     end: last :: <integer> = vector.size)
  for (current-key :: <integer> from first + 1 below last)
    let current-element = vector[current-key];
    for (insert-key :: <integer> from current-key - 1 to first by -1,
	 while: test(current-element, vector[insert-key]))
      vector[insert-key + 1] := vector[insert-key];
    finally
      vector[insert-key + 1] := current-element;
    end for;
  end for;
  vector;
end method insertion-sort!;

// insertion-sort -- internal
//
// This version of insertion sort does not modify the original vector.  It
// calls the destructive version on copy of the vector.
//
define method insertion-sort
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
     end: last :: <integer> = vector.size)
  let sort-vector = copy-sequence(vector, start: first, end: last);
  insertion-sort!(sort-vector, test: test);
end method insertion-sort;



//// Recursive Sorting Algorithms

// $small-sort-size$ -- internal
//
// The simple sorts can be used to sort the small subsequences generated
// by the recursive algorithms.  This parameter defines how small the
// subsequence should be before the simple sorts are called.  (The simple
// sorts can be turned off by setting this to 0.)
//
define variable $small-sort-size$ :: <integer> = 10;

// Merge Sort
//
// Merge sort is a divide-and-conquer algorithm.  It divides the vector in
// half and recursively calls merge sort on the halves.  When the calls
// return, the halves are sorted, and they are merged together.
//
// Merge sort is stable.  There is a version that sorts in place, and
// modifies the original vector.  This uses a small amount of extra space
// in the process (it merges the sorted halves into a new vector and then
// copies back to the original).  There is also a version that uses as
// much extra space as it needs, and sorts non-destructively.

// merge! -- internal
//
// This function merges two contiguous sorted subsequences of a vector.
// It accepts four keyword arguments in addition to a vector.  TEST
// specifies the ascending order for the sort/merge.  START and MIDDLE
// give the beginnings of the two subsequences, and END is the end of the
// second subsequence.  (Again, START and MIDDLE are inclusive bounds for
// the subsequences, and MIDDLE and END are exclusive end bounds.  (The
// subsequences must be contiguous in the vector.))
//
// Again, merging assumes the subsequences are sorted.  Two pointers run
// down each subsequence.  The smallest of the two elements is copied to a
// merge vector and the pointer for its subsequence is incremented.  This
// continues until both pointers reach the end of the subsequences.
// Finally the merge vector is copied into the original vector in
// position.
//
define method merge!
    (vector :: <simple-object-vector>,
     #key test: test :: <function>, start: first :: <integer>,
          middle: middle :: <integer>, end: last :: <integer>)
  let merge-size :: <integer> = last - first;
  let merge-vector = make(<simple-object-vector>, size: merge-size);
  let start-key :: <integer> = first;
  let middle-key :: <integer> = middle;
  for (merge-key :: <integer> from 0 below merge-size)
    case
      start-key >= middle =>
	merge-vector[merge-key] := vector[middle-key];
        middle-key := middle-key + 1;
      middle-key >= last =>
	merge-vector[merge-key] := vector[start-key];
        start-key := start-key + 1;
      test(vector[middle-key], vector[start-key]) =>
	merge-vector[merge-key] := vector[middle-key];
        middle-key := middle-key + 1;
      otherwise =>
	merge-vector[merge-key] := vector[start-key];
        start-key := start-key + 1;
    end case;
  end for;
  for (merge-key :: <integer> from 0 below merge-size,
       copy-key :: <integer> from first)
    vector[copy-key] := merge-vector[merge-key]
  end for;
end method merge!;


// merge-sort! -- internal
//
// Sorts a vector in place using merge sort.  Computes the middle of the
// vector and recursively calls MERGE-SORT! on both halves.  Merges the
// halves when both calls return.  If the vector is smaller than
// $SMALL-SORT-SIZE$, however, INSERTION-SORT! is used instead.  Recursive
// calls to MERGE-SORT! terminate (by doing nothing) when the vector to be
// sorted contains only one element (or when insertion sort is used).
//
// Three keywords are accepted by this function.  The TEST specifies the
// ascending order for the sort, and START and END give the bounds of the
// subvector to be operated on in VECTOR.
//
define method merge-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
          end: last :: <integer> = vector.size)
  let (div, mod) = floor/(last - first, 2);
  let middle :: <integer> = first + div;
  case
    (last - first) < $small-sort-size$ =>
      insertion-sort!(vector, test: test, start: first, end: last);
    (last - first) > 1 =>
      merge-sort!(vector, test: test, start: first, end: middle);
      merge-sort!(vector, test: test, start: middle, end: last);
      merge!(vector, start: first, middle: middle, end: last, test: test);
    otherwise => #f;
  end case;
  vector;
end method merge-sort!;

// merge -- internal
//
// This non-destructive version of MERGE merges two vectors as does
// MERGE!, but the two vectors are given separately.  Also, the merge
// vector is simply returned, rather than copied into any of the
// arguments.  The TEST key gives the test for ascending order.
//
define method merge
    (vector1 :: <simple-object-vector>, vector2 :: <simple-object-vector>,
     #key test :: <function> = \<)
  let size1 :: <integer> = size(vector1);
  let size2 :: <integer> = size(vector2);
  let merge-size :: <integer> = size1 + size2;
  let merge-vector = make(<simple-object-vector>, size: merge-size);
  let key1 :: <integer> = 0;
  let key2 :: <integer> = 0;
  for (merge-key :: <integer> from 0 below merge-size)
    case
      key1 >= size1 =>
	merge-vector[merge-key] := vector2[key2];
	key2 := key2 + 1;
      key2 >= size2 =>
	merge-vector[merge-key] := vector1[key1];
        key1 := key1 + 1;
      test(vector2[key2], vector1[key1]) =>
	merge-vector[merge-key] := vector2[key2];
	key2 := key2 + 1;
      otherwise =>
	merge-vector[merge-key] := vector1[key1];
	key1 := key1 + 1;
    end case;
  end for;
  merge-vector;
end method merge;

// merge-sort -- internal
//
// This works the same as MERGE-SORT!, except that the recursive calls
// terminate by returning a new vector rather than by changing the old.
// Thus, the recursion might terminate when the vector to be sorted
// contains less than two elements, and a call to COPY-SEQUENCE is
// returned; or when the insertion sort kicks in.  The non-destructive
// version of insertion sort is used.
//
// MERGE-SORT still takes the keywords START and END, because the
// recursive calls are still made on two halves of the same vector.  (The
// halves of the vector are not copied before the calls.)
//
define method merge-sort
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
          end: last :: <integer> = vector.size)
  let (div, mod) = floor/(last - first, 2);
  let middle :: <integer> = first + div;
  case
    (last - first) < $small-sort-size$ =>
      insertion-sort(vector, test: test, start: first, end: last);
    (last - first) > 1 =>
      merge(merge-sort(vector, test: test, start: first, end: middle),
	    merge-sort(vector, test: test, start: middle, end: last),
	    test: test);
    otherwise =>
      copy-sequence(vector, start: first, end: last);
  end case;
end method merge-sort;

// Quick Sort
//
// Quick sort is also a divide-and-conquer algorithm.  It partitions the
// vector by choosing a pivot, and separating elements smaller than the
// pivot from elements larger than the pivot.  Then quick sort is called
// recursively on the two subsequences to sort them in place.  When the
// recursive calls return, the vector is sorted, because all the elements
// in the first subsequence are smaller than those in the second.
//
// Quick sort sorts in place, destructively, but it is not stable.

// median-of-three -- internal
//
// Pick the index of the pivot point by picking the index corresponding to
// median(vec[first], vec[middle], vec[last - 1]).  Note: In accordance with
// convention, "last" is an exclusive bound.
//
define constant median-of-three = method 
    (vec :: <simple-object-vector>, first :: <integer>,
     last :: <integer>, less-than :: <function>)
 => pivot-index :: <integer>;
  let first-elem = vec[first];
  let last-elem = vec[last - 1];
  let middle :: <integer> = truncate/(first + last, 2);
  let middle-elem = vec[middle];
  if (less-than(first-elem, last-elem))
    if (less-than(middle-elem, last-elem))
      middle;
    else 
      last;
    end if;
  else  // last-elem <= first-elem
    if (less-than(middle-elem, first-elem))
      middle;
    else
      first;
    end if;
  end if;
end method;

// partition! -- internal
//
// Partitions a vector and returns the partition position.  The pivot
// element is chosen by the median-of-three method.  Pointers are
// started at the beginning and end of the vector.  The "small" pointer
// moves forward over elements smaller than the pivot element, and stops
// at those larger.  The "large" point moves backward over elements larger
// than the pivot element, and stops at those smaller.  The two elements
// at the places where the pointers stop are swapped.  This continues
// until the pointers cross each other.  Then the small pointer is
// returned as the partition position.
//
// PARTITION! takes the usual keyword arguments TEST, START, and END.
//
define method partition!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
          end: last :: <integer> = vector.size)
  let pivot-key = median-of-three(vector, first, last - 1, test);
  let pivot-element = vector[pivot-key];
  let small-key :: <integer> = first;
  let large-key :: <integer> = last - 1;
  block (break-while)
    while (#t)
      while (test(vector[small-key], pivot-element))
	small-key := small-key + 1;
      end while;
      while (test(pivot-element, vector[large-key]))
	large-key := large-key - 1;
      end while;
      unless (small-key < large-key)
	break-while();
      end unless;
      swap-elements!(vector, small-key, large-key);
      small-key := small-key + 1;
      large-key := large-key - 1;
    end while;
  end block;
  small-key;
end method partition!;

// quick-sort! -- internal
//
// Sorts a vector in place using quick sort.  The vector is partitioned by
// PARTITION!.  The two subsequences formed by START up to the partition
// position and from there to END are sorted recursively.  The recursion
// terminates if the vector has less than two elements, and nothing is
// done; or if the size of the subvector is small and INSERTION-SORT! is
// called on it.
//
// QUICK-SORT! takes the usual keyword arguments TEST, START, and END.
//
define method quick-sort!
    (vector :: <simple-object-vector>,
     #key test :: <function> = \<, start: first :: <integer> = 0,
          end: last :: <integer> = vector.size)
  case
    (last - first) < $small-sort-size$ =>
      insertion-sort!(vector, test: test, start: first, end: last);
    (last - first) > 1 =>
      let middle = partition!(vector, test: test, start: first, end: last);
      quick-sort!(vector, test: test, start: first, end: middle);
      quick-sort!(vector, test: test, start: middle, end: last);
    otherwise => #f;
  end case;
  vector;
end method quick-sort!;



//// Sort for Sequences

// sort! -- public
//
// Returns a sorted sequence, possibly modifying the original.  This is
// the default method for sequences.  It accepts two keyword arguments.
// The TEST key specifies the ascending order for the sort.  The STABLE
// key indicates whether the sort should be a stable sorting algorithm or
// whether it does not matter.
//
// The sequence is sorted by coercing it to a vector, and using the
// sorting functions for vectors defined above.  After the vector is
// sorted it is coerced to the class for copy of the original sequence.
//
define method sort!
    (sequence :: <sequence>, #key test :: <function> = \<,
     stable)
 => (new-seq :: <sequence>);
  let vector = as(<simple-object-vector>, sequence);
  let result = if (stable) merge-sort!(vector, test: test);
	       else quick-sort!(vector, test: test);
	       end if;
  as(type-for-copy(sequence), result);
end method sort!;

// sort -- public
//
// Returns a new sorted sequence from the original sequence.  Calls SORT!
// on a copy of the original sequence.
//
define method sort
    (sequence :: <sequence>, #key test :: <function> = \<, stable)
 => (result :: <sequence>);
  sort!(copy-sequence(sequence), test: test, stable: stable);
end method sort;
