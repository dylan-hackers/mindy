module: dylan-viscera
rcs-header: $Header: /scm/cvs/src/d2c/runtime/dylan/sort.dylan,v 1.3 2002/12/11 14:38:41 bruce Exp $

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
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
define inline method swap-elements! (vector :: <vector>,
                                     key1 :: <integer>,
                                     key2 :: <integer>)
 => ();
  let element1 = vector[key1];
  let element2 = vector[key2];
  vector[key1] := element2;
  vector[key2] := element1;
end method swap-elements!;


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
     test :: <function>,
     first :: <integer>,
     last :: <integer>)
 => ();
  for (current-key :: <integer> from first + 1 below last)
    let current-element = vector[current-key];
    for (insert-key :: <integer> from current-key - 1 to first by -1,
	 while: test(current-element, vector[insert-key]))
      vector[insert-key + 1] := vector[insert-key];
    finally
      vector[insert-key + 1] := current-element;
    end for;
  end for;
end method insertion-sort!;



//// Recursive Sorting Algorithms

// $small-sort-size$ -- internal
//
// The simple sorts can be used to sort the small subsequences generated
// by the recursive algorithms.  This parameter defines how small the
// subsequence should be before the simple sorts are called.  (The simple
// sorts can be turned off by setting this to 0.)
//
define constant $small-sort-size$ :: <integer> = 15;

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
     test :: <function>,
     first :: <integer>,
     middle :: <integer>,
     last :: <integer>)
 => ();
  let merge-size :: <integer> = last - first;

  // BGH -- this does not need to cons!!  Can merge in-place.  Later.
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
//
// Caution: merge-sort! leaves the vector not *quite* sorted, to be
// cleaned up afterwards by insertion-sort!

define method merge-sort!
    (vector :: <simple-object-vector>,
     test :: <function>,
     first :: <integer>,
     last :: <integer>)
 => ();
  if ((last - first) > $small-sort-size$)
    let middle :: <integer> = ash(first + last, -1);
    merge-sort!(vector, test, first, middle);
    merge-sort!(vector, test, middle, last);
    merge!(vector, test, first, middle, last);
  end if;
end method merge-sort!;


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
define inline method median-of-three
    (vec :: <simple-object-vector>,
     first :: <integer>,
     last :: <integer>,
     less-than :: <function>)
 => (pivot-index :: <integer>);
  let left = vec[first];
  let right = vec[last - 1];
  let middle :: <integer> = ash(first + last, -1);
  let mid = vec[middle];

  if (less-than(left, mid))
    if (less-than(mid, right))
      middle
    elseif (less-than(left, right))
      last
    else
      first
    end
  else
    if (less-than(left, right))
      first
    elseif (less-than(mid, right))
      last
    else
      middle
    end
  end;
end method median-of-three;


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
define inline method partition!
    (vector :: <simple-object-vector>,
     test :: <function>,
     first :: <integer>,
     last :: <integer>)
 => (middle :: <integer>);
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
// Caution: quick-sort! leaves the vector not *quite* sorted, to be
// cleaned up afterwards by insertion-sort!
//
// The depth-charge saves us from O(N^2) worse-case behaviour.  Median-of-3
// pivoting makes it statistically unlikely, depth-charge prevents it
// entirely by punting to a GUARANTEED O(N.log(N)) method while we've still
// only done O(N.log(N)) work ourselves.  Traditionally you use heapsort,
// but mergesort will do, since we have it already.

define function qsort-depth-allowed(i :: <integer>)
 => (depth :: <integer>);
  for (d from 0,
       n = i then ash(n, -1),
       while: n > 0)
  finally
    // 95/5 split on average before bailing
    16 * d;
  end;
end function qsort-depth-allowed;


define method quick-sort!
    (vector :: <simple-object-vector>,
     test :: <function>,
     first :: <integer>,
     last :: <integer>,
     depth-charge :: <integer>)
 => ();
  block (return)
    while ((last - first) > $small-sort-size$)
      if (depth-charge > 0)
        depth-charge := depth-charge - 1;
        let middle = partition!(vector, test, first, last);
        if ((middle - first) < (last - middle))
          quick-sort!(vector, test, first, middle, depth-charge);
          first := middle + 1;
        else
          quick-sort!(vector, test, middle + 1, last, depth-charge);
          last := middle;
        end;
      else
        merge-sort!(vector, test, first, last);
        return();
      end;
    end while;
  end block;
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
  let sz = vector.size;
  if (stable)
    merge-sort!(vector, test, 0, sz);
  else
    quick-sort!(vector, test, 0, sz, sz.qsort-depth-allowed);
  end if;
  insertion-sort!(vector, test, 0, sz);
  as(type-for-copy(sequence), vector);
end method sort!;


define method sort!
    (vector :: <simple-object-vector>, #key test :: <function> = \<,
     stable)
 => (vector :: <simple-object-vector>);
  let sz = vector.size;
  if (stable)
    merge-sort!(vector, test, 0, sz);
  else
    quick-sort!(vector, test, 0, sz, sz.qsort-depth-allowed);
  end if;
  insertion-sort!(vector, test, 0, sz);
  vector;
end method sort!;


define method sort!
    (vector :: <stretchy-object-vector>, #key test :: <function> = \<,
     stable)
 => (vector :: <stretchy-object-vector>);
  // just sort the contained <simple-object-vector>!
  // NB sort to size of stretchy NOT size of ssv-data, because of
  // unused elements
  let sz = vector.size;
  if (stable)
    merge-sort!(vector.ssv-data, test, 0, sz);
  else
    quick-sort!(vector.ssv-data, test, 0, sz, sz.qsort-depth-allowed);
  end if;
  insertion-sort!(vector.ssv-data, test, 0, sz);
  vector;
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
