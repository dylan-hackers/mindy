module:     Inspector
author:     Russell M. Schaaf (rsbe@cs.cmu.edu)
synopsis:   Interactive object inspector/class browser
copyright:  See below.
rcs-header: $Header: /home/housel/work/rcs/gd/src/mindy/libraries/inspector/Attic/inspector.dylan,v 1.3 1996/03/19 23:46:09 nkramer Exp $

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

// Display-object-info will take an object, and print information about the
// object to "display-stream" which defaults to *standard-output* from the
// streams module.  Unless told otherwise using the key arguments,
// display-object-info will print out a comprehensive list of information
// about the object.  Display-object-info returns nothing, all of what it
// does is a side effect.
//
define generic display-object-info(object :: <object>,
				   #key display-stream,
				   display-print-level,
				   display-print-length,
				   display-elements?,
				   display-slots?,
				   display-superclasses?,
				   display-subclasses?);

// Inspect is an interactive object inspector.  The inspect function defines
// the interface and most of the implementation of the inspector.  This is
// the exported function that is used to inspect objects, and all of the key
// arguments here are passed on to other functions.  Inspect will print all
// output to the *standard-output* stream in the streams module unless told
// otherwise, using the inspect-stream keyword.  Inspect currently takes all
// input from *standard-input*.  Inspect returns nothing, all of what it does
// is a side effect.
//
define generic inspect(object, #key inspect-stream,
		       inspect-print-length,
		       inspect-print-level,
		       inspect-elements?,
		       inspect-slots?,
		       inspect-superclasses?,
		       inspect-subclasses?);

// Object-info is called from within inspect, it is used like a limited
// display-object-info, printing only information relavent to the object
// being inspected.  If the object is an instance of a class, it will print
// the class name, elements, and slots.  If the object is a class, it will
// print slots, subclasses, and superclasses.  Object-info returns a
// <stretchy-vector> which contains the objects that were printed out, in the
// order in which they were printed.
//
define generic object-info(object, #key inspect-stream,
			   inspect-print-length,
			   inspect-print-level,
			   inspect-elements?,
			   inspect-slots?,
			   inspect-superclasses?,
			   inspect-subclasses?)
=> info-vect :: <stretchy-vector>;

define method display-one-object(object :: <singleton>,
				 #key display-stream = *standard-output*,
				 display-print-level = 1,
				 display-print-length = 5)
   write("singleton (", display-stream);
  print(object.singleton-object, display-stream,
	level: display-print-level, length: display-print-length);
  write(")", display-stream);
end method display-one-object;

define method display-one-object(object :: <limited-integer>,
				 #key display-stream = *standard-output*,
				 display-print-level = 1,
				 display-print-length = 5)
  write("<limited-integer> (", display-stream);
  let l-i-min = object.limited-integer-minimum;
  let l-i-max = object.limited-integer-maximum;
  if (l-i-min)
    if (l-i-max)
      print(l-i-min, display-stream);
      write(" <= x <= ", display-stream);
      print(l-i-max, display-stream);
      write(")", display-stream);
    else
      print(l-i-min, display-stream);
      write(" <= x)", display-stream);
    end if;
  else
    if (l-i-max)
      write("x <= ", display-stream);
      print(l-i-max, display-stream);
      write(")", display-stream);
    else
      write(")", display-stream);
    end if;
  end if;
end method display-one-object;

define method display-one-object(object :: <function>,
				 #key display-stream = *standard-output*,
				 display-print-level = 1,
				 display-print-length = 5)
  if (object.function-name)
    write(as(<byte-string>, object.object-class.class-name), display-stream);
    write(" ", display-stream);
    write(as(<byte-string>, object.function-name), display-stream);
  else
    write("unnamed ", display-stream);
    write(as(<byte-string>, object.object-class.class-name), display-stream);
  end if;
  write("(", display-stream);
  let (args, rest, kwd) = object.function-arguments;
  if (args ~= 0)
    let mthd-specializers = object.function-specializers;
    display-one-object(mthd-specializers[0],
		       display-stream: display-stream);
    for (j from 1 below args)
      write(", ", display-stream);
      display-one-object(mthd-specializers[j],
			 display-stream: display-stream);
    end for;
  end if;
  write(")", display-stream);
end method display-one-object;

define method display-one-object(object :: <class>,
				 #key display-stream = *standard-output*,
				 display-print-level = 1,
				 display-print-length = 5)
  write(as(<byte-string>, object.class-name), display-stream);
end method display-one-object;

define method display-one-object(object :: <object>,
				 #key display-stream = *standard-output*,
				 display-print-level = 1,
				 display-print-length = 5)
  write(as(<byte-string>, object.object-class.class-name), display-stream);
end method display-one-object;

define method display-one-object(object :: <union>,
				 #key display-stream = *standard-output*,
				 display-print-level = 1,
				 display-print-length = 5)
  write("union of types", display-stream);
end method display-one-object;


define method display-object-info (object :: <singleton>,
				   #key display-stream = *standard-output*,
				   display-print-level = 1,
				   display-print-length = 5,
				   display-elements? = #t,
				   display-slots? = #t,
				   display-superclasses? = #t,
				   display-subclasses? = #t)
  write("Singleton of class ", display-stream);
  write-line(as(<byte-string>, object.singleton-object.object-class.class-name)
	       , display-stream);
  write("  Value: ", display-stream);
  print(object.singleton-object, display-stream, level: display-print-level,
	length: display-print-length);
  write-line("", display-stream);
end method display-object-info;

define method display-object-info (object :: <union>,
				   #key display-stream = *standard-output*,
				   display-print-level = 1,
				   display-print-length = 5,
				   display-elements? = #t,
				   display-slots? = #t,
				   display-superclasses? = #t,
				   display-subclasses? = #t)
  write-line("Union of types", display-stream);
  force-output(display-stream);
  for (i in object.union-members)
    write("  ", display-stream);
    display-one-object(i, display-stream: display-stream,
		       display-print-length: display-print-length,
		       display-print-level: display-print-level);
    write-line("", display-stream);
  end for;
  write-line("", display-stream);
end method display-object-info;

define method display-object-info (object :: <function>,
				   #key display-stream = *standard-output*,
				   display-print-level = 1,
				   display-print-length = 5,
				   display-elements? = #t,
				   display-slots? = #t,
				   display-superclasses? = #t,
				   display-subclasses? = #t)
  if (object.function-name)
    if (instance?(object, <generic-function>))
      write("Generic Function ", display-stream);
    else
      write("Method ", display-stream);
    end if;
    print(as(<byte-string>, object.function-name), display-stream);
    write-line("", display-stream);
  else
    write("Unnamed function ", display-stream);
    print(object.object-address, display-stream);
    write-line("", display-stream);
  end if;

// Didn't work before.  Maybe now we can have some cool function inspection...

  write-line(" Required  Arguments:", display-stream);
  let (args, rest, kwd) = object.function-arguments;
  if (args ~= 0)
    for (i in object.function-specializers)
      write("  ", display-stream);
      display-one-object(i, display-stream: display-stream,
			 display-print-length: display-print-length,
			 display-print-level: display-print-level);
      write-line("", display-stream);
    end for;
    if (rest)
      write-line("  #rest", display-stream);
    end if;
  else
    if (rest)
      write-line("  #rest", display-stream);
    else
      write-line("  none", display-stream);
    end if;
  end if;
  
  write-line(" Keywords:", display-stream);
  if (~kwd)
    write-line("  none", display-stream);
  else
    if (kwd = #"all")
      write-line("  all-keys", display-stream);
    else
      for (i in kwd)
	write("  ", display-stream);
	write(as(<byte-string>, i), display-stream);
	write-line(":", display-stream);
      end for;
    end if;
  end if;
  
  write-line(" Returns:", display-stream);
  let (return-types, rest-types) = object.function-return-values;
  if (~return-types.empty?)
    for (i in return-types)
      if (instance?(i, <union>))
	write-line("  union of types:", display-stream);
	for (j in i.union-members)
	  write-line("  ", display-stream);
	  display-one-object(j, display-stream: display-stream,
			     display-print-length: display-print-length,
			     display-print-level: display-print-level);
	end for;
      else
	write("  ", display-stream);
	display-one-object(i, display-stream: display-stream,
			   display-print-length: display-print-length,
			   display-print-level: display-print-level);
	write-line("", display-stream);
      end if;
    end for;
  end if;
  if (rest-types)
    write-line("  #rest: ", display-stream);
    write("   ", display-stream);
    write-line(as(<byte-string>, rest-types.class-name), display-stream);
  end if;
  if (return-types.empty? & ~rest-types)
    write-line("  none", display-stream);
  end if;

  // If it's a generic function, display all of the methods.  
  if (instance?(object, <generic-function>))
    write(" Methods:", display-stream);
    let mthds = generic-function-methods(object);
    if (~mthds.empty?)
      write-line("", display-stream);
      for (i in mthds)
	write("  ", display-stream);
	if (i.function-name)
	  write(as(<byte-string>, i.function-name), display-stream);
	else
	  write("unnamed method", display-stream);
	end if;
	write("(", display-stream);
	if (args ~= 0)
	  let mthd-specializers = i.function-specializers;
	  display-one-object(mthd-specializers[0],
			     display-stream: display-stream);
	  for (j from 1 below args)
	    write(", ", display-stream);
	    display-one-object(mthd-specializers[j],
			       display-stream: display-stream);
	  end for;
	end if;
	write-line(")", display-stream);
      end for;
    else
      write-line("  none", display-stream);
    end if;
  end if;

  write-line("", display-stream);  
end method display-object-info;

define method display-object-info (object :: <object>,
				   #key display-stream = *standard-output*,
				   display-print-level = 1,
				   display-print-length = 5,
				   display-elements? = #t,
				   display-slots? = #t,
				   display-superclasses? = #t,
				   display-subclasses? = #t)

  let class-of-object = object.object-class;

  // Print "Class <foo>" or "Instance of <foo>" when appropriate, also set
  // class-of-object to be <foo>, regardless of whether object was an instance
  // of <foo> or <foo> itself.  This is used when getting information about
  // <foo>,
  select (class-of-object by subtype?)
//  if (subtype?(class-of-object, <class>))
    <union> =>
      write-line("Union of types: ", display-stream);
    <class> =>
      write("Class ", display-stream);
      write-line(as(<byte-string>, object.class-name), display-stream);
      class-of-object := object;
    <singleton> =>
      write("Singleton with value (", display-stream);
      print(object.singleton-object, display-stream,
	    level: display-print-level, length: display-print-length);
      write-line(")", display-stream);
    <function> =>
      write("Function ", display-stream);
      print(as(<byte-string>, object.function-name), display-stream,
	    length: display-print-length, level: display-print-level);
      write-line("", display-stream);
    otherwise =>
      write("An instance of class ", display-stream);
      write-line(as(<byte-string>, object.object-class.class-name),
		 display-stream);
      if (class-of-object.slot-descriptors.empty?)
	write-line(" Current value:", display-stream);
	write("  ", display-stream);
	print(object, display-stream, level: display-print-level,
	      length: display-print-length);
	write-line("", display-stream);
      end if;
  end select;

	

  // Routine for displaying the slots of "object"
  if (display-slots?)
    write-line(" Slots:", display-stream);
    // get a list of all of the slots
    let slot-list = class-of-object.slot-descriptors;
    if (~slot-list.empty?)
      // Print all of the slots
      for (slot-item in slot-list)
	write("  ", display-stream);
	write(as(<byte-string>, slot-item.slot-name).as-uppercase,
	      display-stream);
	write(": ", display-stream);
	display-one-object(slot-item.slot-type, display-stream: display-stream,
			  display-print-length: display-print-length,
			  display-print-level: display-print-level);
	// If the object is an instance of a class, print the values of
	// all it's slots
	if (class-of-object ~= object)
	  let (value, initted?) = slot-value(slot-item, object);
	  if (initted?)
	    write(" = ", display-stream);
	    print(value, display-stream, level: display-print-level,
		  length: display-print-length, pretty?: #t);
	    write-line("", display-stream);
	  else
	    write-line("", display-stream);
	  end if;
	else
	  write-line("", display-stream);
	end if;
      end for;
    else
      write-line("  none", display-stream);
    end if;
  end if;

  // Routine for displaying elements of "object"
  if (display-elements?)
    // Make sure object is an instance of a <collection>
    if (subtype?(class-of-object, <collection>) & object ~= class-of-object)
      write-line(" Elements:", display-stream);
      if (~object.empty?)
	// Print all of the elements
	for (i in object)
	  write("  ", display-stream);
	  print(i, display-stream, level: display-print-level,
		length: display-print-length);
	  write-line("", display-stream);
	end for;
      else
	write-line("  none", display-stream);
      end if;
    end if;
  end if;

  // Routine for displayig superclasses
  if (display-superclasses?)
    write-line(" Direct Superclasses:", display-stream);
    // Get a list of superclasses of object
    let superclasses = class-of-object.direct-superclasses;
    superclasses := remove(superclasses, object);
    if (~superclasses.empty?)
      // Print all of the superclasses
      for (class-item in superclasses)
	write("  ", display-stream);
	write-line(as(<byte-string>, class-name(class-item)),
		   display-stream);
      end for;
    else
      write-line("  none", display-stream);
    end if;
  end if;

  // Routine for displaying subclasses
  if (display-subclasses?)
    write-line(" Direct Subclasses:", display-stream);
    // Get a list of subclasses of object
    let subclasses = class-of-object.direct-subclasses;
    if (~subclasses.empty?)
      // Print all of the subclasses
      for (class-item in subclasses)
	write("  ", display-stream);
	write-line(as(<byte-string>, class-name(class-item)),
		   display-stream);
      end for;
    else
      write-line("  none", display-stream);
    end if;
    write-line("", display-stream);
  end if;
  force-output(display-stream);
end method display-object-info;
  
define method object-info (object :: <function>,
				   #key inspect-stream = *standard-output*,
				   inspect-print-level = 1,
				   inspect-print-length = 5,
				   inspect-elements? = #t,
				   inspect-slots? = #t,
				   inspect-superclasses? = #t,
				   inspect-subclasses? = #t)
 => temp-vect :: <stretchy-vector>;

  let temp-vect = make(<stretchy-vector>);
  temp-vect := add!(temp-vect, object);

  let x = 1;
  
  if (object.function-name)
    if (instance?(object, <generic-function>))
      write("Generic Function ", inspect-stream);
    elseif (instance?(object, <method>))
      write("Method ", inspect-stream);
    else
      write(as(<byte-string>, object.object-class.class-name), inspect-stream);
      write(" ", inspect-stream);
    end if;
    print(as(<byte-string>, object.function-name), inspect-stream);
    write-line("", inspect-stream);
  else
    write("Unnamed function ", inspect-stream);
    write-line("", inspect-stream);
  end if;

  write-line(" Required  Arguments:", inspect-stream);
  let (args, rest, kwd) = object.function-arguments;
  if (args ~= 0)
    for (i in object.function-specializers)
      write("  ", inspect-stream);
      print(x, inspect-stream);
      write("] ", inspect-stream);
      display-one-object(i, display-stream: inspect-stream,
			 display-print-length: inspect-print-length,
			 display-print-level: inspect-print-level);
      write-line("", inspect-stream);
      temp-vect := add!(temp-vect, i);
      x := x + 1;
    end for;
    if (rest)
      write-line("  #rest", inspect-stream);
    end if;
  else
    if (rest)
      write-line("  #rest", inspect-stream);
    else
      write-line("  none", inspect-stream);
    end if;
  end if;
  
  write-line(" Keywords:", inspect-stream);
  if (~kwd)
    write-line("  none", inspect-stream);
  else
    if (kwd = #"all")
      write-line("  all-keys", inspect-stream);
    else
      for (i in kwd)
	write("  ", inspect-stream);
	write(as(<byte-string>, i), inspect-stream);
	write-line(":", inspect-stream);
      end for;
    end if;
  end if;
  
  write-line(" Returns:", inspect-stream);
  let (return-types, rest-types) = object.function-return-values;
  if (~return-types.empty?)
    for (i in return-types)
      if (instance?(i, <union>))
	write-line("  union of types:", inspect-stream);
	for (j in i.union-members)
	  write("   ", inspect-stream);
	  print(x, inspect-stream);
	  write("] ", inspect-stream);
	  display-one-object(j, display-stream: inspect-stream,
			     display-print-length: inspect-print-length,
			     display-print-level: inspect-print-level);
	  write-line("", inspect-stream);
	  temp-vect := add!(temp-vect, j);
	  x := x + 1;
	end for;
      else
	write("  ", inspect-stream);
	print(x, inspect-stream);
	write("] ", inspect-stream);
	display-one-object(i, display-stream: inspect-stream,
			   display-print-length: inspect-print-length,
			   display-print-level: inspect-print-level);
	write-line("", inspect-stream);
	temp-vect := add!(temp-vect, i);
	x := x + 1;
      end if;
    end for;
  end if;
  if (rest-types)
    write-line("  #rest: ", inspect-stream);
    write("   ", inspect-stream);
    print(x, inspect-stream);
    write("] ", inspect-stream);
    write-line(as(<byte-string>, rest-types.class-name), inspect-stream);
    temp-vect := add!(temp-vect, rest-types);
    x := x + 1;
  end if;
  if (return-types.empty? & ~rest-types)
    write-line("  none", inspect-stream);
  end if;
  
  // If it's a generic function, display all of the methods.  
  if (instance?(object, <generic-function>))
    write(" Methods:", inspect-stream);
    let mthds = generic-function-methods(object);
    if (~mthds.empty?)
      write-line("", inspect-stream);
      for (i in mthds)
	write("  ", inspect-stream);
	print(x, inspect-stream);
	write("] ", inspect-stream);
	if (i.function-name)
	  write(as(<byte-string>, i.function-name), inspect-stream);
	else
	  write("unnamed method", inspect-stream);
	end if;
	write("(", inspect-stream);
	if (args ~= 0)
	  let mthd-specializers = i.function-specializers;
	  display-one-object(mthd-specializers[0],
			     display-stream: inspect-stream);
	  for (j from 1 below args)
	    write(", ", inspect-stream);
	    display-one-object(mthd-specializers[j],
			       display-stream: inspect-stream);
	  end for;
	end if;
	write-line(")", inspect-stream);
	temp-vect := add!(temp-vect, i);
	x := x + 1;
      end for;
    else
      write-line("  none", inspect-stream);
    end if;
  end if;
  
  write-line("", inspect-stream);

  temp-vect;
end method object-info;

define method object-info (item :: <singleton>, #key inspect-stream,
			   inspect-print-length,
			   inspect-print-level,
			   inspect-slots?,
			   inspect-elements?,
			   inspect-superclasses?,
			   inspect-subclasses?)
 => info-vect :: <stretchy-vector>;

  let temp-vect = make(<stretchy-vector>);
  temp-vect := add!(temp-vect, item);

  write("Singleton of class", inspect-stream);
  display-one-object(item.singleton-object.object-class,
		     display-stream: inspect-stream,
		     display-print-length: inspect-print-length,
		     display-print-level: inspect-print-level);
  write-line("", inspect-stream);
  
  write-line(" Value:", inspect-stream);
  write("  1] ", inspect-stream);
  display-one-object(item.singleton-object, display-stream: inspect-stream,
		     display-print-level: inspect-print-level,
		     display-print-length: inspect-print-length);
  write-line("", inspect-stream);
  temp-vect := add!(temp-vect, item.singleton-object);
  
  write-line("", inspect-stream);
  temp-vect;
end method object-info;
  

define method object-info (the-union :: <union>, #key inspect-stream,
			   inspect-print-length,
			   inspect-print-level,
			   inspect-slots?,
			   inspect-elements?,
			   inspect-superclasses?,
			   inspect-subclasses?)
 => info-vect :: <stretchy-vector>;
  let temp-vect = make(<stretchy-vector>);
  temp-vect := add!(temp-vect, the-union);

  write-line("Union of types", inspect-stream);

  let n = 1;

  if (the-union.union-members.empty?)
    write-line("  none", inspect-stream);
  else
    for (i in the-union.union-members)
      write("  ", inspect-stream);
      print(n, inspect-stream);
      write("] ", inspect-stream);
      display-one-object(i, display-stream: inspect-stream,
			 display-print-length: inspect-print-length,
			 display-print-level: inspect-print-level);
      write-line("", inspect-stream);
      temp-vect := add!(temp-vect, i);
      n := n + 1;
    end for;
  end if;
  write-line("", inspect-stream);
  temp-vect;
end method object-info;

// This method on object-info is for displaying the information for a class
// The keyword arguments have no default values because all values get passed
// to them via inspect
define method object-info (the-class :: <class>,
			   #key inspect-stream,
			   inspect-print-length,
			   inspect-print-level,
			   inspect-slots?,
			   inspect-elements?,
			   inspect-superclasses?,
			   inspect-subclasses?)
 => info-vect :: <stretchy-vector>;
  // temp-vect will later be returned as info-vect, it holds all of the slots,
  // subclasses and superclasses of the-class
  let temp-vect = make(<stretchy-vector>);
  // The-class is added to temp-vect for bookkeeping reasons
  temp-vect := add!(temp-vect, the-class);

  write("Class ", inspect-stream);
  write-line(as(<byte-string>,the-class.class-name), inspect-stream);

  let n = 1;

  // Routine for inspecting the slots of the-class
  if (inspect-slots?)
    write-line(" Slots:", inspect-stream);
    // Get a list of slots
    let slot-list = the-class.slot-descriptors;
    if (~slot-list.empty?)
      // Print the list of slots, numbering each, and adding it to temp-vect
      for (slot-item in slot-list)
	write("  ", inspect-stream);
	print(n, inspect-stream);
	write("] ", inspect-stream);
	write(as(<byte-string>, slot-item.slot-name).as-uppercase,
	      inspect-stream);
	write(": ", inspect-stream);
	display-one-object(slot-item.slot-type, display-stream: inspect-stream,
			 display-print-length: inspect-print-length,
			 display-print-level: inspect-print-level);
	write-line("", inspect-stream);
	temp-vect := add!(temp-vect, slot-item.slot-type);
	n := n + 1;
      end for;
    else
      write-line("  none", inspect-stream);
    end if;
  end if;

  // Routine for inspecting superclasses
  if (inspect-superclasses?)
    write-line(" Direct Superclasses:", inspect-stream);
    // Get a list of superclasses
    let superclasses = the-class.direct-superclasses;
    superclasses := remove(superclasses, the-class);
    if (~superclasses.empty?)
      // print the list of superclasses, numbering each and adding it to
      // temp-vect
      for (class-item in superclasses)
	write("  ", inspect-stream);
	print(n, inspect-stream);
	write("] ", inspect-stream);
	write-line(as(<byte-string>, class-item.class-name),
		   inspect-stream);
	temp-vect := add!(temp-vect, class-item);
	n := n + 1;
      end for;
    else
      write-line("  none", inspect-stream);
    end if;
  end if;

  // Routine for inspecting subclasses
  if (inspect-subclasses?)
    write-line(" Direct subclasses:", inspect-stream);
    // Get a list of subclasses
    let subclasses = the-class.direct-subclasses;
    if (~subclasses.empty?)
      // Print the list, numbering each, and adding it to temp-vect
      for (class-item in subclasses)
	write("  ", inspect-stream);
	print(n, inspect-stream);
	write("] ", inspect-stream);
	write-line(as(<byte-string>, class-item.class-name),
		   inspect-stream);
	temp-vect := add!(temp-vect, class-item);
	n := n + 1;
      end for;
    else
      write-line("  none", inspect-stream);
    end if;
  end if;
  write-line("", inspect-stream);
  // Return temp-vect as info-vect
  temp-vect;
end method object-info;
  
// This method of object-info displays information about an object which is
// not a class (i.e. an instance of a class).  The keyword arguments have no
// default values because all values get passed to them from inspect.
define method object-info (the-object,
			   #key inspect-stream,
			   inspect-print-length,
			   inspect-print-level,
			   inspect-slots?,
			   inspect-elements?,
			   inspect-superclasses?,
			   inspect-subclasses?)
 => info-vect :: <stretchy-vector>;
  // Create a new <stretchy-vector> that will be returned as info-vect
  let temp-vect = make(<stretchy-vector>);
  // Add the-object to temp-vect, this is done for bookkeeping.
  temp-vect := add!(temp-vect, the-object);

  write-line("An instance of class", inspect-stream);
  write("  1] ", inspect-stream);
  write-line(as(<byte-string>,class-name(object-class(the-object))),
	inspect-stream);
  temp-vect := add!(temp-vect, the-object.object-class);

  let n = 2;

  // Routine for inspecting the slots of the-object
  if (inspect-slots?)
    // Get a list of the slots
    let slot-list = the-object.object-class.slot-descriptors;
    if (~slot-list.empty?)
      write-line(" Slots:", inspect-stream);
      // print the list, adding each to temp-vect
      for (slot-item in slot-list)
	write("  ", inspect-stream);
	print(n, inspect-stream);
	write("] ", inspect-stream);
	// Print slot name
	write(as(<byte-string>,slot-item.slot-name).as-uppercase,
	      inspect-stream);
	write(": ", inspect-stream);
	// Print slot class
	display-one-object(slot-item.slot-type, display-stream: inspect-stream,
			   display-print-length: inspect-print-length,
			   display-print-level: inspect-print-level);
	// If the slot is initialized, print it's value, and add it to
	// temp-vect, otherwise, add the class of the slot to temp-vect
	let (value, initted?) = slot-value(slot-item, the-object);
	if (initted?)
	  write(" = ", inspect-stream);
	  print(value, inspect-stream, level: inspect-print-level,
		length: inspect-print-length, pretty?: #t);
	  write-line("", inspect-stream);
	  temp-vect := add!(temp-vect, value);
	else
	  write-line("", inspect-stream);
	  temp-vect := add!(temp-vect, slot-item.slot-type);
	end if;
	n := n + 1;
      end for;
    else
      write("     current value: ", inspect-stream);
      print(the-object, inspect-stream, level: inspect-print-level,
	    length: inspect-print-length);
      write-line("", inspect-stream);
    end if;
  end if;

  // Routine for inspecting the-object's elements
  if (inspect-elements?)
    if (subtype?(the-object.object-class, <collection>))
      write-line(" Elements:", inspect-stream);
      if (~the-object.empty?)
	// Go through all of the elements, printing each and adding it to
	// temp-vect.
	for (i in the-object)
	  write("  ", inspect-stream);
	  print(n, inspect-stream);
	  write("] ", inspect-stream);
	  print(i, inspect-stream, level: inspect-print-level,
		length: inspect-print-length);
	  write-line("", inspect-stream);
	  temp-vect := add!(temp-vect, i);
	  n := n + 1;
	end for;
      else
	write-line("  none", inspect-stream);
      end if;
    end if;
  end if;
  write-line("", inspect-stream);
  temp-vect;
end method object-info;

define constant command-size = 10;
define constant info-size = 10;

//    **************************************************
//  Some constants, to represent the possible input strings.
//    **************************************************
//
define constant quit-seq =  #["quit", "q", "e", "exit"];
define constant hist-seq = #["history", "h", "hi"];
define constant help-seq = #["?", "help", "he"];
define constant print-seq = #["print", "p"];
define constant view-seq = #["view", "v"];
define constant up-seq = #["u", "up"];

// Routine for printing the help page.  This should (possibly in a future
// revision) be taken from a file rather than hard coded.
//
define method help-page ( #key inspect-stream);
  write-line("", inspect-stream);
  write-line("Inspector online help", inspect-stream);
  write-line("(all commands may be abbreviated by their first letter)",
	     inspect-stream);
  write-line("1, 2, ...      Inspects the corresponding object",
	     inspect-stream);
  write-line("history        Shows the inspected object stack",
	     inspect-stream);
  write-line("up             Moves up the inspected object stack",
	     inspect-stream);
  write("print          Prints the object, ", inspect-stream);
  write-line("using the standard print functions", inspect-stream);
  write-line("view           Redisplays the current object",
	     inspect-stream);
  write-line("?, help        Displays this page", inspect-stream);
  write-line("quit, exit     Quits the object inspector", inspect-stream);
end method help-page;

// Prints the "inspect>" line and reads in the input.  The input string
// is returned to inspect.  This also flushes the inspect-stream buffer.
//
define method command-prompt ( #key inspect-stream) => command :: <string>;
  let temp-str = make(<string>, size: command-size);
  write("inspect> ", inspect-stream);
  force-output(inspect-stream);
  temp-str := read-line(*standard-input*, signal-eof?: #f);
  temp-str;
end method command-prompt;

// This is the main loop of the inspector.  This method processes commands, and
// responds appropriately. All keywords have defaults, and all of the keywords
// get passed on to object-info whenever it is called.
define method inspect (object :: <object>,
		       #key inspect-stream = *standard-output*,
		       inspect-print-length = 5,
		       inspect-print-level = 1,
		       inspect-slots? = #t,
		       inspect-elements? = #t,
		       inspect-superclasses? = #t,
		       inspect-subclasses? = #t)
  // Create the vector that will hold the objects that can be inspected.
  let info-vect = make(<stretchy-vector>, size: info-size);
  // Create a deque to hold the previously created objects.
  let history :: <deque> = make(<deque>);
  write-line("", inspect-stream);
  // Display info about "object".
  info-vect := object-info(object, inspect-stream: inspect-stream,
			   inspect-print-length: inspect-print-length,
			   inspect-print-level: inspect-print-level,
			   inspect-slots?: inspect-slots?,
			   inspect-elements?: inspect-elements?,
			   inspect-superclasses?: inspect-superclasses?,
			   inspect-subclasses?: inspect-subclasses?);
  // Get the first command
  let command = make(<string>, size: command-size, fill: ' ');
  // Main loop, repeat as long a command does not match anything in the
  // quit sequence above.
  while (~member?(command.as-lowercase, quit-seq, test: \=))
    command := command-prompt(inspect-stream: inspect-stream);
    case

      // User didn't type anything, this needs to be a special case, otherwise
      // further comparisons will fail.
      //
      command = #() =>
	write-line("Type ? for help", inspect-stream);

      // If the first character is a number, then the command might be to
      // look at a part of the current object.
      //
      command[0].digit? =>
	// If all of the characters are digits, then the corresponding object
	// should be inspected.
	if (choose(digit?, command) = command)
	  let i = command.string-to-integer;
	  if (i < info-vect.size & i ~= 0)
	    write-line("", inspect-stream);
	    // Add current object (info-vect[0]) onto the history stack
	    push(history, info-vect[0]);
	    // Inspect the ith object in info-vect.
	    info-vect := object-info(info-vect[i],
				     inspect-stream: inspect-stream,
				     inspect-print-length:
				       inspect-print-length,
				     inspect-print-level: inspect-print-level,
				     inspect-slots?: inspect-slots?,
				     inspect-elements?: inspect-elements?,
				     inspect-superclasses?:
				       inspect-superclasses?,
				     inspect-subclasses?: inspect-subclasses?);
	  else
	    if (info-vect.size = 1)
	      // if the list of objects is empty except for element 0 (the
	      // current object), print an error
	      write-line("There are no objects to inspect at this level.",
			 inspect-stream);
	    else 
	      // if i is not a legal number, print an error
	      write("You cannot inspect number ", inspect-stream);
	      write(command, inspect-stream);
	      write(", valid numbers are 1 through ", inspect-stream);
	      print(info-vect.size - 1, inspect-stream);
	      write-line("", inspect-stream);
	    end if;
	  end if;
	else
	  // If command is a combination of digits and other characters, print
	  // an error.
	  write-line("unknown command (type ? for help)", inspect-stream);
	end if;

      // If the command is one of the up-seg, move up the history stack.
      //
      member?(command.as-lowercase, up-seq, test: \=) =>
	write-line("", inspect-stream);
	if (~history.empty?)
	  info-vect := object-info(history.pop,inspect-stream: inspect-stream,
				   inspect-print-length: inspect-print-length,
				   inspect-print-level: inspect-print-level,
				   inspect-slots?: inspect-slots?,
				   inspect-elements?: inspect-elements?,
				   inspect-superclasses?:
				     inspect-superclasses?,
				   inspect-subclasses?: inspect-subclasses?);
	else
	  write-line("This is the first object", inspect-stream);
	end if;

      // If the command is one of print-seq, display the object using the print
      // function and the default print length and level
      //
      member?(command.as-lowercase, print-seq, test: \=) =>
	write-line("", inspect-stream);
	print(info-vect[0], inspect-stream,
	      pretty?: #t, level: inspect-print-level,
	      length: inspect-print-length);
	write-line("", inspect-stream);
	write-line("", inspect-stream);
	
      // if the command is one of hist-seq, display the elements of the history
      // stack.
      //
      member?(command.as-lowercase, hist-seq, test: \=) =>
	// Add the current object, so that it will print out as well
	push(history, info-vect[0]);
	write-line("", inspect-stream);
/*      for (i from 0 below history.size)
*/
	// Go backwards through the stack, so that the current object is on the
	// bottom of the list.
	for (i from history.size - 1 to 0 by -1)
	  if (subtype?(history[i].object-class, <class>))
	    write("Class ", inspect-stream);
	    write-line(as(<byte-string>, history[i].class-name),
		       inspect-stream);
	  else
	    write("Instance of ", inspect-stream);
	    write-line(as(<byte-string>, history[i].object-class.class-name),
		       inspect-stream);
	  end if;
	end for;
	// Remove the current object from the history stack (otherwise, it
	// would be hard to descend the stack.
	pop(history);
	write-line("", inspect-stream);

      // If the command is one of view-seq, redisplay the object's information.
      //
      member?(command.as-lowercase, view-seq, test: \=) =>
	write-line("", inspect-stream);
	info-vect := object-info(info-vect[0],
				 inspect-stream: inspect-stream,
				 inspect-print-length: inspect-print-length,
				 inspect-print-level: inspect-print-level,
				 inspect-slots?: inspect-slots?,
				 inspect-elements?: inspect-elements?,
				 inspect-superclasses?: inspect-superclasses?,
				 inspect-subclasses?: inspect-subclasses?);

      // If the command is one of help-seq, print out the help page
      // 
      member?(command.as-lowercase, help-seq, test: \=) =>
	help-page(inspect-stream: inspect-stream);

      // Otherwise if it's not any of the above, and it's not the quit command
      // let the user know the command he enterd doesn't exist
      //
      ~member?(command.as-lowercase, quit-seq, test: \=) =>
	write-line("unknown command (type ? for help)", inspect-stream);
    end case;
  end while;
end method inspect;
