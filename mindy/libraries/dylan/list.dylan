module: dylan

######################################################################
##
##  Copyright (C) 1994, Carnegie Mellon University
##  All rights reserved.
##
##  This code was produced by the Gwydion Project at Carnegie Mellon
##  University.  If you are interested in using this code, contact
##  "Scott.Fahlman@cs.cmu.edu" (Internet).
##
######################################################################
##
##  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/list.dylan,v 1.1 1994/03/24 21:49:57 wlott Exp $
##
##  This file does whatever.
##

define constant list_fip_next_state =
  method (list :: <list>, state :: <list>) :: <list>;
    tail(state);
  end method;

define constant list_fip_finished-state? =
  method (list :: <list>, state :: <list>, limit)
    state == #();
  end method;

define constant list_fip_current_key =
  method (list :: <list>, state :: <list>) :: <integer>;
    for (key from 0,
	 scan = list then tail(scan),
	 until scan == state)
      if (scan == #())
	error("State not part of list?");
      end;
    finally
      key;
    end for;
  end method;


define constant list_fip_current_element =
  method (list :: <list>, state :: <list>) :: <object>;
    head(state);
  end method;

define constant list_fip_current_element-setter =
  method (value :: <object>, list :: <list>, state :: <list>) :: <object>;
    head(state) := value;
  end method;

define constant list_fip_copy_state =
  method (list :: <list>, state :: <list>) :: <list>;
    state;
  end method;

define method forward-iteration-protocol (list :: <list>)
  values(list, #f, list_fip_next_state, list_fip_finished-state?,
	 list_fip_current_key, list_fip_current_element,
	 list_fip_current_element-setter, list_fip_copy_state);
end method forward-iteration-protocol;

define method make(cls == <list>, #rest keys,
		   #key size: size (0), fill: fill (#f)) :: <list>;
  let result = for (i from 0 below size,
		    list = #() then pair(fill, list))
	       finally
		 list;
	       end for;
  apply(initialize, result, keys);
  result;
end method make;

define method class-for-copy(list :: <list>) :: <class>;
  <list>;
end method class-for-copy;
