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
##  $Header: /home/housel/work/rcs/gd/src/mindy/libraries/dylan/vec.dylan,v 1.1 1994/03/24 21:48:03 wlott Exp $
##
##  This file does whatever.
##

define constant vector-prev-state =
  begin
    local
      method vector-prev-state (vec :: <vector>, state :: <integer>)
	:: <integer>;
	state - 1;
      end;
    vector-prev-state;
  end;

define constant vector-next-state =
  begin
    local
      method vector-next-state (vec :: <vector>, state :: <integer>)
	  :: <integer>;
	state + 1;
      end;
    vector-next-state;
  end;

define constant vector-finished? =
  begin
    local
      method vector-finished? (vec :: <vector>, state :: <integer>,
			       limit :: <integer>)
	state == limit;
      end;
    vector-finished?;
  end;

define constant vector-current-key =
  begin
    local
      method vector-current-key (vec :: <vector>, state :: <integer>)
	  :: <object>;
	state;
      end;
    vector-current-key;
  end;

define constant vector-current-element =
  begin
    local
      method vector-current-element (vec :: <vector>, state :: <integer>)
	  :: <object>;
	element(vec, state);
      end;
    vector-current-element;
  end;

define constant vector-current-element-setter =
  begin
    local
      method vector-current-element-setter (value :: <object>, vec :: <vector>,
					    state :: <integer>)
	  :: <object>;
	element(vec, state) := value;
      end;
    vector-current-element-setter;
  end;

define constant vector-copy-state =
  begin
    local
      method vector-copy-state (vec :: <vector>, state :: <integer>)
	  :: <integer>;
	state;
      end;
    vector-copy-state;
  end;

define method forward-iteration-protocol (vec :: <vector>)
  values(0, size(vec), vector-next-state, vector-finished?,
	 vector-current-key, vector-current-element,
	 vector-current-element-setter, vector-copy-state);
end;

define method backward-iteration-protocol (vec :: <vector>)
  values(size(vec) - 1, -1, vector-prev-state, vector-finished?,
	 vector-current-key, vector-current-element,
	 vector-current-element-setter, vector-copy-state);
end;
