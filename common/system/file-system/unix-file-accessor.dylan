Module:       system-internals
Synopsis:     Unix stream accessors (assuming ~ System V release 5.3 semantics)
Author:       Eliot Miranda, Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1994-2001 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <unix-file-accessor> (<external-file-accessor>)
  slot fd-file-accessor :: false-or(<external-file-accessor>) = #f;
end class <unix-file-accessor>;

// An attempt at a portable flexible interface to OS read/write/seek
// functionality.  Legal values for TYPE might include #"file", #"pipe",
// #"tcp", #"udp".  Legal values for LOCATOR depend on TYPE.  
define sideways method platform-accessor-class
    (type == #"file", locator :: <object>)
 => (class :: singleton(<unix-file-accessor>))
  <unix-file-accessor>
end method platform-accessor-class;

define constant $file_create_permissions
  = logior($S_IRUSR, $S_IWUSR, $S_IRGRP, $S_IWGRP, $S_IROTH, $S_IWOTH);

// Legal values for direction are #"input", #"output", #"input-output"
// Legal values for if-exists are #"new-version", #"overwrite", #"replace",
//                                #"truncate", #"signal", #"append"
// NB #"append" does _not_ imply unix open(2) append semantics, _only_
// that writing is likely to continue from the end.  So its merely a hint
// as to where to go first.
// Legal values for if-does-not-exist are #"signal", #"create"
define method accessor-open
    (accessor :: <unix-file-accessor>,
     #key direction = #"input", if-exists, if-does-not-exist,
       locator,
       file-position: initial-file-position = #f, // :: false-or(<integer>)?
       file-size: initial-file-size = #f, // :: false-or(<integer>)?
     #all-keys) => ()
  block (return)
    let pathstring = as(<byte-string>, locator);
    let (stat-err?, st) = %stat(pathstring);
    let exists = ~stat-err?;
    let (mode-code, if-exists, if-does-not-exist)
      = select (direction)
          #"input" =>
	    values($O_RDONLY, 
		   #"overwrite",
		   (if-does-not-exist | #"signal"));
	  #"output" =>
	    values(logior($O_WRONLY, $O_SYNC),
		   (if-exists | #"new-version"),
		   (if-does-not-exist | #"create"));
	  #"input-output" =>
	    values(logior($O_RDWR, $O_SYNC),
		   (if-exists | #"overwrite"),
		   (if-does-not-exist | #"create"));
        end;
    let mode-code 
      = if (exists)
	  select (if-exists)
	    #"signal" =>
	      return(signal(make(<file-exists-error>,
				 locator: as(<posix-file-locator>, locator))));
	    #"new-version", #"replace" =>
	      if (~%unlink(pathstring))
		logior(mode-code, $O_CREAT);
	      else
                let errno = unix-last-error();
		if (errno = $EACCES)
		  return(signal(make(<invalid-file-permissions-error>,
				     locator: locator)));
		else
		  unix-file-error("unlink", "%s", locator);
		end;
	      end;
	    #"overwrite", #"append" => 
	      mode-code;
	    #"truncate" =>
	      logior(mode-code, $O_TRUNC);
	  end
	else
	  select (if-does-not-exist)
	    #"signal" =>
	      return(signal(make(<file-does-not-exist-error>,
                                 locator: as(<posix-file-locator>, locator))));
	    #"create" =>
	      logior(mode-code, $O_CREAT);
	  end
	end;
    let fd = %open(pathstring, mode-code, $file_create_permissions);
    if (fd < 0)
      let errno = unix-last-error();
      if (errno = $EACCES)
	return(signal(make(<invalid-file-permissions-error>,
			   locator: as(<posix-file-locator>, locator))));
      else
        unix-file-error("open", "%s", locator);
      end
    else
      let fd-accessor
        = new-accessor(#"file", locator: fd, file-descriptor: fd);
      accessor.fd-file-accessor := fd-accessor;
      *open-accessors*[accessor] := #t;
      if (if-exists == #"append")
        fd-accessor.accessor-position := fd-accessor.accessor-size;
      end;
      // IMPORTANT!!
      // Once the file has been created the required reopen behaviour is
      // overwrite.  E.g., if an if-exists: #"truncate" file-stream is
      // reopened after close we don't want it truncated again.
      // accessor.exists-behaviour = #"overwrite";
      // By the same token, if the underlying file has been removed by the
      // time a reopen occurs a signal is appropriate.
      // accessor.not-exists-behaviour = #"signal";
    end
  end
end method accessor-open;

define method accessor-close
    (accessor :: <unix-file-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  if (accessor.fd-file-accessor)
    accessor-close(accessor.fd-file-accessor);
    accessor.fd-file-accessor := #f;
    #t
  end
end method accessor-close;

define method accessor-open?
    (accessor :: <unix-file-accessor>) => (open? :: <boolean>)
  accessor.fd-file-accessor & accessor-open?(accessor.fd-file-accessor)
end method accessor-open?;

define method accessor-preferred-buffer-size
    (accessor :: <unix-file-accessor>)
 => (preferred-buffer-size :: <integer>)
  accessor-preferred-buffer-size(accessor.fd-file-accessor);
end method accessor-preferred-buffer-size;

define method accessor-size
    (accessor :: <unix-file-accessor>)
 => (size :: false-or(<integer>))
  accessor-size(accessor.fd-file-accessor);
end method accessor-size;

define inline method accessor-position
    (accessor :: <unix-file-accessor>)
 => (position :: <integer>)
  accessor-position(accessor.fd-file-accessor);
end method accessor-position;

define method accessor-position-setter
    (position :: <integer>, accessor :: <unix-file-accessor>)
 => (position :: <integer>)
  accessor-position-setter(position, accessor.fd-file-accessor);
end method accessor-position-setter;

define method accessor-read-into!
    (accessor :: <unix-file-accessor>, stream :: <file-stream>,
     offset :: <integer>, count :: <integer>, #key buffer)
 => (nread :: <integer>)
  accessor-read-into!(accessor.fd-file-accessor, stream,
                      offset, count, buffer: buffer);
end method accessor-read-into!;

define method accessor-write-from
    (accessor :: <unix-file-accessor>, stream :: <file-stream>,
     offset :: <integer>, count :: <integer>, #key buffer,
     return-fresh-buffer? = #f)
 => (nwritten :: <integer>, new-buffer :: <buffer>)
  accessor-write-from(accessor.fd-file-accessor, stream,
                      offset, count,
                      buffer: buffer,
                      return-fresh-buffer?: return-fresh-buffer?);
end method accessor-write-from;

define method accessor-newline-sequence
    (accessor :: <unix-file-accessor>)
 => (string :: <string>)
  "\n"
end method accessor-newline-sequence;
