Module:       locators-internals
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <server-locator> (<locator>)
end class <server-locator>;

define open abstract class <physical-locator> (<locator>) 
end class <physical-locator>;

define open abstract class <directory-locator> (<physical-locator>)
end class <directory-locator>;

define open abstract class <file-locator> (<physical-locator>)
end class <file-locator>;

define open generic locator-server
    (locator :: <locator>) => (server :: false-or(<server-locator>));
define open generic locator-host
    (locator :: <locator>) => (host :: false-or(<string>));
define open generic locator-volume
    (locator :: <locator>) => (volume :: false-or(<string>));
define open generic locator-directory
    (locator :: <locator>) => (directory :: false-or(<directory-locator>));
define open generic locator-relative?
    (locator :: <locator>) => (relative? :: <boolean>);
define open generic locator-path
    (locator :: <locator>) => (path :: <sequence>);
define open generic locator-base
    (locator :: <locator>) => (base :: false-or(<string>));
define open generic locator-extension
    (locator :: <locator>) => (extension :: false-or(<string>));

///---*** Bootstrapping code, remove when 2.0b2c1 has been distributed
define constant <pathname> = type-union(<string>, <physical-locator>);


/// Locator coercion

//---*** andrewa: This caching scheme doesn't work yet, so disable it.
define constant $cache-locators?        = #f;
define constant $cache-locator-strings? = #f;

define constant $locator-to-string-cache = make(<object-table>, weak: #"key");
define constant $string-to-locator-cache = make(<string-table>, weak: #"value");

define open generic locator-as-string
    (class :: subclass(<string>), locator :: <locator>)
 => (string :: <string>);

define open generic string-as-locator
    (class :: subclass(<locator>), string :: <string>)
 => (locator :: <locator>);

define sealed sideways method as
    (class :: subclass(<string>), locator :: <locator>)
 => (string :: <string>)
  let string = element($locator-to-string-cache, locator, default: #f);
  if (string)
    as(class, string)
  else
    let string = locator-as-string(class, locator);
    if ($cache-locator-strings?)
      element($locator-to-string-cache, locator) := string;
    else
      string
    end
  end
end method as;

define sealed sideways method as
    (class :: subclass(<locator>), string :: <string>)
 => (locator :: <locator>)
  let locator = element($string-to-locator-cache, string, default: #f);
  if (instance?(locator, class))
    locator
  else
    let locator = string-as-locator(class, string);
    if ($cache-locators?)
      element($string-to-locator-cache, string) := locator;
    else
      locator
    end
  end
end method as;

define method as
    (class == <physical-locator>, string :: <string>)
 => (locator :: <physical-locator>)
  as(<native-physical-locator>, string)
end method as;

define method as
    (class == <directory-locator>, string :: <string>)
 => (locator :: <directory-locator>)
  as(<native-directory-locator>, string)
end method as;

define method make
    (class == <directory-locator>,
     #key server :: false-or(<server-locator>) = #f,
          path :: <sequence> = #[],
          relative? :: <boolean> = #f,
          name :: false-or(<string>) = #f)
 => (locator :: <directory-locator>)
  make(<native-directory-locator>,
       server:    server,
       path:      path,
       relative?: relative?,
       name:      name)
end method make;

define method as
    (class == <file-locator>, string :: <string>)
 => (locator :: <file-locator>)
  as(<native-file-locator>, string)
end method as;

define method make
    (class == <file-locator>,
     #key directory :: false-or(<directory-locator>) = #f,
          base :: false-or(<string>) = #f,
          extension :: false-or(<string>) = #f,
          name :: false-or(<string>) = #f)
 => (locator :: <directory-locator>)
  make(<native-file-locator>,
       directory: directory,
       base:      base,
       extension: extension,
       name:      name)
end method make;


/// Locator conditions

define class <locator-error> (<format-string-condition>, <error>)
end class <locator-error>;

define function locator-error
    (format-string :: <string>, #rest format-arguments)
  error(make(<locator-error>, 
	     format-string:    format-string,
	     format-arguments: format-arguments))
end function locator-error;


/// Useful locator protocols

define open generic locator-test
    (locator :: <directory-locator>) => (test ::<function>);

define method locator-test
    (locator :: <directory-locator>) => (test :: <function>)
  \=
end method locator-test;

define open generic locator-might-have-links?
    (locator :: <directory-locator>) => (links? :: <boolean>);

define method locator-might-have-links?
    (locator :: <directory-locator>) => (links? :: singleton(#f))
  #f
end method locator-might-have-links?;

define method locator-relative?
    (locator :: <file-locator>) => (relative? :: <boolean>)
  let directory = locator.locator-directory;
  ~directory | directory.locator-relative?
end method locator-relative?;

define method current-directory-locator?
    (locator :: <directory-locator>) => (current-directory? :: <boolean>)
  locator.locator-relative?
    & locator.locator-path = #[#"self"]
end method current-directory-locator?;

define method locator-directory
    (locator :: <directory-locator>) => (parent :: false-or(<directory-locator>))
  let path = locator.locator-path;
  unless (empty?(path))
    make(object-class(locator),
	 server:    locator.locator-server,
	 path:      copy-sequence(path, end: path.size - 1),
	 relative?: locator.locator-relative?)
  end
end method locator-directory;


/// Simplify locator

define open generic simplify-locator
    (locator :: <physical-locator>)
 => (simplified-locator :: <physical-locator>);

define method simplify-locator
    (locator :: <directory-locator>)
 => (simplified-locator :: <directory-locator>)
  let path = locator.locator-path;
  let resolve-parent? = ~locator.locator-might-have-links?;
  let simplified-path = simplify-path(path, resolve-parent?: resolve-parent?);
  if (path ~= simplified-path)
    make(object-class(locator),
	 server:    locator.locator-server,
	 path:      simplified-path,
	 relative?: locator.locator-relative?)
  else
    locator
  end
end method simplify-locator;

define method simplify-locator
    (locator :: <file-locator>) => (simplified-locator :: <file-locator>)
  let directory = locator.locator-directory;
  let simplified-directory = directory & simplify-locator(directory);
  if (directory ~= simplified-directory)
    make(object-class(locator),
	 directory: simplified-directory,
	 base:      locator.locator-base,
	 extension: locator.locator-extension)
  else
    locator
  end
end method simplify-locator;


/// Subdirectory locator

define open generic subdirectory-locator
    (locator :: <directory-locator>, #rest sub-path)
 => (subdirectory :: <directory-locator>);

define method subdirectory-locator
    (locator :: <directory-locator>, #rest sub-path)
 => (subdirectory :: <directory-locator>)
  let old-path = locator.locator-path;
  let new-path = concatenate-as(<simple-object-vector>, old-path, sub-path);
  make(object-class(locator),
       server:    locator.locator-server,
       path:      new-path,
       relative?: locator.locator-relative?)
end method subdirectory-locator;


/// Relative locator

define open generic relative-locator
    (locator :: <physical-locator>, from-locator :: <physical-locator>)
 => (relative-locator :: <physical-locator>);

define method relative-locator
    (locator :: <directory-locator>, from-locator :: <directory-locator>)
 => (relative-locator :: <directory-locator>)
  let path = locator.locator-path;
  let from-path = from-locator.locator-path;
  case
    ~locator.locator-relative? & from-locator.locator-relative? =>
      locator-error
	("Cannot find relative path of absolute locator %= from relative locator %=",
	 locator, from-locator);
    locator.locator-server ~= from-locator.locator-server =>
      locator;
    path = from-path =>
      make(object-class(locator),
	   path: vector(#"self"),
	   relative?: #t);
    otherwise =>
      make(object-class(locator),
	   path: relative-path(path, from-path, test: locator.locator-test),
	   relative?: #t);
  end
end method relative-locator;

define method relative-locator
    (locator :: <file-locator>, from-directory :: <directory-locator>)
 => (relative-locator :: <file-locator>)
  let directory = locator.locator-directory;
  let relative-directory = directory & relative-locator(directory, from-directory);
  if (relative-directory ~= directory)
    simplify-locator
      (make(object-class(locator),
	    directory: relative-directory,
	    base:      locator.locator-base,
	    extension: locator.locator-extension))
  else
    locator
  end
end method relative-locator;

define method relative-locator
    (locator :: <physical-locator>, from-locator :: <file-locator>)
 => (relative-locator :: <physical-locator>)
  let from-directory = from-locator.locator-directory;
  case
    from-directory =>
      relative-locator(locator, from-directory);
    ~locator.locator-relative? =>
      locator-error
	("Cannot find relative path of absolute locator %= from relative locator %=",
	 locator, from-locator);
    otherwise =>
      locator;
  end
end method relative-locator;


/// Merge locators

define open generic merge-locators
    (locator :: <pathname>, from-locator :: <pathname>)
 => (merged-locator :: <physical-locator>);

///---*** Bootstrapping code, remove when 2.0b2c1 has been distributed
define method merge-locators
    (locator :: <pathname>, from-locator :: <pathname>)
 => (merged-locator :: <physical-locator>)
  merge-locators(as(<physical-locator>, locator),
		 as(<physical-locator>, from-locator))
end method merge-locators;

define method merge-locators
    (locator :: <directory-locator>, from-locator :: <directory-locator>)
 => (merged-locator :: <directory-locator>)
  if (locator.locator-relative?)
    let path = concatenate(from-locator.locator-path, locator.locator-path);
    simplify-locator
      (make(object-class(locator),
	    server:    from-locator.locator-server,
	    path:      path,
	    relative?: from-locator.locator-relative?))
  else
    locator
  end
end method merge-locators;

define method merge-locators
    (locator :: <file-locator>, from-locator :: <directory-locator>)
 => (merged-locator :: <file-locator>)
  let directory = locator.locator-directory;
  let merged-directory 
    = if (directory)
	merge-locators(directory, from-locator)
      else
	simplify-locator(from-locator)
      end;
  if (merged-directory ~= directory)
    make(object-class(locator),
	 directory: merged-directory,
	 base:      locator.locator-base,
	 extension: locator.locator-extension)
  else
    locator
  end
end method merge-locators;

define method merge-locators
    (locator :: <physical-locator>, from-locator :: <file-locator>)
 => (merged-locator :: <physical-locator>)
  let from-directory = from-locator.locator-directory;
  if (from-directory)
    merge-locators(locator, from-directory)
  else
    locator
  end
end method merge-locators;


/// Locator protocols

define method supports-open-locator?
    (locator :: <file-locator>) => (openable? :: <boolean>)
  ~locator.locator-relative?
end method supports-open-locator?;

define method open-locator
    (locator :: <file-locator>, #rest keywords, #key, #all-keys)
 => (stream :: <stream>)
  apply(open-file-stream, as(<string>, locator), keywords)
end method open-locator;

//---*** This is a little odd, there should be just one place in
//---*** streams/locators that turns locators into strings.
define sideways sealed method open-file-stream
    (locator :: <file-locator>, #rest keywords, #key, #all-keys)
 => (stream :: <stream>)
  apply(open-file-stream, as(<string>, locator), keywords)
end method open-file-stream;


///---*** Bootstrapping code, remove when 2.0b2c1 has been distributed

define constant abbreviate-locator = relative-locator;

define constant <native-locator> = <locator>;

define sideways method as
    (class == <locator>, string :: <string>)
 => (locator :: <locator>)
  as(<physical-locator>, string)
end method as;

define sideways method as
    (class :: subclass(<locator>), locator :: <locator>)
 => (locator :: <locator>)
  if (instance?(locator, class))
    locator
  else
    locator-error("Cannot convert %= to %=",
		  locator, class)
  end
end method as;

define sideways method make
    (class == <locator>,
     #key volume,
          host,
          directory,
          base,
          type,
          extension = type,
          name)
 => (locator :: <physical-locator>)
  let server
  = case
      volume    => make(<microsoft-volume-locator>, volume: volume);
      host      => make(<microsoft-unc-locator>, host: host);
      otherwise => #f;
    end;
  let directory-locator
    = if (directory)
	make(<directory-locator>,
	     server: server,
	     path:   if (instance?(directory, <path>))
		       directory.elements
		     else
		       directory | #[]
		     end,
	     relative?: if (instance?(directory, <path>))
			  directory.relative-path?
			end)
      end;
  if (base | extension | name)
    make(<file-locator>,
	 directory: directory-locator,
	 base:      base,
	 extension: extension,
	 name:      name)
  else
    directory-locator
  end
end method make;

define method locator-prefix
    (locator :: <directory-locator>) => (prefix :: <string>)
  as(<string>, locator)
end method locator-prefix;

define method locator-prefix
    (locator :: <file-locator>) => (prefix :: <string>)
  let directory = locator.locator-directory;
  if (directory)
    directory.locator-prefix
  else
    ""
  end
end method locator-prefix;

define method override-locator
    (locator :: <directory-locator>,
     #rest args,
     #key host = $unsupplied,
	  volume = $unsupplied,
	  directory-path = $unsupplied,
	  directory = directory-path,
	  base = $unsupplied,
	  type = $unsupplied,
	  prefix = $unsupplied,
	  name = $unsupplied,
	  extension = type,
     #all-keys)
 => (result :: <physical-locator>)
  let old-server = locator.locator-server;
  let new-server
    = case
	host ~== $unsupplied =>
	  if (host)
	    make(<microsoft-unc-locator>, host: host)
	  end;
	volume ~== $unsupplied =>
	  if (volume)
	    make(<microsoft-volume-locator>, volume: volume)
	  end;
	otherwise =>
	  old-server;
      end;
  let directory
    = make(object-class(locator),
	   server:    new-server,
	   path:      case
			directory == $unsupplied =>
			  locator.locator-path;
			~directory =>
			  #[];
			instance?(directory, <path>) =>
			  directory.elements;
			otherwise =>
			  directory;
		      end,
	   relative?: case
			directory == $unsupplied =>
			  locator.locator-relative?;
			instance?(directory, <path>) =>
			  directory.relative-path?;
			otherwise =>
			  #f;
		      end);
  let new-locator
    = if ((name & name ~== $unsupplied)
	    | (base & base ~== $unsupplied)
	    | (extension & extension ~== $unsupplied))
	make(<microsoft-file-locator>,
	     directory:    directory,
	     name:         if (name ~== $unsupplied) name end,
	     base:         if (base ~== $unsupplied) base end,
	     extension:    case
			     extension == $unsupplied =>
			       #f;
			     extension & ~empty?(extension) & extension[0] == '.' =>
			       copy-sequence(extension, start: 1);
			     otherwise =>
			       extension;
			   end)
      else
	directory
      end;
  // debug-message("Override %s => %s: arguments: %=",
  //               locator, new-locator, args);
  new-locator
end method override-locator;

define method override-locator
    (locator :: <file-locator>,
     #rest args,
     #key host = $unsupplied,
	  volume = $unsupplied,
	  directory-path = $unsupplied,
	  directory = directory-path,
	  base = $unsupplied,
	  type = $unsupplied,
	  prefix = $unsupplied,
	  name = $unsupplied,
	  extension = type,
     #all-keys)
 => (result :: <physical-locator>)
  let old-directory = locator.locator-directory;
  let new-directory
    = case
	old-directory =>
	  override-locator(old-directory,
			   host: host,
			   volume: volume,
			   directory-path: directory-path,
			   directory: directory);
	(~directory | directory == $unsupplied) =>
	  #f;
	otherwise =>
	  let server
	    = case 
		(host & host ~== $unsupplied) =>
		  make(<microsoft-unc-locator>, host: host);
		(volume & volume ~== $unsupplied) =>
		  make(<microsoft-volume-locator>, volume: volume);
		otherwise =>
		  #f;
	      end;
	  make(<directory-locator>,
	       server:    server,
	       path:      if (instance?(directory, <path>))
			    directory.elements
			  else
			    directory
			  end,
	       relative?: if (instance?(directory, <path>))
			    directory.relative-path?
			  end);
      end;
  let (base, extension)
    = case
	name ~== $unsupplied =>
	  if (name)
	    let filename = as(object-class(locator), name);
	    values(filename.locator-base, filename.locator-extension)
	  else
	    values(#f, #f);
	  end;
	(base ~== $unsupplied | extension ~== $unsupplied) =>
	  values(if (base ~== $unsupplied)
		   base
		 else
		   locator.locator-base
		 end,
		 case
		   extension == $unsupplied =>
		     locator.locator-extension;
		   extension & ~empty?(extension) & extension[0] == '.' =>
		     copy-sequence(extension, start: 1);
		   otherwise =>
		     extension;
		 end);
	otherwise =>
	  values(locator.locator-base, locator.locator-extension);
      end;
  let new-locator
    = if (base)
	make(object-class(locator),
	     directory: new-directory,
	     base:      base,
	     extension: extension)
      else
	new-directory
      end;
  // debug-message("Override %s => %s: arguments: %=",
  //               locator, new-locator, args);
  new-locator
end method override-locator;


// Paths

define class <path> (<mutable-sequence>)
  constant slot relative-path? :: <boolean> = #f,
    init-keyword: relative-path?:;
  constant slot elements :: <list>,
    init-keyword: elements:;
end class <path>;

define constant <directory-path> = <path>;

define method locator-directory-path
    (locator :: <directory-locator>) => (path :: <path>)
  make(<path>,
       relative-path?: locator.locator-relative?,
       elements:       as(<list>, locator.locator-path))
end method locator-directory-path;

define method locator-directory-path
    (locator :: <file-locator>) => (path :: <path>)
  let directory = locator.locator-directory;
  if (directory)
    directory.locator-directory-path
  else
    make(<path>,
	 relative-path?: #t,
	 elements:       #())
  end
end method locator-directory-path;

define method element
    (sequence :: <path>, key :: <object>, #rest all-keys, #key default)
 => (object :: <object>)
  apply(element, elements(sequence), key, all-keys)
end method element;

define method element-setter
    (new-value, sequence :: <path>, key :: <object>)
 => (object :: <object>)
  element(elements(sequence), key) := new-value;
end method element-setter;

define function path-elements 
    (path :: <path>) => (elements :: <list>)
  path.elements
end function;

define inline function path-next-state
    (collection :: <path>, state :: <list>) => (l :: <list>)
  state.tail
end function;

define inline function path-finished-state?
    (collection :: <path>, state :: <list>, limit) => (result :: <boolean>)
  state == #()
end function;

define inline function path-current-key
    (collection :: <path>, state :: <list>) => (result :: <integer>)
  iterate search (l :: <list> = elements(collection), k :: <integer> = 0)
    if (l == state)
      k
    else
      search(l.tail, k + 1)
    end if
  end iterate
end function;

define inline function path-current-element
    (collection :: <path>, state :: <list>) => (current-element)
  state.head
end function;

define inline function path-current-element-setter
    (current-element, collection :: <path>, state :: <list>)
 => (current-element)
  state.head := current-element
end function;

define method forward-iteration-protocol 
    (path :: <path>)
 => (initial-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>)
  values(elements(path),
	 #(),
	 path-next-state,
	 path-finished-state?,
	 path-current-key,
	 path-current-element,
	 path-current-element-setter,
	 method (collection, state) => (value) state end)
end method;

define method \= 
    (path1 :: <path>, path2 :: <path>) => (equal? :: <boolean>)
  object-class(path1) == object-class(path2)
  & relative-path?(path1) = relative-path?(path2)
  & elements(path1) = elements(path2);
end method;
