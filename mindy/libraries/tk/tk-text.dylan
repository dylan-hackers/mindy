module: tk

define class <text> (<window>, <editable>) end class;

define-widget(<text>, "text",
	      #"exportselection", #"font", #"height", #"insertbackground",
	      #"insertborderwidth", #"insertofftime", #"insertontime",
	      #"insertwidth", #"selectbackground", #"selectborderwidth",
	      #"selectforeground", #"setgrid", #"state", #"textvariable",
	      #"width", #"wrap", #"yscrollcommand");

define class <text-index> (<object>)
  slot line :: limited(<integer>, min: 1), required-init-keyword: #"line";
  slot character :: limited(<integer>, min: 0),
    init-value: 0,
    init-keyword: #"character";
end class <text-index>;
  
define method text-at
    (line :: limited(<integer>, min: 1),
     character :: limited(<integer>, min: 0))
 => (result :: <text-index>);
  make(<text-index>, line: line, character: character);
end method text-at;

define method line-end
    (line :: limited(<integer>, min: 1)) => (result :: <string>);
  concatenate(tk-as(<string>, line), ".end");
end method line-end;

define constant text-index-matcher
  = make-regexp-positioner("^([0-9]+)\\.([0-9]+)$");
define method as (cls == <text-index>, value :: <string>);
  let (all-start, all-end, line-start, line-end, char-start, char-end)
    = text-index-matcher(value);
  if (all-start)
    make(<text-index>,
	 line: tk-as(<integer>,
		  copy-sequence(value, start: line-start, end: line-end)),
	 character: tk-as(<integer>,
		       copy-sequence(value,
				     start: char-start, end: char-end)));
  else
    error("Cannot interpret '%s' as a text index.", value);
  end if;
end method as;

define method as (cls == <string>, value :: <text-index>);
  concatenate(tk-as(<string>, value.line), ".",
	      tk-as(<string>, value.character));
end method as;

define class <text-mark> (<object>)
  slot widget :: <text>, required-init-keyword: #"in";
  slot name :: <string>,
    init-keyword: #"name",
    init-function: curry(anonymous-name, prefix: "mark");
  virtual slot value, init-keyword: value;
end class <text-mark>;

define method initialize
    (object :: <text-mark>, #next next, #key value, #all-keys);
  if (value)
    object.value := value;
  else
    // Make sure the mark has some value
    let names = call-tk-function(object.widget, " mark names");
    // "end" seems a somewhat magical mark.
    if (object.name ~= "end" & ~member?(object.name, names))
	object.value := "1.0";
    end if;
  end if;
end method initialize;

define method value
    (object :: <text-mark>) => (result :: <text-index>);
  as(<text-index>,
     call-tk-function(object.widget, " index ", object.name));
end method value;

define method value-setter
    (value :: <object>, object :: <text-mark>);
  put-tk-line(object.widget, " mark  set ", object.name, " ", value);
end method value-setter;

define method as
    (cls == <string>, object :: <text-mark>) => (result :: <string>);
  object.name;
end method as;

define method as (cls == <text-index>, mark :: <text-mark>)
  as(<text-index>, call-tk-function(mark.widget, " index ", mark.name));
end method as;

define method marks (text :: <text>) => (result :: <sequence>);
  map(curry(make, <text-mark>, #"name"),
      call-tk-function(text, " mark name"));
end method marks;

define method delete
    (text :: <text>, index, #next next, #key end: last) => (text :: <text>);
  let index =
    if (instance?(index, <integer>)) text-at(index, 0) else index end if;
  let last =
    if (instance?(last, <integer>)) text-at(last, 0) else last end if;
  next(text, index, end: last)
end method delete;

define method insert
    (text :: <text>, index, #next next, #rest elements) => (text :: <text>);
  let index =
    if (instance?(index, <integer>)) text-at(index, 0) else index end if;
  apply(next, text, index, elements);
end method insert;

define method get-all (text :: <text>) => (result :: <string>);
  tk-unquote(call-tk-function(text, " get 1.0 end"));
end method get-all;

define method get-elements
    (widget :: <text>, index, #key end: last) => (result :: <string>);
  let index =
    if (instance?(index, <integer>)) text-at(index, 0) else index end if;
  let last =
    if (instance?(last, <integer>)) text-at(last, 0) else last end if;

  tk-unquote(call-tk-function(widget, " get ", index, " ",
				if (last) last else "" end));
end method get-elements;

define method yview(text :: <text>, index) => text :: <text>;
  put-tk-line(text, " yview ", tk-as(<string>, index));
  text;
end method yview;

define class <text-tag> (<object>)
  slot widget :: <text>, required-init-keyword: #"in";
  slot name :: <string>,
    init-keyword: #"name",
    init-function: curry(anonymous-name, prefix: "tag");
end class <text-tag>;

define method initialize
    (object :: <text-tag>, #next next, #rest options, #all-keys);
  apply(put-tk-line, object.widget, " tag configure ", object.name,
	std-options(#[#"bgstipple", #"fgstipple", #"font", #"underline"],
		    #t, options));
end method initialize;

define method configure-tag
    (tag :: <text-tag>, #rest options) => (tag :: <text-tag>);
  apply(put-tk-line, tag.widget, " tag configure ", tag.name,
	std-options(#[#"bgstipple", #"fgstipple", #"font", #"underline"],
		    #t, options));
end method configure-tag;

define method tag-configuration
    (tag :: <text-tag>, index :: <object>) => (result :: <sequence>);
  let string = call-tk-function(tag.widget, " tag configure ", tag.name);
  parse-tk-list(string, depth: 2);
end method tag-configuration;

define method as
    (cls == <string>, object :: <text-tag>) => (result :: <string>);
  object.name;
end method as;

define method tags (text :: <text>) => (result :: <sequence>);
  map(curry(make, <text-tag>, #"name"),
      call-tk-function(text, " tag name"));
end method tags;

define method bind-tag
    (tag :: <text-tag>, event :: <string>, command, #key append = #f)
 => (tag :: <text-tag>);
  put-tk-line(tag.widget, " tag bind ", tag.name, " ", event,
	       if (append) " +{" else " {" end if,
	       command,
	       "}");
  tag;
end method bind-tag;

define method delete-tag (tag :: <text-tag>) => ();
  put-tk-line(tag.widget, " tag delete ", tag.name);
end method delete-tag;

define method raise-tag (tag :: <text-tag>, #key past :: <text-tag>) => ();
  put-tk-line(tag.widget, " tag raise ", tag.name, " ",
	       if (past) past else "" end if);
end method raise-tag;

define method lower-tag (tag :: <text-tag>, #key past :: <text-tag>) => ();
  put-tk-line(tag.widget, " tag lower ", tag.name, " ",
	       if (past) past else "" end if);
end method lower-tag;

define method next-range (tag :: <text-tag>, #key start, end: last)
 => (result :: false-or(<pair>));
  let start = case
		instance?(start, <integer>) => text-at(start, 0);
		start => start;
		otherwise => "1.0";
	      end case;
  let last = case
		instance?(last, <integer>) => text-at(last, 0);
		last => last;
		otherwise => "end";
	      end case;
  let result = call-tk-function(tag.widget, " tag nextrange ",tag.name, " ",
				 start, " ", last);
  if (empty?(result)) #f else apply(pair,parse-tk-list(result, depth: 1)) end;
end method next-range;

define method add-tag
    (tag :: <text-tag>, #key start, end: last) => (tag :: <text-tag>);
  let start = case
		instance?(start, <integer>) => text-at(start, 0);
		start => start;
		otherwise => "1.0";
	      end case;
  let last = case
		instance?(last, <integer>) => text-at(last, 0);
		last => last;
		otherwise => "end";
	      end case;
  let result = put-tk-line(tag.widget, " tag add ", tag.name, " ",
			    start, " ", last);
  tag;
end method add-tag;

define method remove-tag
    (tag :: <text-tag>, #key start, end: last) => (tag :: <text-tag>);
  let start = case
		instance?(start, <integer>) => text-at(start, 0);
		start => start;
		otherwise => "1.0";
	      end case;
  let last = case
		instance?(last, <integer>) => text-at(last, 0);
		last => last;
		otherwise => "end";
	      end case;
  let result = put-tk-line(tag.widget, " tag remove ", tag.name, " ",
			    start, " ", last);
  tag;
end method remove-tag;


  
