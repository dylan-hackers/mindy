module: dylan

define abstract class <boolean> (<object>)
end;

define class <true> (<boolean>)
end;

define sealed method make (class == <true>, #key) => res :: type-or();
  error("Can't make new instances of <true>, #t is it.");
end;

define class <false> (<boolean>)
end;

define sealed method make (class == <false>, #key) => res :: type-or();
  error("Can't make new instances of <false>, #f is it.");
end;

define sealed inline method \~ (thing :: <object>) => res :: <boolean>;
  %%primitive not (thing);
end;

