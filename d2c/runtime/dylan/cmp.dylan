module: dylan


define generic \== (x :: <object>, y :: <object>) => answer :: <boolean>;

define sealed method \== (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  if (x.object-class.class-functional? & y.object-class.class-functional?)
    functional-==(x, y);
  else
    %%primitive \== (x, y);
  end;
end;


define open generic functional-== (x :: <object>, y :: <object>)
    => answer :: <boolean>;

define method functional-== (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  #f;
end;


define generic \~== (x :: <object>, y :: <object>) => res :: <boolean>;

define inline method \~== (x :: <object>, y :: <object>) => res :: <boolean>;
  ~(x == y);
end;


define open generic \= (x :: <object>, y :: <object>) => answer :: <boolean>;

define inline method \= (x :: <object>, y :: <object>) => answer :: <boolean>;
  x == y;
end;


define open generic \< (x :: <object>, y :: <object>) => answer :: <boolean>;

// No default method for <.


define open generic \<= (x :: <object>, y :: <object>) => answer :: <boolean>;

define inline method \<= (x :: <object>, y :: <object>) => answer :: <boolean>;
  ~(y < x);
end method;


define open generic \~= (x :: <object>, y :: <object>) => answer :: <boolean>;

define inline method \~= (x :: <object>, y :: <object>) => answer :: <boolean>;
  ~(x = y);
end method;


define generic \>= (x :: <object>, y :: <object>) => answer :: <boolean>;

define sealed inline method \>= (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  y <= x;
end method;


define generic \> (x :: <object>, y :: <object>) => answer :: <boolean>;

define sealed inline method \> (x :: <object>, y :: <object>)
    => answer :: <boolean>;
  y < x;
end method;
