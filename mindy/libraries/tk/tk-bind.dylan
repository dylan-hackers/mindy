module: tk

define method bind
    (window :: <window>, event :: <string>, command)
 => (window :: <window>);
  put-tk-line("bind ", window, " ", event, " {", command, "}");
  window;
end method bind;

define method get-binding (window :: <window>, event) => (result :: <string>);
  call-tk-function("bind ", window, " ", event);
end method;

define method get-bindings (window :: <window>) => (result :: <sequence>);
  map(method (event) pair(event, get-binding(window, event)) end method,
      parse-tk-list(call-tk-function("bind ", window), depth: 1));
end method get-bindings;
