Subject: Each-subclass slots
module: PR-3

define class <foo>(<object>)
  each-subclass slot bar1;
end;


define class <bar>(<object>)
  each-subclass slot bar2;
end;


define class <foo-bar>(<foo> , <bar>)
  each-subclass slot bar3;
end;
