module: PR-48

define macro capture-element
  {
    capture-element(?coll:expression, ?index:expression)
  }
  =>
  {
    ?=element(?coll, ?index);
  }
end;


begin

  local element(c, i)
          "test succeeded!";
        end;

  format-out(capture-element(#("test failed!"), 0));
end;
