module: PR-47

define function unbuggy() => ();
   let handler <condition> = #t.always;
end;

define function unbuggy2() => ();
  block (return)
   let handler <condition> = #t.always;
  end block;
end;

define function buggy() => ();
  block (return)
   let handler <condition> = #t.always;
   return();
  end block;
end;
