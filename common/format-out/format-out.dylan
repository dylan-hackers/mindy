module: Format-out

define method format-out(control-string :: <byte-string>, #rest args)
 => ();
  apply(format, *standard-output*, control-string, args);
end method format-out;
