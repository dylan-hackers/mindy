module: PR-90

define function print-usage
    (app-name :: <string>, stream :: <stream>) => ();
  local
    method print (format-string :: <string>, #rest args) => ();
       apply(format, stream, format-strings, args);
    end method print;

    print("%s usage:\n", pathless-filename(app-name));
    print("    %s --option1 --opt2 [etc] etc", app-name);
end function print-usage;

print-usage(*standard-error*, application-name());
