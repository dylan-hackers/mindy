module: PR-46

define constant <category> = apply(one-of, map(curry(as, <symbol>),
#("I", 
"II", "IIa", "III", "IV", "V")));

define class <foo> (<object>)
  slot category :: <category>, init-keyword: category:;
end class <foo>;
