Subject: Forward imports not possible
module: dylan-user

define library PR-103
  use dylan;
  export a, b;
end;

define module a
  use b, import: all;
// --> Error: Undefined variable: bar { while processing PR#103a.dylan }
end;

define module a2
  use b, import: {bar};
// --> Error: Can't import bar from module b because it isn't exported. { while Parsing PR#103.dylan }
end;

define module b
  use dylan;
  export bar;
end;


