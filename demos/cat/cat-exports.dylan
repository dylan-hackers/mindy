module: dylan-user
rcs-header: $Header: /scm/cvs/src/demos/cat/cat-exports.dylan,v 1.1 1998/05/03 19:55:59 andreas Exp $

define library cat
  use Dylan;
  use streams;
  use Standard-IO;
end;

define module cat
  use Dylan;
  use Extensions;
  use streams;
  use Standard-IO;
end;
