module: Dylan-User
author: Gwydion Group

define library Format-out
  use Dylan;
  use Format,
    export: {Format};
  use Standard-IO,
    export: {Standard-io};

  export Format-out;
//  export Format;
//  export Standard-io;
end library Format-out;

define module Format-out
  use Dylan;
  use Format;
  use Standard-IO;

  export format-out;
end module Format-out;
