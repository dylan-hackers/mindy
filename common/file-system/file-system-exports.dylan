module:      dylan-user
rcs-header:  $Header: /scm/cvs/src/common/file-system/Attic/file-system-exports.dylan,v 1.1 1999/04/14 19:32:59 tree Exp $
author:      Tom Emerson, tree@tiac.net
copyright:   Copyright 1999 Thomas R. Emerson

define library file-system
  use dylan;
  use melange-support;
  export file-system;
end library;

define module file-system
  use dylan;
  use extensions;
  use melange-support;

  export <copy/rename-disposition>,
         <pathname>;
  export create-directory,
         delete-directory,
         do-directory,
         ensure-directories-exist;

  export copy-file,
         delete-file,
         file-exists?;

end module file-system;
