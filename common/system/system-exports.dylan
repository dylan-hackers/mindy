module:      dylan-user
rcs-header:  $Header: /scm/cvs/src/common/system/Attic/system-exports.dylan,v 1.5 1999/04/20 07:32:38 emk Exp $
author:      Tom Emerson, tree@tiac.net
             [based on stubs from Eric Kidd]
copyright:   Copyright 1999 Thomas R. Emerson

/*
   Copyright (C) 1999 Thomas R. Emerson

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA  02111-1307, USA.

   Bug reports, questions, comments, and suggestions should be sent by
   E-mail to the Internet address "gwydion-bugs@randomhacks.com".
*/

define library system
  use dylan;
  use melange-support;

  use common-dylan;
  use file-system, export: all;
  export
    operating-system;
end library;

define module operating-system
  use dylan;
  use extensions;
  use melange-support;

  use common-dylan,
    exclude: {subclass},
    export: {application-name,
	     application-filename,
	     application-arguments,
	     exit-application,
	     register-exit-application-function};

  export
    $architecture-little-endian?,
    $os-name,
    $os-variant,
    $os-version,
    $machine-name,
    $platform-name;

  export
    login-name,
    login-group,
    owner-name,
    owner-organization;

  export
    environment-variable,
    environment-variable-setter,
    tokenize-environment-variable;
end module;
