Module:       dylan-user
Synopsis:     Abstract modeling of locations
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library locators
  use common-dylan;

  export locators;
  //---*** Bootstrapping: remove when 2.0b2c1 has been distributed
  export locator-internals;
end library locators;

define module locators
  use locators-protocol, export: all;

  // Protocol classes
  create <server-locator>,
         <physical-locator>,
         <directory-locator>,
         <file-locator>;

  // Protocol functions
  create locator-host,
         locator-server,
         locator-volume,
         locator-directory,
         locator-relative?,
         locator-path,
         locator-base,
         locator-extension,
         locator-name;

  // Coercion protocols
  create locator-as-string,
         string-as-locator;

  // Locator conditions
  create <locator-error>,
         locator-error;

  // Utilities
  create simplify-locator,
         subdirectory-locator,
         relative-locator,
         merge-locators;

  // Posix locators
  create <posix-physical-locator>,
         <posix-directory-locator>,
         <posix-file-locator>;

  // Microsoft locators
  create <microsoft-server-locator>,
         <microsoft-unc-locator>,
         <microsoft-volume-locator>,
         <microsoft-physical-locator>,
         <microsoft-directory-locator>,
         <microsoft-file-locator>;

  // Native locators
  create <native-physical-locator>,
         <native-directory-locator>,
         <native-file-locator>;

  // Web locators
  create <web-locator>,
         <url>,
         <server-url>,
         <http-server>,
         <ftp-server>,
         <file-server>,
         <directory-url>,
         <file-url>,
         <file-index-url>,
         <cgi-url>,
         <mail-to-locator>,
         locator-address,
         locator-cgi-string,
         locator-index,
         locator-password,
         locator-protocol,
         locator-username,
         http-parser,
         ftp-parser,
         file-parser,
         mailto-parser;

  //---*** Bootstrapping: remove when 2.0b2c1 has been distributed
  create <native-locator>,
         locator-prefix,
         <path>,
         <directory-path>,
         path-elements,
         relative-path?,
         locator-directory-path,
         abbreviate-locator,
         override-locator;
end module locators;

//---*** Bootstrapping: remove when 2.0b2c1 has been distributed
define module locator-internals
  use locators;
end module locator-internals;

define module locators-internals
  use common-dylan;
  use streams-protocol;

  use locators-protocol;
  use locators;

  //---*** Bootstrapping: remove when 2.0b2c1 has been distributed
  use locator-internals;
end module locators-internals;
