Module:       system-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2003 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define interface
  #include { "config.h", "unix-portability.h" },
    define: { "_XOPEN_SOURCE" => "600" }, // SUSv3 (Issue 6)
    import: { "system_errno" => unix-last-error,
              "system_errno_setter" => unix-last-error-setter,
              "ENOENT", "EACCES", "EINVAL", "ERANGE",
              "strerror" => %strerror,

              "struct tm", "system_localtime" => %system-localtime,
              "struct timeval", "gettimeofday" => %gettimeofday,

              "struct utsname",
              "uname" => %uname,

              "getlogin" => %getlogin,

              "getgid" => %getgid, "struct group", "getgrgid" => %getgrgid,

              "getenv" => %getenv, "putenv" => %putenv,
              "unsetenv" => %unsetenv,

	      "system" => %system,

              "struct passwd",
              "getpwnam" => %getpwnam, "getpwuid" => %getpwuid,

              "struct stat",
              "S_IFMT" => $S_IFMT, "S_IFDIR" => $S_IFDIR,
              "S_IFLNK" => $S_IFLNK, "S_IFREG" => $S_IFREG,
              "S_IRWXU" => $S_IRWXU, "S_IRWXG" => $S_IRWXG,
              "S_IRWXO" => $S_IRWXO,
              "S_IRUSR" => $S_IRUSR, "S_IRGRP" => $S_IRGRP,
              "S_IROTH" => $S_IROTH,
              "S_IWUSR" => $S_IWUSR, "S_IWGRP" => $S_IWGRP,
              "S_IWOTH" => $S_IWOTH,
              "S_IXUSR" => $S_IXUSR, "S_IXGRP" => $S_IXGRP,
              "S_IXOTH" => $S_IXOTH,
              "stat" => %stat, "lstat" => %lstat,

              "pathconf" => %pathconf,
              "_PC_SYMLINK_MAX" => $_PC_SYMLINK_MAX,
              "_PC_PATH_MAX" => $_PC_PATH_MAX,

              "readlink" => %readlink,

              "unlink" => %unlink,
              "rename" => %rename,
              "chmod" => %chmod,

              "access" => %access,
              "R_OK" => $R_OK, "W_OK" => $W_OK, "X_OK" => $X_OK,

              "DIR *" => <DIR*>,
              "opendir" => %opendir,
              "struct dirent", "readdir" => %readdir,
              "closedir" => %closedir,

              "mkdir" => %mkdir,
              "rmdir" => %rmdir,
              "getcwd" => %getcwd,
              "chdir" => %chdir,

              "system_open" => %open,
              "O_RDONLY" => $O_RDONLY, "O_WRONLY" => $O_WRONLY,
              "O_RDWR" => $O_RDWR,
              "O_APPEND" => $O_APPEND, "O_CREAT" => $O_CREAT,
              "O_TRUNC" => $O_TRUNC, "O_SYNC" => $O_SYNC },
    name-mapper: minimal-name-mapping,
    equate: { "char *" => <c-string> },
    map: { "char *" => <byte-string> };

  function "system_localtime",
    map-result: <boolean>,
    output-argument: 2,
    output-argument: 3,
    output-argument: 4;
  function "gettimeofday",
    map-result: <boolean>,
    output-argument: 1,
    equate-argument: { 2 => <statically-typed-pointer> };

  struct "struct utsname",
    read-only: #t;
  function "uname",
    output-argument: 1;
  
  function "putenv",
    map-result: <c-string>;

  struct "struct passwd",
    read-only: #t;

  struct "struct stat",
    read-only: #t;
  function "stat",
    map-result: <boolean>,
    output-argument: 2;
  function "lstat",
    map-result: <boolean>,
    output-argument: 2;
  function "unlink",
    map-result: <boolean>;
  function "rename",
    map-result: <boolean>;

  function "getcwd",
    map-result: <c-string>,
    map-argument: { 1 => <c-string> };
  function "readlink",
    map-result: <boolean>,
    map-argument: { 2 => <c-string> };

  struct "struct dirent",
    read-only: #t,
    rename: { "d_name" => dirent-name };
end interface;

define function unix-last-error-message () => (message :: <string>)
  %strerror(unix-last-error())
end function;

define function unix-file-error
    (operation :: <string>, additional-information,
     #rest additional-information-args)
 => (will-never-return :: <bottom>)
  let status-message = unix-last-error-message();
  if (additional-information)
    error(make(<file-system-error>,
               format-string:
                 concatenate("%s: Can't %s ", additional-information),
               format-arguments:
                 concatenate(list(status-message),
                             list(operation),
                             map(method (x)
                                   if (instance?(x, <locator>))
                                     as(<string>, x)
                                   else
                                     x
                                   end
                                 end method,
                                 additional-information-args))))
  else
    error(make(<file-system-error>,
               format-string: "%s: Can't %s",
               format-arguments: list(status-message, operation)))
  end;
end function unix-file-error;
