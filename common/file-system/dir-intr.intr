module: dir-commands

// To create dir-intr.dylan, execute the following line:
// melange -v --d2c -I/usr/include --shadow-structs dir-intr.intr dir-intr.dylan

define interface
  #include { "sys/stat.h", "dirent.h" },
    import: { "readdir", "opendir", "closedir", "lstat", 
             "S_IFMT", "S_IFDIR", "S_IFLNK", "S_IFREG"},
  name-mapper: minimal-name-mapping;
end interface;

define method is-dir?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, $S-IFMT) = $S-IFDIR;
end method is-dir?;

define method is-link?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, $S-IFMT) = $S-IFLNK;
end method is-link?;

define method is-regular-file?(mode :: <integer>) => (ans :: <boolean>)
  logand(mode, $S-IFMT) = $S-IFREG;
end method is-regular-file?;

define function stat-mode(file :: <pathname>) => (bar :: <integer>)
  with-pointer(stat* = <stat>)
    with-pointer(str* = file)
      lstat(str*, stat*);
      st-mode(stat*);
    end with-pointer;
  end with-pointer;
end function stat-mode;

