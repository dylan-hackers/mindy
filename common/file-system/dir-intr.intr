module: dir-commands

// call me with:
// melange -v --d2c -I/usr/include --shadow-structs dir.intr dir-intr.dylan

define interface
  #include { "sys/stat.h", "dirent.h" };
end interface;
