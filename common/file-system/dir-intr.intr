module: dir-commands

// To create dir-intr.dylan, execute the following line:
// melange -v --d2c -I/usr/include --shadow-structs dir-intr.intr dir-intr.dylan

define interface
  #include { "sys/stat.h", "dirent.h" },
    import: { "readdir", "opendir", "closedir", "lstat" },
    exclude: { 	"seekdir", "rewinddir", "telldir", "fstat", 
	       	"mknod", "fchmod", "mkfifo", "umask", "_ST_FSTYPSZ",
	       	"S_IFMT", "S_IAMB", "S_IFIFO", "S_IFCHR", "S_IFNAM",
		"S_INSEM", "S_INSHD", "S_IFBLK", "S_IFDOOR" };
end interface;
