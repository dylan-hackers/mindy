module: dir-commands

// To create dir-intr.dylan, execute the following line:
// melange -v --d2c -I/usr/include --shadow-structs dir-intr.intr dir-intr.dylan

c-include("/usr/include/sys/stat.h");
c-include("/usr/include/dirent.h");

define functional class <anonymous-9> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-9>));

define method pointer-value
    (ptr :: <anonymous-9>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-9>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-9>)) => (result :: <integer>);
  1;
end method content-size;

define constant <ulong-t> = <integer>;

define constant <dev-t> = <ulong-t>;

define functional class <anonymous-4> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-4>));

define method pointer-value
    (ptr :: <anonymous-4>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-4>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-4>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-50> (<anonymous-4>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-50>));

define method content-size (value == <anonymous-50>)  => (result :: <integer>);
  3;
end method content-size;

define constant <ino-t> = <ulong-t>;

define constant <mode-t> = <ulong-t>;

define constant <nlink-t> = <ulong-t>;

define constant <uid-t> = <integer>;

define constant <gid-t> = <uid-t>;

define functional class <anonymous-51> (<anonymous-4>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-51>));

define method content-size (value == <anonymous-51>)  => (result :: <integer>);
  2;
end method content-size;

define constant <off-t> = <integer>;

define constant <time-t> = <integer>;

define functional class <timespec> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<timespec>));

define sealed method timespec$tv-sec
    (ptr :: <timespec>) => (result :: <time-t>);
  signed-long-at(ptr, offset: 0);
end method timespec$tv-sec;

define sealed method timespec$tv-sec-setter
    (value :: <time-t>, ptr :: <timespec>) => (result :: <time-t>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method timespec$tv-sec-setter;

define sealed method timespec$tv-nsec
    (ptr :: <timespec>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method timespec$tv-nsec;

define sealed method timespec$tv-nsec-setter
    (value :: <integer>, ptr :: <timespec>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method timespec$tv-nsec-setter;

define method pointer-value (value :: <timespec>, #key index = 0) => (result :: <timespec>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<timespec>)) => (result :: <integer>);
  8;
end method content-size;

define class <virtual-timespec> (<object>)
  slot c-type :: <timespec>, required-init-keyword: c-type:;
  constant virtual slot tv-sec :: <time-t>;
  constant virtual slot tv-nsec :: <integer>;
end class <virtual-timespec>;

define method c-struct-size(of == <virtual-timespec>)
 => (ans :: <integer>);
  content-size(<timespec>)
end method c-struct-size;

define method tv-sec (struct :: <virtual-timespec>)
 => (ans :: <time-t>);
  timespec$tv-sec(struct.c-type)
end method tv-sec;

define method tv-nsec (struct :: <virtual-timespec>)
 => (ans :: <integer>);
  timespec$tv-nsec(struct.c-type)
end method tv-nsec;

define constant <timestruc-t> = <timespec>;

define constant <blkcnt-t> = <integer>;

define functional class <anonymous-52> (<anonymous-9>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-52>));

define method content-size (value == <anonymous-52>)  => (result :: <integer>);
  16;
end method content-size;

define functional class <anonymous-53> (<anonymous-4>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-53>));

define method content-size (value == <anonymous-53>)  => (result :: <integer>);
  8;
end method content-size;

define functional class <stat> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<stat>));

define sealed method stat$st-dev
    (ptr :: <stat>) => (result :: <dev-t>);
  unsigned-long-at(ptr, offset: 0);
end method stat$st-dev;

define sealed method stat$st-dev-setter
    (value :: <dev-t>, ptr :: <stat>) => (result :: <dev-t>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method stat$st-dev-setter;

define sealed method stat$st-pad1
    (ptr :: <stat>) => (result :: <anonymous-50>);
  as(<anonymous-50>, ptr + 4);
end method stat$st-pad1;

define sealed method stat$st-ino
    (ptr :: <stat>) => (result :: <ino-t>);
  unsigned-long-at(ptr, offset: 16);
end method stat$st-ino;

define sealed method stat$st-ino-setter
    (value :: <ino-t>, ptr :: <stat>) => (result :: <ino-t>);
  unsigned-long-at(ptr, offset: 16) := value;
  value;
end method stat$st-ino-setter;

define sealed method stat$st-mode
    (ptr :: <stat>) => (result :: <mode-t>);
  unsigned-long-at(ptr, offset: 20);
end method stat$st-mode;

define sealed method stat$st-mode-setter
    (value :: <mode-t>, ptr :: <stat>) => (result :: <mode-t>);
  unsigned-long-at(ptr, offset: 20) := value;
  value;
end method stat$st-mode-setter;

define sealed method stat$st-nlink
    (ptr :: <stat>) => (result :: <nlink-t>);
  unsigned-long-at(ptr, offset: 24);
end method stat$st-nlink;

define sealed method stat$st-nlink-setter
    (value :: <nlink-t>, ptr :: <stat>) => (result :: <nlink-t>);
  unsigned-long-at(ptr, offset: 24) := value;
  value;
end method stat$st-nlink-setter;

define sealed method stat$st-uid
    (ptr :: <stat>) => (result :: <uid-t>);
  signed-long-at(ptr, offset: 28);
end method stat$st-uid;

define sealed method stat$st-uid-setter
    (value :: <uid-t>, ptr :: <stat>) => (result :: <uid-t>);
  signed-long-at(ptr, offset: 28) := value;
  value;
end method stat$st-uid-setter;

define sealed method stat$st-gid
    (ptr :: <stat>) => (result :: <gid-t>);
  signed-long-at(ptr, offset: 32);
end method stat$st-gid;

define sealed method stat$st-gid-setter
    (value :: <gid-t>, ptr :: <stat>) => (result :: <gid-t>);
  signed-long-at(ptr, offset: 32) := value;
  value;
end method stat$st-gid-setter;

define sealed method stat$st-rdev
    (ptr :: <stat>) => (result :: <dev-t>);
  unsigned-long-at(ptr, offset: 36);
end method stat$st-rdev;

define sealed method stat$st-rdev-setter
    (value :: <dev-t>, ptr :: <stat>) => (result :: <dev-t>);
  unsigned-long-at(ptr, offset: 36) := value;
  value;
end method stat$st-rdev-setter;

define sealed method stat$st-pad2
    (ptr :: <stat>) => (result :: <anonymous-51>);
  as(<anonymous-51>, ptr + 40);
end method stat$st-pad2;

define sealed method stat$st-size
    (ptr :: <stat>) => (result :: <off-t>);
  signed-long-at(ptr, offset: 48);
end method stat$st-size;

define sealed method stat$st-size-setter
    (value :: <off-t>, ptr :: <stat>) => (result :: <off-t>);
  signed-long-at(ptr, offset: 48) := value;
  value;
end method stat$st-size-setter;

define sealed method stat$st-pad3
    (ptr :: <stat>) => (result :: <integer>);
  signed-long-at(ptr, offset: 52);
end method stat$st-pad3;

define sealed method stat$st-pad3-setter
    (value :: <integer>, ptr :: <stat>) => (result :: <integer>);
  signed-long-at(ptr, offset: 52) := value;
  value;
end method stat$st-pad3-setter;

define sealed method stat$st-atim
    (ptr :: <stat>) => (result :: <timestruc-t>);
  as(<timestruc-t>, ptr + 56);
end method stat$st-atim;

define sealed method stat$st-mtim
    (ptr :: <stat>) => (result :: <timestruc-t>);
  as(<timestruc-t>, ptr + 64);
end method stat$st-mtim;

define sealed method stat$st-ctim
    (ptr :: <stat>) => (result :: <timestruc-t>);
  as(<timestruc-t>, ptr + 72);
end method stat$st-ctim;

define sealed method stat$st-blksize
    (ptr :: <stat>) => (result :: <integer>);
  signed-long-at(ptr, offset: 80);
end method stat$st-blksize;

define sealed method stat$st-blksize-setter
    (value :: <integer>, ptr :: <stat>) => (result :: <integer>);
  signed-long-at(ptr, offset: 80) := value;
  value;
end method stat$st-blksize-setter;

define sealed method stat$st-blocks
    (ptr :: <stat>) => (result :: <blkcnt-t>);
  signed-long-at(ptr, offset: 84);
end method stat$st-blocks;

define sealed method stat$st-blocks-setter
    (value :: <blkcnt-t>, ptr :: <stat>) => (result :: <blkcnt-t>);
  signed-long-at(ptr, offset: 84) := value;
  value;
end method stat$st-blocks-setter;

define sealed method stat$st-fstype
    (ptr :: <stat>) => (result :: <anonymous-52>);
  as(<anonymous-52>, ptr + 88);
end method stat$st-fstype;

define sealed method stat$st-pad4
    (ptr :: <stat>) => (result :: <anonymous-53>);
  as(<anonymous-53>, ptr + 104);
end method stat$st-pad4;

define method pointer-value (value :: <stat>, #key index = 0) => (result :: <stat>);
  value + index * 136;
end method pointer-value;

define method content-size (value :: subclass(<stat>)) => (result :: <integer>);
  136;
end method content-size;

define class <virtual-stat> (<object>)
  slot c-type :: <stat>, required-init-keyword: c-type:;
  constant virtual slot st-dev :: <dev-t>;
  constant virtual slot st-pad1 :: <anonymous-50>;
  constant virtual slot st-ino :: <ino-t>;
  constant virtual slot st-mode :: <mode-t>;
  constant virtual slot st-nlink :: <nlink-t>;
  constant virtual slot st-uid :: <uid-t>;
  constant virtual slot st-gid :: <gid-t>;
  constant virtual slot st-rdev :: <dev-t>;
  constant virtual slot st-pad2 :: <anonymous-51>;
  constant virtual slot st-size :: <off-t>;
  constant virtual slot st-pad3 :: <integer>;
  constant virtual slot st-atim :: <timestruc-t>;
  constant virtual slot st-mtim :: <timestruc-t>;
  constant virtual slot st-ctim :: <timestruc-t>;
  constant virtual slot st-blksize :: <integer>;
  constant virtual slot st-blocks :: <blkcnt-t>;
  constant virtual slot st-fstype :: <anonymous-52>;
  constant virtual slot st-pad4 :: <anonymous-53>;
end class <virtual-stat>;

define method c-struct-size(of == <virtual-stat>)
 => (ans :: <integer>);
  content-size(<stat>)
end method c-struct-size;

define method st-dev (struct :: <virtual-stat>)
 => (ans :: <dev-t>);
  stat$st-dev(struct.c-type)
end method st-dev;

define method st-pad1 (struct :: <virtual-stat>)
 => (ans :: <anonymous-50>);
  stat$st-pad1(struct.c-type)
end method st-pad1;

define method st-ino (struct :: <virtual-stat>)
 => (ans :: <ino-t>);
  stat$st-ino(struct.c-type)
end method st-ino;

define method st-mode (struct :: <virtual-stat>)
 => (ans :: <mode-t>);
  stat$st-mode(struct.c-type)
end method st-mode;

define method st-nlink (struct :: <virtual-stat>)
 => (ans :: <nlink-t>);
  stat$st-nlink(struct.c-type)
end method st-nlink;

define method st-uid (struct :: <virtual-stat>)
 => (ans :: <uid-t>);
  stat$st-uid(struct.c-type)
end method st-uid;

define method st-gid (struct :: <virtual-stat>)
 => (ans :: <gid-t>);
  stat$st-gid(struct.c-type)
end method st-gid;

define method st-rdev (struct :: <virtual-stat>)
 => (ans :: <dev-t>);
  stat$st-rdev(struct.c-type)
end method st-rdev;

define method st-pad2 (struct :: <virtual-stat>)
 => (ans :: <anonymous-51>);
  stat$st-pad2(struct.c-type)
end method st-pad2;

define method st-size (struct :: <virtual-stat>)
 => (ans :: <off-t>);
  stat$st-size(struct.c-type)
end method st-size;

define method st-pad3 (struct :: <virtual-stat>)
 => (ans :: <integer>);
  stat$st-pad3(struct.c-type)
end method st-pad3;

define method st-atim (struct :: <virtual-stat>)
 => (ans :: <timestruc-t>);
  stat$st-atim(struct.c-type)
end method st-atim;

define method st-mtim (struct :: <virtual-stat>)
 => (ans :: <timestruc-t>);
  stat$st-mtim(struct.c-type)
end method st-mtim;

define method st-ctim (struct :: <virtual-stat>)
 => (ans :: <timestruc-t>);
  stat$st-ctim(struct.c-type)
end method st-ctim;

define method st-blksize (struct :: <virtual-stat>)
 => (ans :: <integer>);
  stat$st-blksize(struct.c-type)
end method st-blksize;

define method st-blocks (struct :: <virtual-stat>)
 => (ans :: <blkcnt-t>);
  stat$st-blocks(struct.c-type)
end method st-blocks;

define method st-fstype (struct :: <virtual-stat>)
 => (ans :: <anonymous-52>);
  stat$st-fstype(struct.c-type)
end method st-fstype;

define method st-pad4 (struct :: <virtual-stat>)
 => (ans :: <anonymous-53>);
  stat$st-pad4(struct.c-type)
end method st-pad4;

define method lstat
    (arg1 :: <anonymous-9>, arg2 :: <stat>)
 => (result :: <integer>);
  let result-value
    = call-out("lstat", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  values(result-value);
end method lstat;

define functional class <DIR> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<DIR>));

define sealed method DIR$dd-fd
    (ptr :: <DIR>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method DIR$dd-fd;

define sealed method DIR$dd-fd-setter
    (value :: <integer>, ptr :: <DIR>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method DIR$dd-fd-setter;

define sealed method DIR$dd-loc
    (ptr :: <DIR>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method DIR$dd-loc;

define sealed method DIR$dd-loc-setter
    (value :: <integer>, ptr :: <DIR>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method DIR$dd-loc-setter;

define sealed method DIR$dd-size
    (ptr :: <DIR>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method DIR$dd-size;

define sealed method DIR$dd-size-setter
    (value :: <integer>, ptr :: <DIR>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method DIR$dd-size-setter;

define sealed method DIR$dd-buf
    (ptr :: <DIR>) => (result :: <anonymous-9>);
  pointer-at(ptr, offset: 12, class: <anonymous-9>);
end method DIR$dd-buf;

define sealed method DIR$dd-buf-setter
    (value :: <anonymous-9>, ptr :: <DIR>) => (result :: <anonymous-9>);
  pointer-at(ptr, offset: 12, class: <anonymous-9>) := value;
  value;
end method DIR$dd-buf-setter;

define method pointer-value (value :: <DIR>, #key index = 0) => (result :: <DIR>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<DIR>)) => (result :: <integer>);
  16;
end method content-size;

define class <virtual-DIR> (<object>)
  slot c-type :: <DIR>, required-init-keyword: c-type:;
  constant virtual slot dd-fd :: <integer>;
  constant virtual slot dd-loc :: <integer>;
  constant virtual slot dd-size :: <integer>;
  constant virtual slot dd-buf :: <anonymous-9>;
end class <virtual-DIR>;

define method c-struct-size(of == <virtual-DIR>)
 => (ans :: <integer>);
  content-size(<DIR>)
end method c-struct-size;

define method dd-fd (struct :: <virtual-DIR>)
 => (ans :: <integer>);
  DIR$dd-fd(struct.c-type)
end method dd-fd;

define method dd-loc (struct :: <virtual-DIR>)
 => (ans :: <integer>);
  DIR$dd-loc(struct.c-type)
end method dd-loc;

define method dd-size (struct :: <virtual-DIR>)
 => (ans :: <integer>);
  DIR$dd-size(struct.c-type)
end method dd-size;

define method dd-buf (struct :: <virtual-DIR>)
 => (ans :: <anonymous-9>);
  DIR$dd-buf(struct.c-type)
end method dd-buf;

define method opendir
    (arg1 :: <anonymous-9>)
 => (result :: <DIR>);
  let result-value
    = call-out("opendir", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<DIR>, pointer: result-value);
  values(result-value);
end method opendir;

define method closedir
    (arg1 :: <DIR>)
 => (result :: <integer>);
  let result-value
    = call-out("closedir", int:, ptr: (arg1).raw-value);
  values(result-value);
end method closedir;

define functional class <anonymous-63> (<anonymous-9>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-63>));

define method content-size (value == <anonymous-63>)  => (result :: <integer>);
  1;
end method content-size;

define functional class <dirent> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<dirent>));

define sealed method dirent$d-ino
    (ptr :: <dirent>) => (result :: <ino-t>);
  unsigned-long-at(ptr, offset: 0);
end method dirent$d-ino;

define sealed method dirent$d-ino-setter
    (value :: <ino-t>, ptr :: <dirent>) => (result :: <ino-t>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method dirent$d-ino-setter;

define sealed method dirent$d-off
    (ptr :: <dirent>) => (result :: <off-t>);
  signed-long-at(ptr, offset: 4);
end method dirent$d-off;

define sealed method dirent$d-off-setter
    (value :: <off-t>, ptr :: <dirent>) => (result :: <off-t>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method dirent$d-off-setter;

define sealed method dirent$d-reclen
    (ptr :: <dirent>) => (result :: <integer>);
  unsigned-short-at(ptr, offset: 8);
end method dirent$d-reclen;

define sealed method dirent$d-reclen-setter
    (value :: <integer>, ptr :: <dirent>) => (result :: <integer>);
  unsigned-short-at(ptr, offset: 8) := value;
  value;
end method dirent$d-reclen-setter;

define sealed method dirent$d-name
    (ptr :: <dirent>) => (result :: <anonymous-63>);
  as(<anonymous-63>, ptr + 10);
end method dirent$d-name;

define method pointer-value (value :: <dirent>, #key index = 0) => (result :: <dirent>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<dirent>)) => (result :: <integer>);
  12;
end method content-size;

define class <virtual-dirent> (<object>)
  slot c-type :: <dirent>, required-init-keyword: c-type:;
  constant virtual slot d-ino :: <ino-t>;
  constant virtual slot d-off :: <off-t>;
  constant virtual slot d-reclen :: <integer>;
  constant virtual slot d-name :: <anonymous-63>;
end class <virtual-dirent>;

define method c-struct-size(of == <virtual-dirent>)
 => (ans :: <integer>);
  content-size(<dirent>)
end method c-struct-size;

define method d-ino (struct :: <virtual-dirent>)
 => (ans :: <ino-t>);
  dirent$d-ino(struct.c-type)
end method d-ino;

define method d-off (struct :: <virtual-dirent>)
 => (ans :: <off-t>);
  dirent$d-off(struct.c-type)
end method d-off;

define method d-reclen (struct :: <virtual-dirent>)
 => (ans :: <integer>);
  dirent$d-reclen(struct.c-type)
end method d-reclen;

define method d-name (struct :: <virtual-dirent>)
 => (ans :: <anonymous-63>);
  dirent$d-name(struct.c-type)
end method d-name;

define method readdir
    (arg1 :: <DIR>)
 => (result :: <dirent>);
  let result-value
    = call-out("readdir", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<dirent>, pointer: result-value);
  values(result-value);
end method readdir;

