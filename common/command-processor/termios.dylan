module: command-processor

c-include("/usr/include/termios.h");

define constant <speed-t> = <integer>;

define constant <tcflag-t> = <integer>;

define constant <cc-t> = <integer>;

define functional class <anonymous-1> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-1>));

define method pointer-value
    (ptr :: <anonymous-1>, #key index = 0)
 => (result :: <cc-t>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <cc-t>, ptr :: <anonymous-1>, #key index = 0)
 => (result :: <cc-t>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-1>)) => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-0> (<anonymous-1>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-0>));

define method content-size (value == <anonymous-0>)  => (result :: <integer>);
  32;
end method content-size;

define functional class <termios> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<termios>));

define sealed method termios$c-iflag
    (ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 0);
end method termios$c-iflag;

define sealed method termios$c-iflag-setter
    (value :: <tcflag-t>, ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method termios$c-iflag-setter;

define sealed method termios$c-oflag
    (ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 4);
end method termios$c-oflag;

define sealed method termios$c-oflag-setter
    (value :: <tcflag-t>, ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method termios$c-oflag-setter;

define sealed method termios$c-cflag
    (ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 8);
end method termios$c-cflag;

define sealed method termios$c-cflag-setter
    (value :: <tcflag-t>, ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 8) := value;
  value;
end method termios$c-cflag-setter;

define sealed method termios$c-lflag
    (ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 12);
end method termios$c-lflag;

define sealed method termios$c-lflag-setter
    (value :: <tcflag-t>, ptr :: <termios>) => (result :: <tcflag-t>);
  unsigned-long-at(ptr, offset: 12) := value;
  value;
end method termios$c-lflag-setter;

define sealed method termios$c-line
    (ptr :: <termios>) => (result :: <cc-t>);
  unsigned-byte-at(ptr, offset: 16);
end method termios$c-line;

define sealed method termios$c-line-setter
    (value :: <cc-t>, ptr :: <termios>) => (result :: <cc-t>);
  unsigned-byte-at(ptr, offset: 16) := value;
  value;
end method termios$c-line-setter;

define sealed method termios$c-cc
    (ptr :: <termios>) => (result :: <anonymous-0>);
  as(<anonymous-0>, ptr + 17);
end method termios$c-cc;

define sealed method termios$c-ispeed
    (ptr :: <termios>) => (result :: <speed-t>);
  unsigned-long-at(ptr, offset: 52);
end method termios$c-ispeed;

define sealed method termios$c-ispeed-setter
    (value :: <speed-t>, ptr :: <termios>) => (result :: <speed-t>);
  unsigned-long-at(ptr, offset: 52) := value;
  value;
end method termios$c-ispeed-setter;

define sealed method termios$c-ospeed
    (ptr :: <termios>) => (result :: <speed-t>);
  unsigned-long-at(ptr, offset: 56);
end method termios$c-ospeed;

define sealed method termios$c-ospeed-setter
    (value :: <speed-t>, ptr :: <termios>) => (result :: <speed-t>);
  unsigned-long-at(ptr, offset: 56) := value;
  value;
end method termios$c-ospeed-setter;

define method pointer-value (value :: <termios>, #key index = 0) => (result :: <termios>);
  value + index * 60;
end method pointer-value;

define method content-size (value :: subclass(<termios>)) => (result :: <integer>);
  60;
end method content-size;

define method cfgetospeed
    (arg1 :: <termios>)
 => (result :: <speed-t>);
  let result-value
    = call-out("cfgetospeed", unsigned-int:, ptr: (arg1).raw-value);
  values(result-value);
end method cfgetospeed;

define method cfgetispeed
    (arg1 :: <termios>)
 => (result :: <speed-t>);
  let result-value
    = call-out("cfgetispeed", unsigned-int:, ptr: (arg1).raw-value);
  values(result-value);
end method cfgetispeed;

define method cfsetospeed
    (arg1 :: <termios>, arg2 :: <speed-t>)
 => (result :: <integer>);
  let result-value
    = call-out("cfsetospeed", int:, ptr: (arg1).raw-value, unsigned-int: arg2);
  values(result-value);
end method cfsetospeed;

define method cfsetispeed
    (arg1 :: <termios>, arg2 :: <speed-t>)
 => (result :: <integer>);
  let result-value
    = call-out("cfsetispeed", int:, ptr: (arg1).raw-value, unsigned-int: arg2);
  values(result-value);
end method cfsetispeed;

define method cfsetspeed
    (arg1 :: <termios>, arg2 :: <speed-t>)
 => (result :: <integer>);
  let result-value
    = call-out("cfsetspeed", int:, ptr: (arg1).raw-value, unsigned-int: arg2);
  values(result-value);
end method cfsetspeed;

define method tcgetattr
    (arg1 :: <integer>, arg2 :: <termios>)
 => (result :: <integer>);
  let result-value
    = call-out("tcgetattr", int:, int: arg1, ptr: (arg2).raw-value);
  values(result-value);
end method tcgetattr;

define method tcsetattr
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <termios>)
 => (result :: <integer>);
  let result-value
    = call-out("tcsetattr", int:, int: arg1, int: arg2, ptr: (arg3).raw-value);
  values(result-value);
end method tcsetattr;

define method cfmakeraw
    (arg1 :: <termios>)
 => ();
  call-out("cfmakeraw", void:, ptr: (arg1).raw-value);
  values();
end method cfmakeraw;

define method tcsendbreak
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("tcsendbreak", int:, int: arg1, int: arg2);
  values(result-value);
end method tcsendbreak;

define method tcdrain
    (arg1 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("tcdrain", int:, int: arg1);
  values(result-value);
end method tcdrain;

define method tcflush
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("tcflush", int:, int: arg1, int: arg2);
  values(result-value);
end method tcflush;

define method tcflow
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("tcflow", int:, int: arg1, int: arg2);
  values(result-value);
end method tcflow;

define constant $_TERMIOS-H = 1;

define constant $TTYDEF-IFLAG = 11554;

define constant $TTYDEF-OFLAG = 6149;

define constant $TTYDEF-LFLAG = 35355;

define constant $TTYDEF-CFLAG = 1440;

define constant $TTYDEF-SPEED = 13;

define constant $CEOF = 4;

define constant $CEOL = 1;

define constant $CERASE = 127;

define constant $CINTR = 3;

define constant $CSTATUS = 1;

define constant $CKILL = 21;

define constant $CMIN = 1;

define constant $CQUIT = 28;

define constant $CSUSP = 26;

define constant $CTIME = 0;

define constant $CDSUSP = 25;

define constant $CSTART = 17;

define constant $CSTOP = 19;

define constant $CLNEXT = 22;

define constant $CDISCARD = 15;

define constant $CWERASE = 23;

define constant $CREPRINT = 18;

define constant $CEOT = 4;

define constant $CBRK = 1;

define constant $CRPRNT = 18;

define constant $CFLUSH = 15;

define constant $NCCS = 32;

define constant $VINTR = 0;

define constant $VQUIT = 1;

define constant $VERASE = 2;

define constant $VKILL = 3;

define constant $VEOF = 4;

define constant $VTIME = 5;

define constant $VMIN = 6;

define constant $VSWTC = 7;

define constant $VSTART = 8;

define constant $VSTOP = 9;

define constant $VSUSP = 10;

define constant $VEOL = 11;

define constant $VREPRINT = 12;

define constant $VDISCARD = 13;

define constant $VWERASE = 14;

define constant $VLNEXT = 15;

define constant $VEOL2 = 16;

define constant $IGNBRK = 1;

define constant $BRKINT = 2;

define constant $IGNPAR = 4;

define constant $PARMRK = 8;

define constant $INPCK = 16;

define constant $ISTRIP = 32;

define constant $INLCR = 64;

define constant $IGNCR = 128;

define constant $ICRNL = 256;

define constant $IUCLC = 512;

define constant $IXON = 1024;

define constant $IXANY = 2048;

define constant $IXOFF = 4096;

define constant $IMAXBEL = 8192;

define constant $OPOST = 1;

define constant $OLCUC = 2;

define constant $ONLCR = 4;

define constant $OCRNL = 8;

define constant $ONOCR = 16;

define constant $ONLRET = 32;

define constant $OFILL = 64;

define constant $OFDEL = 128;

define constant $NLDLY = 256;

define constant $NL0 = 0;

define constant $NL1 = 256;

define constant $CRDLY = 1536;

define constant $CR0 = 0;

define constant $CR1 = 512;

define constant $CR2 = 1024;

define constant $CR3 = 1536;

define constant $TABDLY = 6144;

define constant $TAB0 = 0;

define constant $TAB1 = 2048;

define constant $TAB2 = 4096;

define constant $TAB3 = 6144;

define constant $BSDLY = 8192;

define constant $BS0 = 0;

define constant $BS1 = 8192;

define constant $FFDLY = 32768;

define constant $FF0 = 0;

define constant $FF1 = 32768;

define constant $VTDLY = 16384;

define constant $VT0 = 0;

define constant $VT1 = 16384;

define constant $XTABS = 6144;

define constant $CBAUD = 4111;

define constant $B0 = 0;

define constant $B50 = 1;

define constant $B75 = 2;

define constant $B110 = 3;

define constant $B134 = 4;

define constant $B150 = 5;

define constant $B200 = 6;

define constant $B300 = 7;

define constant $B600 = 8;

define constant $B1200 = 9;

define constant $B1800 = 10;

define constant $B2400 = 11;

define constant $B4800 = 12;

define constant $B9600 = 13;

define constant $B19200 = 14;

define constant $B38400 = 15;

define constant $EXTA = 14;

define constant $EXTB = 15;

define constant $CSIZE = 48;

define constant $CS5 = 0;

define constant $CS6 = 16;

define constant $CS7 = 32;

define constant $CS8 = 48;

define constant $CSTOPB = 64;

define constant $CREAD = 128;

define constant $PARENB = 256;

define constant $PARODD = 512;

define constant $HUPCL = 1024;

define constant $CLOCAL = 2048;

define constant $CBAUDEX = 4096;

define constant $B57600 = 4097;

define constant $B115200 = 4098;

define constant $B230400 = 4099;

define constant $B460800 = 4100;

define constant $B500000 = 4101;

define constant $B576000 = 4102;

define constant $B921600 = 4103;

define constant $B1000000 = 4104;

define constant $B1152000 = 4105;

define constant $B1500000 = 4106;

define constant $B2000000 = 4107;

define constant $B2500000 = 4108;

define constant $B3000000 = 4109;

define constant $B3500000 = 4110;

define constant $B4000000 = 4111;

define constant $__MAX-BAUD = 4111;

define constant $CIBAUD = 269418496;

define constant $ISIG = 1;

define constant $ICANON = 2;

define constant $XCASE = 4;

define constant $ECHO = 8;

define constant $ECHOE = 16;

define constant $ECHOK = 32;

define constant $ECHONL = 64;

define constant $NOFLSH = 128;

define constant $TOSTOP = 256;

define constant $ECHOCTL = 512;

define constant $ECHOPRT = 1024;

define constant $ECHOKE = 2048;

define constant $FLUSHO = 4096;

define constant $PENDIN = 16384;

define constant $IEXTEN = 32768;

define constant $TCOOFF = 0;

define constant $TCOON = 1;

define constant $TCIOFF = 2;

define constant $TCION = 3;

define constant $TCIFLUSH = 0;

define constant $TCOFLUSH = 1;

define constant $TCIOFLUSH = 2;

define constant $TCSANOW = 0;

define constant $TCSADRAIN = 1;

define constant $TCSAFLUSH = 2;

define constant $_FEATURES-H = 1;

define constant $__USE-ANSI = 1;

define constant $_BSD-SOURCE = 1;

define constant $_SVID-SOURCE = 1;

define constant $_POSIX-SOURCE = 1;

define constant $_POSIX-C-SOURCE = 199506;

define constant $__USE-POSIX = 1;

define constant $__USE-POSIX2 = 1;

define constant $__USE-POSIX199309 = 1;

define constant $__USE-POSIX199506 = 1;

define constant $__USE-MISC = 1;

define constant $__USE-BSD = 1;

define constant $__USE-SVID = 1;

define constant $__STDC-IEC-559-- = 1;

define constant $__STDC-IEC-559-COMPLEX-- = 1;

define constant $__STDC-ISO-10646-- = 200009;

define constant $__GNU-LIBRARY-- = 6;

define constant $__GLIBC-- = 2;

define constant $__GLIBC-MINOR-- = 2;

define constant $_SYS-CDEFS-H = 1;

define constant <__signed> = <integer>;

define constant <__ptr-t> = <machine-pointer>;

define constant <__long-double-t> = <extended-float>;

