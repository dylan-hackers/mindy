module: network-internal

c-include(".//sockets-helper.h");

define functional class <anonymous-36> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-36>));

define method pointer-value
    (ptr :: <anonymous-36>, #key index = 0)
 => (result :: <integer>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-36>, #key index = 0)
 => (result :: <integer>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-36>)) => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-115> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-115>));

define method content-size (value == <anonymous-115>)  => (result :: <integer>);
  14;
end method content-size;

define functional class <osockaddr> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<osockaddr>));

define sealed method get-sa-family
    (ptr :: <osockaddr>) => (result :: <integer>);
  unsigned-short-at(ptr, offset: 0);
end method get-sa-family;

define sealed method get-sa-family-setter
    (value :: <integer>, ptr :: <osockaddr>) => (result :: <integer>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method get-sa-family-setter;

define sealed method get-sa-data
    (ptr :: <osockaddr>) => (result :: <anonymous-115>);
  as(<anonymous-115>, ptr + 2);
end method get-sa-data;

define method pointer-value (value :: <osockaddr>, #key index = 0) => (result :: <osockaddr>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<osockaddr>)) => (result :: <integer>);
  16;
end method content-size;

define constant <anonymous-116> = limited(<integer>, min: 0, max: 2);
define constant $SHUT-RD :: <anonymous-116> = 0;
define constant $SHUT-WR :: <anonymous-116> = 1;
define constant $SHUT-RDWR :: <anonymous-116> = 2;

define method socket
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("socket", int:, int: arg1, int: arg2, int: arg3);
  values(result-value);
end method socket;

define functional class <anonymous-7> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-7>));

define method pointer-value
    (ptr :: <anonymous-7>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-7>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-7>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-118> (<anonymous-7>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-118>));

define method content-size (value == <anonymous-118>)  => (result :: <integer>);
  2;
end method content-size;

define method socketpair
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <anonymous-118>)
 => (result :: <integer>);
  let result-value
    = call-out("socketpair", int:, int: arg1, int: arg2, int: arg3, ptr: (arg4).raw-value);
  values(result-value);
end method socketpair;

define constant <sa-family-t> = <integer>;

define functional class <c-char-vector> (<c-vector>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<c-char-vector>));

define method pointer-value
    (ptr :: <c-char-vector>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <c-char-vector>, #key index = 0)
 => (result :: <integer>);
  signed-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<c-char-vector>)) => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-30> (<c-char-vector>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-30>));

define method content-size (value == <anonymous-30>)  => (result :: <integer>);
  14;
end method content-size;

define functional class <sockaddr> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<sockaddr>));

define sealed method get-sa-family
    (ptr :: <sockaddr>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0);
end method get-sa-family;

define sealed method get-sa-family-setter
    (value :: <sa-family-t>, ptr :: <sockaddr>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method get-sa-family-setter;

define sealed method get-sa-data
    (ptr :: <sockaddr>) => (result :: <anonymous-30>);
  as(<anonymous-30>, ptr + 2);
end method get-sa-data;

define method pointer-value (value :: <sockaddr>, #key index = 0) => (result :: <sockaddr>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<sockaddr>)) => (result :: <integer>);
  16;
end method content-size;

define constant <__socklen-t> = <integer>;

define constant <socklen-t> = <__socklen-t>;

define method bind
    (arg1 :: <integer>, arg2 :: <sockaddr>, arg3 :: <socklen-t>)
 => (result :: <integer>);
  let result-value
    = call-out("bind", int:, int: arg1, ptr: (arg2).raw-value, unsigned-int: arg3);
  values(result-value);
end method bind;

define functional class <anonymous-121> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-121>));

define method pointer-value
    (ptr :: <anonymous-121>, #key index = 0)
 => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <socklen-t>, ptr :: <anonymous-121>, #key index = 0)
 => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-121>)) => (result :: <integer>);
  4;
end method content-size;

define method getsockname
    (arg1 :: <integer>, arg2 :: <sockaddr>, arg3 :: <anonymous-121>)
 => (result :: <integer>);
  let result-value
    = call-out("getsockname", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method getsockname;

define method connect
    (arg1 :: <integer>, arg2 :: <sockaddr>, arg3 :: <socklen-t>)
 => (result :: <integer>);
  let result-value
    = call-out("connect", int:, int: arg1, ptr: (arg2).raw-value, unsigned-int: arg3);
  values(result-value);
end method connect;

define method getpeername
    (arg1 :: <integer>, arg2 :: <sockaddr>, arg3 :: <anonymous-121>)
 => (result :: <integer>);
  let result-value
    = call-out("getpeername", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method getpeername;

define constant <__ssize-t> = <integer>;

define constant <ssize-t> = <__ssize-t>;

define constant <size-t> = <integer>;

define method send
    (arg1 :: <integer>, arg2 :: <machine-pointer>, arg3 :: <size-t>, arg4 :: <integer>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("send", int:, int: arg1, ptr: (arg2).raw-value, long: arg3, int: arg4);
  values(result-value);
end method send;

define method recv
    (arg1 :: <integer>, arg2 :: <machine-pointer>, arg3 :: <size-t>, arg4 :: <integer>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("recv", int:, int: arg1, ptr: (arg2).raw-value, long: arg3, int: arg4);
  values(result-value);
end method recv;

define method sendto
    (arg1 :: <integer>, arg2 :: <machine-pointer>, arg3 :: <size-t>, arg4 :: <integer>, arg5 :: <sockaddr>, arg6 :: <socklen-t>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("sendto", int:, int: arg1, ptr: (arg2).raw-value, long: arg3, int: arg4, ptr: (arg5).raw-value, unsigned-int: arg6);
  values(result-value);
end method sendto;

define method recvfrom
    (arg1 :: <integer>, arg2 :: <machine-pointer>, arg3 :: <size-t>, arg4 :: <integer>, arg5 :: <sockaddr>, arg6 :: <anonymous-121>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("recvfrom", int:, int: arg1, ptr: (arg2).raw-value, long: arg3, int: arg4, ptr: (arg5).raw-value, ptr: (arg6).raw-value);
  values(result-value);
end method recvfrom;

define functional class <iovec> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<iovec>));

define sealed method get-iov-base
    (ptr :: <iovec>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 0, class: <machine-pointer>);
end method get-iov-base;

define sealed method get-iov-base-setter
    (value :: <machine-pointer>, ptr :: <iovec>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 0, class: <machine-pointer>) := value;
  value;
end method get-iov-base-setter;

define sealed method get-iov-len
    (ptr :: <iovec>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 4);
end method get-iov-len;

define sealed method get-iov-len-setter
    (value :: <size-t>, ptr :: <iovec>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method get-iov-len-setter;

define method pointer-value (value :: <iovec>, #key index = 0) => (result :: <iovec>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<iovec>)) => (result :: <integer>);
  8;
end method content-size;

define functional class <msghdr> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<msghdr>));

define sealed method get-msg-name
    (ptr :: <msghdr>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 0, class: <machine-pointer>);
end method get-msg-name;

define sealed method get-msg-name-setter
    (value :: <machine-pointer>, ptr :: <msghdr>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 0, class: <machine-pointer>) := value;
  value;
end method get-msg-name-setter;

define sealed method get-msg-namelen
    (ptr :: <msghdr>) => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: 4);
end method get-msg-namelen;

define sealed method get-msg-namelen-setter
    (value :: <socklen-t>, ptr :: <msghdr>) => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method get-msg-namelen-setter;

define sealed method get-msg-iov
    (ptr :: <msghdr>) => (result :: <iovec>);
  pointer-at(ptr, offset: 8, class: <iovec>);
end method get-msg-iov;

define sealed method get-msg-iov-setter
    (value :: <iovec>, ptr :: <msghdr>) => (result :: <iovec>);
  pointer-at(ptr, offset: 8, class: <iovec>) := value;
  value;
end method get-msg-iov-setter;

define sealed method get-msg-iovlen
    (ptr :: <msghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method get-msg-iovlen;

define sealed method get-msg-iovlen-setter
    (value :: <integer>, ptr :: <msghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method get-msg-iovlen-setter;

define sealed method get-msg-control
    (ptr :: <msghdr>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 16, class: <machine-pointer>);
end method get-msg-control;

define sealed method get-msg-control-setter
    (value :: <machine-pointer>, ptr :: <msghdr>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 16, class: <machine-pointer>) := value;
  value;
end method get-msg-control-setter;

define sealed method get-msg-controllen
    (ptr :: <msghdr>) => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: 20);
end method get-msg-controllen;

define sealed method get-msg-controllen-setter
    (value :: <socklen-t>, ptr :: <msghdr>) => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: 20) := value;
  value;
end method get-msg-controllen-setter;

define sealed method get-msg-flags
    (ptr :: <msghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24);
end method get-msg-flags;

define sealed method get-msg-flags-setter
    (value :: <integer>, ptr :: <msghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24) := value;
  value;
end method get-msg-flags-setter;

define method pointer-value (value :: <msghdr>, #key index = 0) => (result :: <msghdr>);
  value + index * 28;
end method pointer-value;

define method content-size (value :: subclass(<msghdr>)) => (result :: <integer>);
  28;
end method content-size;

define method sendmsg
    (arg1 :: <integer>, arg2 :: <msghdr>, arg3 :: <integer>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("sendmsg", int:, int: arg1, ptr: (arg2).raw-value, int: arg3);
  values(result-value);
end method sendmsg;

define method recvmsg
    (arg1 :: <integer>, arg2 :: <msghdr>, arg3 :: <integer>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("recvmsg", int:, int: arg1, ptr: (arg2).raw-value, int: arg3);
  values(result-value);
end method recvmsg;

define method getsockopt
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <machine-pointer>, arg5 :: <anonymous-121>)
 => (result :: <integer>);
  let result-value
    = call-out("getsockopt", int:, int: arg1, int: arg2, int: arg3, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method getsockopt;

define method setsockopt
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <machine-pointer>, arg5 :: <socklen-t>)
 => (result :: <integer>);
  let result-value
    = call-out("setsockopt", int:, int: arg1, int: arg2, int: arg3, ptr: (arg4).raw-value, unsigned-int: arg5);
  values(result-value);
end method setsockopt;

define method listen
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("listen", int:, int: arg1, int: arg2);
  values(result-value);
end method listen;

define method accept
    (arg1 :: <integer>, arg2 :: <sockaddr>, arg3 :: <anonymous-121>)
 => (result :: <integer>);
  let result-value
    = call-out("accept", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method accept;

define method shutdown
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("shutdown", int:, int: arg1, int: arg2);
  values(result-value);
end method shutdown;

define method isfdtype
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("isfdtype", int:, int: arg1, int: arg2);
  values(result-value);
end method isfdtype;

define constant $_SYS-SOCKET-H = 1;

define constant <__SOCKADDR-ARG> = <sockaddr>;

define constant <__CONST-SOCKADDR-ARG> = <sockaddr>;

define constant <__socket-type> = limited(<integer>, min: 1, max: 10);
define constant $SOCK-STREAM :: <__socket-type> = 1;
define constant $SOCK-DGRAM :: <__socket-type> = 2;
define constant $SOCK-RAW :: <__socket-type> = 3;
define constant $SOCK-RDM :: <__socket-type> = 4;
define constant $SOCK-SEQPACKET :: <__socket-type> = 5;
define constant $SOCK-PACKET :: <__socket-type> = 10;

define constant <__uint32-t> = <integer>;

define functional class <anonymous-31> (<c-char-vector>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-31>));

define method content-size (value == <anonymous-31>)  => (result :: <integer>);
  120;
end method content-size;

define functional class <sockaddr-storage> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<sockaddr-storage>));

define sealed method get-ss-family
    (ptr :: <sockaddr-storage>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0);
end method get-ss-family;

define sealed method get-ss-family-setter
    (value :: <sa-family-t>, ptr :: <sockaddr-storage>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method get-ss-family-setter;

define sealed method get-__ss-align
    (ptr :: <sockaddr-storage>) => (result :: <__uint32-t>);
  unsigned-long-at(ptr, offset: 4);
end method get-__ss-align;

define sealed method get-__ss-align-setter
    (value :: <__uint32-t>, ptr :: <sockaddr-storage>) => (result :: <__uint32-t>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method get-__ss-align-setter;

define sealed method get-__ss-padding
    (ptr :: <sockaddr-storage>) => (result :: <anonymous-31>);
  as(<anonymous-31>, ptr + 8);
end method get-__ss-padding;

define method pointer-value (value :: <sockaddr-storage>, #key index = 0) => (result :: <sockaddr-storage>);
  value + index * 128;
end method pointer-value;

define method content-size (value :: subclass(<sockaddr-storage>)) => (result :: <integer>);
  128;
end method content-size;

define constant <anonymous-32> = limited(<integer>, min: 1, max: 32768);
define constant $MSG-OOB :: <anonymous-32> = 1;
define constant $MSG-PEEK :: <anonymous-32> = 2;
define constant $MSG-DONTROUTE :: <anonymous-32> = 4;
define constant $MSG-CTRUNC :: <anonymous-32> = 8;
define constant $MSG-PROXY :: <anonymous-32> = 16;
define constant $MSG-TRUNC :: <anonymous-32> = 32;
define constant $MSG-DONTWAIT :: <anonymous-32> = 64;
define constant $MSG-EOR :: <anonymous-32> = 128;
define constant $MSG-WAITALL :: <anonymous-32> = 256;
define constant $MSG-FIN :: <anonymous-32> = 512;
define constant $MSG-SYN :: <anonymous-32> = 1024;
define constant $MSG-CONFIRM :: <anonymous-32> = 2048;
define constant $MSG-RST :: <anonymous-32> = 4096;
define constant $MSG-ERRQUEUE :: <anonymous-32> = 8192;
define constant $MSG-NOSIGNAL :: <anonymous-32> = 16384;
define constant $MSG-MORE :: <anonymous-32> = 32768;

define functional class <cmsghdr> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<cmsghdr>));

define sealed method get-cmsg-len
    (ptr :: <cmsghdr>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 0);
end method get-cmsg-len;

define sealed method get-cmsg-len-setter
    (value :: <size-t>, ptr :: <cmsghdr>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method get-cmsg-len-setter;

define sealed method get-cmsg-level
    (ptr :: <cmsghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method get-cmsg-level;

define sealed method get-cmsg-level-setter
    (value :: <integer>, ptr :: <cmsghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-cmsg-level-setter;

define sealed method get-cmsg-type
    (ptr :: <cmsghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-cmsg-type;

define sealed method get-cmsg-type-setter
    (value :: <integer>, ptr :: <cmsghdr>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-cmsg-type-setter;

define method pointer-value (value :: <cmsghdr>, #key index = 0) => (result :: <cmsghdr>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<cmsghdr>)) => (result :: <integer>);
  12;
end method content-size;

define method __cmsg-nxthdr
    (arg1 :: <msghdr>, arg2 :: <cmsghdr>)
 => (result :: <cmsghdr>);
  let result-value
    = call-out("__cmsg_nxthdr", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<cmsghdr>, pointer: result-value);
  values(result-value);
end method __cmsg-nxthdr;

define constant <anonymous-34> = limited(<integer>, min: 1, max: 3);
define constant $SCM-RIGHTS :: <anonymous-34> = 1;
define constant $SCM-CREDENTIALS :: <anonymous-34> = 2;
define constant $__SCM-CONNECT :: <anonymous-34> = 3;

define constant <__pid-t> = <integer>;

define constant <pid-t> = <__pid-t>;

define constant <__u-int> = <integer>;

define constant <__uid-t> = <__u-int>;

define constant <uid-t> = <__uid-t>;

define constant <__gid-t> = <__u-int>;

define constant <gid-t> = <__gid-t>;

define functional class <ucred> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<ucred>));

define sealed method get-pid
    (ptr :: <ucred>) => (result :: <pid-t>);
  signed-long-at(ptr, offset: 0);
end method get-pid;

define sealed method get-pid-setter
    (value :: <pid-t>, ptr :: <ucred>) => (result :: <pid-t>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-pid-setter;

define sealed method get-uid
    (ptr :: <ucred>) => (result :: <uid-t>);
  unsigned-long-at(ptr, offset: 4);
end method get-uid;

define sealed method get-uid-setter
    (value :: <uid-t>, ptr :: <ucred>) => (result :: <uid-t>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method get-uid-setter;

define sealed method get-gid
    (ptr :: <ucred>) => (result :: <gid-t>);
  unsigned-long-at(ptr, offset: 8);
end method get-gid;

define sealed method get-gid-setter
    (value :: <gid-t>, ptr :: <ucred>) => (result :: <gid-t>);
  unsigned-long-at(ptr, offset: 8) := value;
  value;
end method get-gid-setter;

define method pointer-value (value :: <ucred>, #key index = 0) => (result :: <ucred>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<ucred>)) => (result :: <integer>);
  12;
end method content-size;

define functional class <linger> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<linger>));

define sealed method get-l-onoff
    (ptr :: <linger>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-l-onoff;

define sealed method get-l-onoff-setter
    (value :: <integer>, ptr :: <linger>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-l-onoff-setter;

define sealed method get-l-linger
    (ptr :: <linger>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method get-l-linger;

define sealed method get-l-linger-setter
    (value :: <integer>, ptr :: <linger>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-l-linger-setter;

define method pointer-value (value :: <linger>, #key index = 0) => (result :: <linger>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<linger>)) => (result :: <integer>);
  8;
end method content-size;

define constant $PF-UNSPEC = 0;

define constant $PF-LOCAL = 1;

define constant $PF-UNIX = 1;

define constant $PF-FILE = 1;

define constant $PF-INET = 2;

define constant $PF-AX25 = 3;

define constant $PF-IPX = 4;

define constant $PF-APPLETALK = 5;

define constant $PF-NETROM = 6;

define constant $PF-BRIDGE = 7;

define constant $PF-ATMPVC = 8;

define constant $PF-X25 = 9;

define constant $PF-INET6 = 10;

define constant $PF-ROSE = 11;

define constant $PF-DE-Cnet = 12;

define constant $PF-NETBEUI = 13;

define constant $PF-SECURITY = 14;

define constant $PF-KEY = 15;

define constant $PF-NETLINK = 16;

define constant $PF-ROUTE = 16;

define constant $PF-PACKET = 17;

define constant $PF-ASH = 18;

define constant $PF-ECONET = 19;

define constant $PF-ATMSVC = 20;

define constant $PF-SNA = 22;

define constant $PF-IRDA = 23;

define constant $PF-PPPOX = 24;

define constant $PF-WANPIPE = 25;

define constant $PF-BLUETOOTH = 31;

define constant $PF-MAX = 32;

define constant $AF-UNSPEC = 0;

define constant $AF-LOCAL = 1;

define constant $AF-UNIX = 1;

define constant $AF-FILE = 1;

define constant $AF-INET = 2;

define constant $AF-AX25 = 3;

define constant $AF-IPX = 4;

define constant $AF-APPLETALK = 5;

define constant $AF-NETROM = 6;

define constant $AF-BRIDGE = 7;

define constant $AF-ATMPVC = 8;

define constant $AF-X25 = 9;

define constant $AF-INET6 = 10;

define constant $AF-ROSE = 11;

define constant $AF-DE-Cnet = 12;

define constant $AF-NETBEUI = 13;

define constant $AF-SECURITY = 14;

define constant $AF-KEY = 15;

define constant $AF-NETLINK = 16;

define constant $AF-ROUTE = 16;

define constant $AF-PACKET = 17;

define constant $AF-ASH = 18;

define constant $AF-ECONET = 19;

define constant $AF-ATMSVC = 20;

define constant $AF-SNA = 22;

define constant $AF-IRDA = 23;

define constant $AF-PPPOX = 24;

define constant $AF-WANPIPE = 25;

define constant $AF-BLUETOOTH = 31;

define constant $AF-MAX = 32;

define constant $SOL-RAW = 255;

define constant $SOL-DECNET = 261;

define constant $SOL-X25 = 262;

define constant $SOL-PACKET = 263;

define constant $SOL-ATM = 264;

define constant $SOL-AAL = 265;

define constant $SOL-IRDA = 266;

define constant $SOMAXCONN = 128;

define constant <__ss-aligntype> = <__uint32-t>;

define constant $_SS-SIZE = 128;

define constant $_SS-PADSIZE = 120;

define constant $SOL-SOCKET = 1;

define constant $SO-DEBUG = 1;

define constant $SO-REUSEADDR = 2;

define constant $SO-TYPE = 3;

define constant $SO-ERROR = 4;

define constant $SO-DONTROUTE = 5;

define constant $SO-BROADCAST = 6;

define constant $SO-SNDBUF = 7;

define constant $SO-RCVBUF = 8;

define constant $SO-KEEPALIVE = 9;

define constant $SO-OOBINLINE = 10;

define constant $SO-NO-CHECK = 11;

define constant $SO-PRIORITY = 12;

define constant $SO-LINGER = 13;

define constant $SO-BSDCOMPAT = 14;

define constant $SO-PASSCRED = 16;

define constant $SO-PEERCRED = 17;

define constant $SO-RCVLOWAT = 18;

define constant $SO-SNDLOWAT = 19;

define constant $SO-RCVTIMEO = 20;

define constant $SO-SNDTIMEO = 21;

define constant $SO-SECURITY-AUTHENTICATION = 22;

define constant $SO-SECURITY-ENCRYPTION-TRANSPORT = 23;

define constant $SO-SECURITY-ENCRYPTION-NETWORK = 24;

define constant $SO-BINDTODEVICE = 25;

define constant $SO-ATTACH-FILTER = 26;

define constant $SO-DETACH-FILTER = 27;

define constant $SO-PEERNAME = 28;

define constant $SO-TIMESTAMP = 29;

define constant $SCM-TIMESTAMP = 29;

define constant $SO-ACCEPTCONN = 30;

define constant $FIOSETOWN = 35073;

define constant $SIOCSPGRP = 35074;

define constant $FIOGETOWN = 35075;

define constant $SIOCGPGRP = 35076;

define constant $SIOCATMARK = 35077;

define constant $SIOCGSTAMP = 35078;

define constant $_BITS-SOCKADDR-H = 1;

define constant $__SOCKADDR-COMMON-SIZE = 2;

define constant <__u-char> = <integer>;

define constant <u-char> = <__u-char>;

define constant <__u-short> = <integer>;

define constant <u-short> = <__u-short>;

define constant <u-int> = <__u-int>;

define constant <__u-long> = <integer>;

define constant <u-long> = <__u-long>;

define functional class <anonymous-1> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-1>));

define method pointer-value
    (ptr :: <anonymous-1>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-1>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-1>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-0> (<anonymous-1>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-0>));

define method content-size (value == <anonymous-0>)  => (result :: <integer>);
  2;
end method content-size;

define functional class <__quad-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__quad-t>));

define sealed method get-__val
    (ptr :: <__quad-t>) => (result :: <anonymous-0>);
  as(<anonymous-0>, ptr + 0);
end method get-__val;

define method pointer-value (value :: <__quad-t>, #key index = 0) => (result :: <__quad-t>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<__quad-t>)) => (result :: <integer>);
  8;
end method content-size;

define constant <quad-t> = <__quad-t>;

define functional class <anonymous-4> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-4>));

define method pointer-value
    (ptr :: <anonymous-4>, #key index = 0)
 => (result :: <__u-long>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <__u-long>, ptr :: <anonymous-4>, #key index = 0)
 => (result :: <__u-long>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-4>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-3> (<anonymous-4>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-3>));

define method content-size (value == <anonymous-3>)  => (result :: <integer>);
  2;
end method content-size;

define functional class <__u-quad-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__u-quad-t>));

define sealed method get-__val
    (ptr :: <__u-quad-t>) => (result :: <anonymous-3>);
  as(<anonymous-3>, ptr + 0);
end method get-__val;

define method pointer-value (value :: <__u-quad-t>, #key index = 0) => (result :: <__u-quad-t>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<__u-quad-t>)) => (result :: <integer>);
  8;
end method content-size;

define constant <u-quad-t> = <__u-quad-t>;

define functional class <anonymous-6> (<anonymous-7>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-6>));

define method content-size (value == <anonymous-6>)  => (result :: <integer>);
  2;
end method content-size;

define functional class <__fsid-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__fsid-t>));

define sealed method get-__val
    (ptr :: <__fsid-t>) => (result :: <anonymous-6>);
  as(<anonymous-6>, ptr + 0);
end method get-__val;

define method pointer-value (value :: <__fsid-t>, #key index = 0) => (result :: <__fsid-t>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<__fsid-t>)) => (result :: <integer>);
  8;
end method content-size;

define constant <fsid-t> = <__fsid-t>;

define constant <__loff-t> = <__quad-t>;

define constant <loff-t> = <__loff-t>;

define constant <__ino-t> = <__u-long>;

define constant <ino-t> = <__ino-t>;

define constant <__dev-t> = <__u-quad-t>;

define constant <dev-t> = <__dev-t>;

define constant <__mode-t> = <__u-int>;

define constant <mode-t> = <__mode-t>;

define constant <__nlink-t> = <__u-int>;

define constant <nlink-t> = <__nlink-t>;

define constant <__off-t> = <integer>;

define constant <off-t> = <__off-t>;

define constant <__id-t> = <__u-int>;

define constant <id-t> = <__id-t>;

define constant <__daddr-t> = <integer>;

define constant <daddr-t> = <__daddr-t>;

define constant <__caddr-t> = <c-char-vector>;

define constant <caddr-t> = <__caddr-t>;

define constant <__key-t> = <integer>;

define constant <key-t> = <__key-t>;

define constant <ulong> = <integer>;

define constant <ushort> = <integer>;

define constant <uint> = <integer>;

define constant <u-int8-t> = <integer>;

define constant <u-int16-t> = <integer>;

define constant <u-int32-t> = <integer>;

define constant <register-t> = <integer>;

define constant <__blkcnt-t> = <integer>;

define constant <blkcnt-t> = <__blkcnt-t>;

define constant <__fsblkcnt-t> = <__u-long>;

define constant <fsblkcnt-t> = <__fsblkcnt-t>;

define constant <__fsfilcnt-t> = <__u-long>;

define constant <fsfilcnt-t> = <__fsfilcnt-t>;

define constant $_SYS-TYPES-H = 1;

define constant $__BIT-TYPES-DEFINED-- = 1;

define constant $_SYS-SYSMACROS-H = 1;

define constant $_ENDIAN-H = 1;

define constant $__LITTLE-ENDIAN = 1234;

define constant $__BIG-ENDIAN = 4321;

define constant $__PDP-ENDIAN = 3412;

define constant $__FLOAT-WORD-ORDER = 1234;

define constant $LITTLE-ENDIAN = 1234;

define constant $BIG-ENDIAN = 4321;

define constant $PDP-ENDIAN = 3412;

define constant $BYTE-ORDER = 1234;

define constant $__BYTE-ORDER = 1234;

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

define functional class <anonymous-24> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-24>));

define method pointer-value
    (ptr :: <anonymous-24>, #key index = 0)
 => (result :: <integer>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <integer>, ptr :: <anonymous-24>, #key index = 0)
 => (result :: <integer>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-24>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-23> (<anonymous-24>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-23>));

define method content-size (value == <anonymous-23>)  => (result :: <integer>);
  32;
end method content-size;

define functional class <__sigset-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__sigset-t>));

define sealed method get-__val
    (ptr :: <__sigset-t>) => (result :: <anonymous-23>);
  as(<anonymous-23>, ptr + 0);
end method get-__val;

define method pointer-value (value :: <__sigset-t>, #key index = 0) => (result :: <__sigset-t>);
  value + index * 128;
end method pointer-value;

define method content-size (value :: subclass(<__sigset-t>)) => (result :: <integer>);
  128;
end method content-size;

define constant <sigset-t> = <__sigset-t>;

define constant <__suseconds-t> = <integer>;

define constant <suseconds-t> = <__suseconds-t>;

define constant <__fd-mask> = <integer>;

define functional class <anonymous-27> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-27>));

define method pointer-value
    (ptr :: <anonymous-27>, #key index = 0)
 => (result :: <__fd-mask>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <__fd-mask>, ptr :: <anonymous-27>, #key index = 0)
 => (result :: <__fd-mask>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-27>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-26> (<anonymous-27>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-26>));

define method content-size (value == <anonymous-26>)  => (result :: <integer>);
  32;
end method content-size;

define functional class <fd-set> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<fd-set>));

define sealed method get-__fds-bits
    (ptr :: <fd-set>) => (result :: <anonymous-26>);
  as(<anonymous-26>, ptr + 0);
end method get-__fds-bits;

define method pointer-value (value :: <fd-set>, #key index = 0) => (result :: <fd-set>);
  value + index * 128;
end method pointer-value;

define method content-size (value :: subclass(<fd-set>)) => (result :: <integer>);
  128;
end method content-size;

define constant <fd-mask> = <__fd-mask>;

define constant <__time-t> = <integer>;

define functional class <timeval> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<timeval>));

define sealed method get-tv-sec
    (ptr :: <timeval>) => (result :: <__time-t>);
  signed-long-at(ptr, offset: 0);
end method get-tv-sec;

define sealed method get-tv-sec-setter
    (value :: <__time-t>, ptr :: <timeval>) => (result :: <__time-t>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-tv-sec-setter;

define sealed method get-tv-usec
    (ptr :: <timeval>) => (result :: <__suseconds-t>);
  signed-long-at(ptr, offset: 4);
end method get-tv-usec;

define sealed method get-tv-usec-setter
    (value :: <__suseconds-t>, ptr :: <timeval>) => (result :: <__suseconds-t>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-tv-usec-setter;

define method pointer-value (value :: <timeval>, #key index = 0) => (result :: <timeval>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<timeval>)) => (result :: <integer>);
  8;
end method content-size;

define method posix-select
    (arg1 :: <integer>, arg2 :: <fd-set>, arg3 :: <fd-set>, arg4 :: <fd-set>, arg5 :: <timeval>)
 => (result :: <integer>);
  let result-value
    = call-out("select", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method posix-select;

define constant $_SYS-SELECT-H = 1;

define constant $__NFDBITS = 32;

define constant $FD-SETSIZE = 1024;

define constant $NFDBITS = 32;

define constant $_STRUCT-TIMEVAL = 1;

define constant <__int8-t> = <integer>;

define constant <__uint8-t> = <integer>;

define constant <__int16-t> = <integer>;

define constant <__uint16-t> = <integer>;

define constant <__int32-t> = <integer>;

define constant <__qaddr-t> = <__quad-t>;

define constant <__rlim-t> = <__u-long>;

define constant <__rlim64-t> = <__u-quad-t>;

define constant <__useconds-t> = <integer>;

define constant <__swblk-t> = <integer>;

define constant <__clock-t> = <integer>;

define constant <__clockid-t> = <integer>;

define constant <__timer-t> = <integer>;

define constant <__ipc-pid-t> = <integer>;

define constant <__blksize-t> = <integer>;

define constant <__blkcnt64-t> = <__quad-t>;

define constant <__fsblkcnt64-t> = <__u-quad-t>;

define constant <__fsfilcnt64-t> = <__u-quad-t>;

define constant <__ino64-t> = <__u-quad-t>;

define constant <__off64-t> = <__loff-t>;

define constant <__t-scalar-t> = <integer>;

define constant <__t-uscalar-t> = <integer>;

define constant <__intptr-t> = <integer>;

define constant $_BITS-TYPES-H = 1;

define constant $__FD-SETSIZE = 1024;

define constant <__atomic-lock-t> = <integer>;

define functional class <_pthread-fastlock> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<_pthread-fastlock>));

define sealed method get-__status
    (ptr :: <_pthread-fastlock>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-__status;

define sealed method get-__status-setter
    (value :: <integer>, ptr :: <_pthread-fastlock>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-__status-setter;

define sealed method get-__spinlock
    (ptr :: <_pthread-fastlock>) => (result :: <__atomic-lock-t>);
  signed-long-at(ptr, offset: 4);
end method get-__spinlock;

define sealed method get-__spinlock-setter
    (value :: <__atomic-lock-t>, ptr :: <_pthread-fastlock>) => (result :: <__atomic-lock-t>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-__spinlock-setter;

define method pointer-value (value :: <_pthread-fastlock>, #key index = 0) => (result :: <_pthread-fastlock>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<_pthread-fastlock>)) => (result :: <integer>);
  8;
end method content-size;

define functional class <_pthread-descr-struct> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<_pthread-descr-struct>));

define method pointer-value (value :: <_pthread-descr-struct>, #key index = 0) => (result :: <_pthread-descr-struct>);
  value + index * 0;
end method pointer-value;

define method content-size (value :: subclass(<_pthread-descr-struct>)) => (result :: <integer>);
  0;
end method content-size;

define constant <_pthread-descr> = <_pthread-descr-struct>;

define functional class <__sched-param> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__sched-param>));

define sealed method get-__sched-priority
    (ptr :: <__sched-param>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-__sched-priority;

define sealed method get-__sched-priority-setter
    (value :: <integer>, ptr :: <__sched-param>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-__sched-priority-setter;

define method pointer-value (value :: <__sched-param>, #key index = 0) => (result :: <__sched-param>);
  value + index * 4;
end method pointer-value;

define method content-size (value :: subclass(<__sched-param>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <__pthread-attr-s> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<__pthread-attr-s>));

define sealed method get-__detachstate
    (ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-__detachstate;

define sealed method get-__detachstate-setter
    (value :: <integer>, ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-__detachstate-setter;

define sealed method get-__schedpolicy
    (ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method get-__schedpolicy;

define sealed method get-__schedpolicy-setter
    (value :: <integer>, ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-__schedpolicy-setter;

define sealed method get-__schedparam
    (ptr :: <__pthread-attr-s>) => (result :: <__sched-param>);
  as(<__sched-param>, ptr + 8);
end method get-__schedparam;

define sealed method get-__inheritsched
    (ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method get-__inheritsched;

define sealed method get-__inheritsched-setter
    (value :: <integer>, ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method get-__inheritsched-setter;

define sealed method get-__scope
    (ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 16);
end method get-__scope;

define sealed method get-__scope-setter
    (value :: <integer>, ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 16) := value;
  value;
end method get-__scope-setter;

define sealed method get-__guardsize
    (ptr :: <__pthread-attr-s>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 20);
end method get-__guardsize;

define sealed method get-__guardsize-setter
    (value :: <size-t>, ptr :: <__pthread-attr-s>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 20) := value;
  value;
end method get-__guardsize-setter;

define sealed method get-__stackaddr-set
    (ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24);
end method get-__stackaddr-set;

define sealed method get-__stackaddr-set-setter
    (value :: <integer>, ptr :: <__pthread-attr-s>) => (result :: <integer>);
  signed-long-at(ptr, offset: 24) := value;
  value;
end method get-__stackaddr-set-setter;

define sealed method get-__stackaddr
    (ptr :: <__pthread-attr-s>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 28, class: <machine-pointer>);
end method get-__stackaddr;

define sealed method get-__stackaddr-setter
    (value :: <machine-pointer>, ptr :: <__pthread-attr-s>) => (result :: <machine-pointer>);
  pointer-at(ptr, offset: 28, class: <machine-pointer>) := value;
  value;
end method get-__stackaddr-setter;

define sealed method get-__stacksize
    (ptr :: <__pthread-attr-s>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 32);
end method get-__stacksize;

define sealed method get-__stacksize-setter
    (value :: <size-t>, ptr :: <__pthread-attr-s>) => (result :: <size-t>);
  unsigned-long-at(ptr, offset: 32) := value;
  value;
end method get-__stacksize-setter;

define method pointer-value (value :: <__pthread-attr-s>, #key index = 0) => (result :: <__pthread-attr-s>);
  value + index * 36;
end method pointer-value;

define method content-size (value :: subclass(<__pthread-attr-s>)) => (result :: <integer>);
  36;
end method content-size;

define constant <pthread-attr-t> = <__pthread-attr-s>;

define functional class <pthread-cond-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<pthread-cond-t>));

define sealed method get-__c-lock
    (ptr :: <pthread-cond-t>) => (result :: <_pthread-fastlock>);
  as(<_pthread-fastlock>, ptr + 0);
end method get-__c-lock;

define sealed method get-__c-waiting
    (ptr :: <pthread-cond-t>) => (result :: <_pthread-descr>);
  pointer-at(ptr, offset: 8, class: <_pthread-descr>);
end method get-__c-waiting;

define sealed method get-__c-waiting-setter
    (value :: <_pthread-descr>, ptr :: <pthread-cond-t>) => (result :: <_pthread-descr>);
  pointer-at(ptr, offset: 8, class: <_pthread-descr>) := value;
  value;
end method get-__c-waiting-setter;

define method pointer-value (value :: <pthread-cond-t>, #key index = 0) => (result :: <pthread-cond-t>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<pthread-cond-t>)) => (result :: <integer>);
  12;
end method content-size;

define functional class <pthread-condattr-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<pthread-condattr-t>));

define sealed method get-__dummy
    (ptr :: <pthread-condattr-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-__dummy;

define sealed method get-__dummy-setter
    (value :: <integer>, ptr :: <pthread-condattr-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-__dummy-setter;

define method pointer-value (value :: <pthread-condattr-t>, #key index = 0) => (result :: <pthread-condattr-t>);
  value + index * 4;
end method pointer-value;

define method content-size (value :: subclass(<pthread-condattr-t>)) => (result :: <integer>);
  4;
end method content-size;

define constant <pthread-key-t> = <integer>;

define functional class <pthread-mutex-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<pthread-mutex-t>));

define sealed method get-__m-reserved
    (ptr :: <pthread-mutex-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-__m-reserved;

define sealed method get-__m-reserved-setter
    (value :: <integer>, ptr :: <pthread-mutex-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-__m-reserved-setter;

define sealed method get-__m-count
    (ptr :: <pthread-mutex-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method get-__m-count;

define sealed method get-__m-count-setter
    (value :: <integer>, ptr :: <pthread-mutex-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-__m-count-setter;

define sealed method get-__m-owner
    (ptr :: <pthread-mutex-t>) => (result :: <_pthread-descr>);
  pointer-at(ptr, offset: 8, class: <_pthread-descr>);
end method get-__m-owner;

define sealed method get-__m-owner-setter
    (value :: <_pthread-descr>, ptr :: <pthread-mutex-t>) => (result :: <_pthread-descr>);
  pointer-at(ptr, offset: 8, class: <_pthread-descr>) := value;
  value;
end method get-__m-owner-setter;

define sealed method get-__m-kind
    (ptr :: <pthread-mutex-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method get-__m-kind;

define sealed method get-__m-kind-setter
    (value :: <integer>, ptr :: <pthread-mutex-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method get-__m-kind-setter;

define sealed method get-__m-lock
    (ptr :: <pthread-mutex-t>) => (result :: <_pthread-fastlock>);
  as(<_pthread-fastlock>, ptr + 16);
end method get-__m-lock;

define method pointer-value (value :: <pthread-mutex-t>, #key index = 0) => (result :: <pthread-mutex-t>);
  value + index * 24;
end method pointer-value;

define method content-size (value :: subclass(<pthread-mutex-t>)) => (result :: <integer>);
  24;
end method content-size;

define functional class <pthread-mutexattr-t> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<pthread-mutexattr-t>));

define sealed method get-__mutexkind
    (ptr :: <pthread-mutexattr-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-__mutexkind;

define sealed method get-__mutexkind-setter
    (value :: <integer>, ptr :: <pthread-mutexattr-t>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-__mutexkind-setter;

define method pointer-value (value :: <pthread-mutexattr-t>, #key index = 0) => (result :: <pthread-mutexattr-t>);
  value + index * 4;
end method pointer-value;

define method content-size (value :: subclass(<pthread-mutexattr-t>)) => (result :: <integer>);
  4;
end method content-size;

define constant <pthread-once-t> = <integer>;

define constant <pthread-t> = <integer>;

define constant $_BITS-PTHREADTYPES-H = 1;

define constant $__defined-schedparam = 1;

define constant <__SIZE-TYPE--> = <integer>;

define constant <time-t> = <__time-t>;

define constant <clockid-t> = <__clockid-t>;

define constant <timer-t> = <__timer-t>;

define constant $__time-t-defined = 1;

define constant $__clockid-t-defined = 1;

define constant $__timer-t-defined = 1;

define functional class <timespec> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<timespec>));

define sealed method get-tv-sec
    (ptr :: <timespec>) => (result :: <__time-t>);
  signed-long-at(ptr, offset: 0);
end method get-tv-sec;

define sealed method get-tv-sec-setter
    (value :: <__time-t>, ptr :: <timespec>) => (result :: <__time-t>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-tv-sec-setter;

define sealed method get-tv-nsec
    (ptr :: <timespec>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method get-tv-nsec;

define sealed method get-tv-nsec-setter
    (value :: <integer>, ptr :: <timespec>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-tv-nsec-setter;

define method pointer-value (value :: <timespec>, #key index = 0) => (result :: <timespec>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<timespec>)) => (result :: <integer>);
  8;
end method content-size;

define constant $__timespec-defined = 1;

define constant <__sig-atomic-t> = <integer>;

define constant $_SIGSET-H-types = 1;

define constant $_SIGSET-NWORDS = 32;

define constant $CHAR-BIT = 8;

define constant $MB-LEN-MAX = 1;

define constant $SCHAR-MIN = -128;

define constant $SCHAR-MAX = 127;

define constant $UCHAR-MAX = 255;

define constant $CHAR-MIN = -128;

define constant $CHAR-MAX = 127;

define constant $SHRT-MIN = -32768;

define constant $SHRT-MAX = 32767;

define constant $USHRT-MAX = 65535;

define constant $__INT-MAX-- = 2147483647;

define constant $INT-MIN = -2147483648;

define constant $INT-MAX = 2147483647;

define constant $UINT-MAX = -1;

define constant $__LONG-MAX-- = 2147483647;

define constant $LONG-MIN = -2147483648;

define constant $LONG-MAX = 2147483647;

define constant $ULONG-MAX = -1;

define method readv
    (arg1 :: <integer>, arg2 :: <iovec>, arg3 :: <integer>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("readv", int:, int: arg1, ptr: (arg2).raw-value, int: arg3);
  values(result-value);
end method readv;

define method writev
    (arg1 :: <integer>, arg2 :: <iovec>, arg3 :: <integer>)
 => (result :: <ssize-t>);
  let result-value
    = call-out("writev", int:, int: arg1, ptr: (arg2).raw-value, int: arg3);
  values(result-value);
end method writev;

define constant $_SYS-UIO-H = 1;

define constant $UIO-MAXIOV = 1024;

define constant <anonymous-14> = limited(<integer>, min: 0, max: 256);
define constant $IPPROTO-IP :: <anonymous-14> = 0;
define constant $IPPROTO-HOPOPTS :: <anonymous-14> = 0;
define constant $IPPROTO-ICMP :: <anonymous-14> = 1;
define constant $IPPROTO-IGMP :: <anonymous-14> = 2;
define constant $IPPROTO-IPIP :: <anonymous-14> = 4;
define constant $IPPROTO-TCP :: <anonymous-14> = 6;
define constant $IPPROTO-EGP :: <anonymous-14> = 8;
define constant $IPPROTO-PUP :: <anonymous-14> = 12;
define constant $IPPROTO-UDP :: <anonymous-14> = 17;
define constant $IPPROTO-IDP :: <anonymous-14> = 22;
define constant $IPPROTO-TP :: <anonymous-14> = 29;
define constant $IPPROTO-IPV6 :: <anonymous-14> = 41;
define constant $IPPROTO-ROUTING :: <anonymous-14> = 43;
define constant $IPPROTO-FRAGMENT :: <anonymous-14> = 44;
define constant $IPPROTO-RSVP :: <anonymous-14> = 46;
define constant $IPPROTO-GRE :: <anonymous-14> = 47;
define constant $IPPROTO-ESP :: <anonymous-14> = 50;
define constant $IPPROTO-AH :: <anonymous-14> = 51;
define constant $IPPROTO-ICMPV6 :: <anonymous-14> = 58;
define constant $IPPROTO-NONE :: <anonymous-14> = 59;
define constant $IPPROTO-DSTOPTS :: <anonymous-14> = 60;
define constant $IPPROTO-MTP :: <anonymous-14> = 92;
define constant $IPPROTO-ENCAP :: <anonymous-14> = 98;
define constant $IPPROTO-PIM :: <anonymous-14> = 103;
define constant $IPPROTO-COMP :: <anonymous-14> = 108;
define constant $IPPROTO-RAW :: <anonymous-14> = 255;
define constant $IPPROTO-MAX :: <anonymous-14> = 256;

define constant <uint16-t> = <integer>;

define constant <in-port-t> = <uint16-t>;

define constant <anonymous-15> = limited(<integer>, min: 7, max: 5000);
define constant $IPPORT-ECHO :: <anonymous-15> = 7;
define constant $IPPORT-DISCARD :: <anonymous-15> = 9;
define constant $IPPORT-SYSTAT :: <anonymous-15> = 11;
define constant $IPPORT-DAYTIME :: <anonymous-15> = 13;
define constant $IPPORT-NETSTAT :: <anonymous-15> = 15;
define constant $IPPORT-FTP :: <anonymous-15> = 21;
define constant $IPPORT-TELNET :: <anonymous-15> = 23;
define constant $IPPORT-SMTP :: <anonymous-15> = 25;
define constant $IPPORT-TIMESERVER :: <anonymous-15> = 37;
define constant $IPPORT-NAMESERVER :: <anonymous-15> = 42;
define constant $IPPORT-WHOIS :: <anonymous-15> = 43;
define constant $IPPORT-MTP :: <anonymous-15> = 57;
define constant $IPPORT-TFTP :: <anonymous-15> = 69;
define constant $IPPORT-RJE :: <anonymous-15> = 77;
define constant $IPPORT-FINGER :: <anonymous-15> = 79;
define constant $IPPORT-TTYLINK :: <anonymous-15> = 87;
define constant $IPPORT-SUPDUP :: <anonymous-15> = 95;
define constant $IPPORT-EXECSERVER :: <anonymous-15> = 512;
define constant $IPPORT-LOGINSERVER :: <anonymous-15> = 513;
define constant $IPPORT-CMDSERVER :: <anonymous-15> = 514;
define constant $IPPORT-EFSSERVER :: <anonymous-15> = 520;
define constant $IPPORT-BIFFUDP :: <anonymous-15> = 512;
define constant $IPPORT-WHOSERVER :: <anonymous-15> = 513;
define constant $IPPORT-ROUTESERVER :: <anonymous-15> = 520;
define constant $IPPORT-RESERVED :: <anonymous-15> = 1024;
define constant $IPPORT-USERRESERVED :: <anonymous-15> = 5000;

define constant <uint32-t> = <integer>;

define constant <in-addr-t> = <uint32-t>;

define functional class <in-addr> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<in-addr>));

define sealed method get-s-addr
    (ptr :: <in-addr>) => (result :: <in-addr-t>);
  unsigned-long-at(ptr, offset: 0);
end method get-s-addr;

define sealed method get-s-addr-setter
    (value :: <in-addr-t>, ptr :: <in-addr>) => (result :: <in-addr-t>);
  unsigned-long-at(ptr, offset: 0) := value;
  value;
end method get-s-addr-setter;

define method pointer-value (value :: <in-addr>, #key index = 0) => (result :: <in-addr>);
  value + index * 4;
end method pointer-value;

define method content-size (value :: subclass(<in-addr>)) => (result :: <integer>);
  4;
end method content-size;

define constant <uint8-t> = <integer>;

define functional class <anonymous-17> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-17>));

define method pointer-value
    (ptr :: <anonymous-17>, #key index = 0)
 => (result :: <uint8-t>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define method pointer-value-setter
    (value :: <uint8-t>, ptr :: <anonymous-17>, #key index = 0)
 => (result :: <uint8-t>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-17>)) => (result :: <integer>);
  1;
end method content-size;

define functional class <anonymous-16> (<anonymous-17>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-16>));

define method content-size (value == <anonymous-16>)  => (result :: <integer>);
  16;
end method content-size;

define functional class <anonymous-19> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-19>));

define method pointer-value
    (ptr :: <anonymous-19>, #key index = 0)
 => (result :: <uint16-t>);
  unsigned-short-at(ptr, offset: index * 2);
end method pointer-value;

define method pointer-value-setter
    (value :: <uint16-t>, ptr :: <anonymous-19>, #key index = 0)
 => (result :: <uint16-t>);
  unsigned-short-at(ptr, offset: index * 2) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-19>)) => (result :: <integer>);
  2;
end method content-size;

define functional class <anonymous-18> (<anonymous-19>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-18>));

define method content-size (value == <anonymous-18>)  => (result :: <integer>);
  8;
end method content-size;

define functional class <anonymous-21> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-21>));

define method pointer-value
    (ptr :: <anonymous-21>, #key index = 0)
 => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: index * 4);
end method pointer-value;

define method pointer-value-setter
    (value :: <uint32-t>, ptr :: <anonymous-21>, #key index = 0)
 => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-21>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-20> (<anonymous-21>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-20>));

define method content-size (value == <anonymous-20>)  => (result :: <integer>);
  4;
end method content-size;

define functional class <anonymous-22> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-22>));

define sealed method get-u6-addr8
    (ptr :: <anonymous-22>) => (result :: <anonymous-16>);
  as(<anonymous-16>, ptr + 0);
end method get-u6-addr8;

define sealed method get-u6-addr16
    (ptr :: <anonymous-22>) => (result :: <anonymous-18>);
  as(<anonymous-18>, ptr + 0);
end method get-u6-addr16;

define sealed method get-u6-addr32
    (ptr :: <anonymous-22>) => (result :: <anonymous-20>);
  as(<anonymous-20>, ptr + 0);
end method get-u6-addr32;

define method pointer-value (value :: <anonymous-22>, #key index = 0) => (result :: <anonymous-22>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<anonymous-22>))  => (result :: <integer>);
  16;
end method content-size;

define functional class <in6-addr> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<in6-addr>));

define sealed method get-in6-u
    (ptr :: <in6-addr>) => (result :: <anonymous-22>);
  as(<anonymous-22>, ptr + 0);
end method get-in6-u;

define method pointer-value (value :: <in6-addr>, #key index = 0) => (result :: <in6-addr>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<in6-addr>)) => (result :: <integer>);
  16;
end method content-size;

define sealed method in6addr-any () => (result :: <in6-addr>);
  as(<in6-addr>, c-variable-ref(ptr: "&in6addr_any"));
end method in6addr-any;

define sealed method in6addr-loopback () => (result :: <in6-addr>);
  as(<in6-addr>, c-variable-ref(ptr: "&in6addr_loopback"));
end method in6addr-loopback;

define functional class <anonymous-35> (<anonymous-36>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-35>));

define method content-size (value == <anonymous-35>)  => (result :: <integer>);
  8;
end method content-size;

define functional class <sockaddr-in> (<sockaddr>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<sockaddr-in>));

define sealed method get-sin-family
    (ptr :: <sockaddr-in>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0);
end method get-sin-family;

define sealed method get-sin-family-setter
    (value :: <sa-family-t>, ptr :: <sockaddr-in>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method get-sin-family-setter;

define sealed method get-sin-port
    (ptr :: <sockaddr-in>) => (result :: <in-port-t>);
  unsigned-short-at(ptr, offset: 2);
end method get-sin-port;

define sealed method get-sin-port-setter
    (value :: <in-port-t>, ptr :: <sockaddr-in>) => (result :: <in-port-t>);
  unsigned-short-at(ptr, offset: 2) := value;
  value;
end method get-sin-port-setter;

define sealed method get-sin-addr
    (ptr :: <sockaddr-in>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 4);
end method get-sin-addr;

define sealed method get-sin-zero
    (ptr :: <sockaddr-in>) => (result :: <anonymous-35>);
  as(<anonymous-35>, ptr + 8);
end method get-sin-zero;

define method pointer-value (value :: <sockaddr-in>, #key index = 0) => (result :: <sockaddr-in>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<sockaddr-in>)) => (result :: <integer>);
  16;
end method content-size;

define functional class <sockaddr-in6> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<sockaddr-in6>));

define sealed method get-sin6-family
    (ptr :: <sockaddr-in6>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0);
end method get-sin6-family;

define sealed method get-sin6-family-setter
    (value :: <sa-family-t>, ptr :: <sockaddr-in6>) => (result :: <sa-family-t>);
  unsigned-short-at(ptr, offset: 0) := value;
  value;
end method get-sin6-family-setter;

define sealed method get-sin6-port
    (ptr :: <sockaddr-in6>) => (result :: <in-port-t>);
  unsigned-short-at(ptr, offset: 2);
end method get-sin6-port;

define sealed method get-sin6-port-setter
    (value :: <in-port-t>, ptr :: <sockaddr-in6>) => (result :: <in-port-t>);
  unsigned-short-at(ptr, offset: 2) := value;
  value;
end method get-sin6-port-setter;

define sealed method get-sin6-flowinfo
    (ptr :: <sockaddr-in6>) => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: 4);
end method get-sin6-flowinfo;

define sealed method get-sin6-flowinfo-setter
    (value :: <uint32-t>, ptr :: <sockaddr-in6>) => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: 4) := value;
  value;
end method get-sin6-flowinfo-setter;

define sealed method get-sin6-addr
    (ptr :: <sockaddr-in6>) => (result :: <in6-addr>);
  as(<in6-addr>, ptr + 8);
end method get-sin6-addr;

define sealed method get-sin6-scope-id
    (ptr :: <sockaddr-in6>) => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: 24);
end method get-sin6-scope-id;

define sealed method get-sin6-scope-id-setter
    (value :: <uint32-t>, ptr :: <sockaddr-in6>) => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: 24) := value;
  value;
end method get-sin6-scope-id-setter;

define method pointer-value (value :: <sockaddr-in6>, #key index = 0) => (result :: <sockaddr-in6>);
  value + index * 28;
end method pointer-value;

define method content-size (value :: subclass(<sockaddr-in6>)) => (result :: <integer>);
  28;
end method content-size;

define functional class <ipv6-mreq> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<ipv6-mreq>));

define sealed method get-ipv6mr-multiaddr
    (ptr :: <ipv6-mreq>) => (result :: <in6-addr>);
  as(<in6-addr>, ptr + 0);
end method get-ipv6mr-multiaddr;

define sealed method get-ipv6mr-interface
    (ptr :: <ipv6-mreq>) => (result :: <integer>);
  unsigned-long-at(ptr, offset: 16);
end method get-ipv6mr-interface;

define sealed method get-ipv6mr-interface-setter
    (value :: <integer>, ptr :: <ipv6-mreq>) => (result :: <integer>);
  unsigned-long-at(ptr, offset: 16) := value;
  value;
end method get-ipv6mr-interface-setter;

define method pointer-value (value :: <ipv6-mreq>, #key index = 0) => (result :: <ipv6-mreq>);
  value + index * 20;
end method pointer-value;

define method content-size (value :: subclass(<ipv6-mreq>)) => (result :: <integer>);
  20;
end method content-size;

define method ntohl
    (arg1 :: <uint32-t>)
 => (result :: <uint32-t>);
  let result-value
    = call-out("ntohl", unsigned-int:, unsigned-int: arg1);
  values(result-value);
end method ntohl;

define method ntohs
    (arg1 :: <uint16-t>)
 => (result :: <uint16-t>);
  let result-value
    = call-out("ntohs", unsigned-short:, unsigned-short: arg1);
  values(result-value);
end method ntohs;

define method htonl
    (arg1 :: <uint32-t>)
 => (result :: <uint32-t>);
  let result-value
    = call-out("htonl", unsigned-int:, unsigned-int: arg1);
  values(result-value);
end method htonl;

define method htons
    (arg1 :: <uint16-t>)
 => (result :: <uint16-t>);
  let result-value
    = call-out("htons", unsigned-short:, unsigned-short: arg1);
  values(result-value);
end method htons;

define method bindresvport
    (arg1 :: <integer>, arg2 :: <sockaddr-in>)
 => (result :: <integer>);
  let result-value
    = call-out("bindresvport", int:, int: arg1, ptr: (arg2).raw-value);
  values(result-value);
end method bindresvport;

define functional class <in6-pktinfo> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<in6-pktinfo>));

define sealed method get-ipi6-addr
    (ptr :: <in6-pktinfo>) => (result :: <in6-addr>);
  as(<in6-addr>, ptr + 0);
end method get-ipi6-addr;

define sealed method get-ipi6-ifindex
    (ptr :: <in6-pktinfo>) => (result :: <integer>);
  unsigned-long-at(ptr, offset: 16);
end method get-ipi6-ifindex;

define sealed method get-ipi6-ifindex-setter
    (value :: <integer>, ptr :: <in6-pktinfo>) => (result :: <integer>);
  unsigned-long-at(ptr, offset: 16) := value;
  value;
end method get-ipi6-ifindex-setter;

define method pointer-value (value :: <in6-pktinfo>, #key index = 0) => (result :: <in6-pktinfo>);
  value + index * 20;
end method pointer-value;

define method content-size (value :: subclass(<in6-pktinfo>)) => (result :: <integer>);
  20;
end method content-size;

define constant $_NETINET-IN-H = 1;

define constant $IN-CLASSA-NET = #e4278190080;

define constant $IN-CLASSA-NSHIFT = 24;

define constant $IN-CLASSA-HOST = #e16777215;

define constant $IN-CLASSA-MAX = 128;

define constant $IN-CLASSB-NET = #e4294901760;

define constant $IN-CLASSB-NSHIFT = 16;

define constant $IN-CLASSB-HOST = #e65535;

define constant $IN-CLASSB-MAX = 65536;

define constant $IN-CLASSC-NET = #e4294967040;

define constant $IN-CLASSC-NSHIFT = 8;

define constant $IN-CLASSC-HOST = #e255;

define constant $INADDR-ANY = 0;

define constant $INADDR-BROADCAST = #e4294967295;

define constant $INADDR-NONE = #e4294967295;

define constant $IN-LOOPBACKNET = 127;

define constant $INADDR-LOOPBACK = 2130706433;

define constant $INADDR-UNSPEC-GROUP = #e3758096384;

define constant $INADDR-ALLHOSTS-GROUP = #e3758096385;

define constant $INADDR-ALLRTRS-GROUP = #e3758096386;

define constant $INADDR-MAX-LOCAL-GROUP = #e3758096639;

define constant $INET-ADDRSTRLEN = 16;

define constant $INET6-ADDRSTRLEN = 46;

define constant $_BITS-BYTESWAP-H = 1;

define functional class <anonymous-37> (<c-char-vector>, <c-vector>) end class;

define sealed domain make (singleton(<anonymous-37>));

define method content-size (value == <anonymous-37>)  => (result :: <integer>);
  40;
end method content-size;

define functional class <ip-opts> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<ip-opts>));

define sealed method get-ip-dst
    (ptr :: <ip-opts>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 0);
end method get-ip-dst;

define sealed method get-ip-opts
    (ptr :: <ip-opts>) => (result :: <anonymous-37>);
  as(<anonymous-37>, ptr + 4);
end method get-ip-opts;

define method pointer-value (value :: <ip-opts>, #key index = 0) => (result :: <ip-opts>);
  value + index * 44;
end method pointer-value;

define method content-size (value :: subclass(<ip-opts>)) => (result :: <integer>);
  44;
end method content-size;

define functional class <ip-mreq> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<ip-mreq>));

define sealed method get-imr-multiaddr
    (ptr :: <ip-mreq>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 0);
end method get-imr-multiaddr;

define sealed method get-imr-interface
    (ptr :: <ip-mreq>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 4);
end method get-imr-interface;

define method pointer-value (value :: <ip-mreq>, #key index = 0) => (result :: <ip-mreq>);
  value + index * 8;
end method pointer-value;

define method content-size (value :: subclass(<ip-mreq>)) => (result :: <integer>);
  8;
end method content-size;

define functional class <ip-mreqn> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<ip-mreqn>));

define sealed method get-imr-multiaddr
    (ptr :: <ip-mreqn>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 0);
end method get-imr-multiaddr;

define sealed method get-imr-address
    (ptr :: <ip-mreqn>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 4);
end method get-imr-address;

define sealed method get-imr-ifindex
    (ptr :: <ip-mreqn>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-imr-ifindex;

define sealed method get-imr-ifindex-setter
    (value :: <integer>, ptr :: <ip-mreqn>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-imr-ifindex-setter;

define method pointer-value (value :: <ip-mreqn>, #key index = 0) => (result :: <ip-mreqn>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<ip-mreqn>)) => (result :: <integer>);
  12;
end method content-size;

define functional class <in-pktinfo> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<in-pktinfo>));

define sealed method get-ipi-ifindex
    (ptr :: <in-pktinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-ipi-ifindex;

define sealed method get-ipi-ifindex-setter
    (value :: <integer>, ptr :: <in-pktinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-ipi-ifindex-setter;

define sealed method get-ipi-spec-dst
    (ptr :: <in-pktinfo>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 4);
end method get-ipi-spec-dst;

define sealed method get-ipi-addr
    (ptr :: <in-pktinfo>) => (result :: <in-addr>);
  as(<in-addr>, ptr + 8);
end method get-ipi-addr;

define method pointer-value (value :: <in-pktinfo>, #key index = 0) => (result :: <in-pktinfo>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<in-pktinfo>)) => (result :: <integer>);
  12;
end method content-size;

define constant $IP-TOS = 1;

define constant $IP-TTL = 2;

define constant $IP-HDRINCL = 3;

define constant $IP-OPTIONS = 4;

define constant $IP-ROUTER-ALERT = 5;

define constant $IP-RECVOPTS = 6;

define constant $IP-RETOPTS = 7;

define constant $IP-PKTINFO = 8;

define constant $IP-PKTOPTIONS = 9;

define constant $IP-PMTUDISC = 10;

define constant $IP-MTU-DISCOVER = 10;

define constant $IP-RECVERR = 11;

define constant $IP-RECVTTL = 12;

define constant $IP-RECVTOS = 13;

define constant $IP-MULTICAST-IF = 32;

define constant $IP-MULTICAST-TTL = 33;

define constant $IP-MULTICAST-LOOP = 34;

define constant $IP-ADD-MEMBERSHIP = 35;

define constant $IP-DROP-MEMBERSHIP = 36;

define constant $IP-RECVRETOPTS = 7;

define constant $IP-PMTUDISC-DONT = 0;

define constant $IP-PMTUDISC-WANT = 1;

define constant $IP-PMTUDISC-DO = 2;

define constant $SOL-IP = 0;

define constant $IP-DEFAULT-MULTICAST-TTL = 1;

define constant $IP-DEFAULT-MULTICAST-LOOP = 1;

define constant $IP-MAX-MEMBERSHIPS = 20;

define constant $IPV6-ADDRFORM = 1;

define constant $IPV6-PKTINFO = 2;

define constant $IPV6-HOPOPTS = 3;

define constant $IPV6-DSTOPTS = 4;

define constant $IPV6-RTHDR = 5;

define constant $IPV6-PKTOPTIONS = 6;

define constant $IPV6-CHECKSUM = 7;

define constant $IPV6-HOPLIMIT = 8;

define constant $IPV6-NEXTHOP = 9;

define constant $IPV6-AUTHHDR = 10;

define constant $IPV6-UNICAST-HOPS = 16;

define constant $IPV6-MULTICAST-IF = 17;

define constant $IPV6-MULTICAST-HOPS = 18;

define constant $IPV6-MULTICAST-LOOP = 19;

define constant $IPV6-JOIN-GROUP = 20;

define constant $IPV6-LEAVE-GROUP = 21;

define constant $IPV6-ROUTER-ALERT = 22;

define constant $IPV6-MTU-DISCOVER = 23;

define constant $IPV6-MTU = 24;

define constant $IPV6-RECVERR = 25;

define constant $IPV6-RXHOPOPTS = 3;

define constant $IPV6-RXDSTOPTS = 4;

define constant $IPV6-ADD-MEMBERSHIP = 20;

define constant $IPV6-DROP-MEMBERSHIP = 21;

define constant $IPV6-PMTUDISC-DONT = 0;

define constant $IPV6-PMTUDISC-WANT = 1;

define constant $IPV6-PMTUDISC-DO = 2;

define constant $SOL-IPV6 = 41;

define constant $SOL-ICMPV6 = 58;

define constant $IPV6-RTHDR-LOOSE = 0;

define constant $IPV6-RTHDR-STRICT = 1;

define constant $IPV6-RTHDR-TYPE-0 = 0;

define constant <int8-t> = <integer>;

define constant <int16-t> = <integer>;

define constant <int32-t> = <integer>;

define constant <int64-t> = <integer>;

define constant <uint64-t> = <integer>;

define constant <int-least8-t> = <integer>;

define constant <int-least16-t> = <integer>;

define constant <int-least32-t> = <integer>;

define constant <int-least64-t> = <integer>;

define constant <uint-least8-t> = <integer>;

define constant <uint-least16-t> = <integer>;

define constant <uint-least32-t> = <integer>;

define constant <uint-least64-t> = <integer>;

define constant <int-fast8-t> = <integer>;

define constant <int-fast16-t> = <integer>;

define constant <int-fast32-t> = <integer>;

define constant <int-fast64-t> = <integer>;

define constant <uint-fast8-t> = <integer>;

define constant <uint-fast16-t> = <integer>;

define constant <uint-fast32-t> = <integer>;

define constant <uint-fast64-t> = <integer>;

define constant <intptr-t> = <integer>;

define constant <uintptr-t> = <integer>;

define constant <intmax-t> = <integer>;

define constant <uintmax-t> = <integer>;

define constant $_STDINT-H = 1;

define constant $INT8-MIN = -128;

define constant $INT16-MIN = -32768;

define constant $INT32-MIN = -2147483648;

define constant $INT64-MIN = #e-9223372036854775808;

define constant $INT8-MAX = 127;

define constant $INT16-MAX = 32767;

define constant $INT32-MAX = 2147483647;

define constant $INT64-MAX = #e9223372036854775807;

define constant $UINT8-MAX = 255;

define constant $UINT16-MAX = 65535;

define constant $UINT32-MAX = #e4294967295;

define constant $UINT64-MAX = #e18446744073709551615;

define constant $INT-LEAST8-MIN = -128;

define constant $INT-LEAST16-MIN = -32768;

define constant $INT-LEAST32-MIN = -2147483648;

define constant $INT-LEAST64-MIN = #e-9223372036854775808;

define constant $INT-LEAST8-MAX = 127;

define constant $INT-LEAST16-MAX = 32767;

define constant $INT-LEAST32-MAX = 2147483647;

define constant $INT-LEAST64-MAX = #e9223372036854775807;

define constant $UINT-LEAST8-MAX = 255;

define constant $UINT-LEAST16-MAX = 65535;

define constant $UINT-LEAST32-MAX = #e4294967295;

define constant $UINT-LEAST64-MAX = #e18446744073709551615;

define constant $INT-FAST8-MIN = -128;

define constant $INT-FAST16-MIN = -2147483648;

define constant $INT-FAST32-MIN = -2147483648;

define constant $INT-FAST64-MIN = #e-9223372036854775808;

define constant $INT-FAST8-MAX = 127;

define constant $INT-FAST16-MAX = 2147483647;

define constant $INT-FAST32-MAX = 2147483647;

define constant $INT-FAST64-MAX = #e9223372036854775807;

define constant $UINT-FAST8-MAX = 255;

define constant $UINT-FAST16-MAX = #e4294967295;

define constant $UINT-FAST32-MAX = #e4294967295;

define constant $UINT-FAST64-MAX = #e18446744073709551615;

define constant $INTPTR-MIN = -2147483648;

define constant $INTPTR-MAX = 2147483647;

define constant $UINTPTR-MAX = #e4294967295;

define constant $INTMAX-MIN = #e-9223372036854775808;

define constant $INTMAX-MAX = #e9223372036854775807;

define constant $UINTMAX-MAX = #e18446744073709551615;

define constant $PTRDIFF-MIN = -2147483648;

define constant $PTRDIFF-MAX = 2147483647;

define constant $SIG-ATOMIC-MIN = -2147483648;

define constant $SIG-ATOMIC-MAX = 2147483647;

define constant $SIZE-MAX = #e4294967295;

define constant $WCHAR-MIN = -2147483648;

define constant $WCHAR-MAX = 2147483647;

define constant $WINT-MIN = 0;

define constant $WINT-MAX = #e4294967295;

define constant $__WORDSIZE = 32;

define constant $_BITS-WCHAR-H = 1;

define constant $__WCHAR-MIN = -2147483648;

define constant $__WCHAR-MAX = 2147483647;

define sealed method h-errno () => (result :: <integer>);
  as(<integer>, c-variable-ref(int: "&h_errno"));
end method h-errno;

define sealed method h-errno-setter (value :: <integer>) => (result :: <integer>);
  c-variable-ref(int: "&h_errno") := value;
  value;
end method h-errno-setter;

define method __h-errno-location
    ()
 => (result :: <anonymous-7>);
  let result-value
    = call-out("__h_errno_location", ptr:);
  let result-value = make(<anonymous-7>, pointer: result-value);
  values(result-value);
end method __h-errno-location;

define method herror
    (arg1 :: <c-char-vector>)
 => ();
  call-out("herror", void:, ptr: (arg1).raw-value);
  values();
end method herror;

define method hstrerror
    (arg1 :: <integer>)
 => (result :: <c-char-vector>);
  let result-value
    = call-out("hstrerror", ptr:, int: arg1);
  let result-value = make(<c-char-vector>, pointer: result-value);
  values(result-value);
end method hstrerror;

define functional class <c-char-pointer-vector> (<c-vector>, <statically-typed-pointer>) end;

define sealed domain make (singleton(<c-char-pointer-vector>));

define method pointer-value
    (ptr :: <c-char-pointer-vector>, #key index = 0)
 => (result :: <c-char-vector>);
  pointer-at(ptr, offset: index * 4, class: <c-char-vector>);
end method pointer-value;

define method pointer-value-setter
    (value :: <c-char-vector>, ptr :: <c-char-pointer-vector>, #key index = 0)
 => (result :: <c-char-vector>);
  pointer-at(ptr, offset: index * 4, class: <c-char-vector>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<c-char-pointer-vector>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <hostent> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<hostent>));

define sealed method get-h-name
    (ptr :: <hostent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>);
end method get-h-name;

define sealed method get-h-name-setter
    (value :: <c-char-vector>, ptr :: <hostent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>) := value;
  value;
end method get-h-name-setter;

define sealed method get-h-aliases
    (ptr :: <hostent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>);
end method get-h-aliases;

define sealed method get-h-aliases-setter
    (value :: <c-char-pointer-vector>, ptr :: <hostent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>) := value;
  value;
end method get-h-aliases-setter;

define sealed method get-h-addrtype
    (ptr :: <hostent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-h-addrtype;

define sealed method get-h-addrtype-setter
    (value :: <integer>, ptr :: <hostent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-h-addrtype-setter;

define sealed method get-h-length
    (ptr :: <hostent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method get-h-length;

define sealed method get-h-length-setter
    (value :: <integer>, ptr :: <hostent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method get-h-length-setter;

define sealed method get-h-addr-list
    (ptr :: <hostent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 16, class: <c-char-pointer-vector>);
end method get-h-addr-list;

define sealed method get-h-addr-list-setter
    (value :: <c-char-pointer-vector>, ptr :: <hostent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 16, class: <c-char-pointer-vector>) := value;
  value;
end method get-h-addr-list-setter;

define method pointer-value (value :: <hostent>, #key index = 0) => (result :: <hostent>);
  value + index * 20;
end method pointer-value;

define method content-size (value :: subclass(<hostent>)) => (result :: <integer>);
  20;
end method content-size;

define method sethostent
    (arg1 :: <integer>)
 => ();
  call-out("sethostent", void:, int: arg1);
  values();
end method sethostent;

define method endhostent
    ()
 => ();
  call-out("endhostent", void:);
  values();
end method endhostent;

define method gethostent
    ()
 => (result :: <hostent>);
  let result-value
    = call-out("gethostent", ptr:);
  let result-value = make(<hostent>, pointer: result-value);
  values(result-value);
end method gethostent;

define method gethostbyaddr
    (arg1 :: <machine-pointer>, arg2 :: <__socklen-t>, arg3 :: <integer>)
 => (result :: <hostent>);
  let result-value
    = call-out("gethostbyaddr", ptr:, ptr: (arg1).raw-value, unsigned-int: arg2, int: arg3);
  let result-value = make(<hostent>, pointer: result-value);
  values(result-value);
end method gethostbyaddr;

define method gethostbyname
    (arg1 :: <byte-string>)
 => (result :: <hostent>);
  let result-value
    = call-out("gethostbyname", ptr:, ptr: (export-value(<c-string>, arg1)).raw-value);
  let result-value = make(<hostent>, pointer: result-value);
  values(result-value);
end method gethostbyname;

define method gethostbyname2
    (arg1 :: <c-char-vector>, arg2 :: <integer>)
 => (result :: <hostent>);
  let result-value
    = call-out("gethostbyname2", ptr:, ptr: (arg1).raw-value, int: arg2);
  let result-value = make(<hostent>, pointer: result-value);
  values(result-value);
end method gethostbyname2;

define functional class <anonymous-63> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-63>));

define method pointer-value
    (ptr :: <anonymous-63>, #key index = 0)
 => (result :: <hostent>);
  pointer-at(ptr, offset: index * 4, class: <hostent>);
end method pointer-value;

define method pointer-value-setter
    (value :: <hostent>, ptr :: <anonymous-63>, #key index = 0)
 => (result :: <hostent>);
  pointer-at(ptr, offset: index * 4, class: <hostent>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-63>)) => (result :: <integer>);
  4;
end method content-size;

define method gethostent-r
    (arg1 :: <hostent>, arg2 :: <c-char-vector>, arg3 :: <size-t>, arg4 :: <anonymous-63>, arg5 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("gethostent_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method gethostent-r;

define method gethostbyaddr-r
    (arg1 :: <machine-pointer>, arg2 :: <__socklen-t>, arg3 :: <integer>, arg4 :: <hostent>, arg5 :: <c-char-vector>, arg6 :: <size-t>, arg7 :: <anonymous-63>, arg8 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("gethostbyaddr_r", int:, ptr: (arg1).raw-value, unsigned-int: arg2, int: arg3, ptr: (arg4).raw-value, ptr: (arg5).raw-value, long: arg6, ptr: (arg7).raw-value, ptr: (arg8).raw-value);
  values(result-value);
end method gethostbyaddr-r;

define method gethostbyname-r
    (arg1 :: <c-char-vector>, arg2 :: <hostent>, arg3 :: <c-char-vector>, arg4 :: <size-t>, arg5 :: <anonymous-63>, arg6 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("gethostbyname_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, ptr: (arg5).raw-value, ptr: (arg6).raw-value);
  values(result-value);
end method gethostbyname-r;

define method gethostbyname2-r
    (arg1 :: <c-char-vector>, arg2 :: <integer>, arg3 :: <hostent>, arg4 :: <c-char-vector>, arg5 :: <size-t>, arg6 :: <anonymous-63>, arg7 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("gethostbyname2_r", int:, ptr: (arg1).raw-value, int: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, long: arg5, ptr: (arg6).raw-value, ptr: (arg7).raw-value);
  values(result-value);
end method gethostbyname2-r;

define method setnetent
    (arg1 :: <integer>)
 => ();
  call-out("setnetent", void:, int: arg1);
  values();
end method setnetent;

define method endnetent
    ()
 => ();
  call-out("endnetent", void:);
  values();
end method endnetent;

define functional class <netent> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<netent>));

define sealed method get-n-name
    (ptr :: <netent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>);
end method get-n-name;

define sealed method get-n-name-setter
    (value :: <c-char-vector>, ptr :: <netent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>) := value;
  value;
end method get-n-name-setter;

define sealed method get-n-aliases
    (ptr :: <netent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>);
end method get-n-aliases;

define sealed method get-n-aliases-setter
    (value :: <c-char-pointer-vector>, ptr :: <netent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>) := value;
  value;
end method get-n-aliases-setter;

define sealed method get-n-addrtype
    (ptr :: <netent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-n-addrtype;

define sealed method get-n-addrtype-setter
    (value :: <integer>, ptr :: <netent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-n-addrtype-setter;

define sealed method get-n-net
    (ptr :: <netent>) => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: 12);
end method get-n-net;

define sealed method get-n-net-setter
    (value :: <uint32-t>, ptr :: <netent>) => (result :: <uint32-t>);
  unsigned-long-at(ptr, offset: 12) := value;
  value;
end method get-n-net-setter;

define method pointer-value (value :: <netent>, #key index = 0) => (result :: <netent>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<netent>)) => (result :: <integer>);
  16;
end method content-size;

define method getnetent
    ()
 => (result :: <netent>);
  let result-value
    = call-out("getnetent", ptr:);
  let result-value = make(<netent>, pointer: result-value);
  values(result-value);
end method getnetent;

define method getnetbyaddr
    (arg1 :: <uint32-t>, arg2 :: <integer>)
 => (result :: <netent>);
  let result-value
    = call-out("getnetbyaddr", ptr:, unsigned-int: arg1, int: arg2);
  let result-value = make(<netent>, pointer: result-value);
  values(result-value);
end method getnetbyaddr;

define method getnetbyname
    (arg1 :: <c-char-vector>)
 => (result :: <netent>);
  let result-value
    = call-out("getnetbyname", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<netent>, pointer: result-value);
  values(result-value);
end method getnetbyname;

define functional class <anonymous-73> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-73>));

define method pointer-value
    (ptr :: <anonymous-73>, #key index = 0)
 => (result :: <netent>);
  pointer-at(ptr, offset: index * 4, class: <netent>);
end method pointer-value;

define method pointer-value-setter
    (value :: <netent>, ptr :: <anonymous-73>, #key index = 0)
 => (result :: <netent>);
  pointer-at(ptr, offset: index * 4, class: <netent>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-73>)) => (result :: <integer>);
  4;
end method content-size;

define method getnetent-r
    (arg1 :: <netent>, arg2 :: <c-char-vector>, arg3 :: <size-t>, arg4 :: <anonymous-73>, arg5 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("getnetent_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, ptr: (arg4).raw-value, ptr: (arg5).raw-value);
  values(result-value);
end method getnetent-r;

define method getnetbyaddr-r
    (arg1 :: <uint32-t>, arg2 :: <integer>, arg3 :: <netent>, arg4 :: <c-char-vector>, arg5 :: <size-t>, arg6 :: <anonymous-73>, arg7 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("getnetbyaddr_r", int:, unsigned-int: arg1, int: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, long: arg5, ptr: (arg6).raw-value, ptr: (arg7).raw-value);
  values(result-value);
end method getnetbyaddr-r;

define method getnetbyname-r
    (arg1 :: <c-char-vector>, arg2 :: <netent>, arg3 :: <c-char-vector>, arg4 :: <size-t>, arg5 :: <anonymous-73>, arg6 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("getnetbyname_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, ptr: (arg5).raw-value, ptr: (arg6).raw-value);
  values(result-value);
end method getnetbyname-r;

define functional class <servent> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<servent>));

define sealed method get-s-name
    (ptr :: <servent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>);
end method get-s-name;

define sealed method get-s-name-setter
    (value :: <c-char-vector>, ptr :: <servent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>) := value;
  value;
end method get-s-name-setter;

define sealed method get-s-aliases
    (ptr :: <servent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>);
end method get-s-aliases;

define sealed method get-s-aliases-setter
    (value :: <c-char-pointer-vector>, ptr :: <servent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>) := value;
  value;
end method get-s-aliases-setter;

define sealed method get-s-port
    (ptr :: <servent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-s-port;

define sealed method get-s-port-setter
    (value :: <integer>, ptr :: <servent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-s-port-setter;

define sealed method get-s-proto
    (ptr :: <servent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 12, class: <c-char-vector>);
end method get-s-proto;

define sealed method get-s-proto-setter
    (value :: <c-char-vector>, ptr :: <servent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 12, class: <c-char-vector>) := value;
  value;
end method get-s-proto-setter;

define method pointer-value (value :: <servent>, #key index = 0) => (result :: <servent>);
  value + index * 16;
end method pointer-value;

define method content-size (value :: subclass(<servent>)) => (result :: <integer>);
  16;
end method content-size;

define method setservent
    (arg1 :: <integer>)
 => ();
  call-out("setservent", void:, int: arg1);
  values();
end method setservent;

define method endservent
    ()
 => ();
  call-out("endservent", void:);
  values();
end method endservent;

define method getservent
    ()
 => (result :: <servent>);
  let result-value
    = call-out("getservent", ptr:);
  let result-value = make(<servent>, pointer: result-value);
  values(result-value);
end method getservent;

define method getservbyname
    (arg1 :: <c-char-vector>, arg2 :: <c-char-vector>)
 => (result :: <servent>);
  let result-value
    = call-out("getservbyname", ptr:, ptr: (arg1).raw-value, ptr: (arg2).raw-value);
  let result-value = make(<servent>, pointer: result-value);
  values(result-value);
end method getservbyname;

define method getservbyport
    (arg1 :: <integer>, arg2 :: <c-char-vector>)
 => (result :: <servent>);
  let result-value
    = call-out("getservbyport", ptr:, int: arg1, ptr: (arg2).raw-value);
  let result-value = make(<servent>, pointer: result-value);
  values(result-value);
end method getservbyport;

define functional class <anonymous-82> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-82>));

define method pointer-value
    (ptr :: <anonymous-82>, #key index = 0)
 => (result :: <servent>);
  pointer-at(ptr, offset: index * 4, class: <servent>);
end method pointer-value;

define method pointer-value-setter
    (value :: <servent>, ptr :: <anonymous-82>, #key index = 0)
 => (result :: <servent>);
  pointer-at(ptr, offset: index * 4, class: <servent>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-82>)) => (result :: <integer>);
  4;
end method content-size;

define method getservent-r
    (arg1 :: <servent>, arg2 :: <c-char-vector>, arg3 :: <size-t>, arg4 :: <anonymous-82>)
 => (result :: <integer>);
  let result-value
    = call-out("getservent_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, ptr: (arg4).raw-value);
  values(result-value);
end method getservent-r;

define method getservbyname-r
    (arg1 :: <c-char-vector>, arg2 :: <c-char-vector>, arg3 :: <servent>, arg4 :: <c-char-vector>, arg5 :: <size-t>, arg6 :: <anonymous-82>)
 => (result :: <integer>);
  let result-value
    = call-out("getservbyname_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, long: arg5, ptr: (arg6).raw-value);
  values(result-value);
end method getservbyname-r;

define method getservbyport-r
    (arg1 :: <integer>, arg2 :: <c-char-vector>, arg3 :: <servent>, arg4 :: <c-char-vector>, arg5 :: <size-t>, arg6 :: <anonymous-82>)
 => (result :: <integer>);
  let result-value
    = call-out("getservbyport_r", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, long: arg5, ptr: (arg6).raw-value);
  values(result-value);
end method getservbyport-r;

define functional class <protoent> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<protoent>));

define sealed method get-p-name
    (ptr :: <protoent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>);
end method get-p-name;

define sealed method get-p-name-setter
    (value :: <c-char-vector>, ptr :: <protoent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>) := value;
  value;
end method get-p-name-setter;

define sealed method get-p-aliases
    (ptr :: <protoent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>);
end method get-p-aliases;

define sealed method get-p-aliases-setter
    (value :: <c-char-pointer-vector>, ptr :: <protoent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>) := value;
  value;
end method get-p-aliases-setter;

define sealed method get-p-proto
    (ptr :: <protoent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-p-proto;

define sealed method get-p-proto-setter
    (value :: <integer>, ptr :: <protoent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-p-proto-setter;

define method pointer-value (value :: <protoent>, #key index = 0) => (result :: <protoent>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<protoent>)) => (result :: <integer>);
  12;
end method content-size;

define method setprotoent
    (arg1 :: <integer>)
 => ();
  call-out("setprotoent", void:, int: arg1);
  values();
end method setprotoent;

define method endprotoent
    ()
 => ();
  call-out("endprotoent", void:);
  values();
end method endprotoent;

define method getprotoent
    ()
 => (result :: <protoent>);
  let result-value
    = call-out("getprotoent", ptr:);
  let result-value = make(<protoent>, pointer: result-value);
  values(result-value);
end method getprotoent;

define method getprotobyname
    (arg1 :: <byte-string>)
 => (result :: <protoent>);
  let result-value
    = call-out("getprotobyname", ptr:, ptr: (export-value(<c-string>, arg1)).raw-value);
  let result-value = make(<protoent>, pointer: result-value);
  values(result-value);
end method getprotobyname;

define method getprotobynumber
    (arg1 :: <integer>)
 => (result :: <protoent>);
  let result-value
    = call-out("getprotobynumber", ptr:, int: arg1);
  let result-value = make(<protoent>, pointer: result-value);
  values(result-value);
end method getprotobynumber;

define functional class <anonymous-91> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-91>));

define method pointer-value
    (ptr :: <anonymous-91>, #key index = 0)
 => (result :: <protoent>);
  pointer-at(ptr, offset: index * 4, class: <protoent>);
end method pointer-value;

define method pointer-value-setter
    (value :: <protoent>, ptr :: <anonymous-91>, #key index = 0)
 => (result :: <protoent>);
  pointer-at(ptr, offset: index * 4, class: <protoent>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-91>)) => (result :: <integer>);
  4;
end method content-size;

define method getprotoent-r
    (arg1 :: <protoent>, arg2 :: <c-char-vector>, arg3 :: <size-t>, arg4 :: <anonymous-91>)
 => (result :: <integer>);
  let result-value
    = call-out("getprotoent_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, ptr: (arg4).raw-value);
  values(result-value);
end method getprotoent-r;

define method getprotobyname-r
    (arg1 :: <c-char-vector>, arg2 :: <protoent>, arg3 :: <c-char-vector>, arg4 :: <size-t>, arg5 :: <anonymous-91>)
 => (result :: <integer>);
  let result-value
    = call-out("getprotobyname_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, ptr: (arg5).raw-value);
  values(result-value);
end method getprotobyname-r;

define method getprotobynumber-r
    (arg1 :: <integer>, arg2 :: <protoent>, arg3 :: <c-char-vector>, arg4 :: <size-t>, arg5 :: <anonymous-91>)
 => (result :: <integer>);
  let result-value
    = call-out("getprotobynumber_r", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, ptr: (arg5).raw-value);
  values(result-value);
end method getprotobynumber-r;

define method setnetgrent
    (arg1 :: <c-char-vector>)
 => (result :: <integer>);
  let result-value
    = call-out("setnetgrent", int:, ptr: (arg1).raw-value);
  values(result-value);
end method setnetgrent;

define method endnetgrent
    ()
 => ();
  call-out("endnetgrent", void:);
  values();
end method endnetgrent;

define method getnetgrent
    (arg1 :: <c-char-pointer-vector>, arg2 :: <c-char-pointer-vector>, arg3 :: <c-char-pointer-vector>)
 => (result :: <integer>);
  let result-value
    = call-out("getnetgrent", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value);
  values(result-value);
end method getnetgrent;

define method innetgr
    (arg1 :: <c-char-vector>, arg2 :: <c-char-vector>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>)
 => (result :: <integer>);
  let result-value
    = call-out("innetgr", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values(result-value);
end method innetgr;

define method getnetgrent-r
    (arg1 :: <c-char-pointer-vector>, arg2 :: <c-char-pointer-vector>, arg3 :: <c-char-pointer-vector>, arg4 :: <c-char-vector>, arg5 :: <size-t>)
 => (result :: <integer>);
  let result-value
    = call-out("getnetgrent_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value, long: arg5);
  values(result-value);
end method getnetgrent-r;

define method rcmd
    (arg1 :: <c-char-pointer-vector>, arg2 :: <integer>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>, arg5 :: <c-char-vector>, arg6 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("rcmd", int:, ptr: (arg1).raw-value, unsigned-short: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value);
  values(result-value);
end method rcmd;

define method rcmd-af
    (arg1 :: <c-char-pointer-vector>, arg2 :: <integer>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>, arg5 :: <c-char-vector>, arg6 :: <anonymous-7>, arg7 :: <sa-family-t>)
 => (result :: <integer>);
  let result-value
    = call-out("rcmd_af", int:, ptr: (arg1).raw-value, unsigned-short: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value, unsigned-short: arg7);
  values(result-value);
end method rcmd-af;

define method rexec
    (arg1 :: <c-char-pointer-vector>, arg2 :: <integer>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>, arg5 :: <c-char-vector>, arg6 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("rexec", int:, ptr: (arg1).raw-value, int: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value);
  values(result-value);
end method rexec;

define method rexec-af
    (arg1 :: <c-char-pointer-vector>, arg2 :: <integer>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>, arg5 :: <c-char-vector>, arg6 :: <anonymous-7>, arg7 :: <sa-family-t>)
 => (result :: <integer>);
  let result-value
    = call-out("rexec_af", int:, ptr: (arg1).raw-value, int: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, ptr: (arg5).raw-value, ptr: (arg6).raw-value, unsigned-short: arg7);
  values(result-value);
end method rexec-af;

define method ruserok
    (arg1 :: <c-char-vector>, arg2 :: <integer>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>)
 => (result :: <integer>);
  let result-value
    = call-out("ruserok", int:, ptr: (arg1).raw-value, int: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values(result-value);
end method ruserok;

define method ruserok-af
    (arg1 :: <c-char-vector>, arg2 :: <integer>, arg3 :: <c-char-vector>, arg4 :: <c-char-vector>, arg5 :: <sa-family-t>)
 => (result :: <integer>);
  let result-value
    = call-out("ruserok_af", int:, ptr: (arg1).raw-value, int: arg2, ptr: (arg3).raw-value, ptr: (arg4).raw-value, unsigned-short: arg5);
  values(result-value);
end method ruserok-af;

define method rresvport
    (arg1 :: <anonymous-7>)
 => (result :: <integer>);
  let result-value
    = call-out("rresvport", int:, ptr: (arg1).raw-value);
  values(result-value);
end method rresvport;

define method rresvport-af
    (arg1 :: <anonymous-7>, arg2 :: <sa-family-t>)
 => (result :: <integer>);
  let result-value
    = call-out("rresvport_af", int:, ptr: (arg1).raw-value, unsigned-short: arg2);
  values(result-value);
end method rresvport-af;

define functional class <addrinfo> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<addrinfo>));

define sealed method get-ai-flags
    (ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0);
end method get-ai-flags;

define sealed method get-ai-flags-setter
    (value :: <integer>, ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 0) := value;
  value;
end method get-ai-flags-setter;

define sealed method get-ai-family
    (ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4);
end method get-ai-family;

define sealed method get-ai-family-setter
    (value :: <integer>, ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 4) := value;
  value;
end method get-ai-family-setter;

define sealed method get-ai-socktype
    (ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-ai-socktype;

define sealed method get-ai-socktype-setter
    (value :: <integer>, ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-ai-socktype-setter;

define sealed method get-ai-protocol
    (ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12);
end method get-ai-protocol;

define sealed method get-ai-protocol-setter
    (value :: <integer>, ptr :: <addrinfo>) => (result :: <integer>);
  signed-long-at(ptr, offset: 12) := value;
  value;
end method get-ai-protocol-setter;

define sealed method get-ai-addrlen
    (ptr :: <addrinfo>) => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: 16);
end method get-ai-addrlen;

define sealed method get-ai-addrlen-setter
    (value :: <socklen-t>, ptr :: <addrinfo>) => (result :: <socklen-t>);
  unsigned-long-at(ptr, offset: 16) := value;
  value;
end method get-ai-addrlen-setter;

define sealed method get-ai-addr
    (ptr :: <addrinfo>) => (result :: <sockaddr>);
  pointer-at(ptr, offset: 20, class: <sockaddr>);
end method get-ai-addr;

define sealed method get-ai-addr-setter
    (value :: <sockaddr>, ptr :: <addrinfo>) => (result :: <sockaddr>);
  pointer-at(ptr, offset: 20, class: <sockaddr>) := value;
  value;
end method get-ai-addr-setter;

define sealed method get-ai-canonname
    (ptr :: <addrinfo>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 24, class: <c-char-vector>);
end method get-ai-canonname;

define sealed method get-ai-canonname-setter
    (value :: <c-char-vector>, ptr :: <addrinfo>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 24, class: <c-char-vector>) := value;
  value;
end method get-ai-canonname-setter;

define sealed method get-ai-next
    (ptr :: <addrinfo>) => (result :: <addrinfo>);
  pointer-at(ptr, offset: 28, class: <addrinfo>);
end method get-ai-next;

define sealed method get-ai-next-setter
    (value :: <addrinfo>, ptr :: <addrinfo>) => (result :: <addrinfo>);
  pointer-at(ptr, offset: 28, class: <addrinfo>) := value;
  value;
end method get-ai-next-setter;

define method pointer-value (value :: <addrinfo>, #key index = 0) => (result :: <addrinfo>);
  value + index * 32;
end method pointer-value;

define method content-size (value :: subclass(<addrinfo>)) => (result :: <integer>);
  32;
end method content-size;

define functional class <anonymous-108> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-108>));

define method pointer-value
    (ptr :: <anonymous-108>, #key index = 0)
 => (result :: <addrinfo>);
  pointer-at(ptr, offset: index * 4, class: <addrinfo>);
end method pointer-value;

define method pointer-value-setter
    (value :: <addrinfo>, ptr :: <anonymous-108>, #key index = 0)
 => (result :: <addrinfo>);
  pointer-at(ptr, offset: index * 4, class: <addrinfo>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-108>)) => (result :: <integer>);
  4;
end method content-size;

define method getaddrinfo
    (arg1 :: <c-char-vector>, arg2 :: <c-char-vector>, arg3 :: <addrinfo>, arg4 :: <anonymous-108>)
 => (result :: <integer>);
  let result-value
    = call-out("getaddrinfo", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, ptr: (arg4).raw-value);
  values(result-value);
end method getaddrinfo;

define method freeaddrinfo
    (arg1 :: <addrinfo>)
 => ();
  call-out("freeaddrinfo", void:, ptr: (arg1).raw-value);
  values();
end method freeaddrinfo;

define method gai-strerror
    (arg1 :: <integer>)
 => (result :: <c-char-vector>);
  let result-value
    = call-out("gai_strerror", ptr:, int: arg1);
  let result-value = make(<c-char-vector>, pointer: result-value);
  values(result-value);
end method gai-strerror;

define method getnameinfo
    (arg1 :: <sockaddr>, arg2 :: <socklen-t>, arg3 :: <c-char-vector>, arg4 :: <socklen-t>, arg5 :: <c-char-vector>, arg6 :: <socklen-t>, arg7 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("getnameinfo", int:, ptr: (arg1).raw-value, unsigned-int: arg2, ptr: (arg3).raw-value, unsigned-int: arg4, ptr: (arg5).raw-value, unsigned-int: arg6, unsigned-int: arg7);
  values(result-value);
end method getnameinfo;

define constant $_NETDB-H = 1;

define constant $NETDB-INTERNAL = -1;

define constant $NETDB-SUCCESS = 0;

define constant $HOST-NOT-FOUND = 1;

define constant $TRY-AGAIN = 2;

define constant $NO-RECOVERY = 3;

define constant $NO-DATA = 4;

define constant $NO-ADDRESS = 4;

define constant $AI-PASSIVE = 1;

define constant $AI-CANONNAME = 2;

define constant $AI-NUMERICHOST = 4;

define constant $EAI-BADFLAGS = -1;

define constant $EAI-NONAME = -2;

define constant $EAI-AGAIN = -3;

define constant $EAI-FAIL = -4;

define constant $EAI-NODATA = -5;

define constant $EAI-FAMILY = -6;

define constant $EAI-SOCKTYPE = -7;

define constant $EAI-SERVICE = -8;

define constant $EAI-ADDRFAMILY = -9;

define constant $EAI-MEMORY = -10;

define constant $EAI-SYSTEM = -11;

define constant $NI-MAXHOST = 1025;

define constant $NI-MAXSERV = 32;

define constant $NI-NUMERICHOST = 1;

define constant $NI-NUMERICSERV = 2;

define constant $NI-NOFQDN = 4;

define constant $NI-NAMEREQD = 8;

define constant $NI-DGRAM = 16;

define functional class <rpcent> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<rpcent>));

define sealed method get-r-name
    (ptr :: <rpcent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>);
end method get-r-name;

define sealed method get-r-name-setter
    (value :: <c-char-vector>, ptr :: <rpcent>) => (result :: <c-char-vector>);
  pointer-at(ptr, offset: 0, class: <c-char-vector>) := value;
  value;
end method get-r-name-setter;

define sealed method get-r-aliases
    (ptr :: <rpcent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>);
end method get-r-aliases;

define sealed method get-r-aliases-setter
    (value :: <c-char-pointer-vector>, ptr :: <rpcent>) => (result :: <c-char-pointer-vector>);
  pointer-at(ptr, offset: 4, class: <c-char-pointer-vector>) := value;
  value;
end method get-r-aliases-setter;

define sealed method get-r-number
    (ptr :: <rpcent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8);
end method get-r-number;

define sealed method get-r-number-setter
    (value :: <integer>, ptr :: <rpcent>) => (result :: <integer>);
  signed-long-at(ptr, offset: 8) := value;
  value;
end method get-r-number-setter;

define method pointer-value (value :: <rpcent>, #key index = 0) => (result :: <rpcent>);
  value + index * 12;
end method pointer-value;

define method content-size (value :: subclass(<rpcent>)) => (result :: <integer>);
  12;
end method content-size;

define method setrpcent
    (arg1 :: <integer>)
 => ();
  call-out("setrpcent", void:, int: arg1);
  values();
end method setrpcent;

define method endrpcent
    ()
 => ();
  call-out("endrpcent", void:);
  values();
end method endrpcent;

define method getrpcbyname
    (arg1 :: <c-char-vector>)
 => (result :: <rpcent>);
  let result-value
    = call-out("getrpcbyname", ptr:, ptr: (arg1).raw-value);
  let result-value = make(<rpcent>, pointer: result-value);
  values(result-value);
end method getrpcbyname;

define method getrpcbynumber
    (arg1 :: <integer>)
 => (result :: <rpcent>);
  let result-value
    = call-out("getrpcbynumber", ptr:, int: arg1);
  let result-value = make(<rpcent>, pointer: result-value);
  values(result-value);
end method getrpcbynumber;

define method getrpcent
    ()
 => (result :: <rpcent>);
  let result-value
    = call-out("getrpcent", ptr:);
  let result-value = make(<rpcent>, pointer: result-value);
  values(result-value);
end method getrpcent;

define functional class <anonymous-50> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<anonymous-50>));

define method pointer-value
    (ptr :: <anonymous-50>, #key index = 0)
 => (result :: <rpcent>);
  pointer-at(ptr, offset: index * 4, class: <rpcent>);
end method pointer-value;

define method pointer-value-setter
    (value :: <rpcent>, ptr :: <anonymous-50>, #key index = 0)
 => (result :: <rpcent>);
  pointer-at(ptr, offset: index * 4, class: <rpcent>) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<anonymous-50>)) => (result :: <integer>);
  4;
end method content-size;

define method getrpcbyname-r
    (arg1 :: <c-char-vector>, arg2 :: <rpcent>, arg3 :: <c-char-vector>, arg4 :: <size-t>, arg5 :: <anonymous-50>)
 => (result :: <integer>);
  let result-value
    = call-out("getrpcbyname_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, ptr: (arg5).raw-value);
  values(result-value);
end method getrpcbyname-r;

define method getrpcbynumber-r
    (arg1 :: <integer>, arg2 :: <rpcent>, arg3 :: <c-char-vector>, arg4 :: <size-t>, arg5 :: <anonymous-50>)
 => (result :: <integer>);
  let result-value
    = call-out("getrpcbynumber_r", int:, int: arg1, ptr: (arg2).raw-value, ptr: (arg3).raw-value, long: arg4, ptr: (arg5).raw-value);
  values(result-value);
end method getrpcbynumber-r;

define method getrpcent-r
    (arg1 :: <rpcent>, arg2 :: <c-char-vector>, arg3 :: <size-t>, arg4 :: <anonymous-50>)
 => (result :: <integer>);
  let result-value
    = call-out("getrpcent_r", int:, ptr: (arg1).raw-value, ptr: (arg2).raw-value, long: arg3, ptr: (arg4).raw-value);
  values(result-value);
end method getrpcent-r;

define constant $_RPC-NETDB-H = 1;

