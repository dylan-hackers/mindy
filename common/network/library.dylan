module: dylan-user

define library network
  use common-dylan;
  use io;
  use melange-support;

  export network-internal;
end library network;

define module network-internal
  use common-dylan;
  use format-out;
  use melange-support, 
    export: { pointer-value, pointer-at,
             content-size, <c-vector>};

  export 
    gethostbyname,
    getprotobyname, 
    socket,
    connect,
    htons,
    $PF-INET, 
    $SOCK-STREAM,
    get-p-proto,
    get-sa-data,
    get-sin-family,
    get-sin-family-setter,
    get-sin-port,
    get-sin-port-setter,
    get-h-addr-list,
    get-h-addrtype,
    get-h-length,
    <sockaddr-in>;
end module network-internal;

