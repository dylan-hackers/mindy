module: getaddrinfo-test
use-libraries: common-dylan, io, network, melange-support
use-modules: common-dylan, common-extensions, format, format-out, standard-io, network-internal, melange-support, byte-vector, print, introspection

define constant $null-pointer = as(<statically-typed-pointer>, 0);

define class <network-address> (<object>)
  slot data :: <byte-vector>, required-init-keyword: data:;
end class <network-address>;

define class <ipv4-network-address> (<network-address>)
end class <ipv4-network-address>;

define method address-length(a == <ipv4-network-address>)
  4;
end method address-length;

// HACK!!1!
define method sockaddr-offset(a == <ipv4-network-address>)
  2;
end method sockaddr-offset;

define class <ipv6-network-address> (<network-address>)
end class <ipv6-network-address>;

define method address-length(a == <ipv6-network-address>)
  16;
end method address-length;

// HACK!!1!
define method sockaddr-offset(a == <ipv6-network-address>)
  6;
end method sockaddr-offset;

define method make(class == <network-address>, #rest args, 
                   #key address-family, sockaddr, #all-keys)
 => (address :: <network-address>);
  let desired-class = select(address-family)
                        $AF-INET => <ipv4-network-address>;
                        $AF-INET6 => <ipv6-network-address>;
                        otherwise => signal("Unknown Address Family in call to make(<network-address>, address-family: )");
                      end select;
  let new-data = copy-vector(<byte-vector>, 
                             sockaddr.get-sa-data 
                               + desired-class.sockaddr-offset, 
                             size: desired-class.address-length);

  apply(make, desired-class, #"data", new-data, args);
end method make;

define method print-object(address :: <network-address>, s :: <stream>) => ();
  format(s, "{%s %=}", address.object-class.class-name, address.data);
end method print-object;

begin
  let host = application-arguments()[0];
  let (rc, address-list) = getaddrinfo(host, "http", as(<addrinfo>, 0));
  if(rc ~= 0)
    format-out("Error: %=\n", rc);
  else
    for(a = address-list then a.get-ai-next,
        until: a = $null-pointer)
      format-out("Flags: %=, Family: %=, Socktype: %=, Protocol: %=\n",
                 a.get-ai-flags,
                 a.get-ai-family,
                 a.get-ai-socktype,
                 a.get-ai-protocol);
      
      let canonical-name = a.get-ai-canonname;
      if(canonical-name ~= $null-pointer)
        format-out("Canoncial name: %=\n", canonical-name);
      end;
      let address = make(<network-address>, 
                         address-family: a.get-ai-family,
                         sockaddr: a.get-ai-addr);
      format-out("Address: %=\n", address);
//      format-out("Address: %=\n", copy-vector(<vector>, a.get-ai-addr.get-sa-data, size: a.get-ai-addrlen));
    end for;
  end;
end;

define method copy-vector(class :: subclass(<vector>), c-vector :: <c-vector>, #key size)
 => (new-vector :: <vector>);
  let vector-size = c-vector.object-class.content-size;
  let new-size = if(size)
                   if(vector-size)
                     if(vector-size < size)
                       signal("Accessing C array beyond known bounds");
                     end if
                   else
                     signal("Accessing C array of unknown size");
                   end if;
                   size;
                 else
                   if(vector-size)
                     vector-size
                   else
                     error("Accessing C array of unknown size and no size: keyword given to call of as(c == <vector>, v :: <c-vector>");
                   end if
                 end if;
                           
  let new-vector = make(class, size: new-size);
  for(i from 0 below new-size)
    new-vector[i] := modulo(c-vector[i], 256);
  end for;
  new-vector;
end method copy-vector;

    