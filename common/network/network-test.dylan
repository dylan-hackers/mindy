module: network-test

define method tcp-client-test()
  let hostname = if(application-arguments().size > 0)
                   application-arguments()[0]
                 else
                   "www.gwydiondylan.org"
                 end;
  let port = if(application-arguments().size > 1)
                   string-to-integer(application-arguments()[1], 
                                     default: 80)
                 else
                   80
                 end;
  let request = if(application-arguments().size > 2)
                   application-arguments()[2]
                 else
                   "GET /"
                 end;

  let host = gethostbyname(hostname);
  let server-address = make(<sockaddr-in>);
  server-address.get-sin-family := host.get-h-addrtype;
  for(i from 0 below host.get-h-length)
    server-address.get-sa-data[i + 2] // yuck
      := host.get-h-addr-list[0][i];
  end for;
  server-address.get-sin-port := htons(port);

  let foo = socket($PF-INET, $SOCK-STREAM, $IPPROTO-TCP);
  if(foo == -1)
    error("socket() failed");
  end if;

  let rc = connect(foo, server-address, <sockaddr-in>.content-size);
  if(foo == -1)
    error("connect() failed");
  end if;

  let input-stream = make(<fd-stream>, fd: foo, direction: #"input");
  let output-stream = make(<fd-stream>, fd: foo, direction: #"output");

  let poll-list = make(<pollfd>, element-count: 4);
  map(get-fd-setter, list(foo, foo,
                       *standard-input*.file-descriptor,
                       *standard-output*.file-descriptor),
      poll-list);
  
  map(get-events-setter, 
      list($POLLIN, $POLLOUT, $POLLIN, $POLLOUT), 
      poll-list);
  
  format(output-stream, "%s\n", request);
  force-output(output-stream);
  let running = #t;
  while(running)
    let rc = poll(poll-list, 4, 1000);
    for(i from 0 below 4)
      format(*standard-error*, "revents[%=] = %=  ", i, poll-list[i].get-revents);
    end for;
    format(*standard-error*, "\n");
    if(logand(poll-list[0].get-revents, $POLLIN) > 0)
      format(*standard-output*, "%s\n", read-line(input-stream));
      force-output(*standard-output*);
    end if;
  end while;
end;

define method udp-client-test()
  let hostname = if(application-arguments().size > 0)
                   application-arguments()[0]
                 else
                   "www.gwydiondylan.org"
                 end;
  let port = if(application-arguments().size > 1)
                   string-to-integer(application-arguments()[1], 
                                     default: 80)
                 else
                   80
                 end;
  let request = if(application-arguments().size > 2)
                   application-arguments()[2]
                 else
                   "GET /"
                 end;

  let host = gethostbyname(hostname);
  let server-address = make(<sockaddr-in>);
  server-address.get-sin-family := host.get-h-addrtype;
  for(i from 0 below host.get-h-length)
    server-address.get-sa-data[i + 2] // yuck
      := host.get-h-addr-list[0][i];
  end for;
  server-address.get-sin-port := htons(port);

  let foo = socket($PF-INET, $SOCK-DGRAM, $IPPROTO-UDP);
  if(foo == -1)
    error("socket() failed");
  end if;

  let rc = sendto(foo, request, request.size, 0,
                  server-address, <sockaddr-in>.content-size);
  if(foo == -1)
    error("sendto() failed");
  end if;
end;

begin
  tcp-client-test();
  let tv = make(<timeval>);
  let foo = gettimeofday(tv, as(<timezone>, 0));
  format-out("%=.%=\n", tv.get-tv-sec, tv.get-tv-usec);
end;