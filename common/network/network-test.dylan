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
  
//  format(output-stream, "%s\n", request);
//  force-output(output-stream);
  let running = #t;
  while(running)
    let rc = poll(poll-list, 4, 1000);
//    for(i from 0 below 4)
//      format(*standard-error*, "revents[%=] = %=  ", i, poll-list[i].get-revents);
//    end for;
//    format(*standard-error*, "\n");
    if(logand(poll-list[0].get-revents, $POLLIN) > 0)
      format(*standard-output*, "%s\n", read-line(input-stream));
      force-output(*standard-output*);
    end if;
    if(logand(poll-list[2].get-revents, $POLLIN) > 0)
      format(output-stream, "%s\n", read-line(*standard-input*));
      force-output(output-stream);
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
  if(rc == -1)
    error("sendto() failed");
  end if;
end;

define method as(class == <double-float>, tv :: <timeval>) 
 => (d :: <double-float>);
  as(<double-float>, tv.get-tv-sec) +
    as(<double-float>, tv.get-tv-usec) / 1000000.0;
end method as;

define method as(class == <timeval>, d :: <double-float>)
 => (tv :: <timeval>);
  if(d < 0.0)
    error("Cannot convert negative time value to <timeval>");
  end if;
  let tv = make(<timeval>);
  let (seconds, rest) = floor(d);
  tv.get-tv-sec := seconds;
  tv.get-tv-usec := floor/(rest, 1000000.0);
  tv;
end method as;

define method now()
  let tv = make(<timeval>);
  gettimeofday(tv, as(<timezone>, 0));
  as(<double-float>, tv);
end method now;

define method print-object(tv :: <timeval>, s :: <stream>) => ()
  format(s, "{<timeval>: %=.%= sec}", tv.get-tv-sec, tv.get-tv-usec);
end method;

define class <streamer> (<object>) // class for sending out data at defined bitrate
  slot fd;
  slot address;
  slot interval;
  slot wanted-interval;
  slot last-sent;
  slot data, init-keyword: data:;
end class <streamer>;

define method initialize(s :: <streamer>, 
                         #key bitrate, target-address, #all-keys)
  force-output(*standard-output*);
  next-method();
  s.interval := as(<double-float>, s.data.size) * 8.0 / 
    as(<double-float>, bitrate);

  format-out("Interval is: %=\n", s.interval);
  force-output(*standard-output*);

  let host = gethostbyname(target-address);
  s.address := make(<sockaddr-in>);
  s.address.get-sin-family := host.get-h-addrtype;
  for(i from 0 below host.get-h-length)
    s.address.get-sa-data[i + 2] // yuck
      := host.get-h-addr-list[0][i];
  end for;
  s.address.get-sin-port := htons(1178);

  s.fd := socket($PF-INET, $SOCK-DGRAM, $IPPROTO-UDP);
  if(s.fd == -1)
    error("socket() failed");
  end if;
  s.wanted-interval := s.interval;
  s.last-sent := now();
end method initialize;

define method bitrate(s :: <streamer>) => (rate :: <double-float>);
  s.data.size * 8.0 / s.interval
end method bitrate;

define constant *expected-jitter* = 0.1;

define method send-data(s :: <streamer>)
  let stamp = now();

/*
  if(stamp >  s.last-sent)
    format-out("We're %= late.\n", stamp - s.last-sent);
  end;
*/
  if(stamp >  s.last-sent + *expected-jitter*)
    s.interval := s.interval * 1.01;
    format-out("Oh my golly, that's more than expected jitter. ");
    format-out("Adjusting bitrate to %=\n", s.bitrate);
    s.last-sent := s.last-sent + *expected-jitter* / 2.0; // empty buffer
    force-output(*standard-output*);
  end;

/*
  if(stamp < s.last-sent)
    format-out("We're %= early.\n", s.last-sent - stamp);
  end;
*/

  let rc = sendto(s.fd, s.data, s.data.size, 0,
                  s.address, <sockaddr-in>.content-size);
  if(rc == -1)
    error("sendto() failed");
  end if;

  s.last-sent := s.last-sent + s.interval;
end method send-data;

begin
  let args = application-arguments();
  let target-address = "127.0.0.1";
  let data-size = 1024;
  let wanted-bitrate = 20000;
  if(args.size = 3)
    target-address := args[0];
    data-size := string-to-integer(args[1]);
    wanted-bitrate := string-to-integer(args[2]);
  end if;
//  tcp-client-test();
  let foo = make(<streamer>, target-address: target-address, 
                 data: make(<byte-string>, size: data-size, fill: 'A'),
                 bitrate: wanted-bitrate);
  while(1)
    send-data(foo);
    let stamp = now();
    if(stamp < foo.last-sent)
      let time-to-burn = foo.last-sent - stamp;
      if(foo.wanted-interval < foo.interval)
        let less-time-wanted = foo.interval - foo.wanted-interval;
        if(less-time-wanted < 0.05 * foo.wanted-interval)
          foo.interval := foo.wanted-interval;
        else
          foo.interval := foo.interval 
            - min(time-to-burn, less-time-wanted) * 0.05;
        end;
        format-out("Increasing bitrate to %=.\n", foo.bitrate);
        force-output(*standard-output*);
      end if;
      poll(as(<pollfd>, 0), 0, floor(time-to-burn * 1000.0));
    end if;
  end while;
end;
