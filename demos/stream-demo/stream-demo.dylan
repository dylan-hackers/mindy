rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/stream-demo/stream-demo.dylan,v 1.4 1996/07/12 01:12:03 bfw Exp $
module: test

define method main (foo, #rest stuff)
  
  write(*standard-output*, "File to type: ");
  force-output(*standard-output*);
  let res = read-line(*standard-input*);
  write(*standard-output*, "You said: ");
  write-line(*standard-output*, res);
  force-output(*standard-output*);
  let stream = make(<file-stream>, locator: res, direction: #"input");
  block (punt)
    for (while: #t)
      let wot = read-line(stream, on-end-of-stream: #f);
      if (wot)
        write-line(*standard-output*, wot);
      else
        punt();
      end if;
    end for;
  end block;
  close(stream);
  force-output(*standard-output*);

end method;

main("foo");
