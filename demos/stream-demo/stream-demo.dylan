rcs-header: $Header: /home/housel/work/rcs/gd/src/demos/stream-demo/stream-demo.dylan,v 1.3 1996/01/04 15:28:18 ram Exp $
module: test

define method main (foo, #rest stuff)
  
  write("File to type: ", *standard-output*);
  force-output(*standard-output*);
  let res = read-line(*standard-input*);
  write("You said: ", *standard-output*);
  write-line(res, *standard-output*);
  force-output(*standard-output*);
  let stream = make(<file-stream>, name: res, direction: #"input");
  block (punt)
    for (while: #t)
      let wot = read-line(stream, signal-eof?: #f);
      if (wot)
        write-line(wot, *standard-output*);
      else
        punt();
      end if;
    end for;
  end block;
  close(stream);
  force-output(*standard-output*);

end method;

main("foo");
