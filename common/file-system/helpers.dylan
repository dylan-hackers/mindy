module:      helpers
author:      Douglas M. Auclair, dauclair@hotmail.com

// Some functions that help the implementation modules do their job

define generic do-file-operation(fn :: <function>, 
				 old-file :: <pathname>,
				 new-file :: <pathname>,
				 disposition :: <copy/rename-disposition>);

define method do-file-operation(fn :: <function>,
				old-file :: <pathname>,
				new-file :: <pathname>,
				force-overwrite :: <copy/rename-disposition>)
  try(fn,old-file, new-file);
end method do-file-operation;

define method do-file-operation(fn :: <function>,
				old-file :: <pathname>,
				new-file :: <pathname>,
				disposition == #"signal")
  if(~ file-exists?(new-file) | go-ahead?(new-file))
    next-method();
  end if;
end method do-file-operation;

define method try(fn :: <function>, #rest args)
  block()
    apply(fn, args)
  exception(cond :: <condition>)
    file-signal(fn, args);
  end;
end method try;

define method try-call-out(fn-name :: <string>, 
			   param :: <string>, 
			   #rest args)
  let fn = raw-value(export-value(<c-string>, fn-name));
  let string = raw-value(export-value(<c-string>, param));
  let result :: <integer> = // apply(\call-out, fn, int:, string, args);
    if(args)
      call-out(fn, int:, ptr: string, int: args[0]);
    else
      call-out(fn, int:, ptr: string);
    end if;

  unless(result = 0)
    file-signal(fn-name, list(param, args))
  end unless;
end method try-call-out;

define constant $path-separator = "/";

// a helper for the file-system-error class
define method file-signal(#rest args) => err :: <file-system-error>;
  signal(make(<file-system-error>,
	      format-string: "A file system error occurred in %="
		" with arguments %=",
	      format-arguments: args));
end method file-signal;

define function as-dir(path :: <pathname>) => directory :: <pathname>;
  concatenate(path, $path-separator);
end function as-dir;

define function append(a :: <list>, b :: <string>) => c :: <list>;
  reverse(pair(b, reverse(a)));
end function append;

//-------------------------------------------------------
// c interfacing

define macro with-pointer
{
 with-pointer(?ptr:variable = ?obj:expression)
     ?:body 
 end 
} 
  => {
      let ptr = #f;
      let obj = ?obj;
      block()
	ptr := create-pointer(object-class(obj), obj);
	let ?ptr = ptr;
	?body;
      cleanup
	if(ptr)
	  destroy(ptr)
	end if;
      end block
}
end macro with-pointer;

define function convert-to-string(c :: <statically-typed-pointer>)
 => b :: <string>;
  let char :: <character> = '*';
  let str = make(<deque>);
  for(index from 0, until: char = '\0')
    char := as(<character>, unsigned-byte-at(c, offset: index));
    push-last(str, char);
  end for;
  pop-last(str);
  as(<string>, str);
end function convert-to-string;

define generic create-pointer(c :: <class>, obj);

// -------------------------------------------------------
// Internal fns
// -------------------------------------------------------

define method go-ahead?(file :: <pathname>) => (b :: <boolean>)
  let ans = flush-and-read("*WARNING* %s exists!  Overwrite? [No] ", file);
  size(ans) > 0 & (ans[0] = 'Y' | ans[0] = 'y')
end method go-ahead?;

define method flush-and-read(control-string :: <string>, #rest args)
 => (t :: <string>)
  apply(format-out, control-string, args);
  force-output(*standard-output*);
  let ans = make(<byte-string-stream>, 
		 contents: make(<byte-string>, size: 256),
	         direction: #"output");
  let char :: <character> = '*';
  until(char = '\n')
    char := read-element(*standard-input*,on-end-of-stream: '\n');
    write-element(ans,char);
  end until;
  let string = ans.stream-contents;
  copy-sequence(string, end: size(string) - 1);
end method flush-and-read;
