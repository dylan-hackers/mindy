module: eager-stream

define library stream-extensions
  use dylan;
  use streams;

  export eager-stream;
end library stream-extensions;

define module eager-stream
  use dylan;
  use extensions;
  use streams;

  export <eager-stream>;
end module eager-stream;

define class <eager-stream> (<stream>)
  slot sub-stream :: <stream>, required-init-keyword: #"stream";
  slot buffer :: false-or(<buffer>), init-value: #f;
end class <eager-stream>;

define method close (stream :: <eager-stream>) => ();
  close(stream.sub-stream);
end method close;

define method stream-extension-get-input-buffer
    (stream :: <eager-stream>)
 => (buffer :: <buffer>, next :: <buffer-index>, last :: <buffer-index>);
  error("stream-extension-get-input-buffer not meaningful "
	  "for <eager-stream>s.");
end method stream-extension-get-input-buffer;

define method stream-extension-release-input-buffer
    (stream :: <eager-stream>, next :: <buffer-index>, last :: <buffer-index>)
 => ();
  error("stream-extension-release-input-buffer not meaningful "
	  "for <eager-stream>s.");
end method stream-extension-release-input-buffer;

define method stream-extension-fill-input-buffer
    (stream :: <eager-stream>, start :: <buffer-index>)
 => (last :: <buffer-index>);
  error("stream-extension-fill-input-buffer not meaningful "
	  "for <eager-stream>s.");
end method stream-extension-fill-input-buffer;

define method stream-extension-input-available-at-source?
    (stream :: <eager-stream>) => (input-available? :: <boolean>);
  #f;
end method stream-extension-input-available-at-source?;

define method stream-extension-get-output-buffer
    (stream :: <eager-stream>)
 => (buffer :: <buffer>, next :: <buffer-index>, last :: <buffer-index>);
  let (buffer, next, last) = 
    stream-extension-get-output-buffer(stream.sub-stream);
  stream.buffer := buffer;
  values(buffer, next, last);
end method stream-extension-get-output-buffer;

define constant newline-value = as(<integer>, '\n');
define method stream-extension-release-output-buffer
    (stream :: <eager-stream>, next :: <buffer-index>)
 => ();
  let buff = stream.buffer;
  if (~buff)
    error("Attempt to realease a buffer which isn't held: %=.", stream);
  end if;
  stream.buffer := #f;

  for (i from next - 1 to 0 by -1, until: buff[i] == newline-value)
  finally
    if (i >= 0)
      // ERROR: fails if result-class is anything other than <byte-string>
      let extra = buffer-subsequence(buff, <byte-string>, i + 1, next);
      stream-extension-empty-output-buffer(stream.sub-stream, i + 1);
      stream-extension-synchronize(stream.sub-stream);
      copy-into-buffer!(extra, buff, 0);
      stream-extension-release-output-buffer(stream.sub-stream, extra.size);
    else
      stream-extension-release-output-buffer(stream.sub-stream, next);
    end if;
  end for;
end method stream-extension-release-output-buffer;

define method stream-extension-empty-output-buffer
    (stream :: <eager-stream>, last :: <buffer-index>)
 => ();
  stream-extension-empty-output-buffer(stream.sub-stream, last);
end method stream-extension-empty-output-buffer;

// Error: documentation claims there are 2 arguments
define method stream-extension-synchronize
    (stream :: <eager-stream>) => ();
  stream-extension-synchronize(stream.sub-stream);
end method stream-extension-synchronize;
