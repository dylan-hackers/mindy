module: dylan-user
rcs-header: $Header: /scm/cvs/src/demos/diff/diff-exports.dylan,v 1.1 1998/05/03 19:55:59 andreas Exp $

define library diff
  use dylan;
  use streams;
  use standard-io;
  use format;
  use collection-extensions;
end library diff;

// The module name "diff" is already used by the collection-extensions
// module that contains the actual differencing algorithm
//
define module diff-program
  use dylan;
  use extensions, import: {main, %main};
  use streams, import: {<file-stream>, read-line, force-output};
  use standard-io, import: {*standard-output*};
  use format, import: {format};
  use sequence-diff;
end module diff-program;
