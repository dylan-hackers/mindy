Module: dylan-user
RCS-Header: $Header: /home/housel/work/rcs/gd/src/demos/html2txt/html2txt-exports.dylan,v 1.1 1997/05/31 01:20:15 ram Exp $

define library html
  use dylan;
  use streams;
  use standard-io;
  use collection-extensions;
  use string-extensions;
end library html;

define module html
  use dylan;
  
  // A few basic definitions not present in the Dylan spec
  use extensions, import: {main, %main};
  
  // Additional collection classes and operations from "collection-extensions"
  use subseq;
  use self-organizing-list;

  // From string-extensions:
  use substring-search;
  
  // I/O support from the "streams" and "standard-io" libraries
  use streams;
  use standard-io;
  
  export html2text;
end module html;
