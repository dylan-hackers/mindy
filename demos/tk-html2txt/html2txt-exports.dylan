Module: dylan-user

define library html
  use dylan;
  use streams;
  use standard-io;
  use collection-extensions;
  use string-extensions;
  use tk;
end library html;

define module html
  use dylan;
  
  // A few basic definitions not present in the Dylan spec
  use extensions;
  use threads;
  
  // Additional collection classes and operations from "collection-extensions"
  use subseq;
  use self-organizing-list;

  // From string-extensions:
  use substring-search;
  
  // I/O support from the "streams" and "standard-io" libraries
  use streams;
  use standard-io;

  // And, of course, the nifty new tk/TK library
  use tk;
  use tk-extension;
  
  export html2text;
end module html;
