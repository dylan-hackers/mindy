module:      types
rcs-header:  $Header: /scm/cvs/src/common/file-system/types.dylan,v 1.1 2000/10/21 03:39:41 dauclair Exp $
author:      Douglas M. Auclair, dauclair@hotmail.com

// The types used by the file-system module, these types mirror the 
// Function Developer file-system types found at
// http://www.functionalobjects.com/products/doc/io/io_196.htm

define constant <file-type> = one-of(#"file", #"directory", #"link");

define constant <pathname> = <byte-string>;
define constant <copy/rename-disposition> = one-of(#"signal", #"replace");

define class <file-system-error> (<error>, <simple-condition>) end;
