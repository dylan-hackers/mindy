module: carbon

/*
  Mac Process Manager
*/

/*
  Includes
*/

c-include( "Carbon.h" );


/*
  <ProcessSerialNumber*>
*/

define functional class <ProcessSerialNumber*> (<statically-typed-pointer>)
end class;

define method content-size ( cls == <ProcessSerialNumber*> )
 => ( result :: <integer> )
  c-expr( int: "sizeof(ProcessSerialNumber)" );
end method content-size;


/*
  <ProcessSerialNumber*> accessors.
*/

define method highLongOfPSN-value ( psn :: <ProcessSerialNumber*> )
 => ( highLongOfPSN :: <integer> )
  unsigned-long-at( psn, offset: 0 );
end method highLongOfPSN-value;

define method highLongOfPSN-value-setter (value :: <integer>, psn :: <ProcessSerialNumber*>)
 => (value :: <integer>);
  unsigned-long-at( psn, offset: 0 ) := value;
end method highLongOfPSN-value-setter;

define method lowLongOfPSN-value ( psn :: <ProcessSerialNumber*> )
 => ( lowLongOfPSN :: <integer> )
  unsigned-long-at( psn, offset: 4 );
end method lowLongOfPSN-value;

define method lowLongOfPSN-value-setter (value :: <integer>, psn :: <ProcessSerialNumber*>)
 => (value :: <integer>);
  unsigned-long-at( psn, offset: 4 ) := value;
end method lowLongOfPSN-value-setter;


/*
  GetCurrentProcess
*/

define method GetCurrentProcess ()
 => ( result :: <OSStatus>, outPSN :: <ProcessSerialNumber*> )
  let temp :: <ProcessSerialNumber*> = make( <ProcessSerialNumber*> );
  let result = call-out( "GetCurrentProcess", int:, ptr: temp.raw-value );
  values( as(<OSErr>, result), temp )
end method GetCurrentProcess;


/*
  SetFrontProcess
*/

define method SetFrontProcess ( psn :: <ProcessSerialNumber*> )
 => ( result :: <OSStatus> )
  as( <OSErr>, call-out( "SetFrontProcess", int:, ptr: psn.raw-value ) )
end method SetFrontProcess;
