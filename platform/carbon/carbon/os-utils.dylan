module: os-utils

/*
	OSUtils
*/


/*
	Includes
*/


c-include("Carbon.h");


/*
	<DateTimeRec>
*/

define functional class <DateTimeRec> ( <statically-typed-pointer> ) 
end class <DateTimeRec>;


/*
	content-size
	The size of object a <DateTimeRec> contains
*/

define method content-size( cls == <DateTimeRec> )
=>( result :: <integer> )
	14;
end method content-size;


/*
	<DateTimeRec> Accessors
*/

define method year (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 0);
end method year;


define method month (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 2);
end method month;


define method day (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 4);
end method day;


define method hour (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 6);
end method hour;


define method minute (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 8);
end method minute;


define method seconds (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 10);
end method seconds;


define method dayOfWeek (dateTime :: <DateTimeRec>) => (result :: <integer>);
	signed-short-at(dateTime, offset: 12);
end method dayOfWeek;


/*
	GetDateTime
*/

define method GetDateTime( rec :: <DateTimeRec> )
=> ()
	call-out( "GetDateTime", void:, ptr: rec.raw-value );
	values();
end method;


/*
	SecondsToDate
*/

define method SecondsToDate( seconds :: <integer>, rec :: <DateTimeRec> )
=> ()
	call-out( "SecondsToDate", void:, long: seconds, ptr: rec.raw-value );
	values();
end method SecondsToDate;
											
			
/*
	<SysEnvRec>
*/								
											
/*define functional class <SysEnvRec> ( <statically-typed-pointer> )
end class;*/


/*
	content-size
	The size of object a <SysEnvRec> contains
*/

/*define method content-size( cls == <SysEnvRec> )
=>( result :: <integer> )
	18;
end method content-size;
*/

/*
	<SysEnvRec> accessors
*/

/*define method environsVersion (col :: <SysEnvRec>) => (result :: <integer>);
	signed-short-at(col, offset: 0);
end method;


define method machineType (col :: <SysEnvRec>) => (result :: <integer>);
	signed-short-at(col, offset: 2);
end method;


define method systemVersion (col :: <SysEnvRec>) => (result :: <integer>);
	signed-short-at(col, offset: 4);
end method;


define method processor (col :: <SysEnvRec>) => (result :: <integer>);
	signed-short-at(col, offset: 6);
end method;


define method hasFPU (col :: <SysEnvRec>) => (result :: <boolean>);
	let res = signed-byte-at(col, offset: 8);
	if (res = 0)
		#f;
	else
		#t;
	end if;
end method;


define method hasColorQD (col :: <SysEnvRec>) => (result :: <boolean>);
	let res = signed-byte-at(col, offset: 9);
	if (res = 0)
		#f;
	else
		#t;
	end if;
end method;
*/

/*
	SysEnvirons
*/

/*define method SysEnvirons( versionRequested :: <integer>, rec :: <SysEnvRec> )
=> ( result :: <OSErr> )
	call-out( "SysEnvirons", int:, int: versionRequested, ptr: rec.raw-value );
end method SysEnvirons;*/
