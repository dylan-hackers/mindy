module: Toolbox
author: Patrick Beard, maintenance by Rob Myers (yarrel@netscape.net)
copyright: (c)1999 Patrick Beard

// Rob replaced stack-alloc with thread-unsafe constants

define constant $longPtr = NewPtr( 4 );

// OSUtils.

define class <DateTimeRec> (<Ptr>) end class;

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

define constant GetDateTime =
begin
	let func = get-c-function("GetDateTime", args: list(<Ptr>),
											result: #(), file: *InterfaceLib*);
	method () => (time :: <integer>);
		//let longPtr = stack-alloc(<Ptr>, 4);
		func($longPtr);
		// build the result as an <extended-integer> since it's a large unsigned number.
		// let time = as(<extended-integer>, unsigned-short-at(longPtr));
		// time * 65536 + as(<extended-integer>, unsigned-short-at(longPtr, offset: 2));
		unsigned-long-at($longPtr);	// newer runtime promotes to <extended-integer> as needed.
	end method;
end;

define constant SecondsToDate = get-c-function("SecondsToDate", args: list(<extended-integer>, <DateTimeRec>),
											result: #(), file: *InterfaceLib*);