module: params

define constant $fixed-integer-bits = 32;

define constant $minimum-fixed-integer
  = ash(as(<extended-integer>, -1),
	$fixed-integer-bits - 1);

define constant $maximum-fixed-integer
  = lognot($minimum-fixed-integer);

