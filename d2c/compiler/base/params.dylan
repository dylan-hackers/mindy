module: params

define constant $minimum-fixed-integer
  = ash(as(<extended-integer>, -1), 32);

define constant $maximum-fixed-integer
  = lognot($minimum-fixed-integer);

