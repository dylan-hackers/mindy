module:  test
author: Patrick Beard
copyright: (c)1999 Patrick Beard

define method fact (n :: <integer>) => result :: <integer>;
	if (n >= 2)
		n * fact (n - 1);
	else
		1;
	end if;
end method fact;
