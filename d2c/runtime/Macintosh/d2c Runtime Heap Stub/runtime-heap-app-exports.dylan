Module: dylan-user

define library runtime-heap-app

	use melange-support;				// melange-support


	use garbage-collection;				// garbage-collection


	use Dylan;							// Dylan
    
    
    use collection-extensions;			// coll-ext
    
    
    use format;							// Format
    
    
    use Format-Out;						// Format-out
    
    
    use parse-arguments;				// GetOpt
    
    
    use Matrix;							// Matrix
    
    
    use print;							// print
    
    
    use regular-expressions;			// regexp
    
    
    use Standard-IO;					// Standard-IO
    
    
    use stream-extensions;				// stream-ext
    
    
    use streams;						// streams
    
    
    use string-extensions;				// string-ext
    
    
    use Table-Extensions;				// table-ext
    
    
    use Transcendental;					// transcendental
    
    
    use random;							// random

end library runtime-heap-app;


define module runtime-heap-app

  	use melange-support;				// melange-support

  
  	use garbage-collection;				// garbage-collection

  
	use Dylan;							// Dylan
	use Extensions;
	use Cheap-IO;
	use System;
    use Introspection;
    use Magic;
    use %Hash-Tables;


	use self-organizing-list;			// coll-ext
	use subseq;
	use vector-search;
	/*use heap;*/
	//use SDE-vector;
	use sequence-diff;
	use Sequence-Utilities;


	//imported in cheap-io: use format;							// format
	
	use Format-Out;						// Format-Out
	
	use option-parser-protocol;			// GetOpt
	use parse-arguments;
	
	
	use Matrix;							// Matrix
	
	
	//imported in cheap-io: use print;							// print
	use pprint;
	
	
	use regular-expressions;			// regular-expressions
	
	
	use Standard-IO;					// Standard-IO
	
	
	use stream-extensions;				// stream-extentions
	use indenting-streams;
	use concatenated-streams;
	
	
	use streams;						// streams
	use piped-exec;
	
	
	use string-conversions;				// string-extentions
	use character-type;
	use string-hacking;
	use substring-search;
	use %parse-string;
	use %do-replacement;
	
	
	use Table-Extensions;				// table-ext
    
    
    use Transcendental;					// transcendental
	
	
	use random;							// random
	
end module runtime-heap-app;
