

#include "Types.r"
#include "SysTypes.r"
#include "AEUserTermTypes.r"

resource 'aete' (0, "MindyComp Terminology") {
	0x1,
	0x0,
	english,
	roman,
	{	
		
		"Required Suite",
		"Terms that every application should supp"
		"ort",
		'reqd',
		1,
		1,
		{	
		},
		{	
		},
		{	
		},
		{	
		},
		
		"MindyComp Suite",
		"Suite pertaining to MindyComp",
		'MndC',
		1,
		1,
		{	
			
			"compile",
			"Compile a file",
			'MndC',
			'comp',
			noReply,
			"",
			replyOptional,
			singleItem,
			notEnumerated,
			notTightBindingFunction,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			'TEXT',
			"the full name of the file to compile",
			reserved,
			singleItem,
			notEnumerated,
			doesntChangeState,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			reserved,
			{	
				
				"into library",
				'libr',
				'TEXT',
				"the library for the file",
				optional,
				singleItem,
				notEnumerated,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved,
				reserved
			}
		},
		{	
		},
		{	
		},
		{	
		}
	}
};
