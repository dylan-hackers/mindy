module: dylan-user

/*
	os-utils
*/

define module os-utils

	use Dylan;
	//use Extensions;						// <extended-integer>
	use melange-support;
	use mac-types;
	
	export	// OS Utils.
			GetDateTime, SecondsToDate,
			<DateTimeRec>,
			year, month, day, hour, minute, seconds, dayOfWeek;
			/*SysEnvirons,
			<SysEnvRec>,
			environsVersion, machineType, systemVersion, processor, hasFPU, hasColorQDi;*/
		
end module os-utils;








