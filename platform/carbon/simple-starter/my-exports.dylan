module: dylan-user

define library my-simple

	use dylan;
	use carbon;
	use format;
	use simple;
	
end library my-simple;

define module my-simple

	use dylan;
	use extensions;
	
	use mac-types;
        use dialogs;
	use events;
        use memory;
	use quickdraw;
	use windows;
	
	use simple;
	
end module my-simple;
