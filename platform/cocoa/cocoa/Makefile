default: compile

export CCFLAGS = -ObjC

flat-cocoa.h:
	cc -traditional-cpp -dD -E -Wno-four-char-constants -arch ppc -Wp,-lang-objc -P \
		-o flat-cocoa.h \
		-I/System/Library/Frameworks/AppKit.framework/Headers \
		/System/Library/Frameworks/AppKit.framework/Headers/AppKit.h
        
cocoa-wrappers.dylan:
	../tools/i2d --module cocoa-wrappers -header AppKit/Appkit.h flat-cocoa.h
	
cocoa-exports.dylan:
	make-exports cocoa-exports.input cocoa-basics.dylan cocoa-wrappers.dylan cocoa-wrappers-continued.dylan

cocoa.lib.du:
	/usr/local/bin/d2c ./cocoa.lid

compile: flat-cocoa.h cocoa-wrappers.dylan cocoa-exports.dylan cocoa.lib.du

clean: 
	-rm -f *.o *.s *.a *.c *.mak *~ 
	-rm -f flat-cocoa.h cocoa-wrappers.dylan cocoa-wrappers-continued.dylan cocoa-exports.dylan cocoa.lib.du
	-rm -rf .libs

install:
