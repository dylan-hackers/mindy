export CCFLAGS = -ObjC

cocoa-test: cocoa-test.lid cocoa-test.dylan cocoa-test-exports.dylan
	d2c -L../cocoa cocoa-test.lid
	../tools/compose-cocoa-app cocoa-test

clean:
	-rm -f *.o *.s *.a *.c *.mak  *~ cocoa-test
	-rm -rf *.app *.app.old .libs
