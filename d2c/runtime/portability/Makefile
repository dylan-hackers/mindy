portability.lib.du: portability.lid portability.dylan portability-exports.dylan
	d2c portability-`uname`.lid

clean:
	-rm *.o *.s *.a *.c *.mak *~ portability.lib.du

install: portability.lib.du 
	/usr/bin/install -c libportability.a portability.lib.du `d2c --dylan-user-location`
