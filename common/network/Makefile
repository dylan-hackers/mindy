all: network-test

sockets-api.dylan: sockets-api.intr sockets-helper.h
	melange --d2c -I`gcc --print-file-name=include` sockets-api.intr sockets-api.dylan

network.lib.du: network.lid library.dylan sockets-api.dylan Makefile
	d2c -g network.lid

network-test: network-test.lid network-test-imports.dylan network-test.dylan network.lib.du Makefile
	d2c -g network-test.lid

clean:
	-rm *.o *.lo *.c *.mak *~ *.du *.a *.la network-test