all: ansi-c-test

.PHONY: clean

clean:
	rm -f *.c *.s *.mak *.a *.du *.o
	rm -f ansi-c-test
	rm -f core

ansi-c.lid: ansi-c-exports.dylan c-types.dylan c-declarations.dylan
	touch $@

ansi-c-test.lid: ansi-c-test-exports.dylan ansi-c-test.dylan
	touch $@

ansi-c.lib.du: ansi-c.lid
	d2c $<

ansi-c-test: ansi-c-test.lid ansi-c.lib.du
	d2c $<

