all: ansi-c-test

.PHONY: clean

clean:
	rm -f *.c *.s *.mak *.a *.du *.o
	rm -f ansi-c-test pidgin
	rm -f core
	rm -f *.cffi *.log gmon.out

# parser-utilities library

parser-utilities.lid: parser-utilities-exports.dylan source-locations.dylan \
		      parse-conditions.dylan multistring.dylan
	touch $@

parser-utilities.lib.du: parser-utilities.lid
	d2c $<


# ansi-c library

ansi-c.lid: ansi-c-exports.dylan c-types.dylan c-declarations.dylan \
            c-type-repository.dylan c-file.dylan
	touch $@

ansi-c.lib.du: ansi-c.lid parser-utilities.lib.du
	d2c $<

c-ffi-output.lib.du: c-ffi-output.lid
	d2c $<

# c-parser library

c-parser-engine.dylan: c-parser-engine.input	
	parsergen $< $@

c-parser.lib.du: c-parser.lid c-parser-exports.dylan \
	         c-parser-engine.dylan c-parser.dylan c-lexer.dylan \
	         c-lexer-cpp.dylan c-parser-stubs.dylan \
                 platforms.dylan includes.dylan \
	         ansi-c.lib.du parser-utilities.lib.du
	d2c $<


# ansi-c-test application

ansi-c-test.lid: ansi-c-test-exports.dylan ansi-c-test.dylan
	touch $@

ansi-c-test: ansi-c-test.lid ansi-c.lib.du c-parser.lib.du c-ffi-output.lib.du
	d2c $<

# ansi-c-test application

pidgin.lid: pidgin-exports.dylan pidgin.dylan
	touch $@

pidgin: pidgin.lid ansi-c.lib.du c-parser.lib.du
	d2c $<
