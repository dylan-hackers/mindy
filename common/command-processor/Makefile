command-processor.lib.du: command-processor.lid command-processor.dylan command-processor-exports.dylan termios.dylan
	d2c command-processor.lid

termios.dylan: termios.intr
	melange --d2c termios.intr > termios.dylan, && mv termios.dylan, termios.dylan

clean:
	-rm -f *.o *.s *.a *.c *.mak *~ command-processor.lib.du
	-rm -rf .libs

install: command-processor.lib.du 
	libtool /usr/bin/install -c libcommand-processor.a command-processor.lib.du `d2c --dylan-user-location`
