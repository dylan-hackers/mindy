all: gtk-2.lib.du

gtk.dylan: gtk.intr gtk-includes.h
	melange --no-struct-accessors --d2c -I`gcc --print-file-name=include` `pkg-config --cflags gtk+-2.0` -m gtk-module.dylan, gtk.intr gtk.dylan,
	mv gtk-module.dylan, gtk-module.dylan
	mv gtk.dylan, gtk.dylan

gtk-2.lib.du: gtk.lid gtk-library.dylan gtk-module.dylan gtk.dylan gtk-more-module.dylan gtk-support.dylan
	CCOPTS=" `pkg-config --cflags gtk+-2.0`" d2c gtk.lid
