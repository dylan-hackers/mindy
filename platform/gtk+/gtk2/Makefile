all: gtk-2.lib.du

gtk-module.dylan: gtk.intr gtk-includes.h
	~/gd/stage2/tools/melange/melange --no-struct-accessors --d2c -I`gcc --print-file-name=include` `pkg-config --cflags gtk+-2.0` -m gtk-module.dylan, gtk.intr gtk.dylan,
	mv gtk-module.dylan, gtk-module.dylan
	mv gtk.dylan, gtk.dylan

gtk-2-internal.lib.du: gtk.lid gtk-library.dylan gtk.dylan gtk-internal-support.dylan gtk-more-module.dylan
	CCOPTS=" `pkg-config --cflags gtk+-2.0`" d2c --static -g gtk.lid

gtk-2.lib.du: gtk-support.lid gtk-support-library.dylan gtk-support.dylan gtk-2-internal.lib.du
	CCOPTS=" `pkg-config --cflags gtk+-2.0`" d2c --static -g gtk-support.lid

hello-world: hello-world.dylan gtk-2.lib.du gtk-2-internal.lib.du
	CCOPTS=" `pkg-config --cflags gtk+-2.0`" d2c -g hello-world.dylan
