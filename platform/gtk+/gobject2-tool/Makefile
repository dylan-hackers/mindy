../gtk+/gtk.intr: gobject2-tool
	./gobject2-tool > ../gtk2/gtk.intr

gobject2-tool: gobject2-tool.c
	gcc -o gobject2-tool gobject2-tool.c `pkg-config --cflags --libs gtk+-2.0`
