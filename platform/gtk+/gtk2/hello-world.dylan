module: hello-world
use-libraries: common-dylan, io, gtk-2
use-modules: common-dylan, format-out, gtk-2

define method hello(widget :: <GtkWidget>, data) => ()
  format-out ("Hello, world!\n");
end method hello;

define method delete-event(widget :: <GtkWidget>, event :: <GdkEvent>, data)
  => (deny-deletion? :: <boolean>)
  format-out ("Delete event occurred\n");
  #t
end method delete-event;

define method destroy(widget :: <GtkWidget>, data) => ()
  gtk-main-quit ()
end method destroy;

begin
  gtk-init(argc, argv);
  
  let window = gtk-window-new ($GTK-WINDOW-TOPLEVEL);
  g-signal-connect (window, "delete_event", delete-event, #f);
  g-signal-connect (window, "destroy", destroy, #f);
  gtk-container-set-border-width (window, 10);

  let button = gtk-button-new-with-label ("Hello, world!");
  g-signal-connect (window, "clicked", hello, #f);
  g-signal-connect-swapped (button, "clicked", gtk-widget-destroy, window);

  gtk-container-add (window, button);

  gtk-widget-show (button);
  gtk-widget-show (window);

  gtk-main();
end;
