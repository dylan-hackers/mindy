module: hello-world
use-libraries: dylan, common-dylan, io, gtk-2
use-modules: common-dylan, system, streams, standard-io, format-out, gtk

define constant hello = callback-method
 (widget :: <GtkWidget>, data :: <raw-pointer>) => ();
  format-out ("Hello, world!\n");
  force-output(*standard-output*);
end;

define method delete-event(widget :: <GtkWidget>, event :: <GdkEvent>, data)
  => (deny-deletion? :: <boolean>)
  format-out ("Delete event occurred\n");
  #t
end method delete-event;

define method destroy(widget :: <GtkWidget>, data) => ()
  gtk-main-quit ()
end method destroy;

begin
  gtk-init(application-name(), application-arguments());
  
  let window = gtk-window-new ($GTK-WINDOW-TOPLEVEL);
//  g-signal-connect (window, "delete_event", delete-event, #f);
//  g-signal-connect (window, "destroy", destroy, #f);
  gtk-container-set-border-width (as(<GtkContainer>, window), 10);

  let button = gtk-button-new-with-label ("Hello, world!");
  g-signal-connect (button, "clicked", hello, #f);
//  g-signal-connect-swapped (button, "clicked", gtk-widget-destroy, window);

  gtk-container-add (as(<GtkContainer>, window), button);

  gtk-widget-show (button);
  gtk-widget-show (window);

  gtk-main();
end;
