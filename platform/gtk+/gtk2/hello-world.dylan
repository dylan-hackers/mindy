module: hello-world
use-libraries: dylan, common-dylan, io, gtk-2
use-modules: common-dylan, streams, standard-io, format-out, gtk

define method hello(widget :: <GtkWidget>)
  format-out("Hello, World, %=!\n", widget);
  force-output(*standard-output*);
  let dialog = 
    gtk-message-dialog-new(gtk-widget-get-parent(widget), 
                           $GTK-DIALOG-DESTROY-WITH-PARENT,
                           $GTK-MESSAGE-WARNING, 
                           $GTK-BUTTONS-YES-NO,
                           "foo!");
  g-signal-connect(dialog, "response", 
                   method(widget, data)
                       format-out ("Response event occurred: %= got %=\n", 
                                   widget, data);
                       force-output(*standard-output*);
                       gtk-widget-destroy(widget);
                   end method);
  gtk-widget-show(dialog);
end method hello;

define method delete-event(widget :: <GtkWidget>, event :: <GdkEvent>)
  => (deny-deletion? :: <boolean>)
  format-out ("Delete event occurred: %= got %=\n", widget, event);
  force-output(*standard-output*);
  #t
end method delete-event;

define method destroy-event(widget :: <GtkWidget>) => ()
  format-out ("Destroy event occurred: %=\n", widget);
  force-output(*standard-output*);
  gtk-main-quit ()
end method destroy-event;

begin
  gtk-init(application-name(), application-arguments());
  
  let window = gtk-window-new ($GTK-WINDOW-TOPLEVEL);
  g-signal-connect (window, "delete_event", delete-event);
  g-signal-connect (window, "destroy", destroy-event);
  gtk-container-set-border-width (window, 10);

  let button = gtk-button-new-with-label ("Hello, world!");
  g-signal-connect (button, "clicked", hello);
/*
  g-signal-connect (button, "clicked", method(#rest args) 
                                           gtk-widget-destroy(window);
                                       end method);
*/
  gtk-container-add (window, button);

  gtk-widget-show (button);
  gtk-widget-show (window);

  gtk-main();
end;
