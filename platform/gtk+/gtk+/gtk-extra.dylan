Module: gtk-extra
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define constant <GtkType> = <integer>;
define functional class <GtkType*> (<statically-typed-pointer>) end;

define functional class <GtkGC**> (<indexable-statically-typed-pointer>) end;
define sealed domain make(singleton(<GtkGC**>));

define constant <GtkFunction> = <function-pointer>;
define constant <GtkSignalFunc> = <function-pointer>;
define constant <GtkCallbackMarshal> = <function-pointer>;
define constant <GtkDestroyNotify> = <function-pointer>;
