Module: gdk-extra
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define constant <GdkAtom> = <integer>;

define functional class <GdkModifierType*>
    (<indexable-statically-typed-pointer>)
end;
define sealed domain make(singleton(<GdkModifierType*>));

define functional class <GdkWindow**>
    (<indexable-statically-typed-pointer>)
end;
define sealed domain make(singleton(<GdkWindow**>));

define functional class <GdkGC**> (<indexable-statically-typed-pointer>) end;
define sealed domain make(singleton(<GdkGC**>));
