Module: glib
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

c-include("glib.h");

define constant <gchar> = <integer>;
define constant <guchar> = <integer>;
define constant <gint> = <integer>;
define constant <guint> = <integer>;
define constant <gboolean> = <gint>;
define constant <glong> = <integer>;
define constant <gulong> = <integer>;
define constant <gfloat> = <single-float>;
define constant <gdouble> = <double-float>;
define constant <gchar*> = <c-string>;

define open primary functional class <indexable-statically-typed-pointer>
    (<statically-typed-pointer>, <mutable-collection>)
end class;

define constant <gpointer> = <statically-typed-pointer>;

define functional class <gchar**> (<indexable-statically-typed-pointer>) end;
define functional class <gint*> (<indexable-statically-typed-pointer>) end;
define functional class <guint*> (<indexable-statically-typed-pointer>) end;
define functional class <gulong*> (<indexable-statically-typed-pointer>) end;
define functional class <gfloat*> (<indexable-statically-typed-pointer>) end;

define functional class <GList*> (<statically-typed-pointer>) end;

define method data-value
    (obj :: <GList*>)
 => (value :: <gpointer>);
  make(<gpointer>,
       pointer: c-struct-field(ptr:, obj.raw-value,
                               "GList", "data"));
end;

define method next-value
    (obj :: <GList*>)
 => (value :: <GList*>);
  make(<GList*>,
       pointer: c-struct-field(ptr:, obj.raw-value,
                               "GList", "next"));
end;

define method prev-value
    (obj :: <GList*>)
 => (value :: <GList*>);
  make(<GList*>,
       pointer: c-struct-field(ptr:, obj.raw-value,
                               "GList", "prev"));
end;

define functional class <GSList*> (<indexable-statically-typed-pointer>) end;

