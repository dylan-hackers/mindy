Module: Dylan-User
Copyright: Copyright 2001 Peter S. Housel
License: GNU Lesser General Public License

define library glib
  use Common-Dylan;
  use Melange-Support;
  export glib;
end library;

define module glib
  use Common-Dylan;
  use Melange-Support, exclude: { subclass };
  export
    <gint>, <guint>, <glong>, <gulong>, <gfloat>, <gdouble>, <gboolean>;
  export <indexable-statically-typed-pointer>;
  export <gpointer>, <gchar*>, <gchar**>, <gint*>, <guint*>, <gulong*>;
  export <gfloat*>;
  export <GList*>, <GSlist*>;
  export data-value, next-value, prev-value;
end module;
