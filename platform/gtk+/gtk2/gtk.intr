module: gtk-internal

define interface
  #include "gtk/gtk.h",
    import: all-recursive,
    exclude: {
	"g_scanner_cur_value", // returns a union of size 8
	"glib_dummy_decl",
	"g_thread_init",
	"_g_param_type_register_static_constant",
	"g_thread_init_with_errorcheck_mutexes",
	"_gtk_container_clear_resize_widgets"
	},
    equate: {"gchar*" => <c-string>,
             "char*" => <c-string>},
    map: {"gchar*" => <byte-string>,
          "char*" => <byte-string>,
          "GCallback" => <function> },
    rename: {"gtk_init" => %gtk-init },
    name-mapper: minimal-name-mapping;

  pointer "char**" => <c-string-vector>,
    superclasses: {<c-vector>};

  struct "struct _GtkAccelGroup",
    superclasses: {<GObject>};
  struct "struct _GtkAccelLabel",
    superclasses: {<GtkLabel>, <AtkImplementorIface>};
  struct "struct _GtkAccessible",
    superclasses: {<AtkObject>};
  struct "struct _GtkAdjustment",
    superclasses: {<GtkObject>};
  struct "struct _GtkAlignment",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkArrow",
    superclasses: {<GtkMisc>, <AtkImplementorIface>};
  struct "struct _GtkAspectFrame",
    superclasses: {<GtkFrame>, <AtkImplementorIface>};
  struct "struct _GtkBin",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkBox",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkButtonBox",
    superclasses: {<GtkBox>, <AtkImplementorIface>};
  struct "struct _GtkButton",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkCalendar",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkCellRenderer",
    superclasses: {<GtkObject>};
  struct "struct _GtkCellRendererPixbuf",
    superclasses: {<GtkCellRenderer>};
  struct "struct _GtkCellRendererText",
    superclasses: {<GtkCellRenderer>};
  struct "struct _GtkCellRendererToggle",
    superclasses: {<GtkCellRenderer>};
  struct "struct _GtkCheckButton",
    superclasses: {<GtkToggleButton>, <AtkImplementorIface>};
  struct "struct _GtkCheckMenuItem",
    superclasses: {<GtkMenuItem>, <AtkImplementorIface>};
  struct "struct _GtkCList",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkColorSelectionDialog",
    superclasses: {<GtkDialog>, <AtkImplementorIface>};
  struct "struct _GtkColorSelection",
    superclasses: {<GtkVBox>, <AtkImplementorIface>};
  struct "struct _GtkCombo",
    superclasses: {<GtkHBox>, <AtkImplementorIface>};
  struct "struct _GtkContainer",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkCTree",
    superclasses: {<GtkCList>, <AtkImplementorIface>};
  struct "struct _GtkCurve",
    superclasses: {<GtkDrawingArea>, <AtkImplementorIface>};
  struct "struct _GtkDialog",
    superclasses: {<GtkWindow>, <AtkImplementorIface>};
  struct "struct _GtkDrawingArea",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkEntry",
    superclasses: {<GtkWidget>, <AtkImplementorIface>, <GtkEditable>, <GtkCellEditable>};
  struct "struct _GtkEventBox",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkFileSelection",
    superclasses: {<GtkDialog>, <AtkImplementorIface>};
  struct "struct _GtkFixed",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkFontSelectionDialog",
    superclasses: {<GtkDialog>, <AtkImplementorIface>};
  struct "struct _GtkFontSelection",
    superclasses: {<GtkVBox>, <AtkImplementorIface>};
  struct "struct _GtkFrame",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkGammaCurve",
    superclasses: {<GtkVBox>, <AtkImplementorIface>};
  struct "struct _GtkHandleBox",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkHBox",
    superclasses: {<GtkBox>, <AtkImplementorIface>};
  struct "struct _GtkHButtonBox",
    superclasses: {<GtkButtonBox>, <AtkImplementorIface>};
  struct "struct _GtkHPaned",
    superclasses: {<GtkPaned>, <AtkImplementorIface>};
  struct "struct _GtkHRuler",
    superclasses: {<GtkRuler>, <AtkImplementorIface>};
  struct "struct _GtkHScale",
    superclasses: {<GtkScale>, <AtkImplementorIface>};
  struct "struct _GtkHScrollbar",
    superclasses: {<GtkScrollbar>, <AtkImplementorIface>};
  struct "struct _GtkHSeparator",
    superclasses: {<GtkSeparator>, <AtkImplementorIface>};
  struct "struct _GtkIconFactory",
    superclasses: {<GObject>};
  struct "struct _GtkIMContext",
    superclasses: {<GObject>};
  struct "struct _GtkIMContextSimple",
    superclasses: {<GtkIMContext>};
  struct "struct _GtkIMMulticontext",
    superclasses: {<GtkIMContext>};
  struct "struct _GtkImage",
    superclasses: {<GtkMisc>, <AtkImplementorIface>};
  struct "struct _GtkImageMenuItem",
    superclasses: {<GtkMenuItem>, <AtkImplementorIface>};
  struct "struct _GtkInputDialog",
    superclasses: {<GtkDialog>, <AtkImplementorIface>};
  struct "struct _GtkInvisible",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkItemFactory",
    superclasses: {<GtkObject>};
  struct "struct _GtkItem",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkLabel",
    superclasses: {<GtkMisc>, <AtkImplementorIface>};
  struct "struct _GtkLayout",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkList",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkListItem",
    superclasses: {<GtkItem>, <AtkImplementorIface>};
  struct "struct _GtkListStore",
    superclasses: {<GObject>, <GtkTreeModel>, <GtkTreeDragSource>, <GtkTreeDragDest>, <GtkTreeSortable>};
  struct "struct _GtkMenuBar",
    superclasses: {<GtkMenuShell>, <AtkImplementorIface>};
  struct "struct _GtkMenu",
    superclasses: {<GtkMenuShell>, <AtkImplementorIface>};
  struct "struct _GtkMenuItem",
    superclasses: {<GtkItem>, <AtkImplementorIface>};
  struct "struct _GtkMenuShell",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkMessageDialog",
    superclasses: {<GtkDialog>, <AtkImplementorIface>};
  struct "struct _GtkMisc",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkNotebook",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkObject",
    superclasses: {<GObject>};
  struct "struct _GtkOptionMenu",
    superclasses: {<GtkButton>, <AtkImplementorIface>};
  struct "struct _GtkPaned",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkPixmap",
    superclasses: {<GtkMisc>, <AtkImplementorIface>};
  struct "struct _GtkPlug",
    superclasses: {<GtkWindow>, <AtkImplementorIface>};
  struct "struct _GtkPreview",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkProgressBar",
    superclasses: {<GtkProgress>, <AtkImplementorIface>};
  struct "struct _GtkProgress",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkRadioButton",
    superclasses: {<GtkCheckButton>, <AtkImplementorIface>};
  struct "struct _GtkRadioMenuItem",
    superclasses: {<GtkCheckMenuItem>, <AtkImplementorIface>};
  struct "struct _GtkRange",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkRcStyle",
    superclasses: {<GObject>};
  struct "struct _GtkRuler",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkScale",
    superclasses: {<GtkRange>, <AtkImplementorIface>};
  struct "struct _GtkScrollbar",
    superclasses: {<GtkRange>, <AtkImplementorIface>};
  struct "struct _GtkScrolledWindow",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkSeparator",
    superclasses: {<GtkWidget>, <AtkImplementorIface>};
  struct "struct _GtkSeparatorMenuItem",
    superclasses: {<GtkMenuItem>, <AtkImplementorIface>};
  struct "struct _GtkSettings",
    superclasses: {<GObject>};
  struct "struct _GtkSizeGroup",
    superclasses: {<GObject>};
  struct "struct _GtkSocket",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkSpinButton",
    superclasses: {<GtkEntry>, <AtkImplementorIface>, <GtkEditable>, <GtkCellEditable>};
  struct "struct _GtkStatusbar",
    superclasses: {<GtkHBox>, <AtkImplementorIface>};
  struct "struct _GtkStyle",
    superclasses: {<GObject>};
  struct "struct _GtkTable",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkTearoffMenuItem",
    superclasses: {<GtkMenuItem>, <AtkImplementorIface>};
  struct "struct _GtkTextBuffer",
    superclasses: {<GObject>};
  struct "struct _GtkTextChildAnchor",
    superclasses: {<GObject>};
  struct "struct _GtkTextMark",
    superclasses: {<GObject>};
  struct "struct _GtkTextTag",
    superclasses: {<GObject>};
  struct "struct _GtkTextTagTable",
    superclasses: {<GObject>};
  struct "struct _GtkTextView",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkTipsQuery",
    superclasses: {<GtkLabel>, <AtkImplementorIface>};
  struct "struct _GtkToggleButton",
    superclasses: {<GtkButton>, <AtkImplementorIface>};
  struct "struct _GtkToolbar",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkTooltips",
    superclasses: {<GtkObject>};
  struct "struct _GtkTreeModelSort",
    superclasses: {<GObject>, <GtkTreeModel>, <GtkTreeSortable>};
  struct "struct _GtkTreeSelection",
    superclasses: {<GObject>};
  struct "struct _GtkTreeStore",
    superclasses: {<GObject>, <GtkTreeModel>, <GtkTreeDragSource>, <GtkTreeDragDest>, <GtkTreeSortable>};
  struct "struct _GtkTreeViewColumn",
    superclasses: {<GtkObject>};
  struct "struct _GtkTreeView",
    superclasses: {<GtkContainer>, <AtkImplementorIface>};
  struct "struct _GtkVBox",
    superclasses: {<GtkBox>, <AtkImplementorIface>};
  struct "struct _GtkVButtonBox",
    superclasses: {<GtkButtonBox>, <AtkImplementorIface>};
  struct "struct _GtkViewport",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkVPaned",
    superclasses: {<GtkPaned>, <AtkImplementorIface>};
  struct "struct _GtkVRuler",
    superclasses: {<GtkRuler>, <AtkImplementorIface>};
  struct "struct _GtkVScale",
    superclasses: {<GtkScale>, <AtkImplementorIface>};
  struct "struct _GtkVScrollbar",
    superclasses: {<GtkScrollbar>, <AtkImplementorIface>};
  struct "struct _GtkVSeparator",
    superclasses: {<GtkSeparator>, <AtkImplementorIface>};
  struct "struct _GtkWidget",
    superclasses: {<GtkObject>, <AtkImplementorIface>};
  struct "struct _GtkWindow",
    superclasses: {<GtkBin>, <AtkImplementorIface>};
  struct "struct _GtkWindowGroup",
    superclasses: {<GObject>};
end interface;

