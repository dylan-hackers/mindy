module: tk

// Mixin for windows which contain one ore more strings which can be
// inserted and modified.
//
define abstract class <editable> (<object>) end class;

define generic delete
    (widget :: <window>, index, #key end:) => (widget :: <window>);

define method delete
    (widget :: <editable>, index, #key end: last) => (widget :: <window>);
  put-tk-line(widget, " delete ", index, " ", if (last) last else "" end if);
  widget;
end method delete;

define generic get-all
    (widget :: <editable>) => (result :: <string>);

define generic get-elements
    (widget :: <editable>, index, #key end:) => (result :: <string>);

define generic insert
    (widget :: <editable>, index, #rest elements) => (widget :: <editable>);

define method insert
    (widget :: <editable>, index, #rest elements) => (widget :: <editable>);
  put-tk-line(widget, " insert ", tk-as(<string>, index),
	       apply(concatenate,
		     map(method (str)
			   concatenate(" \"", tk-quote(str), "\"")
			 end method, elements)));
  widget;
end method insert;

define generic scan-mark
    (widget :: <window>, #rest coords) => (widget :: <window>);

define method scan-mark
    (widget :: <editable>, #rest coords) => (widget :: <window>);
  put-tk-line(widget, " scan mark ", apply(join-tk-args, coords));
  widget;
end method scan-mark;

define generic scan-dragto
    (widget :: <window>, #rest coords) => (widget :: <window>);

define method scan-dragto
    (widget :: <editable>, #rest coords) => (widget :: <window>);
  put-tk-line(widget, " scan dragto ", apply(join-tk-args, coords));
  widget;
end method scan-dragto;

define generic select-adjust
    (widget :: <window>, index) => (widget :: <window>);

define method select-adjust
    (widget :: <editable>, index) => (widget :: <window>);
  put-tk-line(widget, " select adjust ", index);
  widget;
end method select-adjust;

define generic select-clear (widget :: <window>) => (widget :: <window>);

define method select-clear (widget :: <window>) => (widget :: <window>);
  put-tk-line(widget, " select clear");
  widget;
end method select-clear;

define generic select-from
    (widget :: <window>, index) => (widget :: <window>);

define method select-from
    (widget :: <editable>, index) => (widget :: <window>);
  put-tk-line(widget, " select from ", index);
  widget;
end method select-from;

define generic select-to
    (widget :: <window>, index) => (widget :: <window>);

define method select-to
    (widget :: <editable>, index) => (widget :: <window>);
  put-tk-line(widget, " select to ", index);
  widget;
end method select-to;

