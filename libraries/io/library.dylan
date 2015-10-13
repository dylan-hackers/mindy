module: dylan-user

define library io
  use dylan;

  use streams, export: {streams};
  use print, export: {print, pprint};
  use format, export: {format};
  use standard-io, export: {standard-io};
  use format-out, export: {format-out};
end library;
