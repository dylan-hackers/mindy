module: dylan-user

define library io
  use streams, export: {streams};
  use print, export: {print};
  use format, export: {format};
  use standard-io, export: {standard-io};
  use format-out, export: {format-out};
end library;
