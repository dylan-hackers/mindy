module: dir-commands

define interface
  #include { "dir-intr-impl.h" },
    import: all,
    name-mapper: minimal-name-mapping;
  function "gd_is_dir" => gd-is-dir?, map-result: <boolean>;
  function "gd_is_link" => gd-is-link?, map-result: <boolean>;
  function "gd_is_regular_file" => gd-is-regular-file?, map-result: <boolean>;
end interface;
