module: network-test

define interface
  #include "sys/time.h",
    import: { "struct timeval", "gettimeofday", "struct timezone"},
    name-mapper: c-to-dylan;
end interface;