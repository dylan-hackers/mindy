module: dylan-user

define library portability
  use common-dylan;

  export portability;
end library;

define module portability
  use common-dylan;
  use system, import: {getenv};  // win32 only
  // use regular-expressions;       // win32 only			  
  export
    $default-defines,
    $enum-size,
    $pointer-size, $function-pointer-size,
    $integer-size, $short-int-size,
    $long-int-size, $char-size,
    $longlong-int-size,
    $float-size, $double-float-size,
    $long-double-size;
end module portability;
