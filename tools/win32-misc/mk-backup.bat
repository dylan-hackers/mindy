mkdir \backup\%1
mkdir \backup\%1\bin
copy \gwydion\bin\*.* \backup\%1\bin
mkdir \backup\%1\lib
copy \gwydion\lib\*.* \backup\%1\lib
mkdir \backup\%1\etc
copy \gwydion\etc\*.* \backup\%1\etc
mkdir \backup\%1\elisp
copy \gwydion\elisp\*.* \backup\%1\elisp
