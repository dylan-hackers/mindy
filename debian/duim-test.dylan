module: duim-test
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  format-out("Hello, world!\n");
  contain(make(<push-button>, label:"Hello, world"));
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
