module: c-parser

define class <c-platform> (<object>)
  // Useful defaults.
  slot c-platform-default-defines,
    required-init-keyword: default-defines:;
  slot c-platform-default-include-path,
    required-init-keyword: default-include-path:;
end class;

define constant $generic-platform =
  make(<c-platform>,
       default-defines:
	 #["const", "",
	   "volatile", "",
	   "__STDC__", "1"],
       default-include-path:
	 #["/usr/include"]);
       
define constant $i386-linux-platform =
  make(<c-platform>,
       default-defines:
         #["const", "",
	   "volatile", "",
           "__STDC__", "",
           
           // The following six declarations should be removed someday, as
           // soon as we fix a bug in MINDY.
           // XXX - I have no idea what the above comment refers to. Check the
           // Melange changelogs.
	   //"__GNUC__", "2",
	   //"__GNUC_MINPR__", "7",
	   //"__signed__", "",
	   //"__const", "",
	   //"__CONSTVALUE", "",
	   //"__CONSTVALUE2", "",
	   
	   // Parameterized macros which remove various GCC extensions from our
	   // source code. The last item in the list is the right-hand side of
	   // the define; all the items preceding it are named parameters.
	   "__attribute__", #(#("x"), ""),
	   "__signed__", "", 
           "__signed", "",
	   "__inline__", "",
	   "inline", "",
	   "__inline", "",
	   
	   "__ELF__", "",
	   "unix", "",
	   "i386", "",
	   "linux", "",
	   "__unix__", "",
	   "__i386__", "",
	   "__linux__", "",
	   "__unix", "",
	   "__i386", "",
	   "__linux", "",
	   "__builtin_va_list", "void*"],
       default-include-path:
	 #["/usr/include",
	   // XXX - We'll need heavy magic for this eventually.
	   "/usr/lib/gcc-lib/i386-redhat-linux/2.7.2.3/include"]);

define constant $ppc-linux-platform =
  make(<c-platform>,
       default-defines:
	 #["const", "",
	   "volatile", "",
	   "__STDC__", "",
	   
	   // Parameterized macros which remove various GCC extensions from our
	   // source code. The last item in the list is the right-hand side of
	   // the define; all the items preceding it are named parameters.
	   "__attribute__", #(#("x"), ""), 
	   "__signed__", "", 
	   "__signed", "", 
	   "__inline__", "",
	   "inline", "",
	   "__inline", "",
	   
	   // These are the standard platform defines. Some day, configure should
	   // figure them out by running gcc -E -o - -dM on an empty file.
	   "powerpc", "",
	   "__linux__", "",
	   "__PPC__", "",
	   "linux", "",
	   "_BIG_ENDIAN", "",
	   "PPC", "",
	   "__GNUC_MINOR__", "",
	   "__CHAR_UNSIGNED__", "",
	   "_ARCH_PPC", "",
	   "__unix", "",
	   "__unix__", "",
	   "_CALL_SYSV", "",
	   "__linux", "",
	   "__PPC", "",
	   "__ELF__", "",
	   "__BIG_ENDIAN__", "",
	   "__powerpc__", "",
	   "__powerpc", "",
	   "unix", "",
	   "__builtin_va_list", "void*"], 
       default-include-path:
	 #["/usr/include",
	   // Find out the correct value for this path by calling
	   // gcc -print-file-name=include
	   " /usr/lib/gcc-lib/ppc-redhat-linux/egcs-2.91.66/include/"]);

define constant $i386-freebsd-platform =
  make(<c-platform>,
       default-defines:
         #["const", "",
           "volatile", "",
           "__STDC__", "",

           // Parameterized macros which remove various GCC extensions from our
           // source code. The last item in the list is the right-hand side of
           // the define; all the items preceding it are named parameters.
           "__attribute__", #(#("x"), ""), 
           "__signed__", "", 
           "__signed", "", 
           "__inline__", "",
           "__inline", "",
           
           "__FreeBSD__", "4",
           "__i386__", "1",
           "__i386", "1",
           "i386", "1",
           "__unix", "1",
           "__unix__", "1",
           "__ELF__", "1",
           "unix", "1"],
       default-include-path:
	 #["/usr/include"]);

       
define constant $sparc-solaris-platform =
  make(<c-platform>,
       default-defines:
	 #["const", "",
	   "volatile", "",
	   "__STDC__", "",
	   
	   // Parameterized macros which remove various GCC extensions from our
	   // source code. The last item in the list is the right-hand side of
	   // the define; all the items preceding it are named parameters.
	   "__attribute__", #(#("x"), ""), 
	   "__signed__", "", 
	   "__signed", "", 
	   "__inline__", "",
	   "inline", "",
	   "__inline", "",
	   
	   // These are the standard platform defines. Some day, configure should
	   // figure them out by running gcc -E -o - -dM on an empty file.
	   "sun", "1",
	   "__sun", "1",
	   "__sun__", "1",
	   "__svr4__", "1",
	   "__SVR4", "1",
	   "__sparc", "1",
	   "__sparc__", "1",
	   "sparc", "1",
	   "__VERSION__", "\"3.0\"",
	   "__REGISTER_PREFIX__", "",
	   "__GCC_NEW_VARARGS__", "1",
	   "__USER_LABEL_PREFIX__", "",
	   "__SIZE_TYPE__", "unsigned int",
	   "__PTRDIFF_TYPE__", "int",
	   "__WCHAR_TYPE__", "long int",
	   "__WINT_TYPE__", "long int",
	   "__builtin_va_list", "const struct __builtin_va_list_TRICK*", // trick
	   "__HAVE_BUILTIN_SETJMP__", "1",
	   "__STDC_HOSTED__", "1",
	   "__NO_INLINE__", "1",
	   "__unix", "1",
	   "__unix__", "1",
	   "unix", "1"], 
       default-include-path:
	 #["/usr/include",
	   // Find out the correct value for this path by calling
	   // gcc -print-file-name=include
	   " /cadappl/gcc/3.0/lib/gcc-lib/sparc-sun-solaris2.7/3.0/include"]);

       
