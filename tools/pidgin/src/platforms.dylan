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
	   "__linux", ""],
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
	   "unix", ""], 
       default-include-path:
	 #["/usr/include",
	   // Find out the correct value for this path by calling
	   // gcc -print-file-name=include
	   " /usr/lib/gcc-lib/ppc-redhat-linux/egcs-2.91.66/include/"]);
