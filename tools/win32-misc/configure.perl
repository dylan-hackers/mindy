#!perl

# Warning! This program is a major kluge!

# A fake autoconf utility for Mindy/NT.  Creates Makefile's out of
# Makefile.in's; finds the Makefile.in's by looking in configure.in
# and distribution-options.

# There's some code in here for massaging Makefiles for compiling C
# code.  I've never gotten the C code from Mindy/NT to compile using
# these makefiles; I've always had to use the Visual C++ project.  But
# I'm going to leave the C makefile kluges in here, in case someone
# else wants to try.


# Scan configure.in and distribution-options for any Makefile.in files
# we're supposed to turn into Makefiles
sub get_makefile_dot_ins {
    print "Reading configure.in\n";
    open(CONFIGURE, "configure.in") || die("Can't find configure.in");

    # For configure.in, look for "AC_OUTPUT(...)", which we assume
    # appears only once in the file.  Shove it all into @Makefiles, 
    # "AC_OUTPUT" and all.
    while (<CONFIGURE>) {
	if (/^AC_OUTPUT/) {      
	    chop;
	    push(@Makefiles, $_);
	    until (/\)/) {
		$_ = <CONFIGURE>;
		chop;
		push(@Makefiles, $_);
	    }
        }
        break;
    }
    close CONFIGURE;

    print "Reading distribution-options\n";
    open(CONFIGURE, "distribution-options") 
	|| die("Can't find distribution-options");

    # For distribution-options, we look for BUNDLED_LIBRARY_DIRS and
    # BUNDLED_LIBRARY_MAKEFILES.  The former we assume is exactly one
    # line.  The latter we pull the same trick as we did with
    # configure.in, saving text that isn't actually a file name.
    while (<CONFIGURE>) {
	if (/BUNDLED_LIBRARY_DIRS="(.*)"/) {
	    $bundled_library_dirs = $1;
	    print $bundled_library_dirs, "\n";
	}
	if (/^define\(BUNDLED_LIBRARY_MAKEFILES,/) {
	    chop;
	    push(@Makefiles, $_);
	    until (/\)/) {
		$_ = <CONFIGURE>;
		chop;
		push(@Makefiles, $_);
	    }
        }
        break;
    }
    close CONFIGURE;

    # Now pick out all the crap in our list of Makefiles.
    local($files) = join(' ', @Makefiles);
    $files =~ s/define\(BUNDLED_LIBRARY_MAKEFILES,//;
    $files =~ s/AC_OUTPUT\(//;
    $files =~ s/BUNDLED_LIBRARY_MAKEFILES//;
    $files =~ tr/)//d;
    $files =~ tr/\\//d;
    local($file2) = join(" ", split('\s+', $files));
    return $file2;
}

# do_makefile(dest) -- turn one Makefile.in into a Makefile.
sub do_makefile {
    local($destname) = @_;
    local($srcname) = "$destname.in";
    print "Generating $destname\n";
    open(SRC, $srcname) || die("Can't open $srcname");
    open(DEST, ">$destname") || die("Can't write to $destname");
    while (<SRC>) {
	# Recursive makes usually use for loops.  Convert Unix for
	# loops into MS-DOS for loops.  (Highly klugy)
	s|\tfor (\w+) in (\$.SUBDIRS.); do \(cd \$\$\1; (\$.MAKE.+)\);\s*done|\tfor %9 in (\2) do \@cmd /c "cd %9 && \3"|;

	# Add the /nologo flag to recursive makes
	s|\$\{MAKE\}|\$\(MAKE\) /nologo|g;

	# Use dbclink to link .dbc files
	s|^\tcat\s+(.*)>(.*)$|\tdbclink \2 \1|;

	# Change / to \ for the definition of MC
	if (/^MC\s*=\s*/) {
	    tr|/|\\|;
	}

	# Change .o to .obj
	s/\.o/\.obj/g;

	# Use "lib" instead of "ar" for a C linker, and ignore RANLIB 
	# completely 
	s|\$\{AR\} ru libcompat.a|lib /out:compat.lib|;
	s|^\t\$\{RANLIB\} libcompat.a||;

	# Change libfoo.a to foo.lib
	s/lib(\w+)\.a/\1\.lib/g;

	# Use "link" instead of "cc" to link executables
	s|\t\$\{CC\} \$\{OBJS\} \$\{LIBS\} -o ,(\w+)\s*$|\tlink /subsystem:console /out:\1.exe \$(OBJS) \$(LIBS)\n|;

	# ignore all "mv -f" commands
	if (/\tmv -f ,(\w+) \1/) {
	    break;        # Skip this line entirely
	} 

	# Change @prefix@ and @srcdir@ to .
        s/\@prefix\@/./;
	s/\@srcdir\@/./;

	# exec_prefix -- where to install to.  Use the current value
	# of the environment variable GwydionDir
	local($GwydionDir) = $ENV{'GwydionDir'};
	s|\@exec_prefix\@|$GwydionDir|;

	# Define @CC@, @CCOPTS@, and @DEFS@
	s/\@CC\@/cl/;
	s/\@CCOPTS\@/-O2/;
	s|\@DEFS\@|/nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c|;

	# A special hack just for ext-init.c, which needs to be
	# compiled without optimizations -- /Od instead of /O2
	s|(\t\$\{CC\} -c )\$\{CFLAGS\}(.*)(ext-init\.c)\s*$|\1/nologo /W3 /GX /Od /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c\2\3|;

	# Give standard values for @YACC@, @LEX@, and @LFLAGS@
	s/\@YACC\@/bison/;
	s/\@LEX\@/flex/;
	s/\@LFLAGS\@//;

	# Ignore @RANLIB@
	s/\@RANLIB\@//;

	# Use dbccopy to install all files (including those that
	# aren't .dbc files).  dbccopy takes care of all the
	# slash/backslash/drive letter nonsense.
	s/\@INSTALL\@/dbccopy/;
	s/\@INSTALL_PROGRAM\@/dbccopy/;
	s/\@INSTALL_DATA\@/dbccopy/;

	# Add some LIBOBJS
	s/\@LIBOBJS\@/sigaction.obj rint.obj/;
	s/\@LIBS\@//;
	s/\@BUNDLED_LIBRARY_DIRS\@/$bundled_library_dirs/;

	# nmake doesn't like dashes (-) in variable names, nor does it
	# like {} when you could use ().  Handle the most common cases
	# -- zero, one, or two dashes.
	s/{(\w+)}/(\1)/g;
	s/{(\w+)-(\w+)}/(\1_\2)/g;
	s/{(\w+)-(\w+)-(\w+)}/(\1_\2_\3)/g;

	# Like above but the {}'s have already been handled
	s/\((\w+)-(\w+)\)/\(\1_\2\)/g;
	s/^(\w+)-(\w+)(\s+)=/\1_\2\3=/g;

	# Delete ; at end of line
	s/;\s*$//;

	# replace "cd dir; dosomething" with NT-style "cd dir &&
	# dosomething && cd .." (assumes dir only moves us one level)
	s/^\tcd (\w+);(.*)/\tcd \1 && \2 && cd ../;

	print DEST $_;
    }
    close(SRC);
    close(DEST);
}


$files = &get_makefile_dot_ins();
@Files = split(' ', $files);
foreach $makefile (@Files) {
    &do_makefile($makefile);
}
