#!perl

# A fake autoconf utility.  Creates Makefile's out of Makefile.in's;
# finds the Makefile.in's by looking in configure.in and
# distribution-options.

sub get_makefile_dot_ins {
    print "Reading configure.in\n";
    open(CONFIGURE, "configure.in") || die("Can't find configure.in");
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
    local($files) = join(' ', @Makefiles);
    $files =~ s/define\(BUNDLED_LIBRARY_MAKEFILES,//;
    $files =~ s/AC_OUTPUT\(//;
    $files =~ s/BUNDLED_LIBRARY_MAKEFILES//;
    $files =~ tr/)//d;
    $files =~ tr/\\//d;
    local($file2) = join(" ", split('\s+', $files));
    return $file2;
}

sub do_makefile {
    local($destname) = @_;
    local($srcname) = $destname . ".in";
    print "Generating $destname\n";
    open(SRC, $srcname) || die("Can't open $srcname");
    open(DEST, ">$destname") || die("Can't write to $destname");
    while (<SRC>) {
	s|\tfor (\w+) in (\$.SUBDIRS.); do \(cd \$\$\1; (\$.MAKE.+)\);\s*done|\tfor %9 in (\2) do \@cmd /c "cd %9 && \3"|;
	s|\$\{MAKE\}|\$\(MAKE\) /nologo|g;

	s|^\tcat\s+(.*)>(.*)$|\tdbclink \2 \1|;

	if (/^MC\s*=\s*/) {
	    tr|/|\\|;
	}
	s/\.o/\.obj/g;
	s|\$\{AR\} ru libcompat.a|lib /out:compat.lib|;
	s|^\t\$\{RANLIB\} libcompat.a||;
	s/lib(\w+)\.a/\1\.lib/g;
	s|\t\$\{CC\} \$\{OBJS\} \$\{LIBS\} -o ,(\w+)\s*$|\tlink /subsystem:console /out:\1.exe \$(OBJS) \$(LIBS)\n|;
	if (/\tmv -f ,(\w+) \1/) {
	    break;        # Skip this line entirely
	} 

        s/\@prefix\@/./;
	s/\@srcdir\@/./;

	# exec_prefix -- where to install to.
	local($GwydionDir) = $ENV{'GwydionDir'};
	s|\@exec_prefix\@|$GwydionDir|;

	s/\@CC\@/cl/;
	s/\@CCOPTS\@/-O2/;
	s|\@DEFS\@|/nologo /W3 /GX /O2 /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c|;

# ext-init.c needs to be compiled without optimizations -- /Od instead of /O2
	s|(\t\$\{CC\} -c )\$\{CFLAGS\}(.*)(ext-init\.c)\s*$|\1/nologo /W3 /GX /Od /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "NO_BSTRING_H" /D "USE_DIRENT2_H" /D "NO_UNISTD_H" /D "NO_SYS_TIME_H" /D "NO_SYS_WAIT_H" /D "NO_SIGACTION" /D "NO_FD_SET" /D "NO_PWD_H" /D "NO_SYS_PARAM_H" /D "NO_SYS_FILE_H" /YX /c\2\3|;

	s/\@YACC\@/bison/;
	s/\@LEX\@/flex/;
	s/\@LFLAGS\@//;
	s/\@RANLIB\@//;
	s/\@INSTALL\@/dbccopy/;
	s/\@INSTALL_PROGRAM\@/dbccopy/;
	s/\@INSTALL_DATA\@/dbccopy/;

	s/\@LIBOBJS\@/sigaction.obj rint.obj/;
	s/\@LIBS\@//;
	s/\@BUNDLED_LIBRARY_DIRS\@/$bundled_library_dirs/;

	s/{(\w+)}/(\1)/g;
	s/{(\w+)-(\w+)}/(\1_\2)/g;
	s/{(\w+)-(\w+)-(\w+)}/(\1_\2_\3)/g;

	s/\((\w+)-(\w+)\)/\(\1_\2\)/g;
	s/^(\w+)-(\w+)(\s+)=/\1_\2\3=/g;

	s/;\s*$//;   # Delete ; at end of line
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
