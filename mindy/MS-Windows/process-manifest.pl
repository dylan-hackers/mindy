#!/usr/local/bin/perl

# Removes the following files:
#      Released/ and anything under it
#      Debug/ and anything under it
#      libraries/*.dbc
#      prepare-for-vc.bat
#      process-manifest.pl (this file)
#      Makefile.in
#      configure, configure.in, distribution-options
#      mkinstalldirs, install.sh
#      INSTALL
#      compile.exe

while (<>) {
    if (!m|Released| && !m|Debug| && !m|libraries/.*\.dbc|
        && !m|prepare-for-vc.bat| && !m|process-manifest.pl|
	&& !m|Makefile.in|        && !m|configure|
	&& !m|configure.in|       && !m|compile.exe|
	&& !m|mkinstalldirs|      && !m|install.sh|
	&& !m|INSTALL|
	&& !m|distribution-options|) {
	print $_;
    }
}
