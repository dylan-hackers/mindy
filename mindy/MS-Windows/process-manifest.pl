#!/usr/local/bin/perl

# Removes the following files:
#      Released/ and anything under it
#      Debug/ and anything under it
#      libraries/*.dbc
#      prepare-for-vc.bat
#      process-manifest.pl (this file)

while (<>) {
    if (!m|Released| && !m|Debug| && !m|libraries/.*\.dbc|
        && !m|prepare-for-vc.bat| && !m|process-manifest.pl|) {
	print $_;
    }
}
