#!/usr/bin/perl

if ($#ARGV != 0) { die("usage: $0 rcs-dir\n") }

do mk_rcs_links('', $ARGV[0], '.');

sub mk_rcs_links {
    local($prefix, $rcsdir, $srcdir) = @_;
    local(@subdirs);

    opendir(RCSDIR, $rcsdir) || die("can't look in $rcsdir: $!\n");

    $srcdir =~ s/^\.\///;

    unless (-d $srcdir) {
	(-e $srcdir) &&	die("$srcdir already exists and isn't a directory.\n");
	print STDOUT "Making $srcdir\n";
	mkdir($srcdir, 0777) || die("mkdir($srcdir) failed: $!\n");
    }

    @subdirs = ();

    while ($entry = readdir(RCSDIR)) {
	if ($entry eq '.' || $entry eq '..') {
	}
	elsif ($entry eq "RCS") {
	    do make_rcs_link($rcsdir.'/'.$entry, $srcdir.'/'.$entry);
	}
	elsif (-d ($rcsdir . '/' . $entry)) {
	    push(@subdirs, $entry);
	}
    }

    closedir(RCSDIR);

    unless ($rcsdir =~ /^\//) {
	$prefix .= '../';
    }

    foreach $subdir (@subdirs) {
	do mk_rcs_links($prefix, $rcsdir.'/'.$subdir, $srcdir.'/'.$subdir);
    }
}

sub make_rcs_link {
    local($real, $link) = @_;

    print "Opening $link, linking to $real\n";
    open(RCS, ">$link") || die("Can't open $link: $!\n");
    print RCS $real, "\n";
    close(RCS);
}
