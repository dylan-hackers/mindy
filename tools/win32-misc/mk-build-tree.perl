#!/usr/local/bin/perl

if (@ARGV) { die("usage: $0\n") }

(-e 'Defaults') || die("No Defaults file.\n");

do "./Defaults";
die("Problem loading Defaults:\n  $@\n") if $@;

if ($buildroot) {
    $root_inode = (stat($buildroot))[1];
    $dot_inode = (stat('.'))[1];
    unless ($root_inode == $dot_inode) {
	die("Defaults set \$buildroot to:\n  $buildroot\nbut that is not the same as the current directory.\n");
    }
}

($srcroot =~ /^(\w:)?\//) || die("\$srcroot is not absolute:\n  $srcroot\n");

do mk_build_tree($srcroot, '.');

sub mk_build_tree {
    local ($srcdir, $builddir) = @_;

    $srcdir =~ s/^\.\///;
    $builddir =~ s/^\.\///;

    (-d $srcdir) || die("$srcdir is not a directory.\n");
    opendir(SRCDIR, $srcdir) || die("Can't open $srcdir: $!\n");

    unless (-d $builddir) {
	(-e $builddir)
	    && die("$builddir already exists and isn't a directory.");
	print "Making $builddir\n";
	mkdir($builddir, 0777) || die("mkdir($builddir) failed: $!\n");
    }

    local (@subdirs) = ();
    $has_makegen = 0;

    while ($entry = readdir(SRCDIR)) {
	if ($entry eq '.' || $entry eq '..') {
	}
	elsif ($entry eq "Makegen") {
	    $has_makegen = 1;
	}
	else {
	    $subdir = $srcdir . '/' . $entry;

	    if (-d $subdir && ! -l $subdir) {
		push(@subdirs, $entry);
	    }
	}
    }

    closedir(SRCDIR);
    
    if ($has_makegen) {
	print "Generating makefile for $builddir\n";
	system('gen-makefile', $builddir)
	    && die("gen-makefile $builddir failed.\n");
    }

    foreach $subdir (@subdirs) {
	do mk_build_tree($srcdir.'/'.$subdir, $builddir.'/'.$subdir);
    }
}
