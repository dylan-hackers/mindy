#!perl

# Usage:
#        tree-diff [-w]
#

# Tree-diff looks at the RCS'ed version of this directory, then gives
# a list of all files that are writable, that are missing from the
# current directory, or are new to the current directory (has no RCS
# equivalent).  Next to each file, it tells you which of those three
# ways the file is different -- "writeable", "deleted", or "new".

# If -w is specified, it only lists writeable files, and it doesn't
# use the word "writeable" next to them.  The -w option is
# particularly useful for constructing complex commands, like
# "tree-diff -w | mass-diff"


# found_writeable(filename) -- Generate output declaring that filename
# is writeable.
sub found_writeable {
    local($filename) = @_;
    if ($writeable_files_only) {
	print "$filename\n";
    } else {
	print "\twriteable\t$filename\n";
    }
}

# found_deleted(filename) -- Check to see if the -w switch was
# specified, and if not, generate output saying that filename was
# deleted.
sub found_deleted {
    local($filename) = @_;
    if (!$writeable_files_only) {
	print "\tdeleted\t\t$filename\n";
    }
}

# found_new(filename) -- Check to see if the -w switch was specified,
# and if not, generate output saying that filename is new.
sub found_new {
    local($filename) = @_;
    if (!$writeable_files_only) {
	print "\tnew\t\t$filename\n";
    }
}

# examine(localfile, rcsfile) -- Compare localfile to its rcsfile, and
# output the appropriate message if not.  (rcsfile must exist, but
# localfile might not, in which case the file was deleted)
sub examine {
    local($localfile, $rcsfile) = @_;
    if (-w $localfile) {
	&found_writeable($localfile);
    } elsif (! (-e $localfile)) {
	&found_deleted($localfile);
    }
}

# expand_fake_link(link_name) -- Expand our fake RCS sym-link.
sub expand_fake_link {
    local($link_name) = @_;
    open(LINKFILE, $link_name) || die("Can't find file $link_name");
    $expanded_link = <LINKFILE>;
    chop($expanded_link);
    close(LINKFILE);
    return $expanded_link;
}

# examine_rcs_files(localdir) -- Examine all files in localdir,
# generating appropriate output.  Recursively visiting subdirs is left
# up to examine_directory.
sub examine_rcs_files {
    local($localdir) = @_;
    if (! -e "$localdir/RCS") {
	return;
    }
    local($rcsdir) = &expand_fake_link("$localdir/RCS");
    if (!opendir(DIR, $rcsdir)) {
	print "-> Couldn't open dir $rcsdir\n";
	return;
    }
    local(@allfiles) = readdir(DIR);
    closedir(DIR);
    local($file);
    local(%rcsfiles);
    foreach $file (@allfiles) {
        if ($file =~ /,v$/) {     # file ends in ",v"
            local($localfile) = "$localdir/$file";
            chop $localfile;   chop $localfile;   # remove ,v
            &examine($localfile, "$rcsdir/$file");
	    $rcsfiles{$file} = 1;
        }
    }
    opendir(DIR, $localdir) || die("Couldn't open local dir $localdir");
    local(@all_local_files) = readdir(DIR);
    closedir(DIR);
    foreach $file (@all_local_files) {
	if ($file ne "RCS" && $file ne "." && $file ne ".." 
	    && -f "$localdir/$file" && (! $rcsfiles{$file . ",v"})) {
	    # if normal file, not any of RCS, ., or .., and isn't a file
	    # that we saw in the RCS directory...
	    &found_new("$localdir/$file");
	}
    }
}

# examine_directory(local) -- Examines all files in and below that
# dir, generating appropriate output.
sub examine_directory {
    local($dirname) = @_;
    &examine_rcs_files($dirname);
    opendir(DIR, $dirname) || die("Couldn't open $dirname.");
    local(@allfiles) = readdir(DIR);
    closedir(DIR);
    local($file);
    foreach $file (@allfiles) {
        local($subdir) = "$dirname/$file";
        if (-d $subdir && $file ne "RCS" && $file ne "." && $file ne "..") {
            &examine_directory($subdir);
        }
    }
}

if ($#ARGV == 0 && $ARGV[0] eq '-w') {
    $writeable_files_only = 1;
}

&examine_directory(".");
