#!perl

# tree-diff.  Finds all writeable files, and all files that have no
# counterpart in the RCS tree.  Only for Windows/NT.

sub found_writeable {
    local($filename) = @_;
    if ($writeable_files_only) {
	print "$filename\n";
    } else {
	print "\twriteable\t$filename\n";
    }
}

sub found_deleted {
    local($filename) = @_;
    if (!$writeable_files_only) {
	print "\tdeleted\t\t$filename\n";
    }
}

sub found_new {
    local($filename) = @_;
    if (!$writeable_files_only) {
	print "\tnew\t\t$filename\n";
    }
}

sub examine {
    local($localfile, $rcsfile) = @_;
    if (-w $localfile) {
	&found_writeable($localfile);
    } elsif (! (-e $localfile)) {
	&found_deleted($localfile);
    }
}

sub expand_fake_link {
    local($link_name) = @_;
    open(LINKFILE, $link_name) || die("Can't find file $link_name");
    $expanded_link = <LINKFILE>;
    chop($expanded_link);
    close(LINKFILE);
    return $expanded_link;
}

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

# Takes a directory name, and examines all files in and below that dir.
#
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
