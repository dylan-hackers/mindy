#!perl

# rcsupdate for NT.  Primary problem: No symbolic links in NTFS.
# Currently, this doesn't do any kind of caching like the Unix version
# does, because speed doesn't really seem to be a problem.  (It's
# slower, but not by enough to really get worked up about) Also goes
# purely on timestamps, and not on rcsdiff.

sub update {
    local($localfile, $rcsfile) = @_;
    if (-w $localfile) {
	print "  $localfile is writeable, hence ignoring.\n";
    } else {
	local($should_update) = ! (-e $localfile);
	if (! $should_update) {
	    local($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime,
		  $mtime, $ctime, $blksize, $blocks) 
		= stat($localfile);
	    local($rcs_dev, $rcs_ino, $rcs_mode, $rcs_nlink, $rcs_uid, 
		  $rcs_gid, $rcs_rdev, $rcs_size, $rcs_atime,
		  $rcs_mtime, $rcs_ctime, $rcs_blksize, $rcs_blocks) 
		= stat($rcsfile);
	    $should_update = $mtime < $rcs_mtime;
	}

	if ($should_update) {
	    print "  Updating $localfile\n";
	    system("$co -q $localfile $rcsfile")
		&& print "co failed!\n";
	}
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

sub update_rcs_files {
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
    foreach $file (@allfiles) {
        if ($file =~ /,v$/) {     # file ends in ",v"
            local($localfile) = "$localdir/$file";
            chop $localfile;   chop $localfile;   # remove ,v
            &update($localfile, "$rcsdir/$file");
        }
    }
}

# Takes a directory name, and updates all files in and below that dir.
#
sub update_directory {
    local($dirname) = @_;
    print "Updating $dirname:\n";
    &update_rcs_files($dirname);
    opendir(DIR, $dirname) || die("Couldn't open $dirname.");
    local(@allfiles) = readdir(DIR);
    closedir(DIR);
    local($file);
    foreach $file (@allfiles) {
        local($subdir) = "$dirname/$file";
        if (-d $subdir && $file ne "RCS" && $file ne "." && $file ne "..") {
            &update_directory($subdir);
        }
    }
}

$co = shift(@ARGV);
&update_directory(".");
