#!perl

sub examine_file {
    local ($file) = @_;
    open(DYLAN_FILE, "<$file");
    local ($has_rcsheader) = 0;
    while (<DYLAN_FILE>) { 
	$has_rcsheader = $has_rcsheader || /\$Header(\$|\:)/;
    }
    close(DYLAN_FILE);
    if (!$has_rcsheader) {
	print "$file\n";
    }
}

sub examine_one_directory {
    local($localdir) = @_;
    if (!opendir(DIR, $localdir)) {
	print "-> Couldn't open dir $localdir\n";
	return;
    }
    local(@allfiles) = readdir(DIR);
    closedir(DIR);
    local($file);
    foreach $file (@allfiles) {
        if ($file =~ /\.dylan$/) { # file ends in ".dylan"
	    &examine_file("$localdir/$file");
        }
    }
}

# Takes a directory name, and examines all files in and below that dir.
#
sub examine_directory {
    local($dirname) = @_;
    &examine_one_directory($dirname);
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

if ($#ARGV != 0) {
    die("Usage: find-dylan-files dirname");
}

&examine_directory($ARGV[0]);
