#!perl

# supports -name, -print, and -exec

$num_args{"-print"} = 0;
$num_args{"-name"} = 1;
$num_args{"-exec"} = -1;

sub run_filter {
    local($filename, $filtname, $filtarg) = @_;
#    print "  runfilter $filename $filtname $filtarg\n";
    if ($filtname eq "-name") {
	local($regexp) = $filtarg;
	$regexp =~ s/\./\\./g;
	$regexp =~ s/\*/\.\*/g;
#	print "regexp is $regexp\n";
	return ($filename =~ /^$regexp$/);  ### regexp syntaxes differ...
    } elsif ($filtname eq "-print") {
	print "$filename\n";
	return 1;
    } elsif ($filtname eq "-exec") {
	local($cmd) = $filtarg;
	$cmd =~ s/\{\}/$filename/g;
	return system($cmd);
    } else {
	die "Unknown filter $filtname";
    }
}

sub examine_file {
    local($file) = @_;
    local($i);

#    print "Examining $file\n";

  endloop:
    for ($i=0; $i<=$#filter; $i++) {
	if (! &run_filter($file, $filter[$i], $filter_arg[$i])) {
	    last endloop;
	}
    }
}
    

sub examine_one_directory {
    local($localdir) = @_;
#    print "Examining $localdir\n";
    if (!opendir(DIR, $localdir)) {
	print "-> Couldn't open dir $localdir\n";
	return;
    }
    local(@allfiles) = readdir(DIR);
    closedir(DIR);
    local($file);
    foreach $file (@allfiles) {
        if (-d $file || $file eq "." || $file eq "..") {
#	    print "  Not good enough: $file\n";
	} else {
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
        if (-d $subdir && $file ne "." && $file ne "..") {
            &examine_directory($subdir);
        }
    }
}

$startdir = shift(@ARGV);
for ($i=0; $#ARGV >= 0; $i++) {
    $filter[$i] = shift(@ARGV);
    if ($num_args{$filter[$i]} == 0) {
	$filter_arg[$i] = "";
    } elsif ($num_args{$filter[$i]} == 1) {
	$filter_arg[$i] = shift(@ARGV);
    } elsif ($num_args{$filter[$i]} == -1) {
	$filter_arg[$i] = "";
	local($foo);
	while ( ($foo = shift(@ARGV)) ne ';') {
	    $filter_arg[$i] .= " $foo";
	}
    }
}

#print "$#filter\n";
&examine_directory($startdir);
