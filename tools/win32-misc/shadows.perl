#!perl

# Tells you which commands have been shadowed.

sub search_dir {
    local($dir, $command) = @_;
    opendir(DIR, $dir) || die("Bad dir in path: $dir");
    local(@allfiles) = readdir(DIR);
    closedir(DIR);
    local($bat) = 0;
    local($com) = 0;
    local($exe) = 0;
    foreach $file (@allfiles) {
	$file =~ tr/A-Z/a-z/;
	if ($file =~ /(.*)\.(bat|com|exe)$/) {
	    local($basename) = $1; # split('.', $file);
#	    print $basename;
	    if (! $commands{$basename}) {
		$commands{$basename} = 1;
#		print "Adding $file\n";
	    } else {
#		print $commands{$basename};
		print "$dir\\$file\n";
	    }
	}
    }
}

@Paths = split(';', ".;" . $ENV{'PATH'});
$name = shift(@ARGV);
foreach $path (@Paths) {
    &search_dir($path, $name);
}
