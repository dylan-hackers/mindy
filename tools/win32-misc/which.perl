#!perl

# This version of which will also print out the paths to all
# executables that are shadowed (the real one comes first, followed by
# things that are shadowed).  I call this a feature, although your
# millage may vary.

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
	if ($command . ".bat" eq $file) {
	    $bat = "$dir\\$file";
	} elsif ($command . ".com" eq $file) {
	    $com = "$dir\\$file";
	} elsif ($command . ".exe" eq $file) {
	    $exe = "$dir\\$file";
	}
    }
    if ($bat) { print $bat, "\n"; }
    if ($com) { print $com, "\n"; }
    if ($exe) { print $exe, "\n"; }
}

@Paths = split(';', ".;" . $ENV{'PATH'});
$name = shift(@ARGV);
foreach $path (@Paths) {
    &search_dir($path, $name);
}
