#!perl

$exe = shift(@ARGV);

while ($arg = shift(@ARGV)) {
    push(@NEWARGV, $arg);
    if ($arg !~ /^-/) {
	($path, $file) = ($arg =~ /(.*[\/\\])?([^\/\\]*)/);
	$rcsdir = $paths{$path};
	if ($rcsdir) {
	    push(@NEWARGV, $rcsdir . "/" . $file . ",v");
	} elsif (open(FILE, "${path}RCS")) {
	    $rcsdir = <FILE>;
	    chop($rcsdir);
	    close(FILE);
	    push(@NEWARGV, $rcsdir . "/" . $file . ",v");
	    $paths{$path} = $rcsdir;
	}
    }
}

system($exe, " ", join(' ', @NEWARGV));

