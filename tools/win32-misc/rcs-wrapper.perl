#!perl

$exe = shift(@ARGV);

open(FILE, "RCS") || die("Can't find file RCS");
$rcsdir = <FILE>;
chop($rcsdir);
close(FILE);

while ($arg = shift(@ARGV)) {
    if ($arg =~ /^-/) {
        push(@NEWARGV, $arg);
    } else {
        push(@NEWARGV, $rcsdir . "/" . $arg . ",v");
    }
}

print $exe, " ", join(' ', @NEWARGV), "\n";
system($exe, " ", join(' ', @NEWARGV));
