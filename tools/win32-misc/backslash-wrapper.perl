#!perl

# Translates / to \ for the anal DOS commands

$exe = shift(@ARGV);

while ($arg = shift(@ARGV)) {
    $arg =~ tr|/|\\|;
    push(@NEWARGV, $arg);
}

print $exe, " ", join(' ', @NEWARGV), "\n";
system($exe, " ", join(' ', @NEWARGV));
