#!perl

# Do an rcsdiff on files you get from stdinput

open(OUTPUT, ">mass-diff-temp.bat");

print OUTPUT "\@echo off\n";

while (<>) {
    print OUTPUT "echo ", $_;
    chop;
    @components = split('/', $_);
    $file = $components[$#components]; # last component
#    print "size = $#components\n";
    @new_components = splice(@components, 0, $#components);
    $dir = join('\\', @new_components); # all but last
    # splice destroys @components array...
    $cmd = "cmd /c \"cd $dir && rcsdiff $file\"";
    print OUTPUT $cmd, "\n";
#    system(cmd) || die "Failed: $cmd";
}

close(OUTPUT);
system("mass-diff-temp.bat");
# return value of system is probably 1, meaning there were differences
unlink("mass-diff-temp.bat");
