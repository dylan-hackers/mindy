#!perl

# Does an rcsdiff on a list of filenames taken from standard input.
# The usual way we use this program is
#
#    tree-diff -w | mass-diff > diffs.txt
#
# Any command line options passed to mass-diff will be passed on to
# rcsdiff.

$cmd_line_options = join(' ', @ARGV); # pass cmd line options to rcsdiff

# We implement mass-diff by creating a batch file to do the diff'ing.
# We do this rather than diffing ourselves because you have to rcsdiff
# from the directory of the file being diffed.  Using batch files
# makes it easier to "un-cd" (un-change directory) after we're done
# diffing each file.  [### With Robert's modifications to rcswrapper,
# do we still need this?  Or can we do the diff'ing direct ourselves?]

open(OUTPUT, ">mass-diff-temp.bat");
print OUTPUT "\@echo off\n";

while (<STDIN>) {
    print OUTPUT "echo ", $_;
    chop;
    @components = split('/', $_);
    $file = $components[$#components]; # last component
    @new_components = splice(@components, 0, $#components);
    $dir = join('\\', @new_components); # all but last
    # splice destroys @components array...
    $cmd = "cmd /c \"cd $dir && rcsdiff $cmd_line_options $file\"";
    print OUTPUT $cmd, "\n";
}

close(OUTPUT);
system("mass-diff-temp.bat");
# return value of system() is probably 1, meaning there were differences
unlink("mass-diff-temp.bat");
