#!perl

# Usage:
#     mass-ci logs.txt
# or  mass-ci < logs.txt
#
# mass-ci is used for doing an RCS ci on a whole bunch of files at
# once, with many of those files residing in subdirectories of the
# current dir.  mass-ci uses standard Perl techniques to get its
# input, so you can either give it a file name with log info, or pass
# this info in via stdin.
#
# The actual log info (ie, the contents of logs.txt or stdin) looks
# like this:
#
# file1
# blah blah blah
# blah blah
# .
# file2
# blah blah
# blah blah blah
# .
#
# where "blah" is what is entered into the RCS log

require "pwd.pl";
&initpwd;

$original_dir = $ENV{"PWD"};

while (<>) {
    chop;
    @components = split('/', $_);
    $file = $components[$#components]; # last component
    @new_components = splice(@components, 0, $#components);
    $dir = join('\\', @new_components); # all but last
    # splice destroys @components array...
    &chdir($dir);
    open(OUT, "| ci $file") || die "Can't spawn ci $file\n";
  loop:
    while (<>) {
	print OUT $_;
	chop;
	if ($_ eq ".") {
	    last loop; # end while loop
	}
    }
    close(OUT);
    &chdir($original_dir);
}
