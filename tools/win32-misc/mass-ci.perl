#!perl

# Checks in a whole slew of files.  What it reads from stdin looks like:

# file1
# blah blah blah
# blah blah
# .
# file2
# blah blah
# blah blah blah
# .

# where blah is what is entered into the RCS log

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
#    print("ci $file\n");
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
