#!perl

# rcs-wrapper usage:
#    perl rcs-wrapper.perl rcs-command [arg1 arg2 arg3...]
#
# This little script tries to compensate for the lack of symbolic
# links in Windows/NT.  It looks through the arguments you're passing
# to the RCS utility being wrapped, and any argument that doesn't
# begin with a dash will have fake symlinks expanded into a real file
# name.  Fake symlinks are named "RCS", and the contents of them is
# the name of the directory they point at.  After expanding all fake
# symlinks, the wrapped rcs-command is invoked.

$exe = shift(@ARGV);

while ($arg = shift(@ARGV)) {
    push(@NEWARGV, $arg);
    if ($arg !~ /^-/) {
	($path, $file) = ($arg =~ /(.*[\/\\])?([^\/\\]*)/);
	$rcsdir = $paths{$path};
	# if the contents of "RCS" fake symlink is already cached, use it.
	if ($rcsdir) {
	    push(@NEWARGV, $rcsdir . "/" . $file . ",v");
	} 
	# Otherwise, try opening the "RCS" fake symlink.  If
	# unsuccessful, don't expand the fake symlink.
	elsif (open(FILE, "${path}RCS")) {
	    $rcsdir = <FILE>;
	    chop($rcsdir);
	    close(FILE);
	    push(@NEWARGV, $rcsdir . "/" . $file . ",v");
	    $paths{$path} = $rcsdir;
	}
    }
}

system($exe, " ", join(' ', @NEWARGV));

