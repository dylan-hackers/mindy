#! perl

# Parameters: The names of all the exports files you care about.

# Helps you discover which modules use which modules, so that you can
# break libraries up into smaller chunks

# Assumes the same module name never comes up twice.

sub qualify {
    local ($mod) = @_;
    if ( $module_owned_by{$mod} ) {
	return $module_owned_by{$mod} . "/$mod";
    } else {
	return "unknown/$mod";
    }
}

sub add_library_use {
    local ($lib_used) = @_;
    $library_uses{$current_library} .= " $lib_used";
    $library_used_by{$lib_used} .= " $current_library";
}

sub add_module_use {
    local ($mod_used) = @_;
    $mod_used = &qualify($mod_used);
    $module_uses{&qualify($current_module)} .= " $mod_used";
    $module_used_by{$mod_used} .= " " . &qualify($current_module);
    $library_uses_modules{$current_library} .= " $mod_used";
}

while (<>) {
    if (/define library ([-a-zA-Z0-9]+)/) {
	$current_library = $1;
	print "Doing library $current_library\n";
	$inside_library = 1;
    } elsif (/define module ([-a-zA-Z0-9]+)/) {
	$current_module = $1;
	$module_owned_by{$current_module} = $current_library;
	print "  Doing module $current_module\n";
	$inside_library = 0;
    } elsif (/^\s*use ([-a-zA-Z0-9]+)/) {
	$use_clause = $1;
	print "    uses $use_clause\n";
	if ($inside_library) {
	    add_library_use($use_clause);
	} else {
	    add_module_use($use_clause);
	}
    }
}

print "\n\n-------------------------------------------\n\n";
print "Who is used by who\n\n";
foreach $library (sort keys(%library_used_by)) {
    local (@uses) = split(' ', $library_used_by{$library});
    print "Library $library:\n";
    foreach $use (@uses) {
	print "  used-by $use\n";
    }
}

print "\n\n";

foreach $module (sort keys(%module_used_by)) {
    print "Module $module:\n";
    local (@uses) = split(' ', $module_uses{$module});
    foreach $use (@uses) {
	print "  uses $use\n";
    }
    @uses = split(' ', $module_used_by{$module});
    foreach $use (@uses) {
	print "  used-by $use\n";
    }
}

print "\n\n";
foreach $library (sort keys(%library_used_by)) {
    print "Library $library module usage:\n";
    local (@uses) = split(' ', $library_uses_modules{$library});
    local (%u); # for removing duplicates
    foreach $use (@uses) {
	$u{$use} = $use;
    }
    foreach $use (sort keys(%u)) {
	print "  uses module $use\n";
    }
}

