#! /usr/local/bin/perl

while (<>) {
    chop($_);
    $names{$_}++;
}

open(out1, ">,extern1.def");
@keys = keys(%names);
print out1 "#define extern_sym_count " . scalar(@keys) . "\n";
foreach $name (@keys) {
    print out1 "extern void *$name;\n";
}
$symcounter = 0;
$counter = 0;
print out1 "\nvoid init$counter()\n{\n";
foreach $name (@keys) {
    if (++$symcounter >= 25) {
	$symcounter = 0;
	$counter++;
	print out1 "}\n\nvoid init$counter()\n{\n";
    }
    ($name2 = $name) =~ s/\(\)$//;
    print out1 "    add_explicit_symbol(\"$name2\", &$name2);\n";
}
print out1 "}\n\n";
close(out1);

open(out2, ">,extern2.def");
for $num (0..$counter) {
    print out2 "    init$num();\n";
}
close(out2);
