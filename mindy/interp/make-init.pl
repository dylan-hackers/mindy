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
close(out1);

open(out2, ">,extern2.def");
foreach $name (@keys) {
    ($name2 = $name) =~ s/\(\)$//;
    print out2 "add_explicit_symbol(\"$name2\", &$name2);\n";
}
close(out2);
