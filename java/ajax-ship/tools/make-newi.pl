my($n) = 0;

open(F, "classlist");
open(O, ">out");
while (<F>) {
    my($classname);

    if (m!([A-Za-z0-9_/.]+)\.class$!) {
        $classname = $1;
        $classname =~ s!/!.!g;
    } elsif (m!^([A-Za-z0-9_.]+)$!) {
        $classname = $1;
    } elsif (m!\"([A-Za-z0-9_.]+)\"!) {
        $classname = $1;
    }

    if (defined($classname)) {
        print O "L$n:
    EXN = new $classname;
    $classname.<init>(EXN);
    $classname.<init>(EXN, STR);
    goto L;
";
        $n++;
    }
}

print O "    goto L", join(", L", 0..($n-1)), ";\n";
print O "    return = C\n";
