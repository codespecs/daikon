#!perl -w

my($stop_global) = 0;
if ($ARGV[0] eq "-g") {
    shift(@ARGV);
    $stop_global = 1;
}

my($id) = shift(@ARGV);
my($num) = 0;
my($id2);
my($num2) = 99999999;

if (defined($ARGV[0]) && !($ARGV[0] =~ m![\\/]!)) {
    $id2 = shift(@ARGV);
}

if (!defined($id)) {
    die 'Usage: [-g] id[@num] [id[@num]] [<file>]\n';
}

if ($id =~ /^(.*)@(.*)$/) {
    $id = $1;
    $num = $2;
}

if (defined($id2)) {
    $num2 = 0;
    if ($id2 =~ /^(.*)@(.*)$/) {
        $id2 = $1;
        $num2 = $2;
    }
}

while (<ARGV>) {
    my($printed) = 0;
    
    if (/$id/ && /^..? /) {
        if ($num == 0) {
            $printed = 1;
            print;
        }
        
        if (/^M (.*)<-$id$/) {
            if ($num == 0) {
                $id = $1;
            } else {
                $num--;
            }
        }
        
        if ($stop_global && /^G/) {
            exit;
        }
    }
    
    if (defined($id2)) {
        if (/$id2/) {
            if ($num2 == 0 && !$printed) {
                $printed = 1;
                print;
            }
            
            if (/^[MLS][ ]*(.*)<-$id2$/) {
                if ($num2 == 0) {
                    $id2 = $1;
                } else {
                    $num2--;
                }
            }
        }
        
        if ($id eq $id2) {
            print "Found collision!\n";
            exit;
        }
    }
}
