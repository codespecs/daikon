#!perl -w

while (<STDIN>) {
    chop;
    
    if (/\.([^.]+)$/) {
        my($fullname) = $_;
        my($classname) = $1;
        
        system("cmd /c \"javap -c -private -classpath .;examples\\jess44;javacc\\javacc.zip;d:\\programs\\jdk117\\lib\\classes.zip $_ > results\\$classname.p\"");
    }
}

