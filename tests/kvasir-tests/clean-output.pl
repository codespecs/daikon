#!/usr/bin/env perl

my $machine_type;

chomp($machine_type = `uname -m`);

while (<>) {
    s/: +[0-9]+ Aborted +/: Aborted: /;
    s/\(core dumped\) *//;
    s/==\d+==/==PID==/;

    if ($machine_type eq "x86_64") {

        # Valgrind AMD64/Linux locations

        s/0xff([0-9a-f]{7,10})(?![0-9a-f])/<STACK_ADDR>/ig;  # stack

        # These addresses are reported by kvasir when it gets confused
        # about structs being passed by value (in regs or on the stack).
        # We pretend they're statics just to get consistent results.
        s/0x8[01]([0-9a-f]{7})(?![0-9a-f])/<STATIC_ADDR>/ig;

        # There is no easy way to differentiate between dynamically
        # allocated data (re malloc) and dynamically loaded code and data.
        # Hence, we will just call it all 'HEAP'.
        s/0x[45]([0-9a-f]{6})(?![0-9a-f])/<HEAP_ADDR>/ig;
        s/0x3([0-9a-f]{9})(?![0-9a-f])/<HEAP_ADDR>/ig;

        # The originally loaded executable - our test case.
        s/0x[45]([0-9a-f]{5})(?![0-9a-f])/<STATIC_ADDR>/ig;  # r/o data
        s/0x6[01]([0-9a-f]{4})(?![0-9a-f])/<STATIC_ADDR>/ig; # r/w data

    } else {
        # we just assume ...
        # Valgrind X86/Linux locations
        s/0xb[ef]([0-9a-f]{6})/<STACK_ADDR>/ig;  # stack

        s/0x38([0-9a-f]{6})/<HEAP_ADDR>/ig;      # heap
        s/0x4b[678]([0-9a-f]{5})/<HEAP_ADDR>/ig; # heap
        s/0x40([0-9a-f]{5})/<HEAP_ADDR>/ig;      # heap

        # The originally loaded executable - our test case.
        s/0x6[123]([0-9a-f]{6})/<STATIC_ADDR>/ig;# r/w data
        s/0x8[01]([0-9a-f]{5})/<STATIC_ADDR>/ig; # r/o and r/w data
    }    

    s/[0-9]:[0-9]{2}:[0-9]{2}/<TIME>/g;      
    s/(.+)Time:(.*)seconds(.*)/<TIME>/g;  
    s/(.+)Rendering line(.*)/<TIME>/g;        # Time (povray prints time taken to execute)

    s/Invalid read of.*/<MEMCHECK ERROR>/g;                # povray has multiple memcheck errors
    s/Address .* bytes after a block .*/<MEMCHECK_ERROR>/g; # replace the information with generic
                                                           # sizes and addresses. Don't remove completely though
                                                           # as we'd still like them to be around to catch errors
                                                           # in other tests

    s/Total Alloc.*/<Allocation statistics>/g;
    s/Peak memory.*/<Allocation statistics>/g;

    s/([0-9]+ ms)/<TIMING>/;
    s/([0-9]+ ms total)/<TIMING>/;

    s/: __write_nocancel .*/: __write_nocancel/;
    s/Copyright \(C\) .*$/COPYRIGHT/;
    s/kvasir-[\d.]+,/kvasir-VERSION/;
    s[Using Valgrind-.* and LibVEX; rerun with \-h for copyright info]
      [Using Valgrind and LibVEX; rerun with \-h for copyright info];
    s/\(vg_replace_malloc.c:(\d+)\)/(vg_replace_malloc.c:XXX)/;
    print;
}
