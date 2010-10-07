#!/usr/bin/env perl

while (<>) {
    s/: +[0-9]+ Aborted +/: Aborted: /;
    s/\(core dumped\) *//;
    s/==\d+==/==PID==/;
    #s/0x([0-9a-f]{1,2})([0-9a-f]{6})/0x$1****/ig;
    s/0xb[ef]([0-9a-f]{6})/<STACK_ADDR>/ig;  # Valgrind x86/Linux location
    s/0xf[ef]([0-9a-f]{6})/<STACK_ADDR>/ig;  # Valgrind 32-on-64/Linux location
    s/0x7f([0-9a-f]{7})/<STACK_ADDR>/ig;     # Valgrind AMD64/Linux location

    s/0x4[0-8]([0-9a-f]{5})/<HEAP_ADDR>/ig;  # Valgrind x86/Linux location
    s/0x6[0-3]([0-9a-f]{5})/<HEAP_ADDR>/ig;  # Valgrind 32-on-64/Linux location
    s/0x4d([0-9a-f]{5})/<HEAP_ADDR>/ig;      # Valgrind AMD64/Linux location

    s/0x8[01]([0-9a-f]{5})/<STATIC_ADDR>/ig; # Valgrind x86/Linux location
    s/0x4[01]([0-9a-f]{4})/<STATIC_ADDR>/ig; # Valgrind AMD64/Linux rodata
    s/0x5[01]([0-9a-f]{4})/<STATIC_ADDR>/ig; # Valgrind AMD64/Linux data


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

    s/134\d{7}/134*******/g;
    s/134\d{6}/134******/g;
    s/138\d{7}/138*******/g;
    s/138\d{6}/138******/g;
    s/kvasir-[\d.]+,/kvasir-VERSION/;
    s[Using Valgrind-3.6.0.SVN and LibVEX; rerun with \-h for copyright info]
      [Using Valgrind and LibVEX; rerun with \-h for copyright info];
    s/\(vg_replace_malloc.c:(\d+)\)/(vg_replace_malloc.c:XXX)/;
    print;
}
