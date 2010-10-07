#!/usr/bin/env perl

while (<>) {
    s/: +[0-9]+ Aborted +/: Aborted: /;
    s/\(core dumped\) *//;
    s/==\d+==/==PID==/;
    s/0x([0-9a-f]{3,4})([0-9a-f]{4})/0x$1****/ig;
    s/134\d{7}/134*******/g;
    s/134\d{6}/134******/g;
    s/138\d{7}/138*******/g;
    s/138\d{6}/138******/g;
    s/kvasir-[\d.]+,/kvasir-VERSION/;
    print;
}
