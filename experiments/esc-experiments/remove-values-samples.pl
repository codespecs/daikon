#!/usr/bin/perl -wpi.bak

s/\([0-9]+ values?, [0-9]+ samples?\)//;
s/  [0-9]+ samples?$//;
s/^    Samples breakdown:.*\n//;
