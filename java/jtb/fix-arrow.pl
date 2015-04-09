#!/usr/bin/env perl


while (<>) {

    # delete lines that start with "  /**" or "  */"
    if ((/^ *\/\*\*$/) || (/^ +\*\/$/)) {
        next;
    }

    # change "  * " to " // "
    s/^ ( *)\* /\1\/\/ /;
    # special case for no comment after the '*'
    s/^ ( *)\*$/\1\/\/ /;
    print;
}

