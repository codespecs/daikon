#!/usr/bin/perl -pi
# #!/usr/bin/perl -pi.bak

use strict;
use English;
$WARNING = 1;

s{(http://[-a-zA-Z0-9.]*/)&nbsp;}{\1~}g;
