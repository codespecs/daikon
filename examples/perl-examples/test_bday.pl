use Birthday;

my $size = $ARGV[0] || 10;

my $bday = new Birthday("February", 23, 1975);

for (1 .. $size * 100) {
    my $sign = $bday->zodiac;
    if ($bday->is_today) {
        print "Happy birthday!\n";
    }
#    print "$sign ", %{$bday}, "\n";
    $bday->next_day;
}

for my $year (1994 .. 1994 + 10 * $size) {
    my $day = new Birthday("January", 1, $year);
    print "$year is " . ($day->leap_year ? "" : "not ") . "a leap year.\n";
}

# my $h;

# $h = new Birthday("February", 18, 1975);
# print $h->month . " " . $h->day . " " . $h->zodiac . "\n";
# $h = new Birthday("February", 19, 1975);
# print $h->month . " " . $h->day . " " . $h->zodiac . "\n";

# $h = new Birthday("June", 20, 1975);
# print $h->month . " " . $h->day . " " . $h->zodiac . "\n";
# $h = new Birthday("June", 21, 1975);
# print $h->month . " " . $h->day . " " . $h->zodiac . "\n";
# $h = new Birthday("June", 22, 1975);
# print $h->month . " " . $h->day . " " . $h->zodiac . "\n";
# $h = new Birthday("June", 23, 1975);
# print $h->month . " " . $h->day . " " . $h->zodiac . "\n";
