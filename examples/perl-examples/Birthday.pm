package Birthday;

use strict;

my @months = qw(January February March April May June July August
                September October November December);

my %month_numbers;
@month_numbers{@months} = 1 .. 12;

sub new {
    my $class = shift;
    my($month, $day, $year) = @_;
    die unless exists $month_numbers{$month};
    return bless {month => $month, day => $day, year => $year}, $class;
}

sub month {
    my $self = shift;
    return $self->{'month'};
}

sub month_number {
    my $self = shift;
    return $month_numbers{$self->month};
}

sub day {
    my $self = shift;
    return $self->{'day'};
}

sub year {
    my $self = shift;
    return $self->{'year'};
}

sub is_today {
    my($self) = shift;
    my(undef,undef,undef,$day,$mon) = localtime(time);
    return $self->month eq $months[$mon] && $self->day eq $day;
}

my @dividers = (19, 18, 20, 20, 20, 21, 22, 23, 22, 22, 21, 21);
my @signs = qw(Capricorn Aquarius Pisces Aries Taurus Gemini Cancer
               Leo Virgo Libra Scorpio Sagittarius);
sub zodiac {
    my $self = shift;
    my $m = $month_numbers{$self->month} - 1;
    if ($self->day <= $dividers[$m]) {
        return $signs[$m];
    } else {
        return $signs[($m + 1) % 12];
    }
}

sub leap_year {
    my $self = shift;
    my $y = $self->year;
    return ($y % 4 == 0 and $y % 100 != 0) || $y % 400 == 0;
}

my @last_day = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
sub days_in_month {
    my $self = shift;
    if ($self->leap_year and $self->month eq "February") {
        return 29;
    } else {
        return $last_day[$self->month_number - 1];
    }
}

sub next_day {
    my $self = shift;
    $self->{'day'}++;
    if ($self->day > $self->days_in_month) {
        $self->{'day'} = 1;
        $self->next_month;
    }
}

sub next_month {
    my $self = shift;
    if ($self->month ne "December") {
        $self->{'month'} = $months[$self->month_number + 1 - 1];
    } else {
        $self->{'month'} = "January";
        $self->next_year;
    }
}

sub next_year {
    my $self = shift;
    $self->{'year'}++;
}
