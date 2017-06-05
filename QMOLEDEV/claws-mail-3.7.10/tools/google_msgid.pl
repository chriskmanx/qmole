#!/usr/bin/perl
my $browser   = 'mozilla';
my $google_id = 'http://groups.google.com/groups?as_umsgid';
$_ = <>;
chomp;
s/<|>//eg;
system("$browser '$google_id=$_' &");
