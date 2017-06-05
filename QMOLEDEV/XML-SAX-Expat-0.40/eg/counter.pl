#!/usr/bin/perl -w
use strict;
use warnings;
use XML::SAX::Expat;

sub Counter::new { bless { count => 0 }, shift }
sub Counter::start_element { shift->{count}++  }

if (@ARGV != 1) {
  print "Usage: $0 example.xml\n";
  exit;
}

my $counter = Counter->new;
my $parser = XML::SAX::Expat->new(Handler => $counter);
$parser->parse_file($ARGV[0]);

printf "%s has %d elements\n", $ARGV[0], $counter->{count};
