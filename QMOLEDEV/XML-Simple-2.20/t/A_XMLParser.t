
use strict;
use warnings;
use Test::More;
use IO::File;
use File::Spec;


# The suppress-able warnings still check the global flag

$^W = 1;

# Initialise filenames and check they're there

my $XMLFile = File::Spec->catfile('t', 'test1.xml');  # t/test1.xml

unless(-e $XMLFile) {
  plan skip_all => 'Test data missing';
}

eval { require XML::Parser; };
unless($INC{'XML/Parser.pm'}) {
  plan skip_all => 'no XML::Parser';
}

plan tests => 14;

use XML::Simple;

my $last_warning = '';
my $opt;


# Use environment variable to set preferred parser

$ENV{XML_SIMPLE_PREFERRED_PARSER} = 'XML::Parser';


# Try using a SAX-only option

{
  local($SIG{__WARN__}) = \&warn_handler;

  $@ = '';
  $opt = eval { XMLin('<x y="z" />', nsexpand => 1) };
}

isnt($last_warning, '', "Parsing caused warning (as expected)");
like($last_warning, qr/'nsexpand' option requires XML::SAX/,
  'Message contained expected text');
is_deeply($opt, {y => 'z'}, "Parsing was successful");


# Check for deprecation warning

{
  local($SIG{__WARN__}) = \&warn_handler;

  $@ = '';
  $last_warning = '';
  $opt = eval { XMLin('<x y="z" />', ParserOpts => [ ParseParamEnt => 1 ]) };
}

isnt($last_warning, '', "Using ParserOpts caused warning (as expected)");
like($last_warning, qr/'ParserOpts' is deprecated/,
  'Message contained expected text');
is_deeply($opt, {y => 'z'}, "Parsing was successful");


# Check it doesn't happen if warnings disabled

{
  local($SIG{__WARN__}) = \&warn_handler;

  $@ = '';
  $last_warning = '';
  local($^W) = 0;
  $opt = eval { XMLin('<x y="z" />', ParserOpts => [ ParseParamEnt => 1 ]) };
}

is($last_warning, '', "ParserOpts warning uppressed successfully");
is_deeply($opt, {y => 'z'}, "Parsing was successful");



# Try parsing a string

$@ = '';
$opt = eval {
  XMLin(q(<opt name1="value1" name2="value2"></opt>));
};

my $expected = {
                 name1 => 'value1',
                 name2 => 'value2',
               };

is($@, '', "No error when parsing");
is_deeply($opt, $expected, 'matches expectations (attributes)');


# Try parsing a named external file

$@ = '';
$opt = eval{ XMLin($XMLFile); };
is($@, '', "XML::Parser didn't choke on named external file");
is_deeply($opt, {
  location => 't/test1.xml'
}, 'and contents parsed as expected');


# Try parsing from an IO::Handle

$@ = '';
my $fh = new IO::File;
$XMLFile = File::Spec->catfile('t', '1_XMLin.xml');  # t/1_XMLin.xml
eval {
  $fh->open($XMLFile) || die "$!";
  $opt = XMLin($fh);
};
is($@, '', "XML::Parser didn't choke on an IO::File object");
is($opt->{location}, 't/1_XMLin.xml', 'and it parsed the right file');


exit(0);

sub warn_handler {
  $last_warning = $_[0];
}
