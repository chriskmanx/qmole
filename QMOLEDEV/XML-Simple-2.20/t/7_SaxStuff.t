
use strict;
use warnings;
use Test::More;
use File::Spec;
use IO::File;


BEGIN {
  unshift @INC, File::Spec->catfile('t', 'lib');

  eval { require XML::SAX; };
  if($@) {
    plan skip_all => 'no XML::SAX';
  }
}

use TagsToUpper;

# Initialise filenames and check they're there

my $SrcFile   = File::Spec->catfile('t', 'desertnet.src');
my $XMLFile   = File::Spec->catfile('t', 'desertnet.xml');
my $CacheFile = File::Spec->catfile('t', 'desertnet.stor');

unless(-e $SrcFile) {
  plan skip_all => 'test data missing';
}


plan tests => 14;


##############################################################################
#                   S U P P O R T   R O U T I N E S
##############################################################################

##############################################################################
# Copy a file
#

sub CopyFile {
  my($Src, $Dst) = @_;

  open(IN, $Src) || return(undef);
  local($/) = undef;
  my $Data = <IN>;
  close(IN);

  open(OUT, ">$Dst") || return(undef);
  print OUT $Data;
  close(OUT);

  return(1);
}


##############################################################################
#                      T E S T   R O U T I N E S
##############################################################################

use XML::Simple;

# Initialise test data

my $Expected  = {
  'server' => {
                'sahara' => {
                              'osversion' => '2.6',
                              'osname' => 'solaris',
                              'address' => [
                                             '10.0.0.101',
                                             '10.0.1.101'
                                           ]
                            },
                'gobi' => {
                            'osversion' => '6.5',
                            'osname' => 'irix',
                            'address' => '10.0.0.102'
                          },
                'kalahari' => {
                                'osversion' => '2.0.34',
                                'osname' => 'linux',
                                'address' => [
                                               '10.0.0.103',
                                               '10.0.1.103'
                                             ]
                              }
              }
};

my $xml = '';


# Force default behaviour of using SAX parser if it is available (which it
# is or we wouldn't be here).

$XML::Simple::PREFERRED_PARSER = '';

ok(CopyFile($SrcFile, $XMLFile), 'created source XML file');
if ('VMS' eq $^O) {
  1 while (unlink($CacheFile));
} else {
  unlink($CacheFile);
}
ok(! -e $CacheFile, 'deleted old cache files');

# Pass in a filename to check parse_uri()

my $opt = XMLin($XMLFile);
is_deeply($opt, $Expected, 'parsed expected value from file');


# Pass in an IO::File object to test parse_file()

my $fh = IO::File->new("<$XMLFile");
isa_ok($fh, 'IO::File', '$fh');
$opt = XMLin($fh);
is_deeply($opt, $Expected, 'parsed expected value from IO::File object');
$fh->close();


# Pass in a string to test parse_string()

if(open(XMLFILE, "<$XMLFile")) {
  local($/) = undef;
  $xml = <XMLFILE>;
  close(XMLFILE);
}
$opt = XMLin($xml);
is_deeply($opt, $Expected, 'parsed expected value from string');


# Pass in '-' for STDIN

open(OLDSTDIN, "<&STDIN");
close(STDIN);
open(STDIN, "<$XMLFile");
$opt = XMLin('-');
is_deeply($opt, $Expected, "parsed expected value from STDIN ('-')");

open(STDIN, "<&OLDSTDIN");
close(OLDSTDIN);


# Try using XML:Simple object as a SAX handler

my $simple = XML::Simple->new();
my $parser = XML::SAX::ParserFactory->parser(Handler => $simple);

$opt = $parser->parse_uri($XMLFile);
is_deeply($opt, $Expected,
  'XML::Simple as a SAX handler returned expected value');


# Try again but make sure options from the constructor are being used

$simple = XML::Simple->new(
  keyattr    => { server => 'osname' },
  forcearray => ['address'],
);
$parser = XML::SAX::ParserFactory->parser(Handler => $simple);

$opt = $parser->parse_uri($XMLFile);
my $Expected2 = {
  'server' => {
                'irix' => {
                            'address' => [ '10.0.0.102' ],
                            'osversion' => '6.5',
                            'name' => 'gobi'
                          },
                'solaris' => {
                               'address' => [ '10.0.0.101', '10.0.1.101' ],
                               'osversion' => '2.6',
                               'name' => 'sahara'
                             },
                'linux' => {
                             'address' => [ '10.0.0.103', '10.0.1.103' ],
                             'osversion' => '2.0.34',
                             'name' => 'kalahari'
                           }
              }
};

is_deeply($opt, $Expected2, 'options passed to handler contructor work');


# Try using XML::Simple to drive a SAX pipeline

my $Expected3  = {
  'SERVER' => {
                'sahara' => {
                              'OSVERSION' => '2.6',
                              'OSNAME' => 'solaris',
                              'ADDRESS' => [
                                             '10.0.0.101',
                                             '10.0.1.101'
                                           ]
                            },
                'gobi' => {
                            'OSVERSION' => '6.5',
                            'OSNAME' => 'irix',
                            'ADDRESS' => '10.0.0.102'
                          },
                'kalahari' => {
                                'OSVERSION' => '2.0.34',
                                'OSNAME' => 'linux',
                                'ADDRESS' => [
                                               '10.0.0.103',
                                               '10.0.1.103'
                                             ]
                              }
              }
};
my $simple2 = XML::Simple->new(keyattr => [qw(NAME)]);
my $filter = TagsToUpper->new(Handler => $simple2);

my $opt2 = XMLout($opt,
  keyattr    => { server => 'osname' },
  Handler    => $filter,
);
is_deeply($opt2, $Expected3, 'driving a SAX pipeline with XML::Simple worked');


# Confirm that 'handler' is a synonym for 'Handler'

$simple2 = XML::Simple->new(keyattr => [qw(NAME)]);
$filter = TagsToUpper->new(Handler => $simple2);
$opt2 = XMLout($opt,
  keyattr    => { server => 'osname' },
  handler    => $filter,
);
is_deeply($opt2, $Expected3, "'handler' is a synonym for 'Handler'");


# Confirm that DataHandler routine gets called

$xml = q(<opt><anon>one</anon><anon>two</anon><anon>three</anon></opt>);
$simple = XML::Simple->new(
  DataHandler => sub {
                   my $xs = shift;
                   my $data = shift;
                   return(join(',', @$data));
                 }
);
$parser = XML::SAX::ParserFactory->parser(Handler => $simple);
my $result = $parser->parse_string($xml);

is($result, 'one,two,three', "'DataHandler' option works");


# Confirm that 'datahandler' is a synonym for 'DataHandler'

$simple = XML::Simple->new(
  datahandler => sub {
                   my $xs = shift;
                   my $data = shift;
                   return(join(',', reverse(@$data)));
                 }
);
$parser = XML::SAX::ParserFactory->parser(Handler => $simple);
$result = $parser->parse_string($xml);

is($result, 'three,two,one', "'datahandler' is a synonym for 'DataHandler'");


# Confirm keeproot logic gets called

$simple = XML::Simple->new(keeproot => 1);
$parser = XML::SAX::ParserFactory->parser(Handler => $simple);
$opt = $parser->parse_string('<opt a="1" b="2" />');
is_deeply($opt, {opt => {a => 1, b => 2}}, "keeproot works with SAX pipelines");

# Clean up and go

unlink($CacheFile);
unlink($XMLFile);
exit(0);

