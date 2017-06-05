
use strict;
use warnings;
use Test::More;
use File::Spec;


# Initialise filenames and check they're there

my $SrcFile   = File::Spec->catfile('t', 'desertnet.src');
my $XMLFile   = File::Spec->catfile('t', 'desertnet.xml');

unless(-e $SrcFile) {
  plan skip_all => 'test data missing';
}

# Make sure we can write to the filesystem and check it uses the same
# clock as the machine we're running on.

my $t0 = time();
unless(open(XML, ">$XMLFile")) {
  plan skip_all => "can't create test file: $!";
}
close(XML);
my $t1 = (stat($XMLFile))[9];
my $t2 = time();

if($t1 < $t0  or  $t2 < $t1) {
  plan skip_all => 'time moved backwards!'
}


plan tests => 8;

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
# Wait until the current time is greater than the supplied value
#

sub PassTime {
  my($Target) = @_;

  while(time <= $Target) {
    sleep 1;
  }
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

ok(CopyFile($SrcFile, $XMLFile), 'copied known good source file');
$t0 = (stat($XMLFile))[9];         # Remember its timestamp

                                   # Parse it with caching enabled
my $opt = XMLin($XMLFile, cache => 'memshare');
is_deeply($opt, $Expected, 'parsed expected data from file');

if ('VMS' eq $^O) {
  1 while (unlink($XMLFile));
} else {
  unlink($XMLFile);
}
ok(! -e $XMLFile, 'deleted the XML source file');
open(FILE, ">$XMLFile");              # Re-create it (empty)
close(FILE);
ok(-e $XMLFile, 'and recreated it (empty)');
$t1 = $t0 - 1;
eval { utime($t1, $t1, $XMLFile); };  # but wind back the clock
$t2 = (stat($XMLFile))[9];         # Skip these tests if that didn't work
SKIP: {
  skip 'no utime', 2 if($t2 >= $t0);

  $opt = XMLin($XMLFile, cache => 'memshare');
  is_deeply($opt, $Expected, 'got expected values from the cache');
  is(-s $XMLFile, 0, 'even though the XML file is empty');
}
PassTime(time());                      # Ensure timestamp changes

open(FILE, ">$XMLFile");               # Write some new data to the XML file
print FILE qq(<opt one="1" two="2"></opt>\n);
close(FILE);
PassTime(time());                      # Ensure current time later than file time

                                       # Parse again with caching enabled
$opt = XMLin($XMLFile, cache => 'memshare');
is_deeply($opt, { one => 1, two => 2}, 'parsed new data through cache');

$opt->{three} = 3;                     # Alter the returned structure
                                       # Retrieve again from the cache
my $opt2 = XMLin($XMLFile, cache => 'memshare');

is($opt2->{three}, 3, 'cache was modified');


exit(0);

