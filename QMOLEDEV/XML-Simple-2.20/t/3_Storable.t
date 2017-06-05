
use strict;
use warnings;
use Test::More;
use File::Spec;


eval { require Storable; };
unless($INC{'Storable.pm'}) {
  plan skip_all => 'no Storable.pm';
}
unless(UNIVERSAL::can(Storable => 'lock_nstore')) {
  plan skip_all => 'Storable.pm is too old - no file locking support';
}


# Initialise filenames and check they're there

my $SrcFile   = File::Spec->catfile('t', 'desertnet.src');
my $XMLFile   = File::Spec->catfile('t', 'desertnet.xml');
my $CacheFile = File::Spec->catfile('t', 'desertnet.stor');

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


plan tests => 23;

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
# Delete a file - portably
#

sub DeleteFile {
  my($Filename) = @_;

  if ('VMS' eq $^O) {
    1 while (unlink($Filename));
  } else {
    unlink($Filename);
  }
}


##############################################################################
# Create a file, making sure that its timestamp is newer than another
# existing file.
#

sub MakeNewerFile {
  my($File1, $File2, $CodeRef) = @_;

  my $t0 = (stat($File1))[9];
  while(1) {
    unlink($File2);
    $CodeRef->();
    return if (stat($File2))[9] > $t0;
    sleep(1);
  }
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
unlink($CacheFile);
ok(! -e $CacheFile, 'no cache files lying around');

my $opt = XMLin($XMLFile);
is_deeply($opt, $Expected, 'parsed expected data from file');
ok(! -e $CacheFile, 'and no cache file was created');
PassTime(time());                     # Ensure cache file will be newer

$opt = XMLin($XMLFile, cache => 'storable');
is_deeply($opt, $Expected, 'parsed expected data from file (again)');
ok(-e $CacheFile, 'but this time a cache file was created');
$t0 = (stat($CacheFile))[9];       # Remember cache timestamp
PassTime($t0);

$opt = XMLin($XMLFile, cache => ['storable']);
is_deeply($opt, $Expected, 'got expected data from cache');
$t1 = (stat($CacheFile))[9];
is($t0, $t1, 'and cache timestamp has not changed');

PassTime(time());
$t0 = time();
open(FILE, ">>$XMLFile");             # Touch the XML file
print FILE "\n";
close(FILE);
$opt = XMLin($XMLFile, cache => 'storable');
is_deeply($opt, $Expected, 'parsed in expected value again');
$t2 = (stat($CacheFile))[9];
isnt($t1, $t2, 'and this time the cache timestamp has changed');

DeleteFile($XMLFile);
ok(! -e $XMLFile, 'deleted the source file');
open(FILE, ">$XMLFile");              # Re-create it (empty)
close(FILE);
ok(-e $XMLFile, 'recreated the source file');
is(-s $XMLFile, 0, 'but with nothing in it');
MakeNewerFile($XMLFile, $CacheFile, sub { # Make sure cache file is newer
  Storable::nstore($Expected, $CacheFile);
});
$opt = XMLin($XMLFile, cache => 'storable');
is_deeply($opt, $Expected, 'got the expected data from the cache');
$t2 = (stat($CacheFile))[9];
PassTime($t2);
open(FILE, ">$XMLFile") ||            # Write some new data to the XML file
  die "open(>$XMLFile): $!\n";
print FILE qq(<opt one="1" two="2"></opt>\n);
close(FILE);

$opt = XMLin($XMLFile);               # Parse with no caching
is_deeply($opt, { one => 1, two => 2}, 'parsed in expected data from file');
$t0 = (stat($CacheFile))[9];          # And timestamp on cache file
my $s0 = (-s $CacheFile);
is($t0, $t2, 'and the cache file was not touched');

                                      # Parse again with caching enabled
$opt = XMLin($XMLFile, cache => 'storable');
is_deeply($opt, { one => 1, two => 2}, 'parsed expected data through cache');
$t1 = (stat($CacheFile))[9];
my $s1 = (-s $CacheFile);
ok(($t0 != $t1) || ($s0 != $s1),
'and the cache was updated'); # Content changes but date may not on Win32

ok(CopyFile($SrcFile, $XMLFile), 'copied back the original file');
PassTime($t1);
$opt = XMLin($XMLFile, cache => 'storable');
is_deeply($opt, $Expected, 'parsed expected data in through cache');

# Make sure scheme name is case-insensitive

$opt = XMLin($XMLFile, cache => 'Storable');
is_deeply($opt, $Expected, 'scheme name is case-insensitive');

# Make sure bad scheme names are trapped

$@='';
$_ = eval { XMLin($XMLFile, cache => 'Storubble'); };
is($_, undef, 'bad cache scheme names are trapped');
like($@, qr/Unsupported caching scheme: storubble/,
'with correct error message');


# Clean up and go

unlink($CacheFile);
unlink($XMLFile);
exit(0);

