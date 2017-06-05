
use strict;
use Test::More;
use File::Spec;

plan tests => 12;

use_ok('XML::Simple');

SKIP: {
  eval { require Tie::IxHash };

  skip "Tie::IxHash not installed", 3 if $@;

  $@ = '';
  eval <<'EOF';

    package SimpleOrder;

    use base qw(XML::Simple);
    use Tie::IxHash;

    sub new_hashref {
      my $self = shift;
      my %hash;
      tie %hash, 'Tie::IxHash', @_;
      return \%hash;
    }
EOF
  ok(!$@, 'no errors processing SimpleOrder');

  my $xs = SimpleOrder->new;
  my $xml = q{
    <nums>
      <num id="one">I</num>
      <num id="two">II</num>
      <num id="three">III</num>
      <num id="four">IV</num>
      <num id="five">V</num>
      <num id="six">VI</num>
      <num id="seven">VII</num>
    </nums>
  };
  my $expected = {
    'one'   => { 'content' => 'I'   },
    'two'   => { 'content' => 'II'  },
    'three' => { 'content' => 'III' },
    'four'  => { 'content' => 'IV'  },
    'five'  => { 'content' => 'V'   },
    'six'   => { 'content' => 'VI'  },
    'seven' => { 'content' => 'VII' },
  };

  my $data = $xs->xml_in($xml);

  is_deeply($data->{num}, $expected, 'hash content looks good');

  is_deeply(
    [ keys %{$data->{num}} ],
    [ qw(one two three four five six seven) ],
    'order of the hash keys looks good too'
  );

}


my $xs = XML::Simple->new(cache => 'storable');
my $sx = ElbarotsXS->new(cache => 'storable');

isa_ok($sx, 'XML::Simple', 'object of class ElbarotsXS');

my $src_file = File::Spec->catfile('t', 'test1.xml');

is(
  $xs->storable_filename($src_file),
  File::Spec->catfile('t', 'test1.stor'),
  'default storable cache filename looks good'
);

my $cache_file = File::Spec->catfile('t', '1tset.stor'),;
is(
  $sx->storable_filename($src_file),
  $cache_file,
  'overridden storable cache filename looks good'
);

SKIP: {
  eval { require Storable };

  skip "Storable not installed", 2 if $@;

  unlink($cache_file),
  ok(! -e $cache_file, 'overridden cache file does not exist before parse');
  my $data = $sx->xml_in($src_file);
  ok(-e $cache_file, 'overridden cache file does exist after parse');
  unlink($cache_file),
}

my $data = eval {
  $xs = XML::Simple->new(cache => 'floogle');
  $xs->xml_in($src_file);
};
ok($@, 'bad cache scheme was rejected');

$data = eval {
  $sx = ElbarotsXS->new(cache => 'floogle');
  $sx->xml_in($src_file);
};
ok(! $@, 'custom cache scheme was not rejected');
is_deeply(
  $data,
  { data => 'floogle' },
  'custom cache reading method delivered the goods'
);

exit 0;


package ElbarotsXS;

use base 'XML::Simple';

sub storable_filename {
  my($self, $path) = @_;

  my($vol, $dir, $file) = File::Spec->splitpath( $path );
  $file =~ s{\.xml$}{};

  return File::Spec->catpath($vol, $dir, reverse($file) . '.stor');
}

sub cache_read_floogle {
  return { data => 'floogle' };
}
