
use strict;
use warnings;
use Test::More;

plan tests => 201;


##############################################################################
#                   S U P P O R T   R O U T I N E S
##############################################################################

##############################################################################
# Read file and return contents as a scalar.
#

sub ReadFile {
  local($/) = undef;

  open(_READ_FILE_, $_[0]) || die "open($_[0]): $!";
  my $data = <_READ_FILE_>;
  close(_READ_FILE_);
  return($data);
}

use XML::Simple;

# Confirm error when mandatory parameter missing

$_ = eval {
  XMLout();
};
ok(!defined($_), 'call with no args proves fatal');
like($@, qr/XMLout\(\) requires at least one argument/,
'with correct error message');

# Try encoding a scalar value

my $xml = XMLout("scalar");
ok(1, 'XMLout did not crash');
ok(defined($xml), 'and it returned an XML string');
is(XMLin($xml), 'scalar', 'which parses back OK');


# Next try encoding a hash

my $hashref1 = { one => 1, two => 'II', three => '...' };
my $hashref2 = { one => 1, two => 'II', three => '...' };

# Expect:
# <opt one="1" two="II" three="..." />

$_ = XMLout($hashref1);
is_deeply(XMLin($_), $hashref1, 'encoded a hash');
ok(s/one="1"//, 'first key encoded OK');
ok(s/two="II"//, 'second key encoded OK');
ok(s/three="..."//, 'third key encoded OK');
like($_, qr/^<\w+\s+\/>/, 'no other attributes encoded');


# Now try encoding a hash with a nested array

my $ref = {array => [qw(one two three)]};
# Expect:
# <opt>
#   <array>one</array>
#   <array>two</array>
#   <array>three</array>
# </opt>

$_ = XMLout($ref);
is_deeply(XMLin($_), $ref, 'encoded a hash with nested array');
ok(s{<array>one</array>\s*
         <array>two</array>\s*
         <array>three</array>}{}sx, 'array elements encoded in correct order');
like($_, qr/^<(\w+)\s*>\s*<\/\1>\s*$/s, 'no other spurious encodings');


# Now try encoding a nested hash

$ref = { value => '555 1234',
         hash1 => { one => 1 },
         hash2 => { two => 2 } };
# Expect:
# <opt value="555 1234">
#   <hash1 one="1" />
#   <hash2 two="2" />
# </opt>

$_ = XMLout($ref);
is_deeply(XMLin($_), $ref, 'encoded nested hashes');

ok(s{<hash1 one="1" />\s*}{}s, 'nested hash 1 ok');
ok(s{<hash2 two="2" />\s*}{}s, 'nested hash 2 ok');
like($_, qr{^<(\w+)\s+value="555 1234"\s*>\s*</\1>\s*$}s, 'whole OK');


# Now try encoding an anonymous array

$ref = [ qw(1 two III) ];
# Expect:
# <opt>
#   <anon>1</anon>
#   <anon>two</anon>
#   <anon>III</anon>
# </opt>

$_ = XMLout($ref);
is_deeply(XMLin($_), $ref, 'encoded anonymous array');

like($_, qr{
  ^<(\w+)\s*>
  \s*<anon>1</anon>
  \s*<anon>two</anon>
  \s*<anon>III</anon>
  \s*</\1>\s*$}sx, 'output matches expectations');


# Now try encoding a nested anonymous array

$ref = [ [ qw(1.1 1.2) ], [ qw(2.1 2.2) ] ];
# Expect:
# <opt>
#   <anon>
#     <anon>1.1</anon>
#     <anon>1.2</anon>
#   </anon>
#   <anon>
#     <anon>2.1</anon>
#     <anon>2.2</anon>
#   </anon>
# </opt>

$_ = XMLout($ref);
is_deeply(XMLin($_), $ref, 'encoded nested anonymous arrays');

like($_, qr{
  <(\w+)\s*>
  \s*<anon\s*>
  \s*<anon\s*>1\.1</anon\s*>
  \s*<anon\s*>1\.2</anon\s*>
  \s*</anon\s*>
  \s*<anon\s*>
  \s*<anon\s*>2\.1</anon\s*>
  \s*<anon\s*>2\.2</anon\s*>
  \s*</anon\s*>
  \s*</\1\s*>
}sx, 'output matches expectations');


# Now try encoding a hash of hashes with key folding disabled

$ref = { country => {
                      England => { capital => 'London' },
                      France  => { capital => 'Paris' },
                      Turkey  => { capital => 'Istanbul' },
                    }
       };
# Expect:
# <opt>
#   <country>
#     <England capital="London" />
#     <France capital="Paris" />
#     <Turkey capital="Istanbul" />
#   </country>
# </opt>

$_ = XMLout($ref, keyattr => []);
is_deeply(XMLin($_), $ref, 'encoded hash of hashes with folding disabled');
ok(s{<England\s+capital="London"\s*/>\s*}{}s, 'nested hash 1 ok');
ok(s{<France\s+capital="Paris"\s*/>\s*}{}s, 'nested hash 2 ok');
ok(s{<Turkey\s+capital="Istanbul"\s*/>\s*}{}s, 'nested hash 3 ok');
ok(s{<country\s*>\s*</country>}{}s, 'container hash ok');
ok(s{^<(\w+)\s*>\s*</\1>$}{}s, 'document ok');


# Try encoding same again with key folding set to non-standard value

# Expect:
# <opt>
#   <country fullname="England" capital="London" />
#   <country fullname="France" capital="Paris" />
#   <country fullname="Turkey" capital="Istanbul" />
# </opt>

my $expected = qr{
  ^<(\w+)\s*>\s*
    (
      <country(\s*fullname="Turkey"|\s*capital="Istanbul"){2}\s*/>\s*
     |<country(\s*fullname="France"|\s*capital="Paris"){2}\s*/>\s*
     |<country(\s*fullname="England"|\s*capital="London"){2}\s*/>\s*
    ){3}
  </\1>$
}xs;

$xml = XMLout($ref, keyattr => ['fullname']);
is_deeply(XMLin($xml, keyattr => ['fullname']), $ref,
'encoded hash of hashes with explicit folding enabled');

like($xml, $expected, 'output as expected');


# Same again but specify name as scalar rather than array

$xml = XMLout($ref, keyattr => 'fullname');
like($xml, $expected, 'still works when keyattr is scalar');


# Same again but specify keyattr as hash rather than array

$xml = XMLout($ref, keyattr => { country => 'fullname' });
like($xml, $expected, 'still works when keyattr is hash');


# Same again but add leading '+'

$xml = XMLout($ref, keyattr => { country => '+fullname' });
like($xml, $expected, "still works when keyattr is hash with leading '+'");


# and leading '-'

$xml = XMLout($ref, keyattr => { country => '-fullname' });
like($xml, $expected, "still works when keyattr is hash with leading '-'");


# One more time but with default key folding values

# Expect:
# <opt>
#   <country name="England" capital="London" />
#   <country name="France" capital="Paris" />
#   <country name="Turkey" capital="Istanbul" />
# </opt>

$expected = qr{
  ^<(\w+)\s*>\s*
    (
      <country(\s*name="Turkey"|\s*capital="Istanbul"){2}\s*/>\s*
     |<country(\s*name="France"|\s*capital="Paris"){2}\s*/>\s*
     |<country(\s*name="England"|\s*capital="London"){2}\s*/>\s*
    ){3}
  </\1>$
}xs;

$xml = XMLout($ref);
is_deeply(XMLin($xml), $ref,
'encoded hash of hashes with default folding enabled');
like($xml, $expected, "expected output with default keyattr");


# Finally, confirm folding still works with only one nested hash

# Expect:
# <opt>
#   <country name="England" capital="London" />
# </opt>

$ref = { country => { England => { capital => 'London' } } };
$_ = XMLout($ref);
is_deeply(XMLin($_, forcearray => 1), $ref, 'single nested hash unfolded');
ok(s{\s*name="England"}{uk}s, 'attr 1 ok');
ok(s{\s*capital="London"}{uk}s, 'attr 2 ok');
ok(s{<countryukuk\s*/>\s*}{}s, 'element ok');
ok(s{^<(\w+)\s*>\s*</\1>$}{}s, 'document ok');


# Check that default XML declaration works
#
# Expect:
# <?xml version='1.0' standalone='yes'?>
# <opt one="1" />

$ref = { one => 1 };

$_ = XMLout($ref, xmldecl => 1);
is_deeply(XMLin($_), $ref, 'generated doc with XML declaration');
ok(s{^\Q<?xml version='1.0' standalone='yes'?>\E}{}s, 'XML declaration OK');
like($_, qr{^\s*<opt\s+one="1"\s*/>}s, 'data OK too');


# Check that custom XML declaration works
#
# Expect:
# <?xml version='1.0' standalone='yes'?>
# <opt one="1" />

$_ = XMLout($ref, xmldecl => "<?xml version='1.0' standalone='yes'?>");
is_deeply(XMLin($_), $ref, 'generated doc with custom XML declaration');
ok(s{^\Q<?xml version='1.0' standalone='yes'?>\E}{}s, 'XML declaration OK');
like($_, qr{^\s*<opt\s+one="1"\s*/>}s, 'data OK too');


# Check that special characters do get escaped

$ref = { a => '<A>', b => '"B"', c => '&C&' };
$_ = XMLout($ref);
is_deeply(XMLin($_), $ref, 'generated document with escaping');
ok(s{a="&lt;A&gt;"}{}s, 'angle brackets escaped OK');
ok(s{b="&quot;B&quot;"}{}s, 'double quotes escaped OK');
ok(s{c="&amp;C&amp;"}{}s, 'ampersands escaped OK');
ok(s{^<(\w+)\s*/>$}{}s, 'data OK too');


# unless we turn escaping off

$ref = { a => '<A>', b => '"B"', c => ['&C&'] };
$_ = XMLout($ref, noescape => 1);
ok(s{a="<A>"}{}s, 'generated unescaped angle brackets');
ok(s{b=""B""}{}s, 'generated unescaped double quotes');
ok(s{<c>&C&</c>}{}s, 'generated unescaped ampersands');
ok(s{^<(\w+)\s*>\s*</\1>$}{}s, 'data OK too');

# same again but with a scalar

$xml = XMLout("<scalar>", noescape => 1);
like($xml, qr{^<(\w+)><scalar></\1>}, "Unescaped scalar as expected too");

# Try encoding a circular data structure and confirm that it fails

$_ = eval {
  my $ref = { a => '1' };
  $ref->{b} = $ref;
  XMLout($ref);
};
ok(!defined($_), 'caught circular data structure');
like($@, qr/circular data structures not supported/,
'with correct error message');


# Try encoding a repetitive (but non-circular) data structure and confirm that
# it does not fail

$_ = eval {
  my $a = { alpha => 1 };
  my $ref = { a => $a, b => $a };
  XMLout($ref);
};
ok(defined($_), 'repetitive (non-circular) data structure not fatal');
like($_, qr{^
<opt>
  (
    \s*<a\s+alpha="1"\s*/>
  |
    \s*<b\s+alpha="1"\s*/>
  ){2}
\s*</opt>
}xs, 'and encodes as expected');


# Try encoding a non array/hash blessed reference and confirm that it fails

$_ = eval { my $ref = bless \*STDERR, 'BogoClass'; XMLout($ref) };
is($_, undef, 'caught blessed non array/hash reference in data structure');
like($@, qr/Can't encode a value of type: /, 'with correct error message');


# Repeat some of the above tests with named root element

# Try encoding a scalar value

$xml = XMLout("scalar", rootname => 'TOM');
ok(defined($xml), 'generated document with named root element');
is(XMLin($xml), 'scalar', 'parsed it back correctly');
like($xml, qr/^\s*<TOM>scalar<\/TOM>\s*$/si, 'XML as expected');


# Next try encoding a hash

# Expect:
# <DICK one="1" two="II" three="..." />

$_ = XMLout($hashref1, rootname => 'DICK');
is_deeply(XMLin($_), $hashref1, 'same again but encoded a hash');
ok(s/one="1"//, 'first key encoded OK');
ok(s/two="II"//, 'second key encoded OK');
ok(s/three="..."//, 'third key encoded OK');
like($_, qr/^<DICK\s+\/>/, 'XML looks OK');


# Now try encoding a hash with a nested array

$ref = {array => [qw(one two three)]};
# Expect:
# <LARRY>
#   <array>one</array>
#   <array>two</array>
#   <array>three</array>
# </LARRY>

$_ = XMLout($ref, rootname => 'LARRY');
is_deeply(XMLin($_), $ref, 'same again but with array in hash');
ok(s{<array>one</array>\s*
         <array>two</array>\s*
         <array>three</array>}{}sx,  'array encoded in correct order');
like($_, qr/^<(LARRY)\s*>\s*<\/\1>\s*$/s, 'only expected root element left');


# Now try encoding a nested hash

$ref = { value => '555 1234',
         hash1 => { one => 1 },
         hash2 => { two => 2 } };
# Expect:
# <CURLY value="555 1234">
#   <hash1 one="1" />
#   <hash2 two="2" />
# </CURLY>

$_ = XMLout($ref, rootname => 'CURLY');
is_deeply(XMLin($_), $ref, 'same again but with nested hashes');

ok(s{<hash1 one="1" />\s*}{}s, 'hash 1 encoded OK');
ok(s{<hash2 two="2" />\s*}{}s, 'hash 2 encoded OK');
like($_, qr{^<(CURLY)\s+value="555 1234"\s*>\s*</\1>\s*$}s, 'document OK');


# Now try encoding an anonymous array

$ref = [ qw(1 two III) ];
# Expect:
# <MOE>
#   <anon>1</anon>
#   <anon>two</anon>
#   <anon>III</anon>
# </MOE>

$_ = XMLout($ref, rootname => 'MOE');
is_deeply(XMLin($_), $ref, 'same again but with nested anonymous array');
like($_, qr{
  ^<(MOE)\s*>
    \s*<anon>1</anon>
    \s*<anon>two</anon>
    \s*<anon>III</anon>
  \s*</\1>\s*$}sx, 'document OK');


# Test again, this time with no root element

# Try encoding a scalar value

like(XMLout("scalar", rootname => ''),    qr/scalar\s+/s,
  'encoded scalar with no root element');
like(XMLout("scalar", rootname => undef), qr/scalar\s+/s,
  'same again but with rootname = undef');


# Next try encoding a hash

# Expect:
#   <one>1</one>
#   <two>II</two>
#   <three>...</three>

$_ = XMLout($hashref1, rootname => '');
is_deeply(XMLin("<opt>$_</opt>"), $hashref1,
  'generated doc with no root element from hash');
ok(s/<one>1<\/one>//, 'first key encoded OK');
ok(s/<two>II<\/two>//, 'second key encoded OK');
ok(s/<three>...<\/three>//, 'third key encoded OK');
like($_, qr/^\s*$/, 'document OK');


# Now try encoding a nested hash

$ref = { value => '555 1234',
         hash1 => { one => 1 },
         hash2 => { two => 2 } };
# Expect:
#   <value>555 1234</value>
#   <hash1 one="1" />
#   <hash2 two="2" />

$_ = XMLout($ref, rootname => '');
is_deeply(XMLin("<opt>$_</opt>"), $ref,
  'generated docucment with no root element from nested hashes');
ok(s{<value>555 1234<\/value>\s*}{}s, 'first element OK');
ok(s{<hash1 one="1" />\s*}{}s, 'second element OK');
ok(s{<hash2 two="2" />\s*}{}s, 'third element OK');
like($_, qr{^\s*$}s, 'document OK');


# Now try encoding an anonymous array

$ref = [ qw(1 two III) ];
# Expect:
#   <anon>1</anon>
#   <anon>two</anon>
#   <anon>III</anon>

$_ = XMLout($ref, rootname => '');
is_deeply(XMLin("<opt>$_</opt>"), $ref,
  'generated doc with no root name from array');
like($_, qr{
  ^\s*<anon>1</anon>
  \s*<anon>two</anon>
  \s*<anon>III</anon>
  \s*$}sx, 'document OK');


# Test option error handling

$_ = eval { XMLout($hashref1, searchpath => []) }; # only valid for XMLin()
ok(!defined($_), 'caught attempt to specify searchpath on XMLout');
like($@, qr/Unrecognised option:/, 'with correct error message');

$_ = eval { XMLout($hashref1, 'bogus') };
ok(!defined($_), 'caught attempt to specify odd number of option args');
like($@, qr/Options must be name=>value pairs \(odd number supplied\)/,
  'with correct error message');


# Test output to file

my $TestFile = 'testoutput.xml';
unlink($TestFile);
ok(!-e $TestFile, 'output file does not exist');

$xml = XMLout($hashref1);
eval { XMLout($hashref1, outputfile => $TestFile); };
ok(-e $TestFile, 'created xml output file');
is(ReadFile($TestFile), $xml, 'Contents match expectations');
unlink($TestFile);


# Test output to an IO handle

ok(!-e $TestFile);
eval {
  open my $fh, '>', $TestFile or die "$!";
  XMLout($hashref1, outputfile => $fh);
  $fh->close();
};
ok(-e $TestFile, 'create XML output file via IO::File');
is(ReadFile($TestFile), $xml, 'Contents match expectations');
unlink($TestFile);

# After all that, confirm that the original hashref we supplied has not
# been corrupted.

is_deeply($hashref1, $hashref2, 'original data not corrupted');


# Confirm that hash keys with leading '-' are skipped

$ref = {
  'a'  => 'one',
  '-b' => 'two',
  '-c' => {
            'one' => 1,
            'two' => 2
          }
};

$_ = XMLout($ref, rootname => 'opt');
like($_, qr{^\s*<opt\s+a="one"\s*/>\s*$}s, "skipped hashkeys with '-' prefix");


# Try a more complex unfolding with key attributes named in a hash

$ref = {
  'car' => {
    'LW1804' => {
      'option' => {
        '9926543-1167' => { 'key' => 1, 'desc' => 'Steering Wheel' }
      },
      'id' => 2,
      'make' => 'GM'
    },
    'SH6673' => {
      'option' => {
        '6389733317-12' => { 'key' => 2, 'desc' => 'Electric Windows' },
        '3735498158-01' => { 'key' => 3, 'desc' => 'Leather Seats' },
        '5776155953-25' => { 'key' => 4, 'desc' => 'Sun Roof' },
      },
      'id' => 1,
      'make' => 'Ford'
    }
  }
};

# Expect:
# <opt>
#   <car license="LW1804" id="2" make="GM">
#     <option key="1" pn="9926543-1167" desc="Steering Wheel" />
#   </car>
#   <car license="SH6673" id="1" make="Ford">
#     <option key="2" pn="6389733317-12" desc="Electric Windows" />
#     <option key="3" pn="3735498158-01" desc="Leather Seats" />
#     <option key="4" pn="5776155953-25" desc="Sun Roof" />
#   </car>
# </opt>

$_ = XMLout($ref, keyattr => { 'car' => 'license', 'option' => 'pn' });
is_deeply(XMLin($_,
  forcearray => 1,
  keyattr => { 'car' => 'license', 'option' => 'pn' }
), $ref, 'generated document from complex nested hash with unfolding');
ok(s{\s*make="GM"}{gm}s, 'element 1 attribute 1 OK');
ok(s{\s*id="2"}{gm}s, 'element 1 attribute 2 OK');
ok(s{\s*license="LW1804"}{gm}s, 'element 1 attribute 3 OK');
ok(s{\s*desc="Steering Wheel"}{opt}s, 'element 1.1 attribute 1 OK');
ok(s{\s*pn="9926543-1167"}{opt}s, 'element 1.1 attribute 2 OK');
ok(s{\s*key="1"}{opt}s, 'element 1.1 attribute 3 OK');
ok(s{\s*<cargmgmgm>\s*<optionoptoptopt\s*/>\s*</car>}{CAR}s,
  'elements 1 and 1.1 OK');
ok(s{\s*make="Ford"}{ford}s, 'element 2 attribute 1 OK');
ok(s{\s*id="1"}{ford}s, 'element 2 attribute 2 OK');
ok(s{\s*license="SH6673"}{ford}s, 'element 2 attribute 3 OK');
ok(s{\s*desc="Electric Windows"}{1}s, 'element 2.1 attribute 1 OK');
ok(s{\s*pn="6389733317-12"}{1}s, 'element 2.1 attribute 2 OK');
ok(s{\s*key="2"}{1}s, 'element 2.1 attribute 3 OK');
ok(s{\s*<option111}{<option}s, 'element 2.1 OK');
ok(s{\s*desc="Leather Seats"}{2}s, 'element 2.2 attribute 1 OK');
ok(s{\s*pn="3735498158-01"}{2}s, 'element 2.2 attribute 2 OK');
ok(s{\s*key="3"}{2}s, 'element 2.2 attribute 3 OK');
ok(s{\s*<option222}{<option}s, 'element 2.2 OK');
ok(s{\s*desc="Sun Roof"}{3}s, 'element 2.3 attribute 1 OK');
ok(s{\s*pn="5776155953-25"}{3}s, 'element 2.3 attribute 2 OK');
ok(s{\s*key="4"}{3}s, 'element 2.3 attribute 3 OK');
ok(s{\s*<option333}{<option}s, 'element 2.3 OK');
ok(s{\s*<carfordfordford>\s*(<option\s*/>\s*){3}</car>}{CAR}s, 'element 2 OK');
ok(s{^<(\w+)\s*>\s*CAR\s*CAR\s*</\1>$}{}s, 'document OK');


# Check that empty hashes translate to empty tags

$ref = {
  'one' => {
    'attr1' => 'avalue1',
    'nest1' => [ 'nvalue1' ],
    'nest2' => {}
  },
  two => {}
};

$_ = XMLout($ref);
ok(s{<nest2\s*></nest2\s*>\s*}{<NNN>}, 'nested empty hash OK');
ok(s{<nest1\s*>nvalue1</nest1\s*>\s*}{<NNN>}, 'array OK');
ok(s{<one\s*attr1\s*=\s*"avalue1">\s*}{<one>}, 'scalar OK');
ok(s{<one\s*>\s*<NNN>\s*<NNN>\s*</one>}{<nnn>}, 'nesting OK');
ok(s{<two\s*></two\s*>\s*}{<nnn>}, 'empty hash OK');
like($_, qr{^\s*<(\w+)\s*>\s*<nnn>\s*<nnn>\s*</\1\s*>\s*$}, 'document OK');


# Check undefined values generate warnings

{
  local($^W) = 1;
  my $warn = '';
  local $SIG{__WARN__} = sub { $warn = $_[0] };
  $ref = { 'one' => 1, 'two' => undef };
  my $expect = qr/^<\w+(\s+one="1"|\s+two=""){2}/;

  $_ = XMLout($ref);
  like($warn, qr/Use of uninitialized value/,
    'caught warning re uninitialised value');
  like($_, $expect, 'undef maps to any empty attribute by default');

  # unless warnings are disabled
  $^W = 0;
  $warn = '';
  $_ = XMLout($ref);
  is($warn, '', 'no warning re uninitialised value if warnings off');
  like($_, $expect, 'undef still maps to any empty attribute');
}


# Unless undef is mapped to empty elements

$ref = { 'tag' => undef };
$_ = XMLout($ref, suppressempty => undef);
like($_, qr{^\s*<(\w*)\s*>\s*<tag\s*></tag\s*>\s*</\1\s*>\s*$}s,
  'uninitialiased values successfully mapped to empty elements');


# Set suppressempty to 1 to not output anything for undef

$ref = { 'one' => 1, 'two' => undef };
$_ = XMLout($ref, suppressempty => 1, noattr => 1);
like($_, qr{^\s*<(\w*)\s*>\s*<one\s*>1</one\s*>\s*</\1\s*>\s*$}s,
  'uninitialiased values successfully skipped');


# Try undef in an array

$ref = { a => [ 'one', undef, 'three' ] };
$_ = XMLout($ref);
like($_,
  qr{
    ^\s*<(\w*)\s*>
    \s*<a\s*>one</a\s*>
    \s*<a\s*></a\s*>
    \s*<a\s*>three</a\s*>
    \s*</\1\s*>\s*$
  }xs,
  'uninitialiased value in array is empty element');


# And again with SuppressEmpty enabled

$_ = XMLout($ref, SuppressEmpty => 1);
like($_,
  qr{
    ^\s*<(\w*)\s*>
    \s*<a\s*>one</a\s*>
    \s*<a\s*>three</a\s*>
    \s*</\1\s*>\s*$
  }xs,
  'uninitialiased value in array is skipped');


# Test the keeproot option

$ref = {
  'seq' => {
    'name' => 'alpha',
    'alpha' => [ 1, 2, 3 ]
  }
};

my $xml1 = XMLout($ref, rootname => 'sequence');
my $xml2 = XMLout({ 'sequence' => $ref }, keeproot => 1);

is_deeply($xml1, $xml2, 'keeproot works as expected');


# Test that items with text content are output correctly
# Expect: <opt one="1">text</opt>

$ref = { 'one' => 1, 'content' => 'text' };

$_ = XMLout($ref);

like($_, qr{^\s*<opt\s+one="1">text</opt>\s*$}s, 'content keys mapped OK');


# Even if we change the default value for the 'contentkey' option

$ref = { 'one' => 1, 'text_content' => 'text' };

$_ = XMLout($ref, contentkey => 'text_content');

like($_, qr{^\s*<opt\s+one="1">text</opt>\s*$}s, 'even when name changed');


# and also if we add the '-' prefix

$_ = XMLout($ref, contentkey => '-text_content');

like($_, qr{^\s*<opt\s+one="1">text</opt>\s*$}s, 'even with "-" prefix');


# Confirm content key works with undef values (and no warnings)

{
  $^W = 1;
  my $warn = '';
  local $SIG{__WARN__} = sub { $warn = $_[0] };
  $_ = eval {
    $ref = {
      column => [
        { name => 'title',   content => 'A Title' },
        { name => 'sponsor', content => undef },
      ],
    };
    XMLout($ref, suppress_empty => undef, content_key => 'content');
  };
  ok(!$warn,  'no warnings with suppress_empty => undef');
  like($_, qr{^<(\w+)>
      \s*<column\s+name="title"\s*>A\sTitle</column>
      \s*<column\s+name="sponsor"\s*></column>
      \s*
      </\1>$
    }sx, "undef does not cause content tags in output"
  );
}


# Check 'noattr' option

$ref = {
  attr1  => 'value1',
  attr2  => 'value2',
  nest   => [ qw(one two three) ]
};

# Expect:
#
# <opt>
#   <attr1>value1</attr1>
#   <attr2>value2</attr2>
#   <nest>one</nest>
#   <nest>two</nest>
#   <nest>three</nest>
# </opt>
#

$_ = XMLout($ref, noattr => 1);

unlike($_, qr{=}s, 'generated document with no attributes');
is_deeply(XMLin($_), $ref, 'parses ok');
ok(s{\s*<(attr1)>value1</\1>\s*}{NEST}s, 'scalar 1 mapped ok');
ok(s{\s*<(attr2)>value2</\1>\s*}{NEST}s, 'scalar 2 mapped ok');
ok(s{\s*<(nest)>one</\1>\s*<\1>two</\1>\s*<\1>three</\1>}{NEST}s,
'array mapped ok');
like($_, qr{^<(\w+)\s*>(NEST\s*){3}</\1>$}s, 'document OK');


# Check noattr doesn't screw up keyattr

$ref = { number => {
  'twenty one' => { dec => 21, hex => '0x15' },
  'thirty two' => { dec => 32, hex => '0x20' }
  }
};

# Expect:
#
# <opt>
#   <number>
#     <dec>21</dec>
#     <word>twenty one</word>
#     <hex>0x15</hex>
#   </number>
#   <number>
#     <dec>32</dec>
#     <word>thirty two</word>
#     <hex>0x20</hex>
#   </number>
# </opt>
#

$_ = XMLout($ref, noattr => 1, keyattr => [ 'word' ]);

unlike($_, qr{=}s, 'same again but with unfolding too');
is_deeply(XMLin($_, keyattr => [ 'word' ]), $ref, 'parsed OK');
ok(s{\s*<(dec)>21</\1>\s*}{21}s, 'scalar 1.1 mapped OK');
ok(s{\s*<(hex)>0x15</\1>\s*}{21}s, 'scalar 1.2 mapped OK');
ok(s{\s*<(word)>twenty one</\1>\s*}{21}s, 'scalar 1.3 mapped OK');
ok(s{\s*<(number)>212121</\1>\s*}{NUM}s, 'element 1 OK');
ok(s{\s*<(dec)>32</\1>\s*}{32}s, 'scalar 2.1 mapped OK');
ok(s{\s*<(hex)>0x20</\1>\s*}{32}s, 'scalar 2.1 mapped OK');
ok(s{\s*<(word)>thirty two</\1>\s*}{32}s, 'scalar 2.1 mapped OK');
ok(s{\s*<(number)>323232</\1>\s*}{NUM}s, 'element 2 OK');
like($_, qr{^<(\w+)\s*>NUMNUM</\1>$}, 'document OK');


# Check grouped tags get ungrouped correctly

$ref = {
  prefix => 'before',
  dirs   => [ '/usr/bin', '/usr/local/bin' ],
  suffix => 'after',
};

# Expect:
#
# <opt>
#   <prefix>before</prefix>
#   <dirs>
#     <dir>/usr/bin</dir>
#     <dir>/usr/local/bin</dir>
#   </dirs>
#   <suffix>after</suffix>
# </opt>
#

$@ = '';
$_ = eval { XMLout($ref, grouptags => {dirs => 'dirs'}, noattr => 1); };
ok($@, 'bad GroupTags value was caught');
like("$@", qr{Bad value in GroupTags: 'dirs' => 'dirs'},
  'error message looks good');

$@ = '';
$_ = eval { XMLout($ref, grouptags => {dirs => 'dir'}, noattr => 1); };
ok(!$@, 'good GroupTags value caused no error');

ok(s{\s*<(prefix)>before</\1>\s*}{ELEM}s, 'prefix OK');
ok(s{\s*<(suffix)>after</\1>\s*}{ELEM}s,  'suffix OK');
ok(s{\s*<dir>/usr/bin</dir>\s*<dir>/usr/local/bin</dir>\s*}{LIST}s,  'list OK');
ok(s{\s*<dirs>LIST</dirs>\s*}{ELEM}s,  'group OK');
like($_, qr{^<(\w+)\s*>ELEMELEMELEM</\1>$}, 'document OK');

is_deeply($ref, {
  prefix => 'before',
  dirs   => [ '/usr/bin', '/usr/local/bin' ],
  suffix => 'after',
}, 'original ref is not messed with');

# Try again with multiple groupings

$ref = {
  dirs   => [ '/usr/bin', '/usr/local/bin' ],
  terms  => [ 'vt100', 'xterm' ],
};

# Expect:
#
# <opt>
#   <dirs>
#     <dir>/usr/bin</dir>
#     <dir>/usr/local/bin</dir>
#   </dirs>
#   <terms>
#     <term>vt100</term>
#     <term>xterm</term>
#   </terms>
# </opt>
#

$_ = XMLout($ref, grouptags => {dirs => 'dir', terms => 'term'}, noattr => 1);

ok(s{\s*<dir>/usr/bin</dir>\s*<dir>/usr/local/bin</dir>\s*}{LIST}s,  'list 1 OK');
ok(s{\s*<dirs>LIST</dirs>\s*}{ELEM}s,  'group 1 OK');
ok(s{\s*<term>vt100</term>\s*<term>xterm</term>\s*}{LIST}s,  'list 2 OK');
ok(s{\s*<terms>LIST</terms>\s*}{ELEM}s,  'group 2 OK');
like($_, qr{^<(\w+)\s*>ELEMELEM</\1>$}, 'document OK');


# Confirm unfolding and grouping work together

$ref = {
  dirs   => {
              first   => { content => '/usr/bin'       },
              second  => { content => '/usr/local/bin' },
            },
};

# Expect:
#
# <opt>
#   <dirs>
#     <dir name="first">/usr/bin</dir>
#     <dir name="second">/usr/local/bin</dir>
#   </dirs>
# </opt>
#

$_ = XMLout($ref,
  grouptags => {dirs => 'dir'}, keyattr => {dir => 'name'},
);

ok(s{\s*<dir\s+name="first">/usr/bin</dir>\s*}{ITEM}s, 'item 1 OK');
ok(s{\s*<dir\s+name="second">/usr/local/bin</dir>\s*}{ITEM}s, 'item 2 OK');
ok(s{\s*<dirs>ITEMITEM</dirs>\s*}{GROUP}s,  'group OK');
like($_, qr{^<(\w+)\s*>GROUP</\1>$}, 'document OK');


# Combine unfolding, grouping and stripped content - watch it fail :-(

$ref = {
  dirs   => {
              first   => '/usr/bin',
              second  => '/usr/local/bin'
            },
};

# Expect:
#
# <opt>
#   <dirs first="/usr/bin" second="/usr/local/bin" />
# </opt>
#

$_ = XMLout($ref,
  grouptags => {dirs => 'dir'}, keyattr => {dir => 'name'},
  contentkey => '-content'
);

like($_, qr{
  ^<(\w+)>\s*
    <dirs>\s*
      <dir
        (?:
          \s+first="/usr/bin"
         |\s+second="/usr/local/bin"
        ){2}\s*
      />\s*
    </dirs>\s*
  </\1>$
}x, 'Failed to unwrap/group stripped content - as expected');


# Check 'NoIndent' option

$ref = {
  nest   => [ qw(one two three) ]
};

# Expect:
#
# <opt><nest>one</nest><nest>two</nest><nest>three</nest></opt>
#

$_ = XMLout($ref, NoIndent => 1);

is_deeply(XMLin($_), $ref, 'parses ok');
is($_, '<opt><nest>one</nest><nest>two</nest><nest>three</nest></opt>',
'NoIndent worked ok');


# Check 'NoIndent' works with KeyAttr

$ref = {
  person => {
    bob  => { age => 25 },
    kate => { age => 22 },
  },
};

# Expect:
#
# <opt><person name="bob" age="25"><person name="kate" age="22"></opt>
#

$_ = XMLout($ref, NoIndent => 1, KeyAttr => {person => 'name'});

is_deeply(XMLin($_), $ref, 'parses ok');
like($_, qr{
  <opt>
    (
    <person(\s+name="bob"|\s+age="25"){2}\s*/>
    |<person(\s+name="kate"|\s+age="22"){2}\s*/>
    ){2}
  </opt>
}sx,
'NoIndent worked ok with KeyAttr');


# Try the 'AttrIndent' option (assume NoSort defaults to off)

$ref = {
  beta => '2',
  gamma => '3',
  alpha => '1',
  colours => {
    red => '#ff0000',
    green => '#00ff00',
  }
};

$_ = XMLout($ref, AttrIndent => 1, RootName => 'opt');

is($_, '<opt alpha="1"
     beta="2"
     gamma="3">
  <colours green="#00ff00"
           red="#ff0000" />
</opt>
', 'AttrIndent seems to work');


# Test the attribute/element sorting algorithm

$xml = q{
<opt>
  <test id="beta"  animal="elephant" vegetable="carrot" />
  <test id="gamma" animal="tiger"    vegetable="turnip" />
  <test id="alpha" animal="giraffe"  vegetable="pumpkin" />
  <box size="small" key="a" />
  <box size="medium" id="b" />
</opt>
};

$ref = XMLin($xml);

$_ = XMLout($ref, RootName => 'opt');

is($_, qq(<opt>\n) .
       qq(  <box name="a" size="small" />\n) .
       qq(  <box name="b" size="medium" />\n) .
       qq(  <test name="alpha" animal="giraffe" vegetable="pumpkin" />\n) .
       qq(  <test name="beta" animal="elephant" vegetable="carrot" />\n) .
       qq(  <test name="gamma" animal="tiger" vegetable="turnip" />\n) .
       qq(</opt>\n),
'sorting by default key attribute works');


# Try again but with specific key fields:

$ref = XMLin($xml, KeyAttr => {test => 'vegetable', box => 'size'});

$_ = XMLout($ref,
  RootName => 'opt',
  KeyAttr => {test => 'vegetable', box => 'size'}
);

is($_, qq(<opt>\n) .
       qq(  <box size="medium" id="b" />\n) .
       qq(  <box size="small" key="a" />\n) .
       qq(  <test vegetable="carrot" animal="elephant" id="beta" />\n) .
       qq(  <test vegetable="pumpkin" animal="giraffe" id="alpha" />\n) .
       qq(  <test vegetable="turnip" animal="tiger" id="gamma" />\n) .
       qq(</opt>\n),
'sorting by specified key attributes works');


# Try again but with no key fields:

$ref = XMLin($xml, KeyAttr => {});

$_ = XMLout($ref, RootName => 'opt', KeyAttr => {});

like($_, qr{^<opt>\s*
  (
    (
      <test\s+animal="elephant"\s+id="beta"\s+vegetable="carrot"\s*/>\s*
      <test\s+animal="tiger"\s+id="gamma"\s+vegetable="turnip"\s*/>\s*
      <test\s+animal="giraffe"\s+id="alpha"\s+vegetable="pumpkin"\s*/>\s*
    )
    |(
      <box\s+key="a"\s+size="small"\s*/>\s*
      <box\s+id="b"\s+size="medium"\s*/>\s*
    )
  ){2}
</opt>\s*
$}sx, 'sorting with no key attribute works');


# Check that sorting can be disabled

$@ = '';
SKIP: {
  eval { require Tie::IxHash };

  skip "Tie::IxHash not installed", 1 if $@;

  my(%hash1, %hash2);
  tie %hash1, 'Tie::IxHash', Jan => 1, Feb => 2, Mar => 3, Apr => 4, May => 5;
  tie %hash2, 'Tie::IxHash', X => { b => 2 }, A => { c => 3 }, Z => { a => 1 },
                             M => { f => 6 }, K => { e => 4 }, O => { d => 5 };
  $hash1{func} = \%hash2;

  $_ = XMLout(\%hash1, NoSort => 1, KeyAttr => {func => 'name'});

  like($_, qr{
    ^<opt\sJan="1"\sFeb="2"\sMar="3"\sApr="4"\sMay="5">\s+
      <func(\sb="2"|\sname="X"){2}\s/>\s+
      <func(\sc="3"|\sname="A"){2}\s/>\s+
      <func(\sa="1"|\sname="Z"){2}\s/>\s+
      <func(\sf="6"|\sname="M"){2}\s/>\s+
      <func(\se="4"|\sname="K"){2}\s/>\s+
      <func(\sd="5"|\sname="O"){2}\s/>\s+
    </opt>\s*$
  }sx, 'Suppressing sort worked');

}

# Check ValueAttr => {} can expand the relevant records

$ref = { one => 1, two => 2, six => 6 };

$xml = XMLout($ref, ValueAttr => { one => 'value', six => 'num' });

like($xml, qr{
    ^<opt\s+two="2"\s*>
      (
        \s*<one\s+value="1"\s*/>
      | \s*<six\s+num="6"\s*/>
      ){2}
    \s*</opt>$
  }sx, 'Correct attributes inserted when ValueAttr specified'
);

# Try out the NumericEscape option

SKIP: {
    skip "Perl 5.6 or better required", 4 unless($] >= 5.006);

    $ref = { euro => "\x{20AC}", nbsp => "\x{A0}" };

    $xml = XMLout($ref);   # Default: no numeric escaping
    my $ents = join ',', sort ($xml =~ m{&#(\d+);}g);
    is($ents, '', "No numeric escaping by default");

    $xml = XMLout($ref, NumericEscape => 0);
    $ents = join ',', sort ($xml =~ m{&#(\d+);}g);
    is($ents, '', "No numeric escaping: explicit");

    $xml = XMLout($ref, NumericEscape => 2);
    $ents = join ',', sort ($xml =~ m{&#(\d+);}g);
    is($ents, '160,8364', "Level 2 numeric escaping looks good");

    $xml = XMLout($ref, NumericEscape => 1);
    $ents = join ',', sort ($xml =~ m{&#(\d+);}g);
    is($ents, '8364', "Level 1 numeric escaping looks good");
}

# 'Stress test' with a data structure that maps to several thousand elements.
# Unfold elements with XMLout() and fold them up again with XMLin()

my $opt1 =  {};
foreach my $i (0..40) {
  foreach my $j (0..$i) {
    $opt1->{TypeA}->{$i}->{Record}->{$j} = { Hex => sprintf("0x%04X", $j) };
    $opt1->{TypeB}->{$i}->{Record}->{$j} = { Oct => sprintf("%04o", $j) };
    $opt1->{List}->[$i]->[$j] = "$i:$j";
  }
}

$xml = XMLout($opt1, keyattr => { TypeA => 'alpha', TypeB => 'beta', Record => 'id' });

my $opt2 = XMLin($xml, keyattr => { TypeA => 'alpha', TypeB => 'beta', Record => 'id' }, forcearray => 1);

is_deeply($opt1, $opt2, 'large datastructure mapped to XML and back OK');

exit(0);



