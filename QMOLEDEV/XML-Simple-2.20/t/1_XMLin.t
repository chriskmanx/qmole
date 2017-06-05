
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

plan tests => 131;


my $last_warning = '';

$@ = '';
eval "use XML::Simple;";
is($@, '', 'Module compiled OK');
my $version = 'unknown';
if(open my $chg, '<Changes') {
  while(<$chg>) {
    last if ($version) = $_ =~ /^([\d\._]+) /;
  }
  close($chg);
}
unless($XML::Simple::VERSION eq $version) {
  diag("Warning: XML::Simple::VERSION = $XML::Simple::VERSION (Changes version: $version)");
}


# Start by parsing an extremely simple piece of XML

my $opt = XMLin(q(<opt name1="value1" name2="value2"></opt>));

my $expected = {
                 name1 => 'value1',
                 name2 => 'value2',
               };

ok(1, "XMLin() didn't crash");
ok(defined($opt), 'and it returned a value');
is(ref($opt), 'HASH', 'and a hasref at that');
is_deeply($opt, $expected, 'matches expectations (attributes)');


# Now try a slightly more complex one that returns the same value

$opt = XMLin(q(
  <opt>
    <name1>value1</name1>
    <name2>value2</name2>
  </opt>
));
is_deeply($opt, $expected, 'same again with nested elements');


# And something else that returns the same (line break included to pick up
# missing /s bug)

$opt = XMLin(q(<opt name1="value1"
                    name2="value2" />));
is_deeply($opt, $expected, 'attributes in empty element');


# Try something with two lists of nested values

$opt = XMLin(q(
  <opt>
    <name1>value1.1</name1>
    <name1>value1.2</name1>
    <name1>value1.3</name1>
    <name2>value2.1</name2>
    <name2>value2.2</name2>
    <name2>value2.3</name2>
  </opt>)
);

is_deeply($opt, {
  name1 => [ 'value1.1', 'value1.2', 'value1.3' ],
  name2 => [ 'value2.1', 'value2.2', 'value2.3' ],
}, 'repeated child elements give arrays of scalars');


# Now a simple nested hash

$opt = XMLin(q(
  <opt>
    <item name1="value1" name2="value2" />
  </opt>)
);

is_deeply($opt, {
  item => { name1 => 'value1', name2 => 'value2' }
}, 'nested element gives hash');


# Now a list of nested hashes

$opt = XMLin(q(
  <opt>
    <item name1="value1" name2="value2" />
    <item name1="value3" name2="value4" />
  </opt>)
);
is_deeply($opt, {
  item => [
            { name1 => 'value1', name2 => 'value2' },
            { name1 => 'value3', name2 => 'value4' }
          ]
}, 'repeated child elements give list of hashes');


# Now a list of nested hashes transformed into a hash using default key names

my $string = q(
  <opt>
    <item name="item1" attr1="value1" attr2="value2" />
    <item name="item2" attr1="value3" attr2="value4" />
  </opt>
);
my $target = {
  item => {
            item1 => { attr1 => 'value1', attr2 => 'value2' },
            item2 => { attr1 => 'value3', attr2 => 'value4' }
          }
};
$opt = XMLin($string);
is_deeply($opt, $target, "array folded on default key 'name'");


# Same thing left as an array by suppressing default key names

$target = {
  item => [
            {name => 'item1', attr1 => 'value1', attr2 => 'value2' },
            {name => 'item2', attr1 => 'value3', attr2 => 'value4' }
          ]
};
my @cont_key = (contentkey => '-content');
$opt = XMLin($string, keyattr => [], @cont_key);
is_deeply($opt, $target, 'not folded when keyattr turned off');


# Same again with alternative key suppression

$opt = XMLin($string, keyattr => {}, @cont_key);
is_deeply($opt, $target, 'still works when keyattr is empty hash');


# Try the other two default key attribute names

$opt = XMLin(q(
  <opt>
    <item key="item1" attr1="value1" attr2="value2" />
    <item key="item2" attr1="value3" attr2="value4" />
  </opt>
), @cont_key);
is_deeply($opt, {
  item => {
            item1 => { attr1 => 'value1', attr2 => 'value2' },
            item2 => { attr1 => 'value3', attr2 => 'value4' }
          }
}, "folded on default key 'key'");


$opt = XMLin(q(
  <opt>
    <item id="item1" attr1="value1" attr2="value2" />
    <item id="item2" attr1="value3" attr2="value4" />
  </opt>
), @cont_key);
is_deeply($opt, {
  item => {
            item1 => { attr1 => 'value1', attr2 => 'value2' },
            item2 => { attr1 => 'value3', attr2 => 'value4' }
          }
}, "folded on default key 'id'");


# Similar thing using non-standard key names

my $xml = q(
  <opt>
    <item xname="item1" attr1="value1" attr2="value2" />
    <item xname="item2" attr1="value3" attr2="value4" />
  </opt>);

$target = {
  item => {
            item1 => { attr1 => 'value1', attr2 => 'value2' },
            item2 => { attr1 => 'value3', attr2 => 'value4' }
          }
};

$opt = XMLin($xml, keyattr => [qw(xname)], @cont_key);
is_deeply($opt, $target, "folded on non-default key 'xname'");


# And with precise element/key specification

$opt = XMLin($xml, keyattr => { 'item' => 'xname' }, @cont_key);
is_deeply($opt, $target, 'same again but keyattr set with hash');


# Same again but with key field further down the list

$opt = XMLin($xml, keyattr => [qw(wibble xname)], @cont_key);
is_deeply($opt, $target, 'keyattr as array with value in second position');


# Same again but with key field supplied as scalar

$opt = XMLin($xml, keyattr => qw(xname), @cont_key);
is_deeply($opt, $target, 'keyattr as scalar');


# Same again but with mixed-case option name

$opt = XMLin($xml, KeyAttr => qw(xname), @cont_key);
is_deeply($opt, $target, 'KeyAttr as scalar');


# Same again but with underscores in option name

$opt = XMLin($xml, key_attr => qw(xname), @cont_key);
is_deeply($opt, $target, 'key_attr as scalar');


# Weird variation, not exactly what we wanted but it is what we expected
# given the current implementation and we don't want to break it accidently

$xml = q(
<opt>
  <item id="one" value="1" name="a" />
  <item id="two" value="2" />
  <item id="three" value="3" />
</opt>
);

$target = { item => {
    'three' => { 'value' => 3 },
    'a'     => { 'value' => 1, 'id' => 'one' },
    'two'   => { 'value' => 2 }
  }
};

$opt = XMLin($xml, @cont_key);
is_deeply($opt, $target, 'fold same array on two different keys');


# Or somewhat more as one might expect

$target = { item => {
    'one'   => { 'value' => '1', 'name' => 'a' },
    'two'   => { 'value' => '2' },
    'three' => { 'value' => '3' },
  }
};
$opt = XMLin($xml, keyattr => { 'item' => 'id' }, @cont_key);
is_deeply($opt, $target, 'same again but with priority switch');


# Now a somewhat more complex test of targetting folding

$xml = q(
<opt>
  <car license="SH6673" make="Ford" id="1">
    <option key="1" pn="6389733317-12" desc="Electric Windows"/>
    <option key="2" pn="3735498158-01" desc="Leather Seats"/>
    <option key="3" pn="5776155953-25" desc="Sun Roof"/>
  </car>
  <car license="LW1804" make="GM"   id="2">
    <option key="1" pn="9926543-1167" desc="Steering Wheel"/>
  </car>
</opt>
);

$target = {
  'car' => {
    'LW1804' => {
      'id' => 2,
      'make' => 'GM',
      'option' => {
          '9926543-1167' => { 'key' => 1, 'desc' => 'Steering Wheel' }
      }
    },
    'SH6673' => {
      'id' => 1,
      'make' => 'Ford',
      'option' => {
          '6389733317-12' => { 'key' => 1, 'desc' => 'Electric Windows' },
          '3735498158-01' => { 'key' => 2, 'desc' => 'Leather Seats' },
          '5776155953-25' => { 'key' => 3, 'desc' => 'Sun Roof' }
      }
    }
  }
};

$opt = XMLin($xml, forcearray => 1,
  keyattr => { 'car' => 'license', 'option' => 'pn' }, @cont_key);
is_deeply($opt, $target, 'folded on multi-key keyattr hash');


# Now try leaving the keys in place

$target = {
  'car' => {
    'LW1804' => {
      'id' => 2,
      'make' => 'GM',
      'option' => {
          '9926543-1167' => { 'key' => 1, 'desc' => 'Steering Wheel',
                              '-pn' => '9926543-1167' }
      },
      license => 'LW1804'
    },
    'SH6673' => {
      'id' => 1,
      'make' => 'Ford',
      'option' => {
          '6389733317-12' => { 'key' => 1, 'desc' => 'Electric Windows',
                               '-pn' => '6389733317-12' },
          '3735498158-01' => { 'key' => 2, 'desc' => 'Leather Seats',
                               '-pn' => '3735498158-01' },
          '5776155953-25' => { 'key' => 3, 'desc' => 'Sun Roof',
                               '-pn' => '5776155953-25' }
      },
      license => 'SH6673'
    }
  }
};
$opt = XMLin($xml, forcearray => 1, keyattr => { 'car' => '+license', 'option' => '-pn' }, @cont_key);
is_deeply($opt, $target, "same again but with '+' prefix to copy keys");


# Confirm the stringifying references bug is fixed

$xml = q(
  <opt>
    <item>
      <name><firstname>Bob</firstname></name>
      <age>21</age>
    </item>
    <item>
      <name><firstname>Kate</firstname></name>
      <age>22</age>
    </item>
  </opt>);

$target = {
  item => [
    { age => '21', name => { firstname => 'Bob'} },
    { age => '22', name => { firstname => 'Kate'} },
  ]
};

{
  local($SIG{__WARN__}) = \&warn_handler;

  $last_warning = '';
  $opt = XMLin($xml, @cont_key);
  is_deeply($opt, $target, "did not fold on default key with non-scalar value");
  is($last_warning, '', 'no warning issued');

  $last_warning = '';
  $opt = XMLin($xml, keyattr => { item => 'name' }, @cont_key);
  is_deeply($opt, $target, "did not fold on specific key with non-scalar value");
  isnt($last_warning, '', 'warning issued as expected');
  like($last_warning,
    qr{<item> element has non-scalar 'name' key attribute},
    'text in warning is correct'
  );

  $last_warning = '';
  $opt = XMLin($xml, keyattr => [ 'name' ], @cont_key);
  is_deeply($opt, $target, "same again but with keyattr as array");
  isnt($last_warning, '', 'warning issued as expected');
  like($last_warning,
    qr{<item> element has non-scalar 'name' key attribute},
    'text in warning is correct'
  );

  $last_warning = '';
  local($^W) = 0;
  $opt = XMLin($xml, keyattr => { item => 'name' }, @cont_key);
  is_deeply($opt, $target, "did not fold on specific key with non-scalar value");
  is($last_warning, '', 'no warning issued (as expected)');

  $last_warning = '';
  $^W = 1;
  my $xitems = q(<opt>
    <item name="color">red</item>
    <item name="mass">heavy</item>
    <item nime="disposition">ornery</item>
  </opt>);
  my $items = {
    'item' => [
      { 'name' => 'color',       'content' => 'red',    },
      { 'name' => 'mass',        'content' => 'heavy',  },
      { 'nime' => 'disposition', 'content' => 'ornery', }
    ]
  };
  $opt = XMLin($xitems, keyattr => { item => 'name' }, @cont_key);
  is_deeply($opt, $items, "did not fold when element missing key attribute");
  like($last_warning, qr{Warning: <item> element has no 'name' key attribute},
    'expected warning issued');

  $last_warning = '';
  $^W = 0;
  $opt = XMLin($xitems, keyattr => { item => 'name' }, @cont_key);
  is_deeply($opt, $items, "same again");
  is($last_warning, '', 'but with no warning this time');

  $last_warning = '';
  $^W = 1;
  $xitems = q(<opt>
    <item name="color">red</item>
    <item name="mass">heavy</item>
    <item name="disposition">ornery</item>
    <item name="color">green</item>
  </opt>);
  $items = {
    'item' => {
      'color'       => 'green',
      'mass'        => 'heavy',
      'disposition' => 'ornery',
    }
  };
  $opt = XMLin($xitems, keyattr => { item => 'name' }, @cont_key);
  is_deeply($opt, $items, "folded elements despite non-unique key attribute");
  like($last_warning, qr{Warning: <item> element has non-unique value in 'name' key attribute: color},
    'expected warning issued');

  $last_warning = '';
  $opt = XMLin($xitems, keyattr => [ 'name' ], @cont_key);
  is_deeply($opt, $items, "same again but with keyattr as array");
  like($last_warning, qr{Warning: <item> element has non-unique value in 'name' key attribute: color},
    'expected warning issued');

  $last_warning = '';
  $^W = 0;
  $opt = XMLin($xitems, keyattr => { item => 'name' }, @cont_key);
  is_deeply($opt, $items, "same again");
  is($last_warning, '', 'but with no warning this time');
}


# Make sure that the root element name is preserved if we ask for it

$target = XMLin("<opt>$xml</opt>", forcearray => 1,
                keyattr => { 'car' => '+license', 'option' => '-pn' },
                @cont_key);

$opt    = XMLin(      $xml,        forcearray => 1, keeproot => 1,
                keyattr => { 'car' => '+license', 'option' => '-pn' },
                @cont_key);

is_deeply($opt, $target, 'keeproot option works');


# confirm that CDATA sections parse correctly

$xml = q{<opt><cdata><![CDATA[<greeting>Hello, world!</greeting>]]></cdata></opt>};
$opt = XMLin($xml, @cont_key);
is_deeply($opt, {
  'cdata' => '<greeting>Hello, world!</greeting>'
}, 'CDATA section parsed correctly');

$xml = q{<opt><x><![CDATA[<y>one</y>]]><![CDATA[<y>two</y>]]></x></opt>};
$opt = XMLin($xml, @cont_key);
is_deeply($opt, {
  'x' => '<y>one</y><y>two</y>'
}, 'CDATA section containing markup characters parsed correctly');


# Try parsing a named external file

$@ = '';
$opt = eval{ XMLin($XMLFile); };
is($@, '', "XMLin didn't choke on named external file");
is_deeply($opt, {
  location => 't/test1.xml'
}, 'and contents parsed as expected');


# Try parsing default external file (scriptname.xml in script directory)

$@ = '';
$opt = eval { XMLin(); };
is($@, '', "XMLin didn't choke on un-named (default) external file");
is_deeply($opt, {
  location => 't/1_XMLin.xml'
}, 'and contents parsed as expected');


# Try parsing named file in a directory in the searchpath

$@ = '';
$opt = eval {
  XMLin('test2.xml', searchpath => [
    'dir1', 'dir2', File::Spec->catdir('t', 'subdir'), @cont_key
  ] );

};
is($@, '', 'XMLin found file using searchpath');
is_deeply($opt, {
  location => 't/subdir/test2.xml'
}, 'and contents parsed as expected');


# Ensure we get expected result if file does not exist

$@ = '';
$opt = undef;
$opt = eval {
  XMLin('bogusfile.xml', searchpath => 't' ); # should 'die'
};
is($opt, undef, 'XMLin choked on nonexistant file');
like($@, qr/Could not find bogusfile.xml in/, 'with the expected message');


# same again, but with no searchpath

$@ = '';
$opt = undef;
$opt = eval { XMLin('bogusfile.xml'); };
is($opt, undef, 'nonexistant file not found in current directory');
like($@, qr/File does not exist: bogusfile.xml/, 'with the expected message');


# Confirm searchpath is ignored if filename includes directory component

$@ = '';
$opt = undef;
$opt = eval {
  XMLin(File::Spec->catfile('subdir', 'test2.xml'), searchpath => 't' );
};
is($opt, undef, 'search path ignored when pathname supplied');


# Try parsing from an IO::Handle

$@ = '';
my $fh = new IO::File;
$XMLFile = File::Spec->catfile('t', '1_XMLin.xml');  # t/1_XMLin.xml
eval {
  $fh->open($XMLFile) || die "$!";
  $opt = XMLin($fh, @cont_key);
};
is($@, '', "XMLin didn't choke on an IO::File object");
is($opt->{location}, 't/1_XMLin.xml', 'and it parsed the right file');


# Try parsing from STDIN

close(STDIN);
$@ = '';
eval {
  open(STDIN, $XMLFile) || die "$!";
  $opt = XMLin('-');
};
is($@, '', "XMLin didn't choke on STDIN ('-')");
is($opt->{location}, 't/1_XMLin.xml', 'and data parsed correctly');


# Confirm anonymous array handling works in general

$xml = q{
  <opt>
    <row>
      <anon>0.0</anon><anon>0.1</anon><anon>0.2</anon>
    </row>
    <row>
      <anon>1.0</anon><anon>1.1</anon><anon>1.2</anon>
    </row>
    <row>
      <anon>2.0</anon><anon>2.1</anon><anon>2.2</anon>
    </row>
  </opt>
};

$expected = {
  row => [
           [ '0.0', '0.1', '0.2' ],
           [ '1.0', '1.1', '1.2' ],
           [ '2.0', '2.1', '2.2' ]
         ]
};

$opt = XMLin($xml, @cont_key);
is_deeply($opt, $expected, 'anonymous arrays parsed correctly');

# Confirm it still works with array folding disabled (was a bug)

$opt = XMLin($xml, keyattr => [], @cont_key);
is_deeply($opt, $expected, 'anonymous arrays parsed correctly');


# Confirm anonymous array handling works in special top level case

$opt = XMLin(q{
  <opt>
    <anon>one</anon>
    <anon>two</anon>
    <anon>three</anon>
  </opt>
}, @cont_key);
is_deeply($opt, [
  qw(one two three)
], 'top level anonymous array returned arrayref');


$opt = XMLin(q(
  <opt>
    <anon>1</anon>
    <anon>
      <anon>2.1</anon>
      <anon>
        <anon>2.2.1</anon>
        <anon>2.2.2</anon>
      </anon>
    </anon>
  </opt>
), @cont_key);
is_deeply($opt, [
  1,
  [
   '2.1', [ '2.2.1', '2.2.2']
  ]
], 'nested anonymous arrays parsed correctly');


# Check for the dreaded 'content' attribute

$xml = q(
  <opt>
    <item attr="value">text</item>
  </opt>
);

$opt = XMLin($xml);
is_deeply($opt, {
  item => {
            content => 'text',
            attr    => 'value'
          }
}, "'content' key appears as expected");


# And check that we can change its name if required

$opt = XMLin($xml, contentkey => 'text_content');
is_deeply($opt, {
  item => {
            text_content => 'text',
            attr         => 'value'
          }
}, "'content' key successfully renamed to 'text'");


# Check that it doesn't get screwed up by forcearray option

$xml = q(<opt attr="value">text content</opt>);

$opt = XMLin($xml, forcearray => 1);
is_deeply($opt, {
  'attr'    => 'value',
  'content' => 'text content'
}, "'content' key not munged by forcearray");


# Test that we can force all text content to parse to hash values

$xml = q(<opt><x>text1</x><y a="2">text2</y></opt>);
$opt = XMLin($xml, forcecontent => 1);
is_deeply($opt, {
    'x' => {           'content' => 'text1' },
    'y' => { 'a' => 2, 'content' => 'text2' }
}, 'gratuitous use of content key works as expected');


# And that this is compatible with changing the key name

$opt = XMLin($xml, forcecontent => 1, contentkey => '0');
is_deeply($opt, {
    'x' => {           0 => 'text1' },
    'y' => { 'a' => 2, 0 => 'text2' }
}, "even when we change it's name to 'text'");


# Confirm that spurious 'content' keys are *not* eliminated after array folding

$xml = q(<opt><x y="one">First</x><x y="two">Second</x></opt>);
$opt = XMLin($xml, forcearray => [ 'x' ], keyattr => {x => 'y'});
is_deeply($opt, {
  x => {
    one => { content => 'First'  },
    two => { content => 'Second' },
  }
}, "spurious content keys not eliminated after folding");


# unless we ask nicely

$xml = q(<opt><x y="one">First</x><x y="two">Second</x></opt>);
$opt = XMLin(
  $xml, forcearray => [ 'x' ], keyattr => {x => 'y'}, contentkey => '-content'
);
is_deeply($opt, {
  x => {
    one => 'First',
    two => 'Second',
  }
}, "spurious content keys not eliminated after folding");


# Check that mixed content parses in the weird way we expect

$xml = q(<opt>
  <p1 class="mixed">Text with a <b>bold</b> word</p1>
  <p2>Mixed <b>but</b> no attributes</p2>
</opt>);

is_deeply(XMLin($xml, @cont_key), {
  'p1' => {
    'content' => [ 'Text with a ', ' word' ],
    'class' => 'mixed',
    'b' => 'bold'
  },
  'p2' => {
    'content' => [ 'Mixed ', ' no attributes' ],
    'b' => 'but'
  }
}, "mixed content doesn't work - no surprises there");


# Confirm single nested element rolls up into a scalar attribute value

$string = q(
  <opt>
    <name>value</name>
  </opt>
);
$opt = XMLin($string);
is_deeply($opt, {
  name => 'value'
}, 'nested element rolls up to scalar');


# Unless 'forcearray' option is specified

$opt = XMLin($string, forcearray => 1, @cont_key);
is_deeply($opt, {
  name => [ 'value' ]
}, 'except when forcearray is enabled');


# Confirm array folding of single nested hash

$string = q(<opt>
  <inner name="one" value="1" />
</opt>);

$opt = XMLin($string, forcearray => 1, @cont_key);
is_deeply($opt, {
  'inner' => { 'one' => { 'value' => 1 } }
}, 'array folding works with single nested hash');


# But not without forcearray option specified

$opt = XMLin($string, forcearray => 0, @cont_key);
is_deeply($opt, {
  'inner' => { 'name' => 'one', 'value' => 1 }
}, 'but not if forcearray is turned off');


# Test advanced features of forcearray

$xml = q(<opt zero="0">
  <one>i</one>
  <two>ii</two>
  <three>iii</three>
  <three>3</three>
  <three>c</three>
</opt>
);

$opt = XMLin($xml, forcearray => [ 'two' ], @cont_key);
is_deeply($opt, {
  'zero' => '0',
  'one' => 'i',
  'two' => [ 'ii' ],
  'three' => [ 'iii', 3, 'c' ]
}, 'selective application of forcearray successful');


# Test forcearray regexes

$xml = q(<opt zero="0">
  <one>i</one>
  <two>ii</two>
  <three>iii</three>
  <four>iv</four>
  <five>v</five>
</opt>
);

$opt = XMLin($xml, forcearray => [ qr/^f/, 'two', qr/n/ ], @cont_key);
is_deeply($opt, {
  'zero'  => '0',
  'one'   => [ 'i'  ],
  'two'   => [ 'ii' ],
  'three' => 'iii',
  'four'  => [ 'iv' ],
  'five'  => [ 'v'  ],
}, 'forcearray using regex successful');


# Same again but a single regexp rather than in an arrayref

$opt = XMLin($xml, forcearray => qr/^f|e$/, @cont_key);
is_deeply($opt, {
  'zero'  => '0',
  'one'   => [ 'i'  ],
  'two'   =>   'ii',
  'three' => [ 'iii'],
  'four'  => [ 'iv' ],
  'five'  => [ 'v'  ],
}, 'forcearray using a single regex successful');


# Test 'noattr' option

$xml = q(<opt name="user" password="foobar">
  <nest attr="value">text</nest>
</opt>
);

$opt = XMLin($xml, noattr => 1, @cont_key);
is_deeply($opt, { nest => 'text' }, 'attributes successfully skipped');


# And make sure it doesn't screw up array folding

$xml = q{<opt>
  <item><key>a</key><value>alpha</value></item>
  <item><key>b</key><value>beta</value></item>
  <item><key>g</key><value>gamma</value></item>
</opt>
};


$opt = XMLin($xml, noattr => 1, @cont_key);
is_deeply($opt, {
 'item' => {
    'a' => { 'value' => 'alpha' },
    'b' => { 'value' => 'beta' },
    'g' => { 'value' => 'gamma' }
  }
}, 'noattr does not intefere with array folding');


# Confirm empty elements parse to empty hashrefs

$xml = q(<body>
  <name>bob</name>
  <outer attr="value">
    <inner1 />
    <inner2></inner2>
  </outer>
</body>);

$opt = XMLin($xml, noattr => 1, @cont_key);
is_deeply($opt, {
  'name' => 'bob',
  'outer' => {
    'inner1' => {},
    'inner2' => {}
  }
}, 'empty elements parse to hashrefs');


# Unless 'suppressempty' is enabled

$opt = XMLin($xml, noattr => 1, suppressempty => 1, @cont_key);
is_deeply($opt, { 'name' => 'bob', }, 'or are suppressed');


# Check behaviour when 'suppressempty' is set to to undef;

$opt = XMLin($xml, noattr => 1, suppressempty => undef, @cont_key);
is_deeply($opt, {
  'name' => 'bob',
  'outer' => {
    'inner1' => undef,
    'inner2' => undef
  }
}, "or parse to 'undef'");

# Check behaviour when 'suppressempty' is set to to empty string;

$opt = XMLin($xml, noattr => 1, suppressempty => '', @cont_key);
is_deeply($opt, {
  'name' => 'bob',
  'outer' => {
    'inner1' => '',
    'inner2' => ''
  }
}, 'or parse to an empty string');

# Confirm completely empty XML parses to undef with 'suppressempty'

$xml = q(<body>
  <outer attr="value">
    <inner1 />
    <inner2></inner2>
  </outer>
</body>);

$opt = XMLin($xml, noattr => 1, suppressempty => 1, @cont_key);
is($opt, undef, 'empty document parses to undef');


# Confirm nothing magical happens with grouped elements

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir>/usr/bin</dir>
    <dir>/usr/local/bin</dir>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml);
is_deeply($opt, {
  prefix => 'before',
  dirs   => {
              dir => [ '/usr/bin', '/usr/local/bin' ]
            },
  suffix => 'after',
}, 'grouped tags parse normally');


# unless we specify how the grouping works

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir>/usr/bin</dir>
    <dir>/usr/local/bin</dir>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml, grouptags => {dirs => 'dir'} );
is_deeply($opt, {
  prefix => 'before',
  dirs   => [ '/usr/bin', '/usr/local/bin' ],
  suffix => 'after',
}, 'disintermediation of grouped tags works');


# try again with multiple groupings

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir>/usr/bin</dir>
    <dir>/usr/local/bin</dir>
  </dirs>
  <infix>between</infix>
  <terms>
    <term>vt100</term>
    <term>xterm</term>
  </terms>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml, grouptags => {dirs => 'dir', terms => 'term'} );
is_deeply($opt, {
  prefix => 'before',
  dirs   => [ '/usr/bin', '/usr/local/bin' ],
  infix  => 'between',
  terms  => [ 'vt100', 'xterm' ],
  suffix => 'after',
}, 'disintermediation works with multiple groups');


# confirm folding and ungrouping work together

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir name="first">/usr/bin</dir>
    <dir name="second">/usr/local/bin</dir>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml, keyattr => {dir => 'name'}, grouptags => {dirs => 'dir'} );
is_deeply($opt, {
  prefix => 'before',
  dirs   => {
              first  => { content => '/usr/bin' },
              second => { content => '/usr/local/bin' },
            },
  suffix => 'after',
}, 'folding and ungrouping work together');


# confirm folding, ungrouping and content stripping work together

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir name="first">/usr/bin</dir>
    <dir name="second">/usr/local/bin</dir>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml,
  contentkey => '-text',
  keyattr    => {dir => 'name'},
  grouptags  => {dirs => 'dir'}
);
is_deeply($opt, {
  prefix => 'before',
  dirs   => {
              first  => '/usr/bin',
              second => '/usr/local/bin',
            },
  suffix => 'after',
}, 'folding, ungrouping and content stripping work together');


# confirm folding fails as expected even with ungrouping but (no forcearray)

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir name="first">/usr/bin</dir>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml,
  contentkey => '-text',
  keyattr    => {dir => 'name'},
  grouptags  => {dirs => 'dir'}
);
is_deeply($opt, {
  prefix => 'before',
  dirs   => { name => 'first', text => '/usr/bin'},
  suffix => 'after',
}, 'folding without forcearray but with ungrouping fails as expected');


# but works with forcearray enabled

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <dir name="first">/usr/bin</dir>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml,
  contentkey => '-text',
  forcearray => [ 'dir' ],
  keyattr    => {dir => 'name'},
  grouptags  => {dirs => 'dir'}
);
is_deeply($opt, {
  prefix => 'before',
  dirs   => {'first' => '/usr/bin'},
  suffix => 'after',
}, 'folding with forcearray and ungrouping works');


# Test variable expansion - when no variables are defined

$xml = q(<opt>
  <file name="config_file">${conf_dir}/appname.conf</file>
  <file name="log_file">${log_dir}/appname.log</file>
  <file name="debug_file">${log_dir}/appname.dbg</file>
  <opt docs="${have_docs}" />
  <bogus value="${undef}" />
</opt>);

$opt = XMLin($xml, contentkey => '-content');
is_deeply($opt, {
  file => {
    config_file => '${conf_dir}/appname.conf',
    log_file    => '${log_dir}/appname.log',
    debug_file  => '${log_dir}/appname.dbg',
  },
  opt => { docs => '${have_docs}' },
  bogus => { value => '${undef}' }
}, 'undefined variables are left untouched');


# try again but with variables defined in advance

$opt = XMLin($xml,
  contentkey => '-content',
  variables  => { conf_dir => '/etc', log_dir => '/var/log',
                  have_docs => 'true' }
);
is_deeply($opt, {
  file => {
    config_file => '/etc/appname.conf',
    log_file    => '/var/log/appname.log',
    debug_file  => '/var/log/appname.dbg',
  },
  opt => { docs => 'true' },
  bogus => { value => '${undef}' }
}, 'substitution of pre-defined variables works');


# now try defining them in the XML

$xml = q(<opt>
  <dir xsvar="conf_dir">/etc</dir>
  <dir xsvar="log_dir">/var/log</dir>
  <cfg xsvar="have_docs">false</cfg>
  <cfg xsvar="host.domain">search.perl.org</cfg>
  <cfg xsvar="bad/name">bogus</cfg>
  <file name="config_file">${conf_dir}/appname.conf</file>
  <file name="log_file">${log_dir}/appname.log</file>
  <file name="debug_file">${log_dir}/appname.dbg</file>
  <file name="bogus_file">${bad/name}</file>
  <opt docs="${have_docs}" />
  <site url="http://${host.domain}/" />
</opt>);

$opt = XMLin($xml, contentkey => '-content', varattr => 'xsvar');
is_deeply($opt, {
  file => {
    config_file => '/etc/appname.conf',
    log_file    => '/var/log/appname.log',
    debug_file  => '/var/log/appname.dbg',
    bogus_file  => '${bad/name}',            # '/' is not valid in a var name
  },
  opt           => { docs => 'false' },
  site          => { url => 'http://search.perl.org/' },
  dir           => [
                     { xsvar => 'conf_dir', content => '/etc'     },
                     { xsvar => 'log_dir',  content => '/var/log' },
                   ],
  cfg           => [
                     { xsvar => 'have_docs',   content => 'false'  },
                     { xsvar => 'host.domain', content => 'search.perl.org' },
                     { xsvar => 'bad/name',    content => 'bogus'  },
                   ],
}, 'variables defined in XML work');


# confirm that variables in XML are merged with pre-defined ones

$xml = q(<opt>
  <dir xsvar="log_dir">/var/log</dir>
  <file name="config_file">${conf_dir}/appname.conf</file>
  <file name="log_file">${log_dir}/appname.log</file>
  <file name="debug_file">${log_dir}/appname.dbg</file>
</opt>);

$opt = XMLin($xml,
  contentkey => '-content',
  varattr    => 'xsvar',
  variables  => { conf_dir => '/etc', log_dir => '/tmp' }
);
is_deeply($opt, {
  file => {
    config_file => '/etc/appname.conf',
    log_file    => '/var/log/appname.log',
    debug_file  => '/var/log/appname.dbg',
  },
  dir           => { xsvar => 'log_dir',  content => '/var/log' },
}, 'variables defined in XML merged successfully with predefined vars');


# confirm that a variables are expanded in variable definitions

$xml = q(<opt>
  <dirs>
    <dir name="prefix">/usr/local/apache</dir>
    <dir name="exec_prefix">${prefix}</dir>
    <dir name="bin_dir">${exec_prefix}/bin</dir>
  </dirs>
</opt>);

$opt = XMLin($xml,
  contentkey => '-content',
  varattr    => 'name',
  grouptags  => { dirs => 'dir' },
);
is_deeply($opt, {
  dirs => {
    prefix      => '/usr/local/apache',
    exec_prefix => '/usr/local/apache',
    bin_dir     => '/usr/local/apache/bin',
  }
}, 'variables are expanded in later variable definitions');


# Confirm only a hash is acceptable to grouptags and variables

$@ = '';
$_ = eval { $opt = XMLin($xml, grouptags  => [ 'dir' ]); };
ok(!defined($_), 'grouptags requires a hash');
like($@, qr/Illegal value for 'GroupTags' option - expected a hashref/,
'with correct error message');

$@ = '';
$_ = eval { $opt = XMLin($xml, variables  => [ 'dir' ]); };
ok(!defined($_), 'variables requires a hash');
like($@, qr/Illegal value for 'Variables' option - expected a hashref/,
'with correct error message');


# Try to disintermediate on the wrong child key

$xml = q(<opt>
  <prefix>before</prefix>
  <dirs>
    <lib>/usr/bin</lib>
    <lib>/usr/local/bin</lib>
  </dirs>
  <suffix>after</suffix>
</opt>);

$opt = XMLin($xml, grouptags => {dirs => 'dir'} );
is_deeply($opt, {
  prefix => 'before',
  dirs   => { lib => [ '/usr/bin', '/usr/local/bin' ] },
  suffix => 'after',
}, 'disintermediation using wrong child key - as expected');


# Test option error handling

$@='';
$_ = eval { XMLin('<x y="z" />', rootname => 'fred') }; # not valid for XMLin()
is($_, undef, 'invalid options are trapped');
like($@, qr/Unrecognised option:/, 'with correct error message');

$@='';
$_ = eval { XMLin('<x y="z" />', 'searchpath') };
is($_, undef, 'invalid number of options are trapped');
like($@, qr/Options must be name=>value pairs \(odd number supplied\)/,
'with correct error message');


# Test the NormaliseSpace option

$xml = q(<opt>
  <user name="  Joe
  Bloggs  " id="  one  two "/>
  <user>
    <name>  Jane
    Doe </name>
    <id>
    three
    four
    </id>
  </user>
</opt>);

$opt = XMLin($xml, KeyAttr => [ 'name' ], NormaliseSpace => 1);
ok(ref($opt->{user}) eq 'HASH', "NS-1: folding OK");
ok(exists($opt->{user}->{'Joe Bloggs'}), "NS-2: space normalised in hash key");
ok(exists($opt->{user}->{'Jane Doe'}), "NS-3: space normalised in hash key");
like($opt->{user}->{'Jane Doe'}->{id}, qr{^\s\s+three\s\s+four\s\s+$}s,
  "NS-4: space not normalised in hash value");

$opt = XMLin($xml, KeyAttr => { user => 'name' }, NormaliseSpace => 1);
ok(ref($opt->{user}) eq 'HASH', "NS-1a: folding OK");
ok(exists($opt->{user}->{'Joe Bloggs'}), "NS-2a: space normalised in hash key");
ok(exists($opt->{user}->{'Jane Doe'}), "NS-3a: space normalised in hash key");
like($opt->{user}->{'Jane Doe'}->{id}, qr{^\s\s+three\s\s+four\s\s+$}s,
  "NS-4a: space not normalised in hash value");

$opt = XMLin($xml, KeyAttr => [ 'name' ], NormaliseSpace => 2);
ok(ref($opt->{user}) eq 'HASH', "NS-5: folding OK");
ok(exists($opt->{user}->{'Joe Bloggs'}), "NS-6: space normalised in hash key");
like($opt->{user}->{'Joe Bloggs'}->{id}, qr{^one\stwo$}s,
  "NS-7: space normalised in attribute value");
ok(exists($opt->{user}->{'Jane Doe'}), "NS-8: space normalised in hash key");
like($opt->{user}->{'Jane Doe'}->{id}, qr{^three\sfour$}s,
  "NS-9: space normalised in element text content");

# confirm NormaliseSpace works in anonymous arrays too

$xml = q(<opt>
  <anon>  one  two </anon><anon> three
  four  five </anon><anon> six </anon><anon> seveneightnine </anon>
</opt>);

$opt = XMLin($xml, NormaliseSpace => 2);
is_deeply($opt, [   'one two', 'three four five', 'six', 'seveneightnine' ],
  "NS-10: space normalised in anonymous array");

# Check that American speeling works too

$opt = XMLin($xml, NormalizeSpace => 2);
is_deeply($opt, [   'one two', 'three four five', 'six', 'seveneightnine' ],
  "NS-11: space normalized in anonymous array");

# Check that attributes called 'value' are not special

$xml = q(<graphics>
  <today value="today.png"/>
  <nav-prev value="prev.png"/>
  <nav-home value="home.png"/>
  <nav-next value="next.png"/>
</graphics>);

$opt = XMLin($xml);

is_deeply($opt, {
  'today'    => { value => "today.png" },
  'nav-prev' => { value => "prev.png"  },
  'nav-home' => { value => "home.png"  },
  'nav-next' => { value => "next.png"  },
}, "Nothing special about 'value' attributes");

# Now turn on the ValueAttr option and try again

$opt = XMLin($xml, ValueAttr => [ 'value' ]);

is_deeply($opt, {
  'today'    => "today.png",
  'nav-prev' => "prev.png",
  'nav-home' => "home.png",
  'nav-next' => "next.png",
}, "ValueAttr as arrayref works");

# Try with a list of different ValueAttr names

$xml = q(<graphics>
  <today xxx="today.png"/>
  <nav-prev yyy="prev.png"/>
  <nav-home zzz="home.png"/>
  <nav-next value="next.png"/>
</graphics>);

$opt = XMLin($xml, ValueAttr => [ qw(xxx yyy zzz) ]);

is_deeply($opt, {
  'today'    => "today.png",
  'nav-prev' => "prev.png",
  'nav-home' => "home.png",
  'nav-next' => { value => "next.png" },
}, "ValueAttr as arrayref works");

# Try specifying ValueAttr as a hashref

$xml = q(<graphics>
  <today xxx="today.png"/>
  <nav-prev value="prev.png"/>
  <nav-home yyy="home.png"/>
  <nav-next value="next.png"/>
</graphics>);

$opt = XMLin($xml,
  ValueAttr => {
    'today'    => 'xxx',
    'nav-home' => 'yyy',
    'nav-next' => 'value'
  }
);

is_deeply($opt, {
  'today'    => "today.png",
  'nav-prev' => { value => "prev.png" },
  'nav-home' => "home.png",
  'nav-next' => "next.png",
}, "ValueAttr as hashref works too");

# Confirm that there's no conflict with KeyAttr or ContentKey defaults

$xml = q(<graphics>
  <today value="today.png"/>
  <animal name="lion" age="7"/>
  <animal name="elephant" age="97"/>
  <colour rgb="#FF0000">red</colour>
</graphics>);

$opt = XMLin($xml, ValueAttr => { 'today'    => 'value' });

is_deeply($opt, {
  today  => 'today.png',
  animal => {
    lion     => { age =>  7 },
    elephant => { age => 97 },
  },
  colour => { rgb => '#FF0000', content => 'red' },
}, "ValueAttr as hashref works too");

# Now for a 'real world' test, try slurping in an SRT config file

$opt = XMLin(File::Spec->catfile('t', 'srt.xml'),
  forcearray => 1, @cont_key
);
$target = {
  'global' => [
    {
      'proxypswd' => 'bar',
      'proxyuser' => 'foo',
      'exclude' => [
        '/_vt',
        '/save\\b',
        '\\.bak$',
        '\\.\\$\\$\\$$'
      ],
      'httpproxy' => 'http://10.1.1.5:8080/',
      'tempdir' => 'C:/Temp'
    }
  ],
  'pubpath' => {
    'test1' => {
      'source' => [
        {
          'label' => 'web_source',
          'root' => 'C:/webshare/web_source'
        }
      ],
      'title' => 'web_source -> web_target1',
      'package' => {
        'images' => { 'dir' => 'wwwroot/images' }
      },
      'target' => [
        {
          'label' => 'web_target1',
          'root' => 'C:/webshare/web_target1',
          'temp' => 'C:/webshare/web_target1/temp'
        }
      ],
      'dir' => [ 'wwwroot' ]
    },
    'test2' => {
      'source' => [
        {
          'label' => 'web_source',
          'root' => 'C:/webshare/web_source'
        }
      ],
      'title' => 'web_source -> web_target1 & web_target2',
      'package' => {
        'bios' => { 'dir' => 'wwwroot/staff/bios' },
        'images' => { 'dir' => 'wwwroot/images' },
        'templates' => { 'dir' => 'wwwroot/templates' }
      },
      'target' => [
        {
          'label' => 'web_target1',
          'root' => 'C:/webshare/web_target1',
          'temp' => 'C:/webshare/web_target1/temp'
        },
        {
          'label' => 'web_target2',
          'root' => 'C:/webshare/web_target2',
          'temp' => 'C:/webshare/web_target2/temp'
        }
      ],
      'dir' => [ 'wwwroot' ]
    },
    'test3' => {
      'source' => [
        {
          'label' => 'web_source',
          'root' => 'C:/webshare/web_source'
        }
      ],
      'title' => 'web_source -> web_target1 via HTTP',
      'addexclude' => [ '\\.pdf$' ],
      'target' => [
        {
          'label' => 'web_target1',
          'root' => 'http://127.0.0.1/cgi-bin/srt_slave.plx',
          'noproxy' => 1
        }
      ],
      'dir' => [ 'wwwroot' ]
    }
  }
};
is_deeply($opt, $target, 'successfully read an SRT config file');


exit(0);


sub warn_handler {
  $last_warning = $_[0];
}
