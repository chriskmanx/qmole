
use strict;
use warnings;
use Test::More;
use File::Spec;
use IO::File;


eval { require XML::SAX; };
if($@) {
  plan skip_all => 'no XML::SAX';
}

eval { require XML::NamespaceSupport; };
if($@) {
  plan skip_all => "no XML::NamespaceSupport";
}
if($XML::NamespaceSupport::VERSION < 1.04) {
  plan skip_all => "XML::NamespaceSupport is too old (upgrade to 1.04 or better)";
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
#                      T E S T   R O U T I N E S
##############################################################################

use XML::Simple;

# Force default behaviour of using SAX parser if it is available (which it
# is or we wouldn't be here).

$XML::Simple::PREFERRED_PARSER = '';

# Confirm that by default qnames are not expanded on input

my $xml = q(<config xmlns:perl="http://www.perl.com/">
  <perl:list count="3" perl:type="array">
    <item>one</item>
    <item>two</item>
    <item>three</item>
    <test xmlns:perl="http://www.microsoft.com" perl:tm="trademark" />
  </perl:list>
</config>);

my $expected = {
  'perl:list' => {
    'count' => '3',
    'item' => [
      'one',
      'two',
      'three'
    ],
    'perl:type' => 'array',
    'test' => {
      'xmlns:perl' => 'http://www.microsoft.com',
      'perl:tm' => 'trademark',
    }
  },
  'xmlns:perl' => 'http://www.perl.com/'
};

my $opt = XMLin($xml);
is_deeply($opt, $expected, 'qnames are not expanded by default');


# Try again with nsexpand option set

$expected = {
  '{http://www.perl.com/}list' => {
    'count' => '3',
    'item' => [
      'one',
      'two',
      'three'
    ],
    '{http://www.perl.com/}type' => 'array',
    'test' => {
      '{http://www.microsoft.com}tm' => 'trademark',
      '{http://www.w3.org/2000/xmlns/}perl' => 'http://www.microsoft.com'
    }
  },
  '{http://www.w3.org/2000/xmlns/}perl' => 'http://www.perl.com/'
};

$opt = XMLin($xml, nsexpand => 1);
is_deeply($opt, $expected, 'qnames are expanded on request');


# Confirm that output expansion does not occur by default

$opt = {
  '{http://www.w3.org/2000/xmlns/}perl' => 'http://www.perl.com/',
  '{http://www.perl.com/}attr' => 'value',
  'bare' => 'Beer!',
  '{http://www.perl.com/}element' => [ 'data' ],
};

$xml = XMLout($opt);
like($xml, qr{
  ^\s*<opt
  (\s+{http://www.w3.org/2000/xmlns/}perl="http://www.perl.com/"
  |\s+{http://www.perl.com/}attr="value"
  |\s+bare="Beer!"){3}
  \s*>
  \s*<{http://www.perl.com/}element\s*>data</{http://www.perl.com/}element\s*>
  \s*</opt>
  \s*$
}sx, 'clarkian names not converted to qnames on output by default');


# Confirm nsexpand option works on output

$xml = XMLout($opt, nsexpand => 1);
ok($xml =~ m{
  ^\s*<opt
  (\s+xmlns:perl="http://www.perl.com/"
  |\s+perl:attr="value"
  |\s+bare="Beer!"){3}
  \s*>
  \s*<perl:element\s*>data</perl:element\s*>
  \s*</opt>
  \s*$
}sx, 'clarkian names are converted to qnames on output on request');


# Check that default namespace is correctly read in ...

$xml = q(<opt xmlns="http://www.orgsoc.org/">
  <list>
    <member>Tom</member>
    <member>Dick</member>
    <member>Larry</member>
  </list>
</opt>
);

$expected = {
  'xmlns' => 'http://www.orgsoc.org/',
  '{http://www.orgsoc.org/}list' => {
    '{http://www.orgsoc.org/}member' => [ 'Tom', 'Dick', 'Larry' ],
  }
};

$opt = XMLin($xml, nsexpand => 1);
is_deeply($opt, $expected, 'expansion of default namespace works');


# ... and written out

$xml = XMLout($opt, nsexpand => 1);
like($xml, qr{
  ^\s*<opt
  \s+xmlns="http://www.orgsoc.org/"
  \s*>
  \s*<list>
  \s*<member>Tom</member>
  \s*<member>Dick</member>
  \s*<member>Larry</member>
  \s*</list>
  \s*</opt>
  \s*$
}sx, 'default namespaces are output correctly too');


# Check that the autogeneration of namespaces works as we expect

$opt = {
  'xmlns' => 'http://www.orgsoc.org/',
  '{http://www.orgsoc.org/}list' => {
    '{http://www.orgsoc.org/}member' => [ 'Tom', 'Dick', 'Larry' ],
    '{http://www.phantom.com/}director' => [ 'Bill', 'Ben' ],
  }
};

$xml = XMLout($opt, nsexpand => 1);
my $prefix = '';
if($xml =~ m{<list\s+xmlns:(\w+)="http://www.phantom.com/"\s*>}) {
  $prefix = $1;
}
  # regex match split in two to workaround 5.8.1/utf8/regex match prob
like($xml, qr{
  \s*<opt
  \s+xmlns="http://www.orgsoc.org/"
  \s*>
  .*?
  </list>
  \s*</opt>
}sx, 'namespace prefixes are generated automatically (part 1)');

like($xml, qr{
  (\s*<member>Tom</member>
   \s*<member>Dick</member>
   \s*<member>Larry</member>
  |\s*<${prefix}:director>Bill</${prefix}:director>
   \s*<${prefix}:director>Ben</${prefix}:director>){2}
  #\s*</list>
}sx, 'namespace prefixes are generated automatically (part 2)');


exit(0);

