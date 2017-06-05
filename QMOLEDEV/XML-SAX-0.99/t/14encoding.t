use Test;
BEGIN { $tests = 0;
    if ($] >= 5.007002) { $tests = 9 }
    plan tests => $tests;
}
if ($tests) {
use XML::SAX::PurePerl;

my $handler = TestHandler->new(); # see below for the TestHandler class
ok($handler);

my $parser = XML::SAX::PurePerl->new(Handler => $handler);
ok($parser);

# warn("utf-16\n");
# verify that the first element is correctly decoded
$handler->{test_elements} = [ "\x{9031}\x{5831}" ];
$parser->parse_uri("testfiles/utf-16.xml");
ok(1);

# warn("utf-16le\n");
$handler->{test_elements} = [ "foo" ];
$parser->parse_uri("testfiles/utf-16le.xml");
ok(1);

# warn("koi8_r\n");
$parser->parse_uri("testfiles/koi8_r.xml");
ok(1);

# warn("8859-1\n");
$parser->parse_uri("testfiles/iso8859_1.xml");
ok(1);

# warn("8859-2\n");
$parser->parse_uri("testfiles/iso8859_2.xml");
ok(1);
}

package TestHandler;
use XML::SAX::PurePerl::DebugHandler;
use base qw(XML::SAX::PurePerl::DebugHandler);
use Test;

sub start_element {
    my $self = shift;
    if ($self->{test_elements} and
        my $value = pop @{$self->{test_elements}}) {
        ok($_[0]->{Name}, $value);
    }
    $self->SUPER::start_element(@_);
}

1;
