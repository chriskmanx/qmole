use strict;
use warnings;

use Test;
BEGIN { plan tests => 4 }

use XML::SAX::PurePerl;

my $handler = CDataHandler->new();
ok($handler);

my $parser = XML::SAX::PurePerl->new(Handler => $handler);
ok($parser);

$parser->parse_string('<code><![CDATA[<crackers & cheese>]]></code>');
ok(1); # parser didn't die

my $expected = '<crackers & cheese>';
ok($handler->cbuffer, $expected);

exit;


package CDataHandler;

use base 'XML::SAX::Base';

sub start_document { shift->{_buf} = '';             }
sub characters     { shift->{_buf} .= shift->{Data}; }
sub cbuffer        { shift->{_buf};                  }

