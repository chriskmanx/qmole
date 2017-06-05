use Test;
BEGIN { plan tests => 2 }
END { ok($loaded == 2) }
use XML::SAX;
$loaded++;

use XML::SAX::PurePerl;
$loaded++;

ok(XML::SAX->VERSION eq XML::SAX::PurePerl->VERSION);

