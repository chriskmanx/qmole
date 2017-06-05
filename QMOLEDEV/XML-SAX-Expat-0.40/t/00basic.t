BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use XML::SAX::Expat;
$loaded = 1;
print "ok 1\n";

# these tests are temporary...
