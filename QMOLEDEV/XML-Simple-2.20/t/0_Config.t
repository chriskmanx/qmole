
use strict;
use Test::More tests => 1;


# Build up a list of installed modules

my @mod_list = qw(XML::Simple Storable XML::Parser XML::SAX);


# If XML::SAX is installed, add a list of installed SAX parsers

eval " use XML::SAX; ";
my $default_parser = '';
unless($@) {
  push @mod_list, 'XML::NamespaceSupport';
  push @mod_list, map { $_->{Name} } @{XML::SAX->parsers()};
  $default_parser = ref(XML::SAX::ParserFactory->parser());
}


# Extract the version number from each module

my(%version);
foreach my $module (@mod_list) {
  eval " require $module; ";
  unless($@) {
    no strict 'refs';
    $version{$module} = ${$module . '::VERSION'} || "Unknown";
  }
}

$default_parser = 'XML::Parser' if(!$default_parser && $version{'XML::Parser'});


# Add version number of the Perl binary

eval ' use Config; $version{perl} = $Config{version} ';  # Should never fail
if($@) {
  $version{perl} = $];
}
unshift @mod_list, 'perl';


# Check for preferred parser via environment setting

my $preferred_parser = $ENV{XML_SIMPLE_PREFERRED_PARSER} || ' ';


# Print details of installed modules on STDERR

diag(sprintf("\r# %-30s %s\n", 'Package', 'Version'));
foreach my $module (@mod_list) {
  $version{$module} = 'Not Installed' unless(defined($version{$module}));
  $version{$module} .= " (default parser)" if($module eq $default_parser);
  $version{$module} .= " (preferred parser)" if($module eq $preferred_parser);
  diag(sprintf(" %-30s %s\n", $module, $version{$module}));
}

# Housekeeping

ok(1, "Dumped config");
