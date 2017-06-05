# Load the Module::Install bundled in ./inc/
use inc::Module::Install;

# Define metadata
name 'XML-NamespaceSupport';
all_from 'lib/XML/NamespaceSupport.pm';

build_requires 'Test::More' => '0.47';

auto_set_repository;
auto_manifest;
WriteAll;
