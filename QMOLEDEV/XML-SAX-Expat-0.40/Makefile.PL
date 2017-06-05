use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME          => 'XML::SAX::Expat',
    VERSION_FROM  => 'Expat.pm',
    AUTHOR        => 'Robin Berjon',
    ABSTRACT      => 'SAX Driver for Expat',
    PREREQ_PM     => {
                        XML::SAX::Base          => '1.00',
                        XML::Parser             => '2.27',
                        XML::NamespaceSupport   => '0.03',
                        XML::SAX                => '0.03',
                     },

    LICENSE       => 'perl',
    'dist'        => {
      PREOP     => 'chmod 600 Makefile.PL',
      TARFLAGS	=> '--group=cpan --owner=bjoern -cvf',
    },

);


## add ourselves to the list of installed parsers
sub MY::install {
    package MY;
    my $script = shift->SUPER::install(@_);
    unless ($ENV{'SKIP_SAX_INSTALL'}) {
      $script =~ s/install :: (.*)$/install :: $1 install_sax_expat/m;
      $script .= <<"INSTALL";

install_sax_expat :
\t\@\$(PERL) -I\$(INSTALLSITELIB) -MXML::SAX -e "XML::SAX->add_parser(q(XML::SAX::Expat))->save_parsers()"

INSTALL
    } else {
      warn "Note: 'make install' will skip XML::SAX::Expat registration with XML::SAX!\n";
    }
    return $script;
}
