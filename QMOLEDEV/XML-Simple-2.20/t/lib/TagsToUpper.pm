package TagsToUpper;

use XML::SAX::Base;

use vars qw(@ISA);

@ISA = ('XML::SAX::Base');

sub start_element {
  my $self    = shift;
  my $element = shift;

#  print Data::Dumper->Dump([$element], ['element']);
  to_upper($element);
  foreach (values(%{$element->{Attributes}})) { to_upper($_); }

  $self->SUPER::start_element($element);
}

sub end_element {
  my $self    = shift;
  my $element = shift;

  to_upper($element);

  $self->SUPER::end_element($element);
}

sub to_upper {
  my $ref = shift;

  $ref->{LocalName} = uc($ref->{LocalName}) if($ref->{LocalName});
  $ref->{Name}      = uc($ref->{Name})      if($ref->{LocalName});
  $ref->{Prefix}    = uc($ref->{Prefix})    if($ref->{LocalName});
}

1;

