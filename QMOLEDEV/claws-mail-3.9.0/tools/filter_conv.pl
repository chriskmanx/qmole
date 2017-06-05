#!/usr/bin/perl -w
use strict;

#  * Copyright 2002 Paul Mangan <paul@claws-mail.org>
#  *
#  * Reimplemented by Torsten Schoenfeld <kaffeetisch@web.de>
#  *
#  * This file is free software; you can redistribute it and/or modify it
#  * under the terms of the GNU General Public License as published by
#  * the Free Software Foundation; either version 3 of the License, or
#  * (at your option) any later version.
#  *
#  * This program is distributed in the hope that it will be useful, but
#  * WITHOUT ANY WARRANTY; without even the implied warranty of
#  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  * General Public License for more details.
#  *
#  * You should have received a copy of the GNU General Public License
#  * along with this program; if not, write to the Free Software
#  * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#  *

my $old_config_dir = "$ENV{HOME}/.sylpheed";
my $config_dir = `claws-mail --config-dir`;
chomp $config_dir;

chdir($ENV{ HOME } . "/$config_dir")
	or die("You don't appear to have Claws Mail installed\n");

###############################################################################

my $normal_headers = qr/^(?:Subject|From|To|Cc)$/;

my @new_filters = ("[global]\n");

###############################################################################

my $mailbox;

open(FOLDERLIST, "<$old_config_dir/folderlist.xml")
	or die("Can't find '$old_config_dir/folderlist.xml'\n");
  while (<FOLDERLIST>) {
    if (m/<folder type="mh" name="([^"]+)" path="[^"]+"/) {
      $mailbox = $1;
      last;
    }
  }
close FOLDERLIST;

###############################################################################

open(FILTERRC, "<$old_config_dir/filterrc")
	or die("Can't find your old filter rules ('$old_config_dir/filterrc')\n");
  while (<FILTERRC>) {
    chomp();

    my ($header_one,
        $value_one,
        $op,
        $header_two,
        $value_two,
        $destination,
        $mode_one,
        $mode_two,
        $action) = split(/\t/);

    $value_one =~ s/\"/\\\"/g ;
    $value_two =~ s/\"/\\\"/g ;
    $action = $action eq "m" ? "move" : "delete";
    $destination = $destination =~ m!^\#mh/! ?
                     $destination :
                     "#mh/$mailbox/$destination";

    my ($predicate_one,
        $predicate_two,
        $match_type_one,
        $match_type_two,
        $new_filter);

    ###########################################################################

    if ($mode_one % 2 == 0) {
      $predicate_one = "~";
    }
    else {
      $predicate_one = "";
    }

    if ($mode_one <= 1) {
      $match_type_one = "matchcase";
    }
    else {
      $match_type_one = "regexpcase";
    }

    ###########################################################################

    if ($mode_two % 2 == 0) {
      $predicate_two = "~";
    }
    else {
      $predicate_two = "";
    }

    if ($mode_two <= 1) {
      $match_type_two = "matchcase";
    }
    else {
      $match_type_two = "regexpcase";
    }

    ###########################################################################

    if ($header_one eq "To" && $header_two eq "Cc" ||
        $header_one eq "Cc" && $header_two eq "To" and
        $value_one eq $value_two and
        $mode_one eq $mode_two and
        $op eq "|") {
      if ($action eq "move") {
        $new_filter = $predicate_one . qq(to_or_cc $match_type_one "$value_one" move "$destination"\n);
      }
      else {
        $new_filter = $predicate_one . qq(to_or_cc $match_type_one "$value_one" delete\n);
      }
    }
    else {
      if ($header_one =~ m/$normal_headers/) {
        $new_filter .= $predicate_one . lc($header_one) . qq( $match_type_one "$value_one");
      }
      else {
        $new_filter .= $predicate_one . qq(header "$header_one" $match_type_one "$value_one");
      }

      if ($op ne " ") {
        if ($header_two =~ m/$normal_headers/) {
          $new_filter .= qq( $op ) . $predicate_two . lc($header_two) . qq( $match_type_two "$value_two");
        }
        else {
          $new_filter .= qq( $op ) . $predicate_two . qq(header "$header_two" $match_type_two "$value_two");
        }
      }

      if (defined($new_filter)) {
        if ($action eq "move") {
          $new_filter .= qq( move "$destination"\n);
        }
        else {
          $new_filter .= qq(delete\n);
        }
      }
    }

    ###########################################################################

    push(@new_filters, $new_filter) if (defined($new_filter));
  }
close(FILTERRC);

###############################################################################

open(MATCHERRC, ">>matcherrc");
  print MATCHERRC @new_filters;
close(MATCHERRC);

print "Converted $#new_filters filters\n";

if ($old_config_dir eq $config_dir) {
	rename("filterrc", "filterrc.old");
	print "Renamed your old filter rules ('filterrc' to 'filterrc.old')\n";
}
###############################################################################

