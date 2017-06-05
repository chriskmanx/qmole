#!/usr/bin/perl

#  * Copyright 2005 Paul Mangan <paul@claws-mail.org>
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
#
# acroread2claws-mail.pl	helper script to send documents from
#				Adobe Reader 7 to claws-mail

use strict;

my $input = <>;

my $pdf;
my $output = $ARGV;

if ($ARGV =~ m/^\//) {
	$pdf = $output;
} elsif ($ARGV =~ m/^mailto/) {
	my @parts = split(/&[a-z]*=/, $output);
	$parts[0] =~ s/^mailto:\?attach=//;
	$pdf = $parts[0];
} elsif ($ARGV =~ m/^to/) {
	my @parts = split(/,/, $output);
	$parts[3] =~ s/^attachment=file:\/\///;
	$pdf = $parts[3];
} else {
	$pdf = $ENV{HOME}."/".$output;
}

exec "claws-mail --attach \"$pdf\"";
## if necessary, change the line above to point to
## your claws-mail executable

exit;
