#!/usr/bin/perl

#  * Copyright 2004-2006 Paul Mangan <paul@claws-mail.org>
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

unless ($ARGV[0]) { exit; }

my $claws = "claws-mail --compose --attach";
my $sel = split_parts();

exec "$claws $sel";

exit;

sub split_parts {
	my $selectedParts = "";
	my $count = 0;

	while ($ARGV[$count]) {
		$selectedParts .= "\"$ARGV[$count]\" ";
		$count++;
	}

	return ($selectedParts);
}
