#!/usr/bin/perl

#  * Copyright 2003-2007 Paul Mangan <paul@claws-mail.org>
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

# Changes:
#	Feb 2007: add support for non ISO-8859-1 compatible locales
#		  by Alex Gorbachenko <agent_007@immo.ru>
#

use Getopt::Long;
use URI::Escape;
use POSIX qw(locale_h);
use Text::Iconv;

my $where = '';
my $what  = '';

GetOptions("where=s"	=> \$where,
	   "what=s"	=> \$what);

$locale = setlocale(LC_CTYPE);
$locale =~ s/\S+\.//;

$converter = Text::Iconv->new("$locale", "UTF-8");
$safe=uri_escape($converter->convert("$what"));
$what=$safe;

chdir($ENV{HOME} . "/.claws-mail") 
	|| die("Can't find your ~/.claws-mail directory\n");

open (CONF, "<multiwebsearch.conf") 
	|| die("Can't open ~/.claws-mail/multiwebsearch.conf\n");
	@conflines = <CONF>;
close CONF;

foreach $confline (@conflines) {
	if ($confline =~ m/^$where\|/) {
		chomp $confline;
		@parts = split(/\|/, $confline);
		$url = $parts[1];
		if ($parts[2]) {
			$what .= $parts[2];
		}
	}
}

if (!$url) {
	die("No url found with the alias \"$where\"\n");
} 

open (SYLRC, "<clawsrc") 
	|| die("Can't open ~/.claws-mail/clawsrc\n");
	@rclines = <SYLRC>;
close SYLRC;

foreach $rcline (@rclines) {
	if ($rcline =~ m/^uri_open_command/) {
		chomp $rcline;
		@browser = split(/=/, $rcline);
		$browser[1] =~ s/%s/$url$what/;
	}
}
system("$browser[1]&");
exit;
