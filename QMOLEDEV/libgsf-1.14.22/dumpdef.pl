#!/usr/bin/perl -w
#
# dumpdef.pl: Extract function declarations from C headers.
#
# Copyright (C) 2005 Ivan, Wong Yat Cheung <email@ivanwong.info>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of version 2.1 of the GNU Lesser General Public
# License as published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA
#

sub parse_header {
    my ($lines, $file, @symbols);

    $file = shift;
    open HEADER, $file or die "Cannot open $file: $!\n";
    $/ = undef;
    $lines = <HEADER>;
    close HEADER;
    while ($lines =~ /^\s*[A-Za-z_]\w*(?:\s+[A-Za-z_]\w*)*[\s\*]+ #function type
		      ([A-Za-z_]\w*)\s*\( #function name
		      (?:\s*[A-Za-z_]\w*(?:[\s\*]+[A-Za-z_]\w*)*[\s\*]+[A-Za-z_]\w*(?:\s*\[\s*\]\s*)? #[first arg
		       (?:\s*,\s*[A-Za-z_]\w*(?:[\s\*]+[A-Za-z_]\w*)*[\s\*]+[A-Za-z_]\w*(?:\s*\[\s*\]\s*)?)* #[more args]]
		       (?:\s*,\s*\.{3})?|void) #vargs or void
		      \s*\)\s* #close
		      (?:(?:G_GNUC_PRINTF|G_GNUC_SCANF)\s*\(\s*\d+\s*,\s*\d+\) # predefined macro modifiers]
		        |G_GNUC_FORMAT\s*\(\s*\d+\s*\)
		        |G_GNUC_NORETURN|G_GNUC_CONST|G_GNUC_UNUSED|G_GNUC_NO_INSTRUMENT
				|G_GNUC_DEPRECATED)?
		      \s*;/gxm) {
	push @symbols, $1;
    }
    while ($lines =~ /^\s*extern\s+ #function type
		      [A-Za-z_]\w*(?:[\s\*]+[A-Za-z_]\w*)*[\s\*]+([A-Za-z_]\w*) #[first arg
		      \s*(?:\[\s*\]\s*)?;/gxm) {
	push @symbols, "$1 DATA";
    }
    @symbols;
}

die "Usage: dumpdef.pl files...\n" if ($#ARGV == -1);

my @symbols;
do {
    push @symbols, parse_header $ARGV[0];
    shift @ARGV;
} while ($#ARGV != -1);

print join "\n", sort @symbols;
print "\n";

