#!/usr/bin/perl -w

#  * Copyright 2002-2003 Ricardo Mones Lastra <mones@aic.uniovi.es>
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
# outlook2claws-mail.pl -- perl script to convert an Outlook generated 
# 			 contact list into a Claws Mail XML address book.
# 
# This script is based on:
# 	out2syl.sh by Rafael Lossurdo <mugas@via-rs.net>
# 	kmail2claws-mail.pl by Paul Mangan <paul@claws-mail.org>
#
# See README file for details and usage.
#

$nboffields = 28;       # change this only if you did read README

# parse parameters
$do_csv = 0;
die "Error: required filename missing\n" unless (defined($ARGV[0]));
$_=$ARGV[0];
if (/--csv/) {
	die "Error: required filename missing\n" unless (defined($ARGV[1]));
	$do_csv = 1;
	$outl_file = $ARGV[1];
}
else {
	$outl_file = $ARGV[0];
}
# some init
$clawsconf = ".claws-mail";
$indexname = "$clawsconf/addrbook--index.xml";

# the next is mostly Paul's code
$time = time;

chdir;
opendir(CLAWS, $clawsconf) || die("Error: can't open $clawsconf directory\n");
	push(@cached,(readdir(CLAWS)));
closedir(CLAWS);

foreach $cached (@cached) {
	if ($cached =~ m/^addrbook/ && $cached =~ m/[0-9].xml$/) {
		push(@addr, "$cached");
	}
}

@sorted = sort {$a cmp $b} @addr;
$last_one = pop(@sorted);
$last_one =~ s/^addrbook-//;
$last_one =~ s/.xml$//;
$last_one++;
$new_book = "/addrbook-"."$last_one".".xml";

# some subs
# warning: output file is global
sub write_header {
	print NEWB "<?xml version=\"1.0\" encoding=\"US-ASCII\" ?>\n";
	print NEWB "<address-book name=\"Outlook Address Book\" >\n"; 
}

sub write_footer {
	print NEWB "</address-book>\n";
}

sub write_person_h {
	my($fn, $ln, $nn, $cn) = @_;
	# one of them must be given
	if (($fn eq "") and ($ln eq "") and ($nn eq "") and ($cn eq "")) { 
		$cn = "No name provided";
		# but return may break XML structure	
	}
	print NEWB "  <person uid=\"", $time++, "\" first-name=\"", $fn, "\" ";
	print NEWB "last-name=\"", $ln, "\" nick-name=\"", $nn, "\" cn=\"", $cn, "\" >\n";
}

sub write_person_f {
	print NEWB "  </person>\n";
}

sub write_addrlist_h {
	print NEWB "    <address-list>\n";
}

sub write_addrlist_f {
	print NEWB "    </address-list>\n";
}

sub write_address {
	my($al, $em, $re) = @_;
	if ($em eq "") {
		$em = "No e-mail address"; 
		# email is a must -> no address breaks claws-mail display
		# (claws-mail says file is ok but no name is shown) 
		# maybe this is a bug on claws-mail?
	}
	print NEWB "      <address uid=\"", $time++, "\" ";
	print NEWB "alias=\"", $al, "\" email=\"", $em, "\" remarks=\"", $re, "\" />\n";
}

sub write_attrlist_h {
	print NEWB "    <attribute-list>\n";
}

sub write_attrlist_f {
	print NEWB "    </attribute-list>\n";
}

sub write_attribute {
	my($aname, $aval) = @_;
	if (($aname eq "") or ($aval eq "")) { return; } # both are must
	print NEWB "      <attribute uid=\"", $time++, "\" ";
	print NEWB "name=\"", $aname, "\" >", $aval, "</attribute>\n";
}

sub process_text {
	write_header();
	$count = 0;
	while (<OUTL>) {
		chomp;
		if (/\s+[0-9]+\s+(.+)/) { $_ = $1; } 
		else { $count += 2 and die "Error: wrong format at line $count \n"; }
		@field = split(/;/); # first is name, second mail addr
		write_person_h("","","",$field[0]);
		write_addrlist_h();
		$field[1] =~ s/\r//; # beware, dangerous chars inside ;)
		write_address("",$field[1],"");
		write_addrlist_f();
		write_person_f();
		++$count;
	}
	write_footer();
}

sub process_csv {
	write_header();
	$count = 0;
	while (<OUTL>) {
		chomp;
		# do something useful: quote XML chars
		s/\&/&amp;/g;
		s/\</&lt;/g;
		s/\>/&gt;/g;
		s/\'/&apos;/g;
		s/\"/&quot;/g;
		@field = split(/,/);
		if ($#field != $nboffields) { $count += 2 and die "Error: wrong format at line $count \n"; }
		# First Name, Last Name, Nickname, Name
		write_person_h($field[0],$field[1],$field[4],$field[3]);
		write_addrlist_h();
		write_address("",$field[5],$field[$nboffields - 1]);
		write_addrlist_f();
		write_attrlist_h(); # the remaining values as attributes 
		foreach $a (2, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27) {
			# add only filled fields (should be trimmed?)
			if (defined($field[$a]) && $field[$a] ne "") {
				write_attribute($headerline[$a],$field[$a]);
			}
		}
		write_attrlist_f();
		write_person_f();
		++$count;
	}
	write_footer();
}

# ok, was enough, do some more bit bashing now
open(OUTL, $outl_file) 
	or die "Error: can't open $outl_file for reading\n";
# 1st line: file format checking (csv) or discarding (default)
$_ = <OUTL>;
chomp;
if ($do_csv) {
	@headerline = split(/,/);
	# check before creating output file
	die "Error: unknown csv file format\n" 
		unless ($#headerline == $nboffields);
}
open(NEWB, '>', "$clawsconf/$new_book") 
	or die "Error: can't open $clawsconf/$new_book for writting\n";
if ($do_csv) { process_csv(); }
else { process_text(); }

close NEWB;
close OUTL;

# update index (more Paul's code :)

open(INDX, $indexname) 
	or die "Error: can't open $indexname for reading\n";
@index_file = <INDX>;
close INDX;

foreach $index_line (@index_file) {
	if ($index_line =~ m/<\/book_list>/) {
		$new_index .= "    <book name=\"Outlook Address Book\" file=\"$new_book\" />\n"."  </book_list>\n";							} else {
		$new_index .= "$index_line";
	}
}
open (INDX, '>', $indexname)
	or die "Error: can't open $indexname for writting\n";
print INDX "$new_index";
close INDX;

print "Done. $count address(es) converted successfully.\n";

