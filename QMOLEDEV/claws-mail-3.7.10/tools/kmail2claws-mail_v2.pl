#!/usr/bin/perl

#  * Copyright 2002 Paul Mangan <paul@claws-mail.org>
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

## script name : kmail2claws-mail_v2.pl

## script purpose : convert an exported Kmail addressbook csv file 
## into a Claws Mail addressbook

## tested with Kmail 1.4.7 and KAddressBook 3.1beta1

use Getopt::Long;

$kmailfile = '';
$iNeedHelp = '';

GetOptions("kmailfile=s" => \$kmailfile,
	   "help"	 => \$iNeedHelp);

if ($kmailfile eq "" || $iNeedHelp) {
	if (!$iNeedHelp) {
		print "No filename given\n";
	}
	print "Use the following format:\n";
	print "\tkmail2claws-mail_v2.pl --kmailfile=/path/to/addressbook.csv\n";
	exit;
}

$claws_dir  = ".claws-mail";
$addr_index = "$claws_dir/addrbook--index.xml";
$new_addressbook = "Kmail address book";
 
$time = time;

chdir;

opendir(CLAWS, $claws_dir) || die("Can't open $claws_dir directory\n");
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
$new_addrbk = "addrbook-"."$last_one".".xml";

open (KFILE, "<$kmailfile") || die("Can't open the kmail file [$kmailfile]\n");
	@kmaillines = <KFILE>;
close KFILE;

$count = 0;
$defs = shift(@kmaillines);
@extra_def = (3,4,5,7 ... 27,29 ... 32,34 ... 42);

(@kmaildefs) = split(/,/,$defs);

$claws_addr = "<?xml version=\"1.0\" encoding=\"US-ASCII\" ?>\n";
$claws_addr .= "<address-book name=\"Kmail address book\" >\n";

foreach $kmailline (@kmaillines) {
    (@kmaildata) = split(/,/,$kmailline); 
	foreach $kmaildata (@kmaildata) {
		$kmaildata =~ s/^"//;
		$kmaildata =~ s/"$//;
		$kmaildata =~ s/"/&quot;/g;
		$kmaildata =~ s/&/&amp;/g;
		$kmaildata =~ s/'/&apos;/g;
		$kmaildata =~ s/</&lt;/g;
		$kmaildata =~ s/>/&gt;/g;
		$kmaildata =~ s/\\n/, /g;
		chomp $kmaildata;
	}
    $claws_addr .= "  <person uid=\"$time\" first-name=\"$kmaildata[2]\""
		 ." last-name=\"$kmaildata[1]\" nick-name=\"$kmaildata[6]\""
		 ." cn=\"$kmaildata[2] $kmaildata[1]\" >\n"
		 ."    <address-list>\n";
    $time++;
    $claws_addr .= "      <address uid=\"$time\" alias=\"\" email=\"$kmaildata[28]\""
		." remarks=\"$kmaildata[33]\" />\n"
		."    </address-list>\n";

	foreach $extra_def (@extra_def) {
		if ($kmaildata[$extra_def] ne "") {
			push (@def_exist, $extra_def);
		}
	}
	if ($def_exist[0]) {
		$claws_addr .= "    <attribute-list>\n";
	}
	foreach $def_exist (@def_exist) {
		$kmaildefs[$def_exist] =~ s/^"//;
		$kmaildefs[$def_exist] =~ s/"$//;
		$kmaildefs[$def_exist] =~ s/'/&apos;/g;
		
		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"$kmaildefs[$def_exist]\" >"
			."$kmaildata[$def_exist]</attribute>\n";
		$attribs = 1;
	}
	if ($attribs == 1) {
		$claws_addr .= "    </attribute-list>\n";
	}
    $claws_addr .=  "  </person>\n";
    $time++;
    $count++;
}
$claws_addr .= "</address-book>\n";

open (NEWADDR, ">$claws_dir/$new_addrbk");
print NEWADDR $claws_addr;
close NEWADDR;

open (ADDRIN, "<$addr_index") 
	|| die("can't open $addr_index for reading");
	@addrindex_file = <ADDRIN>;
close ADDRIN;

foreach $addrindex_line (@addrindex_file) {
	if ($addrindex_line =~ m/<\/book_list>/) {
		$rw_addrindex .= "    <book name=\"$new_addressbook\" file=\"$new_addrbk\" />\n"
			."  </book_list>\n";
	} else {
		$rw_addrindex .= "$addrindex_line";
	}
}

open (NEWADDRIN, ">$addr_index")
	|| die("Can't open $addr_index for writing");
print NEWADDRIN "$rw_addrindex";
close NEWADDRIN;

print "Done. $count address(es) converted successfully.\n";

exit;

