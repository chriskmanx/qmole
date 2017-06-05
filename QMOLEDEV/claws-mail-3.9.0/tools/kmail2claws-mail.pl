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

## script name : kmail2claws-mail.pl

## script purpose : convert a Kmail addressbook into a Claws Mail addressbook

use Getopt::Long;

$kmailfile = '';

GetOptions("kmailfile=s" => \$kmailfile);

$time = time;

$claws_addr = "<?xml version=\"1.0\" encoding=\"US-ASCII\" ?>\n";
$claws_addr .= "<address-book name=\"Kmail Address Book\" >\n";

chdir;

opendir(CLAWS, ".claws-mail") || die("Can't open .claws-mail directory\n");
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

open (KFILE, "<$kmailfile") || die("Can't find the kmail file\n");
	@kmaillines = <KFILE>;
close KFILE;

$dross = shift(@kmaillines);

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
	}
    $claws_addr .= "  <person uid=\"$time\" first-name=\"$kmaildata[0]\""
		 ." last-name=\"$kmaildata[1]\" nick-name=\"$kmaildata[7]\""
		 ." cn=\"$kmaildata[0] $kmaildata[1]\" >\n"
		 ."    <address-list>\n";
    $time++;
    $claws_addr .= "      <address uid=\"$time\" alias=\"\" email=\"$kmaildata[6]\""
		." remarks=\"$kmaildata[8]\" />\n"
		."    </address-list>\n";
    if ($kmaildata[13] ne "" || $kmaildata[9] ne "" || $kmaildata[21] ne "" ||
	$kmaildata[16] ne "" || $kmaildata[5] ne "" || $kmaildata[24] ne "" ||
	$kmaildata[19] ne "" || $kmaildata[12] ne "" || $kmaildata[10] ne "" ||
	$kmaildata[4] ne "" || $kmaildata[2] ne "" || $kmaildata[11] ne "" ||
	$kmaildata[3] ne "" || $kmaildata[14] ne "" || $kmaildata[22] ne "" ||
	$kmaildata[17] ne "" || $kmaildata[20] ne "" || $kmaildata[15] ne "" ||
	$kmaildata[23] ne "" || $kmaildata[18] ne "") {
    	$claws_addr .= "    <attribute-list>\n";

	if ($kmaildata[3] ne "" || $kmaildata[2] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Full Name\" >"
			."$kmaildata[3] $kmaildata[0] $kmaildata[2] $kmaildata[1]</attribute>\n";
	}
	if ($kmaildata[15] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home Street Address\" >"
			."$kmaildata[15]</attribute>\n";
	}
	if ($kmaildata[16] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home City Address\" >"
			."$kmaildata[16]</attribute>\n";
	}
	if ($kmaildata[17] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home State Address\" >"
			."$kmaildata[17]</attribute>\n";
	}
	if ($kmaildata[18] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home Zip Address\" >"
			."$kmaildata[18]</attribute>\n";
	}
	if ($kmaildata[19] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home Country Address\" >"
			."$kmaildata[19]</attribute>\n";
	}
	if ($kmaildata[10] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home Phone\" >"
			."$kmaildata[10]</attribute>\n";
	}
	if ($kmaildata[12] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Home Fax\" >"
			."$kmaildata[12]</attribute>\n";
	}
	if ($kmaildata[11] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Mobile Phone\" >"
			."$kmaildata[11]</attribute>\n";
	}
	if ($kmaildata[14] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Pager\" >"
			."$kmaildata[14]</attribute>\n";
	}
	if ($kmaildata[5] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Company\" >"
			."$kmaildata[5]</attribute>\n";
	}
	if ($kmaildata[4] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Job Title\" >"
			."$kmaildata[4]</attribute>\n";
	}
	if ($kmaildata[20] ne "") {
   		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business Street Address\" >"
			."$kmaildata[20]</attribute>\n";
	}
	if ($kmaildata[21] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business City Address\" >"
			."$kmaildata[21]</attribute>\n";
	}
	if ($kmaildata[22] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business State Address\" >"
			."$kmaildata[22]</attribute>\n";
	}
	if ($kmaildata[23] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business Zip Address\" >"
			."$kmaildata[23]</attribute>\n";
	}
	if ($kmaildata[24] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business Country Address\" >"
			."$kmaildata[24]</attribute>\n";
	}
	if ($kmaildata[9] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business Phone\" >"
			."$kmaildata[9]</attribute>\n";
	}
	if ($kmaildata[13] ne "") {
    		$time++;
    		$claws_addr .= "      <attribute uid=\"$time\" name=\"Business Fax\" >"
			."$kmaildata[13]</attribute>\n";
	}
	$claws_addr .= "    </attribute-list>\n";
    }
    $claws_addr .=  "  </person>\n";
    $time++;
}
$claws_addr .= "</address-book>\n";

open (NEWADDR, ">.claws-mail/$new_addrbk");
print NEWADDR $claws_addr;
close NEWADDR;

open (ADDRIN, "<.claws-mail/addrbook--index.xml") || die("can't open addrbook--index.xml");
	@addrindex_file = <ADDRIN>;
close ADDRIN;

foreach $addrindex_line (@addrindex_file) {
	if ($addrindex_line =~ m/<\/book_list>/) {
		$rewrite_addrin .= "    <book name=\"Kmail Address Book\" file=\"$new_addrbk\" />\n"
			."  </book_list>\n";
	} else {
		$rewrite_addrin .= "$addrindex_line";
	}
}

open (NEWADDRIN, ">.claws-mail/addrbook--index.xml");
print NEWADDRIN "$rewrite_addrin";
close NEWADDRIN;

print "\nYou have sucessfully converted your Kmail addressbook\n";
exit;
