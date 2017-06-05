#!/usr/bin/perl

#  * Copyright 2007 Jérôme Lelong <jerome.lelong@gmail.com>
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

## script name : mew2claws-mail.pl

## script purpose : convert a Mew addressbook into a Claws Mail addressbook

## This script assumes your Mew addressbook is Latin-1 encoded and not
## unicode. In this latter case, you will have to hack this script a
## little.

use Getopt::Long;

## Process the command line options
## the program expects one argument: the Mew addressbook file

my $help=0;
my $mewfile='';
GetOptions("mew-addressbook=s" => \$mewfile,
           "help" => \$help);

if ($help==1) 
{
    print("usage : perl mew2claws-mail.pl [--help] [--mew-addressbook=file] \n");
    print("\t--help: displays this help\n");
    print("\t--mew-addressbook=file : file is the filename of your Mew addressbook\n");
    exit 0;
}
if ($mewfile ne '' && !-f $mewfile)
{
    print("file $mewfile does not exists\n");
    exit 1;
}

$time=time;
$claws_addr='';
$home = glob("~");
$clawsdir=`claws-mail --config-dir`;
chomp($clawsdir);
$clawsdir = $home . '/' . $clawsdir;

opendir(CLAWS, $clawsdir) || die("Can't open $clawsdir directory\n");
push(@cached,(readdir(CLAWS)));
closedir(CLAWS);

## find the first availabel name for a new addressbook in claws-mail
foreach $cached (@cached)
{
    if ($cached =~ m/^addrbook/ && $cached =~ m/[0-9].xml$/)
    {
        push(@addr, "$cached");
    }
}
@sorted = sort {$a cmp $b} @addr;
$last_one = pop(@sorted);
$last_one =~ s/^addrbook-//;
$last_one =~ s/.xml$//;
$last_one++;
$new_addrbk = "addrbook-"."$last_one".".xml";


open (MEWFILE, "<$mewfile") || die("Can't find the Mew addressbook file\n");
@mewentries = <MEWFILE>;
close MEWFILE;

$claws_addr .= "<?xml version=\"1.0\" encoding=\"ISO8859-1\" ?>\n"
. "<address-book name=\"Mew Address Book\" >";


chomp(@mewentries);
foreach $line (@mewentries)
{
    $line =~ s/ *\t/ /g;
    $line =~ s/ *$//g;
    (@fields) = split(/ +/,$line);
    $nickname= shift(@fields);
    @emails=();
    $alias='';
    $firstname='';
    $lastname='';
    while (1) 
    {
        $field = shift(@fields);
        if ($field =~ m/@/)
        {
            $field =~ s/,$//;
            push(@emails, $field);
        } else
        {
            unshift(@fields, $field);
            last;
        }
    }
    $alias = shift(@fields);
    if ($alias eq "\*")
    {
        print($alias . "\n");
        $alias='';
    }
    

    $firstname=shift(@fields); $firstname =~ s/"//g;
    foreach (@fields)
    {
        $lastname .= "$_ "; 
    }
    $lastname =~ s/"//g;
    $lastname =~ s/ *$//g;
    
    $claws_addr .= "  <person uid=\"$time\" first-name=\"$firstname\""
    ." last-name=\"$lastname\" nick-name=\"$nickname\""
    ." cn=\"$firstname $lastname\" >\n"
    ."    <address-list>\n";
    $time++;

    foreach $email (@emails)
    {
        $claws_addr .= "      <address uid=\"$time\" alias=\"$alias\" email=\"$email\""
        ." remarks=\"\" />\n";
        $time++;
    }
    $claws_addr .= "    </address-list>\n"
    . "     <attribute-list>\n"
    . "    </attribute-list>\n";
    $claws_addr .=  "  </person>\n";
    $time++;
}
$claws_addr .= "</address-book>\n";

open (NEWADDR, ">$clawsdir/$new_addrbk") ;
print NEWADDR ($claws_addr);
close NEWADDR;

open (ADDRIN, "<$clawsdir/addrbook--index.xml") || die("can't open addrbook--index.xml");
@addrindex_file = <ADDRIN>;
close ADDRIN;

foreach $addrindex_line (@addrindex_file)
{
    if ($addrindex_line =~ m/<book name=\"Mew Address Book\"/)
    {
        print("An entry already exists for \"Mew Address Book\", you may duplicate it\n");
        print("Continuing anyway...\n");
    }
    if ($addrindex_line =~ m/<\/book_list>/)
    {
        $rewrite_addrin .= "    <book name=\"Mew Address Book\" file=\"$new_addrbk\" />\n"
        ."  </book_list>\n";
    } else
    {
        $rewrite_addrin .= "$addrindex_line";
    }
}

open (NEWADDRIN, ">$clawsdir/addrbook--index.xml");
print NEWADDRIN "$rewrite_addrin";
close NEWADDRIN;

print "\nYou have sucessfully converted your Mew addressbook\n";
exit;
