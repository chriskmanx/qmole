#!/usr/bin/perl -w

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
#  * Copyright 2003-2007 Paul Mangan <paul@claws-mail.org>
#  *
#  * 2007-02-25: several fixes for kmail 1.9.6
#	         --kmaildir now expects the full path
#		 renamed from maildir2claws-mail.pl to kmail-mailbox2claws-mail.pl
#  * 2003-10-01: add --debug and --dry-run options
#  * 2003-09-30: updated/improved by Matthias Förste <itsjustme@users.sourceforge.net>
#  * 2003-05-27: version one

## script name : kmail-mailbox2claws-mail.pl

## script purpose : convert a Kmail mailbox into a Claws Mail mailbox

## USAGE: kmail-mailbox2claws-mail.pl --kmaildir=/full/path/to/kmail/mailbox

## tested with Kmail version 1.9.6

use strict;

use Getopt::Long;
use File::Find;

my $kmaildir  = '';
my $iNeedHelp = '';
# dont actually change anything if set(useful in conjunction with debug)
my $PRETEND = '';
# print debug info if set
my $DEBUG = '';

my $claws_tmpdir = "$ENV{HOME}/claws_tmp";

GetOptions("kmaildir=s" => \$kmaildir,
	   "help"	=> \$iNeedHelp,
	   "dry-run"	=> \$PRETEND,
	   "debug"	=> \$DEBUG);

if ($kmaildir eq "" || $iNeedHelp) {
	if (!$iNeedHelp) {
		print "No directory name given\n";
	}
	print "Use the following format:\n";
	print "\tkmail-mailbox2claws-mail.pl --kmaildir=full-path-to-kmail-dir\n\n";
	exit;
}


my $count = 1;
my $MAIL_dir = "$kmaildir";

my $find_opts = { wanted => \&process };

if (-d $MAIL_dir) {
	find($find_opts , ($MAIL_dir));
} else {
	print "\n$MAIL_dir is not a directory !\n";
	exit;
}

unless ($PRETEND) {
	mkdir("$claws_tmpdir", 0755);
	system("mv $claws_tmpdir $ENV{HOME}/Mail");

	print "\n\nSucessfully converted mailbox \"$MAIL_dir\"\n";
	print "Start claws-mail and right-click \"Mailbox (MH)\" and ";
	print "select \"Rebuild folder tree\"\n";
	print "You may also need to run \"/File/Folder/Check for ";
	print "new messages in all folders\"\n\n";
}

print "\n";
exit;

sub process() {
  	if (-d) {
		process_dir($File::Find::dir);
	} else {
		process_file($File::Find::name);
	}
}

sub process_dir() {
	my $direc = shift();
  	$DEBUG && print "\nDIR $direc";

	if ($direc !~ m/^drafts$/ &&
	    $direc !~ m/^outbox$/ &&
      	    $direc !~ m/^trash$/  && 
    	    $direc !~ m/^inbox$/) {
		my $tmpdir = $direc;
		$tmpdir =~ s/^$MAIL_dir//;
		$tmpdir =~ s/\/sent-mail$/sent/;
		$tmpdir =~ s/\/cur$//;
		$tmpdir =~ s/\/new$//;
		$tmpdir =~ s/^\///;
		$tmpdir =~ s/\.directory//g;
		$tmpdir =~ s/\.//g;
		
		my $newdir = "$claws_tmpdir/$tmpdir";
		$DEBUG && print qq{\n>>> -e "$newdir" || mkdir("$newdir")};
		$PRETEND || -e "$newdir" || mkdir("$newdir");
	}

}

sub process_file {
	my $file = shift;
  	$DEBUG && print "\nFILE $file";

  	my $nfile;
  	my $tmpfile = $file;

    	$tmpfile =~ s|^$kmaildir||;

  	if ($tmpfile =~ m/\/cur\// || 
	    $tmpfile =~ m/\/new\//) {

    		$tmpfile =~ s/\/new//;
    		$tmpfile =~ s/\/cur//;

    		my @spl_str = split("/", $tmpfile);
    		pop(@spl_str);
    		push(@spl_str, "$count");

    		foreach my $spl_str (@spl_str) {
			$spl_str =~ s/\.directory$//;
			$spl_str =~ s/^\.//;
			$spl_str =~ s/^sent-mail$/sent/;
		}
    		$nfile = join("/", @spl_str);
		$nfile = $claws_tmpdir.$nfile;
	}

	if (-e "$file" && $nfile ne "") {
    		$DEBUG && print qq{\n+++ cp "$file" "$nfile"};
    		$PRETEND || system("cp \"$file\" \"$nfile\"");
    		$count++;
  	}

}
