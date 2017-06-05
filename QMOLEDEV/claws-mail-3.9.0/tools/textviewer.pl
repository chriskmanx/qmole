#!/usr/bin/perl

# COPYRIGHT AND LICENSE
#        Copyright (C) 2005-2006 H.Merijn Brand
#
#        This script is free software; you can redistribute it and/or modify it
#        under the same terms as Perl and/or Claws Mail itself. (GPL)

use strict;
use warnings;

sub usage ($;$)
{
    my ($err, $str) = (@_, "");
    $err and select STDERR;
    print
	"usage: $0 [--html] [--type=<type>] file\n",
	"       --html    Generate HTML (if supported)\n",
	"       --type=X  X as mimetype (msword => doc)\n";
    $str and print "$str\n";
    exit $err;
    } # usage

@ARGV == 1 and $ARGV[0] eq "-?" || $ARGV[0] =~ m/^-+help$/ and usage (0);

use Getopt::Long qw(:config bundling nopermute);
my $opt_v = 0;
my $opt_t;
my $opt_h = "text";
GetOptions (
    "v|verbose:1"	=> \$opt_v,
    "t|type|mimetype=s"	=> \$opt_t,
    "h|html"		=> sub { $opt_h = "html" },
    ) or usage (1);

$opt_v and print "$0 @ARGV\n";

my $file = shift or usage (1, "File argument is missing");
-f $file         or usage (1, "File argument is not a plain file");
-r $file         or usage (1, "File argument is not a readable file");
-s $file         or usage (1, "File argument is an empty file");

# anon-list contains all possible commands to show content
# plain text is a reference to same type (alias)
# %f will be replaced with file. If no %f, file will be the last arg
my %fh = (
    text => {
	bin	=> [ "strings"		], # fallback for binary files

	txt	=> [ "cat"		], # Plain text

	html	=> [ "txt2htm",
		     "text2html"	], # HTML

	msword	=> "doc",
	doc	=> [ "antiword -w 72"	], # M$ Word
	"vnd.ms-excel" => "xls",
	"ms-excel"     => "xls",
	xls	=> [ "xlscat -L"	], # M$ Excel
#	ppt	=> [ "ppthtml"		], # M$ PowerPoint
#			ppthtml "$1" | html2text

	rtf	=> [ "rtf2text",
		     "unrtf -t text"	], # RTF
	pdf	=> [ "pdftotext %f -"	], # Adobe PDF

	sxc	=> "xls",		   # OpenOffice spreadsheet
	odt	=> [ "ooo2txt"		], # OpenOffice writer

	csv	=> "xls",		   # Comma Separated Values

	pl	=> [ "perltidy -st -se",
		     "cat"		], # Perl
	pm	=> "pl",

	( map { $_ => "txt" } qw(
	    diff
	    c h ic ec cc
	    sh sed awk
	    plain
	    )),

	bz2	=> [ "bzip2 -d < %f | strings" ],

	test	=> [ \&test		], # Internal
	},

    html => {
	rtf	=> [ "rtf2html"		],
	},
    );

my $ext = $file =~ m/\.(\w+)$/ ? lc $1 : "";
$opt_t && exists $fh{text}{lc $opt_t} and $ext = lc$opt_t;
unless (exists $fh{text}{$ext}) {
    my $ftype = `file --brief $file`;
    $ext =
	$ftype =~ m/^pdf doc/i					? "pdf" :
	$ftype =~ m/^ascii( english)? text/i			? "txt" :
	$ftype =~ m/^(utf-8 unicode|iso-\d+)( english)? text/i	? "txt" :
	$ftype =~ m/^xml doc/i					? "xml" :
	$ftype =~ m/^\w+ compress/i				? "bin" :
								  "bin" ;
    # \w+ archive
    # \w+ image
    # ...
    }
$ext ||= "txt";
exists $fh{$opt_h}{$ext} or $opt_h = "text";
exists $fh{$opt_h}{$ext} or $ext   = "txt";
my          $ref = $fh{$opt_h}{$ext};
ref $ref or $ref = $fh{$opt_h}{$ref};

$opt_v and print STDERR "[ @$ref ] $file\n";

sub which ($)
{
    (my $cmd = shift) =~ s/\s.*//; # Only the command. Discard arguments here
    foreach my $path (split m/:+/, $ENV{PATH}) {
	-x "$path/$cmd" and return "$path/$cmd";
	}
    return 0;
    } # which

my $cmd = "cat -ve";
foreach my $c (@$ref) {
    if (ref $c) {
	$c->($file);
	exit;
	}

    my $cp = which ($c) or next;
    $cmd = $c;
    last;
    }

$cmd =~ s/%f\b/$file/g or $cmd .= " $file";
$opt_v and print "$cmd\n";
exec $cmd;
