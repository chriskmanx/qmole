#!/usr/bin/perl
# calypso_import.pl
# Author: Thorsten Maerz <info@netztorte.de>
# License: GPL
# Dependencies: MIME::Parser, LWP::MediaTypes from www.cpan.org
# Converts mbox files as exported from Calypso to MH format. Regenerates
# Calypso's folder structure and optionally includes the attachments.

use strict ;

our $mboxdir	= ''		|| showhelp();	# enter path to exported mbox
our $mboxfile	= ''		|| showhelp();	# enter name of exported mbox
our $outdir	= ''		|| showhelp();	# enter destination path

my $incl_attach	= 1 ;		# include attachments (needs CPAN modules)
my $verbose	= 1 ;		# show some headers of processed mail
my $testonly	= 0 ;		# dont create any files

################################################################################
# no user servicable parts below :)

if ($incl_attach) {
	use MIME::Parser;
	use LWP::MediaTypes qw(guess_media_type);
}

my $mbox	= "$mboxdir/$mboxfile";
my $calypso_hdr = 'From \?\?\?@\?\?\? ';  #Mon Apr 17 00:37:38 2000
my $hdr_Folder	= 'X-CalypsoFolder:';
my $hdr_HTML	= 'X-CalypsoHtmlBody:';
my $hdr_Account	= 'X-CalypsoAccount:';
my $hdr_Attach	= 'X-Attachment:';
my %mail_nr;
my $create_dirs	= 1 ;		# create dirs from "X-Calypso-Folder:" header

################################################################################
sub showhelp {
	die ( "You have not yet configured this script.\n"
	    . "Please provide the correct path and file names, e.g\n"
	    . "\tour \$mboxdir  = 'Archive'\n"
	    . "\tour \$mboxfile = 'mail.txt'\n"
	    . "\tour \$outdir   = 'Calypso'\n"
	    . "at the top of $0\n"
	    );
}

################################################################################
#
# MAIN : scan $mbox linewise
# Create a separate message for each $calypso_hdr found (MH format)
# $attach_full = filename with path, $attach_short = original attachment name 
# $folder = Calypso folder
#
################################################################################
my ($folder, $html, $html_full, $html_short,
    $account, $attach, $attach_short, $attach_full);
my @lines ;

open (INBOX, "<".$mbox);
while (<INBOX>) {
	s/\x0d\x0a//;
	s/\x0d//;
	s/\x0a//;
	if (m/^$calypso_hdr/) {
		if (@lines) {
			$mail_nr{$folder}++ ;
			shift @lines ;		# remove blank line
			savemail();

			@lines = () ;
			$folder = $html = $html_full = $html_short = $account
			        = $attach = $attach_short = $attach_full = "";

		}
	}
	else {
		if (/^$hdr_Folder /)	{	$folder		= $' ;
						$folder 	=~ s/"//eg ;
						$folder 	=~ tr#\\#\/# ;
		} 
		if (/^$hdr_HTML /)	{	$html		= $' ;
						$html		=~ s/"//eg ;
						$html	 	=~ tr#\\#\/# ;
						if ($html	=~ /; /) {
							$html_full  = $` ;
							$html_short = $' ;
						}
		}
		if (/^$hdr_Account /)	{	$account	= $' ;
						$account	=~ s/"//eg ;
		}
		if (/^$hdr_Attach /)	{	$attach		= $' ;
						$attach 	=~ s/"//eg ;
						$attach 	=~ tr#\\#\/# ;
						if ($attach	=~ /; /) {
							$attach_full  = $` ;
							$attach_short = $' ;
						}
		}
		
		push (@lines, $_ );
	}
}
close (INBOX);

################################################################################
#
# sub:savemail
# Saves mail in @lines to $outdir/$folder/$mail_nr
# Folder is created unless $testonly or (not $create_dirs) is set
#
################################################################################
sub savemail {
	my $mailname = $mail_nr{$folder};
	my %headers;
	my $ishead=1;
	my $lineno=0;
	my $targetdir="";

	# extract headers
	foreach (@lines) {
		my ($hdr,$cnt);
		$lineno++;
	
		m/^$/ and ($ishead="");
		if ( $ishead ) {
			if (m/: /) {
				($hdr,$cnt) = ($`,$');
				$headers{$hdr}=$cnt;
			}
		}
	}

	if ($verbose) {
		print "MAIL : $mailname\n";
		print "FOLDER   : $folder\n"  				if ($folder);
		print "HTML     : $html_short ($html_full)\n"		if ($html);
		print "ACCOUNT  : $account\n" 				if ($account);
		print "ATTACH   : $attach_short ($attach_full)\n"	if ($attach);
		print "\n";
	}
	# write mail to folder
	if (! $testonly ) {
		if ($create_dirs) {
			$targetdir = $outdir.'/'.$folder ;
			my $curdir = '';
			foreach (split('/',$targetdir)) {
				$curdir .= $_ . '/';
				( -d $curdir) || mkdir $curdir;
			}
		}

		open (OUTFILE, ">".$targetdir.'/'.$mailname);
		foreach (@lines) { print OUTFILE "$_\n" ; }
		close (OUTFILE);

		if ($incl_attach) { 
			include_attachment($targetdir.'/'.$mailname);
		}
	}
}

################################################################################
# make inline attachment from external file
# uses MIME::Parser, LWP::MediaTypes from www.cpan.org
# (Currently leaves a blank attachment in converted mails. Feel free to
# improve this script)
sub include_attachment() {
	my $mailname = shift ;
	my $parser = new MIME::Parser ;
	
	my $entity ;
	my %attachments ;
	my %CID ;

	$parser->output_to_core(1);		# dont save to harddisk
	$entity = $parser->parse_open($mailname);

	# look for external attachments
	foreach ($entity->head->get('X-Attachment')) {
		if (m/["']?		# 1. start with " or ' (or none)
			([^"';]+)	# word till quote or separator
			["']?		# delete quote
			\s?;\s?		# separator ; (opt. spaces)
			["']?		# 2. start (s.a.)
			([^"';]+)	#
			["']?
		 	/x ) {	$attachments{$1} = $2 ;
		}
	}
	foreach ($entity->head->get('X-CalypsoHtmlBody')) {
		if (m/["']?		# 1. start with " or ' (or none)
			([^"';]+)	# word till quote or separator
			["']?		# delete quote
			\s?;\s?		# separator ; (opt. spaces)
			["']?		# 2. start (s.o.)
			([^"';]+)	#
			["']?
			/x ) {	$attachments{$1} = $2 ;
		}
	}
	foreach ($entity->head->get('X-CalypsoHtmlImg')) {
		if (m/["']?		# 1. start with " or ' (or none)
			([^"';]+)	# word till quote or separator
			["']?		# delete quote
			\s?;\s?		# separator ; (opt. spaces)
			["']?		# 2. start (s.a.)
			([^"';]+)	#
			["']?
			\s?;\s?		# separator ; (opt. spaces)
			["']?		# 3. start (s.a.)
			([^"';]+)	#
			["']?
			/x ) {	$attachments{$1} = $3 ;
				$CID{$1} = $2 ;
		}
	}
	
	if (%attachments) {
		# read attachment
		foreach my $key (keys (%attachments)) {
			our $attachdir;
			my $type ;
			my $enc ;
			my $fnam = $key;
			$fnam =~ tr#\\#/# 	if -d '/' ; # correct path names on unix like OS
			$fnam = $mboxdir .'/'. $fnam ;
			$type = guess_media_type($fnam);

			if ( $type =~ m/text/i )  { $enc = "8bit" }
			else			  { $enc = "base64" }

			$entity->attach(Path     => $fnam,
					Type     => $type,
					Encoding => $enc,
					Filename => $attachments{$key}
					);
		}
		
		my $lines = $entity->as_string ;
		# correct images names in html messages
		foreach (keys (%CID)) {
			$lines =~ s/CID:$CID{$_}/$attachments{$_}/eg;
		}
		
		print $mailname."\n";
		# qx(mv $mailname $mailname.bak);
		open ( MAIL, ">".$mailname );
		print( MAIL $lines );
		close( MAIL );
	}
}

