#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use URI::Escape;

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
#  * Copyright 2007 Paul Mangan <paul@claws-mail.org>
#  *

#
# Convert Thunderbird filtering rules to Claws Mail filtering rules
#

#
# TABLE OF EQUIVALENTS
#
# thunderbird			:	Claws Mail
#------------------------------------------------------
#
# name="NAME"			:	rulename "NAME"
#
# enabled="yes"			:	enabled / disabled
#
# CONDITION LIST
# --------------
#
# OR				:	|
# AND				:	&
#
# subject			:	subject
# from				:	from
# to				:	to
# cc				:	cc
# to or cc			:	to_or_cc
# body				:	body-part
# date				:	****
# priority			:	****
# status			:	****
# age in days			:	age_greater/age_lower
# size				:	size_greater/size_smaller
# [custom header]		:	header
#
# 2nd level conditions
# --------------------
#
# contains			:	[nothing]
# doesn't contain		:	[append with ~]
# is				:	regexpcase
# isn't				:	regexpcase
# ends with			:	regexpcase
# begins with			:	regexpcase
# is in ab			:	found_in_addressbook
# isn't in ab			:	~found_in_addressbook
#
#
# status 2nd and 3rd level conditions
# -----------------------------------
#
# [is|isn't] replied
# [is|isn't] read
# [is|isn't] new
# [is|isn't] forwarded
# [is|isn't] flagged
#
#
# Date header 2nd  level condition
# --------------------------------
#
# is
# isn't
# is before
# is after
#
#
# Priority header 2nd and 3rd level conditions
# --------------------------------------------
# is 		 [Lowest|Low|Normal|High|Highest]
# is higher than [Lowest|Low|Normal|High|Highest]
# is lower than  [Lowest|Low|Normal|High|Highest]
# 
#
# ACTION LIST
# -----------
#
# Move to folder		:	move
# Copy to folder		:	copy
# Forward			:	****
# Reply				:	****
# Mark read			:	mark_as_read
# Mark flagged			:	mark
# Label				:	****
# Change priority		:	****
# JunkScore 100 [mark as spam]	:	****
# JunkScore 0 [mark as ham]	:	****
# Delete			:	delete
# Delete from Pop3 server	:	delete
# Fetch body from Pop3Server	:	****
#

my $script = "thunderbird-filters-convertor.pl";
my ($tbirdfile, $account, $mailbox, $iNeedHelp) = 0;

GetOptions("tbird-file=s"	=> \$tbirdfile,
	   "account-name=s"	=> \$account,
	   "mailbox-name=s"	=> \$mailbox,
	   "help|h"		=> \$iNeedHelp);

if ($iNeedHelp) {
	help_me();
}

if (!$tbirdfile) {
	print "ERROR: No filename given\n";
	print "Use $script -h for help\n";
	exit;
}

unless (-e $tbirdfile) {
	print "ERROR: $tbirdfile NOT FOUND!!\n";
	exit;
}

if (!$mailbox) {
	print "ERROR: No mailbox name given\n";
	print "Use $script -h for help\n";
	exit;
}

my $config_dir = `claws-mail --config-dir` or die("ERROR:
	You don't appear to have Claws Mail installed\n");
chomp $config_dir;

chdir($ENV{HOME} . "/$config_dir") or die("ERROR:
	Claws Mail config directory not found [~/$config_dir]
	You need to run Claws Mail once, quit it, and then re-run this script\n");

my $acrc = "accountrc";
my $acc_number;

if ($account) {
	$acc_number = find_account_number();
}
if ($account && !$acc_number) {
	print "ERROR: Account '$account' NOT FOUND!\n";
	exit;
}

my @claws_filters = ();

## check if matcherrc already exists
if (-e "matcherrc") {
	print "matcherrc exists!\n";
	read_current_filters();
} else {
	push(@claws_filters, "[preglobal]\n\n[postglobal]\n\n[filtering]\n")
}
##
my ($rule_count,@thunderbird_filters)  = read_thunderbird_filters();

my ($conv_rule,$ignored_rule,$ignore_list) = convert_filters($rule_count,@thunderbird_filters);

if (@claws_filters) {
	system("mv matcherrc matcherrc-safecopy");
	print "Moved ". $ENV{HOME}. "/$config_dir/matcherrc to "
	      . $ENV{HOME}. "/$config_dir/matcherrc-safecopy\n";
}
# write new config
open(MATCHERRC, ">>matcherrc");
	print MATCHERRC @claws_filters;
close(MATCHERRC);

print "We're done!\n";
print "-------------\n";
print "Converted $conv_rule rules";
if (defined($ignored_rule)) {
	print ", ignored $ignored_rule rules";
}
print "\n-------------\n";
print "$ignore_list";

exit;

sub help_me {
	print<<'EOH';
Usage:	
	thunderbird-filters-convertor.pl [options]
Options:
	--help -h			Show this screen.
	--tbird-file=PATH TO FILE	The full path to the file to be converted
	--mailbox-name=NAME		The name of the Claws Mail mailbox
	--account-name=NAME		The name of the account to be used (optional)	
EOH
exit;
}

sub find_account_number {
	my $cur_acc_numb;
	my $cur_acc_name;

	open (ACCOUNTRC, "<$acrc") || 
		die("Can't open the Accounts file [$acrc]\n");
		my @acrclines = <ACCOUNTRC>;
	close ACCOUNTRC;
	
	foreach my $line (@acrclines) {
		unless ($line =~ m/^\[Account/ || 
			$line =~ m/^account_name/) { next; }
		chomp($line);

		if ($line =~ s/^\[Account: //) {
			$line =~ s/]$//;
			$cur_acc_numb = $line;
		}
		if ($line =~ s/^account_name=//) {
			$cur_acc_name = $line;
		}
		if (defined($cur_acc_name) && $cur_acc_name eq $account) {
			return($cur_acc_numb);
		}
	}
}

sub read_current_filters {
	print "Reading current filters\n";

	open (CFILTERS, "<matcherrc") || 
		die("Can't open " . $ENV{HOME} . "/$config_dir/matcherrc");
		@claws_filters = <CFILTERS>;
	close CFILTERS;

	remove_last_empty_lines();
}

sub remove_last_empty_lines {
	my $line = pop(@claws_filters);
	if ($line =~ m/^$/) {
		remove_last_empty_lines();
	} else {
		push(@claws_filters, $line);
	}
}

sub read_thunderbird_filters {
	my @outer_array = ();
	my @inner_array = ();
	my $count = 0;

	open (TBIRDFILE, "<$tbirdfile") || 
		die("Can't open the tbird file [$tbirdfile]\n");
		my @tbirdlines = <TBIRDFILE>;
	close TBIRDFILE;

	foreach my $line (@tbirdlines) {
		if ($line =~ m/^version/ || $line =~ m/^logging/) { next; }

		chomp($line);

		push(@inner_array, "$line") unless $line eq "";
		if ($line =~ m/^condition/) {
			push(@outer_array, [@inner_array]);
			@inner_array = ();
			$count++;
		}
	}
	return($count-1,@outer_array);
}

sub convert_filters {
	my ($rule_count,@thunderbird_filters) = @_;

	my $tbird_action_no_value = qr/^(?:"Mark read"|"Mark flagged"|"Delete"|"Delete from Pop3 server"|"Fetch body from Pop3Server")$/;
	my $tbird_action_ignore = qr/^(?:"Label"|"Change priority"|"JunkScore"|"Fetch body from Pop3Server"|"Delete from Pop3 server"|"Reply")$/;
	my $exact_matches = qr/^(?:subject|from|to|cc)$/;
	my $ignore_matches = qr/^(?:date|priority|status)$/;

	my $conv_rules = my $ignored_rules = 0;
	my $ignored_list = "";
	for (my $outerloop = 0; $outerloop <= $rule_count; $outerloop++) {
		my $part_one = my $part_two = my $part_three = my $part_four = "";
		my $ignore_rule = my $move_rule = my $copy_rule = my $cond_count = 0;
		my %ignore_hash;
		my $bool = my $claws_condition = my $cur_name = "";
		for (my $innerloop = 0; exists($thunderbird_filters[$outerloop][$innerloop]); $innerloop++) {
			my $entry = $thunderbird_filters[$outerloop][$innerloop];
			if ($entry =~ s/^name=//) {
				$cur_name = $entry;
				$part_one = "rulename $entry ";
			} elsif ($entry =~ s/^enabled=//) {
				if ($entry eq "\"yes\"") {
					$part_one = "enabled $part_one";
				} else {
					$part_one = "disabled $part_one";
				}
				if (defined($acc_number)) {
					$part_one .= "account $acc_number ";
				}
			} elsif ($entry =~ s/^type=//) {
				# do nothing : what does 'type' mean??
			} elsif ($entry =~ s/^action=//) {
				if ($entry =~ m/$tbird_action_ignore/ && !$ignore_rule) {
					$ignore_rule = 1;
					unless ($ignore_hash{$cur_name}) {
						$ignored_list .= "Ignored $cur_name because it contains $entry\n";
						$ignored_rules++;
					}
					$ignore_hash{$cur_name}++;
					$part_one = "";
					next;
				} elsif ($entry =~ m/Move to folder/) {
					$part_four = "move ";
					$move_rule = 1;
				} elsif ($entry =~ m/Copy to folder/) {
					$part_three .= "copy";
					$copy_rule = 1;
				} elsif ($entry =~ m/Mark read/) {
					$part_three .= "mark_as_read ";
				} elsif ($entry =~ m/Mark flagged/) {
					$part_three .= "mark";
				} elsif ($entry =~ m/Delete/) {
					$part_three .= "delete";
				}
			} elsif ($entry =~ s/^actionValue=//) {
				if ($ignore_rule) {
					$ignore_rule = 0;
					next;
				} elsif ($move_rule) {
					$entry = rewrite_mailbox_name($entry);
					$part_four .= uri_unescape($entry);
					$move_rule = 0;					
				} elsif ($copy_rule) {
					$entry = rewrite_mailbox_name($entry);
					$part_three .= " " . uri_unescape($entry);
					$copy_rule = 0;
				}
			} elsif ($entry =~ s/^condition=//) {
				if ($entry =~ s/^\"AND//) {
					$bool= "&";
				} elsif ($entry =~ s/^\"OR//) {
					$bool = "|";
				}
				my @tbird_conditions = split(/ \(/, $entry);
				foreach my $cond (@tbird_conditions) {
					my $exact = my $endswith = my $beginswith = my $addrbook = 0;
					my $age_condition = my $size_condition = my $exact_age = 0;
					$cond =~ s/\) OR$//;
					$cond =~ s/\) AND$//;
					$cond =~ s/\)"$//;
					$cond =~ s/\\"/"/g;
					my ($cpart_one, $cpart_two, $cpart_thr) = split(/,/, $cond, 3);
					if ($cond) {
						if ($cpart_one =~ m/$exact_matches/) {
							$claws_condition .= "$cpart_one";
						} elsif ($cpart_one eq "to or cc") {
							$claws_condition .= "to_or_cc";
						} elsif ($cpart_one eq "body") {
							$claws_condition .= "body-part";
						} elsif ($cpart_one eq "age in days") {
							$age_condition = 1;
						} elsif ($cpart_one eq "size") {
							$size_condition = 1;
						} elsif ($cpart_one =~ m/$ignore_matches/) {
							$part_one = $claws_condition = $part_three = $part_four = "";
							next;
						} else {
							$claws_condition = "header $cpart_one";
						}

						if ($cpart_two eq "doesn't contain") {
							$claws_condition = "~$claws_condition matchcase";
						} elsif ($cpart_two eq "contains") {
							$claws_condition = "$claws_condition matchcase";
						} elsif ($cpart_two eq "isn't") {
							$exact = 1;
							$claws_condition = "~$claws_condition regexpcase";
						} elsif ($cpart_two eq "is") {
							if ($size_condition) {
								$claws_condition .= "size_equal";
							} elsif ($age_condition) {
								if ($bool ne "&") {
									$part_one = $claws_condition = $part_three = $part_four = "";
									if (!$ignored_list) {
										$ignored_list .= "Ignored $cur_name because it matches an exact age and is an OR match\n";
									}
									next;
								} else {
									$ignored_rules--;
									$exact_age = 1;
								}
							} else {
								$exact = 1;
								$claws_condition = "$claws_condition regexpcase";
							}
						} elsif ($cpart_two eq "ends with") {
							$endswith = 1;
							$claws_condition = "$claws_condition regexpcase";
						} elsif ($cpart_two eq "begins with") {
							$beginswith = 1;
							$claws_condition = "$claws_condition regexpcase";
						} elsif ($cpart_two eq "is in ab") {
							$addrbook = 1;
							$claws_condition = "found_in_addressbook \"$claws_condition\" in \"Any\" ";
						} elsif ($cpart_two eq "isn't in ab") {
							$addrbook = 1;
							$claws_condition = "~found_in_addressbook \"$claws_condition\" in \"Any\" ";
						} elsif ($cpart_two eq "is greater than") {
							if ($size_condition) {
								$claws_condition .= "size_greater";
							}
							if ($age_condition) {
								$claws_condition .= "age_greater";
							}
						} elsif ($cpart_two eq "is less than") {
							if ($size_condition) {
								$claws_condition .= "size_smaller";
							}
							if ($age_condition) {
								$claws_condition .= "age_lower";
							}
						}							
				
						if ($exact || $beginswith || $endswith) {
							$cpart_thr = escape_regex($cpart_thr);
						}
						if ($exact) {
							$cpart_thr = "^$cpart_thr\$";
						} elsif ($beginswith) {
							$cpart_thr = "^$cpart_thr";
						} elsif ($endswith) {
							$cpart_thr = "$cpart_thr\$";
						}
						unless ($addrbook) {
							if ($exact_age) {
								my $lower_limit = $cpart_thr-1;
								my $upper_limit = $cpart_thr+1;
								$lower_limit =~ s/^\"//;
								$lower_limit =~ s/\"$//;
								$upper_limit =~ s/^\"//;
								$upper_limit =~ s/\"$//;
								$claws_condition = "$claws_condition"."age_lower"
											 . " $upper_limit $bool "
											 . "age_greater $lower_limit ";
							} elsif ($size_condition || $age_condition) {
								$claws_condition = "$claws_condition $cpart_thr ";
							} else {	
								$claws_condition = "$claws_condition \"$cpart_thr\" ";
							}
						}

						if ($tbird_conditions[1] && $cond_count < $#tbird_conditions) {
							$claws_condition = "$claws_condition$bool ";
						}
					}
					$cond_count++;
				}
				if ($part_one) {
					$conv_rules++;
					push(@claws_filters, "$part_one$claws_condition$part_three$part_four\n");
				}
			}
		}
	}
	push(@claws_filters, "\n");
	return($conv_rules,$ignored_rules,$ignored_list);
}

sub rewrite_mailbox_name {
	my ($path) = @_;

	my $new_path;

	my ($front,$back) = split(/\/\//, $path, 2);

	if ($front =~ m/^"mailbox/) {
		$new_path = "\"#mh/$mailbox/";
	} else {
		$new_path = "\"#imap/$mailbox/";
	}

	my ($box,$name) = split(/\//, $back, 2);

	if ($new_path =~ m/^"#mh/) {
		$name =~ s/^Inbox/inbox/;
		$name =~ s/^Sent/sent/;
		$name =~ s/^Drafts/draft/;
		$name =~ s/^Trash/trash/;
	}
	$new_path = $new_path.$name;

	return($new_path);
}

sub escape_regex {
	my ($string) = @_;

	my $escstr = "";
	my $symbols = qr/^(?:\[|\]|\{|\}|\(|\)|\||\+|\*|\.|\-|\$|\^)$/;
	my @chars = split(//, $string);

	foreach my $char (@chars) {
		if ($char =~ m/$symbols/) { $char = "\\\\$char"; }
		$escstr .= $char;
	}

	return($escstr);
}
