#!/usr/bin/perl
# convert_mbox.pl
# perl script to convert mbox file to files in a new MH directory
# aka another mbox -> MH conversion tool
# 29 April 2003  
# Fred Marton <Fred.Marton@uni-bayreuth.de>
# 
# Fixed (hopefully) to account for From lines
# that are of various length and that might have
# time zone info at the end
# 20 January 2004
#
# Note: Running this with the -w flag generates the following warnings:
# Scalar value @word[1] better written as $word[1] at /path/to/convert_mbox.pl line 54
# Scalar value @word[0] better written as $word[1] at /path/to/convert_mbox.pl line 56
# Making these changes requires further changes in the script
# that results in much longer run-times.  
#
# Copyright (c) 2003 Fred Marton
# Copyright (c) 2009 Ricardo Mones, based on the bash script
#                    by Daniel Dickinson [1]
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# [1] http://bugs.debian.org/cgi-bin/bugreport.cgi?msg=15;filename=convert_mbox.sh;att=1;bug=461435

use File::Path;
use File::Basename;

# check for arguments
&usage if ($#ARGV < 1);
if ($ARGV[0] eq "-R") {
  # recursive, need more args
  &usage if ($#ARGV < 2);
  &usage unless (-d $ARGV[1]);
  &usage unless (-d $ARGV[2]);
  &convert_all ($ARGV[1], $ARGV[2]);
}
else {
  # default behaviour
  &convert_mbox ($ARGV[0], $$ARGV[1]);
}

sub strip_sbd {
  # raw translation of bash, probably doable in real Perl better
  $dirname = shift;
  $nosbddir = "";
  $dirleft = $dirname;
  while ($dirleft ne "." and $dirleft ne "") {
    $enddir = &basename ($dirleft, ['.sbd']);
    if ($nosbddir ne "") {
      $nosbddir = "$enddir/$nosbddir";
    }
    else {
      $nosbddir = $enddir;
    }
    $dirleft = &dirname ($dirleft);
  }
  print "$nosbddir\n";
}

sub convert_all {
  ($mboxdir, $targetdir) = @_;
  $tmpbase = '/tmp/frommbox';
  $tmplist = '/tmp/.convert_mbox.filelist';
  $curdir = qx/pwd/;
  chdir ($mboxdir);
  qx/find * -type -d -a ! -name '*.*'i | sort > $tmplist/;
  open (FLH, "<$tmplist") or die "cannot open $tmplist: $!\n";
  while (<FLH>) {
    chomp;
    &convert_one ($_, $targetdir, $tmpbase);
  }
  close (FLH);
  unlink ($tmplist);
  chdir ($curdir);
}

sub convert_one {
  # targetdir isn't used in the original bash script, maybe a bug?
  ($file, $targetdir, $tmpbase) = @_;
  $dirname = &dirname ($file);
  $filename = &basename ($file);
  if ($dirname eq $filename) {
    $dirname = "";
  }
  $dirname = &strip_sbd ($dirname);
  if ($dirname ne "") {
    $dirname = "/$dirname";
  }
  &mkpath ($tmpbase . $dirname);
  $basefile = &basename ($file);
  $base1 = &basename ($file, ['.cmeta']);
  $base2 = &basename ($base1, ['.ibex.index']);
  $base3 = &basename ($base2, ['.ibex.index.data']);
  $base5 = &basename ($base3, ['.ev-summary']);
  $base4 = &basename ($base5, ['.ev-summary-meta']);
  $subdir = &basename ($dirname);
  $basedir = "/" . &dirname ($dirname);
  if ( $basedir eq '/.' or $basedir eq '/' ) {
    $basedir = $dirname;
    $subdir = "";
  }
  $basedir = $tmpbase . $basedir;
  &mkpath ($basedir);
  $dirisfile = 0;
  if ( -f "$basedir/$subdir" ) {
    $dirisfile = 1;
    qx/mv \"$basedir\/$subdir\" \"$basedir\/$subdir.tmp\"/;
    &mkpath ("$basedir/$subdir");
  }
  if ( ! -d "$basedir/$subdir/$base4" ) {
    print "$basedir/$subdir/$base4\n";
    &convert_mbox ($file, "$basedir/$subdir/$base4", 'quiet');
    if ($dirisfile == 1) {
      qx/mv \"$basedir\/$subdir.tmp\/*\" \"$basedir\/$subdir\/\"/;
    }
  }
}


sub convert_mbox {
  ($mbox, $mh, $quiet) = @_;
  # check to make sure there isn't something named MH already
  if (-e $mh) {
     die (" The directory \"$mh\" already exists.  Exiting.\n");
  }
  else {
     mkdir $mh;
  }
  # start numbering
  $i = 0;
  # open the mbox file
  open (IN, $mbox);
  while ($line = <IN>) {
    # check for the beginning of an e-mail
    @word = split(/ +/m,$line);
    # some lines might start with "From ", so check
    # to see if the [second-to-]last word is a year
    @word2 = split(/:/,$line);
    chomp($word2[$#word2]);
    @word3 = split(/ /,$word2[2]);
    $year = @word3[1];
    # ignore the MAILER-DAEMON message from pine
    if (@word[1] ne "MAILER-DAEMON") {
      # start a new file, assuming $year is > 1970
      if (@word[0] eq "From" && $year > 1970) {
        $i++;
        close (OUT);
        open (OUT, ">$mh/$i");
        print OUT $line;
      }
      else {
        # continue the file
        print OUT $line;
      }
    }
  }
  close (OUT);
  close (IN);
  # and we're done
  if (! defined($quiet)) {
    print "\n If it isn't there already, please move the directory \"$mh\"\n"
          . " into your MH directory and rebuild your folder tree.\n\n";
  }
}

sub usage {
   die ("Usage: convert_mbox.pl MBOX MH_DIR\n"
        . "       convert_mbox.pl -R MBOXDIR MH_DIR\n"
        . "Where: \n"
        . "  -R  Converts recursively a directory of mboxes\n");
}


