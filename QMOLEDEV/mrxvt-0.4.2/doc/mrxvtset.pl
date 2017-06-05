#!/usr/bin/perl
#
# All portions of code are copyright by their respective author/s.
# Copyright (c) 2004     Avi Yagodnick  <aviy@bezeqint.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#
# $Id: mrxvtset.pl,v 1.2 2004/12/24 05:24:32 cvs Exp $
#
#===================================================================================
#
#         FILE:  mrxvtset.pl
#
#        USAGE:  ./mrxvtset.pl 
#
#  DESCRIPTION: set (m)rxvt functionality by escape sequences.
#               slowly adding some features for mrxvt, too.
#        FILES:  ---
#        NOTES:  some mrxvt basic features added
#       AUTHOR:  Avi Yagodnick  <aviy@bezeqint.net>
#      COMPANY:  -----
#      VERSION:  2.0
#      CREATED:  12/08/04 10:15:31 IST
#     REVISION:  ---
#===================================================================================

# for debug only
#use strict;

use Cwd;
use Getopt::Long;
#Make program name more compact
$0 =~ s-.*/--;
# set it to whatever place you like
$pixmap_file = "/tmp/rxvtpixmap.xpm";
print_usage("No arguments given") if ($#ARGV < 0);
GetOptions ("-fn=s",            # font number 
            "-fg=s",            # foreground color
            "-fgn=s",           # foreground color number
            "-bg=s",            # background color
            "-bgn=s",           # background color number
            "-sb:s",            # scrollbar visibility control
            "-tb:s",            # tabbar visibility control (MRXVT)
            "-tbb:s",           # tabbar buttons visibility toggle (MRXVT)
            "-mb:s",            # menubar visibility toggle
            "-pixmap=s",        # background pixmap control
            "-title=s",         # terminal title
            "-tab_title=s",     # change current tab title (MRXVT)
            "-new_tab_title=s", # new tab with title   (MRXVT)
            "-tr:s",            # transparency toggle (MRXVT)
             "-usage","-help");
              
if (defined $opt_usage or defined $opt_help){
    print_usage("Help Message");
}

if (defined $opt_pixmap){
    if ($opt_pixmap eq 'none'){
        print "\033[m\033]20;\007";
    }elsif($opt_pixmap eq 'full'){
        print "\033[m\033]20;;100x100+50+50;?\007";
    }elsif($opt_pixmap eq 'tiled'){
        print "\033[m\033]20;;0\007";
    }else{
        if ($opt_pixmap =~ /\.png|\.gif|\.jpg/){
            system "convert $opt_pixmap $pixmap_file";
        }else{
            $pixmap_file = $opt_pixmap;
        }
        print "\033]20;$pixmap_file\007";
    }
}

if (defined $opt_sb){
    if ($opt_sb eq 'no'){
        #low
        print "\033[?30l\007";
    }elsif ($opt_sb eq 'yes'){
        #high
        print "\033[?30h\007";
    }elsif($opt_sb eq ""){
        #toggle
        print "\033[?30t\007";
    }
}
if (defined $opt_tb){
        print "\033]69;\007";
}
if (defined $opt_tbb){
        print "\033]71;\007";
}
if (defined $opt_mb){
    if ($opt_mb eq 'no'){
        #low
        print "\033[?10l\007";
    }elsif ($opt_mb eq 'yes'){
        #high
        print "\033[?10h\007";
    }elsif($opt_mb eq ""){
        #toggle
        print "\033[?10t\007";
    }
}
if (defined $opt_bg){
    open (PIXFILE,">$pixmap_file") || die "Can't open bg file\n";
    print PIXFILE  "/* XPM */
static char *rxvtpixmap[] = {
/* columns rows colors chars-per-pixel */
\"1 1 1 1\",
\"  c $opt_bg\",
/* pixels */
\" \"
};\n";
    close PIXFILE;
    chmod 0666, $pixmap_file;

    print "\033]20;$pixmap_file\007";
}

if (defined $opt_fg){
    print "\033]4;0;$opt_fg\007\033]39;0\007";
}

if (defined $opt_fgn){
    print "\007\033]39;$opt_fgn\007";
}
if (defined $opt_bgn){
    print "\007\033]49;$opt_bgn\007";
}
if (defined $opt_fn){
    if ($opt_fn eq 'normal'){
        print "\033]50;#\007";
    }elsif($opt_fn eq '+'){
        print "\033]50;#+\007";
    }elsif($opt_fn eq '-'){
        print "\033]50;#-\007";
    }else{
        print "\033]50;#$opt_fn\007";
    }
}
if (defined $opt_title){
    print "\033]0;$opt_title\007";
}
if (defined $opt_tab_title){
    print "\033]61;$opt_tab_title\007";
}
if (defined $opt_new_tab_title){
    print "\033]63;$opt_new_tab_title\007";
}
if (defined $opt_tr){
    print "\033]76;\007";
}
sub print_usage{ #{{{1
   my($message) = @_; 
print
"--------------------------------------------------------------------------
$0 Info:
   $message
USAGE: $0  [options to set]
Change rxvt parameters on the fly.
Aterm supports only part of options (doesn't support fg)
-fg <fg color name/spec> 
-fgn <fg color number 0-15> 
-bg <bg color name/spec> 
-bgn <bg color number 0-15> 
-pixmap <xpm filename/full/tiled> if filename is not xpm, uses 'convert' to /tmp/rxvtpixmap.xpm
-fn <font number 0-6 or +/- or 'normal' for default>
-title <new title>
-tab_title <change tab title> MRXVT only
-new_tab_title <create new tab with title> MRXVT only
-sb <yes/no/empty for toggle> scrollbar control
-tb <empty for toggle> tabbar control , MRXVT only
-tbb <empty for toggle> tabbar buttons control , MRXVT only
-tr <empty for toggle> transparency control , MRXVT only
-mb <yes/no/empty for toggle> menubar control
\n";
} #1}}}

