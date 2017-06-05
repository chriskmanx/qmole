#!/usr/bin/env bash

# textviewer.sh
# Copyright 2003 Luke Plant <L.Plant.98@cantab.net>
# and Johann Koenig <johann@mental-graffiti.com>

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

##############################################################################
#
# This script is a text viewer designed to be used with claws-mail actions
# Set up an action with the command line:  textviewer.sh %p |
#
# The script will try to detect file type automatically, and then
# invokes a relevant program to print the file in plain text to
# the standard output.
#
# From v 0.9.7claws7, claws-mail sets the temporary file
# of a part to XXXXXX.mimetmp.[filename of attachment]
# This means we can use the extension of the filename for checking.
# Also use the program 'file' if that fails.
#
# To extend the script just follow the patterns that already exist, or
# contact the author if you have problems.

##############################################################################
#
# Change Log
#
# 2003-03-25
#	- make extension matching case insensitive
#
# 2003-03-23
#	- Support for MS Excel (xlhtml) and Powerpoint (ppthtml)
#
# 2004-03-09
#	- Support for HTML (html2text)
#
# 2004-02-13
#	- added support for perl and shell scripts, and recognize that
#	  'file' will always return 'text' somewhere in its output for
#	  files that, well, contain text
#
# 2004-01-25
#	- added brief messages describing whats going on
#
# 2004-01-23
#	- added support for 'pdftotext,' from xpdf-utils debian package
#
# 2004-01-05
#	- added matcher and action for OpenOffice Writer documents
#	  (requires ooo2txt)
#
# 2004-01-05
#	- changed page width parameter for antiword
#	- fixed matcher for 'diffs'
#	- added a matcher and action for bzip2 - bzip2 files
#	  are decompressed and textviewer.sh run on the result
#	- similarly decompress gzip files and run textviewer.sh
#	  on the result, insteading of doing 'gzip -l'
#
# 2003-12-30
#	added the script to claws-mail/tools
#
# 2003-12-30
#	- use 'fold' after 'unrtf' to wrap to a nice width
#	- added basic file sanity checks
#
# 2003-12-29
#	Added recognition for "Zip " from 'file' output
#
# 2003-12-19
#	Initial public release
#
###############################################################################

if [ $# -eq 0 ]
then
  	echo "No filename supplied." >&2 
  	echo "Usage: textviewer.sh FILE" >&2 
  	exit 1
fi

[ -f "$1" ] ||
{
	echo "File \"$1\" does not exist or is not a regular file." >&2
	exit 1
}

[ -r "$1" ] ||
{	
	echo "Cannot read file \"$1\"." >&2
	exit 1
}

FILETYPE=`file --brief "$1"` || 
{
	echo "Please install the command 'file' to use this script." >&2
	exit 1 
};

FNAME=`echo "$1" | tr [A-Z] [a-z]`
case "$FNAME" in 
	*.doc)	TYPE=MSWORD	;;
	*.ppt)  TYPE=POWERPOINT ;;
	*.zip)	TYPE=ZIP	;;
	*.tar.gz|*.tgz)	TYPE=TARGZ ;;
	*.tar.bz2|*.tar.bz)	TYPE=TARBZ ;;
	*.gz)	TYPE=GZIP	;;
	*.bz2|*.bz)	TYPE=BZIP	;;
	*.tar)	TYPE=TAR	;;
	*.diff)	TYPE=TEXT	;;
	*.txt)	TYPE=TEXT	;;
	*.rtf)	TYPE=RTF	;;
	*.sxw)	TYPE=OOWRITER	;;
	*.pdf)	TYPE=PDF	;;
	*.sh)	TYPE=TEXT	;;
	*.pl)	TYPE=TEXT	;;
        *.html|*.htm) TYPE=HTML ;;
	*.xls)	TYPE=EXCEL	;;
esac

if [ "$TYPE" = "" ]
then
	case $FILETYPE in 
		*"HTML"*)	TYPE=HTML ;;
		*"text"*)	TYPE=TEXT ;;
		gzip*)		TYPE=GZIP ;;
		bzip2*)		TYPE=BZIP ;;
		"POSIX tar archive"*)	TYPE=TAR	;;
		"Zip "*) 	TYPE=ZIP  ;;
		"Rich Text Format"*)	
				TYPE=RTF  ;;
	esac
fi

case $TYPE in
	TARGZ) 	echo -e "Gzip'd tarball contents:\n"	; 
		tar -tzvf "$1"				;;

	TARBZ)	echo -e "Bzip'd tarball contents:\n" 	; 
		tar -tjvf "$1"				;;

	BZIP)	TMP=`mktemp "$1".temp.XXXXXXX` || exit 1;
		bunzip2 -c "$1" > "$TMP"  || exit 1;
		echo -e "Re-running \"$0\" on bunzip'd contents of \"$1\":\n";
		"$0" "$TMP";
		rm "$TMP"					;;

	GZIP)   TMP=`mktemp "$1".temp.XXXXXXX` || exit 1;
		gunzip -c "$1" > "$TMP"  || exit 1;
		echo "Re-running \"$0\" on gunzip'd contents of \"$1\":\n";
		"$0" "$TMP";
		rm "$TMP"					;;

	TAR)	echo -e "Tar archive contents:\n" 	; 
		tar -tvf "$1" 				;;

	ZIP)	echo -e "Zip file contents:\n"		;
		unzip -l "$1"				;;

	RTF)	which unrtf > /dev/null  2>&1 || 
		{
			echo "Program 'unrtf' for displaying RTF files not found" >&2
			exit 1
		};
		echo -e "Displaying \"$1\" using \"unrtf\":\n";
		unrtf -t text "$1" 2>/dev/null | egrep  -v '^### ' | fold -s -w 72  ;;

	TEXT)	cat "$1"				;;

	MSWORD) which antiword  > /dev/null  2>&1 || 
		{
			echo "Program 'antiword' for displaying MS Word files not found" >&2
			exit 1 
		};
		echo -e "Displaying \"$1\" using \"antiword\":\n";
		antiword -w 72 "$1" 				;;

	POWERPOINT) which ppthtml > /dev/null 2>&1 ||
		{ 
			echo "Program 'ppthtml' for displaying Powerpoint files not found" >&2
			exit 1
		};
		which html2text > /dev/null 2>&1 ||                           
                {                                                               
                        echo "Program 'html2text' for displaying Powerpoint files not found" >&2
                        exit 1                                                  
                };   
		echo -e "Displaying \"$1\" using \"ppthtml\" and \"html2text\":\n";
		ppthtml "$1" | html2text			;;

        EXCEL) which xlhtml > /dev/null 2>&1 ||
                {
                        echo "Program 'xlhtml' for displaying Excel files not found" >&2
                        exit 1
                };
                which html2text > /dev/null 2>&1 || 
                {
                        echo "Program 'html2text' for displaying Excel files not found" >&2
                        exit 1
                };
                echo -e "Displaying \"$1\" using \"xlhtml\" and \"html2text\":\n";
                xlhtml "$1" | html2text                        ;;
 
	OOWRITER) which ooo2txt > /dev/null 2>&1 ||
		{
			echo "Program 'ooo2txt' for converting OpenOffice Writer files not files not found" >&2
			exit 1
		};
		echo -e "Displaying \"$1\" using \"ooo2txt\":\n";
		ooo2txt "$1"					;;

	PDF) which pdftotext > /dev/null 2>&1 ||
		{
			echo "Program 'pdftotext' for converting Adobe Portable Document Format to text not found" >&2
			exit 1
		};
		echo -e "Displaying \"$1\" using \"pdftotext\":\n";
		pdftotext "$1"	-				;;

	HTML) which html2text > /dev/null 2>&1 ||
		{
			echo "Program 'html2text' for converting HTML files not found" >&2
			exit 1
		};
		html2text -nobs "$1" ;;

	*)	echo "Unsupported file type \"$FILETYPE\", cannot display.";;
esac
