#!/bin/sh

# This shell script installs symbolic links in icons theme directories
# $1 is the source directory
# $2 is the install directory

# Reference theme names
ref_theme1=xfe-theme
ref_theme2=gnome-theme

# Link theme names
link_theme1=windows-theme
link_theme2=gnomeblue-theme
link_theme3=blue-theme
link_theme4=brown-theme
link_theme5=xfce-theme
link_theme6=tango-theme
link_theme7=kde-theme

echo Installing icons links...

# Loop on PNG files
for iconfile in $1/icons/$ref_theme1/*.png
do

	# Get icon name
	iconname=`basename "$iconfile"`

	# Install a link if the icon does not exist in the source theme directory

	if test ! -f $1/icons/$link_theme1/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname
	fi
	
	if test ! -f $1/icons/$link_theme2/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme2/$iconname $2/icons/$link_theme2/$iconname
	fi

	if test ! -f $1/icons/$link_theme3/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme2/$iconname $2/icons/$link_theme3/$iconname
	fi
	
	if test ! -f $1/icons/$link_theme4/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme2/$iconname $2/icons/$link_theme4/$iconname
	fi

	if test ! -f $1/icons/$link_theme5/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme2/$iconname $2/icons/$link_theme5/$iconname
	fi

	if test ! -f $1/icons/$link_theme6/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme2/$iconname $2/icons/$link_theme6/$iconname
	fi

	if test ! -f $1/icons/$link_theme7/$iconname
	then
		echo ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme1/$iconname;
		ln -s -f ../$ref_theme1/$iconname $2/icons/$link_theme7/$iconname
	fi

done
