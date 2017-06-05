#
#  Top-level makefile for (Open)Watcom C++
#

all: .SYMBOLIC
	cd ..\..\src
	wmake -f makefile.wc
	cd ..\utils
	wmake -f makefile.wc
	cd ..\tests
	wmake -f makefile.wc
	cd ..\adie
	wmake -f makefile.wc
	cd ..\pathfinder
	wmake -f makefile.wc
	cd ..\calculator
	wmake -f makefile.wc

clean: .SYMBOLIC
	cd ..\..\src
	wmake -f makefile.wc clean
	cd ..\utils
	wmake -f makefile.wc clean
	cd ..\tests
	wmake -f makefile.wc clean
	cd ..\adie
	wmake -f makefile.wc clean
	cd ..\pathfinder
	wmake -f makefile.wc clean
	cd ..\calculator
	wmake -f makefile.wc clean

