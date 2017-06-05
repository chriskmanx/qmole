This is a place holder for Aiksaurus.lib and Aiksaurus.h both are a 
requirement to build the Thesaurus DLL plugin for Windows

To create Aiksaurus.lib you will need MC Visual C++ Version 6.0 or better
Check out http://www.aiksaurus.com and get source code for Aiksaurus.

Once you get the source code, there is a subdirectory of Aiksaurus - 
core/src.  Inside there is a MakefileLIB.msc.  To build a statically
linkable Aiksaurus.lib do 
	nmake -f MakefileLIB.msc

With the creation of Aiksaurus.lib, copy/move it and Aiksaurus.h to this 
directory and continue with the AbiWord build process for this plugin.



