////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: this header defines paths for some required files
////////////////////////////////////////////////////////////////////////////

#ifndef CONFIG_H__
#define CONFIG_H__

#define APP_NAME_STR "Atol" 
#define APP_VER_STR "0.7.3"

#define PACKAGE "atol"	//NLS package definition

#ifndef _WIN32
#define INSTALL_PREFIX "/usr"
#endif

//plugin dir for Linux platform
#ifndef _WIN32
#define PLUGIN_DIR "/usr/local/lib/atol"
#endif

#ifdef _WIN32
	#define LOCALE_DIR "" 	//calculated dynamically	
#else
	#define LOCALE_DIR "/share/locale/" //relative to INSTALL_PREFIX	
#endif

// comment this to remove gnome-vfs code
#define HAVE_GNOME_VFS

#endif

