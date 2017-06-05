// Helper functions for various purposes
// Some helper functions are also added to get large files support
// Also supports a timeout on the stat and lstat function (this is the reason
// why some standard FOX function cannot be used and are rewritten here)


#include "config.h"
#include "i18n.h"


#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <libgen.h>

#include <fx.h>
#include <fxkeys.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"



// Note : the original function from FXAccelTable is buggy!!
// Parse accelerator from string
FXHotKey _parseAccel(const FXString& string)
{
	register unsigned int code=0,mods=0;
	register int pos=0;

	// Parse leading space
	while(pos<string.length() && Ascii::isSpace(string[pos]))
		pos++;

	// Parse modifiers
	while(pos<string.length())
	{
		// Modifier
		if(comparecase(&string[pos],"ctl",3)==0)
		{
			mods|=CONTROLMASK;
			pos+=3;
		}
		else if(comparecase(&string[pos],"ctrl",4)==0)
		{
			mods|=CONTROLMASK;
			pos+=4;
		}
		else if(comparecase(&string[pos],"alt",3)==0)
		{
			mods|=ALTMASK;
			pos+=3;
		}
		else if(comparecase(&string[pos],"meta",4)==0)
		{
			mods|=METAMASK;
			pos+=4;
		}
		else if(comparecase(&string[pos],"shift",5)==0)
		{
			mods|=SHIFTMASK;
			pos+=5;
		}
		else
			break;

		// Separator
		if(string[pos]=='+' || string[pos]=='-' || Ascii::isSpace(string[pos]))
			pos++;
	}

	// Test for some special keys
	if(comparecase(&string[pos],"home",4)==0)
		code=KEY_Home;
	else if(comparecase(&string[pos],"end",3)==0)
		code=KEY_End;
	else if(comparecase(&string[pos],"pgup",4)==0)
		code=KEY_Page_Up;
	else if(comparecase(&string[pos],"pgdn",4)==0)
		code=KEY_Page_Down;
	else if(comparecase(&string[pos],"left",4)==0)
		code=KEY_Left;
	else if(comparecase(&string[pos],"right",5)==0)
		code=KEY_Right;
	else if(comparecase(&string[pos],"up",2)==0)
		code=KEY_Up;
	else if(comparecase(&string[pos],"down",4)==0)
		code=KEY_Down;
	else if(comparecase(&string[pos],"ins",3)==0)
		code=KEY_Insert;
	else if(comparecase(&string[pos],"del",3)==0)
		code=KEY_Delete;
	else if(comparecase(&string[pos],"esc",3)==0)
		code=KEY_Escape;
	else if(comparecase(&string[pos],"tab",3)==0)
		code=KEY_Tab;
	else if(comparecase(&string[pos],"return",6)==0)
		code=KEY_Return;
	else if(comparecase(&string[pos],"enter",5)==0)
		code=KEY_Return;
	else if(comparecase(&string[pos],"back",4)==0)
		code=KEY_BackSpace;
	else if(comparecase(&string[pos],"spc",3)==0)
		code=KEY_space;
	else if(comparecase(&string[pos],"space",5)==0)
		code=KEY_space;

	// Test for function keys
	else if(Ascii::toLower(string[pos])=='f' && Ascii::isDigit(string[pos+1]))
	{
		if(Ascii::isDigit(string[pos+2]))
		{			
			// !!!! Hack to fix a bug in FOX !!!!
			code=KEY_F1+10*(string[pos+1]-'0')+(string[pos+2]-'0')-1;
			// !!!! End of hack !!!!
		}
		else
			code=KEY_F1+string[pos+1]-'1';
	}
	// Test if hexadecimal code designator
	else if(string[pos]=='#')
		code=strtoul(&string[pos+1],NULL,16);

	// Test if its a single character accelerator
	else if(Ascii::isPrint(string[pos]))
	{
		if(mods&SHIFTMASK)
			code=Ascii::toUpper(string[pos])+KEY_space-' ';
		else
			code=Ascii::toLower(string[pos])+KEY_space-' ';
	}
	return MKUINT(code,mods);
}


FXbool existCommand(const FXString cmd)
{
	struct stat linfo;

	// Command file path
	FXString cmdpath = cmd.before(' ');

	// If first character is '/' then cmdpath is an absolute path
	if (cmdpath[0]==PATHSEPCHAR)
	{
		// Check if command file name exists and is not a directory
		if (!cmdpath.empty() && (lstatrep(cmdpath.text(),&linfo)==0) && !S_ISDIR(linfo.st_mode))
			return TRUE;	
	}

	// If first character is '~' then cmdpath is a path relative to home directory
	else if (cmdpath[0] == '~')
	{
		// Form command absolute path
		cmdpath=FXSystem::getHomeDirectory() + cmdpath.after('~');
		
		// Check if command file name exists and is not a directory
		if (!cmdpath.empty() && (lstatrep(cmdpath.text(),&linfo)==0) && !S_ISDIR(linfo.st_mode))
			return TRUE;
		
	}
	
	// Simple command name or path relative to the exec path
	else
	{
		// Get exec path
		FXString execpath=FXSystem::getExecPath();
		
		if(execpath != "")
		{
			FXString path;

			for(int i=0;;i++)
			{
				// Obtain path component
				path=execpath.section(':',i);
				if(path=="")
					break;
				
				// Form command absolute path
				path += PATHSEPSTRING + cmdpath;	

				// Check if command file name exists and is not a directory
				if (!path.empty() && (lstatrep(path.text(),&linfo)==0) && !S_ISDIR(linfo.st_mode))
					return TRUE;
			}
		}
    }

	return FALSE;
}


// Get key binding string from user input
// Code adapted from FXAccelTable::unparseAccel() and modified to get strings like 'Ctrl-A' instead of 'ctrl+a'
FXString getKeybinding(FXEvent* event)
{
	// Get modifiers and key
	int mods=event->state;
	int code=event->code;
	
	char buffer[64];
	FXString s;

	// Handle modifier keys
	if(mods&CONTROLMASK)
		s+="Ctrl-";
	if(mods&ALTMASK)
		s+="Alt-";
	if(mods&SHIFTMASK)
		s+="Shift-";
	if(mods&METAMASK)
		s+="Meta-";

	// Handle some special keys
	switch(code)
	{
		case KEY_Home:
			s+="Home";
			break;
		case KEY_End:
			s+="End";
			break;
		case KEY_Page_Up:
			s+="PgUp";
			break;
		case KEY_Page_Down:
			s+="PgDn";
			break;
		case KEY_Left:
			s+="Left";
			break;
		case KEY_Right:
			s+="Right";
			break;
		case KEY_Up:
			s+="Up";
			break;
		case KEY_Down:
			s+="Down";
			break;
		case KEY_Insert:
			s+="Ins";
			break;
		case KEY_Delete:
			s+="Del";
			break;
		case KEY_Escape:
			s+="Esc";
			break;
		case KEY_Tab:
			s+="Tab";
			break;
		case KEY_Return:
			s+="Return";
			break;
		case KEY_BackSpace:
			s+="Back";
			break;
		case KEY_space:
			s+="Space";
			break;
		case KEY_F1:
		case KEY_F2:
		case KEY_F3:
		case KEY_F4:
		case KEY_F5:
		case KEY_F6:
		case KEY_F7:
		case KEY_F8:
		case KEY_F9:
		case KEY_F10:
		case KEY_F11:
		case KEY_F12:
		case KEY_F13:
		case KEY_F14:
		case KEY_F15:
		case KEY_F16:
		case KEY_F17:
		case KEY_F18:
		case KEY_F19:
		case KEY_F20:
		case KEY_F21:
		case KEY_F22:
		case KEY_F23:
		case KEY_F24:
		case KEY_F25:
		case KEY_F26:
		case KEY_F27:
		case KEY_F28:
		case KEY_F29:
		case KEY_F30:
		case KEY_F31:
		case KEY_F32:
		case KEY_F33:
		case KEY_F34:
		case KEY_F35:
			sprintf(buffer,"F%d",code-KEY_F1+1);			
			s+=buffer;
			break;
		default:
		if(Ascii::isPrint(code))
			s+=Ascii::toUpper(code);
		else
			s="";  // Invalid case
		break;
	}
	return s;
}


// Create a directory with its path, like 'mkdir -p'
// Return 0 if success or -1 if fail
// Original author : Niall O'Higgins */
// http://niallohiggins.com/2009/01/08/mkpath-mkdir-p-alike-in-c-for-unix
int mkpath(const char* s, mode_t mode)
{
	char *q, *r = NULL, *path = NULL, *up = NULL;
	int rv;

	rv = -1;
	if (strcmp(s, ".") == 0 || strcmp(s, "/") == 0)
		return (0);

	if ((path = strdup(s)) == NULL)
		exit(1);

	if ((q = strdup(s)) == NULL)
		exit(1);

	if ((r = (char*)dirname(q)) == NULL)
		goto out;

	if ((up = strdup(r)) == NULL)
		exit(1);

	if ((mkpath(up, mode) == -1) && (errno != EEXIST))
		goto out;

	if ((mkdir(path, mode) == -1) && (errno != EEXIST))
		rv = -1;
	else
		rv = 0;

out:
	if (up != NULL)
		free(up);
	free(q);
	free(path);
	return (rv);
}


// Obtain a unique trash files path name based on the file path name
FXString createTrashpathname(FXString pathname, FXString trashfileslocation)
{
	// Initial trash files path name
	FXString trashpathname=trashfileslocation+PATHSEPSTRING+FXPath::name(pathname);

	// Eventually modify the trash files path name by adding a suffix like '_1', '_2', etc.,
	// if the file already exists in the trash can files directory
	for (int i=1; ; i++)
	{
		if (::exists(trashpathname))
		{
			char suffix[32];
			snprintf(suffix,sizeof(suffix)-1,"_%d",i);
			FXString prefix=trashpathname.rbefore('_');
			if (prefix=="")
				prefix=trashpathname;
			trashpathname=prefix+suffix;
		}
		else
			break;
	}
	
	return trashpathname;
}							


// Create trashinfo file based on the pathname and the trashpathname
int createTrashinfo(FXString pathname, FXString trashpathname, FXString trashfileslocation, FXString trashinfolocation)
{
	// Create trash can files if it doesn't exist
	if (!exists(trashfileslocation))
	{
		int mask=umask(0);
		umask(mask);
		int ret=mkpath(trashfileslocation.text(),511 & ~mask);
		return ret;
	}
	
	// Create trash can info if it doesn't exist
	if (!exists(trashinfolocation))
	{
		int mask=umask(0);
		umask(mask);
		int ret=mkpath(trashinfolocation.text(),511 & ~mask);
		return ret;

	}
	
	// Deletion date
	struct timeval tv;
	gettimeofday(&tv,NULL);
	FXString deldate=FXSystem::time("%FT%T",tv.tv_sec);
	
	// Trash info path name
	FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+FXPath::name(trashpathname)+".trashinfo";

	// Create trash info file
	FILE *fp;
	int ret;
	if ((fp=fopen(trashinfopathname.text(),"w"))!=NULL)
	{
		fprintf(fp,"[Trash Info]\n");
		fprintf(fp,"Path=%s\n",pathname.text());
		fprintf(fp,"DeletionDate=%s\n",deldate.text());
		fclose(fp);
		ret=0;
	}
	else
		ret=-1;

	return ret;
}


// Return mime type of a file
// Makes use of the Unix file command, thus this function may be slow
FXString mimetype(FXString filepath)
{
    FXString cmd ="/usr/bin/file -b -i " + filepath;
    FILE *filecmd=popen(cmd.text(),"r");
    if(!filecmd)
    {
        perror("popen");
        exit(1);
    }
    char text[128]={0};
    FXString buf;
    while(fgets(text,sizeof(text),filecmd))
        buf+=text;
	pclose(filecmd);

	return buf.rbefore('\n');
} 


// Quote a filename against shell substitutions
// Thanks to Glynn Clements <glynnc@users.sourceforge.net>
FXString quote(FXString str)
{
	FXString result = "'";
	const char *p;

	for (p = str.text(); *p; p++)
		if (*p == '\'')
			result += "'\\''";
		else
			result += *p;

	result += '\'';
	return result;
}


// Test if a string is encoded in UTF-8
// "length" is the number of bytes of the string to consider
// Taken from the weechat project. Original author FlashCode <flashcode@flashtux.org>
FXbool isUtf8(const char* string, unsigned int length)
{
    unsigned int n=0;
	while (n<length)
    {
        // UTF-8, 2 bytes, should be: 110vvvvv 10vvvvvv
        if (((unsigned char)(string[0]) & 0xE0) == 0xC0)
        {
            if (!string[1] || (((unsigned char)(string[1]) & 0xC0) != 0x80))
                return FALSE;
            string += 2;
			n += 2;
        }
        // UTF-8, 3 bytes, should be: 1110vvvv 10vvvvvv 10vvvvvv
        else if (((unsigned char)(string[0]) & 0xF0) == 0xE0)
        {
            if (!string[1] || !string[2]
                || (((unsigned char)(string[1]) & 0xC0) != 0x80)
                || (((unsigned char)(string[2]) & 0xC0) != 0x80))
                return FALSE;
            string += 3;
			n += 3;
       }
        // UTF-8, 4 bytes, should be: 11110vvv 10vvvvvv 10vvvvvv 10vvvvvv
        else if (((unsigned char)(string[0]) & 0xF8) == 0xF0)
        {
            if (!string[1] || !string[2] || !string[3]
                || (((unsigned char)(string[1]) & 0xC0) != 0x80)
                || (((unsigned char)(string[2]) & 0xC0) != 0x80)
                || (((unsigned char)(string[3]) & 0xC0) != 0x80))
                return FALSE;
            string += 4;
			n += 4;
        }
        // UTF-8, 1 byte, should be: 0vvvvvvv
        else if ((unsigned char)(string[0]) >= 0x80)
		{
            return FALSE;
		}
        // Next byte
		else
		{
            string++;
			n++;
		}
    }
	return TRUE;
}


#if defined(linux)
// Stat function used to test if a mount point is up or down
// Actually, this is simply the lstat() function
int lstatmt(const char* filename, struct stat* buf)
{
	return lstat(filename,buf);
}
#endif


// Safe strcpy function (Public domain, by C.B. Falconer)
// The destination string is always null terminated
// Size sz must be equal to strlen(src)+1
size_t strlcpy(char* dst, const char* src, size_t sz)
{
	const char *start = src;

	if (src && sz--)
	{
		while ((*dst++ = *src))
			if (sz--)
				src++;
			else
		 	{
            	*(--dst) = '\0';
            	break;
         	}
   	}
   	if (src)
   	{
      	while (*src++)
			continue;
      	return src - start - 1;
   	}
   	else if (sz)
		*dst = '\0';
	return 0;
}


// Safe strcat function (Public domain, by C.B. Falconer)
// The destination string is always null terminated
size_t strlcat(char* dst, const char* src, size_t sz)
{
   char *start = dst;

   while (*dst++)      // assumes sz >= strlen(dst)
      if (sz)
	  	sz--;          // i.e. well formed string
   dst--;
   return dst - start + strlcpy(dst, src, sz);
}


// Obtain the non recursive size of a directory
FXulong dirsize(const char* path)
{
    DIR *dp;
	struct dirent *dirp;
	struct stat statbuf;
	char buf[MAXPATHLEN];
	FXulong dsize=0;
	int ret;
			
	if((dp=opendir(path))==NULL)
        return 0;
	
	while((dirp=readdir(dp)))
    {
        if ( streq(dirp->d_name, ".")||streq(dirp->d_name, "..") )
            continue;
		
		if (streq(path,ROOTDIR))
			snprintf(buf,sizeof(buf)-1,"%s%s",path,dirp->d_name);
		else
			snprintf(buf,sizeof(buf)-1,"%s/%s",path,dirp->d_name);
		
#if defined(linux)
		// Mount points are not processed to improve performances 
		if(mtdevices->find(buf))
			continue;
#endif
		
		ret=lstatrep(buf,&statbuf);
		if(ret==0)
		{
			if(!S_ISDIR(statbuf.st_mode))
		  		dsize += (FXulong) statbuf.st_size;
		}
	}
	if(closedir(dp)<0)
        fprintf(stderr,_("Can't close folder %s\n"), path);
	return dsize;
}


// Obtain the recursive size of a directory
// The number of files and the number of sub directories is also stored in the nbfiles and nbsubdirs pointers
// Caution: this only works if nbfiles and nbsubdirs are initialized to 0 in the calling function
// After that, nbfiles contains the total number of files (including the count of sub directories)
// and nbsubdirs the number of sub directories
FXulong pathsize(char* path, unsigned int* nbfiles, unsigned int* nbsubdirs)
{
	struct stat statbuf;
	struct dirent *dirp;
    char *ptr;
    DIR *dp;
	FXulong dsize;
	int ret;

	ret=lstatrep(path,&statbuf);
	if(ret<0)
        return 0;
    dsize = (FXulong) statbuf.st_size;
	(*nbfiles)++;
    
	// Not a directory
	if(!S_ISDIR(statbuf.st_mode))
        return dsize;
	
	// Directory
	(*nbsubdirs)++;

	ptr=(char*)path + strlen(path);
    if(ptr[-1]!='/')
    {
        *ptr++='/';
        *ptr='\0';
    }
    
	if((dp=opendir(path))==NULL)
        return 0;
    
	while((dirp=readdir(dp)))
    {
        if( streq(dirp->d_name, ".") || streq(dirp->d_name, "..") )
            continue;
        strlcpy(ptr, dirp->d_name, strlen(dirp->d_name)+1);
		
		// Recursive call
        dsize+=pathsize(path,nbfiles,nbsubdirs);
    }
	
    ptr[-1]='\0'; // ??
	
    if(closedir(dp)<0)
        fprintf(stderr,_("Can't close folder %s\n"), path);
    return dsize;
}


// Write the file size in human readable form (bytes or Kbytes or Mbytes)
FXString hSize(char* size)
{
    int flag=0;
    char suf[64];
	char buf[128];
	FXString hsize;

    FXulong lsize = strtoull(size,NULL,10);
    float fsize=0.0;

	strlcpy(suf,_("bytes"),sizeof(suf)); 
	if(lsize>1000000000)
    {
        fsize=lsize/1073741824.0;
        strlcpy(suf,_("GB"),sizeof(suf));
        flag=1;
    }
	else if(lsize>1000000)
    {
        fsize=lsize/1048576.0;
        strlcpy(suf,_("MB"),sizeof(suf));
        flag=1;
    }
    else if(lsize>1000)
    {
        fsize=lsize/1024.0;
        strlcpy(suf,_("KB"),sizeof(suf));
        flag=1;
    }
  	
	if(flag)
    {
		if(fsize==(int)fsize)
            snprintf(buf,sizeof(buf),"%.0f %s",fsize,suf);
        else
            snprintf(buf,sizeof(buf),"%.1f %s",fsize,suf);
    }
    else
        snprintf(buf,sizeof(buf),"%llu %s",lsize,suf);
	
	hsize=buf;
	return hsize;
}


// Remove terminating '/' on a path string to simplify a file or directory path
// Thus '/bla/bla////' becomes '/bla/bla'
// Special case : '/' stays to '/'
FXString cleanPath(const FXString path)
{
	FXString in=path, out=path;
	while (1)
	{
		if (in[in.length()-1]=='/' && in.length()!=1)
		{
			out=in.trunc(in.length()-1);
			in=out;
		}
		else
			break;
	}	
	return (out);
}


// Return the absolute path, based on the current directory path
// Remove terminating '/' on a path string to simplify a file or directory path
// Thus '/bla/bla////' becomes '/bla/bla'
// Special case : '/' stays to '/'
FXString filePath(const FXString path)
{
	FXString in=path, out=path;
	while (1)
	{
		if (in[in.length()-1]=='/' && in.length()!=1)
		{
			out=in.trunc(in.length()-1);
			in=out;
		}
		else
			break;
	}
	FXString dir=FXSystem::getCurrentDirectory();

	// If absolute path
	if(ISPATHSEP(out[0]))
		return (out);
	else
		return (dir+PATHSEPSTRING+out);
}


// Return the absolute path, based on the specified directory path
// Remove terminating '/' on a path string to simplify a file or directory path
// Thus '/bla/bla////' becomes '/bla/bla'
// Special case : '/' stays to '/'
FXString filePath(const FXString path, const FXString dir)
{
	FXString in=path, out=path;
	while (1)
	{
		if (in[in.length()-1]=='/' && in.length()!=1)
		{
			out=in.trunc(in.length()-1);
			in=out;
		}
		else
			break;
	}
	// If absolute path
	if(ISPATHSEP(out[0]))
		return (out);
	else
		return (dir+PATHSEPSTRING+out);
}


// Obtain file path from URI specified as file:///bla/bla/bla...
// If no 'file:' prefix is found, return the input string as is
FXString fileFromURI(FXString uri)
{
	if(comparecase("file:",uri,5)==0)
	{
		if(uri[5]==PATHSEPCHAR && uri[6]==PATHSEPCHAR)
			return uri.mid(7,uri.length()-7);
		return uri.mid(5,uri.length()-5);
	}

	return uri;
}


// Return URI of filename
FXString fileToURI(const FXString& file)
{
	return "file://"+file;
}


// Convert the deletion date to the number of seconds since the epoch
// The string representing the deletion date must be in the format YYYY-MM-DDThh:mm:ss
long deltime(FXString delstr)
{	
	// Decompose the date into year, month, day, hour, minutes and seconds
	FXString year=delstr.mid(0,4);
	FXString mon=delstr.mid(5,2);
	FXString mday=delstr.mid(8,2);
	FXString hour=delstr.mid(11,2);
	FXString min=delstr.mid(14,2);
	FXString sec=delstr.mid(17,2);
	
	// Convert date using mktime()
	tm tmval;
	tmval.tm_sec=atoi(sec.text());
	tmval.tm_min=atoi(min.text());
	tmval.tm_hour=atoi(hour.text())-1;
	tmval.tm_mday=atoi(mday.text());
	tmval.tm_mon=atoi(mon.text())-1;
	tmval.tm_year=atoi(year.text())-1900;
	tmval.tm_isdst=0;
	long t=(long)mktime(&tmval);
	
	// If conversion failed, return 0
	if (t<0)
		t=0;
	
	return t;
}
	

// Test if a directory is empty
// Return -1 if not a directory, 1 if empty and 0 if not empty
int isEmptyDir(const FXString directory)
{
	int ret=-1;
	DIR* dir;
	struct dirent *entry;
	int n=0;
	
	if ((dir=opendir(directory.text()))!=NULL)
	{
		// Skip . and .. and read the third entry
		while (n<3)	{entry=readdir(dir); n++;}
		if (entry==NULL)
			ret=1;
		else
			ret=0;
	}
	if (dir)
		closedir(dir);
	return ret;
}
		

// Test if a directory has sub-directories
// Return -1 if not a directory, 1 if has sub-directories, 0 if does not have
int hasSubDirs(const FXString directory)
{
	int ret=-1;
	DIR* dir;
	struct dirent *entry;
	
	if ((dir=opendir(directory.text()))!=NULL)
	{
		ret=0;
		
		// Process directory entries
		while (1)
		{
			entry=readdir(dir);
			
			// No more entries
			if (entry==NULL)
				break;
			
			// Entry is . or ..
			else if (strcmp(entry->d_name,".")==0 || strcmp(entry->d_name,"..")==0)
				continue;
				
			// Regular entry
			// We don't use dirent.d_type anymore because of portability issues
			// (e.g. reiserfs don't know dirent.d_type)
			else
			{
				// Stat entry
				struct stat entrystat;
				FXString entrypath=directory + PATHSEPSTRING + entry->d_name;
				if (statrep(entrypath.text(),&entrystat)!=0)
					continue;
				
				// If directory
				if (S_ISDIR(entrystat.st_mode))
				{
					ret=1;
					break;
				}
			}
		}
	}
	if (dir)
		closedir(dir);
	return ret;
}


// Check if file exists
FXbool exists(const FXString& file)
{
  	struct stat linfo;
  	return !file.empty() && (lstatrep(file.text(),&linfo)==0);
}


// Check if the file represents a directory
FXbool isDirectory(const FXString& file)
{
  	struct stat info;
  	return !file.empty() && (statrep(file.text(),&info)==0) && S_ISDIR(info.st_mode);
}


// Check if file represents a file
FXbool isFile(const FXString& file)
{
  	struct stat info;
  	return !file.empty() && (statrep(file.text(),&info)==0) && S_ISREG(info.st_mode);
}


// Check if current user is member of gid
// (thanks to Armin Buehler <abuehler@users.sourceforge.net>)
FXbool isGroupMember(gid_t gid)
{
	static int ngroups = 0;
	static gid_t *gids = NULL;
	int i;

	// First call : initialization of the number of supplementary groups and the group list
	if (ngroups == 0)
	{
		ngroups = getgroups(0, gids);
		gids = new gid_t[ngroups];
		getgroups(ngroups, gids);
	}
	if (ngroups == 0)
		return FALSE;

	// Check if the group id is contained within the group list
	i = ngroups;
	while (i--)
	{
		if (gid == gids[i])
			return TRUE;
	}
	return FALSE;
}


// Check if the file or the link refered file is readable AND executable
// Function used to test if we can enter a directory
// Uses the access() system function 
FXbool isReadExecutable(const FXString& file)
{	
  	struct stat info;

	// File exists and can be stated
	if (!file.empty() && (statrep(file.text(),&info)==0))
	{
		int ret=access(file.text(),R_OK|X_OK);
		if (ret==0)
			return TRUE;
		else
			return FALSE;
	}	
	
	// File doesn't exist
	else
		return FALSE;
}


FXbool isReadable(const FXString& file)
{	
  	struct stat info;

	// File exists and can be stated
	if (!file.empty() && (statrep(file.text(),&info)==0))
	{
		int ret=access(file.text(),R_OK);
		if (ret==0)
			return TRUE;
		else
			return FALSE;
	}	
	
	// File doesn't exist
	else
		return FALSE;
}


FXbool isWritable(const FXString& file)
{	
  	struct stat info;

	// File exists and can be stated
	if (!file.empty() && (statrep(file.text(),&info)==0))
	{
		int ret=access(file.text(),W_OK);
		if (ret==0)
			return TRUE;
		else
			return FALSE;
	}	
	
	// File doesn't exist
	else
		return FALSE;
}


// Check if file represents a link
FXbool isLink(const FXString& file)
{
  	struct stat linfo;
  	return !file.empty() && (lstatrep(file.text(),&linfo)==0) && S_ISLNK(linfo.st_mode);
}


// Get file info (file or link refered file)
FXbool info(const FXString& file,struct stat& inf)
{
  	return !file.empty() && (statrep(file.text(),&inf)==0);
}


// Return permissions string
// (the FOX function FXSystem::modeString() seems to use another format for the mode field) 
FXString permissions(unsigned int mode)
{
	char result[11];
	result[0]=S_ISLNK(mode) ? 'l' : S_ISREG(mode) ? '-' : S_ISDIR(mode) ? 'd' : S_ISCHR(mode) ? 'c' : S_ISBLK(mode) ? 'b' : S_ISFIFO(mode) ? 'p' : S_ISSOCK(mode) ? 's' : '?';
	result[1]=(mode&S_IRUSR) ? 'r' : '-';
	result[2]=(mode&S_IWUSR) ? 'w' : '-';
	result[3]=(mode&S_ISUID) ? 's' : (mode&S_IXUSR) ? 'x' : '-';
	result[4]=(mode&S_IRGRP) ? 'r' : '-';
	result[5]=(mode&S_IWGRP) ? 'w' : '-';
	result[6]=(mode&S_ISGID) ? 's' : (mode&S_IXGRP) ? 'x' : '-';
	result[7]=(mode&S_IROTH) ? 'r' : '-';
	result[8]=(mode&S_IWOTH) ? 'w' : '-';
	result[9]=(mode&S_ISVTX) ? 't' : (mode&S_IXOTH) ? 'x' : '-';
	result[10]=0;
	return result;
}


// Read symbolic link
FXString readLink(const FXString& file)
{
	char lnk[MAXPATHLEN+1];
	int len=readlink(file.text(),lnk,MAXPATHLEN);
	if(0<=len)
  		return FXString(lnk,len);
  	else
  		return FXString::null;
}


// Return true if files are identical
// Compare file names and inodes for case insensitive filesystems
FXbool identical(const FXString& file1,const FXString& file2)
{
	if(file1!=file2)
	{
    	struct stat linfo1, linfo2;
    	return !::lstatrep(file1.text(),&linfo1) && !::lstatrep(file2.text(),&linfo2) && linfo1.st_ino==linfo2.st_ino && linfo1.st_dev==linfo2.st_dev;
    }
  	return TRUE;
}


// Start or stop wait cursor (start if type is BEGIN_CURSOR, stop if type is END_CURSOR)
// Do nothing if type is QUERY_CURSOR or anything different from BEGIN_CURSOR and END_CURSOR)
// Return wait cursor count (0 means wait cursor is not set)
int setWaitCursor(FXApp* app, unsigned int type)
{
	static int waitcount=0;
	
	// Begin wait cursor
	if (type==BEGIN_CURSOR)
	{
		app->beginWaitCursor();
		waitcount++;
	}
	
	// End wait cursor
	else if (type==END_CURSOR)
	{
		app->endWaitCursor();
		if (waitcount >= 1)
			waitcount--;
		else
			waitcount=0;
	}
	
	// Other cases : do nothing 
	else
		;
	
	return waitcount;
}
		

// Run a command in an internal Xvt terminal
// Return 0 if success, -1 else
// N.B.: zombie process should be dealt with in the main application class
int runinxvt(FXString cmd)
{
	FXString str1, str2;
	int nbargs, i, j;

	// First pass to find the number of commmand arguments
	nbargs=0;
	i=0;
	j=1;
	while (1)
	{
		str1=cmd.section(' ',i);
		if (str1[0]=='\'')       // If a ' is found, ignore the spaces till the next '
		{
			str2=cmd.section('\'',j);
			j+=2;
			i+=str2.contains(' ');
			nbargs++;
		}
		else
			nbargs++;
		
		if(streq(str1.text(),""))
			break;
		
		i++;
	}
	nbargs--;

	// Second pass to allocate the argument strings	
	char** args=(char**)malloc((nbargs + 1)*sizeof(char*));
	nbargs=0;
	i=0;
	j=1;
	while (1)
	{
		str1=cmd.section(' ',i);		
		if (str1[0]=='\'')
		{
			str2=cmd.section('\'',j);
			j+=2;
			i+=str2.contains(' ');
			args[nbargs] = (char *)malloc(str2.length()+1);
			strlcpy(args[nbargs],str2.text(),str2.length()+1);
			nbargs++;
		}
		else
		{
			args[nbargs] = (char *)malloc(str1.length()+1);
			strlcpy(args[nbargs],str1.text(),str1.length()+1);
			nbargs++;
		}
		
		if(streq(str1.text(),""))
			break;

		i++;
	}
	nbargs--;
	args[nbargs]=NULL;

	// Launch the command in an internal Xvt terminal
	int res;
	static pid_t childpid = 0;
    childpid = fork();
 
    // Fork succeeded
    if (childpid >= 0)
    {
        // Child process
        if (childpid == 0)
        {
			xvt(nbargs,args);
            exit(0);
        }
        
        // Parent process
        else
        {
			// Non blocking wait for child
            waitpid(childpid,NULL,WNOHANG);
            res=0;
        }
    }
    
    // Fork failed
    else
    {
		fprintf (stderr,_("Error! Fork failed: %s\n"),strerror (errno));
		res=-1;
    }
	
	// Free allocated strings
	for (int i=0; i<nbargs; i++)
		free(args[i]);
	free(args);
	
	return res;
}


