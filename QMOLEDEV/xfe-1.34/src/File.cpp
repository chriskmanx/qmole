// File management class with progress dialog 

#include "config.h"
#include "i18n.h"

#include <fcntl.h>
#include <utime.h>
#if defined(linux)
#include <sys/statfs.h>
#endif

// For Sun compatibility
#ifdef __sun
#include <alloca.h>
#endif


#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>
#include <FXUTF8Codec.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "OverwriteBox.h"
#include "MessageBox.h"
#include "CommandWindow.h"
#include "File.h"


// Delay before the progress bar should be shown (ms)
#define SHOW_PROGRESSBAR_DELAY 1000

// Progress dialog width
#define PROGRESSDIALOG_WIDTH 200



// Message Map
FXDEFMAP(File) FileMap[]={
                             FXMAPFUNC(SEL_COMMAND,File::ID_CANCEL_BUTTON,File::onCmdCancel),
							 FXMAPFUNC(SEL_TIMEOUT,File::ID_TIMEOUT,File::onTimeout),
                         };

// Object implementation
FXIMPLEMENT(File,DialogBox,FileMap,ARRAYNUMBER(FileMap))

// Construct object
File::File(FXWindow *owner, FXString title, const unsigned int operation):DialogBox(owner,title,DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE)
{
    // Progress window
	FXPacker *buttons=new FXPacker(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,10,10,PROGRESSDIALOG_WIDTH,PROGRESSDIALOG_WIDTH,5,5);
    new FXHorizontalSeparator(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X|SEPARATOR_GROOVE);
    FXVerticalFrame *contents=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Cancel Button
    cancelButton=new FXButton(buttons,_("&Cancel"),NULL,this,File::ID_CANCEL_BUTTON,FRAME_RAISED|FRAME_THICK|LAYOUT_CENTER_X,0,0,0,0,20,20);
    cancelButton->setFocus();
    cancelButton->addHotKey(KEY_Escape);
	cancelled=FALSE;

	// Progress bar
	progressbar=NULL;
	
	// Progress bar colors (foreground, text and alternate text)
	unsigned int r, g, b, l;
	FXColor textcolor, textaltcolor;
	FXColor fgcolor=getApp()->reg().readColorEntry("SETTINGS","pbarcolor",FXRGB(0,0,255));
	FXColor bgcolor=getApp()->reg().readColorEntry("SETTINGS","backcolor",FXRGB(0,0,255));

	// Text color is white or black depending on the foreground luminance
	r=FXREDVAL(fgcolor);                                                     
	g=FXGREENVAL(fgcolor);                                                   
	b=FXBLUEVAL(fgcolor);
	l=(unsigned int)(0.3*r+0.59*g+0.11*b);                                                                                  
	if (l<150)
		textaltcolor=FXRGB(255,255,255);
	else
		textaltcolor=FXRGB(0,0,0);
	
	// Alternate text color is white or black depending on the background luminance
	r=FXREDVAL(bgcolor);                                                     
	g=FXGREENVAL(bgcolor);                                                   
	b=FXBLUEVAL(bgcolor);
	l=(unsigned int)(0.3*r+0.59*g+0.11*b);                                                                                  
	if (l<150)
		textcolor=FXRGB(255,255,255);
	else
		textcolor=FXRGB(0,0,0);
	
	// Progress dialog depends on the file operation
	switch(operation)	
	{
		case COPY:
	    	// Labels and progress bar
			uplabel=new FXLabel(contents,_("Source:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("Target:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
			progressbar=new FXProgressBar(contents,NULL,0,LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK|PROGRESSBAR_PERCENTAGE,0,0,0,0,PROGRESSDIALOG_WIDTH);
			progressbar->setBarColor(fgcolor);
			progressbar->setTextColor(textcolor);
		 	progressbar->setTextAltColor(textaltcolor);
			
			// Timer on
			getApp()->addTimeout(this,File::ID_TIMEOUT,SHOW_PROGRESSBAR_DELAY);
			break;

    	case MOVE:
	    	// Labels and progress bar
			uplabel=new FXLabel(contents,_("Source:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("Target:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
			progressbar=new FXProgressBar(contents,NULL,0,LAYOUT_FILL_X|FRAME_SUNKEN|FRAME_THICK|PROGRESSBAR_PERCENTAGE,0,0,0,0,PROGRESSDIALOG_WIDTH);
			progressbar->setBarColor(fgcolor);
			progressbar->setTextColor(textcolor);
		 	progressbar->setTextAltColor(textaltcolor);

			// Timer on
			getApp()->addTimeout(this,File::ID_TIMEOUT,SHOW_PROGRESSBAR_DELAY);
			break;

    	case DELETE:
	    	// Labels
			uplabel=new FXLabel(contents,_("Delete:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("From:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);

			// Timer on
			getApp()->addTimeout(this,File::ID_TIMEOUT,SHOW_PROGRESSBAR_DELAY);
			break;

    	case CHMOD:
	    	// Labels
			uplabel=new FXLabel(contents,_("Changing permissions..."),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("File:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);

			// Timer on
			getApp()->addTimeout(this,File::ID_TIMEOUT,SHOW_PROGRESSBAR_DELAY);
			break;

    	case CHOWN:
	    	// Labels
			uplabel=new FXLabel(contents,_("Changing owner..."),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("File:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);

			// Timer on
			getApp()->addTimeout(this,File::ID_TIMEOUT,SHOW_PROGRESSBAR_DELAY);
			break;

#if defined(linux)
    	case MOUNT:
	    	// Labels
			uplabel=new FXLabel(contents,_("Mount file system..."),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("Mount the folder:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
			break;

    	case UNMOUNT:
	    	// Labels
			uplabel=new FXLabel(contents,_("Unmount file system..."),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
    		downlabel=new FXLabel(contents,_("Unmount the folder:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_X);
			break;
#endif

		default: // Other : RENAME, SYMLINK, ARCHIVE, EXTRACT, PKG_INSTALL, PKG_UNINSTALL
			// Progress dialog not used
			uplabel=NULL;
    		downlabel=NULL;
	}
	
	FXbool confirm_overwrite=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_overwrite",TRUE);
	
	// Initialize the overwrite flags
	if (confirm_overwrite)
	{
		overwrite=FALSE;
		overwrite_all=FALSE;
		skip_all=FALSE;
	}
	else
	{
		overwrite=TRUE;
		overwrite_all=TRUE;
		skip_all=FALSE;
	}

	// Error message box
	mbox=new MessageBox(this,_("Error"),"",errorbigicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);   
}

// Destructor
File::~File()
{
	getApp()->removeTimeout(this,File::ID_TIMEOUT);
	delete progressbar;
	delete mbox;
}



// Create and initialize
void File::create()
{
	DialogBox::create();
}


// Force timeout for progress dialog (used before opening confirmation or error dialogs)
void File::forceTimeout(void)
{	
	getApp()->removeTimeout(this,File::ID_TIMEOUT);
	hide();
	getApp()->forceRefresh();
	getApp()->flush();
}


// Restart timeout for progress dialog  (used after closing confirmation or error dialogs)
void File::restartTimeout(void)
{	
	getApp()->addTimeout(this,File::ID_TIMEOUT,SHOW_PROGRESSBAR_DELAY);
}


// Read bytes
FXlong File::fullread(int fd, unsigned char* ptr, FXlong len)
{
    FXlong nread;
#ifdef EINTR
    do
        nread=read(fd,ptr,len);
    while(nread<0 && errno==EINTR);
#else
    nread=read(fd,ptr,len);
#endif
    return nread;
}


// Write bytes
FXlong File::fullwrite(int fd, const unsigned char* ptr, FXlong len)
{
    FXlong nwritten, ntotalwritten=0;
    while(len>0)
    {
        nwritten=write(fd,ptr,len);
        if(nwritten<0)
        {
#ifdef EINTR
            if(errno==EINTR)
                continue;
#endif
            return -1;
        }
        ntotalwritten+=nwritten;
        ptr+=nwritten;
        len-=nwritten;
    }
    return ntotalwritten;
}


// Construct overwrite dialog and get user answer
unsigned int File::getOverwriteAnswer(FXString srcpath, FXString tgtpath)
{
	// Message string
	FXString msg;
	if (::isDirectory(tgtpath))
		msg.format(_("Folder %s already exists. Overwrite?\n=> Caution, all files within this directory will be definitively lost!"),tgtpath.text());
	else
		msg.format(_("File %s already exists. Overwrite?"),tgtpath.text());

	// Read time format
	FXString timeformat=getApp()->reg().readStringEntry("SETTINGS","time_format",DEFAULT_TIME_FORMAT);

	// Get the size and mtime of the source and target
	struct stat linfo;
	FXString srcsize, srcmtime, tgtsize, tgtmtime;
	FXbool statsrc=FALSE, stattgt=FALSE;
	if (lstatrep(srcpath.text(),&linfo)==0)
	{
		statsrc=TRUE;
		srcmtime=FXSystem::time(timeformat.text(),linfo.st_mtime);
		char buf[MAXPATHLEN];
		if (S_ISDIR(linfo.st_mode)) // Folder
		{
			FXulong dirsize=0;
			unsigned int nbfiles=0, nbsubfolders=0;
			strlcpy(buf,srcpath.text(),srcpath.length()+1);
			dirsize=pathsize(buf,&nbfiles,&nbsubfolders);
			snprintf(buf,sizeof(buf),"%llu",dirsize);
		}
		else // File
			snprintf(buf,sizeof(buf),"%llu",(FXulong)linfo.st_size);					
		srcsize=::hSize(buf);
	}			
	if (lstatrep(tgtpath.text(),&linfo)==0)
	{
		stattgt=TRUE;
		tgtmtime=FXSystem::time(timeformat.text(),linfo.st_mtime);
		char buf[64];
		if (S_ISDIR(linfo.st_mode)) // Folder
		{
			FXulong dirsize=0;
			unsigned int nbfiles=0, nbsubfolders=0;
			strlcpy(buf,tgtpath.text(),tgtpath.length()+1);
			dirsize=pathsize(buf,&nbfiles,&nbsubfolders);
			snprintf(buf,sizeof(buf),"%llu",dirsize);
		}
		else // File
			snprintf(buf,sizeof(buf),"%llu",(FXulong)linfo.st_size);					
		tgtsize=::hSize(buf);
	}			
	
	// Include the size and mtime in the message
	if (statsrc && stattgt)
	{
		FXString msgsrc, msgtgt;
		msgsrc.format(_("Source size: %s - Modified date: %s"),srcsize.text(),srcmtime.text());
		msgtgt.format(_("Target size: %s - Modified date: %s"),tgtsize.text(),tgtmtime.text());
		msg+="\n\n"+msgsrc+"\n"+msgtgt;
	}
	
	// Overwrite dialog
	OverwriteBox* dlg = new OverwriteBox(this,_("Confirm Overwrite"),msg);
	unsigned int answer=dlg->execute(PLACEMENT_CURSOR);
	delete dlg;
	restartTimeout();

	return answer;
}


// Copy ordinary file
int File::copyfile(const FXString& source, const FXString& target, const FXbool preserve_date)
{
	FXString destfile;
	unsigned char buffer[32768];
    struct stat info;
	struct utimbuf timbuf;
    FXlong nread, nwritten;
	FXlong size, nread_all=0;
	double pct=0;
    int src, dst;
    FXbool ok=FALSE;

	FXbool warn=getApp()->reg().readUnsignedEntry("OPTIONS","preserve_date_warn",TRUE);

    if((src=::open(source.text(),O_RDONLY))>=0)
    {
        if(statrep(source.text(),&info)==0)
        {
			// If destination is a directory
			if(::isDirectory(target))
				destfile=target+PATHSEPSTRING+FXPath::name(source);
			else
				destfile=target;
				
         	// Copy file block by block
			size=info.st_size; 
			if((dst=::open(destfile.text(),O_WRONLY|O_CREAT|O_TRUNC,info.st_mode))>=0)
            {
                while(1)
                {
                    errno=0;
                    nread=File::fullread(src,buffer,sizeof(buffer));
                    int errcode=errno;
                    if(nread<0)
					{
						forceTimeout();

						FXString str;
						if (errcode)
							str.format(_("Can't copy file %s: %s"),target.text(),strerror(errcode));		
						else
							str.format(_("Can't copy file %s"),target.text());
						mbox->setText(str);
						unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

						restartTimeout();
						if(answer == BOX_CLICKED_CANCEL)
						{
							::close(dst);
							::close(src);
							cancelled=TRUE;
							return FALSE;
						}					
					}
                    if(nread==0)
                        break;
											
					// Force timeout checking for progress dialog
					checkTimeout();

					// Set percentage value for progress dialog
					nread_all+=nread;
					pct=100*(double)nread_all/(double)size;	
					if (progressbar)
						progressbar->setProgress((int)pct);

					// Give cancel button an opportunity to be clicked
					if(cancelButton)
						getApp()->runModalWhileEvents(cancelButton);

					// Set labels for progress dialog
					FXString label=_("Source: ")+source;
					uplabel->setText(label);
					label=_("Target: ")+target;
					downlabel->setText(label);
					getApp()->repaint();

					// If cancel button was clicked, close files and return
					if (cancelled)
					{
						::close(dst);
						::close(src);
						return FALSE;
					}
                    errno=0;
                    nwritten=File::fullwrite(dst,buffer,nread);
                    errcode=errno;
                    if(nwritten<0)
					{
						forceTimeout();

						FXString str;
						if (errcode)
							str.format(_("Can't copy file %s: %s"),target.text(),strerror(errcode));		
						else
							str.format(_("Can't copy file %s"),target.text());
						mbox->setText(str);
						unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

						restartTimeout();
						if(answer == BOX_CLICKED_CANCEL)
						{
							::close(dst);
							::close(src);
							cancelled=TRUE;
							return FALSE;
						}					
					}
                }
                ok=TRUE;
                ::close(dst);
				
				// Keep original date if asked
				if (preserve_date)
				{
					timbuf.actime=info.st_atime;
					timbuf.modtime=info.st_mtime;
					errno=0;
					int rc=utime(destfile.text(),&timbuf);
					int errcode=errno;
					if (warn && rc)
					{
						forceTimeout();

						FXString str;
						if (errcode)
							str.format(_("Can't preserve date when copying file %s : %s"),target.text(),strerror(errcode));		
						else
							str.format(_("Can't preserve date when copying file %s"),target.text());
						mbox->setText(str);
						unsigned int answer=mbox->execute(PLACEMENT_CURSOR);
						
						restartTimeout();
						if(answer == BOX_CLICKED_CANCEL)
						{
							::close(src);
							cancelled=TRUE;
							return FALSE;
						}					
					}
				}
            }

#if defined(linux)
			// If source file is on a ISO9660 file system (CD or DVD, thus read-only)
			// then add to the target the write permission for the user
			if (ok)
			{
				struct statfs fs;
				if (statfs(source.text(),&fs)==0 && fs.f_type==0x9660)
					::chmod(target.text(),info.st_mode|S_IWUSR);
			}
#endif
        }
        ::close(src);
    }

	// File cannot be opened
 	else
 	{
		forceTimeout();
		int errcode=errno;

		FXString str;
		if (errcode)
			str.format(_("Can't copy file %s: %s"),target.text(),strerror(errcode));		
		else
			str.format(_("Can't copy file %s"),target.text());
		mbox->setText(str);
		unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

		restartTimeout();
		if(answer == BOX_CLICKED_CANCEL)
		{
			::close(src);
			cancelled=TRUE;
			return FALSE;
		}
		ok=-1; // Prevent displaying an error message
		       // in the calling function			
	}
    return ok;
}


// Copy directory
int File::copydir(const FXString& source,const FXString& target,struct stat& parentinfo,inodelist* inodes, const FXbool preserve_date)
{
    DIR *dirp;
    struct dirent *dp;
    struct stat linfo;
	struct utimbuf timbuf;
    inodelist *in,inode;
    FXString destfile,oldchild,newchild;

	FXbool warn=getApp()->reg().readUnsignedEntry("OPTIONS","preserve_date_warn",TRUE);

	// Destination file
	destfile=target;

    // See if visited this inode already
    for(in=inodes; in; in=in->next)
    {
        if(in->st_ino==parentinfo.st_ino)
            return TRUE;
    }

    // Try make directory, if none exists yet
    if(mkdir(destfile.text(),parentinfo.st_mode|S_IWUSR)!=0 && errno!=EEXIST)
        return FALSE;

    // Can we stat it
    if(lstatrep(destfile.text(),&linfo)!=0 || !S_ISDIR(linfo.st_mode))
        return FALSE;

    // Try open directory to copy
    dirp=opendir(source.text());
    if(!dirp)
        return FALSE;

    // Add this to the list
    inode.st_ino=linfo.st_ino;
    inode.next=inodes;

    // Copy stuff
    while((dp=readdir(dirp))!=NULL)
    {
        if(dp->d_name[0]!='.' || (dp->d_name[1]!='\0' && (dp->d_name[1]!='.' || dp->d_name[2]!='\0')))
        {
            oldchild=source;
            if(!ISPATHSEP(oldchild[oldchild.length()-1]))
                oldchild.append(PATHSEP);
            oldchild.append(dp->d_name);
            newchild=destfile;
            if(!ISPATHSEP(newchild[newchild.length()-1]))
                newchild.append(PATHSEP);
            newchild.append(dp->d_name);
			if(!copyrec(oldchild,newchild,&inode,preserve_date))
            {
				// If the cancel button was pressed
				if (cancelled)
				{
                	closedir(dirp);
                	return FALSE;
				}
				
				// Or a permission problem occured
				else
				{
					FXString str;
					if (::isDirectory(oldchild))
						str.format(_("Can't copy folder %s : Permission denied"),oldchild.text());
					else
						str.format(_("Can't copy file %s : Permission denied"),oldchild.text());
       				forceTimeout();
					mbox->setText(str);
					unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

					restartTimeout();
					if(answer == BOX_CLICKED_CANCEL)
					{
               			closedir(dirp);
						cancelled=TRUE;
                		return FALSE;
					}					
				}
            }
        }
    }

    // Close directory
    closedir(dirp);

	// Keep original date if asked
	if (preserve_date)
	{
		if(lstatrep(source.text(),&linfo)==0)
		{
			timbuf.actime=linfo.st_atime;
			timbuf.modtime=linfo.st_mtime;
			errno=0;
			int rc=utime(destfile.text(),&timbuf);
			int errcode=errno;
			if (warn && rc)
			{
				forceTimeout();
				FXString str;
				if (errcode)
					str.format(_("Can't preserve date when copying folder %s : %s"),target.text(),strerror(errcode));		
				else
					str.format(_("Can't preserve date when copying folder %s"),target.text());
				mbox->setText(str);
				unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

				restartTimeout();
				if(answer == BOX_CLICKED_CANCEL)
				{
					cancelled=TRUE;
					return FALSE;
				}					
			}
		}
	}
	
    // Success
    return TRUE;
}


// Recursive copy
int File::copyrec(const FXString& source,const FXString& target,inodelist* inodes, const FXbool preserve_date)
{
    struct stat linfo1, linfo2;
	
    // Source file or directory does not exist
    if(lstatrep(source.text(),&linfo1)!=0)
        return FALSE;
    
    // If target is not a directory, remove it if allowed
    if(lstatrep(target.text(),&linfo2)==0)
    {
        if(!S_ISDIR(linfo2.st_mode))
        {
			if(!(overwrite|overwrite_all))
               return FALSE;
            if(::unlink(target.text())!=0)
               return FALSE;
        }
    }

	// Source is directory: copy recursively
    if(S_ISDIR(linfo1.st_mode))
        return File::copydir(source,target,linfo1,inodes,preserve_date);
   
    // Source is regular file: copy block by block
    if(S_ISREG(linfo1.st_mode))
        return File::copyfile(source,target,preserve_date);

 	// Remove target if it already exists
 	if (::exists(target))
 	{
 		int ret=File::remove(target);
 		if (!ret)
 			return FALSE;
	}

    // Source is fifo: make a new one
    if(S_ISFIFO(linfo1.st_mode))
        return mkfifo(target.text(),linfo1.st_mode);

    // Source is device: make a new one
    if(S_ISBLK(linfo1.st_mode) || S_ISCHR(linfo1.st_mode) || S_ISSOCK(linfo1.st_mode))
        return mknod(target.text(),linfo1.st_mode,linfo1.st_rdev)==0;

    // Source is symbolic link: make a new one
    if(S_ISLNK(linfo1.st_mode))
    {
		FXString lnkfile=::readLink(source);
        return ::symlink(lnkfile.text(),target.text())==0;
    }

    // This shouldn't happen
    return FALSE;
}



// Copy file (with progress dialog)
// Return  0 to allow displaying an error message in the calling routine
// Return -1 to prevent displaying an error message in the calling routine
int File::copy(const FXString& source, const FXString& target, const FXbool confirm_dialog, const FXbool preserve_date)
{
	FXString targetfile;

	// Source doesn't exist
	if(!::exists(source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Source %s doesn't exist"), source.text());
		return -1;
	}

	// Source and target are identical
	if (::identical(target,source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Destination %s is identical to source"), target.text());
		return -1;
	}

	// Target is an existing directory
	if (::isDirectory(target))
		targetfile=target+PATHSEPSTRING+FXPath::name(source);
	else
		targetfile=target;

	// Source and target file are identical
	if (::identical(targetfile,source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Destination %s is identical to source"), targetfile.text());			
		return -1;
	}

	// Target already exists
	if(::exists(targetfile))
	{
		// Overwrite dialog if necessary
		if (!(overwrite_all | skip_all) & confirm_dialog)
    	{			
			FXString label=_("Source: ")+source;
			uplabel->setText(label);
			label=_("Target: ")+targetfile;
			downlabel->setText(label);
			getApp()->repaint();
			forceTimeout();
			unsigned int answer=getOverwriteAnswer(source,targetfile);
       		switch(answer)
       		{
				// Cancel
				case 0:
					forceTimeout();
					cancelled=TRUE;
					return FALSE;
					break;
				
				// Overwrite
				case 1:
					overwrite=TRUE;
					break;
				
				// Overwrite all
				case 2:
					overwrite_all=TRUE;
					break;
				
				// Skip
				case 3:
					overwrite=FALSE;
					break;
				
				// Skip all
				case 4:
					skip_all=TRUE;
					break;
       		}
		}
		if(!(overwrite | overwrite_all) | skip_all)
			return TRUE;
				
		// Remove targetfile if source is not a directory
		if (!::isDirectory(source))
		{ 
			if (File::remove(targetfile)==FALSE)
			{
				forceTimeout();
				return FALSE;
			}
		}	
	}

	// Copy file or directory
	return File::copyrec(source,targetfile,NULL,preserve_date);

}


// Remove file or directory (with progress dialog)
int File::remove(const FXString& file)
{
	FXString dirname;
    struct stat linfo;
	static FXbool ISDIR=FALSE;  // Caution! ISDIR is common to all File instances, is that we want?

    if(lstatrep(file.text(),&linfo)==0)
    {
        // It is a directory
		if(S_ISDIR(linfo.st_mode))
        {
            DIR *dirp=opendir(file.text());
            if(dirp)
            {			
                struct dirent *dp;
                FXString child;
				
				// Used to display only one progress dialog when deleting a directory
				ISDIR=TRUE;
							
				// Force timeout checking for progress dialog
				checkTimeout();

				// Give cancel button an opportunity to be clicked
				if(cancelButton)
					getApp()->runModalWhileEvents(cancelButton);

				// Set labels for progress dialog
				FXString label=_("Delete folder: ")+file;
				if (uplabel)
					uplabel->setText(label);
				dirname=FXPath::directory(FXPath::absolute(file));
				label=_("From: ")+dirname;
				if (downlabel)
					downlabel->setText(label);
				getApp()->repaint();

				// If cancel button was clicked, return
				if (cancelled)
					return FALSE;

                while((dp=readdir(dirp))!=NULL)
                {
					if(dp->d_name[0]!='.' || (dp->d_name[1]!='\0' && (dp->d_name[1]!='.' || dp->d_name[2]!='\0')))
                    {
                        child=file;
                        if(!ISPATHSEP(child[child.length()-1]))
                            child.append(PATHSEP);
                        child.append(dp->d_name);
                        if(!File::remove(child))
						{
							closedir(dirp);
							return FALSE;
						}
                    }
                }
                closedir(dirp);
            }
			if (rmdir(file.text())==-1)
			{
				int errcode=errno;
				forceTimeout();

				FXString str;
				if (errcode)
					str.format(_("Can't delete folder %s: %s"),file.text(),strerror(errcode));		
				else
					str.format(_("Can't delete folder %s"),file.text());
				mbox->setText(str);
				unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

				restartTimeout();
				if(answer == BOX_CLICKED_CANCEL)
				{
					cancelled=TRUE;
					return FALSE;
				}
				return -1; // To prevent displaying an error message
				           // in the calling function					
			}
			else
				return TRUE;
        }
        else
        {
			// If it was not a directory
			if (!ISDIR)
			{
				// Force timeout checking for progress dialog
				checkTimeout();

				// Give cancel button an opportunity to be clicked
				if(cancelButton)
					getApp()->runModalWhileEvents(cancelButton);

				// Set labels for progress dialog
				FXString label=_("Delete: ")+file;
				if (uplabel)
					uplabel->setText(label);
				dirname=FXPath::directory(FXPath::absolute(file));
				label=_("From: ")+dirname;
				if (downlabel)
					downlabel->setText(label);
				getApp()->repaint();

				// If cancel button was clicked, return
				if (cancelled)
					return FALSE;
			}
			if (::unlink(file.text())==-1)
			{
				int errcode=errno;
				forceTimeout();

				FXString str;
				if (errcode)
					str.format(_("Can't delete file %s: %s"),file.text(),strerror(errcode));			
				else
					str.format(_("Can't delete file %s"),file.text());
				mbox->setText(str);
				unsigned int answer=mbox->execute(PLACEMENT_CURSOR);

				restartTimeout();
				if(answer == BOX_CLICKED_CANCEL)
				{
					cancelled=TRUE;
					return FALSE;
				}
				return -1; // To prevent displaying an error message
				           // in the calling function					
			}
			else
				return TRUE;
        }
    }
    return FALSE;
}


// Rename a file or a directory (no progress dialog)
// Return  0 to allow displaying an error message in the calling routine
// Return -1 to prevent displaying an error message in the calling routine
int File::rename(const FXString& source, const FXString& target)
{
	// Source doesn't exist
    if(!::exists(source))
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("Source %s doesn't exist"), source.text());
		return -1;
	}
			
	// Source and target are identical
	if (::identical(target,source))
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("Destination %s is identical to source"), target.text());
		return -1;
	}

	// Target already exists
	if(::exists(target))
	{
		// Overwrite dialog if necessary
		if (!(overwrite_all | skip_all))
    	{
       		unsigned int answer=getOverwriteAnswer(source,target);
			switch(answer)
       		{
				// Cancel
				case 0:
					return FALSE;
					break;
				
				// Overwrite
				case 1:
					overwrite=TRUE;
					break;
				
				// Overwrite all
				case 2:
					overwrite_all=TRUE;
					break;
				
				// Skip
				case 3:
					overwrite=FALSE;
					break;
				
				// Skip all
				case 4:
					skip_all=TRUE;
					break;
       		}
		}
		if(!(overwrite | overwrite_all) | skip_all)
			return TRUE;
	}
       		
	// Rename file using the standard C function
	// This should only work for files that are on the same file system
	if (::rename(source.text(),target.text())==0)
		return TRUE;
	if(errno!=EXDEV)
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Can't rename to target %s"), target.text());	
		return FALSE;
	}

	// If files are on different file systems, use the copy/delete scheme and preserve the original date
	int ret=this->copy(source,target,FALSE,TRUE);
	if (ret)
		return (remove(source.text())==TRUE);
	else
		return FALSE;
}


// Move files
// Return  0 to allow displaying an error message in the calling routine
// Return -1 to prevent displaying an error message in the calling routine
int File::move(const FXString& source,const FXString& target,const FXbool restore)
{
	// Source doesn't exist
	if(!::exists(source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Source %s doesn't exist"), source.text());
		return -1;
	}

	// Source and target are identical
	if (identical(target,source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Destination %s is identical to source"), target.text());
		return -1;
	}

	// Source path is included into target path
	FXString str=source + PATHSEPSTRING;
	if (target.left(str.length()) == str)
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Target %s is a sub-folder of source"),target.text());
		return -1;
		
	}

	// Target is an existing directory (don't do this in the restore case)
	FXString targetfile;
	if (!restore && ::isDirectory(target))
		targetfile=target+PATHSEPSTRING+FXPath::name(source);
	else
		targetfile=target;

	// Source and target file are identical
	if (::identical(targetfile,source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Destination %s is identical to source"), target.text());
		return -1;
	}

	// Force timeout checking for progress dialog
	checkTimeout();

	// Give cancel button an opportunity to be clicked
	if(cancelButton)
		getApp()->runModalWhileEvents(cancelButton);

	// Set labels for progress dialog
	FXString label=_("Source: ")+source;
	uplabel->setText(label);
	label=_("Target: ")+target;
	downlabel->setText(label);
	getApp()->repaint();

	// Target file already exists
	if(::exists(targetfile))
    {
		// Overwrite dialog if necessary
		if (!overwrite_all & !skip_all)
		{
			forceTimeout();
			unsigned int answer=getOverwriteAnswer(source,targetfile);
 			restartTimeout();
       		switch(answer)
       		{
				// Cancel
				case 0:
					forceTimeout();
					cancelled=TRUE;
					return FALSE;
					break;
				
				// Overwrite
				case 1:
					overwrite=TRUE;
					break;
				
				// Overwrite all
				case 2:
					overwrite_all=TRUE;
					break;
				
				// Skip
				case 3:
					overwrite=FALSE;
					break;
				
				// Skip all
				case 4:
					skip_all=TRUE;
					break;
       		}
			
		}
		if(!(overwrite | overwrite_all) | skip_all)
		{
			// Hide progress dialog and restart timer
			forceTimeout();
 			restartTimeout();
			
			return TRUE;
		}
	}

	// Set the progress to 100% for move file operation
	if (progressbar)
		progressbar->setProgress(100);

	// Rename file using the standard C function
	// This should only work for files that are on the same file system
	if (::rename(source.text(),targetfile.text())==0)
		return TRUE;
/*
	if(errno!=EXDEV)
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Can't rename to target %s"), targetfile.text());
		return FALSE;
	}
*/

	// If files are on different file systems, use the copy/delete scheme and preserve the original date
	int ret=this->copy(source,targetfile,FALSE,TRUE);
	if (ret)
		return (remove(source.text())==TRUE);
	else
		return FALSE;
}


// Symbolic Link file (no progress dialog)
// Return  0 to allow displaying an error message in the calling routine
// Return -1 to prevent displaying an error message in the calling routine
int File::symlink(const FXString& source,const FXString& target)
{
	// Source doesn't exist
    if(!::exists(source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Source %s doesn't exist"), source.text());
		return -1;
	}
			
	// Source and target are identical
	if (::identical(target,source))
	{
		forceTimeout();
		MessageBox::error(this,BOX_OK,_("Error"),_("Destination %s is identical to source"), target.text());
		return -1;
	}

	// Target is an existing directory
	FXString targetfile;
	if (::isDirectory(target))
		targetfile=target+PATHSEPSTRING+FXPath::name(source);
	else
		targetfile=target;

	// Target already exists
	if(::exists(targetfile))
	{
		// Overwrite dialog if necessary
		if (!(overwrite_all | skip_all))
    	{
			unsigned int answer=getOverwriteAnswer(source,targetfile);
       		switch(answer)
       		{
				// Cancel
				case 0:
					forceTimeout();
					return FALSE;
					break;
				
				// Overwrite
				case 1:
					overwrite=TRUE;
					break;
				
				// Overwrite all
				case 2:
					overwrite_all=TRUE;
					break;
				
				// Skip
				case 3:
					overwrite=FALSE;
					break;
				
				// Skip all
				case 4:
					skip_all=TRUE;
					break;
       		}
		}
		if(!(overwrite | overwrite_all) | skip_all)
			return TRUE;
	}
    
	// Create symbolic link using the standard C function
	errno=0;

	// Use the relative path for the symbolic link
	FXString relativepath;
	if (::exists(target) && ::isDirectory(target))
		relativepath=FXPath::relative(target,source);
	else
		relativepath=FXPath::relative(FXPath::directory(target),source);
	
	int ret=::symlink(relativepath.text(),targetfile.text());

	int errcode=errno;	
	if (ret==0)
		return TRUE;
	else
	{
		forceTimeout();
		if (errcode)
			MessageBox::error(this,BOX_OK,_("Error"),_("Can't symlink %s: %s"),target.text(),strerror(errcode));
		else
			MessageBox::error(this,BOX_OK,_("Error"),_("Can't symlink %s"),target.text());
		return FALSE;
	}		
}


// Chmod a file or directory, recursively or not
// We don't process symbolic links (since their permissions cannot be changed)
//
// Note : the variable file returns the last processed file
// It can be different from the initial path, if recursive chmod is used
// (Used to fill an error message, if needed) 
int File::chmod(char* path, char* file, mode_t mode, FXbool rec, const FXbool dironly, const FXbool fileonly)
{			
    struct stat linfo;

	// Initialise the file variable with the initial path
	strlcpy(file, path, strlen(path)+1);

    // If it doesn't exist
	if (lstatrep(path, &linfo))
        return -1;

	// If it's a symbolic link
	if (S_ISLNK(linfo.st_mode))
		return 0;

    if (!S_ISDIR(linfo.st_mode)) // File
	{
        if(dironly)
            return 0;
 
		// Force timeout checking for progress dialog
		checkTimeout();

		// Give cancel button an opportunity to be clicked
		if(cancelButton)
			getApp()->runModalWhileEvents(cancelButton);

		// Set labels for progress dialog
		FXString label=_("Changing permissions...");
		uplabel->setText(label);
		label=_("File: ")+FXString(path);
		downlabel->setText(label);
		getApp()->repaint();

		// If cancel button was clicked, return
		if (cancelled)
			return -1;

        return ::chmod(path,mode);
    }
    else // Directory
    {
		if(rec == FALSE && !fileonly)
        {
			// Force timeout checking for progress dialog
			checkTimeout();

			// Give cancel button an opportunity to be clicked
			if(cancelButton)
				getApp()->runModalWhileEvents(cancelButton);

			// Set labels for progress dialog
			FXString label=_("Changing permissions...");
			uplabel->setText(label);
			label=_("Folder: ")+FXString(path);
			downlabel->setText(label);
			getApp()->repaint();

			// If cancel button was clicked, return
			if (cancelled)
				return -1;

			if(::chmod(path,mode)) // Do not change recursively
				return -1;
        }
        else
            return rchmod(path,file,mode,dironly,fileonly); // Recursive change
    }
    return 0;

}


// Recursive chmod for a directory
// We don't process symbolic links (since their permissions cannot be changed)
int File::rchmod(char* path, char* file, mode_t mode, const FXbool dironly, const FXbool fileonly)
{
    struct stat linfo;

	// Initialize the file variable with the initial path
	strlcpy(file, path, strlen(path)+1);

    // If it doesn't exist
	if (lstatrep(path, &linfo))
        return -1;

	// If it's a symbolic link
	if (S_ISLNK(linfo.st_mode))
		return 0;

    if (!S_ISDIR(linfo.st_mode)) // File
    {
        if(dironly)
            return 0;

		// Force timeout checking for progress dialog
		checkTimeout();

		// Give cancel button an opportunity to be clicked
		if(cancelButton)
			getApp()->runModalWhileEvents(cancelButton);

		// Set labels for progress dialog
		FXString label=_("Changing permissions...");
		uplabel->setText(label);
		label=_("File: ")+FXString(path);
		downlabel->setText(label);
		getApp()->repaint();

		// If cancel button was clicked, return
		if (cancelled)
			return -1;

        return ::chmod(path,mode);
    }

	DIR *dir;
    struct dirent *entry;
    int i, pl = strlen(path);

    if (!(dir = opendir(path)))
        return -1;

    for(i = 0; (entry = readdir(dir)); i++)
	{	
        if (entry->d_name[0] != '.' || (entry->d_name[1] != '\0'
                                        && (entry->d_name[1] != '.' ||
                                            entry->d_name[2] != '\0')))
        {
            int pl1 = pl, l = strlen(entry->d_name);
            char* path1 = (char*)alloca(pl1+l+2);

            strlcpy(path1, path, strlen(path)+1);
            if (path1[pl1-1] != '/')
                path1[pl1++] = '/';
            strlcpy(path1+pl1, entry->d_name, strlen(entry->d_name)+1);

			// Modify the file variable with the new path
			strlcpy(file, path1, strlen(path1)+1);
            if (rchmod(path1,file,mode,dironly,fileonly))
                return -1;
        }
	}

    if (closedir(dir))
        return -1;

	if (fileonly)
		return 0;
	else
		return ::chmod(path,mode);
}


// Chown a file or directory, recursively or not
// We don't follow symbolic links
//
// Note : the variable file returns the last processed file
// It can be different from the initial path, if recursive chmod is used
// (Used to fill an error message, if needed) 
int File::chown(char* path, char* file, uid_t uid, gid_t gid, const FXbool rec, const FXbool dironly, const FXbool fileonly)
{
    struct stat linfo;

	// Initialise the file variable with the initial path
	strlcpy(file, path, strlen(path)+1);

    // If it doesn't exist
	if (lstatrep(path, &linfo))
        return -1;

    if (!S_ISDIR(linfo.st_mode)) // File
    {
        if(dironly)
            return 0;

		// Force timeout checking for progress dialog
		checkTimeout();

		// Give cancel button an opportunity to be clicked
		if(cancelButton)
			getApp()->runModalWhileEvents(cancelButton);

		// Set labels for progress dialog
		FXString label=_("Changing owner...");
		uplabel->setText(label);
		label=_("File: ")+FXString(path);
		downlabel->setText(label);
		getApp()->repaint();

		// If cancel button was clicked, return
		if (cancelled)
			return -1;

        if(::lchown(path,uid,gid))
            return -1;
    }
    else // Directory
    {	
		if(rec == FALSE && !fileonly)
        {
			// Force timeout checking for progress dialog
			checkTimeout();

			// Give cancel button an opportunity to be clicked
			if(cancelButton)
				getApp()->runModalWhileEvents(cancelButton);

			// Set labels for progress dialog
			FXString label=_("Changing owner...");
			uplabel->setText(label);
			label=_("Folder: ")+FXString(path);
			downlabel->setText(label);
			getApp()->repaint();

			// If cancel button was clicked, return
			if (cancelled)
				return -1;

            if(::lchown(path,uid,gid)) // Do not change recursively
                return -1;
        }
        else
            if(rchown(path,file,uid,gid,dironly,fileonly)) // Recursive change
                return -1;
    }
    return 0;

}


// Recursive chown for a directory
// We don't follow symbolic links
int File::rchown(char* path, char* file, uid_t uid, gid_t gid, const FXbool dironly, const FXbool fileonly)
{
    struct stat linfo;

	// Initialise the file variable with the initial path
	strlcpy(file, path, strlen(path)+1);

    // If it doesn't exist
	if (lstatrep(path, &linfo))
        return -1;

    if (!S_ISDIR(linfo.st_mode)) // file
    {
		if(dironly)
			return 0;

		// Force timeout checking for progress dialog
		checkTimeout();

		// Give cancel button an opportunity to be clicked
		if(cancelButton)
			getApp()->runModalWhileEvents(cancelButton);

		// Set labels for progress dialog
		FXString label=_("Changing owner...");
		uplabel->setText(label);
		label=_("File: ")+FXString(path);
		downlabel->setText(label);
		getApp()->repaint();

		// If cancel button was clicked, return
		if (cancelled)
			return -1;

		return ::lchown(path,uid,gid);
    }
	
    DIR *dir;
    struct dirent *entry;
    int i, pl = strlen(path);

    if (!(dir = opendir(path)))
        return -1;

    for(i = 0; (entry = readdir(dir)); i++)
        if (entry->d_name[0] != '.' || (entry->d_name[1] != '\0'
                                        && (entry->d_name[1] != '.' ||
                                            entry->d_name[2] != '\0')))
        {
            int pl1 = pl, l = strlen(entry->d_name);
            char* path1 = (char*)alloca(pl1+l+2);

            strlcpy(path1, path, strlen(path)+1);
            if (path1[pl1-1] != '/')
                path1[pl1++] = '/';
            strlcpy(path1+pl1, entry->d_name, strlen(entry->d_name)+1);
			strlcpy(file, path1, strlen(path1)+1);
            if (rchown(path1,file,uid,gid,dironly,fileonly))
                return -1;
        }

    if (closedir(dir))
        return -1;
 
	if (fileonly)
		return 0;
	else
		return ::lchown(path,uid,gid);
}


// Extract an archive in a specified directory
int File::extract(const FXString name, const FXString dir, const FXString cmd)
{
	// Change to the specified directory
    FXString currentdir=FXSystem::getCurrentDirectory();
	chdir(dir.text());

    // Make and show command window
	CommandWindow *cmdwin=new CommandWindow(getApp(),_("Extract archive"),cmd,30,80);			
	cmdwin->create();
	cmdwin->setIcon(archexticon);

	// The command window object deletes itself after closing the window!

	// Return to initial directory
	chdir(currentdir.text());
	
    return 1;
}


// Create an archive
int File::archive(const FXString name, const FXString cmd)
{
	// Target file already exists
	if(::exists(FXPath::dequote(name)))
    {
		// Overwrite dialog if necessary
		if (!overwrite_all & !skip_all)
		{
			FXString msg;
			msg.format(_("File %s already exists. Overwrite?"),name.text());
       		OverwriteBox* dlg = new OverwriteBox(this,_("Confirm Overwrite"),msg);
       		unsigned int answer=dlg->execute(PLACEMENT_CURSOR);
			delete dlg;
       		switch(answer)
       		{
				// Cancel
				case 0:
					return FALSE;
					break;
				
				// Overwrite
				case 1:
					overwrite=TRUE;
					break;
				
				// Overwrite all
				case 2:
					overwrite_all=TRUE;
					break;
				
				// Skip
				case 3:
					overwrite=FALSE;
					break;
				
				// Skip all
				case 4:
					skip_all=TRUE;
					break;
       		}
		}
		if(!(overwrite | overwrite_all) | skip_all)
			return TRUE;
	}

    // Make and show command window
	CommandWindow *cmdwin=new CommandWindow(getApp(),_("Add to archive"),cmd,30,80);			
	cmdwin->create();
	cmdwin->setIcon(archaddicon);

	// The command window object deletes itself after closing the window!

    return 1;
}


#if defined(linux)
int File::mount(const FXString dir, const FXString msg, const FXString cmd, const unsigned int op)
{
	FXbool mount_messages=getApp()->reg().readUnsignedEntry("OPTIONS","mount_messages",TRUE);

	// Show progress dialog (no timer here)
	show(PLACEMENT_OWNER);
	getApp()->forceRefresh();
	getApp()->flush();

	// Set labels for progress dialog
	uplabel->setText(msg);				
	downlabel->setText(dir.text());
	getApp()->repaint();

	// Give cancel button an opportunity to be clicked
	if(cancelButton)
		getApp()->runModalWhileEvents(cancelButton);

	// If cancel button was clicked, return
	if (cancelled)
		return -1;

	// Perform the mount/unmount command
	FILE *pcmd=popen(cmd.text(),"r");
	if(!pcmd)
	{
		MessageBox::error(this,BOX_OK,_("Error"),_("Failed command: %s"),cmd.text());
		return -1;
	}

	// Get error message if any
	char text[10000]={0};
	FXString buf;
	while(fgets(text,sizeof(text),pcmd))
		buf+=text;
	snprintf(text,sizeof(text)-1,"%s",buf.text());
	
	// Close the stream
	if(pclose(pcmd)==-1 && strcmp(text,"")!=0)
	{
		MessageBox::error(this,BOX_OK,_("Error"),"%s",text);
		return -1;
	}
	// Hide progress dialog
	hide();
	
	// Success message, eventually
	if(mount_messages)
	{
		if (op==MOUNT)
			MessageBox::information(this,BOX_OK,_("Success"),_("Folder %s was successfully mounted."),dir.text());
		else
			MessageBox::information(this,BOX_OK,_("Success"),_("Folder %s was successfully unmounted."),dir.text());
	}
	return 1;
}


// Install / Upgrade package
int File::pkgInstall(const FXString name, const FXString cmd)
{
    // Make and show command window
	CommandWindow *cmdwin=new CommandWindow(getApp(),_("Install/Upgrade package"),cmd,10,80);			
	cmdwin->create();
	
	FXString msg;
	msg.format(_("Installing package: %s \n"),name.text());
	cmdwin->appendText(msg.text());

	// The command window object deletes itself after closing the window!

    return 1;
}

// Uninstall package
int File::pkgUninstall(const FXString name, const FXString cmd)
{
    // Make and show command window
	CommandWindow *cmdwin=new CommandWindow(getApp(),_("Uninstall package"),cmd,10,80);			
	cmdwin->create();
	
	FXString msg;
	msg.format(_("Uninstalling package: %s \n"),name.text());
	cmdwin->appendText(msg.text());

	// The command window object deletes itself after closing the window!

    return 1;
}
#endif


// Handle cancel button in progress bar dialog
long File::onCmdCancel(FXObject*, FXSelector,void*)
{
	cancelled=TRUE;
	return 1;
}


// Handle timeout for progress bar
long File::onTimeout(FXObject*, FXSelector,void*)
{
	show(PLACEMENT_OWNER);
    getApp()->forceRefresh();
    getApp()->flush();
    return 1;
}

