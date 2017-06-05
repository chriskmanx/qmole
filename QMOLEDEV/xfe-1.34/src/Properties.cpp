// Properties box

#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#if defined(linux)
#include <mntent.h>
#endif

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "File.h"
#include "DialogBox.h"
#include "FileDialog.h"
#include "FilePanel.h"
#include "XFileExplorer.h"
#include "MessageBox.h"
#include "Properties.h"



// Global variables
extern FXMainWindow *mainWindow;
extern FXStringDict *fsdevices;
extern FXString xdgdatahome;


// Map
FXDEFMAP(PermFrame) PermFrameMap[]={};

// Object implementation
FXIMPLEMENT(PermFrame,FXVerticalFrame,PermFrameMap,ARRAYNUMBER(PermFrameMap))

PermFrame::PermFrame(FXComposite* parent,FXObject *target):
        FXVerticalFrame(parent,FRAME_RAISED)
{
    FXHorizontalFrame *accessframe=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXHorizontalFrame *chmodframe=new FXHorizontalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    
	// Permissions
	FXGroupBox *group1=new FXGroupBox(accessframe,_("User"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    ur=new FXCheckButton(group1,_("Read"),target,PropertiesBox::ID_RUSR);
    uw=new FXCheckButton(group1,_("Write"),target,PropertiesBox::ID_WUSR);
    ux=new FXCheckButton(group1,_("Execute"),target,PropertiesBox::ID_XUSR);
	FXGroupBox *group2=new FXGroupBox(accessframe,_("Group"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    gr=new FXCheckButton(group2,_("Read"),target,PropertiesBox::ID_RGRP);
    gw=new FXCheckButton(group2,_("Write"),target,PropertiesBox::ID_WGRP);
    gx=new FXCheckButton(group2,_("Execute"),target,PropertiesBox::ID_XGRP);
    FXGroupBox *group3=new FXGroupBox(accessframe,_("Others"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    or_=new FXCheckButton(group3,_("Read"),target,PropertiesBox::ID_ROTH);
    ow=new FXCheckButton(group3,_("Write"),target,PropertiesBox::ID_WOTH);
    ox=new FXCheckButton(group3,_("Execute"),target,PropertiesBox::ID_XOTH);
    FXGroupBox *group4=new FXGroupBox(accessframe,_("Special"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
    suid=new FXCheckButton(group4,_("Set UID"),target,PropertiesBox::ID_SUID);
    sgid=new FXCheckButton(group4,_("Set GID"),target,PropertiesBox::ID_SGID);
    svtx=new FXCheckButton(group4,_("Sticky"),target,PropertiesBox::ID_SVTX);

    // Owner
	FXGroupBox *group5=new FXGroupBox(chmodframe,_("Owner"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    new FXLabel(group5,_("User"));
    user=new FXComboBox(group5,15,NULL,0,COMBOBOX_STATIC);
  	user->setNumVisible(5);
    new FXLabel(group5,_("Group"));
    grp=new FXComboBox(group5,15,NULL,0,COMBOBOX_STATIC);
  	grp->setNumVisible(5);

	// User names (sorted in ascending order)
    struct passwd* pwde;
    while( (pwde = getpwent()) )
        user->appendItem(pwde->pw_name);
    endpwent();
	user->setSortFunc(FXList::ascending);
	user->sortItems();	

	// Group names (sorted in ascending order)
    struct group* grpe;
    while( (grpe = getgrent()) )
        grp->appendItem(grpe->gr_name);
    endgrent();
	grp->setSortFunc(FXList::ascending);
	grp->sortItems();	

    // Command
	FXGroupBox *group6=new FXGroupBox(chmodframe,_("Command"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXMatrix *matrix6 = new FXMatrix(group6,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	cmd_radiotarget.connect(cmd);
	set=new FXRadioButton(matrix6,_("Set marked"),&cmd_radiotarget,FXDataTarget::ID_OPTION+PropertiesBox::ID_SET);
    rec=new FXCheckButton(matrix6,_("Recursively"),NULL,0);
    clear=new FXRadioButton(matrix6,_("Clear marked"),&cmd_radiotarget,FXDataTarget::ID_OPTION+PropertiesBox::ID_CLEAR);
	flt_radiotarget.connect(flt);
    all=new FXRadioButton(matrix6,_("Files and folders"),&flt_radiotarget,FXDataTarget::ID_OPTION+PropertiesBox::ID_ALL);
    add=new FXRadioButton(matrix6,_("Add marked"),&cmd_radiotarget,FXDataTarget::ID_OPTION+PropertiesBox::ID_ADD);
	dironly=new FXRadioButton(matrix6,_("Folders only"),&flt_radiotarget,FXDataTarget::ID_OPTION+PropertiesBox::ID_DIRONLY);
	own=new FXCheckButton(matrix6,_("Owner only"),NULL,0);
    fileonly=new FXRadioButton(matrix6,_("Files only"),&flt_radiotarget,FXDataTarget::ID_OPTION+PropertiesBox::ID_FILEONLY);
}

// Map
FXDEFMAP(PropertiesBox) PropertiesBoxMap[]=
{
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_ACCEPT_SINGLE,PropertiesBox::onCmdAcceptSingle),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_ACCEPT_MULT,PropertiesBox::onCmdAcceptMult),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_CANCEL,PropertiesBox::onCmdCancel),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_RUSR,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_WUSR,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_XUSR,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_RGRP,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_WGRP,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_XGRP,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_ROTH,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_WOTH,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_XOTH,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_SUID,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_SGID,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_SVTX,PropertiesBox::onCmdCheck),
	FXMAPFUNC(SEL_UPDATE,0,PropertiesBox::onUpdSizeAndPerm),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_SET,PropertiesBox::onCmdCommand),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_CLEAR,PropertiesBox::onCmdCommand),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_ADD,PropertiesBox::onCmdCommand),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_DIRONLY,PropertiesBox::onCmdFilter),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_FILEONLY,PropertiesBox::onCmdFilter),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_ALL,PropertiesBox::onCmdFilter),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_BIG_ICON,PropertiesBox::onCmdBrowseIcon),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_MINI_ICON,PropertiesBox::onCmdBrowseIcon),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_BROWSE_OPEN,PropertiesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_BROWSE_VIEW,PropertiesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PropertiesBox::ID_BROWSE_EDIT,PropertiesBox::onCmdBrowse),
	FXMAPFUNC(SEL_KEYPRESS,0,PropertiesBox::onCmdKeyPress),
#ifdef STARTUP_NOTIFICATION
	FXMAPFUNC(SEL_UPDATE,PropertiesBox::ID_SNDISABLE,PropertiesBox::onUpdSnDisable),
#endif
};

// Object implementation
FXIMPLEMENT(PropertiesBox,DialogBox,PropertiesBoxMap,ARRAYNUMBER(PropertiesBoxMap))

// Construct window for one file
PropertiesBox::PropertiesBox(FXWindow *win,FXString file,FXString path): DialogBox(win,_("Properties"), DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE)
{
	FXulong filesize;
    FXString mod,changed,accessed;
	FXString grpid, usrid;	
	FXLabel *sizelabel=NULL;
    struct stat linfo;
    FXString type="", extension, fileassoc;
    FXbool isLink, isBrokenLink;
    FXString filepath, referredpath;
    char mnttype[64], used[64], avail[64], pctr[64], size[64];
    char buf[MAXPATHLEN+1];
	FXString hsize;
	FILE *p;

	// Trash locations
	trashfileslocation=xdgdatahome + PATHSEPSTRING TRASHFILESPATH;
	trashinfolocation=xdgdatahome + PATHSEPSTRING TRASHINFOPATH;

    // Buttons
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,5,5);

    // Contents
    FXVerticalFrame *contents=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Accept
	if (file!="..")
    {
		FXButton *ok = new FXButton(buttons,_("&Accept"),NULL,this,PropertiesBox::ID_ACCEPT_SINGLE,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    	ok->addHotKey(KEY_Return);
	}

    // Cancel
    new FXButton(buttons,_("&Cancel"),NULL,this,PropertiesBox::ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Switcher
    FXTabBook *tabbook = new FXTabBook(contents,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);

    // First item is General
    new FXTabItem(tabbook,_("&General"),NULL);
    FXPacker *genpack = new FXPacker(tabbook,FRAME_RAISED);
    FXGroupBox *generalgroup=new FXGroupBox(genpack,FXString::null,FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXVerticalFrame *generalframe=new FXVerticalFrame(generalgroup,LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Second item is Access Permissions
	FXTabItem *permtab = new FXTabItem(tabbook,_("&Permissions"),NULL);
    perm = new PermFrame(tabbook,this);
	
	// Permission tab is disabled for parent directory
	if (file=="..")
		permtab->disable();

    // Third tab - file associations
    FXTabItem *fassoctab = new FXTabItem(tabbook,_("&File Associations"),NULL);
    FXPacker *fassocpack = new FXPacker(tabbook,FRAME_RAISED);
    FXGroupBox *fassocgroup=new FXGroupBox(fassocpack,FXString::null,FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXVerticalFrame *contassoc=new FXVerticalFrame(fassocgroup,LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXMatrix *matrix = new FXMatrix(contassoc,3,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    fassoctab->disable();
    new FXLabel(matrix,_("Extension:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    ext = new FXLabel(matrix,"",NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    new FXLabel(matrix,"",NULL,0);

    new FXLabel(matrix,_("Description:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    descr = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXLabel(matrix,"",NULL,0);

    new FXLabel(matrix,_("Open:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    open = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_OPEN,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    int is_ar = FALSE;

    FXString viewlbl=_("View:");
    FXString editlbl=_("Edit:");

	extension=file.rafter('.',1);
	extension=extension.lower();
	if (extension=="gz" || extension=="tgz" || extension=="tar" || extension=="taz" || extension=="bz2"
	    || extension=="tbz2" || extension=="tbz" || extension =="xz" || extension=="txz" || extension=="zip"
	    || extension=="7z" || extension=="Z" || extension=="lzh" || extension=="rar"
	    || extension=="ace" || extension=="arj")
	{
		is_ar = TRUE; // archive
		viewlbl = _("Extract:");
	}
#if defined(linux)
	else if(extension=="rpm")
		editlbl = _("Install/Upgrade:");
#endif	
    new FXLabel(matrix,viewlbl,NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    view = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_VIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    if(!is_ar)
    {
        new FXLabel(matrix,editlbl,NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
        edit = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
        new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_EDIT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    }
    else
        edit = NULL;

    new FXLabel(matrix,_("Big Icon:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    bigic = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BIG_ICON,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    new FXLabel(matrix,_("Mini Icon:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    miniic = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_MINI_ICON,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Label
    new FXLabel(generalframe,_("Name"),NULL,JUSTIFY_LEFT);
    input = new FXTextField(generalframe,20,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);

    // Complete file path name
	filepath=path+PATHSEPSTRING+file;
	parentdir=path;
	filename=file;
	
	// Initialize mount point flag
	isMountpoint=FALSE;

	// Warn if non UTF-8 file name
	if (!isUtf8(filepath.text(),filepath.length()))
		new FXLabel(generalframe,_("=> Warning: file name is not UTF-8 encoded!"),NULL,LAYOUT_LEFT|LAYOUT_CENTER_Y|LAYOUT_FILL_ROW);

	// Get file/link stat info
    if(lstatrep(filepath.text(),&linfo)!=0)
        return ;

	// Obtain user name
	usrid=FXSystem::userName(linfo.st_uid);

	// Obtain group name
	grpid=FXSystem::groupName(linfo.st_gid);
    perm->user->setText(usrid);
    perm->grp->setText(grpid);
    oldgrp = grpid;
    oldusr = usrid;

	// Test if link or broken link
	// When valid link, get the referred file path
    isLink=S_ISLNK(linfo.st_mode);
	isBrokenLink=FALSE;
	if(isLink)
	{
		// Broken link
		struct stat info;
		if (statrep(filepath.text(),&info)!=0)
			isBrokenLink=TRUE;

		// Get the path name of the linked file
		referredpath=::readLink(filepath);
	}

    orig_mode=linfo.st_mode;
    
    // Initialize exec flag
    executable=FALSE;

	// Read time format
	FXString timeformat=getApp()->reg().readStringEntry("SETTINGS","time_format",DEFAULT_TIME_FORMAT);

    // Mod time of the file / link
	mod=FXSystem::time(timeformat.text(),linfo.st_mtime);

    // Change time of the file / link
	changed=FXSystem::time(timeformat.text(),linfo.st_ctime);

    // Accessed time of the file / link
    accessed=FXSystem::time(timeformat.text(),linfo.st_atime);;

    // Size of the file / link
    filesize=(FXulong)linfo.st_size;

    // Is it a directory?
	isDirectory=S_ISDIR(linfo.st_mode);
    if(isDirectory)
    {		
		// Directory path
		FXString dirpath=FXPath::absolute(parentdir,file);

#if defined(linux)
		FILE *mtab=setmntent(MTAB_PATH,"r");
        struct mntent *mnt;
        if(mtab)
        {
            while((mnt=getmntent(mtab)))
            {
                if(!streq(mnt->mnt_type,MNTTYPE_IGNORE) && !streq(mnt->mnt_type,MNTTYPE_SWAP))
                {
                    if(streq(mnt->mnt_dir,dirpath.text()))
                    {
                        isMountpoint=TRUE;
                        snprintf(buf,sizeof(buf)-1,_("Filesystem (%s)"),mnt->mnt_fsname);
                        type=buf;
						strlcpy(mnttype,mnt->mnt_type,strlen(mnt->mnt_type)+1);
                    }
                }
            }
            endmntent(mtab);
        }
#endif
		// If it is a mount point
		if(isMountpoint)
		{
			// Caution : use the -P option to be POSIX compatible!
			snprintf(buf,sizeof(buf)-1,"df -P -B 1 '%s'",filepath.text());
			p=popen(buf,"r");
			fgets(buf,sizeof(buf),p);
			fgets(buf,sizeof(buf),p);
			strtok(buf," "); // should check if str!=NULL
			strtok(NULL," ");// get size
			char *pstr;
			pstr=strtok(NULL," ");
			strlcpy(used,pstr,strlen(pstr)+1);// get used
			pstr=strtok(NULL," ");
			strlcpy(avail,pstr,strlen(pstr)+1);// get available
			pstr=strtok(NULL," ");
			strlcpy(pctr,pstr,strlen(pstr)+1);// get percentage
			pclose(p);
		}
			
		// If it is a folder
		else
			type=_("Folder");
    }
	
    else if(S_ISCHR(linfo.st_mode))
        type=_("Character Device");
    else if(S_ISBLK(linfo.st_mode))
        type=_("Block Device");
    else if(S_ISFIFO(linfo.st_mode))
        type=_("Named Pipe");
    else if(S_ISSOCK(linfo.st_mode))
        type=_("Socket");
    
	// Regular file or link
	else
    {
		// Try to use association table	
		extension=FXPath::name(filepath).rafter('.',1);
		extension=extension.lower();
		if (extension!="")
			fileassoc=getApp()->reg().readStringEntry("FILETYPES",extension.text(),"");

		// If we have an association
        if(!fileassoc.empty())
        {
           	FXString c;
            type=fileassoc.section(';',1);    
			if(type=="")
			{
				if (linfo.st_mode&(S_IXUSR|S_IXGRP|S_IXOTH))
				{
                	type=_("Executable");
                	executable=TRUE;
				}
                else
                	type=_("Document");
			}
            ext->setText(extension);
            c=fileassoc.section(';',0);
            descr->setText(fileassoc.section(';',1));
            open->setText(c.section(',',0));
            view->setText(c.section(',',1));
            if(edit)
                edit->setText(c.section(',',2));
            bigic->setText(fileassoc.section(';',2));
            miniic->setText(fileassoc.section(';',3));
            if (!isLink)
				fassoctab->enable();
        }
        else
        {
            ext->setText(extension);
			if(linfo.st_mode&(S_IXUSR|S_IXGRP|S_IXOTH))
			{
                type=_("Executable");
                executable=TRUE;
			}
            else
            {
                type=_("Document");
            }
			if (!isLink)
				fassoctab->enable();
        }
    }
	
	// Modify file type for broken links
	if (isBrokenLink)
		type=_("Broken link");
	
	// For links, get the file type of the referred file
	else if (isLink)
	{
		struct stat info;
		if (statrep(referredpath.text(),&info)==0)
		{
			// Folder 
			if (S_ISDIR(info.st_mode))
				type=_("Folder");
				
			// File
			else
			{
				// Try to use association table	
				extension=FXPath::name(referredpath).rafter('.',1);
				extension=extension.lower();
				if (extension!="")
					fileassoc=getApp()->reg().readStringEntry("FILETYPES",extension.text(),"");

				// If we have an association
				if(!fileassoc.empty())
					type=fileassoc.section(';',1);
				
				// No association
				else
				{
					if(S_ISCHR(info.st_mode))
						type=_("Character Device");
					else if(S_ISBLK(info.st_mode))
						type=_("Block Device");
					else if(S_ISFIFO(info.st_mode))
						type=_("Named Pipe");
					else if(S_ISSOCK(info.st_mode))
						type=_("Socket");
					else if(info.st_mode&(S_IXUSR|S_IXGRP|S_IXOTH))
					{
						type=_("Executable");
						executable=TRUE;
					}
					else
						type=_("Document");
				}
			}
		}
		type=_("Link to ")+type;
	}

	// Parent directory name not editable
	if (file=="..")
		input->setEditable(FALSE);

	// Root directory name not editable
    if (file=="" && path==ROOTDIR)
	{
		input->setText(ROOTDIR);
		input->setEditable(FALSE);
	}
	else		
		input->setText(file);
    
	input->setFocus();

    // Set permissions
    perm->ur->setCheck((linfo.st_mode & S_IRUSR) ? TRUE : FALSE);
    perm->uw->setCheck((linfo.st_mode & S_IWUSR) ? TRUE : FALSE);
    perm->ux->setCheck((linfo.st_mode & S_IXUSR) ? TRUE : FALSE);

    perm->gr->setCheck((linfo.st_mode & S_IRGRP) ? TRUE : FALSE);
    perm->gw->setCheck((linfo.st_mode & S_IWGRP) ? TRUE : FALSE);
    perm->gx->setCheck((linfo.st_mode & S_IXGRP) ? TRUE : FALSE);

    perm->or_->setCheck((linfo.st_mode & S_IROTH) ? TRUE : FALSE);
    perm->ow->setCheck((linfo.st_mode & S_IWOTH) ? TRUE : FALSE);
    perm->ox->setCheck((linfo.st_mode & S_IXOTH) ? TRUE : FALSE);

    perm->suid->setCheck((linfo.st_mode & S_ISUID) ? TRUE : FALSE);
    perm->sgid->setCheck((linfo.st_mode & S_ISGID) ? TRUE : FALSE);
    perm->svtx->setCheck((linfo.st_mode & S_ISVTX) ? TRUE : FALSE);

    perm->set->setCheck();
	perm->all->setCheck();

	FXLabel *mtType=NULL, *mtUsed=NULL, *mtFree=NULL, *fileType=NULL, *fileChanged=NULL, *fileAccessed=NULL, *fileModified=NULL;
	FXbool isInTrash=FALSE;
	
	fileSize=NULL;
	
	// Properties are different for mount points
	if (isMountpoint)
	{
  		FXGroupBox *mtgroup=new FXGroupBox(generalframe,_("Mount point"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  		FXMatrix *mtmatrix=new FXMatrix(mtgroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  		new FXLabel(mtmatrix,_("Mount type:"),NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
  		fileType=new FXLabel(mtmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
  		new FXLabel(mtmatrix,_("Used:"),NULL,LAYOUT_LEFT);
  		mtUsed=new FXLabel(mtmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
  		new FXLabel(mtmatrix,_("Free:"),NULL,LAYOUT_LEFT);
  		mtFree=new FXLabel(mtmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
  		new FXLabel(mtmatrix,_("File system:"),NULL,LAYOUT_LEFT);
  		mtType=new FXLabel(mtmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
  		new FXLabel(mtmatrix,_("Location:"),NULL,LAYOUT_LEFT);
  		location=new FXLabel(mtmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
	}
	else
	{
  		FXGroupBox *attrgroup=new FXGroupBox(generalframe,_("Properties"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  		FXMatrix *attrmatrix=new FXMatrix(attrgroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  		new FXLabel(attrmatrix,_("Type:"),NULL,LAYOUT_LEFT);
  		fileType=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
  		sizelabel=new FXLabel(attrmatrix,_("Total size:"),NULL,LAYOUT_LEFT);
  		fileSize=new FXLabel(attrmatrix,"\n",NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
		new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
		filesSizeDetails=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
  		new FXLabel(attrmatrix,_("Location:"),NULL,LAYOUT_LEFT);
  		location=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN|JUSTIFY_LEFT);
		if (isLink & !isBrokenLink)
		{
			new FXLabel(attrmatrix,_("Link to:"),NULL,LAYOUT_LEFT);
			linkto=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
		}
		else if(isBrokenLink)
		{
			new FXLabel(attrmatrix,_("Broken link to:"),NULL,LAYOUT_LEFT);
			linkto=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
		}
 		
		// If the file is in the trash can
		if (parentdir.left(trashfileslocation.length())==trashfileslocation)
		{
			isInTrash=TRUE;			

   			new FXLabel(attrmatrix,_("Original location:"),NULL,LAYOUT_LEFT);
 			origlocation=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
		}
		
		FXGroupBox *timegroup=new FXGroupBox(generalframe,_("File Time"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
  		FXMatrix *timematrix=new FXMatrix(timegroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
  		new FXLabel(timematrix,_("Last Modified:"),NULL,LAYOUT_LEFT);
  		fileModified=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  		new FXLabel(timematrix,_("Last Changed:"),NULL,LAYOUT_LEFT);
  		fileChanged=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
  		new FXLabel(timematrix,_("Last Accessed:"),NULL,LAYOUT_LEFT);
  		fileAccessed=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);

#ifdef STARTUP_NOTIFICATION

		sngroup=new FXGroupBox(generalframe,_("Startup Notification"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
		snbutton=new FXCheckButton(sngroup,_("Disable startup notification for this executable"),this,ID_SNDISABLE);

		sndisable_prev=FALSE;
		FXString snexcepts=getApp()->reg().readStringEntry("OPTIONS","startup_notification_exceptions","");
		if(snexcepts != "")
		{
			FXString entry;
			for(int i=0; ; i++)
			{
				entry=snexcepts.section(':',i);
				if(streq(entry.text(),""))
					break;
				if(streq(entry.text(),filename.text()))
				{
					sndisable_prev=TRUE;
					break;
				}
			}
		}
		snbutton->setCheck(sndisable_prev);

#endif

		// If the file is in the trash can
		if (isInTrash)
		{
			new FXLabel(timematrix,_("Deletion Date:"),NULL,LAYOUT_LEFT);
			deletiondate=new FXLabel(timematrix,FXString::null,NULL,LAYOUT_LEFT|LAYOUT_FILL_COLUMN);
		}
	}

	// File or mount type
	fileType->setText(type.text());

	// Parent directory
	location->setText(parentdir);
	
	// Referred file for valid or broken link
	if (isLink)
		linkto->setText(referredpath);
		
	// If directory
	if (isDirectory)
	{
		// if mount point
		if(isMountpoint)
		{
			hsize=::hSize(used);
        	snprintf(size,sizeof(size)-1,"%s (%s)",hsize.text(),pctr);        
			mtUsed->setText(size);
			hsize=::hSize(avail);
			mtFree->setText(hsize);
			mtType->setText(mnttype);
		}
		// if folder
		else
		{
			fileModified->setText(mod);
        	fileChanged->setText(changed);
        	fileAccessed->setText(accessed);
		}
	}
	// Regular file
    else
    {
        snprintf(size,sizeof(size)-1,"%llu",filesize);
        hsize=::hSize(size);
        snprintf(size,sizeof(size)-1,_("%s (%llu bytes)"),hsize.text(),filesize);        
		sizelabel->setText(_("Size:"));
		fileSize->setText(size);
        fileModified->setText(mod);
        fileChanged->setText(changed);
        fileAccessed->setText(accessed);
    }

	// If the file is in the trash can
	if (isInTrash)
	{
		// Obtain trash base name and sub path
		FXString subpath=parentdir+PATHSEPSTRING;
		subpath.erase(0,trashfileslocation.length()+1);
		FXString trashbasename=subpath.before('/');
		if (trashbasename=="")
			trashbasename=FXPath::name(filepath);
		subpath.erase(0,trashbasename.length());
		
		// Read the .trashinfo file
		FILE *fp;
		char line[1024];
		FXString origpath="", delstr="";
		long deldate=0;
		FXbool success=TRUE;
		FXString trashinfopathname=trashinfolocation+PATHSEPSTRING+trashbasename+".trashinfo";
		if ((fp=fopen(trashinfopathname.text(),"r"))!=NULL)
		{
			// Read the first three lines and get the strings
			if (fgets(line,sizeof(line),fp)==NULL)
				success=FALSE;
			if (fgets(line,sizeof(line),fp)==NULL)
				success=FALSE;
			if (success)
			{
				origpath=line;
				origpath=origpath.after('=');
				origpath=origpath.before('\n');
			}						
			if (fgets(line,sizeof(line),fp)==NULL)
				success=FALSE;
			if (success)
			{
				delstr=line;
				delstr=delstr.after('=');
				delstr=delstr.before('\n');
			}
			fclose(fp);
		}
		
		// Eventually include sub path in the original path
		if (subpath=="")
			origpath=origpath+subpath;
		else
			origpath=origpath+subpath+FXPath::name(filepath);
		
		// Convert date
		deldate=deltime(delstr);
		if (deldate!=0)
			delstr=FXSystem::time(timeformat.text(),deldate);

		// Maybe there is no deletion information
		if (delstr!="")
		{
			origlocation->setText(origpath);
			deletiondate->setText(delstr);
		}
	}

    mode = orig_mode;
    perm->cmd=PropertiesBox::ID_SET;
    perm->flt=PropertiesBox::ID_ALL;
    files = &file;
    source = file;
    num = 1;	

	descr_prev=descr->getText();
	open_prev=open->getText();
	view_prev=view->getText();
	if (edit)
		edit_prev=edit->getText();
	bigic_prev=bigic->getText();
	miniic_prev=miniic->getText();

	// Flag used to avoid computing recursive size more than once
	recsize=TRUE;
}


// Construct window for multiple files
PropertiesBox::PropertiesBox(FXWindow *win,FXString *file,int n,FXString path): DialogBox(win,_("Properties"), DECOR_TITLE|DECOR_BORDER|DECOR_STRETCHABLE)
{
	struct stat linfo;
	FXString grpid, usrid;
    FXString type, extension, fileassoc;
    char buf[MAXPATHLEN+1];
	int i, isDirectory, nbseldirs=0, nbselfiles=0, dotdot=0;
	FXbool firstfile=TRUE;

    // Buttons
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,5,5);

    // Contents
    FXVerticalFrame *contents=new FXVerticalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Accept
    FXButton *ok = new FXButton(buttons,_("&Accept"),NULL,this,PropertiesBox::ID_ACCEPT_MULT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    ok->addHotKey(KEY_Return);

    // Cancel
    new FXButton(buttons,_("&Cancel"),NULL,this,DialogBox::ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

    // Switcher
    FXTabBook *tabbook = new FXTabBook(contents,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);

    // First item is General
    new FXTabItem(tabbook,_("&General"),NULL);
    FXVerticalFrame *generalframe=new FXVerticalFrame(tabbook,FRAME_RAISED);
	FXGroupBox *attrgroup=new FXGroupBox(generalframe,_("Properties"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	FXMatrix *attrmatrix=new FXMatrix(attrgroup,2,MATRIX_BY_COLUMNS|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(attrmatrix,_("Selection:"),NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	FXLabel *filesSelected=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	FXLabel *filesSelectedDetails=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	new FXLabel(attrmatrix,_("Type:"),NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	FXLabel *filesType=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	new FXLabel(attrmatrix,_("Total size:"),NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	filesSize=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	filesSizeDetails=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	new FXLabel(attrmatrix,_("Location:"),NULL,LAYOUT_LEFT|JUSTIFY_LEFT);
	location=new FXLabel(attrmatrix,FXString::null,NULL,LAYOUT_LEFT|JUSTIFY_LEFT);

    // Second item is Access Permissions
    new FXTabItem(tabbook,_("&Permissions"),NULL);
    perm = new PermFrame(tabbook,this);

    // Get file/link info of the first file of the list
	// This is used as a guess for the username, group and permissions of the whole list
	FXString pathname=path+PATHSEPSTRING+file[0];
    if(lstatrep(pathname.text(),&linfo)!=0)
        return ;
	
	// Obtain user name
	usrid=FXSystem::userName(linfo.st_uid);

	// Obtain group name
	grpid=FXSystem::groupName(linfo.st_gid);

    orig_mode=linfo.st_mode;

    perm->ur->setCheck((linfo.st_mode & S_IRUSR) ? TRUE : FALSE);
    perm->uw->setCheck((linfo.st_mode & S_IWUSR) ? TRUE : FALSE);
    perm->ux->setCheck((linfo.st_mode & S_IXUSR) ? TRUE : FALSE);

    perm->gr->setCheck((linfo.st_mode & S_IRGRP) ? TRUE : FALSE);
    perm->gw->setCheck((linfo.st_mode & S_IWGRP) ? TRUE : FALSE);
    perm->gx->setCheck((linfo.st_mode & S_IXGRP) ? TRUE : FALSE);

    perm->or_->setCheck((linfo.st_mode & S_IROTH) ? TRUE : FALSE);
    perm->ow->setCheck((linfo.st_mode & S_IWOTH) ? TRUE : FALSE);
    perm->ox->setCheck((linfo.st_mode & S_IXOTH) ? TRUE : FALSE);

    perm->suid->setCheck((linfo.st_mode & S_ISUID) ? TRUE : FALSE);
    perm->sgid->setCheck((linfo.st_mode & S_ISGID) ? TRUE : FALSE);
    perm->svtx->setCheck((linfo.st_mode & S_ISVTX) ? TRUE : FALSE);

    perm->add->setCheck();
	perm->all->setCheck();

    perm->user->setText(usrid);
    perm->grp->setText(grpid);

    mode = orig_mode;
    perm->cmd = PropertiesBox::ID_SET;
	perm->flt = PropertiesBox::ID_ALL;
    files = file;
    source = "";
    num = n;

	// Parent directory
	parentdir=path;
	location->setText(parentdir);

	// Total size and files type
	for(i=0;i<num;i++)
	{
		FXString pathname=parentdir+PATHSEPSTRING+files[i];
		if(lstatrep(pathname.text(),&linfo)!=0)
			continue ;

		// Special case of the ".." directory
		if (files[i]=="..")
		{	
			dotdot=1;
			continue;
		}

		// Is it a directory?
		isDirectory=S_ISDIR(linfo.st_mode);
		if(isDirectory)
			nbseldirs++;

		else // Regular file
		{
			nbselfiles++;

			// Try to use association table	
			extension=files[i].rafter('.',1);
			extension=extension.lower();
			if (extension!="")
				fileassoc=getApp()->reg().readStringEntry("FILETYPES",extension.text(),"");

			// Keep the first encountered type
			if (firstfile)
			{
				// If we have an association
        		if(!fileassoc.empty())
 					type=fileassoc.section(';',1);
				else
					type=_("Document");
				firstfile=FALSE;
			}
			else
			{
				// If we have an association
        		if (!fileassoc.empty())
				{
					if (fileassoc.section(';',1)!=type)
						type=_("Multiple types");
				}
				else
				{
					if (type!=_("Document"))
						type=_("Multiple types");
				}
			}
		}
	}

	// Special cases of the file type
	if (nbselfiles==0)
		type=_("Folder");
	
	if (nbseldirs>=1 && nbselfiles>=1)
		type=_("Multiple types");
		
	// Number of selected files
	snprintf(buf,sizeof(buf)-1,_("%d items"),num);
	filesSelected->setText(buf);
	snprintf(buf,sizeof(buf)-1,_("%d files, %d folders"),nbselfiles,nbseldirs+dotdot);
	filesSelectedDetails->setText(buf);

	// Display type of selected files
	filesType->setText(type);

	// Flag used to avoid computing recursive size more than once
	recsize=TRUE;
}


// Make window
void PropertiesBox::create()
{
    DialogBox::create();
}


// Dialog for single selected file
long PropertiesBox::onCmdAcceptSingle(FXObject* o,FXSelector s,void* p)
{
	char **str=NULL;
    int rc=0;
	File *f=NULL;
	char file[MAXPATHLEN];
    FXString oldfileassoc,fileassoc,op,v,e;

	// Don't proceed any task on the parent directory
	if (streq(source.text(),".."))
		return 1;

#ifdef STARTUP_NOTIFICATION
	
	// If file is an executable file
	if (executable)
	{
		FXbool sndisable=snbutton->getCheck();		
		if (sndisable != sndisable_prev)
		{
			// List of startup notification exceptions
			FXString snexcepts=getApp()->reg().readStringEntry("OPTIONS","startup_notification_exceptions","");

			// Add to list if not already present
			if (sndisable)
			{
				FXbool notinlist=TRUE;
				if(snexcepts != "")
				{
					FXString entry;
					for(int i=0; ; i++)
					{
						entry=snexcepts.section(':',i);
						if(streq(entry.text(),""))
							break;
						if(streq(entry.text(),filename.text()))
						{
							notinlist=FALSE;
							break;
						}
					}
				}
				
				if (notinlist)
					snexcepts += filename + ":";
			}
			
			// Remove from list if already present
			else
			{
				FXbool inlist=FALSE;
				int pos=0;
				if(snexcepts != "")
				{
					FXString entry;
					for(int i=0; ; i++)
					{
						entry=snexcepts.section(':',i);
						if(streq(entry.text(),""))
							break;
						if(streq(entry.text(),filename.text()))
						{
							inlist=TRUE;
							break;
						}
						pos += entry.length()+1;
					}
				}
				
				if (inlist)
					snexcepts.erase(pos,filename.length()+1);
			}
			
			// Write updated list to the registry
			getApp()->reg().writeStringEntry("OPTIONS","startup_notification_exceptions",snexcepts.text());
			getApp()->reg().write();
		}
	}		

#endif

	// Handle association if related fields have changed
	FXbool cond;
	if (edit) // Condition is not the same if edit exist or not
	{
		cond= (open->getText()!=open_prev || view->getText()!=view_prev || edit->getText()!=edit_prev
	    || descr->getText()!=descr_prev || bigic->getText()!=bigic_prev || miniic->getText()!=miniic_prev);
	}
	else
	{
		cond= (open->getText()!=open_prev || view->getText()!=view_prev || descr->getText()!=descr_prev
		|| bigic->getText()!=bigic_prev || miniic->getText()!=miniic_prev);
	}
	
	if (cond)
	{
		op = open->getText();
		v = view->getText();
		if(!v.empty())
			v = "," + v;
		if(edit)
		{
			e = edit->getText();
			if(!e.empty())
				e = "," + e;
		}

		fileassoc = ext->getText();
		fileassoc += "=";
		fileassoc += op + v + e + ";";
		fileassoc += descr->getText() + ";";
		fileassoc += bigic->getText() + ";" + miniic->getText() + ";;";

		if (ext->getText()!="")
		{
			oldfileassoc=getApp()->reg().readStringEntry("FILETYPES",ext->getText().text(),"");
			if(oldfileassoc == "" || fileassoc.section('=',1) != oldfileassoc)
			{
				FXString command=fileassoc.section('=',1);
				getApp()->reg().writeStringEntry("FILETYPES",ext->getText().text(),command.text());

				// Handle file association
				str=new char*[2];
				str[0]=new char[strlen(ext->getText().text())+1];
				str[1]=new char[strlen(command.text())+1];
				strlcpy(str[0],ext->getText().text(),ext->getText().length()+1);
				strlcpy(str[1],command.text(),command.length()+1);
				mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_FILE_ASSOC),str);
			}
		}
	}
    
    // Source and target path names
	FXString targetpath;
	FXString sourcepath=parentdir+"/"+source;  
    FXString target=input->getText();
	FXString targetparentdir=FXPath::directory(target);
	if (targetparentdir=="")
	{
		targetparentdir=parentdir;
		targetpath=targetparentdir+"/"+target;
	}
	else
		targetpath=target;
	
	// Nothing entered
	if (target=="")
	{
		MessageBox::warning(this,BOX_OK,_("Warning"),_("File name is empty, operation cancelled"));
		input->setText(source);
		return 0;
	}

    // Rename file if necessary
	if(source != target)
	{
		// Source path is not writable	
		if (!::isWritable(sourcepath))
        	MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't write to %s: Permission denied"),source.text());
		
		// Target parent directory doesn't exist or is not writable
		if (!::exists(targetparentdir))
        {
			MessageBox::error(this,BOX_OK,_("Error"),_("Folder %s doesn't exist"),targetparentdir.text());
        	return 0;
		}
		if (!::isWritable(targetparentdir))
		{
         	MessageBox::error(this,BOX_OK_SU,_("Error"),_("Can't write to %s: Permission denied"),targetparentdir.text());
        	return 0;
		}

		// Rename file or directory
		else
		{
			File *f;
			f=new File(this,_("File rename"),RENAME);
       		f->create();
			f->rename(sourcepath,targetpath);
			delete f;
		}
	}

    // Change perm
    DialogBox::onCmdAccept(o,s,p);
	if (target==".." || target==".")
	{
		input->setText(source);
		return 1;
	}

	// Caution : chown must be done *before* chmod because chown can clear suid and sgid bits
    
	// Chown only if user or group have changed or recursive flag is set
    rc=0;
    if (oldgrp != perm->grp->getText() || oldusr != perm->user->getText() || perm->rec->getCheck())
    {
        f = new File(this,_("File owner"),CHOWN);
        f->create();

        uid_t uid = 32768;
        gid_t gid = 32768;
        struct passwd* pwde;
        while( (pwde = getpwent()) )
            if(perm->user->getText() == pwde->pw_name)
                uid = pwde->pw_uid;
        endpwent();

        struct group* grpe;
        while( (grpe = getgrent()) )
            if(perm->grp->getText() == grpe->gr_name)
                gid = grpe->gr_gid;
        endgrent();

		// Wait cursor
		getApp()->beginWaitCursor();

        // Perform chown on the selected file or directory
		errno=0;
		rc = f->chown((char*)targetpath.text(),file,uid,gid,perm->rec->getCheck(),perm->dironly->getCheck(),perm->fileonly->getCheck());
		int errcode=errno;

		// If action is cancelled in progress dialog
		if (f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Change owner cancelled!"));
			delete f;
			getApp()->endWaitCursor();
			return 1;
		}
		getApp()->endWaitCursor();
		
		// Handle chown errors
        if(rc)
		{
			f->hideProgressDialog();
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Chown in %s failed: %s"), file, strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Chown in %s failed"), file);
			delete f;
			return 0;
		}
		delete f;
    }
	
    // Chmod if owner only is not set and permissions are changed or recursive flag is set
	if (!perm->own->getCheck() && (mode!=orig_mode || perm->rec->getCheck()))
    {
		if (perm->suid->getCheck() || perm->sgid->getCheck() || perm->svtx->getCheck())
		{
			if (BOX_CLICKED_CANCEL==MessageBox::warning(this,BOX_OK_CANCEL,_("Warning"),_("Setting special permissions could be unsafe! Is that you really want to do?")))
				return 0;
		}

        struct stat linfo;
        mode_t m;
		
        if(lstatrep(targetpath.text(),&linfo)!=0)
            return 1;

        f = new File(this,_("File permissions"),CHMOD);
        f->create();

        switch(perm->cmd)
        {
        	case PropertiesBox::ID_ADD:
            	m=linfo.st_mode|mode;
            	break;
        	case PropertiesBox::ID_CLEAR:
            	m=linfo.st_mode & ~mode;
            	break;
        	case PropertiesBox::ID_SET:
            	m=mode;
            	break;
        	default :
				delete f;
            	return 1;
        }

		// Wait cursor
		getApp()->beginWaitCursor();
		
		// Perform chmod on the selected file or directory
		errno=0;
		rc=f->chmod((char*)targetpath.text(),file,m,perm->rec->getCheck(),perm->dironly->getCheck(),perm->fileonly->getCheck());
		int errcode=errno;

		// If action is cancelled in progress dialog
		if (f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Change file permissions cancelled!"));
			delete f;
			getApp()->endWaitCursor();
			return 1;
		}
		getApp()->endWaitCursor();
        
		// Handle chmod errors
		if (rc)
		{
			f->hideProgressDialog();
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Chmod in %s failed: %s"), file, strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Chmod in %s failed"), file);
			delete f;
			return 0;
		}
		delete f;
    }

	// Redraw the file lists
	((XFileExplorer*) mainWindow)->redrawPanels();

    return 1;
}


// Dialog for multiple selected files
long PropertiesBox::onCmdAcceptMult(FXObject* o,FXSelector s,void* p)
{
    int rc=0, i;
	File *f=NULL;
	char file[MAXPATHLEN];

    DialogBox::onCmdAccept(o,s,p);

	// Caution : chown must be done *before* chmod because chown can clear suid and sgid bits

    // Chown
    rc = 0;
	f = new File(this,_("File owner"),CHOWN);
	f->create();

	// Wait cursor
	getApp()->beginWaitCursor();

	for (i=0; i<num; i++)
	{
		struct stat linfo;
		FXString pathname=parentdir+PATHSEPSTRING+files[i];
		if(lstatrep(pathname.text(),&linfo)!=0)
			continue ;

		uid_t uid = 32768;
		gid_t gid = 32768;
		struct passwd* pwde;
		while( (pwde = getpwent()) )
			if(perm->user->getText() == pwde->pw_name)
				uid = pwde->pw_uid;
		endpwent();

		struct group* grpe;
		while( (grpe = getgrent()) )
			if(perm->grp->getText() == grpe->gr_name)
				gid = grpe->gr_gid;
		endgrent();

		errno=0;
		if (files[i]!="..")
			rc = f->chown((char*)pathname.text(),file,uid,gid,perm->rec->getCheck(),perm->dironly->getCheck(),perm->fileonly->getCheck());
		int errcode=errno;
		
		// If action is cancelled in progress dialog
		if (f->isCancelled())
		{
			f->hideProgressDialog();
			MessageBox::error(this,BOX_OK,_("Error"),_("Change owner cancelled!"));
			delete f;
			getApp()->endWaitCursor();
			return 0;
		}
		
		// Handle chown errors
		if(rc)
		{
			f->hideProgressDialog();
			if (errcode)
				MessageBox::error(this,BOX_OK,_("Error"),_("Chown in %s failed: %s"), file, strerror(errcode));
			else
				MessageBox::error(this,BOX_OK,_("Error"),_("Chown in %s failed"), file);
			delete f;
			getApp()->endWaitCursor();
			return 0;
		}
	}
	delete f;
	getApp()->endWaitCursor();

	// Chmod if owner only is not set
	if (!perm->own->getCheck())
    {
		if (perm->suid->getCheck() || perm->sgid->getCheck() || perm->svtx->getCheck())
		{
			if (BOX_CLICKED_CANCEL==MessageBox::warning(this,BOX_OK_CANCEL,_("Warning"),_("Setting special permissions could be unsafe! Is that you really want to do?")))
				return 0;
		}

		f = new File(this,_("File permissions"),CHMOD);
		f->create();

		// Wait cursor
		getApp()->beginWaitCursor();

        for(i=0; i<num; i++)
        {
            struct stat linfo;
            mode_t m;
			FXString pathname=parentdir+PATHSEPSTRING+files[i];
            if(lstatrep(pathname.text(),&linfo)!=0)
                continue ;

            switch(perm->cmd)
            {
            	case PropertiesBox::ID_ADD:
                	m=linfo.st_mode|mode;
                	break;
            	case PropertiesBox::ID_CLEAR:
                	m=linfo.st_mode & ~mode;
                	break;
            	case PropertiesBox::ID_SET:
                	m=mode;
                	break;
            	default :
					delete f;
					getApp()->endWaitCursor();
					return 1;
            }
            
			if (files[i]!=".." && !perm->own->getCheck())
			{
				errno=0;
				rc=f->chmod((char*)pathname.text(),file,m,perm->rec->getCheck(),perm->dironly->getCheck(),perm->fileonly->getCheck());
				int errcode=errno;
				
				// If action is cancelled in progress dialog
				if (f->isCancelled())
				{
					f->hideProgressDialog();
					MessageBox::error(this,BOX_OK,_("Error"),_("Change file(s) permissions cancelled!"));
					delete f;
					getApp()->endWaitCursor();
					return 1;
				}
        		
				// Handle chmod errors
				if(rc)
				{
					f->hideProgressDialog();
					if (errcode)
						MessageBox::error(this,BOX_OK,_("Error"),_("Chmod in %s failed: %s"), file, strerror(errcode));
					else
						MessageBox::error(this,BOX_OK,_("Error"),_("Chmod in %s failed"), file);
					delete f;
					getApp()->endWaitCursor();
					return 0;
				}
			}
        }
		delete f;
		getApp()->endWaitCursor();
    }


	// Redraw the file lists
	((XFileExplorer*) mainWindow)->redrawPanels();

    return 1;
}


long PropertiesBox::onCmdCancel(FXObject* o,FXSelector s,void* p)
{
    DialogBox::onCmdCancel(o,s,p);
	
	// Redraw the file lists
	((XFileExplorer*) mainWindow)->redrawPanels();

	return 1;
}

	
long PropertiesBox::onCmdCommand(FXObject* o,FXSelector s,void* p)
{
    perm->cmd = FXSELID(s);
    return 1;
}


long PropertiesBox::onCmdFilter(FXObject* o,FXSelector s,void* p)
{
    perm->flt = FXSELID(s);
    return 1;
}


long PropertiesBox::onCmdCheck(FXObject* o,FXSelector s,void* p)
{
    int xmode=0;
	switch (FXSELID(s))
	{
		case PropertiesBox::ID_RUSR:
			xmode=S_IRUSR;
			break;
		case PropertiesBox::ID_WUSR:
			xmode=S_IWUSR;
			break;
		case PropertiesBox::ID_XUSR:
			xmode=S_IXUSR;
			break;
		case PropertiesBox::ID_RGRP:
			xmode=S_IRGRP;
			break;
		case PropertiesBox::ID_WGRP:
			xmode=S_IWGRP;
			break;
		case PropertiesBox::ID_XGRP:
			xmode=S_IXGRP;
			break;
		case PropertiesBox::ID_ROTH:
			xmode=S_IROTH;
			break;
		case PropertiesBox::ID_WOTH:
			xmode=S_IWOTH;
			break;
		case PropertiesBox::ID_XOTH:
			xmode=S_IXOTH;
			break;
		case PropertiesBox::ID_SUID:
			xmode=S_ISUID;
			break;
		case PropertiesBox::ID_SGID:
			xmode=S_ISGID;
			break;
		case PropertiesBox::ID_SVTX:
			xmode=S_ISVTX;
			break;
	}
	FXCheckButton* ch = (FXCheckButton*)o;    
	if(!ch->getCheck())
       mode &= ~xmode;
    else
        mode |= xmode;
		
    return 1;

}


long PropertiesBox::onCmdBrowse(FXObject* o,FXSelector s,void* p)
{
    FileDialog browseProgram(this,_("Select an executable file"));
    const char *patterns[]=
        {
            _("All files"),     "*",NULL
        };
	browseProgram.setFilename(ROOTDIR);
	browseProgram.setPatternList(patterns);
	browseProgram.setSelectMode(SELECT_FILE_EXISTING);
	if(browseProgram.execute())
    {
        FXString path=browseProgram.getFilename();
        switch(FXSELID(s))
        {
        	case ID_BROWSE_OPEN:
            	open->setText(FXPath::name(path));
            	break;
        	case ID_BROWSE_VIEW:
            	view->setText(FXPath::name(path));
            	break;
        	case ID_BROWSE_EDIT:
            	if(edit)
                	edit->setText(FXPath::name(path));
            	break;
        }
    }
    return 1;
}


long PropertiesBox::onCmdBrowseIcon(FXObject* o,FXSelector s,void* p)
{
    FXString icon;
    if(FXSELID(s)==ID_BIG_ICON)
        icon = bigic->getText();
    else
        icon= miniic->getText();

	FXString iconpath=getApp()->reg().readStringEntry("SETTINGS","iconpath",DEFAULTICONPATH);
    const char *patterns[]=
        {
            _("PNG Images"),     "*.png",
			_("GIF Images"),     "*.gif",
            _("BMP Images"),     "*.bmp",NULL
        };
	FileDialog browseIcon(this,_("Select an icon file"));
    browseIcon.setFilename(iconpath+PATHSEPSTRING+icon);
    browseIcon.setPatternList(patterns);
	browseIcon.setSelectMode(SELECT_FILE_EXISTING);
    if(browseIcon.execute())
    {
        FXString path=browseIcon.getFilename();
        if(!::exists(path))
            return 0;
        if(FXSELID(s)==ID_BIG_ICON)
            bigic->setText(path);
        else
            miniic->setText(path);
    }
    return 1;
}


// Update recursive directory size and permissions
long PropertiesBox::onUpdSizeAndPerm(FXObject* o,FXSelector s,void* p)
{
	// Update recursive size only one time
	if (recsize)
	{
		getApp()->beginWaitCursor();

		char buf[MAXPATHLEN+1];
		char size[64];
		FXString hsize;

		// Single file
		if (num==1)
		{
			// Directory but not mount point
			if (isDirectory && !isMountpoint)
			{
				FXulong dirsize;
				unsigned int nbfiles=0, nbsubfolders=0;

				FXString dirpath=FXPath::absolute(parentdir,filename);
				strlcpy(buf,dirpath.text(),dirpath.length()+1);
				dirsize=pathsize(buf,&nbfiles,&nbsubfolders);
				snprintf(size,sizeof(size)-1,"%llu",dirsize);				
				hsize=::hSize(size);
				snprintf(size,sizeof(size)-1,_("%s (%llu bytes)"),hsize.text(),dirsize);        
				fileSize->setText(size);
				snprintf(size,sizeof(size)-1,_("%u files, %u subfolders"),nbfiles-nbsubfolders,nbsubfolders-1);        
				filesSizeDetails->setText(size);
			}
		}
		
		// Multiple files
		else
		{
			FXulong filesize, dirsize, totalsize=0;
			unsigned int totalnbfiles=0, totalnbsubfolders=0;
			struct stat info;
			int dotdot=0;

			// Total size and files type
			for (int i=0;i<num;i++)
			{
				if(lstatrep(files[i].text(),&info)!=0)
					continue ;

				// Special case of the ".." directory
				if (files[i]=="..")
				{	
					dotdot=1;
					continue;
				}

				// Is it a directory?
				isDirectory=S_ISDIR(info.st_mode);
				if(isDirectory)
				{
					// Recursive size
					FXString dirpath=FXPath::absolute(parentdir,files[i]);
					strlcpy(buf,dirpath.text(),dirpath.length()+1);
					unsigned int nbfiles=0, nbsubfolders=0;
					dirsize=pathsize(buf,&nbfiles,&nbsubfolders);
					totalsize+=dirsize;
					totalnbfiles+=(nbfiles-nbsubfolders);
					totalnbsubfolders+=(nbsubfolders-1);
				}
				else // Regular file
				{
					// File size	
					filesize=(FXulong)info.st_size;
					totalsize+=filesize;
					totalnbfiles++;
				}
			}
				
			// Display total size
			snprintf(size,sizeof(size)-1,"%llu",totalsize);
			hsize=::hSize(size);
			snprintf(size,sizeof(size)-1,_("%s (%llu bytes)"),hsize.text(),totalsize);        
			filesSize->setText(size);
			snprintf(size,sizeof(size)-1,_("%u files, %u subfolders"),totalnbfiles,totalnbsubfolders);        
			filesSizeDetails->setText(size);
		}

		getApp()->endWaitCursor();
	}
	recsize=FALSE;

	// Update permissions
    if(perm->rec->getCheck())
	{
        perm->dironly->enable();
        perm->fileonly->enable();
		perm->all->enable();
	}
    else
	{
		perm->all->disable();
        perm->dironly->disable();
        perm->fileonly->disable();
	}

    if(perm->own->getCheck())
	{
        perm->set->disable();
        perm->clear->disable();
        perm->add->disable();
		perm->ur->disable();
		perm->uw->disable();
		perm->ux->disable();
		perm->gr->disable();
		perm->gw->disable();
		perm->gx->disable();
		perm->or_->disable();
		perm->ow->disable();
		perm->ox->disable();
		perm->suid->disable();
		perm->sgid->disable();
		perm->svtx->disable();
	}
    else
	{
        perm->set->enable();
        perm->clear->enable();
        perm->add->enable();
		perm->ur->enable();
		perm->uw->enable();
		perm->ux->enable();
		perm->gr->enable();
		perm->gw->enable();
		perm->gx->enable();
		perm->or_->enable();
		perm->ow->enable();
		perm->ox->enable();
		perm->suid->enable();
		perm->sgid->enable();
		perm->svtx->enable();
	}

    return 1;
}


long PropertiesBox::onCmdKeyPress(FXObject* sender,FXSelector sel,void* ptr)
{
    FXEvent* event=(FXEvent*)ptr;
    switch(event->code)
    {
    case KEY_Escape:
        handle(this,FXSEL(SEL_COMMAND,ID_CANCEL),NULL);
		return 1;
    case KEY_KP_Enter:
    case KEY_Return:
        handle(this,FXSEL(SEL_COMMAND,ID_ACCEPT_SINGLE),NULL);
		return 1;
    default:
        FXTopWindow::onKeyPress(sender,sel,ptr);
		return 1;
    }
	return 0;
}


#ifdef STARTUP_NOTIFICATION
// Update the startup notification button depending on the file exec status
long PropertiesBox::onUpdSnDisable(FXObject*,FXSelector,void*)
{
	FXbool usesn=getApp()->reg().readUnsignedEntry("OPTIONS","use_startup_notification",TRUE);
	if (usesn && executable)
	{
		sngroup->enable();
		snbutton->enable();
	}
	else
	{
		sngroup->disable();
		snbutton->disable();
	}
    return 1;
}
#endif
