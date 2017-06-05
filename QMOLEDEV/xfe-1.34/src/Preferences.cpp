// Preferences dialog box

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "icons.h"
#include "xfedefs.h"
#include "xfeutils.h"
#include "FileDialog.h"
#include "FontDialog.h"
#include "XFileExplorer.h"
#include "MessageBox.h"
#include "Keybindings.h"
#include "Preferences.h"


FXbool Theme::operator != (const Theme& t)
{
    for(int i=0;i<NUM_COLORS;i++)
        if(color[i] != t.color[i])
            return TRUE;
    return FALSE;
}


// Main window
extern FXMainWindow *mainWindow;

// Single click navigation
extern FXbool single_click;

// File tooltips
extern FXbool file_tooltips;

// Relative resizing of the panels and columns in detailed mode
extern FXbool relative_resize;

// Save window position 
extern FXbool save_win_pos;


// Create hilite color from given color for gradient controls
FXColor makeHiliteColorGradient(FXColor color)
{
	unsigned int r, g, b;
	
	r=FXREDVAL(color);
	g=FXGREENVAL(color);
	b=FXBLUEVAL(color);

	r=(unsigned int)(FXMIN(1.2*r,255));
	g=(unsigned int)(FXMIN(1.2*g,255));
	b=(unsigned int)(FXMIN(1.2*b,255));

	return FXRGB(r,g,b);
}


// Create shadow color from given color for gradient controls
FXColor makeShadowColorGradient(FXColor color)
{
	unsigned int r, g, b;
	
	r=FXREDVAL(color);
	g=FXGREENVAL(color);
	b=FXBLUEVAL(color);

	r=(unsigned int)(0.7*r);
	g=(unsigned int)(0.7*g);
	b=(unsigned int)(0.7*b);

	return FXRGB(r,g,b);
}


// Map
FXDEFMAP(PreferencesBox) PreferencesMap[]=
{
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_ACCEPT,PreferencesBox::onCmdAccept),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_CANCEL,PreferencesBox::onCmdCancel),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_TXTEDIT,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_TXTVIEW,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_IMGVIEW,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_ARCHIVER,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_PDFVIEW,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_VIDEOVIEW,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_AUDIOVIEW,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_XTERM,PreferencesBox::onCmdBrowse),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_COLOR,PreferencesBox::onCmdColor),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_NORMALFONT,PreferencesBox::onCmdNormalFont),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_TEXTFONT,PreferencesBox::onCmdTextFont),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_THEME,PreferencesBox::onCmdTheme),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_BROWSE_ICON_PATH,PreferencesBox::onCmdBrowsePath),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_SU_CMD,PreferencesBox::onCmdSuMode),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_SUDO_CMD,PreferencesBox::onCmdSuMode),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_STANDARD_CONTROLS,PreferencesBox::onCmdControls),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_CLEARLOOKS_CONTROLS,PreferencesBox::onCmdControls),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_WHEELADJUST,PreferencesBox::onCmdWheelAdjust),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_CHANGE_KEYBINDINGS,PreferencesBox::onCmdChangeKeyBindings),
	FXMAPFUNC(SEL_COMMAND,PreferencesBox::ID_RESTORE_KEYBINDINGS,PreferencesBox::onCmdRestoreKeyBindings),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_STANDARD_CONTROLS,PreferencesBox::onUpdControls),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_CLEARLOOKS_CONTROLS,PreferencesBox::onUpdControls),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_COLOR,PreferencesBox::onUpdColor),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_WHEELADJUST,PreferencesBox::onUpdWheelAdjust),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_SINGLE_CLICK_FILEOPEN,PreferencesBox::onUpdSingleClickFileopen),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_CONFIRM_TRASH,PreferencesBox::onUpdTrash),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_TRASH_BYPASS,PreferencesBox::onUpdTrash),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_CONFIRM_DEL_EMPTYDIR,PreferencesBox::onUpdConfirmDelEmptyDir),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_SU_CMD,PreferencesBox::onUpdSuMode),
	FXMAPFUNC(SEL_UPDATE,PreferencesBox::ID_SUDO_CMD,PreferencesBox::onUpdSuMode),
};

// Object implementation
FXIMPLEMENT(PreferencesBox,DialogBox,PreferencesMap,ARRAYNUMBER(PreferencesMap))

// Construct window
PreferencesBox::PreferencesBox(FXWindow *win, FXColor listbackcolor, FXColor listforecolor, FXColor highlightcolor, FXColor pbarcolor, FXColor attentioncolor, FXColor scrollbarcolor): DialogBox(win,_("Preferences"), DECOR_TITLE|DECOR_BORDER)
{
    currTheme.name = _("Current Theme");
    currTheme.color[0] = getApp()->getBaseColor();
    currTheme.color[1] = getApp()->getBorderColor();
    currTheme.color[2] = getApp()->getBackColor();
    currTheme.color[3] = getApp()->getForeColor();
    currTheme.color[4] = getApp()->getSelbackColor();
    currTheme.color[5] = getApp()->getSelforeColor();
    currTheme.color[6] = listbackcolor;
    currTheme.color[7] = listforecolor;
    currTheme.color[8] = highlightcolor;
    currTheme.color[9] = pbarcolor;
	currTheme.color[10] = attentioncolor;
	currTheme.color[11] = scrollbarcolor;

    Themes[0] = currTheme;
    Themes[1] = Theme("Clearlooks",FXRGB(237,236,235),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(139,175,220),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(121,153,192),FXRGB(255,0,0),FXRGB(149,178,215));
    Themes[2] = Theme("Human",FXRGB(239,235,231),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(0,0,0),FXRGB(211,170,123),FXRGB(255,255,255),FXRGB(238,238,238),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(195,158,114),FXRGB(255,0,0),FXRGB(212,172,124));
    Themes[3] = Theme("Sea Sky",FXRGB(165,178,198),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(49,101,156),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(49,101,156),FXRGB(255,0,0),FXRGB(68,106,146));
    Themes[4] = Theme("Blue Slate",FXRGB(156,186,205),FXRGB(0,0,0),FXRGB(197,194,197),FXRGB(0,0,0),FXRGB(82,129,148),FXRGB(255,255,255),FXRGB(197,194,197),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(82,129,148),FXRGB(255,0,0),FXRGB(156,186,205));
    Themes[5] = Theme("FOX",FXRGB(212,208,200),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(10,36,106),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(10,36,106),FXRGB(255,0,0),FXRGB(212,208,200));
    Themes[6] = Theme("GNOME2",FXRGB(220,218,213),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(75,105,131),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(75,105,131),FXRGB(255,0,0),FXRGB(134,171,217));
    Themes[7] = Theme("KDE3",FXRGB(238,238,230),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(255,222,118),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(255,222,118),FXRGB(255,0,0),FXRGB(238,238,230));
    Themes[8] = Theme("XFCE4",FXRGB(238,238,238),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(0,0,0),FXRGB(99,119,146),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(99,119,146),FXRGB(255,0,0),FXRGB(238,238,238));
    Themes[9] = Theme("CDE",FXRGB(156,153,156),FXRGB(0,0,0),FXRGB(131,129,131),FXRGB(255,255,255),FXRGB(49,97,131),FXRGB(255,255,255),FXRGB(131,129,131),FXRGB(255,255,255),FXRGB(165,162,165),FXRGB(49,97,131),FXRGB(255,0,0),FXRGB(156,153,156));
    Themes[10] = Theme("iMac",FXRGB(205,206,205),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(0,0,128),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(0,0,128),FXRGB(255,0,0),FXRGB(205,206,205));
	Themes[11] = Theme("Windows 95",FXRGB(192,192,192),FXRGB(0,0,0),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(0,0,128),FXRGB(255,255,255),FXRGB(255,255,255),FXRGB(0,0,0),FXRGB(238,238,238),FXRGB(0,0,128),FXRGB(255,0,0),FXRGB(192,192,192));

    // Buttons
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,5,5);

    // Contents
    FXHorizontalFrame *contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH);

	// Accept
    FXButton *ok = new FXButton(buttons,_("&Accept"),NULL,this,PreferencesBox::ID_ACCEPT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    ok->addHotKey(KEY_Return);
    
    // Cancel
    new FXButton(buttons,_("&Cancel"),NULL,this,PreferencesBox::ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    
	// Switcher
    FXTabBook *tabbook = new FXTabBook(contents,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);


    // First tab - General options
    new FXTabItem(tabbook,_("&General"),NULL);
    FXVerticalFrame *options=new FXVerticalFrame(tabbook,FRAME_RAISED);

	FXGroupBox *group=new FXGroupBox(options,_("Options"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    trashcan=new FXCheckButton(group,_("Use trash can for file deletion (safe delete)")); 
	trashbypass=new FXCheckButton(group,_("Include a command to bypass the trash can (permanent delete)"),this,ID_TRASH_BYPASS);
    autosave=new FXCheckButton(group,_("Auto save layout"));
	savewinpos=new FXCheckButton(group,_("Save window position"));
    diropen=new FXCheckButton(group,_("Single click directory open"));
    fileopen=new FXCheckButton(group,_("Single click file open"),this,ID_SINGLE_CLICK_FILEOPEN);
    filetooltips=new FXCheckButton(group,_("Display tooltips in file and directory lists"),this,ID_FILE_TOOLTIPS);
    relativeresize=new FXCheckButton(group,_("Relative resizing of file lists"),this,ID_RELATIVE_RESIZE);
    showpathlink=new FXCheckButton(group,_("Display a path linker above file lists"),this,ID_SHOW_PATHLINK);
#ifdef STARTUP_NOTIFICATION
	usesn=new FXCheckButton(group,_("Notify when applications start up"));
#endif

    FXMatrix *matrix = new FXMatrix(group,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    new FXLabel(matrix,_("Date format used in file and directory lists:\n(Type 'man strftime' in a terminal for help on the format)"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    timeformat = new FXTextField(matrix,15,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    oldtimeformat=getApp()->reg().readStringEntry("SETTINGS","time_format",DEFAULT_TIME_FORMAT);
    timeformat->setText(oldtimeformat);


	group=new FXGroupBox(options,_("Mouse"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    scroll=new FXCheckButton(group,_("Smooth scrolling in file lists and text windows"));
    matrix = new FXMatrix(group,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_Y);
	new FXLabel(matrix,_("Mouse scrolling speed:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    FXSpinner* spinner=new FXSpinner(matrix,3,this,PreferencesBox::ID_WHEELADJUST,JUSTIFY_RIGHT|LAYOUT_FILL_X|LAYOUT_FILL_ROW,0,0,0,0, 2,2,1,1);
    spinner->setRange(1,100);
	FXbool smoothscroll=getApp()->reg().readUnsignedEntry("SETTINGS","smooth_scroll",TRUE);
	scroll->setCheck(smoothscroll);

	group=new FXGroupBox(options,_("Root mode"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	rootmode=new FXCheckButton(group,_("Allow root mode"));
	FXRadioButton* subutton=new FXRadioButton(group,_("Authentication using su (uses root password)"),this,ID_SU_CMD);
	FXRadioButton* sudobutton=new FXRadioButton(group,_("Authentication using sudo (uses user password)"),this,ID_SUDO_CMD);

    FXbool root_mode=getApp()->reg().readUnsignedEntry("OPTIONS","root_mode",TRUE);
    rootmode->setCheck(root_mode);

	if (getuid()==0) // Super user
	{
		rootmode->disable();
		subutton->disable();
		sudobutton->disable();
	}
	use_sudo=getApp()->reg().readUnsignedEntry("OPTIONS","use_sudo",FALSE);

    FXbool use_trash_can=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_can",TRUE);
	trashcan->setCheck(use_trash_can);
    
	if (trashcan->getCheck())
	{
	    FXbool use_trash_bypass=getApp()->reg().readUnsignedEntry("OPTIONS","use_trash_bypass",FALSE);
		trashbypass->setCheck(use_trash_bypass);
	}
	else
		trashbypass->disable();

	FXbool auto_save_layout=getApp()->reg().readUnsignedEntry("OPTIONS","auto_save_layout",TRUE);
	autosave->setCheck(auto_save_layout);

    FXbool save_win_pos=getApp()->reg().readUnsignedEntry("SETTINGS","save_win_pos",FALSE);
	savewinpos->setCheck(save_win_pos);
    
	// Single click navigation
	single_click=getApp()->reg().readUnsignedEntry("SETTINGS","single_click",SINGLE_CLICK_NONE);
	single_click=getApp()->reg().readUnsignedEntry("SETTINGS","single_click",SINGLE_CLICK_NONE);
	if (single_click==SINGLE_CLICK_DIR)
	{
		diropen->setCheck(TRUE);
		fileopen->setCheck(FALSE);
	}
	else if (single_click==SINGLE_CLICK_DIR_FILE)
	{
		diropen->setCheck(TRUE);
		fileopen->setCheck(TRUE);
	}
	else
	{
		diropen->setCheck(FALSE);
		fileopen->setCheck(FALSE);
	}
	
	// File tooltips
	if (file_tooltips==FALSE)
		filetooltips->setCheck(FALSE);
	else
		filetooltips->setCheck(TRUE);
	
	// Relative resizing
	if (relative_resize==FALSE)
		relativeresize->setCheck(FALSE);
	else
		relativeresize->setCheck(TRUE);
		
	// Display path linker
	show_pathlink=getApp()->reg().readUnsignedEntry("SETTINGS","show_pathlinker",TRUE);
	if (show_pathlink==FALSE)
		showpathlink->setCheck(FALSE);
	else
		showpathlink->setCheck(TRUE);	

#ifdef STARTUP_NOTIFICATION
    FXbool use_sn=getApp()->reg().readUnsignedEntry("OPTIONS","use_startup_notification",TRUE);
	usesn->setCheck(use_sn);
#endif
		
    // Second tab - Dialogs
    new FXTabItem(tabbook,_("&Dialogs"),NULL);
    FXVerticalFrame *dialogs=new FXVerticalFrame(tabbook,FRAME_RAISED);
	group=new FXGroupBox(dialogs,_("Confirmations"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    ask=new FXCheckButton(group,_("Confirm copy/move/rename/symlink"));
    dnd=new FXCheckButton(group,_("Confirm drag and drop"));
	trashmv=new FXCheckButton(group,_("Confirm move to trash/restore from trash"),this,ID_CONFIRM_TRASH);
	del=new FXCheckButton(group,_("Confirm delete"));
	del_emptydir=new FXCheckButton(group,_("Confirm delete non empty directories"),this,ID_CONFIRM_DEL_EMPTYDIR);
    overwrite=new FXCheckButton(group,_("Confirm overwrite"));
    exec=new FXCheckButton(group,_("Confirm execute text files"));

	group=new FXGroupBox(dialogs,_("Warnings"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
#if defined(linux)
    mount=new FXCheckButton (group,_("Warn when mount points are not responding"));
	show_mount=new FXCheckButton(group, _("Display mount/umount success messages"));
#endif
	preserve_date_warning = new FXCheckButton (group,_("Warn when date preservation failed"));
    root_warning=new FXCheckButton (group,_("Warn if running as root"));

    FXbool confirm_trash=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_trash",TRUE);
	trashmv->setCheck(confirm_trash);
    FXbool confirm_del=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_delete",TRUE);
	del->setCheck(confirm_del);
    FXbool confirm_del_emptydir=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_delete_emptydir",TRUE);
	del_emptydir->setCheck(confirm_del_emptydir);
    FXbool confirm_overwrite=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_overwrite",TRUE);
	overwrite->setCheck(confirm_overwrite);
    FXbool confirm_exec=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_execute",TRUE);
	exec->setCheck(confirm_exec);
    FXbool ask_before_copy=getApp()->reg().readUnsignedEntry("OPTIONS","ask_before_copy",TRUE);
	ask->setCheck(ask_before_copy);
    FXbool confirm_dnd=getApp()->reg().readUnsignedEntry("OPTIONS","confirm_drag_and_drop",TRUE);
	dnd->setCheck(confirm_dnd);

#if defined(linux)
	FXbool mount_warn=getApp()->reg().readUnsignedEntry("OPTIONS","mount_warn",TRUE);
	FXbool mount_messages=getApp()->reg().readUnsignedEntry("OPTIONS","mount_messages",TRUE);
	mount->setCheck(mount_warn);
	show_mount->setCheck(mount_messages);
#endif

	FXbool preserve_date_warn=getApp()->reg().readUnsignedEntry("OPTIONS","preserve_date_warn",TRUE);
    preserve_date_warning->setCheck(preserve_date_warn);

	FXbool root_warn=getApp()->reg().readUnsignedEntry("OPTIONS","root_warn",TRUE);
	if(getuid()) // Simple user
        root_warning->disable();
    else
        root_warning->setCheck(root_warn);

	// Third tab - Programs
    new FXTabItem(tabbook,_("&Programs"),NULL);
    FXVerticalFrame *programs=new FXVerticalFrame(tabbook,FRAME_RAISED);
    group=new FXGroupBox(programs,_("Default programs"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    matrix = new FXMatrix(group,3,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    new FXLabel(matrix,_("Text editor:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    txteditor = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_TXTEDIT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldtxteditor=getApp()->reg().readStringEntry("PROGS","txteditor",DEFAULT_TXTEDITOR);
    txteditor->setText(oldtxteditor);

	new FXLabel(matrix,_("Text viewer:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    txtviewer = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_TXTVIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldtxtviewer=getApp()->reg().readStringEntry("PROGS","txtviewer",DEFAULT_TXTVIEWER);
    txtviewer->setText(oldtxtviewer);

	new FXLabel(matrix,_("Image editor:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    imgeditor = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_IMGVIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldimgeditor=getApp()->reg().readStringEntry("PROGS","imgeditor",DEFAULT_IMGEDITOR);
    imgeditor->setText(oldimgeditor);

	new FXLabel(matrix,_("Image viewer:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    imgviewer = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_IMGVIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldimgviewer=getApp()->reg().readStringEntry("PROGS","imgviewer",DEFAULT_IMGVIEWER);
    imgviewer->setText(oldimgviewer);

	new FXLabel(matrix,_("Archiver:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    archiver = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_ARCHIVER,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldarchiver=getApp()->reg().readStringEntry("PROGS","archiver",DEFAULT_ARCHIVER);
    archiver->setText(oldarchiver);

	new FXLabel(matrix,_("Pdf viewer:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    pdfviewer = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_PDFVIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldpdfviewer=getApp()->reg().readStringEntry("PROGS","pdfviewer",DEFAULT_PDFVIEWER);
    pdfviewer->setText(oldpdfviewer);

	new FXLabel(matrix,_("Audio player:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    audioviewer = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_AUDIOVIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldaudioviewer=getApp()->reg().readStringEntry("PROGS","audioviewer",DEFAULT_AUDIOVIEWER);
    audioviewer->setText(oldaudioviewer);

	new FXLabel(matrix,_("Video player:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    videoviewer = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_VIDEOVIEW,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldvideoviewer=getApp()->reg().readStringEntry("PROGS","videoviewer",DEFAULT_VIDEOVIEWER);
    videoviewer->setText(oldvideoviewer);

	new FXLabel(matrix,_("Terminal:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    xterm = new FXTextField(matrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix,_("\tSelect file..."),filedialogicon,this,ID_BROWSE_XTERM,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    oldxterm=getApp()->reg().readStringEntry("PROGS","xterm",DEFAULT_TERMINAL);
    xterm->setText(oldxterm);

	// Fourth tab - Visual
    new FXTabItem(tabbook,_("&Themes"),NULL);
    FXVerticalFrame *visual=new FXVerticalFrame(tabbook,FRAME_RAISED);
    FXGroupBox *themes=new FXGroupBox(visual,_("Color theme"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    FXPacker *pack = new FXPacker(themes,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_Y|LAYOUT_FILL_X,0,0,0,0, 0,0,0,0);
    themesList = new FXList(pack,this,ID_THEME,LIST_BROWSESELECT|FRAME_SUNKEN|FRAME_THICK|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    themesList->setNumVisible(7);
	for(int i=0;i<NUM_THEMES;i++)
        themesList->appendItem(Themes[i].name);
    themesList->setCurrentItem(0);

    FXGroupBox *colors=new FXGroupBox(visual,_("Custom colors"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
	FXMatrix *matrix3 = new FXMatrix(colors,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    colorsBox = new FXComboBox(matrix3,NUM_COLORS,NULL,0,COMBOBOX_STATIC|FRAME_SUNKEN|LAYOUT_FILL_X|LAYOUT_SIDE_RIGHT|LAYOUT_CENTER_Y);
  	colorsBox->setNumVisible(NUM_COLORS);
  	cwell=new FXColorWell(matrix3,FXRGB(0,0,0),this,ID_COLOR,LAYOUT_FILL_X|LAYOUT_FILL_COLUMN|LAYOUT_FILL_Y,0,0,0,0, 10,10,0,0);
  	cwell->setTipText(_("Double click to customize the color"));

    colorsBox->appendItem(_("Base color"));
    colorsBox->appendItem(_("Border color"));
    colorsBox->appendItem(_("Background color"));
    colorsBox->appendItem(_("Text color"));
    colorsBox->appendItem(_("Selection background color"));
    colorsBox->appendItem(_("Selection text color"));
    colorsBox->appendItem(_("File list background color"));
    colorsBox->appendItem(_("File list text color"));
	colorsBox->appendItem(_("File list highlight color"));
	colorsBox->appendItem(_("Progress bar color"));
	colorsBox->appendItem(_("Attention color"));
	colorsBox->appendItem(_("Scrollbar color"));
    colorsBox->setCurrentItem(0);

	// Controls theme
	FXGroupBox *button=new FXGroupBox(visual,_("Controls"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	use_clearlooks=getApp()->reg().readUnsignedEntry("SETTINGS","use_clearlooks",TRUE);
	new FXRadioButton(button,_("Standard (classic controls)"),this,ID_STANDARD_CONTROLS);
	new FXRadioButton(button,_("Clearlooks (modern looking controls)"),this,ID_CLEARLOOKS_CONTROLS);

	// Find iconpath from the Xfe registry settings or set it to DEFAULTICONPATH
	FXGroupBox *group2=new FXGroupBox(visual,_("Icon theme path"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X);
	FXMatrix *matrix2 = new FXMatrix(group2,2,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	iconpath = new FXTextField(matrix2,40,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(matrix2,_("\tSelect path..."),filedialogicon,this,ID_BROWSE_ICON_PATH,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
	oldiconpath=getApp()->reg().readStringEntry("SETTINGS","iconpath",DEFAULTICONPATH);
    iconpath->setText(oldiconpath);

    // Fifth tab - Fonts
    new FXTabItem(tabbook,_("&Fonts"),NULL);
    FXVerticalFrame *fonts=new FXVerticalFrame(tabbook,FRAME_RAISED);
    FXGroupBox *fgroup=new FXGroupBox(fonts,_("Fonts"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    FXMatrix *fmatrix = new FXMatrix(fgroup,3,MATRIX_BY_COLUMNS|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    new FXLabel(fmatrix,_("Normal font:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    normalfont = new FXTextField(fmatrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
    new FXButton(fmatrix,_(" Select..."),NULL,this,ID_NORMALFONT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y);//,0,0,0,0,20,20);
    oldnormalfont=getApp()->reg().readStringEntry("SETTINGS","font","Helvetica,100,normal,regular");
    normalfont->setText(oldnormalfont);

    new FXLabel(fmatrix,_("Text font:"),NULL,JUSTIFY_LEFT|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW);
    textfont = new FXTextField(fmatrix,30,NULL,0,FRAME_THICK|FRAME_SUNKEN|LAYOUT_FILL_COLUMN|LAYOUT_FILL_ROW|LAYOUT_FILL_X);
	new FXButton(fmatrix,_(" Select..."),NULL,this,ID_TEXTFONT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y);//0,0,0,0,20,20);
    oldtextfont=getApp()->reg().readStringEntry("SETTINGS","textfont","Helvetica,100,normal,regular");
    textfont->setText(oldtextfont);

    // Sixth tab - Key bindings
    new FXTabItem(tabbook,_("&Key Bindings"),NULL);
    FXVerticalFrame *keybindings=new FXVerticalFrame(tabbook,FRAME_RAISED);
    FXGroupBox *kbgroup=new FXGroupBox(keybindings,_("Key Bindings"),GROUPBOX_TITLE_LEFT|FRAME_GROOVE|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    FXPacker *kbpack = new FXPacker(kbgroup,LAYOUT_FILL_X);
    new FXButton(kbpack,_("Modify key bindings..."),minikeybindingsicon,this,ID_CHANGE_KEYBINDINGS,FRAME_RAISED|FRAME_THICK|ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);//,0,0,0,0,20,20);
    new FXButton(kbpack,_("Restore default key bindings..."),reloadicon,this,ID_RESTORE_KEYBINDINGS,FRAME_RAISED|FRAME_THICK|ICON_BEFORE_TEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X);//,0,0,0,0,20,20);
	
	// Initializations
	bindingsbox=NULL;
	glbBindingsDict=NULL;
	xfeBindingsDict=NULL;
	xfiBindingsDict=NULL;
	xfwBindingsDict=NULL;
}


long PreferencesBox::onUpdColor(FXObject* o,FXSelector s,void* p)
{
    FXColorWell *cwell = (FXColorWell*)o;
    int i = colorsBox->getCurrentItem();
    cwell->setRGBA(currTheme.color[i]);

    return 1;
}


long PreferencesBox::onCmdColor(FXObject* o,FXSelector s,void* p)
{
	FXColorWell *cwell = (FXColorWell*)o;
    int i = colorsBox->getCurrentItem();
    currTheme.color[i] = cwell->getRGBA();
    
    return 1;
}


long PreferencesBox::onCmdTheme(FXObject* o,FXSelector s,void* p)
{
    currTheme = Themes[themesList->getCurrentItem()];
    return 1;
}


long PreferencesBox::onCmdBrowsePath(FXObject* o,FXSelector s,void* p)
{
	FileDialog browse(this,_("Select an icon theme folder or an icon file"));
	browse.setSelectMode(SELECT_FILE_MIXED);
	browse.setDirectory(iconpath->getText());
	if(browse.execute())
	{
		FXString path=browse.getFilename();
		if (::isFile(path))
			iconpath->setText(FXPath::directory(path).text());
		else
			iconpath->setText(path);
	}
	return 1;
}


long PreferencesBox::onCmdBrowse(FXObject* o,FXSelector s,void* p)
{
    FileDialog browse(this,_("Select an executable file"));
    const char *patterns[]=
        {
            _("All files"),     "*",NULL
        };
	browse.setFilename(ROOTDIR);
	browse.setPatternList(patterns);
	browse.setSelectMode(SELECT_FILE_EXISTING);
    if(browse.execute())
    {
		FXString path=browse.getFilename();
		
		switch(FXSELID(s))
        {
        case ID_BROWSE_TXTEDIT:
            txteditor->setText(FXPath::name(path));
            break;
        case ID_BROWSE_TXTVIEW:
            txtviewer->setText(FXPath::name(path));
            break;
        case ID_BROWSE_IMGVIEW:
            imgviewer->setText(FXPath::name(path));
            break;
        case ID_BROWSE_ARCHIVER:
            archiver->setText(FXPath::name(path));
            break;
        case ID_BROWSE_PDFVIEW:
            pdfviewer->setText(FXPath::name(path));
            break;
        case ID_BROWSE_AUDIOVIEW:
            audioviewer->setText(FXPath::name(path));
            break;
        case ID_BROWSE_VIDEOVIEW:
            videoviewer->setText(FXPath::name(path));
            break;
        case ID_BROWSE_XTERM:
            xterm->setText(FXPath::name(path));
            break;
        }
    }
    return 1;
}

// Change normal font
long PreferencesBox::onCmdNormalFont(FXObject*,FXSelector,void*)
{
    FontDialog fontdlg(this,_("Change Normal Font"),DECOR_BORDER|DECOR_TITLE);
    FXFontDesc fontdesc;
	FXString fontspec;

	fontspec=getApp()->reg().readStringEntry("SETTINGS","font","Helvetica,100,normal,regular");
   	FXFont* nfont=new FXFont(getApp(),fontspec);
	nfont->create();
	nfont->getFontDesc(fontdesc);
    
	fontdlg.setFontSelection(fontdesc);
    if(fontdlg.execute())
    {
        fontdlg.getFontSelection(fontdesc);
		nfont->setFontDesc(fontdesc);
		fontspec=nfont->getFont();
        normalfont->setText(fontspec);  
    }
    return 1;
}

// Change text font
long PreferencesBox::onCmdTextFont(FXObject*,FXSelector,void*)
{
    FontDialog fontdlg(this,_("Change Text Font"),DECOR_BORDER|DECOR_TITLE);
    FXFontDesc fontdesc;
	FXString fontspec;
	fontspec=getApp()->reg().readStringEntry("SETTINGS","textfont","Helvetica,100,normal,regular");
   	FXFont* tfont=new FXFont(getApp(),fontspec);
	tfont->create();
	tfont->getFontDesc(fontdesc);
    fontdlg.setFontSelection(fontdesc);
    if(fontdlg.execute())
    {
        fontdlg.getFontSelection(fontdesc);
		tfont->setFontDesc(fontdesc);
		fontspec=tfont->getFont();
        textfont->setText(fontspec);
    }
    return 1;
}


// Change key bindings
long PreferencesBox::onCmdChangeKeyBindings(FXObject*,FXSelector,void*)
{
	FXString key, str;

    // String dictionary used to store global key bindings 
	if (glbBindingsDict==NULL)
		glbBindingsDict=new FXStringDict();

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_back","Ctrl-Backspace");
	str=_("Go to previous directory")+TAB+key;
	glbBindingsDict->insert("go_back",str.text());
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_forward","Shift-Backspace");
	str=_("Go to next directory")+TAB+key;
	glbBindingsDict->insert("go_forward",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_up","Backspace");
	str=_("Go to parent directory")+TAB+key;
	glbBindingsDict->insert("go_up",str.text());
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_home","Ctrl-H");
	str=_("Go to home directory")+TAB+key;
	glbBindingsDict->insert("go_home",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_file","F2");
	str=_("Create new file")+TAB+key;
	glbBindingsDict->insert("new_file",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_folder","F7");
	str=_("Create new folder")+TAB+key;
	glbBindingsDict->insert("new_folder",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy","Ctrl-C");
	str=_("Copy to clipboard")+TAB+key;
	glbBindingsDict->insert("copy",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","cut","Ctrl-X");
	str=_("Cut to clipboard")+TAB+key;
	glbBindingsDict->insert("cut",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","paste","Ctrl-V");
	str=_("Paste from clipboard")+TAB+key;
	glbBindingsDict->insert("paste",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","big_icons","F10");
 	str=_("Big icon list")+TAB+key;
	glbBindingsDict->insert("big_icons",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","small_icons","F11");
 	str=_("Small icon list")+TAB+key;
	glbBindingsDict->insert("small_icons",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","detailed_file_list","F12");
 	str=_("Detailed file list")+TAB+key;
	glbBindingsDict->insert("detailed_file_list",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
 	str=_("Open file")+TAB+key;
	glbBindingsDict->insert("open",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
 	str=_("Quit application")+TAB+key;
	glbBindingsDict->insert("quit",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","select_all","Ctrl-A");
 	str=_("Select all")+TAB+key;
	glbBindingsDict->insert("select_all",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","deselect_all","Ctrl-Z");
 	str=_("Deselect all")+TAB+key;
	glbBindingsDict->insert("deselect_all",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","invert_selection","Ctrl-I");
 	str=_("Invert selection")+TAB+key;
	glbBindingsDict->insert("invert_selection",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","help","F1");
 	str=_("Display help")+TAB+key;
	glbBindingsDict->insert("help",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_files","Ctrl-F6");
  	str=_("Toggle display hidden files")+TAB+key;
	glbBindingsDict->insert("hidden_files",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","thumbnails","Ctrl-F7");
 	str=_("Toggle display thumbnails")+TAB+key;
	glbBindingsDict->insert("thumbnails",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_work","Shift-F2");
 	str=_("Go to working directory")+TAB+key;
	glbBindingsDict->insert("go_work",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
 	str=_("Close window")+TAB+key;
	glbBindingsDict->insert("close",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
 	str=_("Print file")+TAB+key;
	glbBindingsDict->insert("print",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search","Ctrl-F");
 	str=_("Search")+TAB+key;
	glbBindingsDict->insert("search",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_prev","Shift-Ctrl-G");
 	str=_("Search previous")+TAB+key;
	glbBindingsDict->insert("search_prev",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","search_next","Ctrl-G");
 	str=_("Search next")+TAB+key;
	glbBindingsDict->insert("search_next",str.text());


    // Key bindings specific to X File Explorer (Xfe)
	if (xfeBindingsDict==NULL)
		xfeBindingsDict=new FXStringDict();

	key=getApp()->reg().readStringEntry("KEYBINDINGS","refresh","Ctrl-R");
	str=_("Refresh panels")+TAB+key;
	xfeBindingsDict->insert("refresh",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_symlink","Ctrl-J");
	str=_("Create new symbolic link")+TAB+key;
	xfeBindingsDict->insert("new_symlink",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","properties","F9");
	str=_("File properties")+TAB+key;
	xfeBindingsDict->insert("properties",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","move_to_trash","Del");
	str=_("Move files to trash")+TAB+key;
	xfeBindingsDict->insert("move_to_trash",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","restore_from_trash","Alt-Del");
	str=_("Restore files from trash")+TAB+key;
	xfeBindingsDict->insert("restore_from_trash",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","delete","Shift-Del");
	str=_("Delete files")+TAB+key;
	xfeBindingsDict->insert("delete",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_window","F3");
	str=_("Create new window")+TAB+key;
	xfeBindingsDict->insert("new_window",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","new_root_window","Shift-F3");
	str=_("Create new root window")+TAB+key;
	xfeBindingsDict->insert("new_root_window",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","execute_command","Ctrl-E");
	str=_("Execute command")+TAB+key;
	xfeBindingsDict->insert("execute_command",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","terminal","Ctrl-T");
	str=_("Launch terminal")+TAB+key;
	xfeBindingsDict->insert("terminal",str.text());

#if defined(linux)
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","mount","Ctrl-M");
 	str=_("Mount file system (Linux only)")+TAB+key;
	xfeBindingsDict->insert("mount",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","unmount","Ctrl-U");
 	str=_("Unmount file system (Linux only)")+TAB+key;
	xfeBindingsDict->insert("unmount",str.text());

#endif
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","one_panel","Ctrl-F1");
 	str=_("One panel mode")+TAB+key;
	xfeBindingsDict->insert("one_panel",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","tree_panel","Ctrl-F2");
 	str=_("Tree and panel mode")+TAB+key;
	xfeBindingsDict->insert("tree_panel",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","two_panels","Ctrl-F3");
 	str=_("Two panels mode")+TAB+key;
	xfeBindingsDict->insert("two_panels",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","tree_two_panels","Ctrl-F4");
 	str=_("Tree and two panels mode")+TAB+key;
	xfeBindingsDict->insert("tree_two_panels",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","clear_location","Ctrl-L");
 	str=_("Clear location bar")+TAB+key;
	xfeBindingsDict->insert("clear_location",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","rename","Ctrl-N");
 	str=_("Rename file")+TAB+key;
	xfeBindingsDict->insert("rename",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","copy_to","F5");
 	str=_("Copy files to location")+TAB+key;
	xfeBindingsDict->insert("copy_to",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","move_to","F6");
 	str=_("Move files to location")+TAB+key;
	xfeBindingsDict->insert("move_to",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","symlink_to","Ctrl-S");
 	str=_("Symlink files to location")+TAB+key;
	xfeBindingsDict->insert("symlink_to",str.text());

  	key=getApp()->reg().readStringEntry("KEYBINDINGS","add_bookmark","Ctrl-B");
 	str=_("Add bookmark")+TAB+key;
	xfeBindingsDict->insert("add_bookmark",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","synchronize_panels","Ctrl-Y");
 	str=_("Synchronize panels")+TAB+key;
	xfeBindingsDict->insert("synchronize_panels",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","switch_panels","Ctrl-K");
 	str=_("Switch panels")+TAB+key;
	xfeBindingsDict->insert("switch_panels",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","go_to_trash","Ctrl-F8");
 	str=_("Go to trash can")+TAB+key;
	xfeBindingsDict->insert("go_to_trash",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","empty_trash_can","Ctrl-Del");
 	str=_("Empty trash can")+TAB+key;
	xfeBindingsDict->insert("empty_trash_can",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","view","Shift-F4");
 	str=_("View")+TAB+key;
	xfeBindingsDict->insert("view",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","edit","F4");
 	str=_("Edit")+TAB+key;
	xfeBindingsDict->insert("edit",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_dirs","Ctrl-F5");
 	str=_("Toggle display hidden directories")+TAB+key;
	xfeBindingsDict->insert("hidden_dirs",str.text());

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","filter","Ctrl-D");
 	str=_("Filter files")+TAB+key;
	xfeBindingsDict->insert("filter",str.text());


	// Key bindings specific to X File Image (Xfi)
	if (xfiBindingsDict==NULL)
		xfiBindingsDict=new FXStringDict();

	key=getApp()->reg().readStringEntry("KEYBINDINGS","zoom_100","Ctrl-I");
 	str=_("Zoom image to 100%")+TAB+key;
	xfiBindingsDict->insert("zoom_100",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","zoom_win","Ctrl-F");
 	str=_("Zoom to fit window")+TAB+key;
	xfiBindingsDict->insert("zoom_win",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_left","Ctrl-L");
 	str=_("Rotate image to left")+TAB+key;
	xfiBindingsDict->insert("rotate_left",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_right","Ctrl-R");
 	str=_("Rotate image to right")+TAB+key;
	xfiBindingsDict->insert("rotate_right",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","mirror_horizontally","Ctrl-H");
 	str=_("Mirror image horizontally")+TAB+key;
	xfiBindingsDict->insert("mirror_horizontally",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","mirror_vertically","Ctrl-V");
 	str=_("Mirror image vertically")+TAB+key;
	xfiBindingsDict->insert("mirror_vertically",str.text());


	// Key bindings specific to X File Write (Xfw)
	if (xfwBindingsDict==NULL)
		xfwBindingsDict=new FXStringDict();

	key=getApp()->reg().readStringEntry("KEYBINDINGS","new","Ctrl-N");
 	str=_("Create new document")+TAB+key;
	xfwBindingsDict->insert("new",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","save","Ctrl-S");
 	str=_("Save changes to file")+TAB+key;
	xfwBindingsDict->insert("save",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","goto_line","Ctrl-L");
 	str=_("Goto line")+TAB+key;
	xfwBindingsDict->insert("goto_line",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","undo","Ctrl-Z");
 	str=_("Undo last change")+TAB+key;
	xfwBindingsDict->insert("undo",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","redo","Ctrl-Y");
 	str=_("Redo last change")+TAB+key;
	xfwBindingsDict->insert("redo",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","replace","Ctrl-R");
 	str=_("Replace string")+TAB+key;
	xfwBindingsDict->insert("replace",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","word_wrap","Ctrl-K");
 	str=_("Toggle word wrap mode")+TAB+key;
	xfwBindingsDict->insert("word_wrap",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","line_numbers","Ctrl-T");
 	str=_("Toggle line numbers mode")+TAB+key;
	xfwBindingsDict->insert("line_numbers",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","lower_case","Ctrl-U");
 	str=_("Toggle lower case mode")+TAB+key;
	xfwBindingsDict->insert("lower_case",str.text());

	key=getApp()->reg().readStringEntry("KEYBINDINGS","upper_case","Shift-Ctrl-U");
 	str=_("Toggle upper case mode")+TAB+key;
	xfwBindingsDict->insert("upper_case",str.text());

	// Display the key bindings dialog box
	if (bindingsbox==NULL)
		bindingsbox=new KeybindingsBox(this,glbBindingsDict,xfeBindingsDict,xfiBindingsDict,xfwBindingsDict);	
    
	bindingsbox->execute(PLACEMENT_OWNER);
	
    return 1;
}


// Restore default key bindings
long PreferencesBox::onCmdRestoreKeyBindings(FXObject*,FXSelector,void*)
{
    // Confirmation message
	FXString message=_("Do you really want to restore the default key bindings?\n\nAll your customizations will be lost!");
    MessageBox box(this,_("Restore default key bindings"),message,keybindingsicon,BOX_OK_CANCEL|DECOR_TITLE|DECOR_BORDER);
 	if(box.execute(PLACEMENT_CURSOR) != BOX_CLICKED_OK)
    	return 0;

	// Write default key bindings to the registry
	
	// Global key bindings
	getApp()->reg().writeStringEntry("KEYBINDINGS","go_back","Ctrl-Backspace");
	getApp()->reg().writeStringEntry("KEYBINDINGS","go_forward","Shift-Backspace");
	getApp()->reg().writeStringEntry("KEYBINDINGS","go_up","Backspace");
	getApp()->reg().writeStringEntry("KEYBINDINGS","go_home","Ctrl-H");
	getApp()->reg().writeStringEntry("KEYBINDINGS","new_file","F2");
	getApp()->reg().writeStringEntry("KEYBINDINGS","new_folder","F7");
	getApp()->reg().writeStringEntry("KEYBINDINGS","copy","Ctrl-C");
	getApp()->reg().writeStringEntry("KEYBINDINGS","cut","Ctrl-X");
	getApp()->reg().writeStringEntry("KEYBINDINGS","paste","Ctrl-V");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","big_icons","F10");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","small_icons","F11");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","detailed_file_list","F12");
	getApp()->reg().writeStringEntry("KEYBINDINGS","open","Ctrl-O");
	getApp()->reg().writeStringEntry("KEYBINDINGS","quit","Ctrl-Q");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","select_all","Ctrl-A");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","deselect_all","Ctrl-Z");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","invert_selection","Ctrl-I");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","help","F1");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","hidden_files","Ctrl-F6");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","thumbnails","Ctrl-F7");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","go_work","Shift-F2");
	getApp()->reg().writeStringEntry("KEYBINDINGS","close","Ctrl-W");
	getApp()->reg().writeStringEntry("KEYBINDINGS","print","Ctrl-P");
	getApp()->reg().writeStringEntry("KEYBINDINGS","search","Ctrl-F");
	getApp()->reg().writeStringEntry("KEYBINDINGS","search_prev","Shift-Ctrl-G");
	getApp()->reg().writeStringEntry("KEYBINDINGS","search_next","Ctrl-G");
	
	// Key bindings specific to X File Explorer (Xfe)
	getApp()->reg().writeStringEntry("KEYBINDINGS","refresh","Ctrl-R");
	getApp()->reg().writeStringEntry("KEYBINDINGS","new_symlink","Ctrl-J");
	getApp()->reg().writeStringEntry("KEYBINDINGS","properties","F9");
	getApp()->reg().writeStringEntry("KEYBINDINGS","move_to_trash","Del");
	getApp()->reg().writeStringEntry("KEYBINDINGS","restore_from_trash","Alt-Del");
	getApp()->reg().writeStringEntry("KEYBINDINGS","delete","Shift-Del");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","new_window","F3");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","new_root_window","Shift-F3");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","execute_command","Ctrl-E");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","terminal","Ctrl-T");
#if defined(linux)
  	getApp()->reg().writeStringEntry("KEYBINDINGS","mount","Ctrl-M");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","unmount","Ctrl-U");
#endif
  	getApp()->reg().writeStringEntry("KEYBINDINGS","one_panel","Ctrl-F1");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","tree_panel","Ctrl-F2");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","two_panels","Ctrl-F3");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","tree_two_panels","Ctrl-F4");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","clear_location","Ctrl-L");
	getApp()->reg().writeStringEntry("KEYBINDINGS","rename","Ctrl-N");
	getApp()->reg().writeStringEntry("KEYBINDINGS","copy_to","F5");
	getApp()->reg().writeStringEntry("KEYBINDINGS","move_to","F6");
	getApp()->reg().writeStringEntry("KEYBINDINGS","symlink_to","Ctrl-S");
  	getApp()->reg().writeStringEntry("KEYBINDINGS","add_bookmark","Ctrl-B");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","synchronize_panels","Ctrl-Y");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","switch_panels","Ctrl-K");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","go_to_trash","Ctrl-F8");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","empty_trash_can","Ctrl-Del");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","view","Shift-F4");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","edit","F4");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","hidden_dirs","Ctrl-F5");
 	getApp()->reg().writeStringEntry("KEYBINDINGS","filter","Ctrl-D");

	// Key bindings specific to X File Image (Xfi)
	getApp()->reg().writeStringEntry("KEYBINDINGS","zoom_100","Ctrl-I");
	getApp()->reg().writeStringEntry("KEYBINDINGS","zoom_win","Ctrl-F");
	getApp()->reg().writeStringEntry("KEYBINDINGS","rotate_left","Ctrl-L");
	getApp()->reg().writeStringEntry("KEYBINDINGS","rotate_right","Ctrl-R");
	getApp()->reg().writeStringEntry("KEYBINDINGS","mirror_horizontally","Ctrl-H");
	getApp()->reg().writeStringEntry("KEYBINDINGS","mirror_vertically","Ctrl-V");

	// Key bindings specific to X File Write (Xfw)
	getApp()->reg().writeStringEntry("KEYBINDINGS","new","Ctrl-N");
	getApp()->reg().writeStringEntry("KEYBINDINGS","save","Ctrl-S");
	getApp()->reg().writeStringEntry("KEYBINDINGS","goto_line","Ctrl-L");
	getApp()->reg().writeStringEntry("KEYBINDINGS","undo","Ctrl-Z");
	getApp()->reg().writeStringEntry("KEYBINDINGS","redo","Ctrl-Y");
	getApp()->reg().writeStringEntry("KEYBINDINGS","replace","Ctrl-R");
	getApp()->reg().writeStringEntry("KEYBINDINGS","word_wrap","Ctrl-K");
	getApp()->reg().writeStringEntry("KEYBINDINGS","line_numbers","Ctrl-T");
	getApp()->reg().writeStringEntry("KEYBINDINGS","lower_case","Ctrl-U");
	getApp()->reg().writeStringEntry("KEYBINDINGS","upper_case","Shift-Ctrl-U");

	// Finally, update the registry
	getApp()->reg().write();

	// Ask the user if he wants to restart Xfe
	if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Key bindings will be changed after restart.\nRestart X File Explorer now?")))
		mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);

    return 1;
}


long PreferencesBox::onCmdAccept(FXObject* o,FXSelector s,void* p)
{
    FXbool restart_theme=FALSE;
    FXbool restart_scroll=FALSE;
    FXbool restart_pathlink=FALSE;
    FXbool restart_controls=FALSE;
    FXbool restart_normalfont=FALSE;
    FXbool restart_textfont=FALSE;
	
	if (iconpath->getText()=="")
		iconpath->setText(oldiconpath);

	// Icon path has changed
	if(oldiconpath != iconpath->getText())
	{
		getApp()->reg().writeStringEntry("SETTINGS","iconpath",iconpath->getText().text());
 		getApp()->reg().write();
		restart_theme=TRUE;
	}
	
	// Normal font has changed
	if (oldnormalfont != normalfont->getText())
	{
		getApp()->reg().writeStringEntry("SETTINGS","font",normalfont->getText().text());
		getApp()->reg().write();
		restart_normalfont=TRUE;
	}

	// Text font has changed
	if (oldtextfont != textfont->getText())
	{
		getApp()->reg().writeStringEntry("SETTINGS","textfont",textfont->getText().text());
		getApp()->reg().write();
		restart_textfont=TRUE;
	}

    // Text editor has changed
	if(oldtxteditor != txteditor->getText())
	{
        // Update the txteditor string
		FXString newtxteditor=txteditor->getText().text();
		getApp()->reg().writeStringEntry("PROGS","txteditor",newtxteditor.text());
		
		// Update each filetype where the old txteditor was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old txteditor string with the new one
			if (value.contains(oldtxteditor))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace only the open and edit strings, if needed
				if (open==oldtxteditor)
					open=newtxteditor;
				if (edit==oldtxteditor)
					edit=newtxteditor;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // Text viewer has changed
	if(oldtxtviewer != txtviewer->getText())
	{
        // Update the txtviewer string
		FXString newtxtviewer=txtviewer->getText().text();
		getApp()->reg().writeStringEntry("PROGS","txtviewer",newtxtviewer.text());
		
		// Update each filetype where the old txtviewer was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old txtviewer string with the new one
			if (value.contains(oldtxtviewer))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace only the view string, if needed
				if (view==oldtxtviewer)
					view=newtxtviewer;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // Image editor has changed
	if(oldimgeditor != imgeditor->getText())
	{
        // Update the imgeditor string
		FXString newimgeditor=imgeditor->getText().text();
		getApp()->reg().writeStringEntry("PROGS","imgeditor",newimgeditor.text());
		
		// Update each filetype where the old imgeditor was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old imgeditor string with the new one
			if (value.contains(oldimgeditor))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace only the open and edit strings, if needed
				if (open==oldimgeditor)
					open=newimgeditor;
				if (edit==oldimgeditor)
					edit=newimgeditor;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // Image viewer has changed
	if(oldimgviewer != imgviewer->getText())
	{
        // Update the imgviewer string
		FXString newimgviewer=imgviewer->getText().text();
		getApp()->reg().writeStringEntry("PROGS","imgviewer",newimgviewer.text());
		
		// Update each filetype where the old imgviewer was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old imgviewer string with the new one
			if (value.contains(oldimgviewer))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace only the view string, if needed
				if (view==oldimgviewer)
					view=newimgviewer;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // Archiver has changed
	if(oldarchiver != archiver->getText())
	{
        // Update the archiver string
		FXString newarchiver=archiver->getText().text();
		getApp()->reg().writeStringEntry("PROGS","archiver",newarchiver.text());
		
		// Update each filetype where the old archiver was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old archiver string with the new one
			if (value.contains(oldarchiver))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace the open, view and edit strings, if needed
				if (open==oldarchiver)
					open=newarchiver;
				if (view==oldarchiver)
					view=newarchiver;
				if (edit==oldarchiver)
					edit=newarchiver;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // PDF viewer has changed
	if(oldpdfviewer != pdfviewer->getText())
	{
        // Update the PDF viewer string
		FXString newpdfviewer=pdfviewer->getText().text();
		getApp()->reg().writeStringEntry("PROGS","pdfviewer",newpdfviewer.text());
		
		// Update each filetype where the old PDF viewer was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old PDF viewer string with the new one
			if (value.contains(oldpdfviewer))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace the open, view and edit strings, if needed
				if (open==oldpdfviewer)
					open=newpdfviewer;
				if (view==oldpdfviewer)
					view=newpdfviewer;
				if (edit==oldpdfviewer)
					edit=newpdfviewer;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // Audio viewer has changed
	if(oldaudioviewer != audioviewer->getText())
	{
        // Update the audio viewer string
		FXString newaudioviewer=audioviewer->getText().text();
		getApp()->reg().writeStringEntry("PROGS","audioviewer",newaudioviewer.text());
		
		// Update each filetype where the old audio viewer was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old audio viewer string with the new one
			if (value.contains(oldaudioviewer))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace the open, view and edit strings, if needed
				if (open==oldaudioviewer)
					open=newaudioviewer;
				if (view==oldaudioviewer)
					view=newaudioviewer;
				if (edit==oldaudioviewer)
					edit=newaudioviewer;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

    // Video viewer has changed
	if(oldvideoviewer != videoviewer->getText())
	{
        // Update the video viewer string
		FXString newvideoviewer=videoviewer->getText().text();
		getApp()->reg().writeStringEntry("PROGS","videoviewer",newvideoviewer.text());
		
		// Update each filetype where the old video viewer was used
		FXStringDict *strdict = getApp()->reg().find("FILETYPES");
		FileDict *assoc=new FileDict(getApp());

		int i;
		FXString key, value, newvalue;
		FXString strtmp, open, view, edit, command;
		for (i=strdict->first(); i<strdict->size(); i=strdict->next(i))
		{
			// Read key and value of each filetype
			key=strdict->key(i);
			value=strdict->data(i);
			
			// Replace the old video viewer string with the new one
			if (value.contains(oldvideoviewer))
			{
				// Obtain the open, view, edit and command strings
				strtmp=value.before(';',1);
				command=value.after(';',1);
				open=strtmp.section(',',0);
				view=strtmp.section(',',1);
				edit=strtmp.section(',',2);
				
				// Replace the open, view and edit strings, if needed
				if (open==oldvideoviewer)
					open=newvideoviewer;
				if (view==oldvideoviewer)
					view=newvideoviewer;
				if (edit==oldvideoviewer)
					edit=newvideoviewer;
				
				// Replace with the new value
				value=open + "," + view + "," + edit + ";" + command;
				assoc->replace(key.text(),value.text());
			}
		}
	}

	// Terminal has changed
	if(oldxterm != xterm->getText())
        getApp()->reg().writeStringEntry("PROGS","xterm",xterm->getText().text());

	getApp()->reg().writeUnsignedEntry("OPTIONS","auto_save_layout",autosave->getCheck());
	getApp()->reg().writeUnsignedEntry("SETTINGS","save_win_pos",savewinpos->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","use_trash_can",trashcan->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","use_trash_bypass",trashbypass->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","ask_before_copy",ask->getCheck());
    getApp()->reg().writeUnsignedEntry("SETTINGS","single_click",single_click);
	getApp()->reg().writeStringEntry("SETTINGS","time_format",timeformat->getText().text());
    getApp()->reg().writeUnsignedEntry("OPTIONS","confirm_trash",trashmv->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","confirm_delete",del->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","confirm_delete_emptydir",del_emptydir->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","confirm_overwrite",overwrite->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","confirm_execute",exec->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","confirm_drag_and_drop",dnd->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","preserve_date_warn",preserve_date_warning->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","root_warn",root_warning->getCheck());
    getApp()->reg().writeUnsignedEntry("OPTIONS","root_mode",rootmode->getCheck());
#ifdef STARTUP_NOTIFICATION
    getApp()->reg().writeUnsignedEntry("OPTIONS","use_startup_notification",usesn->getCheck());
#endif
#if defined(linux)
    getApp()->reg().writeUnsignedEntry("OPTIONS","mount_warn",mount->getCheck());
	getApp()->reg().writeUnsignedEntry("OPTIONS","mount_messages",show_mount->getCheck());
#endif

	// Smooth scrolling
	getApp()->reg().writeUnsignedEntry("SETTINGS","smooth_scroll",scroll->getCheck());
	if (scroll->getCheck()!=smoothscroll_prev)
	{
 		getApp()->reg().write();
		restart_scroll=TRUE;
	}
		
    // Control themes
    getApp()->reg().writeUnsignedEntry("SETTINGS","use_clearlooks",use_clearlooks);

	if (use_clearlooks!=use_clearlooks_prev)
	{
		FXColor hilitecolor, shadowcolor;

		// Change control hilite and shadow colors when the control theme has changed
		if (use_clearlooks) // clearlooks
		{
			hilitecolor=makeHiliteColorGradient(currTheme.color[0]);
			shadowcolor=makeShadowColorGradient(currTheme.color[0]);
		}
		else // standard
		{
			hilitecolor=makeHiliteColor(currTheme.color[0]);
			shadowcolor=makeShadowColor(currTheme.color[0]);
		}
		getApp()->reg().writeColorEntry("SETTINGS","hilitecolor",hilitecolor);
		getApp()->reg().writeColorEntry("SETTINGS","shadowcolor",shadowcolor);
 
 		getApp()->reg().write();
		restart_controls=TRUE;
	}

	// Update some global options
	if (diropen->getCheck() && fileopen->getCheck())
		single_click=SINGLE_CLICK_DIR_FILE;
	else if (diropen->getCheck() && !fileopen->getCheck())
		single_click=SINGLE_CLICK_DIR;
	else
		single_click=SINGLE_CLICK_NONE;
	
	if (single_click==SINGLE_CLICK_DIR_FILE)
		((XFileExplorer*) mainWindow)->setDefaultCursor(getApp()->getDefaultCursor(DEF_HAND_CURSOR));
	else
		((XFileExplorer*) mainWindow)->setDefaultCursor(getApp()->getDefaultCursor(DEF_ARROW_CURSOR));
   
	// Update the file tooltips flag
	if (filetooltips->getCheck())
		file_tooltips=TRUE;
	else
		file_tooltips=FALSE;
	getApp()->reg().writeUnsignedEntry("SETTINGS","file_tooltips",(unsigned int)file_tooltips);

	// Update the relative resize flag
	if (relativeresize->getCheck())
		relative_resize=TRUE;
	else
		relative_resize=FALSE;
	getApp()->reg().writeUnsignedEntry("SETTINGS","relative_resize",(unsigned int)relative_resize);

	// Update the display path linker flag
	show_pathlink=showpathlink->getCheck();
	getApp()->reg().writeUnsignedEntry("SETTINGS","show_pathlinker",show_pathlink);
	if (show_pathlink!=show_pathlink_prev)
	{
 		getApp()->reg().write();
		restart_pathlink=TRUE;
	}

	// Theme has changed
	if(currTheme != Themes[0])
    {
		getApp()->reg().writeColorEntry("SETTINGS","basecolor",currTheme.color[0]);
        getApp()->reg().writeColorEntry("SETTINGS","bordercolor",currTheme.color[1]);
        getApp()->reg().writeColorEntry("SETTINGS","backcolor",currTheme.color[2]);
        getApp()->reg().writeColorEntry("SETTINGS","forecolor",currTheme.color[3]);
        getApp()->reg().writeColorEntry("SETTINGS","selbackcolor",currTheme.color[4]);
        getApp()->reg().writeColorEntry("SETTINGS","selforecolor",currTheme.color[5]);
		getApp()->reg().writeColorEntry("SETTINGS","listbackcolor",currTheme.color[6]);
		getApp()->reg().writeColorEntry("SETTINGS","listforecolor",currTheme.color[7]);
		getApp()->reg().writeColorEntry("SETTINGS","highlightcolor",currTheme.color[8]);
		getApp()->reg().writeColorEntry("SETTINGS","pbarcolor",currTheme.color[9]);
		getApp()->reg().writeColorEntry("SETTINGS","attentioncolor",currTheme.color[10]);
		getApp()->reg().writeColorEntry("SETTINGS","scrollbarcolor",currTheme.color[11]);

    	// Control themes
		FXColor hilitecolor, shadowcolor;

		// Change control hilite and shadow colors when the control theme has changed
		if (use_clearlooks) // clearlooks
		{
			hilitecolor=makeHiliteColorGradient(currTheme.color[0]);
			shadowcolor=makeShadowColorGradient(currTheme.color[0]);
		}
		else // standard
		{
			hilitecolor=makeHiliteColor(currTheme.color[0]);
			shadowcolor=makeShadowColor(currTheme.color[0]);
		}
		getApp()->reg().writeColorEntry("SETTINGS","hilitecolor",hilitecolor);
		getApp()->reg().writeColorEntry("SETTINGS","shadowcolor",shadowcolor);

		getApp()->reg().write();
        restart_theme=TRUE;
    }
	
	// Restart application if necessary
	if (restart_scroll)
	{
		if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Scrolling mode will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}
	if (restart_theme)
	{
		if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Theme will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}
	
	if (restart_pathlink)
	{
		if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Path linker will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}

	if (restart_controls)
	{
		if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Button style will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}

	if (restart_normalfont)
	{
        if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Normal font will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}

	if (restart_textfont)
	{
        if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Text font will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}

	// Finally, update the registry
	getApp()->reg().write();
		
	// Refresh panels
	mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_REFRESH),NULL);

    DialogBox::onCmdAccept(o,s,p);
    return 1;
}


long PreferencesBox::onCmdCancel(FXObject* o,FXSelector s,void* p)
{
	// Reset preferences to their previous values
	
	// First tab - Options
	trashcan->setCheck(trashcan_prev);
	trashbypass->setCheck(trashbypass_prev);
	autosave->setCheck(autosave_prev);
	savewinpos->setCheck(savewinpos_prev);
	diropen->setCheck(diropen_prev);
	fileopen->setCheck(fileopen_prev);
	filetooltips->setCheck(filetooltips_prev);
	relativeresize->setCheck(relativeresize_prev);
	showpathlink->setCheck(show_pathlink_prev);
	getApp()->setWheelLines(value_prev);	
	use_sudo=use_sudo_prev;
	getApp()->reg().writeUnsignedEntry("OPTIONS","use_sudo",use_sudo);
	scroll->setCheck(smoothscroll_prev);
	rootmode->setCheck(rootmode_prev);
	
	timeformat->setText(oldtimeformat);	

	
	
	
#ifdef STARTUP_NOTIFICATION
	usesn->setCheck(usesn_prev);
#endif
	
	// Second tab - Dialogs
	ask->setCheck(ask_prev);
	dnd->setCheck(dnd_prev);
	trashmv->setCheck(trashmv_prev);
	del->setCheck(del_prev);
	del_emptydir->setCheck(del_emptydir_prev);
	overwrite->setCheck(overwrite_prev);
	exec->setCheck(exec_prev);
#if defined(linux)
	mount->setCheck(mount_prev);
	show_mount->setCheck(show_mount_prev);
#endif
	preserve_date_warning->setCheck(preserve_date_warning_prev);
	root_warning->setCheck(root_warning_prev);
	
	// Third tab - Programs
	txteditor->setText(oldtxteditor);	
	txtviewer->setText(oldtxtviewer);	
	imgeditor->setText(oldimgeditor);	
	imgviewer->setText(oldimgviewer);	
	archiver->setText(oldarchiver);	
	pdfviewer->setText(oldpdfviewer);	
	audioviewer->setText(oldaudioviewer);	
	videoviewer->setText(oldvideoviewer);	
	xterm->setText(oldxterm);

	// Fourth tab - Visual
    themesList->setCurrentItem(themelist_prev);
	currTheme=currTheme_prev;
    iconpath->setText(oldiconpath);
	use_clearlooks=use_clearlooks_prev;
	getApp()->reg().writeUnsignedEntry("SETTINGS","use_clearlooks",use_clearlooks);

	// Fifth tab - Fonts
	normalfont->setText(oldnormalfont);
	textfont->setText(oldtextfont);

	// Finally, update the registry (really necessary?)
	getApp()->reg().write();
	
    DialogBox::onCmdCancel(o,s,p);
    return 1;
}


// Execute dialog box modally
unsigned int PreferencesBox::execute(unsigned int placement)
{
	// Save current preferences to restore them if cancel is pressed
	
	// First tab - Options
	trashcan_prev=trashcan->getCheck();
	trashbypass_prev=trashbypass->getCheck();
	autosave_prev=autosave->getCheck();
	savewinpos_prev=savewinpos->getCheck();
	diropen_prev=diropen->getCheck();
	fileopen_prev=fileopen->getCheck();
	filetooltips_prev=filetooltips->getCheck();
	relativeresize_prev=relativeresize->getCheck();
	show_pathlink_prev=showpathlink->getCheck();
	value_prev=getApp()->getWheelLines();
	use_sudo_prev=use_sudo;
	smoothscroll_prev=scroll->getCheck();
	rootmode_prev=rootmode->getCheck();
#ifdef STARTUP_NOTIFICATION
	usesn_prev=usesn->getCheck();
#endif
	
	// Second tab - Dialogs
	ask_prev=ask->getCheck();
	dnd_prev=dnd->getCheck();
	trashmv_prev=trashmv->getCheck();
	del_prev=del->getCheck();
	del_emptydir_prev=del_emptydir->getCheck();
	overwrite_prev=overwrite->getCheck();
	exec_prev=exec->getCheck();
#if defined(linux)
	mount_prev=mount->getCheck();
	show_mount_prev=show_mount->getCheck();
#endif
	preserve_date_warning_prev=preserve_date_warning->getCheck();
	root_warning_prev=root_warning->getCheck();

    // Third tab - Programs
	oldtxteditor=txteditor->getText();
	oldtxtviewer=txtviewer->getText();
	oldimgeditor=imgeditor->getText();
	oldimgviewer=imgviewer->getText();
	oldarchiver=archiver->getText();
	oldpdfviewer=pdfviewer->getText();
	oldaudioviewer=audioviewer->getText();
	oldvideoviewer=videoviewer->getText();
	oldxterm=xterm->getText();
	
	// Fourth tab - Visual
	themelist_prev=themesList->getCurrentItem();
    currTheme_prev = currTheme;
    oldiconpath=iconpath->getText();
	use_clearlooks_prev=use_clearlooks;
	
	// Fifth tab - Fonts
	oldnormalfont=normalfont->getText();
	oldtextfont=textfont->getText();

	create();
    show(placement);
	getApp()->refresh();
    return getApp()->runModalFor(this);
}


// Update buttons related to the trash can option item
long PreferencesBox::onUpdTrash(FXObject* o,FXSelector,void*)
{
	if (trashcan->getCheck())	
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
    return 1;
}


// Update the confirm delete empty directories option item
long PreferencesBox::onUpdConfirmDelEmptyDir(FXObject* o,FXSelector,void*)
{
	if (del->getCheck())	
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
    return 1;
}


// Set root mode
long PreferencesBox::onCmdSuMode(FXObject*,FXSelector sel,void*)
{
	if (FXSELID(sel)==ID_SU_CMD)
		use_sudo=FALSE;
		
	else if (FXSELID(sel)==ID_SUDO_CMD)
		use_sudo=TRUE;

	getApp()->reg().writeUnsignedEntry("OPTIONS","use_sudo",use_sudo);
	getApp()->reg().write();

	return 1;
}


// Update root mode radio button
long PreferencesBox::onUpdSuMode(FXObject* sender,FXSelector sel,void*)
{
	if (!rootmode->getCheck())
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
	else
	{
		if (getuid()) // Simple user
			sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
		
		FXSelector updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);
		
		if (FXSELID(sel)==ID_SU_CMD)
		{
			if (use_sudo)
				updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);
			else
				updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
		}
		else if (FXSELID(sel)==ID_SUDO_CMD)
		{
			if (use_sudo)
				updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
			else
				updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);

		}		
		sender->handle(this,updatemessage,NULL);
	}
	return 1;
}


// Set root mode
long PreferencesBox::onCmdControls(FXObject*,FXSelector sel,void*)
{
	if (FXSELID(sel)==ID_STANDARD_CONTROLS)
		use_clearlooks=FALSE;
		
	else if (FXSELID(sel)==ID_CLEARLOOKS_CONTROLS)
		use_clearlooks=TRUE;

	getApp()->reg().writeUnsignedEntry("SETTINGS","use_clearlooks",use_clearlooks);
	getApp()->reg().write();

	return 1;
}


// Update root mode radio button
long PreferencesBox::onUpdControls(FXObject* sender,FXSelector sel,void*)
{
	FXSelector updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);
	
	if (FXSELID(sel)==ID_STANDARD_CONTROLS)
	{
		if (use_clearlooks)
			updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);
		else
			updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
	}
	else if (FXSELID(sel)==ID_CLEARLOOKS_CONTROLS)
	{
		if (use_clearlooks)
			updatemessage=FXSEL(SEL_COMMAND,ID_CHECK);
		else
			updatemessage=FXSEL(SEL_COMMAND,ID_UNCHECK);

	}		
	sender->handle(this,updatemessage,NULL);
	return 1;
}



// Set scroll wheel lines (Mathew Robertson <mathew@optushome.com.au>)
long PreferencesBox::onCmdWheelAdjust(FXObject* sender,FXSelector,void*)
{
    unsigned int value;
    sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&value);
    getApp()->setWheelLines(value);
	getApp()->reg().write();
    return 1;
}


// Update the wheel lines button
long PreferencesBox::onUpdWheelAdjust(FXObject* sender,FXSelector,void*)
{
    unsigned int value=getApp()->getWheelLines();
	sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&value);
    return 1;
}


// Update single click file open button
long PreferencesBox::onUpdSingleClickFileopen(FXObject* o,FXSelector,void*)
{
	if (diropen->getCheck())	
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
	else
	{
		fileopen->setCheck(FALSE);
		o->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
	}
    return 1;
}

