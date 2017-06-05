// This code is adapted from 'imageviewer', a demo image viewer found
// in the FOX library and written by Jeroen van der Zijp.

#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include <fx.h>
#include <fxkeys.h>
#include <FXPNGImage.h>
#include <FXJPGImage.h>
#include <FXTIFImage.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"
#include "xfeutils.h"
#include "startupnotification.h"
#include "FileDialog.h"
#include "InputDialog.h"
#include "DirHistBox.h"
#include "MessageBox.h"
#include "FileList.h"
#include "XFileImage.h"

// Add FOX hacks
#include "foxhacks.cpp"

char **args;
FXColor listbackcolor, listforecolor;
FXColor highlightcolor;
FXbool allowPopupScroll=FALSE;
unsigned int single_click;
FXbool file_tooltips;
FXbool relative_resize;
FXbool show_pathlink;
FXbool save_win_pos;
FXString homedir;
FXString xdgconfighome;
FXString xdgdatahome;


// Hand cursor replacement
#define hand_width 32
#define hand_height 32
#define hand_x_hot 6
#define hand_y_hot 1
 static const unsigned char hand_bits[] = {
	0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x90, 0x00, 0x00, 0x00,
	0x90, 0x00, 0x00, 0x00, 0x90, 0x00, 0x00, 0x00, 0x90, 0x07, 0x00, 0x00,
	0x97, 0x1a, 0x00, 0x00, 0x99, 0x2a, 0x00, 0x00, 0x11, 0x28, 0x00, 0x00,
	0x12, 0x20, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00,
	0x04, 0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00,
	0xf0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
 
 static const unsigned char hand_mask_bits[] = {
	0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00,
	0xf0, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x00,
	0xf7, 0x1f, 0x00, 0x00, 0xff, 0x3f, 0x00, 0x00, 0xff, 0x3f, 0x00, 0x00,
	0xfe, 0x3f, 0x00, 0x00, 0xfe, 0x3f, 0x00, 0x00, 0xfe, 0x3f, 0x00, 0x00,
	0xfc, 0x3f, 0x00, 0x00, 0xfc, 0x1f, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00,
	0xf0, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
   

// Predefined zoom factors 
#define NB_ZOOM 24
double zoomtab[NB_ZOOM]={0.01, 0.025, 0.05, 0.075, 0.10, 0.15, 0.20, 0.30, 0.50, 0.75, 1, \
                        1.5, 2, 3, 4, 5, 7.5, 10, 15, 20, 30, 50, 75, 100};
#define ZOOM_100 10

// Maximum image size (in pixels) for zooming in
#define MAX_IMGSIZE 5120

// Patterns for supported image formats
const char *patterns[]=
{
    _("All Files"),		"*",
    _("GIF Image"),		"*.gif",
    _("BMP Image"),		"*.bmp",
    _("XPM Image"),		"*.xpm",
    _("PCX Image"),		"*.pcx",
    _("ICO Image"),		"*.ico",
    _("RGB Image"),		"*.rgb",
    _("XBM Image"),		"*.xbm",
    _("TARGA Image"), 	"*.tga",
    _("PPM Image"),		"*.ppm",
    _("PNG Image"),		"*.png",
    _("JPEG Image"),	"*.jpg",
    _("JPEG Image"),	"*.jpeg",
    _("TIFF Image"),	"*.tif",
    _("TIFF Image"),	"*.tiff",
	NULL
};


const FXString imgpatterns="*.gif,*.bmp,*.xpm,*.pcx,*.ico,*.rgb,*.xbm,*.tga,*.ppm,*.png,*.jpg,*.jpeg,*.tif,*.tiff";


// Helper function to draw a toolbar separator
void toolbarSeparator(FXToolBar* tb)
{
#define SEP_SPACE	1
#define SEP_HEIGHT	15
    new FXFrame(tb,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,SEP_SPACE);
    new FXVerticalSeparator(tb,LAYOUT_SIDE_TOP|LAYOUT_CENTER_Y|SEPARATOR_GROOVE|LAYOUT_FIX_HEIGHT,0,0,0,SEP_HEIGHT);
    new FXFrame(tb,LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FIX_WIDTH|LAYOUT_FIX_HEIGHT,0,0,SEP_SPACE);
}

// Map
FXDEFMAP(XFileImage) XFileImageMap[]=
{
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ABOUT,XFileImage::onCmdAbout),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_OPEN,XFileImage::onCmdOpen),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_TITLE,XFileImage::onUpdTitle),
	FXMAPFUNC(SEL_SIGNAL,XFileImage::ID_HARVEST,XFileImage::onSigHarvest),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_QUIT,XFileImage::onCmdQuit),
	FXMAPFUNC(SEL_SIGNAL,XFileImage::ID_QUIT,XFileImage::onCmdQuit),
	FXMAPFUNC(SEL_CLOSE,XFileImage::ID_TITLE,XFileImage::onCmdQuit),
	FXMAPFUNC(SEL_DOUBLECLICKED,XFileImage::ID_FILELIST,XFileImage::onCmdItemDoubleClicked),
	FXMAPFUNC(SEL_CLICKED,XFileImage::ID_FILELIST,XFileImage::onCmdItemClicked),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_RECENTFILE,XFileImage::onCmdRecentFile),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_TOGGLE_HIDDEN,XFileImage::onCmdToggleHidden),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_TOGGLE_THUMBNAILS,XFileImage::onCmdToggleThumbnails),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_SHOW_DETAILS,XFileImage::onCmdShowDetails),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_SHOW_MINI,XFileImage::onCmdShowMini),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_SHOW_BIG,XFileImage::onCmdShowBig),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ROTATE_90,XFileImage::onCmdRotate),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ROTATE_270,XFileImage::onCmdRotate),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_MIRROR_HOR,XFileImage::onCmdMirror),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_MIRROR_VER,XFileImage::onCmdMirror),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ZOOM_IN,XFileImage::onCmdZoomIn),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ZOOM_OUT,XFileImage::onCmdZoomOut),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ZOOM_100,XFileImage::onCmdZoom100),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_ZOOM_WIN,XFileImage::onCmdZoomWin),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_PRINT,XFileImage::onCmdPrint),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_DIR_UP,XFileImage::onCmdDirUp),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_DIR_UP,XFileImage::onUpdDirUp),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_DIR_BACK,XFileImage::onCmdDirBack),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_DIR_BACK,XFileImage::onUpdDirBack),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_DIR_FORWARD,XFileImage::onCmdDirForward),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_DIR_FORWARD,XFileImage::onUpdDirForward),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_DIR_BACK_HIST,XFileImage::onCmdDirBackHist),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_DIR_BACK_HIST,XFileImage::onUpdDirBackHist),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_DIR_FORWARD_HIST,XFileImage::onCmdDirForwardHist),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_DIR_FORWARD_HIST,XFileImage::onUpdDirForwardHist),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_HOME,XFileImage::onCmdHome),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_PRINT,XFileImage::onUpdImage),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_ROTATE_90,XFileImage::onUpdImage),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_ROTATE_270,XFileImage::onUpdImage),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_MIRROR_HOR,XFileImage::onUpdImage),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_MIRROR_VER,XFileImage::onUpdImage),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_ZOOM_IN,XFileImage::onUpdImage),                                        
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_ZOOM_OUT,XFileImage::onUpdImage),                                        
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_ZOOM_100,XFileImage::onUpdImage),                                        
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_ZOOM_WIN,XFileImage::onUpdImage),                                        
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_SHOW_BIG,XFileImage::onUpdFileView),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_SHOW_MINI,XFileImage::onUpdFileView),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_SHOW_DETAILS,XFileImage::onUpdFileView),                                        
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_TOGGLE_HIDDEN,XFileImage::onUpdToggleHidden),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_TOGGLE_THUMBNAILS,XFileImage::onUpdToggleThumbnails),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_TOGGLE_FIT_WIN,XFileImage::onCmdToggleFitWin),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_TOGGLE_FILTER_IMAGES,XFileImage::onCmdToggleFilterImages),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_TOGGLE_FIT_WIN,XFileImage::onUpdToggleFitWin),
	FXMAPFUNC(SEL_UPDATE,XFileImage::ID_TOGGLE_FILTER_IMAGES,XFileImage::onUpdToggleFilterImages),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_HOME,XFileImage::onCmdHome),
	FXMAPFUNC(SEL_COMMAND,XFileImage::ID_WORK,XFileImage::onCmdWork),
	FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,XFileImage::ID_FILELIST,XFileImage::onCmdPopupMenu),
};


// Object implementation
FXIMPLEMENT(XFileImage,FXMainWindow,XFileImageMap,ARRAYNUMBER(XFileImageMap))


// Make some windows
XFileImage::XFileImage(FXApp* a, FXbool smoothscroll):FXMainWindow(a,"Xfi ",NULL,NULL,DECOR_ALL)
{
    setIcon(xfiicon);

    FXButton* btn=NULL;
	FXHotKey hotkey;
	FXString key;

    setTarget(this);
    setSelector(ID_TITLE);
	
    // Make menu bar
    menubar=new FXMenuBar(this,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED);

	// Sites where to dock
	FXDockSite* topdock=new FXDockSite(this,LAYOUT_SIDE_TOP|LAYOUT_FILL_X);
	new FXDockSite(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
	new FXDockSite(this,LAYOUT_SIDE_LEFT|LAYOUT_FILL_Y);
	new FXDockSite(this,LAYOUT_SIDE_RIGHT|LAYOUT_FILL_Y);

    // Tool bar
  	FXToolBarShell* dragshell1=new FXToolBarShell(this,FRAME_RAISED);
  	toolbar=new FXToolBar(topdock,dragshell1,LAYOUT_DOCK_NEXT|LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_RAISED);
  	new FXToolBarGrip(toolbar,toolbar,FXToolBar::ID_TOOLBARGRIP,TOOLBARGRIP_DOUBLE);

    // File menu
    filemenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&File"),NULL,filemenu);

    // Image Menu
    imagemenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Image"),NULL,imagemenu);

    // Preferences Menu
    prefsmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Preferences"),NULL,prefsmenu);

    // View menu
    viewmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&View"),NULL,viewmenu);

    // Help menu
    helpmenu=new FXMenuPane(this);
    new FXMenuTitle(menubar,_("&Help"),NULL,helpmenu);

    // Splitter
	FXVerticalFrame* splitterbox=new FXVerticalFrame(this,LAYOUT_FILL_X|LAYOUT_FILL_Y,0,0,0,0, 0,0,0,0, 0,0);
    splitter=new FXSplitter(splitterbox,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|LAYOUT_FILL_Y|SPLITTER_TRACKING|SPLITTER_VERTICAL|SPLITTER_REVERSED);

    // Make image widget
    imageview=new FXImageView(splitter,NULL,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_SUNKEN);

    // Frame box for file list and status bar
    filebox=new FXVerticalFrame(splitter,LAYOUT_FILL_X|LAYOUT_FILL_Y|FRAME_NONE,0,0,0,0, 0,0,0,0, 0,0);

    // Container for the action buttons
    FXHorizontalFrame* buttons=new FXHorizontalFrame(filebox,LAYOUT_SIDE_TOP|LAYOUT_FILL_X|FRAME_RAISED,0,0,0,0, 5,5,5,5, 0,0);

    // Container for the path linker
    FXHorizontalFrame* pathframe=new FXHorizontalFrame(filebox,LAYOUT_FILL_X|FRAME_RAISED,0,0,0,0, 0,0,0,0, 0,0);

	// File list
	unsigned int options;
	if (smoothscroll)
		options=LAYOUT_FILL_X|LAYOUT_FILL_Y|_ICONLIST_MINI_ICONS|_ICONLIST_BROWSESELECT;
	else
		options=LAYOUT_FILL_X|LAYOUT_FILL_Y|_ICONLIST_MINI_ICONS|_ICONLIST_BROWSESELECT|SCROLLERS_DONT_TRACK;
    thumbnails=getApp()->reg().readUnsignedEntry("OPTIONS","thumbnails",0);
	filelist=new FileList(this,filebox,this,ID_FILELIST,thumbnails,options);
	filelist->setTextColor(listforecolor);
	filelist->setBackColor(listbackcolor);
	filelist->setHeaderSize(0,getApp()->reg().readUnsignedEntry("OPTIONS","name_size",200));
    filelist->setHeaderSize(1,getApp()->reg().readUnsignedEntry("OPTIONS","size_size",60));
    filelist->setHeaderSize(2,getApp()->reg().readUnsignedEntry("OPTIONS","type_size",100));
    filelist->setHeaderSize(3,getApp()->reg().readUnsignedEntry("OPTIONS","ext_size",100));
    filelist->setHeaderSize(4,getApp()->reg().readUnsignedEntry("OPTIONS","modd_size",150));
    filelist->setHeaderSize(5,getApp()->reg().readUnsignedEntry("OPTIONS","user_size",50));
    filelist->setHeaderSize(6,getApp()->reg().readUnsignedEntry("OPTIONS","grou_size",50));
    filelist->setHeaderSize(7,getApp()->reg().readUnsignedEntry("OPTIONS","attr_size",100));  	

    // Action buttons
    new FXFrame(buttons,LAYOUT_FIX_WIDTH,0,0,4,1);
    new FXButton(buttons,TAB+_("Go back")+TAB+_("Move to previous folder."),dirbackicon,this,ID_DIR_BACK,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	btnbackhist=new FXArrowButton(buttons,this,ID_DIR_BACK_HIST,LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);
    new FXButton(buttons,TAB+_("Go forward")+TAB+_("Move to next folder."),dirforwardicon,this,ID_DIR_FORWARD,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
	btnforwardhist=new FXArrowButton(buttons,this,ID_DIR_FORWARD_HIST,LAYOUT_FILL_Y|FRAME_RAISED|FRAME_THICK|ARROW_DOWN|ARROW_TOOLBAR);
    new FXButton(buttons,TAB+_("Go up one folder")+TAB+_("Move up to higher folder."),dirupicon,this,ID_DIR_UP,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
    new FXButton(buttons,TAB+_("Go to home folder")+TAB+_("Back to home folder."),homeicon,this,ID_HOME,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
    new FXButton(buttons,TAB+_("Go to working folder")+TAB+_("Back to working folder."),workicon,this,ID_WORK,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
    new FXButton(buttons,TAB+_("Show icons")+TAB+_("Display folder with big icons."),bigiconsicon,this,ID_SHOW_BIG,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
    new FXButton(buttons,TAB+_("Show list")+TAB+_("Display folder with small icons."),smalliconsicon,this,ID_SHOW_MINI,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);
    new FXButton(buttons,TAB+_("Show details")+TAB+_("Display detailed folder listing."),detailsicon,this,ID_SHOW_DETAILS,BUTTON_TOOLBAR|FRAME_RAISED,0,0,0,0, 3,3,3,3);

	// Panel title
	pathtext=new TextLabel(pathframe,0,this,0,LAYOUT_FILL_X|LAYOUT_FILL_Y);
	pathtext->setBackColor(getApp()->getBaseColor());

    // Path linker
    pathlink = new PathLinker(pathframe,filelist,NULL,LAYOUT_FILL_X);

	// Status bar
	statusbar=new FXHorizontalFrame(filebox,JUSTIFY_LEFT|LAYOUT_FILL_X|FRAME_RAISED,0,0,0,0, 3,3,0,0);
	
	// Read and set sort function for file list
	FXString sort_func=getApp()->reg().readStringEntry("OPTIONS","sort_func","ascendingCase");
    if (sort_func=="ascendingCase")
		filelist->setSortFunc(filelist->ascendingCase);        
    if (sort_func=="ascendingCaseMix")
		filelist->setSortFunc(filelist->ascendingCaseMix);        
    else if (sort_func=="descendingCase")
		filelist->setSortFunc(filelist->descendingCase);        
    else if (sort_func=="descendingCaseMix")
		filelist->setSortFunc(filelist->descendingCaseMix);        
    else if (sort_func=="ascending")
		filelist->setSortFunc(filelist->ascending);        
	else if (sort_func=="ascendingMix")
		filelist->setSortFunc(filelist->ascendingMix);        
    else if (sort_func=="descending")
		filelist->setSortFunc(filelist->descending);        
    else if (sort_func=="descendingMix")
		filelist->setSortFunc(filelist->descendingMix);        
	else if (sort_func=="ascendingSize")
		filelist->setSortFunc(filelist->ascendingSize);        
	else if (sort_func=="ascendingSizeMix")
		filelist->setSortFunc(filelist->ascendingSizeMix);        
	else if (sort_func=="descendingSize")
		filelist->setSortFunc(filelist->descendingSize);        
	else if (sort_func=="descendingSizeMix")
		filelist->setSortFunc(filelist->descendingSizeMix);        
	else if (sort_func=="ascendingType")
		filelist->setSortFunc(filelist->ascendingType);        
	else if (sort_func=="ascendingTypeMix")
		filelist->setSortFunc(filelist->ascendingTypeMix);        
	else if (sort_func=="descendingType")
		filelist->setSortFunc(filelist->descendingType);        
	else if (sort_func=="descendingTypeMix")
		filelist->setSortFunc(filelist->descendingTypeMix);        
	else if (sort_func=="ascendingExt")
		filelist->setSortFunc(filelist->ascendingExt);        
	else if (sort_func=="ascendingExtMix")
		filelist->setSortFunc(filelist->ascendingExtMix);        
	else if (sort_func=="descendingExt")
		filelist->setSortFunc(filelist->descendingExt);        
	else if (sort_func=="descendingExtMix")
		filelist->setSortFunc(filelist->descendingExtMix);        
	else if (sort_func=="ascendingTime")
		filelist->setSortFunc(filelist->ascendingTime);        
	else if (sort_func=="ascendingTimeMix")
		filelist->setSortFunc(filelist->ascendingTimeMix);        
	else if (sort_func=="descendingTime")
		filelist->setSortFunc(filelist->descendingTime);        
	else if (sort_func=="descendingTimeMix")
		filelist->setSortFunc(filelist->descendingTimeMix);        
	else if (sort_func=="ascendingUser")
		filelist->setSortFunc(filelist->ascendingUser);        
	else if (sort_func=="ascendingUserMix")
		filelist->setSortFunc(filelist->ascendingUserMix);        
	else if (sort_func=="descendingUser")
		filelist->setSortFunc(filelist->descendingUser);         
	else if (sort_func=="descendingUserMix")
		filelist->setSortFunc(filelist->descendingUserMix);         
	else if (sort_func=="ascendingGroup")
		filelist->setSortFunc(filelist->ascendingGroup);        
	else if (sort_func=="ascendingGroupMix")
		filelist->setSortFunc(filelist->ascendingGroupMix);        
	else if (sort_func=="descendingGroup")
		filelist->setSortFunc(filelist->descendingGroup);        
	else if (sort_func=="descendingGroupMix")
		filelist->setSortFunc(filelist->descendingGroupMix);        
	else if (sort_func=="ascendingPerm")
		filelist->setSortFunc(filelist->ascendingPerm);        
	else if (sort_func=="ascendingPermMix")
		filelist->setSortFunc(filelist->ascendingPermMix);        
	else if (sort_func=="descendingPerm")
		filelist->setSortFunc(filelist->descendingPerm);        
	else if (sort_func=="descendingPermMix")
		filelist->setSortFunc(filelist->descendingPermMix);        

	// Single click navigation
	if (single_click==SINGLE_CLICK_DIR_FILE)
		filelist->setDefaultCursor(getApp()->getDefaultCursor(DEF_HAND_CURSOR));

    // Status bar buttons
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_files","Ctrl-F6");
    new FXToggleButton(statusbar,TAB+_("Show hidden files")+PARS(key),TAB+_("Hide hidden files")+PARS(key),showhiddenicon,hidehiddenicon,this->filelist,
	                   FileList::ID_TOGGLE_HIDDEN,BUTTON_TOOLBAR|LAYOUT_LEFT|ICON_BEFORE_TEXT);
    
  	key=getApp()->reg().readStringEntry("KEYBINDINGS","thumbnails","Ctrl-F7");
	new FXToggleButton(statusbar,TAB+_("Show thumbnails")+PARS(key),TAB+_("Hide thumbnails")+PARS(key),showthumbicon,hidethumbicon,this->filelist,
	                   FileList::ID_TOGGLE_THUMBNAILS,BUTTON_TOOLBAR|LAYOUT_LEFT|ICON_BEFORE_TEXT);
	
	new FXStatusBar(statusbar,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X);
	new FXDragCorner(statusbar);

    // Toolbar button: Open file
	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
    new FXButton(toolbar,TAB+_("Open")+PARS(key)+TAB+_("Open image file.")+PARS(key),fileopenicon,this,ID_OPEN,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

    // Toolbar button: Print
	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
	new FXButton(toolbar,TAB+_("Print")+PARS(key)+TAB+_("Print image file.")+PARS(key),printicon,this,ID_PRINT,BUTTON_TOOLBAR|FRAME_RAISED);

	// Separator
	toolbarSeparator(toolbar);

    // Note : Ctrl+ and Ctrl- cannot be changed from the registry! 
	
	// Toolbar button: Zoom in
    btn=new FXButton(toolbar,TAB+_("Zoom in")+PARS("Ctrl+")+TAB+_("Zoom in image.")+PARS("Ctrl+"),zoominicon,this,ID_ZOOM_IN,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);
    hotkey=(CONTROLMASK<<16) |  KEY_KP_Add;
    btn->addHotKey(hotkey);

    // Toolbar button: Zoom out
    btn=new FXButton(toolbar,TAB+_("Zoom out")+PARS("Ctrl-")+TAB+_("Zoom out image.")+PARS("Ctrl-"),zoomouticon,this,ID_ZOOM_OUT,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);
    hotkey=(CONTROLMASK<<16) | KEY_KP_Subtract;
    btn->addHotKey(hotkey);

    // Toolbar button: Zoom 100%
	key=getApp()->reg().readStringEntry("KEYBINDINGS","zoom_100","Ctrl-I");
    new FXButton(toolbar,TAB+_("Zoom 100%")+PARS(key)+TAB+_("Zoom image to 100%.")+PARS(key),zoom100icon,this,ID_ZOOM_100,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

    // Toolbar button: Zoom to fit window
	key=getApp()->reg().readStringEntry("KEYBINDINGS","zoom_win","Ctrl-F");
    new FXButton(toolbar,TAB+_("Zoom to fit")+PARS(key)+TAB+_("Zoom to fit window.")+PARS(key),zoomwinicon,this,ID_ZOOM_WIN,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

	// Separator
	toolbarSeparator(toolbar);

    // Toolbar button: Rotate left
	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_left","Ctrl-L");
    new FXButton(toolbar,TAB+_("Rotate left")+PARS(key)+TAB+_("Rotate left image.")+PARS(key),rotatelefticon,this,ID_ROTATE_90,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

    // Toolbar button: Rotate right
	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_right","Ctrl-R");
    new FXButton(toolbar,TAB+_("Rotate right")+PARS(key)+TAB+_("Rotate right image.")+PARS(key),rotaterighticon,this,ID_ROTATE_270,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

    // Toolbar button: mirror horizontally
	key=getApp()->reg().readStringEntry("KEYBINDINGS","mirror_horizontally","Ctrl-H");
    new FXButton(toolbar,TAB+_("Mirror horizontally")+PARS(key)+TAB+_("Mirror image horizontally.")+PARS(key),fliplricon,this,ID_MIRROR_HOR,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

    // Toolbar button: mirror vertically
	key=getApp()->reg().readStringEntry("KEYBINDINGS","mirror_vertically","Ctrl-V");
    new FXButton(toolbar,TAB+_("Mirror vertically")+PARS(key)+TAB+_("Mirror image vertically.")+PARS(key),flipudicon,this,ID_MIRROR_VER,ICON_ABOVE_TEXT|BUTTON_TOOLBAR|FRAME_RAISED);

    // File Menu entries
	FXMenuCommand* mc = NULL;
	FXString text;

	key=getApp()->reg().readStringEntry("KEYBINDINGS","open","Ctrl-O");
	text=_("&Open...")+TABS(key)+_("Open image file.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,fileopenicon,this,ID_OPEN);
 	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
   
	key=getApp()->reg().readStringEntry("KEYBINDINGS","print","Ctrl-P");
	text=_("&Print...")+TABS(key)+_("Print image file.")+PARS(key);
	mc=new FXMenuCommand(filemenu,text,printicon,this,ID_PRINT);
 	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Recent file menu; this automatically hides if there are no files
    FXMenuSeparator* sep1=new FXMenuSeparator(filemenu);
    sep1->setTarget(&mrufiles);
    sep1->setSelector(FXRecentFiles::ID_ANYFILES);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_1);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_2);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_3);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_4);
    new FXMenuCommand(filemenu,FXString::null,NULL,&mrufiles,FXRecentFiles::ID_FILE_5);
    new FXMenuCommand(filemenu,_("&Clear recent files")+TAB2+_("Clear recent file menu."),NULL,&mrufiles,FXRecentFiles::ID_CLEAR);
    FXMenuSeparator* sep2=new FXMenuSeparator(filemenu);
    sep2->setTarget(&mrufiles);
    sep2->setSelector(FXRecentFiles::ID_ANYFILES);

	key=getApp()->reg().readStringEntry("KEYBINDINGS","quit","Ctrl-Q");
	text=_("&Quit")+TABS(key)+_("Quit Xfi.")+PARS(key);
    mc=new FXMenuCommand(filemenu,text,quiticon,this,ID_QUIT);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
	getAccelTable()->addAccel(KEY_Escape,this,FXSEL(SEL_COMMAND,ID_QUIT));

    // Image Menu entries
    new FXMenuCommand(imagemenu,_("Zoom &in")+TAB+(FXString)"Ctrl+"+TAB+_("Zoom in image.")+PARS("Ctrl+"),zoominicon,this,ID_ZOOM_IN);
	new FXMenuCommand(imagemenu,_("Zoom &out")+TAB+(FXString)"Ctrl-"+TAB+_("Zoom out image.")+PARS("Ctrl-"),zoomouticon,this,ID_ZOOM_OUT);
	
	key=getApp()->reg().readStringEntry("KEYBINDINGS","zoom_100","Ctrl-I");
	text=_("Zoo&m 100%")+TABS(key)+_("Zoom image to 100%.")+PARS(key);
	mc=new FXMenuCommand(imagemenu,text,zoom100icon,this,ID_ZOOM_100);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","zoom_win","Ctrl-F");
	text=_("Zoom to fit &window")+TABS(key)+_("Zoom to fit window.")+PARS(key);
    mc=new FXMenuCommand(imagemenu,text,zoomwinicon,this,ID_ZOOM_WIN);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_right","Ctrl-R");
	text=_("Rotate &right")+TABS(key)+_("Rotate right.")+PARS(key);
	mc=new FXMenuCommand(imagemenu,text,rotaterighticon,this,ID_ROTATE_270);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_left","Ctrl-L");
	text=_("Rotate &left")+TABS(key)+_("Rotate left.")+PARS(key);
    mc=new FXMenuCommand(imagemenu,text,rotatelefticon,this,ID_ROTATE_90);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_horizontally","Ctrl-H");
	text=_("Mirror &horizontally")+TABS(key)+_("Mirror horizontally.")+PARS(key);
    mc=new FXMenuCommand(imagemenu,text,fliplricon,this,ID_MIRROR_HOR);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","rotate_vertically","Ctrl-V");
	text=_("Mirror &vertically")+TABS(key)+_("Mirror vertically.")+PARS(key);
	mc=new FXMenuCommand(imagemenu,text,flipudicon,this,ID_MIRROR_VER);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    // Preferences menu
    new FXMenuCheck(prefsmenu,_("&Toolbar")+TAB2+_("Display toolbar."),toolbar,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(prefsmenu,_("&File list")+TAB2+_("Display file list."),filebox,FXWindow::ID_TOGGLESHOWN);
    new FXMenuCheck(prefsmenu,_("&Filter images")+TAB2+_("List only image files."),this,ID_TOGGLE_FILTER_IMAGES);
    new FXMenuCheck(prefsmenu,_("Fit &window when opening")+TAB2+_("Zoom to fit window when opening an image."),this,ID_TOGGLE_FIT_WIN);

    // View Menu entries
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","hidden_files","Ctrl-F6");
	text=_("&Hidden files")+TABS(key)+_("Show hidden files and directories.")+PARS(key);
    mc=new FXMenuCheck(viewmenu,text,this,ID_TOGGLE_HIDDEN);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","thumbnails","Ctrl-F7");
	text=_("&Thumbnails")+TABS(key)+_("Show image thumbnails.")+PARS(key);
    mc=new FXMenuCheck(viewmenu,text,this,ID_TOGGLE_THUMBNAILS);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(viewmenu);
 
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","big_icons","F10");
	text=_("&Big icons")+TABS(key)+_("Display folders with big icons.")+PARS(key);
    mc=new FXMenuRadio(viewmenu,text,this,ID_SHOW_BIG);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));
 
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","small_icons","F11");
	text=_("&Small icons")+TABS(key)+_("Display folders with small icons.")+PARS(key);
    mc=new FXMenuRadio(viewmenu,text,this,ID_SHOW_MINI);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

 	key=getApp()->reg().readStringEntry("KEYBINDINGS","detailed_file_list","F12");
	text=_("&Detailed file list")+TABS(key)+_("Display detailed folder listing.")+PARS(key);
    mc=new FXMenuRadio(viewmenu,text,this,ID_SHOW_DETAILS);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

    new FXMenuSeparator(viewmenu);
    new FXMenuRadio(viewmenu,_("&Rows")+TAB2+_("View icons row-wise."),filelist,FileList::ID_ARRANGE_BY_ROWS);
    new FXMenuRadio(viewmenu,_("&Columns")+TAB2+_("View icons column-wise."),filelist,FileList::ID_ARRANGE_BY_COLUMNS);
    new FXMenuCheck(viewmenu,_("&Autosize")+TAB2+_("Autosize icon names."),filelist,FileList::ID_AUTOSIZE);

    // Help Menu entries
 	key=getApp()->reg().readStringEntry("KEYBINDINGS","help","F1");
	text=_("&About X File Image")+TABS(key)+_("About X File Image.")+PARS(key);
    mc=new FXMenuCommand(helpmenu,text,NULL,this,ID_ABOUT,0);
	hotkey=_parseAccel(key);
	getAccelTable()->addAccel(hotkey,mc,FXSEL(SEL_COMMAND,FXMenuCommand::ID_ACCEL));

	// Close accelerator
	key=getApp()->reg().readStringEntry("KEYBINDINGS","close","Ctrl-W");
	hotkey=_parseAccel(key);	
	getAccelTable()->addAccel(hotkey,this,FXSEL(SEL_COMMAND,XFileImage::ID_QUIT));

    // Make a tool tip
    new FXToolTip(getApp(),TOOLTIP_NORMAL);
	
	// Images
	img=NULL;
	tmpimg=NULL;
	
	// Dialogs
	printdialog=NULL;

    // Recent files
    mrufiles.setTarget(this);
    mrufiles.setSelector(ID_RECENTFILE);

    // Initialize file name
    filename="";
	
	// Initialize some flags
	fileview=ID_SHOW_MINI;
	hiddenfiles=FALSE;	
	
	// Initialize zoom to 100%
	indZoom=ZOOM_100;
	zoomval=zoomtab[indZoom];
	fitwin=FALSE;
	filterimgs=FALSE;

	// Initialize previous window height
	prev_height=getHeight();
}


// Clean up
XFileImage::~XFileImage()
{
    delete toolbar;
	delete menubar;
	delete statusbar;
	delete filemenu;
    delete imagemenu;
    delete helpmenu;
    delete prefsmenu;
    delete viewmenu;
    delete dragshell1;
	delete pathlink;
	delete pathtext;
	delete filelist;
	delete img;
	delete tmpimg;
	delete printdialog;
	delete btnbackhist;
	delete btnforwardhist;
}


long XFileImage::onCmdPopupMenu(FXObject* o,FXSelector s,void* p)
{
    // Popup menu pane
	FXMenuPane menu(this);
    int x,y;
    unsigned int state;
    getRoot()->getCursorPosition(x,y,state);

	new FXMenuCommand(&menu,_("Go ho&me"),homeicon,this,ID_HOME);
	new FXMenuCommand(&menu,_("Go &work"),workicon,this,ID_WORK);
	new FXMenuSeparator(&menu);
	new FXMenuCheck(&menu,_("&Hidden files"),this,ID_TOGGLE_HIDDEN);
	new FXMenuCheck(&menu,_("Thum&bnails"),this,ID_TOGGLE_THUMBNAILS);
	new FXMenuSeparator(&menu);
	new FXMenuRadio(&menu,_("B&ig icons"),this,ID_SHOW_BIG);
	new FXMenuRadio(&menu,_("&Small icons"),this,ID_SHOW_MINI);
	new FXMenuRadio(&menu,_("Fu&ll file list"),this,ID_SHOW_DETAILS);
	new FXMenuSeparator(&menu);
	new FXMenuRadio(&menu,_("&Rows"),filelist,FileList::ID_ARRANGE_BY_ROWS);
	new FXMenuRadio(&menu,_("&Columns"),filelist,FileList::ID_ARRANGE_BY_COLUMNS);
	new FXMenuCheck(&menu,_("Autosize"),filelist,FileList::ID_AUTOSIZE);
	new FXMenuSeparator(&menu);
	new FXMenuRadio(&menu,_("&Name"),filelist,FileList::ID_SORT_BY_NAME);
	new FXMenuRadio(&menu,_("Si&ze"),filelist,FileList::ID_SORT_BY_SIZE);
	new FXMenuRadio(&menu,_("&Type"),filelist,FileList::ID_SORT_BY_TYPE);
	new FXMenuRadio(&menu,_("E&xtension"),filelist,FileList::ID_SORT_BY_EXT);
	new FXMenuRadio(&menu,_("&Date"),filelist,FileList::ID_SORT_BY_TIME);
	new FXMenuRadio(&menu,_("&User"),filelist,FileList::ID_SORT_BY_USER);
	new FXMenuRadio(&menu,_("&Group"),filelist,FileList::ID_SORT_BY_GROUP);
	new FXMenuRadio(&menu,_("&Permissions"),filelist,FileList::ID_SORT_BY_PERM);
	new FXMenuSeparator(&menu);
	new FXMenuCheck(&menu,_("Ignore c&ase"),filelist,FileList::ID_SORT_CASE);
	new FXMenuCheck(&menu,_("Dir&ectories first"),filelist,FileList::ID_DIRS_FIRST);
	new FXMenuCheck(&menu,_("Re&verse order"),filelist,FileList::ID_SORT_REVERSE);

	menu.create();
	allowPopupScroll=TRUE;  // Allow keyboard scrolling
	menu.popup(NULL,x,y);
	getApp()->runModalWhileShown(&menu);
	allowPopupScroll=FALSE;
	return 1;
}


// User clicked up directory button
long XFileImage::onCmdDirUp(FXObject*,FXSelector,void*)
{
    filelist->setDirectory(FXPath::upLevel(filelist->getDirectory()));
   	pathlink->setPath(filelist->getDirectory());
   	pathtext->setText(filelist->getDirectory());
    return 1;
}


// Can we still go up
long XFileImage::onUpdDirUp(FXObject* sender,FXSelector,void*)
{
    if(FXPath::isTopDirectory(filelist->getDirectory()))
        sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
    return 1;
}


// Directory back
long  XFileImage::onCmdDirBack(FXObject*,FXSelector s,void* p)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=filelist->backhist;
	forwardhist=filelist->forwardhist;

	// Get the previous directory
	item=backhist->getFirst();
	if (item)
		pathname=backhist->getString(item);
	
	// Update the history
	backhist->removeFirstItem();
	forwardhist->insertFirstItem(filelist->getDirectory());
	
	// Go to to the previous directory
	filelist->setDirectory(pathname,FALSE);
   	pathlink->setPath(filelist->getDirectory());
   	pathtext->setText(filelist->getDirectory());

    return 1;
}


// Update directory back
long  XFileImage::onUpdDirBack(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *backhist;
	FXString pathname;

	// Get the filelist history
	backhist=filelist->backhist;
	
	// Gray out the button if no item in history 
	if (backhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory forward
long  XFileImage::onCmdDirForward(FXObject*,FXSelector s,void* p)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=filelist->backhist;
	forwardhist=filelist->forwardhist;

	// Get the next directory
	item=forwardhist->getFirst();
	if (item)
		pathname=forwardhist->getString(item);
	
	// Update the history
	forwardhist->removeFirstItem();
	backhist->insertFirstItem(filelist->getDirectory());
	
	// Go to to the previous directory
	filelist->setDirectory(pathname,FALSE);
   	pathlink->setPath(filelist->getDirectory());
   	pathtext->setText(filelist->getDirectory());

    return 1;
}


// Update directory forward
long  XFileImage::onUpdDirForward(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *forwardhist;
	FXString pathname;

	// Get the filelist history
	forwardhist=filelist->forwardhist;
	
	// Gray out the button if no item in history 
	if (forwardhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory back history
long  XFileImage::onCmdDirBackHist(FXObject *sender,FXSelector sel,void* ptr)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=filelist->backhist;
	forwardhist=filelist->forwardhist;

	// Get all string items and display them in a list box
	int num=backhist->getNumItems();
	if (num>0)
	{
		FXString *dirs= new FXString[num];
		FXString strlist="";
		
		// Get string items
		item=backhist->getFirst();
		for(int i=0; i<=num-1; i++)
		{
			if (item)
			{
				FXString str=backhist->getString(item);
				dirs[i]=str;
				strlist=strlist+str+"\n";
				item=backhist->getNext(item);
			}
		}

		// Display list box
		int pos=DirHistBox::box(btnbackhist,DECOR_NONE,strlist,this->getX()+245,this->getY()+37);
		
		// If an item was selected
		if (pos!=-1)
		{
		
			// Update back history
			if (pos==num-1)
				backhist->removeAllItems();
			else
			{
				item=backhist->getItemAtPos(pos+1);
				backhist->removeAllItemsBefore(item);
			}
			
			// Update forward history
			forwardhist->insertFirstItem(filelist->getDirectory());
			if (pos>0)
			{
				for (int i=0; i<=pos-1; i++)
					forwardhist->insertFirstItem(dirs[i]);
			}
	
			// Go to to the selected directory
			pathname=dirs[pos];
			filelist->setDirectory(pathname,FALSE);
   			pathlink->setPath(filelist->getDirectory());
		   	pathtext->setText(filelist->getDirectory());

		}
		delete[]dirs;
	}

    return 1;
}


// Update directory back
long  XFileImage::onUpdDirBackHist(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *backhist;
	FXString pathname;

	// Get the filelist history
	backhist=filelist->backhist;
	
	// Gray out the button if no item in history 
	if (backhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);	
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Directory forward history
long  XFileImage::onCmdDirForwardHist(FXObject *sender,FXSelector sel,void* ptr)
{
	StringList *backhist, *forwardhist;
	StringItem *item;
	FXString pathname;

	// Get the filelist history
	backhist=filelist->backhist;
	forwardhist=filelist->forwardhist;

	// Get all string items and display them in a list box
	int num=forwardhist->getNumItems();
	if (num>0)
	{
		FXString *dirs= new FXString[num];
		FXString strlist="";
		
		// Get string items
		item=forwardhist->getFirst();
		for(int i=0; i<=num-1; i++)
		{
			if (item)
			{
				FXString str=forwardhist->getString(item);
				dirs[i]=str;
				strlist=strlist+str+"\n";
				item=forwardhist->getNext(item);
			}
		}
		
		// Display list box
		int pos=DirHistBox::box(btnforwardhist,DECOR_NONE,strlist,this->getX()+285,this->getY()+37);
		
		// If an item was selected
		if (pos!=-1)
		{
			// Update forward history
			if (pos==num-1)
				forwardhist->removeAllItems();
			else
			{
				item=forwardhist->getItemAtPos(pos+1);
				forwardhist->removeAllItemsBefore(item);
			}
			
			// Update back history
			backhist->insertFirstItem(filelist->getDirectory());
			if (pos>0)
			{
				for (int i=0; i<=pos-1; i++)
					backhist->insertFirstItem(dirs[i]);
			}
	
			// Go to to the selected directory
			pathname=dirs[pos];
			filelist->setDirectory(pathname,FALSE);
   			pathlink->setPath(filelist->getDirectory());
		   	pathtext->setText(filelist->getDirectory());

		}
		delete[]dirs;
	}

    return 1;
}


// Update directory forward
long  XFileImage::onUpdDirForwardHist(FXObject* sender, FXSelector sel, void* ptr)
{
	StringList *forwardhist;
	FXString pathname;

	// Get the filelist history
	forwardhist=filelist->forwardhist;
	
	// Gray out the button if no item in history 
	if (forwardhist->getNumItems()==0)
		sender->handle(this,FXSEL(SEL_COMMAND,ID_DISABLE),ptr);
	else
		sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),ptr);

    return 1;
}


// Back to home directory
long XFileImage::onCmdHome(FXObject*,FXSelector,void*)
{
    filelist->setDirectory(FXSystem::getHomeDirectory());
   	pathlink->setPath(filelist->getDirectory());
   	pathtext->setText(filelist->getDirectory());
    return 1;
}


// Back to current working directory
long XFileImage::onCmdWork(FXObject*,FXSelector,void*)
{
    filelist->setDirectory(FXSystem::getCurrentDirectory());
   	pathlink->setPath(filelist->getDirectory());
   	pathtext->setText(filelist->getDirectory());
    return 1;
}


// About box
long XFileImage::onCmdAbout(FXObject*,FXSelector,void*)
{
	FXString msg;
	msg.format(_("X File Image Version %s is a simple image viewer.\n\n"),VERSION);
	msg += COPYRIGHT;
	MessageBox about(this,_("About X File Image"),msg.text(),xfiicon,BOX_OK|DECOR_TITLE|DECOR_BORDER,
	                 JUSTIFY_CENTER_X|ICON_BEFORE_TEXT|LAYOUT_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);
    about.execute(PLACEMENT_OWNER);

    return 1;
}


// Load file
FXbool XFileImage::loadimage(const FXString& file)
{
    struct stat info;
    FILE *fp;

    FXString ext=FXPath::extension(file);

    if(stat(file.text(),&info)!=0)
    {
        MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Non-existing file: %s"),file.text());
        return FALSE;
    }
    else
	{
		fp=fopen(file.text(),"r");
    	if(!fp)
    	{
        	MessageBox::error(this,BOX_OK,_("Error Loading File"),_("Unable to open file: %s"),file.text());
        	return FALSE;
    	}
		else
			fclose(fp);
	}

	// Free old image if any, before loading a new one
	if (img)
		delete img;
	if (tmpimg)
		delete tmpimg;

    if(comparecase(ext,"gif")==0)
    {
        img=new FXGIFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXGIFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"bmp")==0)
    {
        img=new FXBMPImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXBMPImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"xpm")==0)
    {
        img=new FXXPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXXPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
	}
    else if(comparecase(ext,"pcx")==0)
    {
        img=new FXPCXImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXPCXImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"ico")==0 || comparecase(ext,"cur")==0)
    {
        img=new FXICOImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXICOImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"tga")==0)
    {
        img=new FXTGAImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXTGAImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"rgb")==0)
    {
        img=new FXRGBImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"xbm")==0)
    {
        img=new FXXBMImage(getApp(),NULL,NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXXBMImage(getApp(),NULL,NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"ppm")==0)
    {
        img=new FXPPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXPPMImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"png")==0)
    {
        img=new FXPNGImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXPNGImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"jpg")==0 || comparecase(ext,"jpeg")==0)
    {
        img=new FXJPGImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXJPGImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
    else if(comparecase(ext,"tif")==0 || comparecase(ext,"tiff")==0)
    {
        img=new FXTIFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
        tmpimg=new FXTIFImage(getApp(),NULL,IMAGE_KEEP|IMAGE_SHMI|IMAGE_SHMP);
    }
	else
	{
		img=NULL;
		tmpimg=NULL;
	}

    // Perhaps failed
    if(img==NULL || tmpimg==NULL)
    {
        MessageBox::error(this,BOX_OK,_("Error Loading Image"),_("Unsupported type: %s"),ext.text());
        return FALSE;
    }

    // Load it
    FXFileStream stream;
    if(stream.open(file,FXStreamLoad))
    {
        getApp()->beginWaitCursor();
        FXbool res=img->loadPixels(stream);
	
        stream.close();
		
		// If failed
		if(!res)
		{
			MessageBox::error(this,BOX_OK,_("Error Loading Image"),_("Unable to load image, the file may be corrupted"));
			getApp()->endWaitCursor();
			return FALSE;
		}

		if (!FXMEMDUP(&tmpdata,img->getData(),FXColor,img->getWidth()*img->getHeight()))
			throw FXMemoryException(_("Unable to load image"));
		tmpimg->setData(tmpdata,IMAGE_OWNED,img->getWidth(),img->getHeight());	

		img->create();
		tmpimg->create();

    	imageview->setImage(tmpimg);
	
		// Initial zoom and image format
		indZoom=ZOOM_100;
		zoomval=zoomtab[indZoom];
    	getApp()->endWaitCursor();
		
		// Zoom to fit window if asked
		if (fitwin)
			handle(this,FXSEL(SEL_COMMAND,ID_ZOOM_WIN),NULL);
    }
    filelist->setDirectory(FXPath::directory(file));
  	pathlink->setPath(filelist->getDirectory());
   	pathtext->setText(filelist->getDirectory());

    return TRUE;
}


// Toggle zoom to fit window on startup
long XFileImage::onCmdToggleFitWin(FXObject*,FXSelector,void*)
{
	fitwin=!fitwin;
    return 1;
}


// Update toggle wrap mode
long XFileImage::onUpdToggleFitWin(FXObject* sender,FXSelector,void*)
{
    if(fitwin)
        sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
    return 1;
}


// Toggle filter image files
long XFileImage::onCmdToggleFilterImages(FXObject*,FXSelector,void*)
{
	filterimgs=!filterimgs;
	if (filterimgs)
		filelist->setPattern(imgpatterns);
	else
		filelist->setPattern("*");

    return 1;
}


// Update filter image files
long XFileImage::onUpdToggleFilterImages(FXObject* sender,FXSelector,void*)
{
    // Disable menu item if the file list is not shown
	if(filebox->shown())
	{
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
		
		// Update menu item
		if(filterimgs)
			sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
		else
			sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
	}	
    else
	{
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
		
		// Update menu item
		if(filterimgs)
			sender->handle(this,FXSEL(SEL_COMMAND,ID_CHECK),NULL);
		else
			sender->handle(this,FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
	}
    return 1;
}


// Open
long XFileImage::onCmdOpen(FXObject*,FXSelector,void*)
{
	FileDialog opendialog(this,_("Open Image"));
	opendialog.setSelectMode(SELECTFILE_EXISTING);
    opendialog.setPatternList(patterns);
  	opendialog.setDirectory(filelist->getDirectory());
    if(opendialog.execute())
    {
        filename=opendialog.getFilename();
        filelist->setCurrentFile(filename);
        mrufiles.appendFile(filename);
        loadimage(filename);
    }

    return 1;
}


// Print the text
long XFileImage::onCmdPrint(FXObject*,FXSelector,void*)
{
    // Read the current print command from the registry
    FXString printcommand, command;	
	printcommand=getApp()->reg().readStringEntry("OPTIONS","print_command","lpr -P printer");
	
	// Open print dialog filled with the current print command
	int rc=1;
	if (printdialog==NULL)
		printdialog=new InputDialog(this,printcommand,_("Print command: \n(ex: lpr -P <printer>)"),_("Print"),"",printbigicon);    
	printdialog->setText(printcommand);
	printdialog->CursorEnd();
    rc=printdialog->execute(PLACEMENT_CURSOR);
	printcommand=printdialog->getText();
	
	// If cancel was pressed, exit
	if (!rc)
		return 0;
	
	// Write the new print command to the registry
	getApp()->reg().writeStringEntry("OPTIONS","print_command",printcommand.text());
	
	// Perform the print command
	command =  "cat " + filename + " |" + printcommand + " &";
	system(command.text());
	
    return 1;
}


// Handle toggle hidden command
long XFileImage::onCmdToggleHidden(FXObject* sender,FXSelector sel,void* ptr)
{
	filelist->handle(sender,FXSEL(SEL_COMMAND,FileList::ID_TOGGLE_HIDDEN),ptr);
    return 1;
}

// Update toggle hidden command
long XFileImage::onUpdToggleHidden(FXObject* sender,FXSelector sel,void* ptr)
{	
    unsigned int msg = FXWindow::ID_UNCHECK;
	hiddenfiles = filelist->shownHiddenFiles();
		
	if(hiddenfiles == TRUE)
		msg = FXWindow::ID_CHECK;
    sender->handle(this,FXSEL(SEL_COMMAND,msg),ptr);

    // Disable menu item if the file list is not shown
	if(filebox->shown())
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

    return 1;
}

// Handle toggle hidden command
long XFileImage::onCmdToggleThumbnails(FXObject* sender,FXSelector sel,void* ptr)
{
	filelist->handle(sender,FXSEL(SEL_COMMAND,FileList::ID_TOGGLE_THUMBNAILS),ptr);
    return 1;
}

// Update toggle hidden command
long XFileImage::onUpdToggleThumbnails(FXObject* sender,FXSelector sel,void* ptr)
{	
    unsigned int msg = FXWindow::ID_UNCHECK;
	thumbnails = filelist->shownThumbnails();
		
	if(thumbnails == TRUE)
		msg = FXWindow::ID_CHECK;
    sender->handle(this,FXSEL(SEL_COMMAND,msg),ptr);

    // Disable menu item if the file list is not shown
	if(filebox->shown())
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

    return 1;
}

// Show mini icons in file list
long XFileImage::onCmdShowMini(FXObject*,FXSelector,void*)
{
	fileview=ID_SHOW_MINI;
	filelist->handle(this,FXSEL(SEL_COMMAND,FileList::ID_SHOW_MINI_ICONS),NULL);
    return 1;
}

// Show big icons in file list
long XFileImage::onCmdShowBig(FXObject*,FXSelector,void*)
{
	fileview=ID_SHOW_BIG;
	filelist->handle(this,FXSEL(SEL_COMMAND,FileList::ID_SHOW_BIG_ICONS),NULL);
    return 1;
}

// Show details in file list
long XFileImage::onCmdShowDetails(FXObject*,FXSelector,void*)
{
	fileview=ID_SHOW_DETAILS;
	filelist->handle(this,FXSEL(SEL_COMMAND,FileList::ID_SHOW_DETAILS),NULL);
    return 1;
}



// Update filelist
long XFileImage::onUpdFileView(FXObject* sender,FXSelector sel,void* ptr)
{
	// Keep the filebox height relative to the window height

	// Get the current height
	int height=getHeight();
	
	// If height has changed
	if (relative_resize && prev_height!=height)
	{			
		// File box shown
		if (filebox->shown())
		{
			// Set filebox height to the new value
			filebox->setHeight((int)round(fileheight_pct*height));
		}
	}
			
	// Update previous window height
	prev_height=height;			
		
	// Update the relative size (only if window size is sufficient)
	if (getHeight()>10)
		fileheight_pct=(double)(filebox->getHeight())/(double)(getHeight());
		
	// Update radio buttons
	unsigned int msg = FXWindow::ID_UNCHECK;
		
    switch(FXSELID(sel))
    {
    case ID_SHOW_MINI:
		if (fileview==ID_SHOW_MINI)
            msg = FXWindow::ID_CHECK;		
        break;
    
	case ID_SHOW_BIG:
		if (fileview==ID_SHOW_BIG)
		    msg = FXWindow::ID_CHECK;
        break;
    
	case ID_SHOW_DETAILS:
		if (fileview==ID_SHOW_DETAILS)
           	msg = FXWindow::ID_CHECK;		
        break;
    }
    sender->handle(this,FXSEL(SEL_COMMAND,msg),NULL);

    // Disable menus items if the file list is not shown
	if(filebox->shown())
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);

    return 1;
}


// Harvest the zombies
long XFileImage::onSigHarvest(FXObject*,FXSelector,void*)
{
	while(waitpid(-1,NULL,WNOHANG)>0);
	return 1;
}


// Quit
long XFileImage::onCmdQuit(FXObject*,FXSelector,void*)
{
	// Save settings 
    saveConfig();

    // Quit
    getApp()->exit(0);
    return 1;
}


// Update title (display image size and actual zoom)
long XFileImage::onUpdTitle(FXObject* sender,FXSelector,void*)
{
    FXString title="Xfi " + filename;
    FXImage* image=imageview->getImage();
    if(image && img!=NULL)
        title+=" (" + FXStringVal(img->getWidth()) + "x" + FXStringVal(img->getHeight()) + " - " + FXStringVal(zoomval*100) + "%" ")";
    sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&title);
    return 1;
}


// Open recent file
long XFileImage::onCmdRecentFile(FXObject*,FXSelector,void* ptr)
{
    filename=(char*)ptr;
    filelist->setCurrentFile(filename);
    loadimage(filename);
    return 1;
}


// Double clicked in the file list
long XFileImage::onCmdItemDoubleClicked(FXObject*,FXSelector,void* ptr)
{
    int index=(int)(FXival)ptr;
    if(0<=index)
    {
        if(filelist->isItemDirectory(index))
        {
			FXString filepath=filelist->getItemPathname(index);

			// Does not have access
			if(!::isReadExecutable(filepath))
			{
				MessageBox::error(this,BOX_OK,_("Error"),_(" Permission to: %s denied."), filepath.text());
				return 0;
			}
            filelist->setDirectory(filepath);
            pathlink->setPath(filepath);
   			pathtext->setText(filepath);
        }
        else if(filelist->isItemFile(index))
        {
            filename=filelist->getItemPathname(index);
            mrufiles.appendFile(filename);
            loadimage(filename);
        }
    }
    return 1;
}


// Single clicked in the file list
long XFileImage::onCmdItemClicked(FXObject* sender,FXSelector sel, void* ptr)
{
	if (single_click != SINGLE_CLICK_NONE)
	{		
		// In detailed mode, avoid single click when cursor is not over the first column
		int x, y;
		unsigned int state;
		getCursorPosition(x,y,state);
		FXbool allow=TRUE;
		if (!(filelist->getListStyle()&(_ICONLIST_BIG_ICONS|_ICONLIST_MINI_ICONS)) && (x-filelist->getXPosition())>filelist->getHeaderSize(0))
			allow=FALSE;

		int index=(int)(FXival)ptr;
		if(0<=index)
		{
			if((single_click != SINGLE_CLICK_NONE) && filelist->isItemDirectory(index) && allow)
			{
				FXString filepath=filelist->getItemPathname(index);

				// Does not have access
				if(!::isReadExecutable(filepath))
				{
					MessageBox::error(this,BOX_OK,_("Error"),_(" Permission to: %s denied."), filepath.text());
					return 0;
				}
				filelist->setDirectory(filepath);
				pathlink->setPath(filepath);
   				pathtext->setText(filepath);
			}
			else if((single_click == SINGLE_CLICK_DIR_FILE) && filelist->isItemFile(index) && allow)
			{
				filename=filelist->getItemPathname(index);
				mrufiles.appendFile(filename);
				loadimage(filename);
			}
		}
	}
    return 1;
}



// Rotate image
long XFileImage::onCmdRotate(FXObject*,FXSelector sel,void*)
{
	getApp()->beginWaitCursor();
    FXImage* image=imageview->getImage();
    switch(FXSELID(sel))
    {
    case ID_ROTATE_90:
        
		// Rotate the actual image
		image->rotate(90);
		
		// Need to also rotate the original image only if the actual size is different
		if (image->getWidth()!=img->getWidth() || image->getHeight()!=img->getHeight())
			img->rotate(90);
        break;

    case ID_ROTATE_270:

		// Rotate the actual image
        image->rotate(270);

		// Need to also rotate the original image only if the actual size is different
		if (image->getWidth()!=img->getWidth() || image->getHeight()!=img->getHeight())
			img->rotate(270);
        break;
    }
    imageview->setImage(image);
	getApp()->endWaitCursor();
    return 1;
}


// Update image
long XFileImage::onUpdImage(FXObject* sender,FXSelector,void*)
{
    if(imageview->getImage())
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_ENABLE),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_DISABLE),NULL);
    return 1;
}


// Mirror image
long XFileImage::onCmdMirror(FXObject*,FXSelector sel,void*)
{
	getApp()->beginWaitCursor();
    FXImage* image=imageview->getImage();
    switch(FXSELID(sel))
    {
    case ID_MIRROR_HOR:

		// Mirror the actual image
        image->mirror(TRUE,FALSE);

		// Need to also mirror the original image only if the actual size is different
		if (image->getWidth()!=img->getWidth() || image->getHeight()!=img->getHeight())
        	img->mirror(TRUE,FALSE);
        break;

    case ID_MIRROR_VER:

		// Mirror the actual image
        image->mirror(FALSE,TRUE);

		// Need to also mirror the original image only if the actual size is different
		if (image->getWidth()!=img->getWidth() || image->getHeight()!=img->getHeight())
        	img->mirror(FALSE,TRUE);
        break;
    }
    imageview->setImage(image);
	getApp()->endWaitCursor();
    return 1;
}


// Zoom in image
long XFileImage::onCmdZoomIn(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();

    // Copy the original image into the actual one
   	if (!FXMEMDUP(&tmpdata,img->getData(),FXColor,img->getWidth()*img->getHeight()))
		throw FXMemoryException(_("Unable to load image"));
	tmpimg->setData(tmpdata,IMAGE_OWNED,img->getWidth(),img->getHeight());	

	// Resize the actual image according to the new zoom factor
	indZoom+=1;
	if (indZoom>NB_ZOOM-1) indZoom=NB_ZOOM-1;
	int sx=(int)(tmpimg->getWidth()*zoomtab[indZoom]);
	int sy=(int)(tmpimg->getHeight()*zoomtab[indZoom]);
	
	// Scale only if the actual image size is different
	if (indZoom==ZOOM_100)
		imageview->setImage(img);
	else
	{
		// Maximum zoom allowed
		if (sx>MAX_IMGSIZE || sy > MAX_IMGSIZE)
		{
			indZoom-=1;
			if (indZoom<0) indZoom=0;
			sx=(int)(tmpimg->getWidth()*zoomtab[indZoom]);
			sy=(int)(tmpimg->getHeight()*zoomtab[indZoom]);
		}

		// Scale image according to the new zoom factor
		tmpimg->scale(sx,sy,1);
		imageview->setImage(tmpimg);
	}

	// Set zoom value for window title
	zoomval=zoomtab[indZoom];
	
	getApp()->endWaitCursor();
    return 1;
}


// Zoom out image
long XFileImage::onCmdZoomOut(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();

    // Copy the original image into the actual one
   	if (!FXMEMDUP(&tmpdata,img->getData(),FXColor,img->getWidth()*img->getHeight()))
		throw FXMemoryException(_("Unable to load image"));
	tmpimg->setData(tmpdata,IMAGE_OWNED,img->getWidth(),img->getHeight());	

	// Resize the image according to the new zoom factor
	indZoom-=1;
	if (indZoom<0) indZoom=0;
	int sx=(int)(tmpimg->getWidth()*zoomtab[indZoom]);
	int sy=(int)(tmpimg->getHeight()*zoomtab[indZoom]);

	// Scale only if the actual image size is different
	if (indZoom==ZOOM_100)
		imageview->setImage(img);
	else
	{
		// Scale image according to the new zoom factor
    	tmpimg->scale(sx,sy,1);
    	imageview->setImage(tmpimg);
	}

	// Set zoom value for window title
	zoomval=zoomtab[indZoom];
	
	getApp()->endWaitCursor();
    return 1;
}


// Zoom to 100%
long XFileImage::onCmdZoom100(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();
	indZoom=ZOOM_100;
	zoomval=zoomtab[indZoom];	
	imageview->setImage(img);
	getApp()->endWaitCursor();
    return 1;
}


// Zoom to fit window
long XFileImage::onCmdZoomWin(FXObject*,FXSelector,void*)
{
	getApp()->beginWaitCursor();

	// Window and image sizes
	int winw=imageview->getWidth();
	int winh=imageview->getHeight();
	int w=img->getWidth();
	int h=img->getHeight();
	
	// Compute zoom factor
	double fitwin;	
	if (double(w)/double(h)>double(winw)/double(winh))
		fitwin=0.98*(double)winw/(double)w;
	else
		fitwin=0.98*(double)winh/(double)h;

	// Find the most approaching predefined zoom
	// This is used in other zoom functions
	for (int k=0; k<NB_ZOOM; k++)
	{
 		if (zoomtab[k]>fitwin)
		{	
			indZoom=k-1;
			break;
		}
	}
	if (indZoom<0) indZoom=0;
	if (indZoom>=NB_ZOOM) indZoom=NB_ZOOM-1;
    	
	// Copy the original image into the actual one
   	if (!FXMEMDUP(&tmpdata,img->getData(),FXColor,img->getWidth()*img->getHeight()))
		throw FXMemoryException(_("Unable to load image"));
	tmpimg->setData(tmpdata,IMAGE_OWNED,img->getWidth(),img->getHeight());	

	// Resize the image according to the new zoom factor
	int sx=(int)(w*fitwin);
	int sy=(int)(h*fitwin);

	// Scale image according to the new zoom factor
    tmpimg->scale(sx,sy,1);
    imageview->setImage(tmpimg);

	// Set zoom value for window title
	zoomval=fitwin;
	
	getApp()->endWaitCursor();
    return 1;
}


// Start the ball rolling
void XFileImage::start(FXString startimage)
{
    filename=startimage;
	if(filename != "")
		loadimage(filename);
}


// Create and show window
void XFileImage::create()
{
	// Get size and position
    unsigned int ww=getApp()->reg().readUnsignedEntry("OPTIONS","width",DEFAULT_WINDOW_WIDTH);    // Workaround for a possible bug in some WMs
    unsigned int hh=getApp()->reg().readUnsignedEntry("OPTIONS","height",DEFAULT_WINDOW_HEIGHT);  // Workaround for a possible bug in some WMs
    fileheight_pct=getApp()->reg().readRealEntry("OPTIONS","fileheight_pct",0.25);
    unsigned int fs=getApp()->reg().readIntEntry("OPTIONS","filesshown",TRUE);

	filelist->setDirectory(FXSystem::getCurrentDirectory());
   	pathlink->setPath(FXSystem::getCurrentDirectory());
   	pathtext->setText(FXSystem::getCurrentDirectory());

	// Display or hide path linker and path text
	if (show_pathlink)
	{
		pathtext->hide();
		pathlink->show();
	}
	else
	{
		pathlink->hide();
		pathtext->show();
	}

    // Hide tree if asked for
    if(!fs)
        filebox->hide();

	// Get toolbar status
    if(getApp()->reg().readUnsignedEntry("OPTIONS","showtoolbar",TRUE)==FALSE)
        toolbar->hide();
   
	// Get status bar status
    if(getApp()->reg().readUnsignedEntry("OPTIONS","showstatusbar",TRUE)==FALSE)
        statusbar->hide();

 	// Get hidden file status
	hiddenfiles=getApp()->reg().readUnsignedEntry("OPTIONS","hiddenfiles",0);
	filelist->showHiddenFiles(hiddenfiles);
  
 	// Get thumbnails status
	//thumbnails=getApp()->reg().readUnsignedEntry("OPTIONS","thumbnails",0);
	//filelist->showThumbnails(thumbnails);
  
	// Get list style
	liststyle=getApp()->reg().readUnsignedEntry("OPTIONS","liststyle",(unsigned int)IconList::ID_SHOW_DETAILS);
	filelist->setListStyle(liststyle);

	// Get file view
    fileview=getApp()->reg().readUnsignedEntry("OPTIONS","fileview",ID_SHOW_MINI);    
	this->handle(this,FXSEL(SEL_COMMAND,fileview),NULL);

	// Get startup zoom
	fitwin=getApp()->reg().readUnsignedEntry("OPTIONS","fitwin",0);
	
	// Get filter images flag
	filterimgs=getApp()->reg().readUnsignedEntry("OPTIONS","filterimgs",FALSE);
	
	// Get position and position window
    if (save_win_pos)
	{
		int xpos=getApp()->reg().readIntEntry("OPTIONS","xpos",DEFAULT_WINDOW_XPOS);
		int ypos=getApp()->reg().readIntEntry("OPTIONS","ypos",DEFAULT_WINDOW_YPOS);
		position(xpos,ypos,ww,hh);
	}
	else
    	position(getX(),getY(),ww,hh);

    FXMainWindow::create();

	if (filterimgs)
		filelist->setPattern(imgpatterns);

	// Set file list height
    filebox->setHeight((int)round(fileheight_pct*getHeight()));

	show();

#ifdef STARTUP_NOTIFICATION
	startup_completed();
#endif
}


// Save configuration when quitting
void XFileImage::saveConfig()
{
    // Write new window size and position back to registry
	getApp()->reg().writeUnsignedEntry("OPTIONS","width",(unsigned int)getWidth());
	getApp()->reg().writeUnsignedEntry("OPTIONS","height",(unsigned int)getHeight());

	if (save_win_pos)
	{
		// Account for the Window Manager border size
		XWindowAttributes xwattr;
		if (XGetWindowAttributes((Display*)getApp()->getDisplay(),this->id(),&xwattr))
		{
			getApp()->reg().writeIntEntry("OPTIONS","xpos",getX()-xwattr.x);
			getApp()->reg().writeIntEntry("OPTIONS","ypos",getY()-xwattr.y);
		}
		else
		{
			getApp()->reg().writeIntEntry("OPTIONS","xpos",getX());
			getApp()->reg().writeIntEntry("OPTIONS","ypos",getY());
		}
	}

    // Width of tree
    getApp()->reg().writeRealEntry("OPTIONS","fileheight_pct",fileheight_pct);

    // Was file box shown
    getApp()->reg().writeIntEntry("OPTIONS","filesshown",filebox->shown());

    // Toolbar status
    if(toolbar->shown())
        getApp()->reg().writeUnsignedEntry("OPTIONS","showtoolbar",TRUE);
    else
        getApp()->reg().writeUnsignedEntry("OPTIONS","showtoolbar",FALSE);

	// Hidden files status
	getApp()->reg().writeUnsignedEntry("OPTIONS","hiddenfiles",hiddenfiles);

	// Thumbnails status
	getApp()->reg().writeUnsignedEntry("OPTIONS","thumbnails",thumbnails);

	// File view
	getApp()->reg().writeUnsignedEntry("OPTIONS","fileview",fileview);

	// List style
	getApp()->reg().writeUnsignedEntry("OPTIONS","liststyle",filelist->getListStyle());

	// Startup zoom
	getApp()->reg().writeUnsignedEntry("OPTIONS","fitwin",fitwin);

	// Filter images in file list
	getApp()->reg().writeUnsignedEntry("OPTIONS","filterimgs",filterimgs);

	// Filelist columns sizes
	getApp()->reg().writeUnsignedEntry("OPTIONS","name_size",filelist->getHeaderSize(0));
	getApp()->reg().writeUnsignedEntry("OPTIONS","size_size",filelist->getHeaderSize(1));
	getApp()->reg().writeUnsignedEntry("OPTIONS","type_size",filelist->getHeaderSize(2));
	getApp()->reg().writeUnsignedEntry("OPTIONS","ext_size",filelist->getHeaderSize(3));
	getApp()->reg().writeUnsignedEntry("OPTIONS","modd_size",filelist->getHeaderSize(4));
	getApp()->reg().writeUnsignedEntry("OPTIONS","user_size",filelist->getHeaderSize(5));
	getApp()->reg().writeUnsignedEntry("OPTIONS","grou_size",filelist->getHeaderSize(6));
	getApp()->reg().writeUnsignedEntry("OPTIONS","attr_size",filelist->getHeaderSize(7));

	// Get and write sort function for search window
	FXString sort_func;
	if (filelist->getSortFunc()==filelist->ascendingCase)
		sort_func="ascendingCase";
	if (filelist->getSortFunc()==filelist->ascendingCaseMix)
		sort_func="ascendingCaseMix";
	else if (filelist->getSortFunc()==filelist->descendingCase)
		sort_func="descendingCase";
	else if (filelist->getSortFunc()==filelist->descendingCaseMix)
		sort_func="descendingCaseMix";
	else if (filelist->getSortFunc()==filelist->ascending)
		sort_func="ascending";
	else if (filelist->getSortFunc()==filelist->ascendingMix)
		sort_func="ascendingMix";
	else if (filelist->getSortFunc()==filelist->descending)
		sort_func="descending";
	else if (filelist->getSortFunc()==filelist->descendingMix)
		sort_func="descendingMix";
	else if (filelist->getSortFunc()==filelist->ascendingSize)
		sort_func="ascendingSize";
	else if (filelist->getSortFunc()==filelist->ascendingSizeMix)
		sort_func="ascendingSizeMix";
	else if (filelist->getSortFunc()==filelist->descendingSize)
		sort_func="descendingSize";
	else if (filelist->getSortFunc()==filelist->descendingSizeMix)
		sort_func="descendingSizeMix";
	else if (filelist->getSortFunc()==filelist->ascendingType)
		sort_func="ascendingType";
	else if (filelist->getSortFunc()==filelist->ascendingTypeMix)
		sort_func="ascendingTypeMix";
	else if (filelist->getSortFunc()==filelist->descendingType)
		sort_func="descendingType";
	else if (filelist->getSortFunc()==filelist->descendingTypeMix)
		sort_func="descendingTypeMix";
	else if (filelist->getSortFunc()==filelist->ascendingExt)
		sort_func="ascendingExt";
	else if (filelist->getSortFunc()==filelist->ascendingExtMix)
		sort_func="ascendingExtMix";
	else if (filelist->getSortFunc()==filelist->descendingExt)
		sort_func="descendingExt";
	else if (filelist->getSortFunc()==filelist->descendingExtMix)
		sort_func="descendingExtMix";
	else if (filelist->getSortFunc()==filelist->ascendingTime)
		sort_func="ascendingTime";
	else if (filelist->getSortFunc()==filelist->ascendingTimeMix)
		sort_func="ascendingTimeMix";
	else if (filelist->getSortFunc()==filelist->descendingTime)
		sort_func="descendingTime";
	else if (filelist->getSortFunc()==filelist->descendingTimeMix)
		sort_func="descendingTimeMix";
	else if (filelist->getSortFunc()==filelist->ascendingUser)
		sort_func="ascendingUser";
	else if (filelist->getSortFunc()==filelist->ascendingUserMix)
		sort_func="ascendingUserMix";
	else if (filelist->getSortFunc()==filelist->descendingUser)
		sort_func="descendingUser";
	else if (filelist->getSortFunc()==filelist->descendingUserMix)
		sort_func="descendingUserMix";
	else if (filelist->getSortFunc()==filelist->ascendingGroup)
		sort_func="ascendingGroup";
	else if (filelist->getSortFunc()==filelist->ascendingGroupMix)
		sort_func="ascendingGroupMix";
	else if (filelist->getSortFunc()==filelist->descendingGroup)
		sort_func="descendingGroup";
	else if (filelist->getSortFunc()==filelist->descendingGroupMix)
		sort_func="descendingGroupMix";
	else if (filelist->getSortFunc()==filelist->ascendingPerm)
		sort_func="ascendingPerm";
	else if (filelist->getSortFunc()==filelist->ascendingPermMix)
		sort_func="ascendingPermMix";
	else if (filelist->getSortFunc()==filelist->descendingPerm)
		sort_func="descendingPerm";
	else if (filelist->getSortFunc()==filelist->descendingPermMix)
		sort_func="descendingPermMix";
	else
		sort_func="ascendingCase";
	getApp()->reg().writeStringEntry("OPTIONS","sort_func",sort_func.text());

	// Write registry settings
	getApp()->reg().write();
}


// Usage message
#define USAGE_MSG	_("\
\nUsage: xfi [options] [image] \n\
\n\
    [options] can be any of the following:\n\
\n\
        -h, --help         Print (this) help screen and exit.\n\
        -v, --version      Print version information and exit.\n\
\n\
    [image] is the path to the image file you want to open on start up.\n\
\n")




// Start the whole thing
int main(int argc,char *argv[])
{
	int i;
	FXString startimage="";
	const char *appname = "xfi";
	const char *xfename = XFEAPPNAME;
	const char *vdrname = XFEVDRNAME;
	FXbool loadicons;

	// Get environment variables $HOME, $XDG_DATA_HOME and $XDG_CONFIG_HOME
	homedir=FXSystem::getHomeDirectory();
    if(homedir=="")
        homedir=ROOTDIR;
	xdgdatahome=getenv("XDG_DATA_HOME");
    if(xdgdatahome=="")
        xdgdatahome=homedir + PATHSEPSTRING DATAPATH;     
	xdgconfighome=getenv("XDG_CONFIG_HOME");
    if(xdgconfighome=="")
        xdgconfighome=homedir + PATHSEPSTRING CONFIGPATH;

#ifdef HAVE_SETLOCALE
	// Set locale via LC_ALL.
	setlocale (LC_ALL, "");
#endif

#if ENABLE_NLS
	// Set the text message domain.
	bindtextdomain (PACKAGE, LOCALEDIR);
	bind_textdomain_codeset(PACKAGE,"utf-8");
	textdomain (PACKAGE);
#endif

	// Parse basic arguments
	for(i = 1; i < argc; ++i)
	{
    	if(compare(argv[i],"-v")==0 || compare(argv[i],"--version")==0)
		{
			fprintf(stdout,"%s version %s\n",PACKAGE,VERSION);
			exit(0);
		}
    	else if(compare(argv[i],"-h")==0 || compare(argv[i],"--help")==0)
		{
			fprintf(stdout,USAGE_MSG);
			exit(0);
		}
		else
	    {
			// Start image, if any
			startimage=argv[i];
	    }
	}

    args=argv;

    // Make application
    FXApp* application=new FXApp(appname,vdrname);

    // Open display
    application->init(argc,argv);

    // Redefine the default hand cursor
    FXCursor* hand=new FXCursor(application,hand_bits,hand_mask_bits,hand_width,hand_height,hand_x_hot,hand_y_hot);
    application->setDefaultCursor(DEF_HAND_CURSOR,hand);

	// Load all application icons
	loadicons=loadAppIcons(application);
	
    // Read the Xfe registry
	FXRegistry* reg_xfe=new FXRegistry(xfename,vdrname);
	reg_xfe->read();

	// Set base color (to change the default base color at first run)
	FXColor basecolor=reg_xfe->readColorEntry("SETTINGS","basecolor",FXRGB(237,233,227));
	application->setBaseColor(basecolor);

	// Set Xfi normal font according to the Xfe registry
	FXString fontspec;
	fontspec=reg_xfe->readStringEntry("SETTINGS","font","Helvetica,100,normal,regular");
	if(!fontspec.empty())
	{
    	FXFont* normalFont=new FXFont(application,fontspec);
		normalFont->create();
        application->setNormalFont(normalFont);
    }

	// Set Xfi file list colors according to the Xfe registry
	listbackcolor=reg_xfe->readColorEntry("SETTINGS","listbackcolor",FXRGB(255,255,255));
	listforecolor=reg_xfe->readColorEntry("SETTINGS","listforecolor",FXRGB(0,0,0));
	highlightcolor=reg_xfe->readColorEntry("SETTINGS","highlightcolor",FXRGB(238,238,238));

	// Set single click navigation according to the Xfe registry
	single_click=reg_xfe->readUnsignedEntry("SETTINGS","single_click",SINGLE_CLICK_NONE);

	// Set smooth scrolling according to the Xfe registry
	FXbool smoothscroll=reg_xfe->readUnsignedEntry("SETTINGS","smooth_scroll",TRUE);
	
	// Set file list tooltip flag according to the Xfe registry
	file_tooltips=reg_xfe->readUnsignedEntry("SETTINGS","file_tooltips",TRUE);

	// Set relative resizing flag according to the Xfe registry
	relative_resize=reg_xfe->readUnsignedEntry("SETTINGS","relative_resize",TRUE);
	
	// Set display pathlinker flag according to the Xfe registry
	show_pathlink=reg_xfe->readUnsignedEntry("SETTINGS","show_pathlinker",TRUE);
	    
    // Get value of the window position flag
	save_win_pos=reg_xfe->readUnsignedEntry("SETTINGS","save_win_pos",FALSE);

	// Delete the Xfe registry
	delete reg_xfe;

	// Make window
    XFileImage* window=new XFileImage(application,smoothscroll);
	
 	// Catch SIGCHLD to harvest zombie child processes
	application->addSignal(SIGCHLD,window,XFileImage::ID_HARVEST,TRUE);

	// Smooth scrolling
	window->setSmoothScroll(smoothscroll);

	// Handle interrupt to save stuff nicely
    application->addSignal(SIGINT,window,XFileImage::ID_QUIT);
 
    // Create it
    application->create();
	if (!loadicons)
		 MessageBox::error(application,BOX_OK,_("Error loading icons"),_("Unable to load some icons. Please check your icons path!"));

	// Tooltips setup time and duration
	application->setTooltipPause(TOOLTIP_PAUSE);
	application->setTooltipTime(TOOLTIP_TIME);

    // Start
    window->start(startimage);

	// Run
    return application->run();
}


