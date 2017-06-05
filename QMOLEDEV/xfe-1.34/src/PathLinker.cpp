// Implementation of a path linker that allows to directly go to any parent directory
// Initially proposed and coded by Julian Mitchell <jupeos@gmail.com>, many thanks to him!

#include <stddef.h>
#include <sstream>
#include <fx.h>

#include "xfedefs.h"
#include "xfeutils.h"
#include "XFileExplorer.h"
#include "PathLinker.h"

#define REFRESH_INTERVAL     1000


FXDEFMAP(PathLinker) PathLinkerMap[]=
{
	FXMAPFUNC(SEL_FOCUSIN, PathLinker::ID_FOCUS_BUTTON, PathLinker::onCmdFocusButton),
	FXMAPFUNCS(SEL_LEFTBUTTONPRESS, PathLinker::ID_START_LINK, PathLinker::ID_END_LINK, PathLinker::pathButtonPressed),
	FXMAPFUNC(SEL_UPDATE, 0, PathLinker::onUpdPath),
};

FXIMPLEMENT(PathLinker, FXHorizontalFrame, PathLinkerMap, ARRAYNUMBER(PathLinkerMap))


// Construct object
PathLinker::PathLinker(FXComposite* a, FileList* flist, DirList* dlist, unsigned int opts): FXHorizontalFrame(a, opts, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2)
{
	filelist=flist;
	dirlist=dlist;
	
	// Add some path links
    int id = ID_START_LINK;
    for(int i = 0; i < MAX_LINKS; i++)
    {
        std::stringstream ss;
        ss << i;
        linkButtons.push_back(new FXButton(this, (ss.str() + PATHSEPSTRING).c_str(), NULL, this, id, BUTTON_NORMAL, 0, 0, 0, 0, 5, 5, 0, 0));
        id++;
        linkButtons[i]->hide();
        linkButtons[i]->setDefaultCursor(getApp()->getDefaultCursor(DEF_HAND_CURSOR));
	}
    
    // Initializations
    visitedPath = PATHSEPSTRING;
    nbActiveButtons = 0;
    currentButton = 0;
	
	// Right most button is a TextLabel and is only used for focus
	focusButton = new TextLabel(this,0,this,ID_FOCUS_BUTTON,LAYOUT_FILL_X|LAYOUT_FILL_Y);
	
	// Create highlight font (bold if normal font is normal, and normal if normal font is bold)
	FXFontDesc fontdesc;
	normalFont=getApp()->getNormalFont();
	normalFont->getFontDesc(fontdesc);
	if (fontdesc.weight==FXFont::Normal)
		fontdesc.weight=FXFont::Bold;
	else
		fontdesc.weight=FXFont::Normal;
	highlightFont=new FXFont(getApp(),fontdesc);
	highlightFont->create();

	// Set the focus button initial color
	focusButton->setBackColor(getApp()->getBaseColor());
}


// Create the path linker
void PathLinker::create()
{
    FXHorizontalFrame::create();
}


// Destruct object
PathLinker::~PathLinker()
{
	delete highlightFont;
}


// Change current path
void PathLinker::setPath(FXString text)
{	
	int nextPos = 0;
    int previousPos = 0;

	// Remove trailing /
	FXString path = ::cleanPath(text);	
	
	// Indicates if actual path is included in the visited path
	int visited;
	if (path==PATHSEPSTRING)
		visited = visitedPath.find(PATHSEPSTRING);
	else
		visited = visitedPath.find(path+PATHSEPSTRING);

	// If actual path is included in the visited path
	unsigned int index = 0;
	if (visited >= 0)
	{		
		nextPos = path.find(PATHSEPSTRING, 0);		
		while(nextPos >= 0)
		{				
			previousPos = nextPos + 1;
			nextPos = path.find(PATHSEPSTRING, previousPos);
			index++;
		}
		if (path.length() ==1)
			index = 0;		
		path = visitedPath;
	}
	
	// File list width
	//int frameWidth = filelist->getWidth();

	// Hide all of the link buttons
    for(int i = 0; i < MAX_LINKS; i++)
    {
        linkButtons[i]->hide();
        linkButtons[i]->setFont(normalFont);
        linkButtons[i]->setState(STATE_UP);
    }

    visitedPath = path;
    //int textWidth = normalFont->getTextWidth(path.text(), path.length());

    FXString displayText = visitedPath;
    
    /* Doesn't work properly!
    if(textWidth > frameWidth)
    {    	
        // The magic '4' is to compensate for the '/...' that is introduced at the front
        // of the the path. This calculation doesn't seem to work quite right as it
        // shifts the splitter's grab bar a small amount, for the left panel.
        displayText = displayText.right(frameWidth / (textWidth / path.length()) - 4);
        displayText = "..." + displayText;
        displayText = PATHSEPSTRING + displayText;
    }
    */
    
    unsigned int ind = 0;
    nextPos = displayText.find(PATHSEPSTRING, 0);
    previousPos = 0;
        
    while(nextPos >= 0)
    {
        // Root path
        if (previousPos==0)
            setText(ind, displayText.mid(previousPos, nextPos - previousPos + 1));
        
        // Other path
        else
        	setText(ind, displayText.mid(previousPos, nextPos - previousPos));
                
        ind++;
        previousPos = nextPos + 1;
        nextPos = displayText.find(PATHSEPSTRING, previousPos);
    }
    nbActiveButtons = ind+1;
    setText(ind, displayText.mid(previousPos, displayText.length()));
    
    // If actual path is included in the visited path
    if (visited >= 0)
    {
    	linkButtons[index]->setFont(highlightFont);
    	linkButtons[index]->setState(STATE_DOWN);
    	currentButton=index;
	}
    else
    {
    	linkButtons[ind]->setFont(highlightFont);
    	linkButtons[ind]->setState(STATE_DOWN);
    	currentButton=ind;
	}
}


// Update current path according to the clicked button
void PathLinker::updatePath(FXString text, unsigned int index)
{	
	// Remove trailing /
	FXString path=::cleanPath(text);
		
	// File list width
	//int frameWidth = filelist->getWidth();

	// Hide all of the link buttons
    for(int i = 0; i < MAX_LINKS; i++)
    {
        linkButtons[i]->hide();
        linkButtons[i]->setFont(normalFont);
        linkButtons[i]->setState(STATE_UP);
    }

    visitedPath = path;
    //FXFont* normalFont = getApp()->getNormalFont();
    //int textWidth = normalFont->getTextWidth(path.text(), path.length());

    FXString displayText = visitedPath;
    
    /* Doesn(t work properly!
    if(textWidth > frameWidth)
    {    	
        // The magic '4' is to compensate for the '/...' that is introduced at the front
        // of the the path. This calculation doesn't seem to work quite right as it
        // shifts the splitter's grab bar a small amount, for the left panel.
        displayText = displayText.right(frameWidth / (textWidth / path.length()) - 4);
        displayText = "..." + displayText;
        displayText = PATHSEPSTRING + displayText;
    }
    */
    
	int nextPos = 0;
    int previousPos = 0;
    unsigned int ind = 0;
    nextPos = displayText.find(PATHSEPSTRING, 0);
        
    while(nextPos >= 0)
    {
        // Root path
        if (previousPos==0)
            setText(ind, displayText.mid(previousPos, nextPos - previousPos + 1));
        
        // Other path
        else
        	setText(ind, displayText.mid(previousPos, nextPos - previousPos));
                
        ind++;
        previousPos = nextPos + 1;
        nextPos = displayText.find(PATHSEPSTRING, previousPos);
    }
    nbActiveButtons = ind+1;
    setText(ind, displayText.mid(previousPos, displayText.length()));
    
   	linkButtons[index]->setFont(highlightFont);
    linkButtons[index]->setState(STATE_DOWN);
    currentButton=index;
}


void PathLinker::setText(unsigned int index, FXString displayText)
{	
    if(index < MAX_LINKS)
    {        
		linkButtons[index]->setText(displayText);
		if (displayText.length())
			linkButtons[index]->show();
    }
}


// Button was pressed
long PathLinker::pathButtonPressed(FXObject * obj ,FXSelector sel ,void* ptr)
{
	// Set the focus on the file list
	filelist->setFocus();
	
	FXString filePath("");
    int endId = FXSELID(sel);

	if(endId == ID_START_LINK)
	{
		// Selecting root dir
		filePath = PATHSEPSTRING;
	}
	else
	{
		int rpos = 0;
		rpos = visitedPath.rfind((char)PATHSEPSTRING[0], 0x7FFFFFFF, nbActiveButtons - (endId - ID_START_LINK + 1));
		filePath = visitedPath.left(rpos+1);
	}

	// Update the path text
	updatePath(visitedPath,endId - ID_START_LINK);
	
	// Update the FileList and DirList directory
	filelist->setDirectory(filePath);
	if (dirlist)
		dirlist->setDirectory(filePath,TRUE);
	
	return 1;
}


// Gives the focus to the file list when clicking on the focus button
long PathLinker::onCmdFocusButton(FXObject * obj ,FXSelector sel ,void* ptr)
{	
	// Set the focus on the file list
	filelist->setFocus();
    return 1;
}


// Update visited path to delete directories that don't exist anymore
// Also update in the case where the actual link differs from the actual path
long PathLinker::onUpdPath(FXObject * obj ,FXSelector sel ,void* ptr)
{	
	// It is not necessary to update when the path linker is not visible
	if (shown())
	{			
		// Current path of the file list (the real path)
		FXString currentpath=::cleanPath(filelist->getDirectory());
		
		// Current path link (the one corresponding to the down button)
		FXString currentlink;
		if (currentButton==0)
			currentlink=PATHSEPSTRING;
		else
			currentlink = visitedPath.before((char)PATHSEPSTRING[0],currentButton+1);

		// Test each link for existence and update to the current path if necessary
		FXString path=visitedPath;
		unsigned int n=1;
		while (path != "")
		{
			if (!::exists(path))
			{
				visitedPath=filelist->getDirectory();
				setPath(visitedPath);
				break;
			}
		
			// Next path to test
			path=visitedPath.rbefore((char)PATHSEPSTRING[0],n);
			n++;
		}
			
		// If current link and current path differ, update to the current path
		if (currentlink != currentpath)
			setPath(currentpath);
	}

   	return 0;
}


void PathLinker::unfocus(void)
{
	this->setBackColor(FXRGB(128,128,128));
    for(int i=0; i <MAX_LINKS; i++)
    {
        linkButtons[i]->setBackColor(FXRGB(128,128,128));
		linkButtons[i]->setTextColor(FXRGB(255,255,255));
	}
	focusButton->setBackColor(FXRGB(128,128,128));
}


void PathLinker::focus(void)
{
	this->setBackColor(getApp()->getBaseColor());
    for(int i=0; i <MAX_LINKS; i++)
    {
        linkButtons[i]->setBackColor(getApp()->getBaseColor());
		linkButtons[i]->setTextColor(getApp()->getForeColor());
    }
	focusButton->setBackColor(getApp()->getBaseColor());
}

