// Bookmarks list. Taken from the FOX library (FXRecentFiles) and slightly modified.

#include "config.h"
#include "i18n.h"

#include <stdio.h>
#include <stdlib.h>

#include <fx.h>

#include "MessageBox.h"
#include "Bookmarks.h"


// Maximum bookmarks number
// If modified, also change appropriate items in BookmarksMap
// and in onUpdBookmark and Bookmarks.h
#define MAX_BOOKMARKS 21



// Message map
FXDEFMAP(Bookmarks) BookmarksMap[]=
{
	FXMAPFUNC(SEL_UPDATE,Bookmarks::ID_ANYBOOKMARKS,Bookmarks::onUpdAnyBookmarks),
	FXMAPFUNC(SEL_UPDATE,Bookmarks::ID_CLEAR,Bookmarks::onUpdAnyBookmarks),
	FXMAPFUNC(SEL_COMMAND,Bookmarks::ID_CLEAR,Bookmarks::onCmdClear),
	FXMAPFUNCS(SEL_COMMAND,Bookmarks::ID_BOOKMARK_1,Bookmarks::ID_BOOKMARK_20,Bookmarks::onCmdBookmark),
	FXMAPFUNCS(SEL_UPDATE,Bookmarks::ID_BOOKMARK_1,Bookmarks::ID_BOOKMARK_20,Bookmarks::onUpdBookmark),
};

// Class implementation
FXIMPLEMENT(Bookmarks,FXObject,BookmarksMap,ARRAYNUMBER(BookmarksMap))


// Make new Bookmarks group with default group
Bookmarks::Bookmarks():group("Bookmarks"),target(NULL),message(0),maxbookmarks(MAX_BOOKMARKS)
{}


// Make new Bookmarks group
Bookmarks::Bookmarks(const FXString& gp,FXObject *tgt,FXSelector sel):group(gp),target(tgt),message(sel),maxbookmarks(MAX_BOOKMARKS)
{}



// Obtain the bookmark at index
FXString Bookmarks::getBookmark(int index) const
{
    char key[20];
    snprintf(key,sizeof(key)-1,"BOOKMARK%d",index);
    return FXApp::instance()->reg().readStringEntry(group.text(),key,FXString::null);
}


// Change the bookmark at index
void Bookmarks::setBookmark(int index,const FXString& bookname)
{
    char key[20];
    snprintf(key,sizeof(key)-1,"BOOKMARK%d",index);
    FXApp::instance()->reg().writeStringEntry(group.text(),key,bookname.text());
}


// Append a bookmark; its added to the top of the list, and everything else
// is moved down the list one notch; the last one is dropped from the list.
void Bookmarks::appendBookmark(const FXString& bookname)
{
    FXString newname=bookname;
    FXString oldname;
    char key[20];
    int i=1,j=1;
	
	FXApp::instance()->reg().read();
    do
    {
        do
        {
            snprintf(key,sizeof(key)-1,"BOOKMARK%d",j++);
            oldname=FXApp::instance()->reg().readStringEntry(group.text(),key,NULL);
		}
        while(oldname==bookname);
        snprintf(key,sizeof(key)-1,"BOOKMARK%d",i++);
        FXApp::instance()->reg().writeStringEntry(group.text(),key,newname.text());
        newname=oldname;
		if(i>MAX_BOOKMARKS)
			MessageBox::warning(FXApp::instance()->getActiveWindow(),BOX_OK,_("Warning"),
			                  _("Bookmarks limit number reached. The last bookmark will be deleted..."));									
    }
    while(!oldname.empty() && i<=maxbookmarks);
		
	FXApp::instance()->reg().write();
}


// Remove a bookmark
void Bookmarks::removeBookmark(const FXString& bookname)
{
    char key[20];
    FXString name;
    int i=1,j=1;
    do
    {
        snprintf(key,sizeof(key)-1,"BOOKMARK%d",i++);
        name=FXApp::instance()->reg().readStringEntry(group.text(),key,NULL);
        FXApp::instance()->reg().deleteEntry(group.text(),key);
        if(name.empty())
            break;
        if(name!=bookname)
        {
            snprintf(key,sizeof(key)-1,"BOOKMARK%d",j++);
            FXApp::instance()->reg().writeStringEntry(group.text(),key,name.text());
        }
    }
    while(i<=maxbookmarks);
}


// Remove all bookmarks from the list
void Bookmarks::clear()
{
	FXApp::instance()->reg().read();
	FXApp::instance()->reg().deleteSection(group.text());
	FXApp::instance()->reg().write();
}


// Clear the bookmarks list
long Bookmarks::onCmdClear(FXObject*,FXSelector,void*)
{
	if(BOX_CLICKED_CANCEL==MessageBox::question(FXApp::instance()->getActiveWindow(),BOX_OK_CANCEL,
	   _("Confirm Clear Bookmarks"),_("Do you really want to clear all your bookmarks?")))
		return 0;
	else
		clear();
	
	return 1;
}


// User clicks on one of the bookmark names
long Bookmarks::onCmdBookmark(FXObject*,FXSelector sel,void*)
{
    const char *bookname;
    char key[20];
    if(target)
    {
        snprintf(key,sizeof(key)-1,"BOOKMARK%d",(FXSELID(sel)-ID_BOOKMARK_1+1));
        bookname=FXApp::instance()->reg().readStringEntry(group.text(),key,NULL);
        if(bookname)
            target->handle(this,FXSEL(SEL_COMMAND,message),(void*)bookname);
    }
    return 1;
}


// Update handler for same
long Bookmarks::onUpdBookmark(FXObject *sender,FXSelector sel,void*)
{
    int which=FXSELID(sel)-ID_BOOKMARK_1+1;
    const char *bookname=NULL;
    FXString string;
    char key[20];
    char _char[11]={'a','b','c','d','e','f','g','h','i','j','k'};
    snprintf(key,sizeof(key)-1,"BOOKMARK%d",which);
    bookname=FXApp::instance()->reg().readStringEntry(group.text(),key,NULL);
    if(bookname)
    {
        FXString string;
        
        // Keyboard shortcut is 1, 2, 3...
        if(which<10)
        {
            string.format("&%d %s",which,bookname);
		}
        
        // Keyboard shortcut is a, b, c...
        else if(which<MAX_BOOKMARKS)
        {
            string.format("&%c %s",_char[which-10],bookname);
		}
        
        // Should not be used
        else
        {
            string.format("2&0 %s",bookname);
		}
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETSTRINGVALUE),(void*)&string);
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    }
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
    return 1;
}


// Show or hide depending on whether there are any bookmarks
long Bookmarks::onUpdAnyBookmarks(FXObject *sender,FXSelector,void*)
{
	FXApp::instance()->reg().deleteSection(group.text());
	FXApp::instance()->reg().read();
	if(FXApp::instance()->reg().readStringEntry(group.text(),"BOOKMARK1",NULL))
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SHOW),NULL);
    else
        sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_HIDE),NULL);
	
    return 1;
}

// Destructor
Bookmarks::~Bookmarks()
{
    target=(FXObject*)-1L;
}

