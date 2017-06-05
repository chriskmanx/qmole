// Dialog used to modify key bindings

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <fxkeys.h>

#include "icons.h"
#include "xfeutils.h"
#include "InputDialog.h"
#include "MessageBox.h"
#include "KeybindingsDialog.h"
#include "XFileExplorer.h"
#include "Keybindings.h"


// Minimum header size for lists
#ifndef MIN_HEADER_SIZE
#define MIN_HEADER_SIZE 50
#endif

// Main window
extern FXMainWindow *mainWindow;


FXDEFMAP(KeybindingsBox) KeybindingsBoxMap[]=
{
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_ACCEPT,KeybindingsBox::onCmdAccept),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_CANCEL,KeybindingsBox::onCmdCancel),
	FXMAPFUNC(SEL_DOUBLECLICKED,KeybindingsBox::ID_GLB_BINDINGS_LIST,KeybindingsBox::onCmdDefineGlbKeybindings),
	FXMAPFUNC(SEL_DOUBLECLICKED,KeybindingsBox::ID_XFE_BINDINGS_LIST,KeybindingsBox::onCmdDefineXfeKeybindings),
	FXMAPFUNC(SEL_DOUBLECLICKED,KeybindingsBox::ID_XFI_BINDINGS_LIST,KeybindingsBox::onCmdDefineXfiKeybindings),
	FXMAPFUNC(SEL_DOUBLECLICKED,KeybindingsBox::ID_XFW_BINDINGS_LIST,KeybindingsBox::onCmdDefineXfwKeybindings),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_GLB_SORT_BY_ACTIONNAME,KeybindingsBox::onCmdGlbSortByActionName),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_GLB_SORT_BY_REGISTRYKEY,KeybindingsBox::onCmdGlbSortByRegistryKey),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_GLB_SORT_BY_KEYBINDING,KeybindingsBox::onCmdGlbSortByKeyBinding),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFE_SORT_BY_ACTIONNAME,KeybindingsBox::onCmdXfeSortByActionName),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFE_SORT_BY_REGISTRYKEY,KeybindingsBox::onCmdXfeSortByRegistryKey),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFE_SORT_BY_KEYBINDING,KeybindingsBox::onCmdXfeSortByKeyBinding),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFI_SORT_BY_ACTIONNAME,KeybindingsBox::onCmdXfiSortByActionName),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFI_SORT_BY_REGISTRYKEY,KeybindingsBox::onCmdXfiSortByRegistryKey),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFI_SORT_BY_KEYBINDING,KeybindingsBox::onCmdXfiSortByKeyBinding),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFW_SORT_BY_ACTIONNAME,KeybindingsBox::onCmdXfwSortByActionName),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFW_SORT_BY_REGISTRYKEY,KeybindingsBox::onCmdXfwSortByRegistryKey),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFW_SORT_BY_KEYBINDING,KeybindingsBox::onCmdXfwSortByKeyBinding),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_GLB_BINDINGS_LIST,KeybindingsBox::onCmdGlbHeaderClicked),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFE_BINDINGS_LIST,KeybindingsBox::onCmdXfeHeaderClicked),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFI_BINDINGS_LIST,KeybindingsBox::onCmdXfiHeaderClicked),
	FXMAPFUNC(SEL_COMMAND,KeybindingsBox::ID_XFW_BINDINGS_LIST,KeybindingsBox::onCmdXfwHeaderClicked),
	FXMAPFUNC(SEL_UPDATE,KeybindingsBox::ID_GLB_BINDINGS_LIST,KeybindingsBox::onUpdGlbHeader),
	FXMAPFUNC(SEL_UPDATE,KeybindingsBox::ID_XFE_BINDINGS_LIST,KeybindingsBox::onUpdXfeHeader),
	FXMAPFUNC(SEL_UPDATE,KeybindingsBox::ID_XFI_BINDINGS_LIST,KeybindingsBox::onUpdXfiHeader),
	FXMAPFUNC(SEL_UPDATE,KeybindingsBox::ID_XFW_BINDINGS_LIST,KeybindingsBox::onUpdXfwHeader),
};


// Object implementation
FXIMPLEMENT(KeybindingsBox,DialogBox,KeybindingsBoxMap,ARRAYNUMBER(KeybindingsBoxMap))


KeybindingsBox::KeybindingsBox(FXWindow *win, FXStringDict* glbbindings, FXStringDict* xfebindings, FXStringDict* xfibindings, FXStringDict* xfwbindings):
        DialogBox(win,_("Key Bindings"), DECOR_TITLE|DECOR_BORDER|DECOR_RESIZE,0,0,800,600)
{
	glbBindingsDict=glbbindings;
	xfeBindingsDict=xfebindings;
	xfiBindingsDict=xfibindings;
	xfwBindingsDict=xfwbindings;
	
    // Buttons
    FXHorizontalFrame *buttons=new FXHorizontalFrame(this,LAYOUT_SIDE_BOTTOM|LAYOUT_FILL_X,0,0,0,0,10,10,5,5);

    // Contents
    FXHorizontalFrame *contents=new FXHorizontalFrame(this,LAYOUT_SIDE_TOP|FRAME_NONE|LAYOUT_FILL_X|LAYOUT_FILL_Y|PACK_UNIFORM_WIDTH);

	// Accept button
    FXButton *ok = new FXButton(buttons,_("&Accept"),NULL,this,KeybindingsBox::ID_ACCEPT,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);
    ok->addHotKey(KEY_Return);
    
    // Cancel button
    new FXButton(buttons,_("&Cancel"),NULL,this,KeybindingsBox::ID_CANCEL,FRAME_RAISED|FRAME_THICK|LAYOUT_RIGHT|LAYOUT_CENTER_Y,0,0,0,0,20,20);

	// Tab book
    FXTabBook *tabbook = new FXTabBook(contents,NULL,0,LAYOUT_FILL_X|LAYOUT_FILL_Y|LAYOUT_RIGHT);
    
	// First tab is global key bindings
	new FXTabItem(tabbook,_("&Global Key Bindings"),NULL);
    FXVerticalFrame *frame1=new FXVerticalFrame(tabbook,LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(frame1,_("These key bindings are common to all Xfe applications.\nDouble click on an item to modify the selected key binding..."),NULL,LAYOUT_LEFT|JUSTIFY_LEFT,0,0,0,0,0,0,20,20);
	glbBindingsList=new IconList(frame1,this,ID_GLB_BINDINGS_LIST,_ICONLIST_STANDARD|HSCROLLER_NEVER|ICONLIST_BROWSESELECT|LAYOUT_SIDE_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);

	// Second tab is Xfe key bindings
	new FXTabItem(tabbook,_("Xf&e Key Bindings"),NULL);
    FXVerticalFrame *frame2=new FXVerticalFrame(tabbook,LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(frame2,_("These key bindings are specific to the X File Explorer application.\nDouble click on an item to modify the selected key binding..."),NULL,LAYOUT_LEFT|JUSTIFY_LEFT,0,0,0,0,0,0,20,20);
	xfeBindingsList=new IconList(frame2,this,ID_XFE_BINDINGS_LIST,_ICONLIST_STANDARD|HSCROLLER_NEVER|ICONLIST_BROWSESELECT|LAYOUT_SIDE_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);

	// Third tab is Xfi key bindings
    new FXTabItem(tabbook,_("Xf&i Key Bindings"),NULL);
    FXVerticalFrame *frame3=new FXVerticalFrame(tabbook,LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(frame3,_("These key bindings are specific to the X File Image application.\nDouble click on an item to modify the selected key binding..."),NULL,LAYOUT_LEFT|JUSTIFY_LEFT,0,0,0,0,0,0,20,20);
	xfiBindingsList=new IconList(frame3,this,ID_XFI_BINDINGS_LIST,_ICONLIST_STANDARD|HSCROLLER_NEVER|ICONLIST_BROWSESELECT|LAYOUT_SIDE_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);

	// Fourth tab is Xfw key bindings
    new FXTabItem(tabbook,_("Xf&w Key Bindings"),NULL);
    FXVerticalFrame *frame4=new FXVerticalFrame(tabbook,LAYOUT_SIDE_TOP|FRAME_RAISED|LAYOUT_FILL_X|LAYOUT_FILL_Y);
	new FXLabel(frame4,_("These key bindings are specific to the X File Write application.\nDouble click on an item to modify the selected key binding..."),NULL,LAYOUT_LEFT|JUSTIFY_LEFT,0,0,0,0,0,0,20,20);
	xfwBindingsList=new IconList(frame4,this,ID_XFW_BINDINGS_LIST,_ICONLIST_STANDARD|HSCROLLER_NEVER|ICONLIST_BROWSESELECT|LAYOUT_SIDE_TOP|LAYOUT_LEFT|LAYOUT_FILL_X|LAYOUT_FILL_Y);

    // Set list headers name and size 
	unsigned int hsize1=getWidth()/2-50;
	unsigned int hsize2=getWidth()/4;
	
	glbBindingsList->appendHeader(_("Action Name"),NULL,hsize1);
	glbBindingsList->appendHeader(_("Registry Key"),NULL,hsize2);
	glbBindingsList->appendHeader(_("Key Binding"),NULL,hsize2);
	
	xfeBindingsList->appendHeader(_("Action Name"),NULL,hsize1);
	xfeBindingsList->appendHeader(_("Registry Key"),NULL,hsize2);
	xfeBindingsList->appendHeader(_("Key Binding"),NULL,hsize2);
	
	xfiBindingsList->appendHeader(_("Action Name"),NULL,hsize1);
	xfiBindingsList->appendHeader(_("Registry Key"),NULL,hsize2);
	xfiBindingsList->appendHeader(_("Key Binding"),NULL,hsize2);
	
	xfwBindingsList->appendHeader(_("Action Name"),NULL,hsize1);
	xfwBindingsList->appendHeader(_("Registry Key"),NULL,hsize2);
	xfwBindingsList->appendHeader(_("Key Binding"),NULL,hsize2);

	// Initialize sort functions
	glbBindingsList->setSortFunc(ascendingActionName);
	xfeBindingsList->setSortFunc(ascendingActionName);
	xfiBindingsList->setSortFunc(ascendingActionName);
	xfwBindingsList->setSortFunc(ascendingActionName);
	
	// Initialize initial binding dicts
	glbBindingsDict_prev=new FXStringDict();
	xfeBindingsDict_prev=new FXStringDict();
	xfiBindingsDict_prev=new FXStringDict();
	xfwBindingsDict_prev=new FXStringDict();

	// Changed flag
	changed=FALSE;
}


// Create window
void KeybindingsBox::create()
{
    DialogBox::create();

	int i;
	FXString str, data, action, keybinding;
	
	// Fullfill the four lists
	glbBindingsList->clearItems();
    for(i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
	{
		data=glbBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+glbBindingsDict->key(i)+TAB+keybinding;
		glbBindingsList->appendItem(str);
	}
	glbBindingsList->sortItems();

	xfeBindingsList->clearItems();
    for(i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
	{
		data=xfeBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfeBindingsDict->key(i)+TAB+keybinding;
		xfeBindingsList->appendItem(str);
	}
	xfeBindingsList->sortItems();

	xfiBindingsList->clearItems();
    for(i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
	{
		data=xfiBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfiBindingsDict->key(i)+TAB+keybinding;
		xfiBindingsList->appendItem(str);
	}
	xfiBindingsList->sortItems();

	xfwBindingsList->clearItems();
    for(i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
	{
		data=xfwBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfwBindingsDict->key(i)+TAB+keybinding;
		xfwBindingsList->appendItem(str);
	}
	xfwBindingsList->sortItems();

	// Deselect all items
	glbBindingsList->killSelection();
	xfeBindingsList->killSelection();
	xfiBindingsList->killSelection();
	xfwBindingsList->killSelection();
}


// Delete objects
KeybindingsBox::~KeybindingsBox()
{
	delete glbBindingsList;
	delete xfeBindingsList;
	delete xfiBindingsList;
	delete xfwBindingsList;
	delete glbBindingsDict_prev;
	delete xfeBindingsDict_prev;
	delete xfiBindingsDict_prev;
	delete xfwBindingsDict_prev;
}
		

// Changes are accepted		
long KeybindingsBox::onCmdAccept(FXObject* o,FXSelector s,void* p)
{
	// If some key binding was modified
	if (changed)
	{
		// Write keybindings to the registry
		FXString data, regkey, keybinding;
		for (int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
		{
			regkey=glbBindingsDict->key(i);
			data=glbBindingsDict->data(i);
			keybinding=data.after('\t');		
  			getApp()->reg().writeStringEntry("KEYBINDINGS",regkey.text(),keybinding.text());
		}
		for (int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
		{
			regkey=xfeBindingsDict->key(i);
			data=xfeBindingsDict->data(i);
			keybinding=data.after('\t');		
  			getApp()->reg().writeStringEntry("KEYBINDINGS",regkey.text(),keybinding.text());
		}
		for (int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
		{
			regkey=xfiBindingsDict->key(i);
			data=xfiBindingsDict->data(i);
			keybinding=data.after('\t');		
  			getApp()->reg().writeStringEntry("KEYBINDINGS",regkey.text(),keybinding.text());
		}
		for (int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
		{
			regkey=xfwBindingsDict->key(i);
			data=xfwBindingsDict->data(i);
			keybinding=data.after('\t');		
  			getApp()->reg().writeStringEntry("KEYBINDINGS",regkey.text(),keybinding.text());
		}
		
		// Update the registry
		getApp()->reg().write();
		
		// Reinit the changed flag
    	changed=FALSE;

		// Ask the user if he wants to restart Xfe
		if(BOX_CLICKED_CANCEL!=MessageBox::question(this,BOX_OK_CANCEL,_("Restart"),_("Key bindings will be changed after restart.\nRestart X File Explorer now?")))
			mainWindow->handle(this,FXSEL(SEL_COMMAND,XFileExplorer::ID_RESTART),NULL);
	}
    DialogBox::onCmdAccept(o,s,p);
	return 1;
}


// Changes are cancelled
long KeybindingsBox::onCmdCancel(FXObject* o,FXSelector s,void* p)
{
	// Restore initial (i.e. before modification) binding dicts
 	FXString data, regkey;
    for (int i=glbBindingsDict_prev->first(); i<glbBindingsDict_prev->size(); i=glbBindingsDict_prev->next(i))
	{
		regkey=glbBindingsDict_prev->key(i);
		data=glbBindingsDict_prev->data(i);
		glbBindingsDict->replace(regkey.text(),data.text());
	}
    for (int i=xfeBindingsDict_prev->first(); i<xfeBindingsDict_prev->size(); i=xfeBindingsDict_prev->next(i))
	{
		regkey=xfeBindingsDict_prev->key(i);
		data=xfeBindingsDict_prev->data(i);
		xfeBindingsDict->replace(regkey.text(),data.text());
	}
    for (int i=xfiBindingsDict_prev->first(); i<xfiBindingsDict_prev->size(); i=xfiBindingsDict_prev->next(i))
	{
		regkey=xfiBindingsDict_prev->key(i);
		data=xfiBindingsDict_prev->data(i);
		xfiBindingsDict->replace(regkey.text(),data.text());
	}
    for (int i=xfwBindingsDict_prev->first(); i<xfwBindingsDict_prev->size(); i=xfwBindingsDict_prev->next(i))
	{
		regkey=xfwBindingsDict_prev->key(i);
		data=xfwBindingsDict_prev->data(i);
		xfwBindingsDict->replace(regkey.text(),data.text());
	}
	
	// Reinit the changed flag
    changed=FALSE;
	
	DialogBox::onCmdCancel(o,s,p);
	return 0;
}


// Compare sectioned strings
int KeybindingsBox::compareSection(const char *p,const char* q,int s)
{
    register int c1,c2,x;
    for (x=s; x && *p; x-=(*p++=='\t'));
    for (x=s; x && *q; x-=(*q++=='\t'));
    do
    {
        c1=(unsigned char)(*p++);
        c2=(unsigned char)(*q++);
    }
    while ('\t'<c1 && (c1==c2));
    return c1-c2;
}


// Sort functions
int KeybindingsBox::ascendingActionName(const IconItem* a,const IconItem* b)
{
    return compareSection(a->getText().text(),b->getText().text(),0);
}


int KeybindingsBox::descendingActionName(const IconItem* a,const IconItem* b)
{
    return compareSection(b->getText().text(),a->getText().text(),0);
}

int KeybindingsBox::ascendingRegistryKey(const IconItem* a,const IconItem* b)
{
    return compareSection(a->getText().text(),b->getText().text(),1);
}


int KeybindingsBox::descendingRegistryKey(const IconItem* a,const IconItem* b)
{
    return compareSection(b->getText().text(),a->getText().text(),1);
}


int KeybindingsBox::ascendingKeybinding(const IconItem* a,const IconItem* b)
{
    return compareSection(a->getText().text(),b->getText().text(),2);
}


int KeybindingsBox::descendingKeybinding(const IconItem* a,const IconItem* b)
{
    return compareSection(b->getText().text(),a->getText().text(),2);
}


// Sort global list by action name
long KeybindingsBox::onCmdGlbSortByActionName(FXObject*,FXSelector,void*)
{
	glbBindingsList->setSortFunc((glbBindingsList->getSortFunc()==ascendingActionName) ? descendingActionName : ascendingActionName);
	glbBindingsList->setSortHeader(0);
	glbBindingsList->clearItems();
    FXString str, data, action, keybinding;
	for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
	{	
		data=glbBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+glbBindingsDict->key(i)+TAB+keybinding;
		glbBindingsList->appendItem(str);
	}
	glbBindingsList->sortItems();

    return 1;
}


// Sort global list by registry key name
long KeybindingsBox::onCmdGlbSortByRegistryKey(FXObject*,FXSelector,void*)
{
	glbBindingsList->setSortFunc((glbBindingsList->getSortFunc()==ascendingRegistryKey) ? descendingRegistryKey : ascendingRegistryKey);
	glbBindingsList->setSortHeader(1);
	glbBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
	{
		data=glbBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+glbBindingsDict->key(i)+TAB+keybinding;
		glbBindingsList->appendItem(str);
	}
	glbBindingsList->sortItems();

    return 1;
}

// Sort global list by key binding
long KeybindingsBox::onCmdGlbSortByKeyBinding(FXObject*,FXSelector,void*)
{
	glbBindingsList->setSortFunc((glbBindingsList->getSortFunc()==ascendingKeybinding) ? descendingKeybinding : ascendingKeybinding);
	glbBindingsList->setSortHeader(2);
	glbBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
	{
		data=glbBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+glbBindingsDict->key(i)+TAB+keybinding;
		glbBindingsList->appendItem(str);
	}
	glbBindingsList->sortItems();

    return 1;
}


// Sort Xfe list by action name
long KeybindingsBox::onCmdXfeSortByActionName(FXObject*,FXSelector,void*)
{
	xfeBindingsList->setSortFunc((xfeBindingsList->getSortFunc()==ascendingActionName) ? descendingActionName : ascendingActionName);
	xfeBindingsList->setSortHeader(0);
	xfeBindingsList->clearItems();
    FXString str, data, action, keybinding;
	for(int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
	{	
		data=xfeBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfeBindingsDict->key(i)+TAB+keybinding;
		xfeBindingsList->appendItem(str);
	}
	xfeBindingsList->sortItems();

    return 1;
}

// Sort Xfe list by registry key name
long KeybindingsBox::onCmdXfeSortByRegistryKey(FXObject*,FXSelector,void*)
{
	xfeBindingsList->setSortFunc((xfeBindingsList->getSortFunc()==ascendingRegistryKey) ? descendingRegistryKey : ascendingRegistryKey);
	xfeBindingsList->setSortHeader(1);
	xfeBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
	{
		data=xfeBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfeBindingsDict->key(i)+TAB+keybinding;
		xfeBindingsList->appendItem(str);
	}
	xfeBindingsList->sortItems();

    return 1;
}

// Sort Xfe list by key binding
long KeybindingsBox::onCmdXfeSortByKeyBinding(FXObject*,FXSelector,void*)
{
	xfeBindingsList->setSortFunc((xfeBindingsList->getSortFunc()==ascendingKeybinding) ? descendingKeybinding : ascendingKeybinding);
	xfeBindingsList->setSortHeader(2);
	xfeBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
	{
		data=xfeBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfeBindingsDict->key(i)+TAB+keybinding;
		xfeBindingsList->appendItem(str);
	}
	xfeBindingsList->sortItems();

    return 1;
}

// Sort Xfi list by action name
long KeybindingsBox::onCmdXfiSortByActionName(FXObject*,FXSelector,void*)
{
	xfiBindingsList->setSortFunc((xfiBindingsList->getSortFunc()==ascendingActionName) ? descendingActionName : ascendingActionName);
	xfiBindingsList->setSortHeader(0);
	xfiBindingsList->clearItems();
    FXString str, data, action, keybinding;
	for(int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
	{
		data=xfiBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfiBindingsDict->key(i)+TAB+keybinding;
		xfiBindingsList->appendItem(str);
	}
	xfiBindingsList->sortItems();

    return 1;
}

// Sort Xfi list by registry key name
long KeybindingsBox::onCmdXfiSortByRegistryKey(FXObject*,FXSelector,void*)
{
	xfiBindingsList->setSortFunc((xfiBindingsList->getSortFunc()==ascendingRegistryKey) ? descendingRegistryKey : ascendingRegistryKey);
	xfiBindingsList->setSortHeader(1);
	xfiBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
	{
		data=xfiBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfiBindingsDict->key(i)+TAB+keybinding;
		xfiBindingsList->appendItem(str);
	}
	xfiBindingsList->sortItems();

    return 1;
}

// Sort Xfi list by key binding
long KeybindingsBox::onCmdXfiSortByKeyBinding(FXObject*,FXSelector,void*)
{
	xfiBindingsList->setSortFunc((xfiBindingsList->getSortFunc()==ascendingKeybinding) ? descendingKeybinding : ascendingKeybinding);
	xfiBindingsList->setSortHeader(2);
	xfiBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
	{
		data=xfiBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfiBindingsDict->key(i)+TAB+keybinding;
		xfiBindingsList->appendItem(str);
	}
	xfiBindingsList->sortItems();

    return 1;
}


// Sort Xfw list by action name
long KeybindingsBox::onCmdXfwSortByActionName(FXObject*,FXSelector,void*)
{
	xfwBindingsList->setSortFunc((xfwBindingsList->getSortFunc()==ascendingActionName) ? descendingActionName : ascendingActionName);
	xfwBindingsList->setSortHeader(0);
	xfwBindingsList->clearItems();
    FXString str, data, action, keybinding;
	for(int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
	{
		data=xfwBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfwBindingsDict->key(i)+TAB+keybinding;
		xfwBindingsList->appendItem(str);
	}
	xfwBindingsList->sortItems();

    return 1;
}

// Sort Xfw list by registry key name
long KeybindingsBox::onCmdXfwSortByRegistryKey(FXObject*,FXSelector,void*)
{
	xfwBindingsList->setSortFunc((xfwBindingsList->getSortFunc()==ascendingRegistryKey) ? descendingRegistryKey : ascendingRegistryKey);
	xfwBindingsList->setSortHeader(1);
	xfwBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
	{
		data=xfwBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfwBindingsDict->key(i)+TAB+keybinding;
		xfwBindingsList->appendItem(str);
	}
	xfwBindingsList->sortItems();

    return 1;
}

// Sort Xfw list by key binding
long KeybindingsBox::onCmdXfwSortByKeyBinding(FXObject*,FXSelector,void*)
{
	xfwBindingsList->setSortFunc((xfwBindingsList->getSortFunc()==ascendingKeybinding) ? descendingKeybinding : ascendingKeybinding);
	xfwBindingsList->setSortHeader(2);
	xfwBindingsList->clearItems();
    FXString str, data, action, keybinding;
    for(int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
	{
		data=xfwBindingsDict->data(i);
		action= data.before('\t');
		keybinding=data.after('\t');		
		str=action+TAB+xfwBindingsDict->key(i)+TAB+keybinding;
		xfwBindingsList->appendItem(str);
	}
	xfwBindingsList->sortItems();

    return 1;
}


// Clicked on a global list header button
long KeybindingsBox::onCmdGlbHeaderClicked(FXObject*,FXSelector,void* ptr)
{	
	unsigned int num=(unsigned int)(FXuval)ptr;
		
	if (num >= 0 && num <3)
	{
		if (num==0)
			handle(this,FXSEL(SEL_COMMAND,ID_GLB_SORT_BY_ACTIONNAME),NULL);
		else if (num==1)
			handle(this,FXSEL(SEL_COMMAND,ID_GLB_SORT_BY_REGISTRYKEY),NULL);
		else if (num==2)
			handle(this,FXSEL(SEL_COMMAND,ID_GLB_SORT_BY_KEYBINDING),NULL);
	}

    return 1;
}


// Clicked on a Xfe list header button
long KeybindingsBox::onCmdXfeHeaderClicked(FXObject*,FXSelector,void* ptr)
{	
	unsigned int num=(unsigned int)(FXuval)ptr;
		
	if (num >= 0 && num <3)
	{
		if (num==0)
			handle(this,FXSEL(SEL_COMMAND,ID_XFE_SORT_BY_ACTIONNAME),NULL);
		else if (num==1)
			handle(this,FXSEL(SEL_COMMAND,ID_XFE_SORT_BY_REGISTRYKEY),NULL);
		else if (num==2)
			handle(this,FXSEL(SEL_COMMAND,ID_XFE_SORT_BY_KEYBINDING),NULL);
	}

    return 1;
}

// Clicked on a Xfi list header button
long KeybindingsBox::onCmdXfiHeaderClicked(FXObject*,FXSelector,void* ptr)
{
	unsigned int num=(unsigned int)(FXuval)ptr;
	if (num >= 0 && num <3)
	{
		if (num==0)
			handle(this,FXSEL(SEL_COMMAND,ID_XFI_SORT_BY_ACTIONNAME),NULL);
		else if (num==1)
			handle(this,FXSEL(SEL_COMMAND,ID_XFI_SORT_BY_REGISTRYKEY),NULL);
		else if (num==2)
			handle(this,FXSEL(SEL_COMMAND,ID_XFI_SORT_BY_KEYBINDING),NULL);
	}

    return 1;
}

// Clicked on a Xfw list header button
long KeybindingsBox::onCmdXfwHeaderClicked(FXObject*,FXSelector,void* ptr)
{
	unsigned int num=(unsigned int)(FXuval)ptr;
	if (num >= 0 && num <3)
	{
		if (num==0)
			handle(this,FXSEL(SEL_COMMAND,ID_XFW_SORT_BY_ACTIONNAME),NULL);
		else if (num==1)
			handle(this,FXSEL(SEL_COMMAND,ID_XFW_SORT_BY_REGISTRYKEY),NULL);
		else if (num==2)
			handle(this,FXSEL(SEL_COMMAND,ID_XFW_SORT_BY_KEYBINDING),NULL);
	}

    return 1;
}


// Update global list header
long KeybindingsBox::onUpdGlbHeader(FXObject*,FXSelector,void*)
{	
	// Update header arrow
  	glbBindingsList->getHeader()->setArrowDir(0,(glbBindingsList->getSortFunc()==ascendingActionName) ? FALSE : (glbBindingsList->getSortFunc()==descendingActionName)? TRUE : MAYBE);
  	glbBindingsList->getHeader()->setArrowDir(1,(glbBindingsList->getSortFunc()==ascendingRegistryKey) ? FALSE : (glbBindingsList->getSortFunc()==descendingRegistryKey)? TRUE : MAYBE);
  	glbBindingsList->getHeader()->setArrowDir(2,(glbBindingsList->getSortFunc()==ascendingKeybinding) ? FALSE : (glbBindingsList->getSortFunc()==descendingKeybinding)? TRUE : MAYBE);

	// Set minimum header size
	if (glbBindingsList->getHeaderSize(0)<MIN_HEADER_SIZE)
		glbBindingsList->setHeaderSize(0,MIN_HEADER_SIZE);
	if (glbBindingsList->getHeaderSize(1)<MIN_HEADER_SIZE)
		glbBindingsList->setHeaderSize(1,MIN_HEADER_SIZE);
	if (glbBindingsList->getHeaderSize(2)<MIN_HEADER_SIZE)
		glbBindingsList->setHeaderSize(2,MIN_HEADER_SIZE);

    return 1;
}


// Update Xfe list header
long KeybindingsBox::onUpdXfeHeader(FXObject*,FXSelector,void*)
{	
	// Update header arrow
  	xfeBindingsList->getHeader()->setArrowDir(0,(xfeBindingsList->getSortFunc()==ascendingActionName) ? FALSE : (xfeBindingsList->getSortFunc()==descendingActionName)? TRUE : MAYBE);
  	xfeBindingsList->getHeader()->setArrowDir(1,(xfeBindingsList->getSortFunc()==ascendingRegistryKey) ? FALSE : (xfeBindingsList->getSortFunc()==descendingRegistryKey)? TRUE : MAYBE);
  	xfeBindingsList->getHeader()->setArrowDir(2,(xfeBindingsList->getSortFunc()==ascendingKeybinding) ? FALSE : (xfeBindingsList->getSortFunc()==descendingKeybinding)? TRUE : MAYBE);

	// Set minimum header size
	if (xfeBindingsList->getHeaderSize(0)<MIN_HEADER_SIZE)
		xfeBindingsList->setHeaderSize(0,MIN_HEADER_SIZE);
	if (xfeBindingsList->getHeaderSize(1)<MIN_HEADER_SIZE)
		xfeBindingsList->setHeaderSize(1,MIN_HEADER_SIZE);
	if (xfeBindingsList->getHeaderSize(2)<MIN_HEADER_SIZE)
		xfeBindingsList->setHeaderSize(2,MIN_HEADER_SIZE);

    return 1;
}


// Update Xfi list header
long KeybindingsBox::onUpdXfiHeader(FXObject*,FXSelector,void*)
{	
	// Update header arrow
  	xfiBindingsList->getHeader()->setArrowDir(0,(xfiBindingsList->getSortFunc()==ascendingActionName) ? FALSE : (xfiBindingsList->getSortFunc()==descendingActionName)? TRUE : MAYBE);
  	xfiBindingsList->getHeader()->setArrowDir(1,(xfiBindingsList->getSortFunc()==ascendingRegistryKey) ? FALSE : (xfiBindingsList->getSortFunc()==descendingRegistryKey)? TRUE : MAYBE);
  	xfiBindingsList->getHeader()->setArrowDir(2,(xfiBindingsList->getSortFunc()==ascendingKeybinding) ? FALSE : (xfiBindingsList->getSortFunc()==descendingKeybinding)? TRUE : MAYBE);

	// Set minimum header size
	if (xfiBindingsList->getHeaderSize(0)<MIN_HEADER_SIZE)
		xfiBindingsList->setHeaderSize(0,MIN_HEADER_SIZE);
	if (xfiBindingsList->getHeaderSize(1)<MIN_HEADER_SIZE)
		xfiBindingsList->setHeaderSize(1,MIN_HEADER_SIZE);
	if (xfiBindingsList->getHeaderSize(2)<MIN_HEADER_SIZE)
		xfiBindingsList->setHeaderSize(2,MIN_HEADER_SIZE);

    return 1;
}


// Update Xfw list header
long KeybindingsBox::onUpdXfwHeader(FXObject*,FXSelector,void*)
{	
	// Update header arrow
  	xfwBindingsList->getHeader()->setArrowDir(0,(xfwBindingsList->getSortFunc()==ascendingActionName) ? FALSE : (xfwBindingsList->getSortFunc()==descendingActionName)? TRUE : MAYBE);
  	xfwBindingsList->getHeader()->setArrowDir(1,(xfwBindingsList->getSortFunc()==ascendingRegistryKey) ? FALSE : (xfwBindingsList->getSortFunc()==descendingRegistryKey)? TRUE : MAYBE);
  	xfwBindingsList->getHeader()->setArrowDir(2,(xfwBindingsList->getSortFunc()==ascendingKeybinding) ? FALSE : (xfwBindingsList->getSortFunc()==descendingKeybinding)? TRUE : MAYBE);

	// Set minimum header size
	if (xfwBindingsList->getHeaderSize(0)<MIN_HEADER_SIZE)
		xfwBindingsList->setHeaderSize(0,MIN_HEADER_SIZE);
	if (xfwBindingsList->getHeaderSize(1)<MIN_HEADER_SIZE)
		xfwBindingsList->setHeaderSize(1,MIN_HEADER_SIZE);
	if (xfwBindingsList->getHeaderSize(2)<MIN_HEADER_SIZE)
		xfwBindingsList->setHeaderSize(2,MIN_HEADER_SIZE);

    return 1;
}


// Double clicked on an item in the global list
long KeybindingsBox::onCmdDefineGlbKeybindings(FXObject*,FXSelector,void*)
{
	// Get selected item string
	FXString itemtext="";
	int index=-1;
	for (int u=0; u< glbBindingsList->getNumItems (); u++)
	{
		if (glbBindingsList->isItemSelected(u))
		{
			itemtext=glbBindingsList->getItemText(u);
			index=u;
		}
	}
	if (index<0) // Should not happen
		return 0;
		
	// Decompose item text
	FXString data=itemtext.rbefore('\t');
	FXString key=itemtext.rafter('\t');
	FXString action=data.before('\t');
	FXString regkey=data.after('\t');

 	// Input dialog
	FXString message;
	message.format(_("Press the combination of keys you want to use for the action: %s"),action.text());
	message = message+ "\n" + _("[Press space to disable the key binding for this action]");
	KeybindingsDialog* kbdialog=new KeybindingsDialog(this,key,message,_("Modify Key Binding"),keybindingsicon);

	// Accept was pressed
    if(kbdialog->execute(PLACEMENT_CURSOR))
    {		
		// Convert the entered string into a valid key binding string
		FXString newkey=kbdialog->getKey();
		if (newkey=="Space")
			newkey="";
		
		// Check if the new key binding is not already used elsewhere
		if (newkey!="")
		{
			FXString dictdata, dictkey;
			FXbool exist_in_glb=FALSE, exist_in_xfe=FALSE, exist_in_xfi=FALSE, exist_in_xfw=FALSE;
			for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
			{
				dictdata=glbBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_glb=TRUE;
					break;
				}
			}
			if (exist_in_glb)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the global section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
			for(int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
			{
				dictdata=xfeBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_xfe=TRUE;
					break;
				}
			}
			if (exist_in_xfe)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the Xfe section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
			for(int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
			{
				dictdata=xfiBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_xfi=TRUE;
					break;
				}
			}
			if (exist_in_xfi)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the Xfi section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
			for(int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
			{
				dictdata=xfwBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_xfw=TRUE;
					break;
				}
			}
			if (exist_in_xfw)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the Xfw section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
		}
		
		// Modify the item text
		itemtext=data+TAB+newkey;
		glbBindingsList->setItemText(index,itemtext);

		// Update dictionary
		FXString str=action+TAB+newkey;
		glbBindingsDict->replace(regkey.text(),str.text());
		
		changed=TRUE;
	}	

	// Cancel was pressed
	else
    {
		delete kbdialog;
		return 0;
    }

	delete kbdialog;
	return 1;

}


// Double clicked on an item in the Xfe list
long KeybindingsBox::onCmdDefineXfeKeybindings(FXObject*,FXSelector,void*)
{
	// Get selected item string
	FXString itemtext="";
	int index=-1;
	for (int u=0; u< xfeBindingsList->getNumItems (); u++)
	{
		if (xfeBindingsList->isItemSelected(u))
		{
			itemtext=xfeBindingsList->getItemText(u);
			index=u;
		}
	}
	if (index<0) // Should not happen
		return 0;
		
	// Decompose item text
	FXString data=itemtext.rbefore('\t');
	FXString key=itemtext.rafter('\t');
	FXString action=data.before('\t');
	FXString regkey=data.after('\t');
 
 	// Input dialog
	FXString message;
	message.format(_("Press the combination of keys you want to use for the action: %s"),action.text());
	message = message+ "\n" + _("[Press space to disable the key binding for this action]");
	KeybindingsDialog* kbdialog=new KeybindingsDialog(this,key,message,_("Modify Key Binding"),keybindingsicon);

	// Accept was pressed
    if(kbdialog->execute(PLACEMENT_CURSOR))
    {		
		// Convert the entered string into a valid key binding string
		FXString newkey=kbdialog->getKey();
		if (newkey=="Space")
			newkey="";
		
		// Check if the new key binding is not already used elsewhere
		if (newkey!="")
		{
			FXString dictdata, dictkey;
			FXbool exist_in_glb=FALSE, exist_in_xfe=FALSE;
			for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
			{
				dictdata=glbBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_glb=TRUE;
					break;
				}
			}
			if (exist_in_glb)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the global section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
			for(int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
			{
				dictdata=xfeBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_xfe=TRUE;
					break;
				}
			}
			if (exist_in_xfe)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the Xfe section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
		}
		
		// Modify the item text
		itemtext=data+TAB+newkey;
		xfeBindingsList->setItemText(index,itemtext);

		// Update dictionary
		FXString str=action+TAB+newkey;
		xfeBindingsDict->replace(regkey.text(),str.text());
		
		changed=TRUE;
	}	

	// Cancel was pressed
	else
    {
		delete kbdialog;
		return 0;
    }

	delete kbdialog;
	return 1;

}


// Double clicked on an item in the Xfi list
long KeybindingsBox::onCmdDefineXfiKeybindings(FXObject*,FXSelector,void*)
{
	// Get selected item string
	FXString itemtext="";
	int index=-1;
	for (int u=0; u< xfiBindingsList->getNumItems (); u++)
	{
		if (xfiBindingsList->isItemSelected(u))
		{
			itemtext=xfiBindingsList->getItemText(u);
			index=u;
		}
	}
	if (index<0) // Should not happen
		return 0;
		
	// Decompose item text
	FXString data=itemtext.rbefore('\t');
	FXString key=itemtext.rafter('\t');
	FXString action=data.before('\t');
	FXString regkey=data.after('\t');
 
 	// Input dialog
	FXString message;
	message.format(_("Press the combination of keys you want to use for the action: %s"),action.text());
	message = message+ "\n" + _("[Press space to disable the key binding for this action]");
	KeybindingsDialog* kbdialog=new KeybindingsDialog(this,key,message,_("Modify Key Binding"),keybindingsicon);

	// Accept was pressed
    if(kbdialog->execute(PLACEMENT_CURSOR))
    {		
		// Convert the entered string into a valid key binding string
		FXString newkey=kbdialog->getKey();
		if (newkey=="Space")
			newkey="";
		
		// Check if the new key binding is not already used elsewhere
		if (newkey!="")
		{
			FXString dictdata, dictkey;
			FXbool exist_in_glb=FALSE, exist_in_xfi=FALSE;
			for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
			{
				dictdata=glbBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_glb=TRUE;
					break;
				}
			}
			if (exist_in_glb)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the global section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
			for(int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
			{
				dictdata=xfiBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_xfi=TRUE;
					break;
				}
			}
			if (exist_in_xfi)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the Xfi section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
		}
		
		// Modify the item text
		itemtext=data+TAB+newkey;
		xfiBindingsList->setItemText(index,itemtext);

		// Update dictionary
		FXString str=action+TAB+newkey;
		xfiBindingsDict->replace(regkey.text(),str.text());
		
		changed=TRUE;
	}	

	// Cancel was pressed
	else
    {
		delete kbdialog;
		return 0;
    }

	delete kbdialog;
	return 1;

}


// Double clicked on an item in the Xfw list
long KeybindingsBox::onCmdDefineXfwKeybindings(FXObject*,FXSelector,void*)
{
	// Get selected item string
	FXString itemtext="";
	int index=-1;
	for (int u=0; u< xfwBindingsList->getNumItems (); u++)
	{
		if (xfwBindingsList->isItemSelected(u))
		{
			itemtext=xfwBindingsList->getItemText(u);
			index=u;
		}
	}
	if (index<0) // Should not happen
		return 0;
		
	// Decompose item text
	FXString data=itemtext.rbefore('\t');
	FXString key=itemtext.rafter('\t');
	FXString action=data.before('\t');
	FXString regkey=data.after('\t');
 
 	// Input dialog
	FXString message;
	message.format(_("Press the combination of keys you want to use for the action: %s"),action.text());
	message = message+ "\n" + _("[Press space to disable the key binding for this action]");
	KeybindingsDialog* kbdialog=new KeybindingsDialog(this,key,message,_("Modify Key Binding"),keybindingsicon);

	// Accept was pressed
    if(kbdialog->execute(PLACEMENT_CURSOR))
    {		
		// Convert the entered string into a valid key binding string
		FXString newkey=kbdialog->getKey();
		if (newkey=="Space")
			newkey="";
		
		// Check if the new key binding is not already used elsewhere
		if (newkey!="")
		{
			FXString dictdata, dictkey;
			FXbool exist_in_glb=FALSE, exist_in_xfw=FALSE;
			for(int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
			{
				dictdata=glbBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_glb=TRUE;
					break;
				}
			}
			if (exist_in_glb)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the global section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
			for(int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
			{
				dictdata=xfwBindingsDict->data(i);
				dictkey=dictdata.after('\t');
				if (dictkey==newkey)
				{
					exist_in_xfw=TRUE;
					break;
				}
			}
			if (exist_in_xfw)
			{
				MessageBox::error(this,BOX_OK,_("Error"),_("The key binding %s is already used in the Xfw section.\n\
	You should erase the existing key binding before assigning it again."),newkey.text());
				
				delete kbdialog;
				return 0;
			}
		}
		
		// Modify the item text
		itemtext=data+TAB+newkey;
		xfwBindingsList->setItemText(index,itemtext);

		// Update dictionary
		FXString str=action+TAB+newkey;
		xfwBindingsDict->replace(regkey.text(),str.text());
		
		changed=TRUE;
	}	

	// Cancel was pressed
	else
    {
		delete kbdialog;
		return 0;
    }

	delete kbdialog;
	return 1;

}


// Execute dialog box modally
unsigned int KeybindingsBox::execute(unsigned int placement)
{
	// Save binding dicts for cancel purpose
 	FXString data, regkey;
    for (int i=glbBindingsDict->first(); i<glbBindingsDict->size(); i=glbBindingsDict->next(i))
	{
		regkey=glbBindingsDict->key(i);
		data=glbBindingsDict->data(i);
		glbBindingsDict_prev->replace(regkey.text(),data.text());
	}
    for (int i=xfeBindingsDict->first(); i<xfeBindingsDict->size(); i=xfeBindingsDict->next(i))
	{
		regkey=xfeBindingsDict->key(i);
		data=xfeBindingsDict->data(i);
		xfeBindingsDict_prev->replace(regkey.text(),data.text());
	}
    for (int i=xfiBindingsDict->first(); i<xfiBindingsDict->size(); i=xfiBindingsDict->next(i))
	{
		regkey=xfiBindingsDict->key(i);
		data=xfiBindingsDict->data(i);
		xfiBindingsDict_prev->replace(regkey.text(),data.text());
	}
    for (int i=xfwBindingsDict->first(); i<xfwBindingsDict->size(); i=xfwBindingsDict->next(i))
	{
		regkey=xfwBindingsDict->key(i);
		data=xfwBindingsDict->data(i);
		xfwBindingsDict_prev->replace(regkey.text(),data.text());
	}
 
	// Execute dialog 
	unsigned int ret=DialogBox::execute(placement);
    return ret;
}

