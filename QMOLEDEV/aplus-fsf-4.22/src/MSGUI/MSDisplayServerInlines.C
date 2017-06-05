#ifndef MSDisplayServerINLINES
#define MSDisplayServerINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

INLINELINKAGE Display *MSDisplayServer::display(void) const
{ return _dpy; }
INLINELINKAGE const char *MSDisplayServer::name(void) const                
{ return _name; }
INLINELINKAGE MSColorManager *MSDisplayServer::colorManager(void) const              
{ return _colorManager; }
INLINELINKAGE MSFontManager *MSDisplayServer::fontManager(void) const                 
{ return _fontManager; }
INLINELINKAGE MSAtomTable& MSDisplayServer::atomTable(void)          
{ return _atomTable; }
INLINELINKAGE const MSAtomTable& MSDisplayServer::atomTable(void) const
{ return _atomTable; }
INLINELINKAGE MSHashTable *MSDisplayServer::shadowHashTable(void) const    
{ return _shadowHashTable; }
INLINELINKAGE MSHashTable *MSDisplayServer::widgetHashTable(void) const
{ return _widgetHashTable; }
INLINELINKAGE MSHashTable *MSDisplayServer::toolTipHashTable(void) const
{ return _toolTipHashTable; }
INLINELINKAGE const MSString& MSDisplayServer::copyBuffer(void) const
{ return _copyBuffer; }
INLINELINKAGE const MSString& MSDisplayServer::pasteBuffer(void) const
{ return _pasteBuffer; }
INLINELINKAGE MSDisplayCursor *MSDisplayServer::watch(void) const                
{ return _watchCursor; }
INLINELINKAGE Font MSDisplayServer::defaultFont(void) const		
{ return _defaultFont; }  			      
INLINELINKAGE unsigned long MSDisplayServer::defaultForeground(void) const	
{ return _defaultFg; }  
INLINELINKAGE unsigned long MSDisplayServer::defaultBackground(void) const	
{ return _defaultBg; }
INLINELINKAGE Window MSDisplayServer::keyboardGrabber(void) const
{ return _keyboardGrabber; }
INLINELINKAGE Window MSDisplayServer::pointerGrabber(void) const
{ return _pointerGrabber; }
INLINELINKAGE unsigned long MSDisplayServer::defaultValueBackground(void) const	
{ return _defaultValueBg; }
INLINELINKAGE unsigned long MSDisplayServer::defaultHighlightColor(void) const	
{ return _defaultHighlightColor; }
INLINELINKAGE const MSWidget *MSDisplayServer::primarySelectionOwner(void) const
{ return _primarySelectionOwner; }
INLINELINKAGE MSWidget *MSDisplayServer::primarySelectionOwner(void)
{ return _primarySelectionOwner; }
INLINELINKAGE void MSDisplayServer::primarySelectionOwner(MSWidget *primarySelectionOwner_)
{ _primarySelectionOwner=primarySelectionOwner_; }
INLINELINKAGE MSWidget *MSDisplayServer::menuGrabber(void) 
{ return _menuGrabber; }
INLINELINKAGE void MSDisplayServer::menuGrabber(MSWidget* menuGrabber_)
{ _menuGrabber=menuGrabber_; }
INLINELINKAGE MSWidget *MSDisplayServer::menuBar(void) 
{ return _menuBar; }
INLINELINKAGE void MSDisplayServer::menuBar(MSWidget* menuBar_)
{ _menuBar=menuBar_; }
INLINELINKAGE MSDisplayCursor *MSDisplayServer::menuGrabCursor(void)
{ return _menuGrabCursor; }
INLINELINKAGE MSWidget* MSDisplayServer::scrollBarMenu(void)
{ return _scrollBarMenu; }
INLINELINKAGE void MSDisplayServer::scrollBarMenu(MSWidget *menu_)
{ _scrollBarMenu=menu_; }

#endif
