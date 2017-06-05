#ifndef MSWidgetINLINES
#define MSWidgetINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else 
#define INLINELINKAGE
#endif

INLINELINKAGE MSWidget *MSWidget::owner(void)
{ return _owner; }  
INLINELINKAGE const MSWidget *MSWidget::owner(void) const                
{ return _owner; }  
INLINELINKAGE MSDisplayServer *MSWidget::server(void)
{ return _server; }
INLINELINKAGE const MSDisplayServer *MSWidget::server(void) const               
{ return _server; }
INLINELINKAGE Display *MSWidget::display(void) const
{ return _server->display(); }

INLINELINKAGE Colormap MSWidget::colormap(void) const                   
{ return _server->colormap(); }
INLINELINKAGE Window MSWidget::window(void) const                   
{ return _window; }

INLINELINKAGE MSColorManager *MSWidget::colorManager(void)
{ return _server->colorManager(); }
INLINELINKAGE const MSColorManager *MSWidget::colorManager(void) const              
{ return _server->colorManager(); }

INLINELINKAGE MSFontManager *MSWidget::fontManager(void)
{ return _server->fontManager(); }
INLINELINKAGE const MSFontManager *MSWidget::fontManager(void) const              
{ return _server->fontManager(); }

INLINELINKAGE int MSWidget::x_origin(void) const                    
{ return MSRect::x(); }
INLINELINKAGE int MSWidget::y_origin(void) const                    
{ return MSRect::y(); }
INLINELINKAGE int MSWidget::width(void) const                       
{ return MSRect::width(); }
INLINELINKAGE int MSWidget::height(void) const                      
{ return MSRect::height(); }
INLINELINKAGE unsigned long MSWidget::background(void) const        
{ return _bg; }
INLINELINKAGE unsigned long MSWidget::foreground(void) const        
{ return _fg; }
INLINELINKAGE Font MSWidget::font(void) const                       
{ return _fontID; }
INLINELINKAGE MSBoolean MSWidget::sensitive(void) const             
{ return _sensitive; }
INLINELINKAGE MSBoolean MSWidget::readOnly(void) const             
{ return _readOnly; }
INLINELINKAGE MSBoolean MSWidget::acceptFocus(void) const           
{ return _acceptFocus; }
INLINELINKAGE MSBoolean MSWidget::acceptTab(void) const             
{ return _acceptTab; }
INLINELINKAGE MSBoolean MSWidget::dynamic(void) const               
{ return _dynamic; }
INLINELINKAGE MSBoolean MSWidget::visible(void) const               
{ return _visible; }
INLINELINKAGE MSBoolean MSWidget::frozen(void) const                
{ return _freezeStatus; }
INLINELINKAGE MSBoolean MSWidget::firstMap(void) const              
{ return _firstMap; }
INLINELINKAGE MSBoolean MSWidget::mapped(void) const                
{ return _mapped; }
INLINELINKAGE unsigned long MSWidget::resizeConstraints(void) const 
{ return _resizeConstraints; }
INLINELINKAGE long MSWidget::eventMask(void) const 
{ return _eventMask; }
INLINELINKAGE MSWidget *MSWidget::focusWindow(void) 
{ return _focusWindow; }

INLINELINKAGE MSKeyTranslationTable *MSWidget::keyTranslationTable(void) 
{ return &_keyTranslationTable; }

INLINELINKAGE Screen *MSWidget::screen(int n_) const                
{ return ScreenOfDisplay(display(),n_); } 
INLINELINKAGE Screen *MSWidget::screen(void) const                  
{ return _server->screen(); } 
INLINELINKAGE int MSWidget::depth(void) const                       
{ return DefaultDepthOfScreen(screen()); }
INLINELINKAGE unsigned long MSWidget::addKeyCallback( const char* pString_, MSKeyCallback* keyCallback_)
{  return keyTranslationTable()->addCallback(pString_,keyCallback_);}
INLINELINKAGE void MSWidget::removeKeyCallback( unsigned long identifier_ )
{  keyTranslationTable()->removeCallback(identifier_);}
INLINELINKAGE void MSWidget::removeKeyCallback( const char* pString_)
{  keyTranslationTable()->removeCallback(pString_);}
INLINELINKAGE MSBoolean MSWidget::hasKeyCallback( const MSKeyPress& keyPress_) 
{ return keyTranslationTable()->hasMatch(keyPress_); }
INLINELINKAGE void MSWidget::removeAllKeyCallbacks()
{  keyTranslationTable()->removeAll(); }



#endif









