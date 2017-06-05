#ifndef MSTreeViewIMPLEMENTATION
#define MSTreeViewIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTreeView.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSBusy.H>
#include <MSTypes/MSStandardOps.H> // look for elementForOps
#include <MSGUI/MSTreeFormatFunctions.H>

const int MSTreeViewDefaultButtonShadowThickness=2;
const int MSTreeViewDefaultButtonNodeMargin=2;
const unsigned long MSTreeViewEventMask=(ExposureMask|ButtonPressMask);
const int MSTreeViewDefaultNodeMargin=8;
const int MSTreeViewDefaultMargin=10;
const int MSTreeViewMinimumEditorWidth=75;

const int MSTreeViewDefaultHorizontalSpacing=25;
const int MSTreeViewDefaultVerticalSpacing=15;

#define MSTreeViewCollapsedButtonPixmapName "__MSTreeViewCollapsedButtonPixmap__"
#define MSTreeViewExpandedButtonPixmapName  "__MSTreeViewExpandedButtonPixmap__"

#define MSTreeView_ExpandedPixmap_width 10
#define MSTreeView_ExpandedPixmap_height 10

#define MSTreeView_CollapsedPixmap_width 10
#define MSTreeView_CollapsedPixmap_height 10

//NodeAttribute

#if !defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
template<class Element>
MSTreeView<Element>::NodeAttribute::NodeAttribute(const NodeAttribute &nodeAttr_)
:_sensitive(nodeAttr_.sensitive()),
 _expandable(nodeAttr_.expandable()),
 _expandedState(nodeAttr_.expandedState()),
 _pixmap(nodeAttr_.pixmap()),
 _insensitivePixmap(nodeAttr_.insensitivePixmap()),
 _selectedPixmap(nodeAttr_.selectedPixmap())
{}

template<class Element>
MSTreeView<Element>::NodeAttribute::NodeAttribute(void)
:_sensitive(MSTrue),_expandable(MSFalse),_expandedState(MSFalse)
{}

template<class Element>
MSTreeView<Element>::NodeAttribute::~NodeAttribute(void)
{}

template<class Element>
typename MSTreeView<Element>::NodeAttribute &MSTreeView<Element>::NodeAttribute::operator=(const NodeAttribute &nodeAttr_)
{
  if (&nodeAttr_!=this)
   {
     _sensitive=nodeAttr_.sensitive();
     _expandable=nodeAttr_.expandable();
     _expandedState=nodeAttr_.expandedState();
     _pixmap=nodeAttr_.pixmap();
     _insensitivePixmap=nodeAttr_.insensitivePixmap();
     _selectedPixmap=nodeAttr_.selectedPixmap();
   }
  return *this;
}

template<class Element>
MSBoolean MSTreeView<Element>::NodeAttribute::operator==(const NodeAttribute &nodeAttr_)
{
  if (sensitive()==nodeAttr_.sensitive()&&
      expandable()==nodeAttr_.expandable()&&
      expandedState()==nodeAttr_.expandedState()&&
      pixmap()==nodeAttr_.pixmap()&&
      insensitivePixmap()==nodeAttr_.insensitivePixmap()&&
      selectedPixmap()==nodeAttr_.selectedPixmap())
   {
     return MSTrue;
   }
  else
   {
     return MSFalse;
   }
}
#endif

#if !defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
// Editor 
template<class Element>
MSTreeView<Element>::Editor::Editor(MSWidget *owner_) : MSTextField(owner_)
{
  _highlightThickness=0;
  color(owner()->background(),owner()->foreground());
  selectInput(ExposureMask|ButtonPressMask);
}

template<class Element>
MSTreeView<Element>::Editor::~Editor(void)
{}

template<class Element>
void MSTreeView<Element>::Editor::activate(void) 
{ ((MSTreeView<Element> *)(owner()->owner()))->editorActivate(); }

template<class Element>
void MSTreeView<Element>::Editor::escape(void)
{ ((MSTreeView<Element> *)(owner()->owner()))->editorEscape(); }

#endif // !MS_INLINE_TEMPLATE_NESTED_CLASS

// MSTreeView
template<class Element>
MSTreeView<Element>::MSTreeView(MSWidget *owner_,TreeModel &model_) :
MSScrollableWidget(owner_),
_nodeShadow(server()),
_selectedShadow(server()),
_selectedCursor(_elementTree),
_activatedNode(0),
_redrawPixmap(0)
{
  init();
  couple(&model_);
}

template<class Element>
MSTreeView<Element>::MSTreeView(MSWidget *owner_) :
MSScrollableWidget(owner_),
_nodeShadow(server()),
_selectedShadow(server()),
_selectedCursor(_elementTree),
_activatedNode(0),
_redrawPixmap(0)
{ init(); }

template<class Element>
void MSTreeView<Element>::init(void)
{
  _pPopupMenu=0;
  _orientation=Horizontal;
  _showButtons=MSTrue;
  _showLabels=MSTrue;
  _showPixmaps=MSTrue;
  _buttonShadowThickness=MSTreeViewDefaultButtonShadowThickness;
  _buttonNodeMargin=MSTreeViewDefaultButtonNodeMargin;
  _collapsedButtonPixmap=0;
  _expandedButtonPixmap=0;
  _showRootNode=MSTrue;
  _horizontalSpacing=MSTreeViewDefaultHorizontalSpacing;
  _verticalSpacing=MSTreeViewDefaultVerticalSpacing;
  _maxWidth=0;
  _maxHeight=0;
  _margin=MSTreeViewDefaultMargin;
  _nodeMargin=MSTreeViewDefaultNodeMargin;
  _pEditor=new Editor(viewPort());
  _pEditor->xMargin(2);
  selectInput(MSTreeViewEventMask);

  _lineForeground=foreground();
  _nodeForeground=foreground();
  _nodeBackground=background();
  _selectedNodeForeground=_nodeBackground;
  _selectedNodeBackground=_nodeForeground;

  _nodeShadow.color(_nodeBackground);
  _selectedShadow.color(_selectedNodeBackground);
  
  editor()->color(_selectedNodeBackground,_selectedNodeForeground);  
  addToFocusList();

  _stipplePixmap=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);

  XGCValues values;    
  values.foreground=foreground();
  values.background=background();
  values.font=font(); 
  values.stipple=stipplePixmap()->pixmap();
  values.fill_style=FillSolid;

  _textGC=XCreateGC(display(),window(),(GCForeground|GCBackground|GCFont|GCStipple|GCFillStyle),
                    &values);
  fontObject().fontStruct(server()->fontStruct(font()));

  values.foreground=foreground();
  values.background=background();
  _pixmapGC=XCreateGC(display(),window(),(GCForeground|GCBackground),&values);

  XSetWindowAttributes attrs;
  attrs.backing_store=WhenMapped;
  XChangeWindowAttributes(display(),viewPort()->window(),CWBackingStore,&attrs);


  buildCollapsedButtonPixmap();
  buildExpandedButtonPixmap();
}

template <class Element>
void MSTreeView<Element>::buildCollapsedButtonPixmap(void)
{
  static char MSTreeView_CollapsedPixmap_bits[] = {
   0x00, 0x00, 0x78, 0x00, 0xfc, 0x00, 0xfe, 0x01, 0xfe, 0x01, 0xfe, 0x01,
   0xfe, 0x01, 0xfc, 0x00, 0x78, 0x00, 0x00, 0x00};

  if (_collapsedButtonPixmap!=0) delete _collapsedButtonPixmap;
  _collapsedButtonPixmap=new MSPixmap(server(),MSTreeViewCollapsedButtonPixmapName,
				      MSTreeView_CollapsedPixmap_bits,
                                      MSTreeView_CollapsedPixmap_width,
                                      MSTreeView_CollapsedPixmap_height);
}

template <class Element>
void MSTreeView<Element>::buildExpandedButtonPixmap(void)
{
  static char MSTreeView_ExpandedPixmap_bits[] = {
   0x00, 0x00, 0x78, 0x00, 0xfc, 0x00, 0xce, 0x01, 0x86, 0x01, 0x86, 0x01,
   0xce, 0x01, 0xfc, 0x00, 0x78, 0x00, 0x00, 0x00};

  if (_expandedButtonPixmap!=0) delete _expandedButtonPixmap;
  _expandedButtonPixmap=new MSPixmap(server(),MSTreeViewExpandedButtonPixmapName,
				     MSTreeView_ExpandedPixmap_bits,
                                     MSTreeView_ExpandedPixmap_width,
                                     MSTreeView_ExpandedPixmap_height);
}

template<class Element>
void MSTreeView<Element>::lineForeground(unsigned long pixel_)
{
  if (pixel_!=_lineForeground)
   {
     _lineForeground=pixel_;
     redrawViewPortImmediately();
   }
}
template<class Element>
void MSTreeView<Element>::nodeForeground(unsigned long pixel_)
{
  if (pixel_!=_nodeForeground)
   {
     if(selectedNodeBackground()==nodeForeground())
      {
        _selectedNodeBackground=pixel_;
        _selectedShadow.color(_selectedNodeBackground);
      }
     _nodeForeground=pixel_;
     redrawViewPortImmediately();
   }
}
template<class Element>
void MSTreeView<Element>::nodeBackground(unsigned long pixel_)
{
  if (pixel_!=_nodeBackground)
   {
     if(selectedNodeForeground()==nodeBackground())
      {
        _selectedNodeForeground=pixel_;
      }
     _nodeBackground=pixel_;
     _nodeShadow.color(_nodeBackground);
     redrawViewPortImmediately();
   }
}
template<class Element>
void MSTreeView<Element>::selectedNodeForeground(unsigned long pixel_)
{
  if (pixel_!=_selectedNodeForeground)
   {
     _selectedNodeForeground=pixel_;
     redrawViewPortImmediately();
   }
}
template<class Element>
void MSTreeView<Element>::selectedNodeBackground(unsigned long pixel_)
{
  if (pixel_!=_selectedNodeBackground)
   {
     _selectedNodeBackground=pixel_;
     _selectedShadow.color(_selectedNodeBackground);
     redrawViewPortImmediately();
   }
}

template<class Element>
void MSTreeView<Element>::lineForeground(const char *pString_)
{ lineForeground(server()->pixel(pString_)); }
template<class Element>
void MSTreeView<Element>::nodeForeground(const char *pString_)
{ nodeForeground(server()->pixel(pString_)); }
template<class Element>
void MSTreeView<Element>::nodeBackground(const char *pString_)
{ nodeBackground(server()->pixel(pString_)); }
template<class Element>
void MSTreeView<Element>::selectedNodeForeground(const char *pString_)
{ selectedNodeForeground(server()->pixel(pString_)); }
template<class Element>
void MSTreeView<Element>::selectedNodeBackground(const char *pString_)
{ selectedNodeBackground(server()->pixel(pString_)); }

template<class Element>
MSTreeView<Element>::~MSTreeView(void)
{
  if (_pPopupMenu!=0) _pPopupMenu->destroy();
  _pEditor->destroy();
  if (_collapsedButtonPixmap!=0) delete _collapsedButtonPixmap;
  if (_expandedButtonPixmap!=0) delete _expandedButtonPixmap;
  if (textGC()!=0) XFreeGC(display(),_textGC);
  if (pixmapGC()!=0) XFreeGC(display(),_pixmapGC);
  if (stipplePixmap()!=0) delete stipplePixmap();
  if (redrawPixmap()!=0) delete _redrawPixmap;
}

template<class Element>
void MSTreeView<Element>::popupMenu(MSPopupMenu *popupMenu_)
{
  if (_pPopupMenu!=0) _pPopupMenu->destroy();
  _pPopupMenu=popupMenu_;
}

template<class Element>
void MSTreeView<Element>::orientation(MSTreeView<Element>::Orientation orientation_)
{
  if (orientation()!=orientation_)
   {
     _orientation=orientation_;
     XFillRectangle(display(),viewPort()->window(),backgroundShadowGC(),0,0,viewPort()->width(),viewPort()->height());
     adjustSize();
   }
}

template<class Element>
void MSTreeView<Element>::verticalSpacing(int value_)
{
  if (verticalSpacing()!=value_)
   {
     _verticalSpacing=value_;
     adjustSize();
   }
}

template<class Element>
void MSTreeView<Element>::horizontalSpacing(int value_)
{
  if (horizontalSpacing()!=value_)
   {
     _horizontalSpacing=value_;
     adjustSize();
   }
}

template<class Element>
void MSTreeView<Element>::editorActivate(void)
{
  if (selectedCursor().isValid()==True)
   {
     TreeNode &aNode=nodeAt(selectedCursor());
     MSString currentString;
     formatOutput(currentString,aNode.cursor());
     if (editor()->text()!=currentString)     
      {
        if (validate(aNode.cursor(),editor()->string())==MSTrue)
         {
           editor()->unmap();
           modelTree().elementChanged(aNode.cursor());
         }
        else server()->bell();
      }
     else editor()->unmap();
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::validate(TreeModelCursor &cursor_,const char *pString_)
{
  if (msTreeSetType(elementForOps(modelTree().elementAt(cursor_)),pString_)==MSError::MSSuccess) return MSTrue;
  else return MSFalse;
}

template<class Element>
const char *MSTreeView<Element>::formatOutput(MSString &buffer_,TreeModelCursor &cursor_)
{
  if (cursor_.isValid()) msTreeFormatType(elementForOps(modelTree().elementAt(cursor_)),buffer_);
  return buffer_.string();
}

template <class Element>
void MSTreeView<Element>::nodeModified(const TreeModelCursor &cursor_,NodeAttribute &attribute_)
{ nodeAttribute(cursor_,attribute_); }

template <class Element>
void MSTreeView<Element>::nodeAttribute(const TreeModelCursor &,NodeAttribute &)
{}


template<class Element>
void MSTreeView<Element>::editorEscape(void)
{
  loseFocusNotify(editor());
  editor()->unmap();
}

template<class Element>
void MSTreeView<Element>::mapEditor(void)
{
  if (showLabels()==MSTrue&&selectedCursor().isValid()==True)
   {
     TreeNode& aNode=nodeAt(selectedCursor());
     int x=aNode.x()-hsb()->value();
     int y=aNode.y()-vsb()->value();
     int pw=0,ph=0;
     if (showPixmaps()==MSTrue)
      {
        calculatePixmapSize(aNode.selectedPixmap(),pw,ph);
      }
     pw+=(pw>0)?buttonNodeMargin():0;
     x+=pw;
     int th=fontObject().textHeight()+nodeMargin();
     if (th<ph) y+=(ph-th)/2;
     editor()->moveTo(x,y);
     int ww=aNode.width()-pw;
     int hh=th;
     
     if (showButtons()==MSTrue&&aNode.expandable()==MSTrue)
      {
        if (orientation()==Horizontal)
         {
           ww-=(collapsedButtonPixmap().width()+buttonShadowThickness()*2+buttonNodeMargin());
         }
      }
     editor()->resize((ww>MSTreeViewMinimumEditorWidth)?ww:MSTreeViewMinimumEditorWidth,hh);
     editor()->foreground(selectedNodeBackground());
     editor()->background(selectedNodeForeground());
     editor()->map();
     takeFocusNotify(editor());
   }
}

template<class Element>
void MSTreeView<Element>::firstMapNotify(void)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  adjustSize();
  MSScrollableWidget::firstMapNotify();
}

template<class Element>
void MSTreeView<Element>::redrawViewPortImmediately(void)
{
  if (firstMap()==MSTrue)
   {
     drawTree();
     if (mapped()==MSTrue) redrawViewPort();
   }
}

template<class Element>
void MSTreeView<Element>::redrawViewPort(void)
{
  if (frozen()==MSFalse)
   {
     XCopyArea(display(),redrawPixmap()->pixmap(),viewPort()->window(),textGC(),
               hsb()->value(),vsb()->value(),viewPort()->width(),viewPort()->height(),0,0);
   }
}

template<class Element>
void MSTreeView<Element>::moveDrawArea(int x_,int y_)
{
  if (x_!=drawAreaWidget()->x()||y_!=drawAreaWidget()->y())
   {
     MSScrollableWidget::moveDrawArea(x_,y_);
     if (editor()->mapped()==MSTrue)
      {
        TreeNode& aNode=nodeAt(selectedCursor());
        int x=aNode.x()-hsb()->value();
        int y=aNode.y()-vsb()->value();
        editor()->moveTo(x,y);
      }
     if (mapped()==MSTrue) redrawViewPort();
   }
}

template<class Element>
void MSTreeView<Element>::adjustSize(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     MSShellBusy busy((MSShell *)top());
     recompVectors();
     _maxWidth=0;
     _maxHeight=0;
     computeMaxExtents();
     int offset=2*(margin()+shadowThickness()+highlightThickness());
     int ww=maxWidth()+offset;
     int hh=maxHeight()+offset;
     resizeRedrawPixmap(ww,hh);
     drawTree();
     if (ww!=drawAreaWidget()->width()||hh!=drawAreaWidget()->height())
      {
        resizeDrawArea(ww,hh);
      }
     redrawViewPort();
   }
}

template<class Element>
void MSTreeView<Element>::resizeRedrawPixmap(int w_,int h_)
{
  // We only want to create new pixmap if new size is bigger than what we already had
  if (redrawPixmap()!=0&&redrawPixmap()->width()>=w_&&redrawPixmap()->height()>=h_) return;

  if (redrawPixmap()!=0) delete _redrawPixmap;
  if(w_>0 && h_>0)
   {
     MSString name((unsigned long)this);
     _redrawPixmap=new MSPixmap(server(),name,w_,h_,foreground(),background());
   }
}


template<class Element>
int MSTreeView<Element>::computeTreeDepth(void)
{
  ElementTreeCursor treeCursor(elementTree());
  if(_startCursor.isValid()) treeCursor=_startCursor;
  else treeCursor.setToRoot();
  if(showRootNode()==MSFalse)
   {
     // root node is not shown and doesn't exist or is not expandable then
     // the depth should be 0, as nothing will be shown.
     if(treeCursor.isValid()==MSFalse) return 0;
     
     const TreeNode &aNode= nodeAt(treeCursor);
     if(aNode.expandable()==MSFalse || aNode.expandedState()==MSFalse)
      {
        return 0;
      }
   }
  return computeTreeDepth(treeCursor,1);
}

template<class Element>
int MSTreeView<Element>::computeTreeDepth(ElementTreeCursor treeCursor_,int depth_)
{
  if (treeCursor_.isValid()==MSTrue)
   {
     const TreeNode &aNode=nodeAt(treeCursor_);
     if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
      {
        depth_++;
        for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
         {
           int maxDepth=computeTreeDepth(treeCursor_,depth_);
           depth_=(maxDepth>depth_)?maxDepth:depth_;
         }
      }
   }
  return depth_;
}

template<class Element>
void MSTreeView<Element>::recompVectors(void)
{
  if (frozen()==MSFalse)
   {
     int offset=margin()+shadowThickness()+highlightThickness();
     int depth=computeTreeDepth();
     if (depth>_verticalPositions.length())
      {
        _verticalPositions=_horizontalPositions=MSIntVector(depth,offset);
      }
     else
      {
        _verticalPositions=offset;
        _horizontalPositions=offset;
      }
     if(depth>0) computePositions();
     setPositions();
   }
}

template<class Element>
void MSTreeView<Element>::computeMaxExtents(void)
{
  ElementTreeCursor treeCursor(elementTree());
  if(_startCursor.isValid()) treeCursor=_startCursor;
  else treeCursor.setToRoot();
  if (treeCursor.isValid()==True) computeMaxExtents(treeCursor);
}

template<class Element>
void MSTreeView<Element>::drawTree(void)
{
  if (frozen()==MSFalse)
   {
     XSetForeground(display(),textGC(),background());
     XFillRectangle(display(),redrawPixmap()->pixmap(),textGC(),0,0,redrawPixmap()->width(),redrawPixmap()->height());
     ElementTreeCursor treeCursor(elementTree());
     if(_startCursor.isValid()) treeCursor=_startCursor;
     else treeCursor.setToRoot();
     if (treeCursor.isValid()==True)
      {
        if (showRootNode()==MSTrue) drawTree(treeCursor,redrawPixmap()->pixmap());
        // do not draw the root - i.e. draw the tree as if it has multiple roots
        else
         {
           const TreeNode &aNode=nodeAt(treeCursor);
           if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
            {
              for (treeCursor.setToFirstExistingChild();treeCursor.isValid();treeCursor.setToNextExistingChild())
               {
                 drawTree(treeCursor,redrawPixmap()->pixmap());
               }
            }
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::drawTree(ElementTreeCursor treeCursor_,Window window_)
{
  if (frozen()==MSFalse)
   {
     drawNode(treeCursor_,window_,MSFalse);
     TreeNode& aNode=nodeAt(treeCursor_);
     int rootX,rootY;
     if (orientation()==Horizontal)
      {
        rootX=aNode.x()+aNode.width();
        rootY=aNode.y()+aNode.height()/2;
      }
     else
      {
        rootX=aNode.x()+aNode.width()/2;
        rootY=aNode.y()+aNode.height();
      }
     if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
      {
        for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
         {
           TreeNode& aChildInstance=nodeAt(treeCursor_);
           XSetForeground(display(),textGC(),foreground());
           int childX,childY;
           if (orientation()==Horizontal)
            {
              childX=aChildInstance.x();
              childY=aChildInstance.y()+(aChildInstance.height()/2);
            }
           else
            {
              childX=aChildInstance.x()+(aChildInstance.width()/2);
              childY=aChildInstance.y();
            }
           XSetForeground(display(),textGC(),lineForegroundColor(&aChildInstance));
           XDrawLine(display(),window_,textGC(),rootX,rootY,childX,childY);
           drawTree(treeCursor_,window_);
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::drawNode(ElementTreeCursor &treeCursor_,Window window_,MSBoolean clear_)
{
  TreeNode& aNode=nodeAt(treeCursor_);
  if (aNode.width()>0&&aNode.height()>0)
   {
     if (clear_==MSTrue)
      {
        XFillRectangle(display(),window_,backgroundShadowGC(),aNode.x(),aNode.y(),
                       aNode.width(),aNode.height());
      }
     MSBoolean displayButton=MSBoolean(showButtons()==MSTrue&&aNode.expandable()==MSTrue);

     // bw and bh are values to be deducted from the node width and node height
     // based on whether the collapse/expand button will be drawn and the orientation
     int bw,bh;
     MSPixmap *buttonPixmap=0;
     if (displayButton==MSTrue)
      {
        if (orientation()==Horizontal)
         {
           if (aNode.expandedState()==MSTrue) buttonPixmap=_expandedButtonPixmap;
           else buttonPixmap=_collapsedButtonPixmap;
           bw=buttonPixmap->width()+buttonShadowThickness()*2+buttonNodeMargin();
           bh=0;
         }
        else
         {
           if (aNode.expandedState()==MSTrue) buttonPixmap=_expandedButtonPixmap;
           else buttonPixmap=_collapsedButtonPixmap;
           bh=buttonPixmap->height()+buttonShadowThickness()*2+buttonNodeMargin();
           bw=0;
         }
      }
     else
      {
        bh=0;
        bw=0;
      }

     // Figure out the sensitivity and the selected state of the node
     MSBoolean sensitive=isSensitive(&aNode);
     MSBoolean selected;
     if (selectedCursor().isValid()==True&&selectedCursor()==treeCursor_) selected=MSTrue;
     else selected=MSFalse;

     // Draw the pixmaps if necessary.  hLimit specifies the available height for
     // pixmap rendering.  The drawPixmaps() method potentially alters the parameter
     // x.  The new x value will be the starting position for the rest of the drawing.
     // i.e., label and button.
     int x=aNode.x();
     int y=aNode.y();
     int hLimit=aNode.height()-bh;
     if (showPixmaps()==MSTrue)
      {
        if (selected==MSTrue) drawPixmaps(window_,selectedPixmap(&aNode),x,y,hLimit);
        else if (sensitive==MSTrue) drawPixmaps(window_,pixmap(&aNode),x,y,hLimit);
        else drawPixmaps(window_,insensitivePixmap(&aNode),x,y,hLimit);
      }
     // Draw the label if necessary.  pw is the width consumed by the pixmaps.
     // th is text height, and tw is text width.
     int pw=x-aNode.x();
     int th=0;
     int tw=0;
     if (showLabels()==MSTrue)
      {
        MSString aString;
        formatOutput(aString,aNode.cursor());

        th=fontObject().textHeight()+nodeMargin();
        if (th<hLimit) y+=(hLimit-th)/2;
        tw=aNode.width()-pw-bw;

        unsigned long bg=nodeBackgroundColor(&aNode);
        MSShadow *pShadow=0;
        GC gc=0;
        if (selected==MSTrue)
         {
           pShadow=&_selectedShadow;
           if (bg==selectedNodeBackground())
            {
              gc=_selectedShadow.backgroundShadowGC();
            }
           else
            {
              gc=textGC();
              XSetForeground(display(),gc,bg);
            }
         }
        else
         {
           pShadow=&_nodeShadow;
           if (bg==nodeBackground())
            {
              gc=_nodeShadow.backgroundShadowGC();
            }
           else
            {
              gc=textGC();
              XSetForeground(display(),gc,bg);
            }
         }
  
        XFillRectangle(display(),window_,gc,x+2,y+2,tw-4,th-4);
        drawShadow(window_,MSRect(x,y,tw,th),2,
                   pShadow->topShadowGC(),pShadow->bottomShadowGC());
  
        unsigned long fg=nodeForegroundColor(&aNode);
        XSetForeground(display(),textGC(),fg);
        if (sensitive==MSFalse) XSetFillStyle(display(),textGC(),FillStippled);
        XDrawString(display(),window_,textGC(),fontObject().fontStruct(),
                    x+4,y+fontObject().textAscent()+4,
                    aString,aString.length());
        if (sensitive==MSFalse) XSetFillStyle(display(),textGC(),FillSolid);
      }

     // Draw the button if necessary
     if (displayButton==MSTrue)
      {
        int startx,starty;
        if (orientation()==Horizontal)
         { 
           startx=aNode.x()+aNode.width()-buttonPixmap->width()-buttonShadowThickness();
           starty=aNode.y()+(aNode.height()-(buttonPixmap->height()+buttonShadowThickness()*2))/2+buttonShadowThickness();
         }
        else
         {
           startx=aNode.x()+(aNode.width()/2)-(buttonPixmap->width()/2);
           starty=aNode.y()+aNode.height()-(buttonPixmap->height()+buttonShadowThickness()*2)+buttonShadowThickness();
         }
        XSetForeground(display(),pixmapGC(),foreground());
	copyPixmap(display(),*buttonPixmap,window_,pixmapGC(),startx,starty);

        if (buttonShadowThickness()>0)
         {
           drawShadow(window_,
                      MSRect(startx-buttonShadowThickness(),starty-buttonShadowThickness(),
                             buttonPixmap->width()+buttonShadowThickness()*2,
                             buttonPixmap->height()+buttonShadowThickness()*2),
                      buttonShadowThickness(),topShadowGC(),bottomShadowGC());
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::drawPixmaps(Window window_,const PixmapList &pixmapList_,
                                      int &x_,int &y_,int hLimit_)
{
  unsigned long nElements=pixmapList_.length();
  for (unsigned long i=0;i<nElements;i++)
   {
     const MSPixmap &pixmap=pixmapList_.elementAt(i);
     int y=y_+(hLimit_-pixmap.height())/2;
     copyPixmap(display(),pixmap,window_,pixmapGC(),x_,y);
     x_+=(pixmap.width()+buttonNodeMargin());
   }
}

template<class Element>
void MSTreeView<Element>::drawShadow(Window window_,const MSRect& aRect_,
                                                       int thickness_,GC topGC_,GC bottomGC_)
{
  XPoint points[7];
  if (thickness_>0)
   {
     points[0].x=points[1].x=points[6].x=aRect_.x();
     points[0].y=points[6].y=aRect_.y()+aRect_.height();
     points[1].y=points[2].y=aRect_.y();
     points[2].x=aRect_.x()+aRect_.width();
     points[3].x=aRect_.x()+aRect_.width()-thickness_;
     points[3].y=points[4].y=aRect_.y()+thickness_;
     points[4].x=points[5].x=aRect_.x()+thickness_;
     points[5].y=aRect_.y()+aRect_.height()-thickness_;
     
     XBFillRectangle(display(),window_,bottomGC_,
		     aRect_.x(),aRect_.y()+aRect_.height()-thickness_,
		     aRect_.width(),thickness_);
     XBFillRectangle(display(),window_,bottomGC_,
		     aRect_.x()+aRect_.width()-thickness_,aRect_.y(),
		     thickness_,aRect_.height());
     XBFillPolygon(display(),window_,topGC_,points,7,Nonconvex,CoordModeOrigin);
   }
}


template<class Element>
void MSTreeView<Element>::shiftVerticalPosition(ElementTreeCursor treeCursor_,int y_)
{
  TreeNode& aNode=nodeAt(treeCursor_);
  aNode.y(aNode.y()+y_);
  for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
   {
     shiftVerticalPosition(treeCursor_,y_);
   }
}

template<class Element>
void MSTreeView<Element>::shiftHorizontalPosition(ElementTreeCursor treeCursor_,int x_)
{
  TreeNode& aNode=nodeAt(treeCursor_);
  aNode.x(aNode.x()+x_);
  for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
   {
     shiftHorizontalPosition(treeCursor_,x_);
   }
}

template<class Element>
void MSTreeView<Element>::setPositions(void)
{
  ElementTreeCursor treeCursor(elementTree());
  if(_startCursor.isValid()) treeCursor=_startCursor;
  else treeCursor.setToRoot();
  if (treeCursor.isValid()==True)
   {
     setPositions(treeCursor,0);
     if (showRootNode()==MSFalse)
      {
        TreeNode& aNode=nodeAt(treeCursor);
        int offset;
        if (orientation()==Horizontal)
         {
           offset=aNode.width()+horizontalSpacing();
           shiftHorizontalPosition(treeCursor,-offset);
         }
        else
         {
           offset=aNode.height()+verticalSpacing();
           shiftVerticalPosition(treeCursor,-offset);
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::setPositions(ElementTreeCursor treeCursor_,int depth_)
{
  TreeNode& aNode=nodeAt(treeCursor_);
  int sum=0;
  int j;
  int offset=margin()+shadowThickness()+highlightThickness();
  if (orientation()==Vertical)
   {
     for (j=0;j<depth_;j++) sum+=_verticalPositions(j);
     aNode.y(depth_*verticalSpacing()+sum+offset);
   }
  else
   {
     for (j=0;j<depth_;j++) sum+=_horizontalPositions(j);
     aNode.x(depth_*horizontalSpacing()+sum+offset);
   }
  for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
   {
     setPositions(treeCursor_,depth_+1);
   }
}

template<class Element>
void MSTreeView<Element>::computeMaxExtents(ElementTreeCursor treeCursor_)
{
  TreeNode& aNode=nodeAt(treeCursor_);
  int w=aNode.x()+aNode.width();
  int h=aNode.y()+aNode.height();
  _maxWidth=(w>maxWidth())?w:maxWidth();
  _maxHeight=(h>maxHeight())?h:maxHeight();
  if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
   {
     for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
      {
        computeMaxExtents(treeCursor_);
      }
   }
}

template<class Element>
void MSTreeView<Element>::computePositions(void)
{
  ElementTreeCursor treeCursor(elementTree());
  if(_startCursor.isValid()) treeCursor=_startCursor;
  else treeCursor.setToRoot();
  if (treeCursor.isValid()==True)
   {
     if (orientation()==Vertical)
      {
        computeHorizontalPositions(treeCursor,0);
      }
     else
      {
        computeVerticalPositions(treeCursor,0);        
      }
   }
}

// computeHorizontalPositions will compute the positions for each node
// when the orientation is Vertical. The Vertical position is
// purely related to the depth of the node in the tree while the
// horizontal position depends on the number of children from all nodes
// at the same depth
template<class Element>
int MSTreeView<Element>::computeHorizontalPositions(ElementTreeCursor treeCursor_,int depth_)
{
  int offset=margin()+shadowThickness()+highlightThickness();
  int depth=0,childDepth=0;
  int hpos=horizontalPosition(depth_);
  int vpos=verticalPosition(depth_);
  
  TreeNode& aNode=nodeAt(treeCursor_);
  int width,height;
  calculateNodeSize(aNode,width,height);

  aNode.configuration(offset,offset,width,height);
  verticalPosition(depth_,(vpos>aNode.height())?vpos:aNode.height());  

  MSBoolean hasChildren(MSFalse);
  if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
   {
     if (elementTree().numberOfSubtreeElements(treeCursor_)>1) hasChildren=MSTrue;
   }
  
  if (hasChildren==MSFalse) aNode.x(hpos);
  else
   {
     ElementTreeCursor saveCursor=treeCursor_;
     for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
      {
        childDepth = computeHorizontalPositions(treeCursor_,depth_+1);
        //compute the maximum child depth
        depth=childDepth>depth?childDepth:depth;
      }
     treeCursor_=saveCursor;
     treeCursor_.setToFirstExistingChild();
     TreeNode& firstChild=nodeAt(treeCursor_);
     treeCursor_.setToParent();
     treeCursor_.setToLastExistingChild();
     TreeNode& lastChild=nodeAt(treeCursor_);
     treeCursor_.setToParent();
     
     int leftPosition=firstChild.x()+(firstChild.width()/2);
     int rightPosition=lastChild.x()+(lastChild.width()/2);

     aNode.x(((leftPosition+rightPosition)/2)-(aNode.width()/2));
     
     if (aNode.x()<hpos)
      {
        int delta=hpos-aNode.x();
        for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
         {
           shiftHorizontalPosition(treeCursor_,delta);
	 }
        for (int j=depth_+1;j<=depth;j++) horizontalPosition(j,horizontalPosition(j)+delta);
        aNode.x(hpos);
      }
   }
  horizontalPosition(depth_,horizontalSpacing()+aNode.x()+aNode.width());
  return (depth_>depth)?depth_:depth;
}

// computeVerticalPositions will compute the positions for each node
// when the orientation is Horizontal. The Horizontal position is
// purely related to the depth of the node in the tree while the
// vertical position depends on the number of children from all nodes
// at the same depth
template<class Element>
int MSTreeView<Element>::computeVerticalPositions(ElementTreeCursor treeCursor_,int depth_)
{
  int offset=margin()+shadowThickness()+highlightThickness();
  int depth=0,childDepth=0;
  int hpos=horizontalPosition(depth_);
  int vpos=verticalPosition(depth_);
  
  TreeNode& aNode=nodeAt(treeCursor_);
  int width,height;
  calculateNodeSize(aNode,width,height);

  aNode.configuration(offset,offset,width,height);
  horizontalPosition(depth_,(hpos>aNode.width())?hpos:aNode.width());

  MSBoolean hasChildren(MSFalse);
  if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
   {
     if (elementTree().numberOfSubtreeElements(treeCursor_)>1) hasChildren=MSTrue;
   }
  
  if (hasChildren==MSFalse) aNode.y(vpos);
  else
   {
     ElementTreeCursor saveCursor=treeCursor_;
     for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
      {
        childDepth=computeVerticalPositions(treeCursor_,depth_+1);
        depth=childDepth>depth?childDepth:depth;
      }
     treeCursor_=saveCursor;
     treeCursor_.setToFirstExistingChild();
     TreeNode& firstChild=nodeAt(treeCursor_);
     treeCursor_.setToParent();
     treeCursor_.setToLastExistingChild();
     TreeNode& lastChild=nodeAt(treeCursor_);
     treeCursor_.setToParent();
     
     int top=firstChild.y()+(firstChild.height()/2);
     int bottom=lastChild.y()+(lastChild.height()/2);

     aNode.y(((top+bottom)/2)-(aNode.height()/2));

     if (aNode.y()<vpos)
      {
        int delta=vpos-aNode.y();
        for (treeCursor_.setToFirstExistingChild();treeCursor_.isValid();treeCursor_.setToNextExistingChild())
         {
           shiftVerticalPosition(treeCursor_,delta);
	 }
        for (int j=depth_+1;j<=depth;j++)
         {
           verticalPosition(j,verticalPosition(j)+delta);
         }
        aNode.y(vpos);
        
      }
   }
  verticalPosition(depth_,verticalSpacing()+aNode.y()+aNode.height());
  return (depth_>depth)?depth_:depth;
}

template<class Element>
MSBoolean MSTreeView<Element>::selectNode(ElementTreeCursor& treeCursor_)
{
  if (showRootNode()==MSFalse&&elementTree().isRoot(treeCursor_)==True) return MSFalse;

  if (selectedCursor().isValid()==True)
   {
     ElementTreeCursor cursor=selectedCursor();
     selectedCursor().invalidate();
     drawNode(cursor,redrawPixmap()->pixmap());
   }
  _selectedCursor=treeCursor_;
  if (selectedCursor().isValid()==True)
   {
     drawNode(selectedCursor(),redrawPixmap()->pixmap());
     if (mapped()==MSTrue) redrawViewPort();
     return MSTrue;
   }
  else
   {
     if (mapped()==MSTrue) redrawViewPort();
     return MSFalse;
   }
}

template<class Element>
void MSTreeView<Element>::buttonPress(const XEvent *pEvent_)
{
  if (sensitive()==MSTrue)
   {
     if (traverseFocus(this)==MSTrue)
      {
        if (editor()->mapped()==MSTrue)
         {
           editorActivate();
         }
        if (editor()->mapped()==MSFalse)
         {
           int x=pEvent_->xbutton.x,y=pEvent_->xbutton.y;
           drawAreaXY(x,y);
           ElementTreeCursor treeCursor=positionToCursor(x,y);
           if (treeCursor.isValid())
            {
              TreeNode& aNode=nodeAt(treeCursor);
              if (aNode.x()<=x&&
                  aNode.y()<=y&&
                  aNode.x()+aNode.width()>=x&&
                  aNode.y()+aNode.height()>=y)     
               {
                 MSBoolean labelSelected;
                 MSBoolean buttonSelected;
                 if (showButtons()==MSTrue&&aNode.expandable()==MSTrue)
                  {
                    if (orientation()==Horizontal)
                     {
                       if (x<(aNode.x()+aNode.width()-collapsedButtonPixmap().width()-
                              buttonShadowThickness()*2-buttonNodeMargin()))
                        {
                          labelSelected=MSTrue;
                          buttonSelected=MSFalse;
                        }
                       else
                        {
                          labelSelected=MSFalse;
                          buttonSelected=MSTrue;
                        }
                     }
                    else
                     {
                       if (y<(aNode.y()+aNode.height()-collapsedButtonPixmap().height()-
                              buttonShadowThickness()*2-buttonNodeMargin()))
                        {
                          labelSelected=MSTrue;
                          buttonSelected=MSFalse;
                        }
                       else if (x<(aNode.x()+aNode.width()/2-((collapsedButtonPixmap().width()+buttonShadowThickness()*2)/2))||
                                x>(aNode.x()+aNode.width()/2+((collapsedButtonPixmap().width()+buttonShadowThickness()*2)/2)))
                        {
                          labelSelected=MSFalse;
                          buttonSelected=MSFalse;
                        }
                       else
                        {
                          labelSelected=MSFalse;
                          buttonSelected=MSTrue;
                        }
                     }
                  }
                 else
                  {
                    labelSelected=MSTrue;
                    buttonSelected=MSFalse;
                  }
                 if (labelSelected==MSTrue&&
                     (showLabels()==MSTrue||showPixmaps()==MSTrue))
                  {
                    if (isSensitive(&aNode)==MSTrue)
                     {
                       MSBoolean alreadySelected=(selectedCursor().isValid()==True&&
                                                  selectedCursor()==treeCursor)?MSTrue:MSFalse;
                       // was this a click on the selected node??
                       // if it is, then if it is a double click we will call
                       // the activate processing method
                       if (pEvent_->xbutton.button==Button1&&alreadySelected==MSTrue)
                        {
                          if (isDoubleClick(pEvent_)==MSTrue) doubleClickNotify();
                        }
                       else
                        {
                          eventTime(pEvent_->xbutton.time);
                          if (alreadySelected==MSFalse)
                           {
                             selectNode(treeCursor);
                             nodeSelectionNotify();
                           }
                          if (pEvent_->xbutton.button==Button3&&popupMenu()!=0)
                           {
                             if (selectedCursor().isValid()==True)
                              {
                                popupMenu()->showAtPointer();
                              }
                           }
                          else if (showLabels()==MSTrue&&pEvent_->xbutton.button==Button2)
                           {
                             if (isNodeProtected(&aNode)==MSFalse)
                              {
                                int pw=0,ph=0;
                                if (showPixmaps()==MSTrue)
                                 {
                                   calculatePixmapSize(aNode.selectedPixmap(),pw,ph);
                                 }
                                if (x>aNode.x()+pw)
                                 {
                                   MSString string;
                                   formatOutput(string,aNode.cursor());
                                   editor()->string(string);
                                   mapEditor();
                                   XEvent *pEvent=(XEvent *)pEvent_;
                                   pEvent->xbutton.x-=editor()->x_origin();
                                   pEvent->xbutton.y-=editor()->y_origin();
                                   buttonPressNotify(editor(),pEvent);
                                 }
                              }
                           }
                        }
                     }
                  }
                 else if (buttonSelected==MSTrue)
                  {
                    _activatedNode=&aNode.cursor();
                    if (aNode.expandedState()==MSTrue)
                     {
                       aNode.expandedState(MSFalse);
                       collapseSubTree(treeCursor);
                       subTreeCollapsedNotify();
                     }
                    else
                     {
                       aNode.expandedState(MSTrue);
                       expandSubTree(treeCursor);
                       subTreeExpandedNotify();
                     }
                    _activatedNode=0;
                  }
                 else backgroundAreaSelection(pEvent_);
               }
            }
           // if we get here, that means we didn't find any node
           else backgroundAreaSelection(pEvent_);
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::activate(void)
{ doubleClickNotify(); }

template<class Element>
void MSTreeView<Element>::doubleClickNotify(void)
{ activateCallback(MSWidgetCallback::doubleclick); }

template<class Element>
void MSTreeView<Element>::nodeSelectionNotify(void)
{ activateCallback(MSWidgetCallback::selection); }

template <class Element>
void MSTreeView<Element>::subTreeCollapsedNotify(void)
{ activateCallback(MSWidgetCallback::subtreecollapsed); }

template <class Element>
void MSTreeView<Element>::subTreeExpandedNotify(void)
{ activateCallback(MSWidgetCallback::subtreeexpanded); }

template<class Element>
void MSTreeView<Element>::backgroundAreaSelection(const XEvent *)
{}

template<class Element>
void MSTreeView<Element>::moveToPreviousNode(void)
{
  if (selectedCursor().isValid()==True && selectedCursor()!=startCursor())
   {
     ElementTreeCursor treeCursor(selectedCursor());
     for (treeCursor.setToPreviousExistingChild();
          treeCursor.isValid();
          treeCursor.setToPreviousExistingChild())
      {
        const TreeNode &node=nodeAt(treeCursor);
        if (isSensitive(&node)==MSTrue)
         {
           if (selectNode(treeCursor)==MSTrue) nodeSelectionNotify();
           return;
         }
      }
     treeCursor=selectedCursor();
     treeCursor.setToParent();
     if (treeCursor.isValid())
      {
        for (treeCursor.setToLastExistingChild();
             treeCursor.isValid()&&treeCursor!=selectedCursor();
             treeCursor.setToPreviousExistingChild())
         {
           const TreeNode &node=nodeAt(treeCursor);
           if (isSensitive(&node)==MSTrue)
            {
              if (selectNode(treeCursor)==MSTrue) nodeSelectionNotify();
              return;
            }
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::moveToNextNode(void)
{
  if (selectedCursor().isValid()==True && selectedCursor()!=startCursor())
   {
     ElementTreeCursor treeCursor(selectedCursor());
     for (treeCursor.setToNextExistingChild();
          treeCursor.isValid();
          treeCursor.setToNextExistingChild())
      {
        const TreeNode &node=nodeAt(treeCursor);
        if (isSensitive(&node)==MSTrue)
         {
           if (selectNode(treeCursor)==MSTrue) nodeSelectionNotify();
           return;
         }
      }
     treeCursor=selectedCursor();
     treeCursor.setToParent();
     if (treeCursor.isValid())
      {
        for (treeCursor.setToFirstExistingChild();
             treeCursor.isValid()&&treeCursor!=selectedCursor();
             treeCursor.setToNextExistingChild())
         {
           const TreeNode &node=nodeAt(treeCursor);
           if (isSensitive(&node)==MSTrue)
            {
              if (selectNode(treeCursor)==MSTrue) nodeSelectionNotify();
              return;
            }
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::moveToParentNode(void)
{
  if (selectedCursor().isValid()==True)
   {
     ElementTreeCursor treeCursor(selectedCursor());
     while (treeCursor!=startCursor() && treeCursor.setToParent()==True)
      {
        const TreeNode &treeNode=nodeAt(treeCursor);
        if (isSensitive(&treeNode)==MSTrue)
         {
           if (selectNode(treeCursor)==MSTrue) nodeSelectionNotify();
           break;
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::moveToChildNode(void)
{
  //This algorithm only moves selection to the first sensitive child node.
  //If all the children of current selected node are insensitive, no search is
  //done to check the next level.
  if (selectedCursor().isValid()==True)
   {
     TreeNode &aNode=nodeAt(selectedCursor());
     if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
      {
        ElementTreeCursor treeCursor(selectedCursor());
        for (treeCursor.setToFirstExistingChild();
             treeCursor.isValid();
             treeCursor.setToNextExistingChild())
         {
           const TreeNode &node=nodeAt(treeCursor);
           if (isSensitive(&node)==MSTrue)
            {
              if (selectNode(treeCursor)==MSTrue) nodeSelectionNotify();
              return;
            }
         }
      }
   }
}

template<class Element>
void MSTreeView<Element>::up(void)
{
  if (orientation()==Horizontal) moveToPreviousNode();
  else moveToParentNode();
}

template<class Element>
void MSTreeView<Element>::down(void)
{
  if (orientation()==Horizontal) moveToNextNode();
  else moveToChildNode();
}

template<class Element>
void MSTreeView<Element>::left(void)
{
  if (orientation()==Horizontal) moveToParentNode();
  else moveToPreviousNode();
}

template<class Element>
void MSTreeView<Element>::right(void)
{
  if (orientation()==Horizontal) moveToChildNode();
  else moveToNextNode();
}

template<class Element>
void MSTreeView<Element>::keyPress(const XEvent *pEvent_,KeySym keysym_,unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keysym_, state_);
  if (selectedCursor().isValid()==True&&keyTranslate(keyPress)==MSFalse)
   {
     if (editor()->mapped()==MSTrue) keyPressNotify(editor(),pEvent_,keysym_,state_,pString_);
     else if (selectedCursor().isValid()==True&&elementTree().isEmpty()==False)
      {
	switch (keysym_)
	 {
	 case XK_Up:     up();	     break;
	 case XK_Down:   down();     break;
	 case XK_Right:  right();    break;
	 case XK_Left:   left();     break;
	 case XK_Return: activate(); break;
	 default:
	   {
             TreeNode& aNode=nodeAt(selectedCursor());
             if (isNodeProtected(&aNode)==MSFalse)
              {
                if (keysym_==XK_BackSpace)
                 {
                   editor()->string("");
                   mapEditor();
                 }
                else if (keysym_==XK_Insert)
                 {
                   MSString string;
                   formatOutput(string,aNode.cursor());
                   editor()->string(string);
                   editor()->editMode(MSTextField::InsertMode);
                   mapEditor();
                 }
                else if (strlen(pString_)>0)
                 {
                   editor()->string("");	      
                   keyPressNotify(editor(),pEvent_,keysym_,state_,pString_);
                   if (editor()->length()>0) mapEditor();
                 }
              }
	     break;
	   }
	 }
      }
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::loseFocus(void)
{
  if (editor()->mapped()==MSTrue) editorActivate();
  if (editor()->mapped()==MSTrue) return MSFalse;
  else 
   { 
     unHighlight(); 
     return MSTrue; 
   }
}

template <class Element>
void MSTreeView<Element>::updateBackground(unsigned long oldBg_)
{
  MSScrollableWidget::updateBackground(oldBg_);
  if (nodeBackground()==oldBg_)
   {
     nodeBackground(background());
   }
  else redrawViewPortImmediately();
}

template <class Element>
void MSTreeView<Element>::updateForeground(unsigned long oldFg_)
{
  MSScrollableWidget::updateForeground(oldFg_);
  if (nodeForeground()==oldFg_)
   {
     nodeForeground(foreground());
   }
  else redrawViewPortImmediately();
}

template<class Element>
void MSTreeView<Element>::updateFont(Font oldfid_)
{
  MSScrollableWidget::updateFont(oldfid_);
  XSetFont(display(),textGC(),font());
  fontObject().fontStruct(server()->fontStruct(font()));
  editor()->font(font());
  adjustSize();
}

template<class Element>
void MSTreeView<Element>::receiveEvent(MSEvent &event_)
{
  if (event_.type()==TreeEvent::symbol())
   {
     TreeEvent &treeEvent=(TreeEvent &)event_;
     switch (treeEvent.treeEventType())
      {
      case MSObservableTreeInsert:
      case MSObservableTreeReplace:
	processReshapeEvent(treeEvent);
	break;
      case MSObservableTreeDelete:
	processDeleteEvent(treeEvent);
	break;
      case MSObservableTreeCopy:
	processCopyEvent(treeEvent);
	break;
      case MSObservableTreeAssign:
	processAssignEvent(treeEvent);
	break;
      case MSObservableTreePermute:
        processPermuteEvent(treeEvent);
        break;
      }
   }
  else if (event_.type()==MSNullEvent::symbol())
   {
     updateData();
   }
}

template <class Element>
void MSTreeView<Element>::processReshapeEvent(TreeEvent &treeEvent_)
{
  if (elementTree().isEmpty()==False)
   {
     TreeModelCursor modelCursor(treeEvent_.cursor());
     unsigned long pos=treeEvent_.position();
     MSBoolean rebuild=MSFalse;
     ElementTreeCursor elementCursor=findElementCursor(modelCursor);
     if (elementCursor.isValid())
      {
	TreeNode &node=elementTree().elementAt(elementCursor);
        modelTree().setToChild(pos,modelCursor);
        NodeAttribute nodeAttr;
        nodeAttribute(modelCursor,nodeAttr);	   
        TreeNode newNode(this,modelCursor,nodeAttr);
        if (treeEvent_.treeEventType()==MSObservableTreeReplace)
         {
           ElementTreeCursor replaceCursor(elementCursor);
           replaceCursor.setToChild(pos);
           elementTree().removeSubtree(replaceCursor);
         }
        elementTree().addAsChild(elementCursor,pos,newNode);
        elementTree().setToChild(pos,elementCursor);
        if (modelTree().isLeaf(modelCursor)==False)
         {
           updateElementTree(modelCursor,elementCursor);
         }
        nodeAttr=node.attribute();
        nodeModified(node.cursor(),nodeAttr);
        node.attribute(this,nodeAttr);
        if (node.expandable()==MSTrue&&node.expandedState()==MSTrue) rebuild=MSTrue;
      }
     if (rebuild==MSTrue&&frozen()==MSFalse)
      {
	adjustSize();
      }
   }
}

template <class Element>
void MSTreeView<Element>::processPermuteEvent(TreeEvent &treeEvent_)
{
  if (elementTree().isEmpty()==False)
   {
     TreeModelCursor modelCursor(treeEvent_.cursor());
     const MSIndexVector& index=treeEvent_.index();
     ElementTreeCursor elementCursor=findElementCursor(modelCursor);
     if (elementCursor.isValid())
      {
	TreeNode &node=elementTree().elementAt(elementCursor);
        elementTree().permuteChildren(elementCursor,index);
      }
     if (frozen()==MSFalse)
      {
	adjustSize();
      }
   }
}


template <class Element>
void MSTreeView<Element>::processDeleteEvent(TreeEvent &treeEvent_)
{
  if (elementTree().isEmpty()==False)
   {
     ElementTreeCursor deleteCursor(elementTree());
     ElementTreeCursor elementCursor=findElementCursor(treeEvent_.cursor());
     if (elementCursor.isValid())
      {
	TreeNode &nodeInfo=elementTree().elementAt(elementCursor);
        deleteCursor=elementCursor;
        elementCursor.setToChild(treeEvent_.position());
        if (selectedCursor().isValid()&&locateTreeCursor(selectedCursor(),elementCursor)==MSTrue)
         {
           selectedCursor().invalidate();
         }
        if (startCursor().isValid()&&locateTreeCursor(startCursor(),elementCursor)==MSTrue)
         {
           startCursor().invalidate();
         }
        elementTree().removeSubtree(elementCursor);
        NodeAttribute nodeAttr(nodeInfo.attribute());
        NodeAttribute before(nodeAttr);
        nodeModified(nodeInfo.cursor(),nodeAttr);
        nodeInfo.attribute(this,nodeAttr);
        if (before==nodeAttr)
         {
           MSBoolean shown=MSTrue;
           while (deleteCursor.isValid())
            {
              const TreeNode node=elementTree().elementAt(deleteCursor);
              if (node.expandable()==MSFalse||node.expandedState()==MSFalse)
               {
                 shown=MSFalse;
                 break;
               }
              deleteCursor.setToParent();
            }
           if (shown==MSTrue) adjustSize();
         }
        else adjustSize();
      }
   }
}

template <class Element>
void MSTreeView<Element>::processCopyEvent(TreeEvent &)
{
  updateData();
}

template <class Element>
void MSTreeView<Element>::processAssignEvent(TreeEvent &)
{
  adjustSize();
}

template<class Element>
void MSTreeView<Element>::updateData(void)
{
  if (firstMap()==MSTrue)
   {
     elementTree().removeAll();
     _selectedCursor.invalidate();
     _startCursor.invalidate();
     rebuildScreen();
     adjustSize();
   }
}

template<class Element>
void MSTreeView<Element>::model(TreeModel &model_)
{
  couple(&model_);
}

template<class Element>
void MSTreeView<Element>::model(const TreeModel &model_)
{
  constCouple(&model_);
}

template<class Element>
void MSTreeView<Element>::selectedNode(const TreeModelCursor &modelCursor_)
{
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     ElementTreeCursor tc(treeCursor);
     MSBoolean resizeNeeded=MSFalse;
     while (elementTree().setToParent(tc))
      {
        TreeNode& parentNode=nodeAt(tc);
        if (parentNode.expandedState()==MSFalse)
         {
           parentNode.expandedState(MSTrue);
           resizeNeeded=MSTrue;
         }
      }
     if (resizeNeeded==MSTrue)
      {
        adjustSize();
        selectNode(treeCursor);
      }
     else if (firstMap()==MSTrue) selectNode(treeCursor);
   }
}


template<class Element>
typename MSTreeView<Element>::TreeModelCursor MSTreeView<Element>::selectedNode(void) const
{
  if (selectedCursor().isValid()) return elementTree().elementAt(selectedCursor()).cursor();
  else return TreeModelCursor(TreeModel());
}

template<class Element>
void MSTreeView<Element>::expandedState(const TreeModelCursor &modelCursor_,MSBoolean expandedState_)
{
  if (elementTree().isEmpty()==True) rebuildScreen(); 
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     if (aNode.expandedState()!=expandedState_)
      {
        aNode.expandedState(expandedState_);
        if (aNode.expandable()==MSTrue)
         {
           if (expandedState_==MSFalse) collapseSubTree(treeCursor);
           else expandSubTree(treeCursor);
         }
      }
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::expandedState(const TreeModelCursor &modelCursor_) const
{
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     return aNode.expandedState();
   }
  return MSFalse;
}

template<class Element>
void MSTreeView<Element>::expandable(const TreeModelCursor &modelCursor_,MSBoolean expandable_)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     if (aNode.expandable()!=expandable_)
      {
        aNode.expandable(expandable_);
        if (aNode.expandable()==MSFalse&&elementTree().isLeaf(treeCursor)==False)
         {
           collapseSubTree(treeCursor);
         }
        else adjustSize();
      }
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::expandable(const TreeModelCursor &modelCursor_) const
{
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     return aNode.expandable();
   }
  return MSFalse;
}

template<class Element>
void MSTreeView<Element>::makeVisible(const TreeModelCursor &modelCursor_)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     MSBoolean redrawNeeded=MSFalse;
     while (elementTree().setToParent(treeCursor))
      {
        TreeNode& parentNode=nodeAt(treeCursor);
        if (parentNode.expandedState()==MSFalse)
         {
           parentNode.expandedState(MSTrue);
           redrawNeeded=MSTrue;
         }
      }
     if (redrawNeeded==MSTrue) adjustSize();
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::isVisible(const TreeModelCursor &modelCursor_) const
{
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     MSBoolean visible=MSTrue;
     while (elementTree().setToParent(treeCursor))
      {
        const TreeNode& parentNode=nodeAt(treeCursor);
        if (parentNode.expandedState()==MSFalse||parentNode.expandable()==MSFalse)
         {
           visible=MSFalse;
           break;
         }
      }
     return visible;
   }
  return MSFalse;
}

template<class Element>
void MSTreeView<Element>::rebuildScreen(Iterator *iterator_)
{
  if (hasModel()==MSTrue&&elementTree().isEmpty()==True)
   {
     TreeModelCursor modelCursor(modelTree());
     modelCursor.setToRoot();
     if (modelCursor.isValid())
      {
        NodeAttribute attr;
        if (iterator_==0) nodeAttribute(modelCursor,attr);
        else iterator_->nodeAttribute(modelCursor,attr);
	elementTree().addAsRoot(TreeNode(this,modelCursor,attr));
	ElementTreeCursor elementCursor(elementTree());
	elementCursor.setToRoot();
	updateElementTree(modelCursor,elementCursor,iterator_);
      }
   }
}

template<class Element>
void MSTreeView<Element>::updateElementTree(TreeModelCursor modelCursor_,ElementTreeCursor &elementCursor_,Iterator *iterator_)
{
  if (elementCursor_.isValid())
   {
     for (modelCursor_.setToFirstExistingChild();modelCursor_.isValid();modelCursor_.setToNextExistingChild())
      {
        NodeAttribute attr;
        if (iterator_==0) nodeAttribute(modelCursor_,attr);
        else iterator_->nodeAttribute(modelCursor_,attr);
	unsigned long pos=modelTree().position(modelCursor_);
	elementTree().addAsChild(elementCursor_,pos,TreeNode(this,modelCursor_,attr));
	ElementTreeCursor newElementCursor(elementCursor_);
	elementTree().setToChild(pos,newElementCursor);
	updateElementTree(modelCursor_,newElementCursor,iterator_);
      }
   }
}

template <class Element>
void MSTreeView<Element>::showButtons(MSBoolean showButtons_)
{
  if (_showButtons!=showButtons_)
   {
     _showButtons=showButtons_;
     adjustSize();
   }
}

template <class Element>
void MSTreeView<Element>::showLabels(MSBoolean showLabels_)
{
  if (_showLabels!=showLabels_)
   {
     _showLabels=showLabels_;
     adjustSize();
   }
}

template <class Element>
void MSTreeView<Element>::showPixmaps(MSBoolean showPixmaps_)
{
  if (_showPixmaps!=showPixmaps_)
   {
     _showPixmaps=showPixmaps_;
     adjustSize();
   }
}

template <class Element>
void MSTreeView<Element>::showRootNode(MSBoolean showRootNode_)
{
  if (_showRootNode!=showRootNode_)
   {
     _showRootNode=showRootNode_;
     adjustSize();
   }
}

template <class Element>
void MSTreeView<Element>::buttonShadowThickness(int buttonShadowThickness_)
{
  if (_buttonShadowThickness!=buttonShadowThickness_)
   {
     _buttonShadowThickness=buttonShadowThickness_;
     adjustSize();
   }
}

template <class Element>
void MSTreeView<Element>::collapsedButtonPixmap(const MSPixmap &collapsedButtonPixmap_)
{
  if (_collapsedButtonPixmap!=0) delete _collapsedButtonPixmap;
  _collapsedButtonPixmap=new MSPixmap(collapsedButtonPixmap_);
  adjustSize();
}

template <class Element>
void MSTreeView<Element>::expandedButtonPixmap(const MSPixmap &expandedButtonPixmap_)
{
  if (_expandedButtonPixmap!=0) delete _expandedButtonPixmap;
  _expandedButtonPixmap=new MSPixmap(expandedButtonPixmap_);
  adjustSize();
}

template <class Element>
void MSTreeView<Element>::set(MSAttrValueList& avList_)
{
  MSScrollableWidget::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="buttonShadowThickness")
      {
	buttonShadowThickness(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="lineForeground")
      {
	lineForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="nodeForeground")
      {
	nodeForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="nodeBackground")
      {
	nodeBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="selectedNodeForeground")
      {
	selectedNodeForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="selectedNodeBackground")
      {
	selectedNodeBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="orientation")
      {
 	orientation(avList_[i].value()=="Vertical"?Vertical:Horizontal);
	index<<i;
      }
     else if (avList_[i].attribute()=="verticalSpacing")
      {
	verticalSpacing(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="horizontalSpacing")
      {
	horizontalSpacing(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="showButtons")
      {
	showButtons(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showLabels")
      {
	showLabels(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showPixmaps")
      {
	showPixmaps(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showRootNode")
      {
	showRootNode(avList_[i].value().asBoolean());
	index<<i;
      }
   }
  avList_.remove(index);
}

template <class Element>
MSAttrValueList& MSTreeView<Element>::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("buttonShadowThickness",MSString(buttonShadowThickness()));
  avList_<<MSAttrValue("lineForeground",_server->colorName(lineForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("nodeForeground",_server->colorName(nodeForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("nodeBackground",_server->colorName(nodeBackground()),MSAttrValue::Color);  
  avList_<<MSAttrValue("selectedNodeForeground",_server->colorName(selectedNodeForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("selectedNodeBackground",_server->colorName(selectedNodeBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("orientation",
		       orientation()==Vertical?"Vertical":"Horizontal",
		       MSStringVector("Vertical\nHorizontal"));
  avList_<<MSAttrValue("verticalSpacing",MSString(verticalSpacing())); 
  avList_<<MSAttrValue("horizontalSpacing",MSString(horizontalSpacing()));
  MSStringVector aBoolVector("MSFalse\nMSTrue");
  avList_<<MSAttrValue("showButtons",showButtons()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("showLabels",showLabels()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("showPixmaps",showPixmaps()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("showRootNode",showRootNode()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("subtreecollapsed","",MSAttrValue::Callback);
  avList_<<MSAttrValue("subtreeexpanded","",MSAttrValue::Callback);
  avList_<<MSAttrValue("doubleclick","",MSAttrValue::Callback);
  avList_<<MSAttrValue("selection","",MSAttrValue::Callback);
  avList_<<MSAttrValue("button2selection","",MSAttrValue::Callback);
  avList_<<MSAttrValue("button3selection","",MSAttrValue::Callback);

  return MSScrollableWidget::get(avList_);
}

template <class Element>
void MSTreeView<Element>::print(const char *file_)
{
  MSBoolean fileOpen=MSFalse;
  MSBoolean open=MSTrue; 
  if (outputMode()==Draw)
   {
     if (file_!=0) displayPrintFileName(file_);
     if ((open=displayPrintOpen(this))==MSTrue)
      {
	fileOpen=MSTrue;
	outputMode(Print);
	displayPrintXorigin(0);
	displayPrintYorigin(0);
      }
   }
  if (open==MSTrue)
   {
     displayPrintOriginInc(drawAreaWidget());
     redrawDrawAreaImmediately();
     redrawViewPortImmediately();
     displayPrintOriginDec(drawAreaWidget());
     redrawScrollableWidgetImmediately();
     if (vsb()->mapped())
      {
        displayPrintOriginInc(vsb());
        vsb()->redraw();
        displayPrintOriginDec(vsb());
      }
     if (hsb()->mapped())
      {
        displayPrintOriginInc(hsb());
        hsb()->redraw();
        displayPrintOriginDec(hsb());
      }
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}

template <class Element>
void MSTreeView<Element>::unfreeze(void)
{
  MSScrollableWidget::unfreeze();
  adjustSize();
}

template <class Element>
void MSTreeView<Element>::show(void)
{
  if (mapped()==MSFalse)
   {
     viewPort()->show();
     map();
   }
}

template <class Element>
void MSTreeView<Element>::focusIn(void)
{
  highlight();
  if (editor()->mapped()==MSTrue) takeFocusNotify(editor());

}

template <class Element>
void MSTreeView<Element>::focusOut(void)
{
  unHighlight();
  if (editor()->mapped()==MSTrue) loseFocusNotify(editor());
}

template <class Element>
typename MSTreeView<Element>::ElementTreeCursor 
MSTreeView<Element>::positionToCursor(int x_,int y_)
{
  ElementTreeCursor elementCursor(elementTree());
  if(_startCursor.isValid()) elementCursor=_startCursor;
  else elementCursor.setToRoot();
  if (elementCursor.isValid()==True)
   {
     TreeNode& aNode=nodeAt(elementCursor);
     if (showRootNode()==MSTrue)
      {
        if (aNode.x()<=x_&&
            aNode.y()<=y_&&
            aNode.x()+aNode.width()>=x_&&
            aNode.y()+aNode.height()>=y_)
         {
           return elementCursor;
         }
      }
     if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
      {
        for (elementCursor.setToFirstExistingChild();elementCursor.isValid();elementCursor.setToNextExistingChild())
         {
           ElementTreeCursor cursor(elementCursor);
           if (positionToCursor(cursor,x_,y_)==MSTrue) return cursor;
         }
      }
   }
  elementCursor.invalidate();
  return elementCursor;
}

template <class Element>
MSBoolean MSTreeView<Element>::positionToCursor(ElementTreeCursor &cursor_,int x_,int y_)
{
  if (cursor_.isValid()==True)
   {
     TreeNode& aNode=nodeAt(cursor_);
     if (aNode.x()<=x_&&
         aNode.y()<=y_&&
         aNode.x()+aNode.width()>=x_&&
         aNode.y()+aNode.height()>=y_)
      {
        return MSTrue;
      }
     else if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
      {
        ElementTreeCursor elementCursor(cursor_);
        for (elementCursor.setToFirstExistingChild();elementCursor.isValid();elementCursor.setToNextExistingChild())
         {
           ElementTreeCursor cursor(elementCursor);
           if (positionToCursor(cursor,x_,y_)==MSTrue)
            {
              cursor_=cursor;
              return MSTrue;
            }
         }
      }
   }
  return MSFalse;
}

template <class Element>
void MSTreeView<Element>::collapseSubTree(ElementTreeCursor &cursor_)
{
  if (elementTree().isLeaf(cursor_)==False)
   {
     if (selectedCursor().isValid()==True)
      {
        for (cursor_.setToFirstExistingChild();cursor_.isValid();cursor_.setToNextExistingChild())
         {
           if (locateTreeCursor(selectedCursor(),cursor_)==MSTrue)
            {
              selectedCursor().invalidate();
            }
         }     
      }
     adjustSize();
   }
  else
   {
     if (firstMap()==MSTrue&&frozen()==MSFalse)
      {
        drawNode(cursor_,redrawPixmap()->pixmap());
        redrawViewPort();
      }  
   }
}

template <class Element>
void MSTreeView<Element>::expandSubTree(ElementTreeCursor &cursor_)
{
  if (elementTree().isLeaf(cursor_)==False) adjustSize();
  else
  {
     if (firstMap()==MSTrue&&frozen()==MSFalse)
      {
        drawNode(cursor_,redrawPixmap()->pixmap());
        redrawViewPort();
      }  
   }
}

template <class Element>
MSBoolean MSTreeView<Element>::locateTreeCursor(ElementTreeCursor locateCursor_,
                                                ElementTreeCursor cursor_)
{
  if (locateCursor_==cursor_) return MSTrue;
  else
   {
     TreeNode &aNode=nodeAt(cursor_);
     if (aNode.expandable()==MSTrue&&aNode.expandedState()==MSTrue)
      {
        for (cursor_.setToFirstExistingChild();cursor_.isValid();cursor_.setToNextExistingChild())
         {
           if (locateTreeCursor(locateCursor_,cursor_)==MSTrue) return MSTrue;
         }
      }
     else return MSFalse;
   }
  return MSFalse;
}

template <class Element>
typename MSTreeView<Element>::TreeModelCursor MSTreeView<Element>::activatedNode(void) const
{
  if (hasModel()==MSTrue)
   {
     if (_activatedNode==0) return TreeModelCursor(modelTree());
     else return *_activatedNode;
   }
  else return TreeModelCursor(TreeModel());
}

template <class Element>
void MSTreeView<Element>::getNodeAttributes(MSTreeView<Element>::ConstIterator &iterator_) const
{
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  NodeAttribute attr;
  ElementTreeCursor elementCursor(elementTree());
  for (elementTree().setToFirst(elementCursor,MSPreorder);
       elementCursor.isValid();
       elementTree().setToNext(elementCursor,MSPreorder))
   {
     const TreeNode &node=elementTree().elementAt(elementCursor);
     attr=node.attribute();
     iterator_.nodeAttribute(node.cursor(),attr);
   }
}

template <class Element>
void MSTreeView<Element>::setNodeAttributes(MSTreeView<Element>::Iterator &iterator_)
{
  if (hasModel()==MSTrue)
   {
     if (elementTree().isEmpty()==True) rebuildScreen(&iterator_);
     else
      {
        NodeAttribute attr;
        ElementTreeCursor elementCursor(elementTree());
        for (elementTree().setToFirst(elementCursor,MSPreorder);
             elementCursor.isValid();
             elementTree().setToNext(elementCursor,MSPreorder))
         {
           TreeNode &node=elementTree().elementAt(elementCursor);
           attr=node.attribute();
           iterator_.nodeAttribute(node.cursor(),attr);
           node.attribute(this,attr);
         }
      }
     adjustSize();
   }
}

template <class Element>
void MSTreeView<Element>::registerPixmap(const MSPixmap &pixmap_)
{
  pixmapRegistry().addOrReplaceElementWithKey(pixmap_);
}

template <class Element>
const MSPixmap *MSTreeView<Element>::pixmap(const MSString &key_) const
{
  PixmapRegistryCursor cursor(_pixmapRegistry);
  _pixmapRegistry.locateElementWithKey(key_,cursor);
  if (cursor.isValid()) return &_pixmapRegistry.elementAt(cursor);
  else return 0;
}

template <class Element>
MSBoolean MSTreeView<Element>::isSensitive(const TreeNode *treeNode_)
{ return treeNode_->sensitive(); }

template <class Element>
MSBoolean MSTreeView<Element>::isExpandable(const TreeNode *treeNode_)
{ return treeNode_->expandable(); }

template <class Element>
MSBoolean MSTreeView<Element>::isExpanded(const TreeNode *treeNode_)
{
  if (treeNode_->expandable()==MSFalse) return MSFalse;
  else return treeNode_->expandedState();
}

template <class Element>
const typename MSTreeView<Element>::PixmapList &MSTreeView<Element>::pixmap(const TreeNode *treeNode_) 
{ return treeNode_->pixmap(); }

template <class Element>
const typename MSTreeView<Element>::PixmapList &MSTreeView<Element>::selectedPixmap(const TreeNode *treeNode_)
{ return treeNode_->selectedPixmap(); }

template <class Element>
const typename MSTreeView<Element>::PixmapList &MSTreeView<Element>::insensitivePixmap(const TreeNode *treeNode_)
{ return treeNode_->insensitivePixmap(); }

template <class Element>
unsigned long MSTreeView<Element>::nodeForegroundColor(const TreeNode *treeNode_)
{
  if (selectedCursor().isValid()&&treeNode_==&nodeAt(selectedCursor())) return selectedNodeForeground();
  else return nodeForeground();
}

template <class Element>
unsigned long MSTreeView<Element>::nodeBackgroundColor(const TreeNode *treeNode_)
{
  if (selectedCursor().isValid()&&treeNode_==&nodeAt(selectedCursor())) return selectedNodeBackground();
  else return nodeBackground();
}

template <class Element>
unsigned long MSTreeView<Element>::lineForegroundColor(const TreeNode * /*treeNode_*/)
{ return lineForeground();}

template <class Element>
void MSTreeView<Element>::calculateNodeSize(TreeNode &treeNode_,int &width_,int &height_)
{
  int pixmapWidth=0,pixmapHeight=0;
  if (showPixmaps()==MSTrue)
   {
     // Calculate the maximum pixmap width and height
     int pw=0,ph=0;
     calculatePixmapSize(pixmap(&treeNode_),pw,ph);
     pixmapHeight=(ph>pixmapHeight)?ph:pixmapHeight;
     pixmapWidth=(pw>pixmapWidth)?pw:pixmapWidth;

     
     calculatePixmapSize(insensitivePixmap(&treeNode_),pw,ph);
     pixmapHeight=(ph>pixmapHeight)?ph:pixmapHeight;
     pixmapWidth=(pw>pixmapWidth)?pw:pixmapWidth;

     calculatePixmapSize(selectedPixmap(&treeNode_),pw,ph);
     pixmapHeight=(ph>pixmapHeight)?ph:pixmapHeight;
     pixmapWidth=(pw>pixmapWidth)?pw:pixmapWidth;
   }
  width_=pixmapWidth;
  height_=pixmapHeight;
  if (showLabels()==MSTrue)
   {
     MSString string;
     formatOutput(string,treeNode_.cursor());

     width_+=(width_>0?buttonNodeMargin():0);
     width_+=(fontObject().textWidth(string)+nodeMargin());
     height_=fontObject().textHeight()+nodeMargin();
     height_=(height_>pixmapHeight)?height_:pixmapHeight;
   }
  
  if (showButtons()==MSTrue&&treeNode_.expandable()==MSTrue)
   {
     int margin=buttonShadowThickness()*2;
     if (orientation()==Vertical)
      {
        height_+=(height_>0?buttonNodeMargin():0);
        height_+=collapsedButtonPixmap().height()+margin;
        int bw=collapsedButtonPixmap().width()+margin;
        width_=(width_>bw)?width_:bw;
      }
     else
      {
        width_+=(width_>0?buttonNodeMargin():0);
        width_+=collapsedButtonPixmap().width()+margin;
        int bh=collapsedButtonPixmap().height()+margin;
        height_=(height_>bh)?height_:bh;
      }
   }
}

template <class Element>
void MSTreeView<Element>::calculatePixmapSize(const PixmapList &pixmapList_,int &w_,int &h_)
{
  w_=h_=0;
  unsigned long nElements=pixmapList_.length();
  for (unsigned long i=0;i<nElements;i++)
   {
     w_+=pixmapList_.elementAt(i).width();
     if (i!=nElements-1) w_+=buttonNodeMargin();
     int pHeight=pixmapList_.elementAt(i).height();
     h_=(pHeight>h_)?pHeight:h_;
   }
}

template<class Element>
void MSTreeView<Element>::sensitive(const TreeModelCursor &modelCursor_,MSBoolean sensitive_)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     if (aNode.sensitive()!=sensitive_)
      {
        aNode.sensitive(sensitive_);
        drawNode(treeCursor,redrawPixmap()->pixmap());
        if (mapped()==MSTrue) redrawViewPort();
      }
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::sensitive(const TreeModelCursor &modelCursor_) const
{
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     return aNode.sensitive();
   }
  return MSFalse;
}

template<class Element>
void MSTreeView<Element>::pixmap(const TreeModelCursor &modelCursor_,const MSStringVector &pixmap_)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     aNode.buildPixmapList(pixmapRegistry(),aNode.pixmap(),pixmap_);
     adjustSize();
   }
}

template<class Element>
MSStringVector MSTreeView<Element>::pixmap(const TreeModelCursor &modelCursor_) const
{
  MSStringVector names;
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     unsigned long nElements=aNode.pixmap().length();
     for (unsigned long i=0;i<nElements;i++)
      {
        names<<aNode.pixmap().elementAt(i).name();
      }
   }
  return names;
}

template<class Element>
void MSTreeView<Element>::insensitivePixmap(const TreeModelCursor &modelCursor_,const MSStringVector &insensitivePixmap_)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     aNode.buildPixmapList(pixmapRegistry(),aNode.insensitivePixmap(),insensitivePixmap_);
     adjustSize();
   }
}

template<class Element>
MSStringVector MSTreeView<Element>::insensitivePixmap(const TreeModelCursor &modelCursor_) const
{
  MSStringVector names;
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     unsigned long nElements=aNode.insensitivePixmap().length();
     for (unsigned long i=0;i<nElements;i++)
      {
        names<<aNode.insensitivePixmap().elementAt(i).name();
      }
   }
  return names;
}

template<class Element>
void MSTreeView<Element>::selectedPixmap(const TreeModelCursor &modelCursor_,const MSStringVector &selectedPixmap_)
{
  if (elementTree().isEmpty()==True) rebuildScreen();
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     aNode.buildPixmapList(pixmapRegistry(),aNode.selectedPixmap(),selectedPixmap_);
     adjustSize();
   }
}

template<class Element>
MSStringVector MSTreeView<Element>::selectedPixmap(const TreeModelCursor &modelCursor_) const
{
  MSStringVector names;
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (elementTree().isEmpty()==True)
   {
     MSTreeView<Element> *myself=(MSTreeView<Element> *)this;
     myself->rebuildScreen();
   }
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     const TreeNode& aNode=nodeAt(treeCursor);
     unsigned long nElements=aNode.selectedPixmap().length();
     for (unsigned long i=0;i<nElements;i++)
      {
        names<<aNode.selectedPixmap().elementAt(i).name();
      }
   }
  return names;
}

template<class Element>
MSBoolean MSTreeView<Element>::sensitive(void) const
{
  return MSScrollableWidget::sensitive();
}

template<class Element>
void MSTreeView<Element>::sensitive(MSBoolean sensitive_)
{
  MSScrollableWidget::sensitive(sensitive_);
}

template <class Element>
class MSTreeViewDefaultIterator : public MSTreeView<Element>::Iterator
{
protected:
  typename MSTreeView<Element>::TreeModel &_treeModel;
public:
  MSTreeViewDefaultIterator(typename MSTreeView<Element>::TreeModel &treeModel_)
  :_treeModel(treeModel_)
  {}
  virtual void nodeAttribute(const typename MSTreeView<Element>::TreeModelCursor&,
                             typename MSTreeView<Element>::NodeAttribute &);
};

template<class Element>
void MSTreeViewDefaultIterator<Element>::nodeAttribute(const typename MSTreeView<Element>::TreeModelCursor &cursor_,
                                                       typename MSTreeView<Element>::NodeAttribute &nodeAttr_)
{
  if (_treeModel.isLeaf(cursor_))
   {
     nodeAttr_.expandable(MSFalse);
     nodeAttr_.expandedState(MSFalse);
   }
  else
   {
     nodeAttr_.expandable(MSTrue);
     nodeAttr_.expandedState(MSTrue);
   }
}


template<class Element>
void MSTreeView<Element>::setDefaultNodeAttributes(void)
{
  if (hasModel()==MSTrue)
   {
     MSTreeViewDefaultIterator<Element> iterator(modelTree());
     setNodeAttributes(iterator);
   }
}

template <class Element>
MSBoolean MSTreeView<Element>::isNodeProtected(const TreeNode *)
{ return isProtected();}

template<class Element>
typename MSTreeView<Element>::TreeModelCursor MSTreeView<Element>::startNode(void) const
{
  if (startCursor().isValid()) return elementTree().elementAt(startCursor()).cursor();
  else return TreeModelCursor(TreeModel());
}

  
template<class Element>
void MSTreeView<Element>::startNode(const TreeModelCursor & modelCursor_)
{
  ElementTreeCursor treeCursor=findElementCursor(modelCursor_);
  if(treeCursor.isValid())
   {
     TreeNode& aNode=nodeAt(treeCursor);
     ElementTreeCursor tc(treeCursor);
     while (elementTree().setToParent(tc))
      {
        TreeNode& parentNode=nodeAt(tc);
        if (parentNode.expandedState()==MSFalse)
         {
           parentNode.expandedState(MSTrue);
         }
      }
     _startCursor=treeCursor;
     if(locateTreeCursor(selectedCursor(),_startCursor)==MSFalse)
      {
        _selectedCursor.invalidate();
      }
     adjustSize();
   }
}

template<class Element>
MSBoolean MSTreeView<Element>::editing(void) const
{
  return _pEditor->mapped();
}

template<class Element>
void MSTreeView<Element>::edit(void)
{
 if (editor()->mapped()==MSFalse) 
  {
    TreeNode& aNode=nodeAt(selectedCursor());
    if (isNodeProtected(&aNode)==MSFalse)
     {
       MSString string;
       formatOutput(string,aNode.cursor());
       editor()->string(string);
//       editor()->selectAll();
       mapEditor();
     }
  }
}


template<class Element>
typename MSTreeView<Element>::ElementTreeCursor MSTreeView<Element>::findElementCursor(const TreeModelCursor& modelCursor_) const
{
  ElementTreeCursor elementCursor(elementTree());
  for (elementTree().setToFirst(elementCursor,MSPreorder);
       elementCursor.isValid();
       elementTree().setToNext(elementCursor,MSPreorder))
   {
     if(elementCursor.element().cursor()==modelCursor_) break;
   }
  return elementCursor;
}

template<class Element>
typename MSTreeView<Element>::TreeModel& MSTreeView<Element>::modelTree(void)
{ return *(TreeModel *)_model; }  

template<class Element>
const typename MSTreeView<Element>::TreeModel& MSTreeView<Element>::modelTree(void) const
{ return *(TreeModel *)_model; }  



template<class Element>
unsigned long  MSTreeView<Element>::addEditorKeyCallback( const char* pString_,MSKeyCallback* keyCallback_)
{ return editor()->addKeyCallback(pString_,keyCallback_);}

template<class Element>
void MSTreeView<Element>::removeEditorKeyCallback(unsigned long id_)
{ editor()->removeKeyCallback(id_); }

template<class Element>
void MSTreeView<Element>::removeEditorKeyCallback(const char* pString_)
{ editor()->removeKeyCallback(pString_); }

#endif
