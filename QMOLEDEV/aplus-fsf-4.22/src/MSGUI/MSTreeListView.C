#ifndef MSTreeListViewIMPLEMENTATION
#define MSTreeListViewIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSUtil.H>
#include <MSGUI/MSTreeListView.H>
#include <MSTypes/MSStandardOps.H> // look for elementForOps
#include <MSTypes/MSMessageLog.H>
#include <MSGUI/MSTreeFormatFunctions.H>

const int MSTreeListViewDefaultRows=15;
const int MSTreeListViewDefaultColumns=30;
const int MSTreeListViewDefaultRowHorizontalSpacing=2;
const int MSTreeListViewDefaultRowVerticalSpacing=2;
const int MSTreeListViewDefaultButtonShadowThickness=2;
const int MSTreeListViewMinimumButtonSize=8;
const int MSTreeListViewDefaultScrollBarSize=15;
const unsigned long MSTreeListViewEventMask=(ButtonPressMask|ButtonReleaseMask|ExposureMask);
const int MSTreeListViewMinimumEditorWidth=75;


// Since this is a template class all global defines should be unique
#define MSTreeListViewCollapsedButtonPixmapName "__MSTreeListViewCollapsedButtonPixmap__"
#define MSTreeListViewExpandedButtonPixmapName "__MSTreeListViewExpandedButtonPixmap__"
#define MSTreeListViewDashPixmapName "__MSTreeListViewDashPixmap__"

#define MSTreeListView_Expanded_width 12
#define MSTreeListView_Expanded_height 12

#define MSTreeListView_Collapsed_width 12
#define MSTreeListView_Collapsed_height 12

#define MSTreeListView_dash_width 12
#define MSTreeListView_dash_height 12

template <class Element>
class VScrollBar : public MSVScrollBar
{
protected:
  virtual void change(void);
public:
  VScrollBar(MSWidget *);
  ~VScrollBar(void);
};

template <class Element>
class HScrollBar : public MSHScrollBar
{
protected:
  virtual void change(void);
public:
  HScrollBar(MSWidget *);
  ~HScrollBar(void);
};

template <class Element>
VScrollBar<Element>::VScrollBar(MSWidget *owner_)
: MSVScrollBar(owner_)
{
  inc(1);
  pageInc(1);
  _highlightThickness=0;
  _acceptFocus=MSFalse;
  width(MSTreeListViewDefaultScrollBarSize);
}

template <class Element>
VScrollBar<Element>::~VScrollBar(void)
{}

template <class Element>
void VScrollBar<Element>::change(void)
{ ((MSTreeListView<Element>*)owner())->vsbChanged(); }

template <class Element>
HScrollBar<Element>::HScrollBar(MSWidget *owner_)
: MSHScrollBar(owner_)
{
  min(0);
  inc(1);
  pageInc(1);
  _highlightThickness=0;
  _acceptFocus=MSFalse;
  height(MSTreeListViewDefaultScrollBarSize);
}

template <class Element>
HScrollBar<Element>::~HScrollBar(void)
{}

template <class Element>
void HScrollBar<Element>::change(void)
{ ((MSTreeListView<Element>*)owner())->hsbChanged(); }

#if !defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
// Editor 
template<class Element>
MSTreeListView<Element>::Editor::Editor(MSWidget *owner_) : MSTextField(owner_)
{
  _highlightThickness=0;
  selectInput(ExposureMask|ButtonPressMask);
}

template<class Element>
MSTreeListView<Element>::Editor::~Editor(void)
{}

template<class Element>
void MSTreeListView<Element>::Editor::activate(void) 
{ ((MSTreeListView<Element> *)(owner()))->editorActivate(); }

template<class Element>
void MSTreeListView<Element>::Editor::escape(void)
{ ((MSTreeListView<Element> *)(owner()))->editorEscape(); }

#endif // !MS_INLINE_TEMPLATE_NESTED_CLASS


template <class Element>
MSTreeListView<Element>::MSTreeListView(MSWidget *owner_,TreeModel &tree_)
: MSWidgetCommon(owner_)
{
  init();
  couple(&tree_);
}

template <class Element>
MSTreeListView<Element>::MSTreeListView(MSWidget *owner_)
: MSWidgetCommon(owner_)
{
  init();
}

template <class Element>
MSTreeListView<Element>::~MSTreeListView(void)
{
  if (_collapsedButtonPixmap!=0) delete _collapsedButtonPixmap;
  if (_expandedButtonPixmap!=0) delete _expandedButtonPixmap;
  if (vsb()!=0) safeDestroy(vsb());
  if (hsb()!=0) safeDestroy(hsb());
  if (clipWindow()!=0) safeDestroy(clipWindow());
  if (redrawPixmap()!=0) delete redrawPixmap();
  if (stipplePixmap()!=0) delete stipplePixmap();
  if (dotPixmap()!=0) delete dotPixmap();
  if (dashPixmap()!=0) delete dashPixmap();
  if (_selectedNode!=0) delete _selectedNode;
  if (_activatedNode!=0) delete _activatedNode;
  _pEditor->destroy();
}

template <class Element>
void MSTreeListView<Element>::init(void)
{
  _useBackingStore=MSFalse;
  _shadowStyle=MSSunken;
  _acceptFocus=MSTrue;
  addToFocusList();
  _hasFocus=MSFalse;
  _vsb=new VScrollBar<Element>(this);
  _hsb=new HScrollBar<Element>(this);
  _clipWindow=new MSWidgetCommon(this);
  _clipWindow->highlightThickness(0);
  _clipWindow->shadowThickness(0);
  _clipWindow->acceptFocus(MSFalse);
  _pEditor = new Editor(this);
  _pEditor->xMargin(2);
  _redrawPixmap=new MSBackingStorePixmap(server(),"MSGeneral");
  _showButtons=MSTrue;
  _showLines=MSTrue;
  _showRootNode=MSTrue;
  _tabSpacing=0;
  _selectedNode=0;
  _firstRow=0;
  _activatedNode=0;
  _collapsedButtonPixmap=0;
  _expandedButtonPixmap=0;
  _lineForeground=foreground();
  _lineStyle=MSSolid;
  _buttonShadowStyle=MSRaised;
  _selectedRowForeground=background();
  _selectedRowBackground=foreground();
  fontObject().fontStruct(server()->fontStruct(font()));
  hsb()->inc(fontObject().charWidth());
  _stipplePixmap=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,1,0,1);
  _dotPixmap=0;
  _dashPixmap=0;
  XGCValues values;
  values.font=font();
  values.stipple=stipplePixmap()->pixmap();
  values.fill_style=FillSolid;
  gc().setGCValues(server(),MSFalse,&values,GCFont|GCStipple|GCFillStyle);
  values.fill_style=FillSolid;
  values.foreground=lineForeground();
  linegc().setGCValues(server(),MSFalse,&values,GCFillStyle|GCForeground);
  values.foreground=foreground();
  values.background=background();
  pixmapgc().setGCValues(server(),MSFalse,&values,GCForeground|GCBackground);
  buildCollapsedButtonPixmap();
  buildExpandedButtonPixmap();
  setGeometryDefaults();
  useBackingStore(MSTrue);
  readOnly(MSTrue);
  selectInput(MSTreeListViewEventMask);  
} 

template <class Element>
void MSTreeListView<Element>::setGeometryDefaults(void)
{
  _highlightThickness=0;
  _shadowThickness=2;
  _rows=MSTreeListViewDefaultRows;
  _columns=MSTreeListViewDefaultColumns;
  _rowHorizontalSpacing=MSTreeListViewDefaultRowHorizontalSpacing;
  _rowVerticalSpacing=MSTreeListViewDefaultRowVerticalSpacing;
  _buttonShadowThickness=MSTreeListViewDefaultButtonShadowThickness;
  _levelOffset=collapsedButtonPixmap().width()+buttonShadowThickness()*2+rowHorizontalSpacing()*2;
  _maxRowHeight=0;
  _maxRowWidth=0;
}

template <class Element>
void MSTreeListView<Element>::rebuildScreen(MSBoolean recomputeSize_)
{
  buildResourceTreeAndScreenVector();
  computeMaxRowSize();
  if (recomputeSize_==MSTrue) computeSize();
  else
   {
     configure();
     redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::expandSubTree(ScreenVectorCursor &cursor_,unsigned long pos_)
{
  if (frozen()==MSFalse)
   {
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor_);
     ManagedInfo &node=resourceTree().elementAt(resourceCursor);
     if (resourceTree().isLeaf(resourceCursor))
      {
	updateRow(node,pos_-1);
      }
     else
      {
        ScreenVectorCursor start(cursor_);
        ScreenVectorCursor end(cursor_);
        end.setToNext();
        updateScreenVector(cursor_,resourceCursor);
        start.setToNext();
        computeMaxRowSize(start,end);
        configure();
        redrawImmediately();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::collapseSubTree(ScreenVectorCursor &cursor_,unsigned long pos_)
{
  if (frozen()==MSFalse)
   {
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor_);
     ManagedInfo &node=resourceTree().elementAt(resourceCursor);
     if (_selectedNode!=0&&node->modelCursor()!=*_selectedNode&&locateSelectedNode(resourceCursor)==MSTrue) resetSelectedNode();
     int level=node->level();
     pos_++;
     if (pos_<=screenVector().numberOfElements())
      {
	while (pos_<=screenVector().numberOfElements())
	 {
	   screenVector().setToPosition(pos_,cursor_);
	   ResourceTreeCursor &rCursor=screenVector().elementAt(cursor_);
	   ManagedInfo &nodeInfo=resourceTree().elementAt(rCursor);
	   if (nodeInfo->level()>level)
	    {
	      screenVector().removeAt(cursor_);
	    }
	   else break;
	 }
	if (dynamic()==MSTrue) adjustView();
        configure();
        redrawImmediately();
      }
     else
      {
	updateRow(node,pos_-2);
      }
   }
}

template <class Element>
void MSTreeListView<Element>::updateScreenVector(ScreenVectorCursor &cursor_,ResourceTreeCursor resourceCursor_)
{
  for (resourceCursor_.setToFirstExistingChild();resourceCursor_.isValid();resourceCursor_.setToNextExistingChild())
   {
     if (screenVector().isEmpty())
      {
	screenVector().addAsFirst(resourceCursor_);
	cursor_.setToFirst();
      }
     else screenVector().addAsNext(resourceCursor_,cursor_);
     ManagedInfo &node=resourceTree().elementAt(resourceCursor_);
     if (isExpanded(node)==MSTrue) updateScreenVector(cursor_,resourceCursor_);
   }
}

template <class Element>
void MSTreeListView<Element>::updateResourceTree(ResourceTreeCursor resourceCursor_,TreeModelCursor modelCursor_)
{
  ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor_);
  for (modelCursor_.setToFirstExistingChild();modelCursor_.isValid();modelCursor_.setToNextExistingChild())
   {
     NodeAttribute nodeAttr;
     nodeAttribute(modelCursor_,nodeAttr);
     NodeInfo *newNode=new NodeInfo(this,nodeAttr,modelCursor_,(NodeInfo*)nodeInfo,nodeInfo->level()+1);
     unsigned long pos=modelTree().position(modelCursor_);
     resourceTree().addAsChild(resourceCursor_,pos,ManagedInfo(newNode,MSInit));
     if (modelTree().isLeaf(modelCursor_)==False)
      {
	ResourceTreeCursor rCursor=resourceCursor_;
	rCursor.setToChild(pos);
	updateResourceTree(rCursor,modelCursor_);
      }
   }
}

template <class Element>
void MSTreeListView<Element>::setAttributes(Iterator &iterator_)
{
  ResourceTreeCursor resourceCursor(resourceTree());
  for (resourceTree().setToFirst(resourceCursor,MSPreorder);
       resourceCursor.isValid();
       resourceTree().setToNext(resourceCursor,MSPreorder))
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     NodeAttribute nodeAttr=info->attribute();
     iterator_.nodeAttribute(info->modelCursor(),nodeAttr);
     info->attribute(this,nodeAttr);
   }
}

template <class Element>
void MSTreeListView<Element>::getAttributes(ConstIterator &iterator_) const
{
  ResourceTreeCursor resourceCursor(resourceTree());
  for (resourceTree().setToFirst(resourceCursor,MSPreorder);
       resourceCursor.isValid();
       resourceTree().setToNext(resourceCursor,MSPreorder))
   {
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     NodeAttribute nodeAttr=info->attribute();
     iterator_.nodeAttribute(info->modelCursor(),nodeAttr);
   }
}

template <class Element>
void MSTreeListView<Element>::buildScreenVector(void)
{
  screenVector().removeAll();
  if (resourceTree().isEmpty()==False)
   {
     ResourceTreeCursor resourceCursor(resourceTree());
     resourceCursor.setToRoot();
     ScreenVectorCursor screenCursor(screenVector());
     if (showRootNode()==MSTrue)
      {
	screenVector().addAsFirst(resourceCursor,screenCursor);
	screenCursor.setToFirst();
      }
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (isExpanded(info)==MSTrue) updateScreenVector(screenCursor,resourceCursor);
   }
}

template <class Element>
void MSTreeListView<Element>::buildResourceTreeAndScreenVector(Iterator *iterator_)
{
  if (MSView::model()!=0&&resourceTree().isEmpty()==True)
   {
     TreeModelCursor modelCursor(modelTree());
     modelCursor.setToRoot();
     if (modelCursor.isValid())
      {
	int level=0;
	if (showRootNode()==MSFalse) level--;
	NodeInfo *nodeInfo;
	NodeAttribute nodeAttr;
	if (iterator_==0)
	 {
	   nodeAttribute(modelCursor,nodeAttr);
	 }
	else
	 {
	   iterator_->nodeAttribute(modelCursor,nodeAttr);
	 }
	nodeInfo=new NodeInfo(this,nodeAttr,modelCursor,0,level);
	ManagedInfo managedInfo(nodeInfo, MSInit);
	resourceTree().addAsRoot(managedInfo);
	ResourceTreeCursor resourceCursor(resourceTree());
	resourceCursor.setToRoot();
	ScreenVectorCursor screenCursor(screenVector());
	if (showRootNode()==MSTrue)
	 {	
	   screenVector().addAsFirst(resourceCursor);
	   screenCursor.setToFirst();
	 }
	updateResourceTreeAndScreenVector(resourceCursor,screenCursor,isExpanded(nodeInfo),iterator_);
      }
   }
}

template <class Element>
void MSTreeListView<Element>::updateResourceTreeAndScreenVector(ResourceTreeCursor &resourceCursor_,
										  ScreenVectorCursor &screenCursor_,
										  MSBoolean parentExpanded_,
										  Iterator *iterator_)
{
  if (resourceCursor_.isValid())
   {
     ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor_);
     TreeModelCursor modelCursor=nodeInfo->modelCursor();
     int level=nodeInfo->level()+1;
     for (modelCursor.setToFirstExistingChild();modelCursor.isValid();modelCursor.setToNextExistingChild())
      {
	NodeInfo *newNodeInfo;
	NodeAttribute nodeAttr;
	if (iterator_==0)
	 {
	   nodeAttribute(modelCursor,nodeAttr);
	 }
	else
	 {
	   iterator_->nodeAttribute(modelCursor,nodeAttr);
	 }
	newNodeInfo=new NodeInfo(this,nodeAttr,modelCursor,(NodeInfo*)nodeInfo,level);
	unsigned long pos=modelTree().position(modelCursor);
	resourceTree().addAsChild(resourceCursor_,pos,ManagedInfo(newNodeInfo,MSInit));
	ResourceTreeCursor newResourceCursor(resourceTree());
	newResourceCursor=resourceCursor_;
	resourceTree().setToChild(pos,newResourceCursor);
	if (parentExpanded_)
	 {
	   if (screenVector().isEmpty())
	    {
	      screenVector().addAsFirst(newResourceCursor);
	      screenCursor_.setToFirst();
	    }
	   else screenVector().addAsNext(newResourceCursor,screenCursor_);
	 }
	MSBoolean expanded=MSFalse;
	if (parentExpanded_==MSTrue&&isExpanded(newNodeInfo)==MSTrue) expanded=MSTrue;
	updateResourceTreeAndScreenVector(newResourceCursor,screenCursor_,expanded,iterator_);
      }
   }
  else reportError("Warning: Invalid Cursor in MSTreeListView::updateResourceTreeAndScreenVector()");
}

template <class Element>
void MSTreeListView<Element>::nodeModified(const TreeModelCursor &cursor_,NodeAttribute &attribute_)
{ nodeAttribute(cursor_,attribute_); }

template <class Element>
void MSTreeListView<Element>::nodeAttribute(const TreeModelCursor &,NodeAttribute &)
{}

template <class Element>
void MSTreeListView<Element>::setNodeAttributes(MSTreeListView<Element>::Iterator &iterator_)
{
  // If resourceTree hasn't been built yet, then build it with the iterator
  // Otherwise just traverse the resourceTree and rebuild only the screen vector
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector(&iterator_);
  else
   {
     setAttributes(iterator_);
     buildScreenVector();
   }
  adjustView();
}

template <class Element>
void MSTreeListView<Element>::getNodeAttributes(MSTreeListView<Element>::ConstIterator &iterator_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  getAttributes(iterator_);
}

template <class Element>
void MSTreeListView<Element>::model(TreeModel &modelTree_)
{
  couple(&modelTree_);
}

template <class Element>
void MSTreeListView<Element>::model(const TreeModel &modelTree_)
{
  constCouple(&modelTree_);
}

template <class Element>
void MSTreeListView<Element>::rows(int rows_)
{
  if (_rows!=rows_)
   {
     _rows=rows_;
     if (firstMap()==MSTrue) computeSize();
   }
}

template <class Element>
void MSTreeListView<Element>::columns(int columns_)
{
  if (_columns!=columns_)
   {
     _columns=columns_;
     if (firstMap()==MSTrue) computeSize();
   }
}

template <class Element>
void MSTreeListView<Element>::firstRow(int firstRow_)
{
  if (_firstRow!=firstRow_)
   {
     int oldFirstRow=firstRow();
     _firstRow=firstRow_;
     if (firstMap()==MSTrue)
      {
	adjustFirstRow();
	if (oldFirstRow!=firstRow())
	 {
	   if (useBackingStore()==MSTrue)
	    {
	      int diff=MSUtil::abs(oldFirstRow-firstRow());
	      if (diff<rows())
	       {
		 if (oldFirstRow<firstRow()) scrollUp(diff);
		 else scrollDown(diff);
	       }
	      else redrawImmediately();
	    }
	   else redrawImmediately();
	 }
      }
   }
}

template <class Element>
void MSTreeListView<Element>::firstNode(const TreeModelCursor &modelCursor_)
{
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     unsigned long pos;
     ScreenVectorCursor screenCursor=findScreenCursor(resourceCursor,pos);
     if (screenCursor.isValid()) firstRow(pos-1);
   }
}

template <class Element>
MSTreeListView<Element>::TreeModelCursor MSTreeListView<Element>::firstNode(void) const
{
  if (MSView::model()!=0&&numRows()>0&&firstRow()<numRows())
   {
     const ResourceTreeCursor resourceCursor=screenVector().elementAtPosition(firstRow()+1);
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     return (info->modelCursor());
   }
  else return TreeModelCursor(TreeModel());
}

template <class Element>
void MSTreeListView<Element>::scrollUp(int numToScroll_)
{
  int offset=highlightThickness()+shadowThickness();
  int border=offset*2;
  int viewWidth=width()-border;
  viewWidth-=(vsb()->mapped()==MSTrue)?vsb()->width():0;
  int viewHeight=height()-border;
  viewHeight-=(hsb()->mapped()==MSTrue)?hsb()->height():0;
  int emptyRegionHeight=numToScroll_*maxRowHeight();
  int x=offset;
  int y=offset+emptyRegionHeight;
  int w=viewWidth;
  int h=viewHeight-emptyRegionHeight;
  XCopyArea(display(),window(),window(),gc().gc(),x,y,w,h,x,offset);
  int from=firstRow()+rows()-numToScroll_;
  int to=firstRow()+rows();
  clearDrawingArea(redrawPixmap()->pixmap());
  drawRows(redrawPixmap()->pixmap(),from,to);
  drawingAreaShadow(redrawPixmap()->pixmap());
  drawHighlight(redrawPixmap()->pixmap());
  y=offset+(from-firstRow())*maxRowHeight();
  h=(numToScroll_+1)*maxRowHeight();
  XCopyArea(display(),redrawPixmap()->pixmap(),window(),gc().gc(),x,y,w,h,x,y);
}

template <class Element>
void MSTreeListView<Element>::scrollDown(int numToScroll_)
{
  clearDrawingArea(redrawPixmap()->pixmap());
  int offset=highlightThickness()+shadowThickness();
  int border=offset*2;
  int viewWidth=width()-border;
  viewWidth-=(vsb()->mapped()==MSTrue)?vsb()->width():0;
  int viewHeight=height()-border;
  viewHeight-=(hsb()->mapped()==MSTrue)?hsb()->height():0;
  int emptyRegionHeight=numToScroll_*maxRowHeight();
  int x=offset;
  int y=offset;
  int w=viewWidth;
  int h=viewHeight-emptyRegionHeight;
  XCopyArea(display(),window(),redrawPixmap()->pixmap(),gc().gc(),x,y,w,h,x,y+emptyRegionHeight);
  int from=firstRow();
  int to=firstRow()+numToScroll_-1;
  drawRows(redrawPixmap()->pixmap(),from,to);
  XCopyArea(display(),redrawPixmap()->pixmap(),window(),gc().gc(),offset,offset,viewWidth,viewHeight,offset,offset);
}

template <class Element>
void MSTreeListView<Element>::collapsedButtonPixmap(const MSPixmap &collapsedButtonPixmap_)
{
  if (_collapsedButtonPixmap!=0) delete _collapsedButtonPixmap;
  _collapsedButtonPixmap=new MSPixmap(collapsedButtonPixmap_);
  adjustView();
}

template <class Element>
void MSTreeListView<Element>::expandedButtonPixmap(const MSPixmap &expandedButtonPixmap_)
{
  if (_expandedButtonPixmap!=0) delete _expandedButtonPixmap;
  _expandedButtonPixmap=new MSPixmap(expandedButtonPixmap_);
  adjustView();
}

template <class Element>
void MSTreeListView<Element>::buttonShadowThickness(int buttonShadowThickness_)
{
  if (_buttonShadowThickness!=buttonShadowThickness_)
   {
     _buttonShadowThickness=buttonShadowThickness_;
     adjustView();
   }
}

template <class Element>
void MSTreeListView<Element>::buttonShadowStyle(MSShadowStyle buttonShadowStyle_)
{
  if (_buttonShadowStyle!=buttonShadowStyle_)
   {
     _buttonShadowStyle=buttonShadowStyle_;
     redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::selectedNode(const TreeModelCursor &modelCursor_)
{
  TreeModelCursor *oldSelectedNode=_selectedNode;
  if (oldSelectedNode==0||*oldSelectedNode!=modelCursor_)
   {
     if (oldSelectedNode!=0&&oldSelectedNode->isValid()&&mapped()==MSTrue)
      {
	_selectedNode=0;
	updateRow(*oldSelectedNode);
	_selectedNode=oldSelectedNode;
      }
   }
  if (_selectedNode!=0) *_selectedNode=modelCursor_;
  else if (MSView::model()!=0)
   {
     _selectedNode=new TreeModelCursor(modelTree());
     *_selectedNode=modelCursor_;
   }
  updateRow(*_selectedNode);
}

template <class Element>
MSTreeListView<Element>::TreeModelCursor MSTreeListView<Element>::selectedNode(void) const
{
  if (MSView::model()!=0)
   {
     if (_selectedNode==0) return TreeModelCursor(modelTree());
     else return *_selectedNode;
   }
  else return TreeModelCursor(TreeModel());
}

template <class Element>
MSTreeListView<Element>::TreeModelCursor MSTreeListView<Element>::activatedNode(void) const
{
  if (MSView::model()!=0)
   {
     if (_activatedNode==0) return TreeModelCursor(modelTree());
     else return *_activatedNode;
   }
  else return TreeModelCursor(TreeModel());
}

template <class Element>
void MSTreeListView<Element>::sensitive(const TreeModelCursor &modelCursor_,MSBoolean sensitive_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (info->sensitive()!=sensitive_)
      {
	info->sensitive(sensitive_);
	if (firstMap()==MSTrue)
	 {
	   unsigned long pos;
	   ScreenVectorCursor screenCursor=findScreenCursor(resourceCursor,pos);
	   if (screenCursor.isValid()&&isRowOnScreen(pos-1))
	    {
	      updateRow(info,pos-1);
	    }
	 }
      }
   }
}

template <class Element>
MSBoolean MSTreeListView<Element>::sensitive(const TreeModelCursor &modelCursor_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
     return nodeInfo->sensitive();
     
   }
  else
   {
     reportError("Warning : MSTreeListView::sensitive() - Can't find element at cursor");
     return MSFalse;
   }
}

template <class Element>
void MSTreeListView<Element>::expandedState(const TreeModelCursor &modelCursor_,MSBoolean expandedState_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (info->expandedState()!=expandedState_)
      {
	info->expandedState(expandedState_);
	if (info->expandable()==MSTrue)
	 {
	   if (firstMap()==MSTrue)
	    {
	      unsigned long pos;
	      ScreenVectorCursor screenCursor=findScreenCursor(resourceCursor,pos);
	      if (screenCursor.isValid()&&isRowOnScreen(pos-1))
	       {
		 if (info->expandedState()==MSTrue) expandSubTree(screenCursor,pos);
		 else collapseSubTree(screenCursor,pos);
	       }
	    }
	 }
      }
   }
}

template <class Element>
MSBoolean MSTreeListView<Element>::expandedState(const TreeModelCursor &modelCursor_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
     return nodeInfo->expandedState();
   }
  else
   {
     reportError("Warning : MSTreeListView::expandedState() - Can't find element at cursor");
     return MSFalse;
   }
}

template <class Element>
void MSTreeListView<Element>::expandable(const TreeModelCursor &modelCursor_,MSBoolean expandable_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (info->expandable()!=expandable_)
      {
	info->expandable(expandable_);
	if (firstMap()==MSTrue)
	 {
	   unsigned long pos;
	   ScreenVectorCursor screenCursor=findScreenCursor(resourceCursor,pos);
	   if (screenCursor.isValid()&&isRowOnScreen(pos-1))
	    {
	      if (info->expandable()==MSTrue&&info->expandedState()==MSTrue) expandSubTree(screenCursor,pos);
	      else updateRow(info,pos-1);
	    }
	 }
      }
   }
}

template <class Element>
MSBoolean MSTreeListView<Element>::expandable(const TreeModelCursor &modelCursor_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.

  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
     return nodeInfo->expandable();
   }
  else
   {
     reportError("Warning : MSTreeListView::expandable() - Can't find element at cursor");
     return MSFalse;
   }
}

template <class Element>
void MSTreeListView<Element>::pixmap(const TreeModelCursor &modelCursor_,const MSStringVector &pixmap_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     info->buildPixmapList(pixmapRegistry(),info->pixmap(),pixmap_);
     if (firstMap()==MSTrue) adjustView();
   }
}

template <class Element>
MSStringVector MSTreeListView<Element>::pixmap(const TreeModelCursor &modelCursor_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  MSStringVector names;
  if (resourceCursor.isValid())
   {
     const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
     const PixmapList &pixmapList=nodeInfo->pixmap();
     unsigned long nElements=pixmapList.length();
     for (unsigned long i=0;i<nElements;i++)
      {
	names<<pixmapList.elementAt(i).name();
      }
   }
  else
   {
     reportError("Warning : MSTreeListView::pixmap() - Can't find element at cursor");
   }
  return names;
}

template <class Element>
void MSTreeListView<Element>::insensitivePixmap(const TreeModelCursor &modelCursor_,const MSStringVector &insensitivePixmap_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     info->buildPixmapList(pixmapRegistry(),info->insensitivePixmap(),insensitivePixmap_);
     if (firstMap()==MSTrue) adjustView();
   }
}

template <class Element>
MSStringVector MSTreeListView<Element>::insensitivePixmap(const TreeModelCursor &modelCursor_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  MSStringVector names;
  if (resourceCursor.isValid())
   {
     const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
     const PixmapList &pixmapList=nodeInfo->insensitivePixmap();
     unsigned long nElements=pixmapList.length();
     for (unsigned long i=0;i<nElements;i++)
      {
	names<<pixmapList.elementAt(i).name();
      }
   }
  else
   {
     reportError("Warning : MSTreeListView::insensitivePixmap() - Can't find element at cursor");
   }
  return names;
}

template <class Element>
void MSTreeListView<Element>::selectedPixmap(const TreeModelCursor &modelCursor_,const MSStringVector &selectedPixmap_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     info->buildPixmapList(pixmapRegistry(),info->selectedPixmap(),selectedPixmap_);
     if (firstMap()==MSTrue) adjustView();
   }
}

template <class Element>
MSStringVector MSTreeListView<Element>::selectedPixmap(const TreeModelCursor &modelCursor_) const
{
  // load the resource tree if it hasn't been loaded.  But since buildResourceTreeAndScreenVector()
  // is not a const method, we need to cast away the const-ness.  This is a work around in order
  // to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  MSStringVector names;
  if (resourceCursor.isValid())
   {
     const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
     const PixmapList &pixmapList=nodeInfo->selectedPixmap();
     unsigned long nElements=pixmapList.length();
     for (unsigned long i=0;i<nElements;i++)
      {
	names<<pixmapList.elementAt(i).name();
      }
   }
  else
   {
     reportError("Warning : MSTreeListView::selectedPixmap() - Can't find element at cursor");
   }
  return names;
}

template <class Element>
void MSTreeListView<Element>::registerPixmap(const MSPixmap &pixmap_)
{
  pixmapRegistry().addOrReplaceElementWithKey(pixmap_);
}

template <class Element>
const MSPixmap *MSTreeListView<Element>::pixmap(const MSString &key_) const
{
  PixmapRegistryCursor cursor(_pixmapRegistry);
  _pixmapRegistry.locateElementWithKey(key_,cursor);
  if (cursor.isValid()) return &_pixmapRegistry.elementAt(cursor);
  else return 0;
}

template <class Element>
void MSTreeListView<Element>::clearDrawingArea(Window window_)
{
  XFillRectangle(display(),window_,backgroundShadowGC(),0,0,width(),height());
}

template <class Element>
void MSTreeListView<Element>::drawingAreaShadow(Window window_)
{
  if (shadowThickness()>0)
   {
     int viewWidth=width()-highlightThickness()*2;
     viewWidth-=(vsb()->mapped()==MSTrue)?vsb()->width():0;
     int viewHeight=height()-highlightThickness()*2;
     viewHeight-=(hsb()->mapped()==MSTrue)?hsb()->height():0;
     drawBevel(window_,MSRect(highlightThickness(),highlightThickness(),viewWidth,viewHeight),shadowStyle(),shadowThickness());
   }
}

template <class Element>
void MSTreeListView<Element>::redrawImmediately(void)
{
  if (frozen()==MSFalse&&mapped()==MSTrue) redraw();
}

template <class Element>
void MSTreeListView<Element>::redraw(void)
{
  clearDrawingArea(redrawPixmap()->pixmap());
  if (MSView::model()!=0&&screenVector().isEmpty()==False)
   {
     int lastRow=firstRow()+rows();
     drawRows(redrawPixmap()->pixmap(),firstRow(),lastRow);
   }
  drawingAreaShadow(redrawPixmap()->pixmap());
  drawHighlight(redrawPixmap()->pixmap());
  XCopyArea(display(),redrawPixmap()->pixmap(),window(),backgroundShadowGC(),0,0,width(),height(),0,0);
}

template <class Element>
void MSTreeListView<Element>::unfreeze(void)
{
  MSWidgetCommon::unfreeze();
  buildScreenVector();
  adjustView();
}

template <class Element>
void MSTreeListView<Element>::drawHighlight(Window window_)
{
  if (highlightThickness()>0)
   {
     GC gc=(highlighted()==MSTrue)?highlightGC():backgroundShadowGC();
     drawFlatShadow(window_,MSRect(0,topShadowOffset(),width(),height()-topShadowOffset()),
		    highlightThickness(),gc);
   }
}


template <class Element>
void MSTreeListView<Element>::clearRow(Window window_,int row_)
{
  int lastRow=firstRow()+rows();
  if (row_>=firstRow()&&row_<=lastRow)
   {
     int offset=highlightThickness()+shadowThickness();
     int x=offset;
     int y=offset+maxRowHeight()*(row_-firstRow());
     int w=width()-offset*2-(vsb()->mapped()==MSTrue?vsb()->width():0);
     int h=maxRowHeight();
     XFillRectangle(display(),window_,backgroundShadowGC(),x,y,w,h);
   }
}

template <class Element>
void MSTreeListView<Element>::buildCollapsedButtonPixmap(void)
{
  static char MSTreeListView_Collapsed_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x60, 0x00, 0x60, 0x00, 0xfc, 0x03,
   0xfc, 0x03, 0x60, 0x00, 0x60, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00};

  if (_collapsedButtonPixmap!=0) delete _collapsedButtonPixmap;
  _collapsedButtonPixmap=new MSPixmap(server(),MSTreeListViewCollapsedButtonPixmapName,
				      MSTreeListView_Collapsed_bits,
                                      MSTreeListView_Collapsed_width,
                                      MSTreeListView_Collapsed_height);
}

template <class Element>
void MSTreeListView<Element>::buildExpandedButtonPixmap(void)
{
  static char MSTreeListView_Expanded_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x03,
   0xfc, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  };

  if (_expandedButtonPixmap!=0) delete _expandedButtonPixmap;
  _expandedButtonPixmap=new MSPixmap(server(),MSTreeListViewExpandedButtonPixmapName,
				     MSTreeListView_Expanded_bits,
                                     MSTreeListView_Expanded_width,
                                     MSTreeListView_Expanded_height);
}

template <class Element>
void MSTreeListView<Element>::updateRow(ManagedInfo &nodeInfo_,int row_)
{
  if (frozen()==MSFalse&&mapped()==MSTrue&&isRowOnScreen(row_)==MSTrue)
   {
     clearRow(redrawPixmap()->pixmap(),row_);
     drawRow(redrawPixmap()->pixmap(),nodeInfo_,row_);
     drawingAreaShadow(redrawPixmap()->pixmap());
     drawHighlight(redrawPixmap()->pixmap());
     int offset=highlightThickness()+shadowThickness();
     int x=offset;
     int y=offset+maxRowHeight()*(row_-firstRow());
     int w=width()-offset*2-((vsb()->mapped()==MSTrue)?vsb()->width():0);
     int h=maxRowHeight();
     XCopyArea(display(),redrawPixmap()->pixmap(),window(),gc().gc(),x,y,w,h,x,y);
   }
}

template <class Element>
void MSTreeListView<Element>::updateRow(TreeModelCursor &modelCursor_)
{
  int lastRow=firstRow()+rows();
  ScreenVectorCursor screenCursor(screenVector());
  int nRows=numRows();
  // We're checking pass lastRow because we Do draw pass the last row
  for (int i=firstRow();i<=lastRow&&i<nRows;i++)
   {
     screenVector().setToPosition(i+1,screenCursor);
     if (screenCursor.isValid()==False) break;
     else
      {
	ResourceTreeCursor &resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &node=resourceTree().elementAt(resourceCursor);
	if (modelCursor_==node->modelCursor())
	 {
	   updateRow(node,i);
	   break;
	 }
      }
   }
}

// This method assumes to_ is always bigger than from_
template <class Element>
void MSTreeListView<Element>::drawRows(Window window_,int from_,int to_)
{
  int nRows=numRows();
  int lastRow=firstRow()+rows();
  lastRow=(lastRow>=nRows)?nRows-1:lastRow;
  from_=(from_<firstRow())?firstRow():from_;
  to_=(to_>lastRow)?lastRow:to_;
  int offset=highlightThickness()+shadowThickness();
  int x=offset-hsb()->value();
  int y=offset+maxRowHeight()*(from_-firstRow());
  ScreenVectorCursor cursor(screenVector());
  screenVector().setToPosition(from_+1,cursor);
  for (int i=from_;i<=to_;i++)
   {
     if (cursor.isValid()==False) break;
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor);
     ManagedInfo &node=resourceTree().elementAt(resourceCursor);
     drawRow(window_,node,x,y);
     y+=maxRowHeight();
     cursor.setToNext();
   }     
}

template <class Element>
void MSTreeListView<Element>::drawRow(Window window_,const ManagedInfo &node_,int row_)
{
  int lastRow=firstRow()+rows();
  if (row_<numRows()&&row_>=firstRow()&&row_<=lastRow)
   {
     int offset=highlightThickness()+shadowThickness();
     int x=offset-hsb()->value();
     int y=offset+maxRowHeight()*(row_-firstRow());
     drawRow(window_,node_,x,y);
   }
}

template <class Element>
void MSTreeListView<Element>::drawRow(Window window_,const ManagedInfo &node_,int x_,int y_)
{
  int bWidth=buttonWidth();
  int level=node_->level();
  int loffset=levelOffset();
  int totalOffset=level*loffset;
  int midY=y_+maxRowHeight()/2;
  int fullY=y_+maxRowHeight();
  int drawLine=(showLines()==MSTrue)&&level>0;
  if (drawLine)
   {
     int len=branchLength();
     int x=x_+totalOffset-len;
     XDrawLine(display(),window_,linegc().gc(),x,midY,x+len,midY);
     if (node_->isLastChild()==MSTrue) XDrawLine(display(),window_,linegc().gc(),x,y_,x,midY);
     else XDrawLine(display(),window_,linegc().gc(),x,y_,x,fullY);
     NodeInfo *parent=node_->parent();
     for (int i=node_->level()-2;i>=0;i--)
      {
	x-=loffset;
	if (parent->isLastChild()==MSFalse) XDrawLine(display(),window_,linegc().gc(),x,y_,x,fullY);
	parent=parent->parent();
      }
   }

  int x=x_+totalOffset;
  int y=y_;
  if (showButtons()==MSTrue)
   {
     if (isExpandable(node_)==MSTrue)
      {
        int pixmapWidth,pixmapHeight;
	x+=rowHorizontalSpacing();
        MSBoolean expanded=isExpanded(node_);
	if (expanded==MSTrue)
         {
           pixmapWidth=expandedButtonPixmap().width();
           pixmapHeight=expandedButtonPixmap().height();
         }
	else
         {
           pixmapWidth=collapsedButtonPixmap().width();
           pixmapHeight=collapsedButtonPixmap().height();
         }
	y=y_+(maxRowHeight()-pixmapHeight-buttonShadowThickness()*2)/2;
        int pixmapX=x+buttonShadowThickness();
        int pixmapY=y+buttonShadowThickness();
	copyPixmap(display(),expanded==MSTrue?expandedButtonPixmap():collapsedButtonPixmap(),
		   window_,pixmapgc().gc(),pixmapX,pixmapY);
        if (buttonShadowThickness()>0)
	 {
	   drawBevel(window_,
		     MSRect(x,y,pixmapWidth+buttonShadowThickness()*2,
			    pixmapHeight+buttonShadowThickness()*2),
		     buttonShadowStyle(),buttonShadowThickness());
	 }
	x+=bWidth;
      }
     else if (drawLine)
      {
	XDrawLine(display(),window_,linegc().gc(),x,midY,x+bWidth+rowHorizontalSpacing(),midY);
	x+=bWidth+rowHorizontalSpacing();
      }
     else if (showRootNode()==MSFalse||level>0) x+=bWidth+rowHorizontalSpacing();
   }
  x+=rowHorizontalSpacing();
  y=y_;
  if (isSelected(node_->modelCursor())==MSTrue) drawPixmaps(window_,selectedPixmap(node_),x,y);
  else if (isSensitive(node_)==MSTrue) drawPixmaps(window_,pixmap(node_),x,y);
  else drawPixmaps(window_,insensitivePixmap(node_),x,y);
  if (tabSpacing()>0) drawTabbedString(window_,node_,x,y);
  else drawString(window_,node_,x,y);
}

template <class Element>
void MSTreeListView<Element>::drawString(Window window_,const ManagedInfo &node_,int x_,int y_)
{
  MSString buffer;
  formatOutput(buffer,node_->modelCursor()); 
  if (buffer.length()>0)
   {
     int tWidth=fontObject().textWidth(buffer);
     int tHeight=fontObject().textHeight();
     int offset=selectedHighlightThickness()+highlightGap();
     int offset2=offset*2;
     if (isSelected(node_->modelCursor())==MSTrue)
      {
	gc().foreground(selectedRowBackground());
	int y=y_+(maxRowHeight()-tHeight-offset2)/2;
	if (hasFocus()==MSTrue)
	 {
	   gc().lineWidth(selectedHighlightThickness());
	   XDrawRectangle(display(),window_,gc().gc(),x_,y,tWidth+offset2-1,tHeight+offset2-1);
	   gc().lineWidth(1);
	 }
	x_+=offset;
	XFillRectangle(display(),window_,gc().gc(),x_,y+offset,tWidth,tHeight);
	gc().foreground(selectedRowForeground());
      }
     else
      {
	x_+=offset;
	gc().foreground(nodeForeground(node_));
      }
     MSBoolean nodeSensitive=isSensitive(node_);
     if (nodeSensitive==MSTrue) gc().fillStyle(FillSolid);
     else gc().fillStyle(FillStippled);
     int y=y_+(maxRowHeight()-tHeight-offset2)/2+fontObject().textAscent()+offset;
     XDrawString(display(),window_,gc().gc(),fontObject().fontStruct(),x_,y,buffer,buffer.length());
     //Reset the fillStyle
     if (nodeSensitive==MSFalse) gc().fillStyle(FillSolid);
   }
}

template <class Element>
void MSTreeListView<Element>::drawTabbedString(Window window_,const ManagedInfo &node_,int x_,int y_)
{
  MSString buffer;
  formatOutput(buffer,node_->modelCursor()); 
  if (buffer.length()>0)
   {
     buffer.change('\t','\n');
     MSStringVector strings(buffer);
     if (strings.length()>0)
      {
	int tWidth=0;
	int tHeight=fontObject().textHeight();
	int tps=tabPixelSpacing();
	int offset=selectedHighlightThickness()+highlightGap();
	int offset2=offset*2;
	unsigned i;
	if (isSelected(node_->modelCursor())==MSTrue)
	 {
	   if (strings.length()>1)
	    {
	      tWidth=maxWidths()(0)+tps-x_-hsb()->value();
	      unsigned len=strings.length()-1;
	      for (i=1;i<len;i++) tWidth+=maxWidths()(i)+tps;
	      tWidth+=fontObject().textWidth(strings(len));
	    }
	   else tWidth=fontObject().textWidth(strings(0));
	   gc().foreground(selectedRowBackground());
	   int y=y_+(maxRowHeight()-tHeight-offset2)/2;
	   if (hasFocus()==MSTrue)
	    {
	      gc().lineWidth(selectedHighlightThickness());
	      XDrawRectangle(display(),window_,gc().gc(),x_,y,tWidth+offset2-1,tHeight+offset2-1);
	      gc().lineWidth(1);
	    }
	   x_+=offset;
	   XFillRectangle(display(),window_,gc().gc(),x_,y+offset,tWidth,tHeight);
	   gc().foreground(selectedRowForeground());
	 }
	else
	 {
	   x_+=offset;
	   gc().foreground(nodeForeground(node_));
	 }
        MSBoolean nodeSensitive=isSensitive(node_);
        if (nodeSensitive==MSTrue) gc().fillStyle(FillSolid);
	else gc().fillStyle(FillStippled);
	int x=x_;
	int y=y_+(maxRowHeight()-tHeight-offset2)/2+fontObject().textAscent()+offset;
	const MSString &text=strings(0);
	XDrawString(display(),window_,gc().gc(),fontObject().fontStruct(),x,y,text,text.length());
	for (i=1;i<strings.length();i++)
	 {
	   const MSString &text=strings(i);
	   x=tabStops()(i-1)-hsb()->value();
	   XDrawString(display(),window_,gc().gc(),fontObject().fontStruct(),x,y,text,text.length());
	 }
	//Reset the fillStyle
	if (nodeSensitive==MSFalse) gc().fillStyle(FillSolid);
      }
   }
}


template <class Element>
void MSTreeListView<Element>::drawPixmaps(Window window_,const PixmapList &pixmapList_,int &x_,int &y_)
{
  unsigned long nElements=pixmapList_.length();
  for (unsigned long i=0;i<nElements;i++)
   {
     const MSPixmap &pixmap=pixmapList_.elementAt(i);
     int y=y_+(maxRowHeight()-pixmap.height())/2;
     Pixmap clipMask=pixmap.clipMask();
     copyPixmap(display(),pixmap,window_,pixmapgc().gc(),x_,y);
     x_+=pixmap.width()+rowHorizontalSpacing();
   }
}

template <class Element>
void MSTreeListView<Element>::naturalSize(void)
{
  setGeometryDefaults();
  resetFirstRowColumn();
  adjustSize();
}

template <class Element>
MSBoolean MSTreeListView<Element>::isSensitive(const NodeInfo *nodeInfo_)
{ return nodeInfo_->sensitive(); }

template <class Element>
MSBoolean MSTreeListView<Element>::isExpandable(const NodeInfo *nodeInfo_)
{ return nodeInfo_->expandable(); }

template <class Element>
MSBoolean MSTreeListView<Element>::isExpanded(const NodeInfo *nodeInfo_)
{
  if (nodeInfo_->expandable()==MSFalse) return MSFalse;
  else return nodeInfo_->expandedState();
}

template <class Element>
MSBoolean MSTreeListView<Element>::isNodeProtected(const NodeInfo *)
{ return isProtected();}

template <class Element>
const MSTreeListView<Element>::PixmapList &MSTreeListView<Element>::pixmap(const NodeInfo *nodeInfo_) 
{ return nodeInfo_->pixmap(); }

template <class Element>
const MSTreeListView<Element>::PixmapList &MSTreeListView<Element>::selectedPixmap(const NodeInfo *nodeInfo_)
{ return nodeInfo_->selectedPixmap(); }

template <class Element>
const MSTreeListView<Element>::PixmapList &MSTreeListView<Element>::insensitivePixmap(const NodeInfo *nodeInfo_)
{ return nodeInfo_->insensitivePixmap(); }

template <class Element>
unsigned long MSTreeListView<Element>::nodeForeground(const NodeInfo *)
{ return foreground(); }

template <class Element>
const char *MSTreeListView<Element>::formatOutput(MSString &buffer_,TreeModelCursor &cursor_)
{
  if (cursor_.isValid()) msTreeFormatType(elementForOps(modelTree().elementAt(cursor_)),buffer_);
  return buffer_.string();
}

template<class Element>
MSBoolean MSTreeListView<Element>::validate(TreeModelCursor &cursor_,const char *pString_)
{
  if (msTreeSetType(elementForOps(modelTree().elementAt(cursor_)),pString_)==MSError::MSSuccess) return MSTrue;
  else return MSFalse;
}

template <class Element>
void MSTreeListView<Element>::configure(void)
{
  _redrawPixmap->resize(width(),height());
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     int border=shadowThickness()*2+highlightThickness()*2;
     int requiredWidth=maxRowWidth()+border;
     int requiredHeight=numRows()*maxRowHeight()+border;
     if (width()>=requiredWidth) hsb()->hide();
     else
      {
	hsb()->show();
	requiredHeight+=hsb()->height();
      }
     if (height()>=requiredHeight) vsb()->hide();
     else
      {
	vsb()->show(); 
	if (hsb()->mapped()==MSFalse)
	 {
	   requiredWidth+=vsb()->width();
	   if (width()<requiredWidth) hsb()->show();
	 }
      }
     adjustRowColumn();
     adjustFirstRow();
     adjustFirstColumn();
     updateHsb();
     updateVsb();
     if (vsb()->mapped()==MSTrue&&hsb()->mapped()==MSTrue)
      {
	clipWindow()->resize(vsb()->width(),hsb()->height());
	clipWindow()->moveTo(hsb()->x()+hsb()->width(),vsb()->y()+vsb()->height());
	clipWindow()->show();
      }
     else clipWindow()->hide();
   }
}

template <class Element>
void MSTreeListView<Element>::adjustRowColumn(void)
{
  int border=highlightThickness()*2+shadowThickness()*2;
  int viewableWidth=width()-border;
  viewableWidth-=(vsb()->mapped()==MSTrue)?vsb()->width():0;
  int viewableHeight=height()-border;
  viewableHeight-=(hsb()->mapped()==MSTrue)?hsb()->height():0;
  int maxRH=(maxRowHeight()>0)?
            maxRowHeight():
            fontObject().textHeight()+selectedHighlightThickness()*2+highlightGap()*2; 
  _rows=viewableHeight/maxRH;
  _columns=viewableWidth/fontObject().charWidth();
}

template <class Element>
void MSTreeListView<Element>::adjustFirstRow(void)
{
  if (firstRow()<0||vsb()->mapped()==MSFalse) _firstRow=0;
  else 
   {
     if (firstRow()+rows()>numRows())
      {
	if (numRows()<=rows()) _firstRow=0;
	else _firstRow=numRows()-rows();
      }
   }
  vsb()->valueChange(firstRow());
}

template <class Element>
void MSTreeListView<Element>::adjustFirstColumn(void)
{
  if (hsb()->mapped()==MSTrue)
   {
     int viewSize=width()-highlightThickness()*2-shadowThickness()*2;
     if (hsb()->value()+viewSize>maxRowWidth())
      {
	if (viewSize<=maxRowWidth()) hsb()->valueChange(0);
	else hsb()->valueChange(maxRowWidth()-viewSize);
      }
   }
  else hsb()->valueChange(0);
}

template <class Element>
void MSTreeListView<Element>::adjustLevels(void)
{
  ResourceTreeCursor resourceCursor(resourceTree());
  for (resourceTree().setToFirst(resourceCursor,MSPreorder);
       resourceCursor.isValid();
       resourceTree().setToNext(resourceCursor,MSPreorder))
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (showRootNode()==MSTrue) info->level(info->level()+1);
     else info->level(info->level()-1);
   }
}

template <class Element>
void MSTreeListView<Element>::adjustSize(void)
{
  int oldMaxRW=maxRowWidth();
  int oldMaxRH=maxRowHeight();
  computeMaxRowSize();
  if (oldMaxRW!=maxRowWidth()||oldMaxRH!=maxRowHeight()) computeSize();
  else redrawImmediately();
}

template <class Element>
void MSTreeListView<Element>::adjustView(void)
{
  computeMaxRowSize();
  configure();
  redrawImmediately();
}

template <class Element>
MSBoolean MSTreeListView<Element>::locateSelectedNode(ResourceTreeCursor resourceCursor_)
{
  if (_selectedNode!=0&&resourceCursor_.isValid())
   {
     ManagedInfo &info=resourceTree().elementAt(resourceCursor_);
     if (info->modelCursor()==*_selectedNode) return MSTrue;
     else
      {
	for (resourceCursor_.setToFirstExistingChild();resourceCursor_.isValid();resourceCursor_.setToNextExistingChild())
	 {
	   if (locateSelectedNode(resourceCursor_)==MSTrue) return MSTrue;
	 }
	return MSFalse;
      }
   }
  else return MSFalse;
}

// computeSize() assumes _maxRowWidth and _maxRowHeight are properly set
template <class Element>
void MSTreeListView<Element>::computeSize(void)
{
  if (firstMap()==MSTrue)
   {
     int border=shadowThickness()*2+highlightThickness()*2;
     int requiredRows=numRows();
     int requiredColumns=maxRowWidth()/fontObject().charWidth();

     int w=columns()*fontObject().charWidth()+border;
     if (rows()<requiredRows) w+=vsb()->width();

     int maxRH=(maxRowHeight()>0)?
               maxRowHeight():
               fontObject().textHeight()+selectedHighlightThickness()*2+highlightGap()*2;
     int h=rows()*maxRH+border;
     if (columns()<requiredColumns) h+=hsb()->height();

     if (width()!=w||height()!=h)
      {
	resize(w,h);
      }
     else
      {
	configure();
	redrawImmediately();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::updateHsb(void)
{
  if (hsb()->mapped()==MSTrue)
   {
     int w=(vsb()->mapped()==MSTrue)?width()-vsb()->width()-highlightThickness()*2:width()-highlightThickness()*2;
     w=(w>0)?w:1;
     hsb()->width(w);
     hsb()->moveTo(highlightThickness(),height()-highlightThickness()-hsb()->height());
     hsb()->max(maxRowWidth());
     int viewSize=width()-highlightThickness()*2-shadowThickness()*2;
     if (vsb()->mapped()==MSTrue) viewSize-=vsb()->width();
     hsb()->viewSize(viewSize);
     hsb()->pageInc(viewSize);
   }
}

template <class Element>
void MSTreeListView<Element>::updateVsb(void)
{
  if (vsb()->mapped()==MSTrue)
   {
     int h=(hsb()->mapped()==MSTrue)?height()-hsb()->height()-highlightThickness()*2:height()-highlightThickness()*2;
     h=(h>0)?h:1;
     vsb()->height(h);
     vsb()->moveTo(width()-highlightThickness()-vsb()->width(),highlightThickness());
     vsb()->max(numRows());
     vsb()->viewSize(rows());
     vsb()->pageInc(rows()-1);
   }
}

template <class Element>
void MSTreeListView<Element>::vsbChanged(void)
{
  if(editor()->mapped()==MSTrue) editorActivate();
  if(editor()->mapped()==MSFalse) firstRow(vsb()->value());
  else vsb()->valueChange(firstRow());
}

template <class Element>
void MSTreeListView<Element>::hsbChanged(void)
{
  if(editor()->mapped()==MSTrue)  editorActivate();
  if(editor()->mapped()==MSFalse) redrawImmediately();
}


// Pre-condition for this method is that screenVector() is already populated.
template <class Element>
void MSTreeListView<Element>::computeMaxRowSize(void)
{
  maxRowHeight(0);
  maxRowWidth(0);
  tabStops().removeAll();
  maxWidths().removeAll();
  int maxHeight=0;
  if (numRows()!=0)
   {
     ScreenVectorCursor cursor(screenVector());
     for (cursor.setToFirst();cursor.isValid();cursor.setToNext())
      {
	ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor);
	const ManagedInfo &node=resourceTree().elementAt(resourceCursor);
	calculateRowSize(node,maxWidths(),maxHeight);
      }
     maxRowHeight(maxHeight);
     int maxWidth=0;
     if (maxWidths().length()>0)
      {
	int tps=tabPixelSpacing();
	int tabstoplen=maxWidths().length()-1;
	for (unsigned i=0;i<tabstoplen;i++)
	 {
	   maxWidth+=(maxWidths()(i)+tps);
	   tabStops()<<maxWidth;
	 }
	maxWidth+=(maxWidths()(maxWidths().length()-1)+selectedHighlightThickness()*2+highlightGap()*2);
      }
     maxRowWidth(maxWidth);
   }
}

template <class Element>
void MSTreeListView<Element>::computeMaxRowSize(ScreenVectorCursor &start_,ScreenVectorCursor &end_)
{
  tabStops().removeAll();
  int maxHeight=maxRowHeight();
  while (start_.isValid())
   {
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(start_);
     ManagedInfo &node=resourceTree().elementAt(resourceCursor);
     calculateRowSize(node,maxWidths(),maxHeight);
     if (end_.isValid()&&start_==end_) start_.invalidate();
     else start_.setToNext();
   }
  maxRowHeight(maxHeight);
  int maxWidth=0;
  if (maxWidths().length()>0)
   {
     int tps=tabPixelSpacing();
     int tabstoplen=maxWidths().length()-1;
     for (unsigned i=0;i<tabstoplen;i++)
      {
	maxWidth+=(maxWidths()(i)+tps);
	tabStops()<<maxWidth;
      }
     maxWidth+=(maxWidths()(maxWidths().length()-1)+selectedHighlightThickness()*2+highlightGap()*2);
   }
  maxRowWidth(maxWidth);
}

template <class Element>
void MSTreeListView<Element>::calculateRowSize(const ManagedInfo &node_,
								 MSIntVector &maxWidths_,
								 int &maxHeight_)
{
  // Each row can be consisted of one to three items along with the connecting lines.
  // The items are Collapsed/Expanded Pixmap, item pixmap, and item label.  Each item is
  // padded on either side by "rowHorizontalSpacing" number of pixels.

  int pixmapHeight=0,pixmapWidth=0;
  int items=0;
  int h=0;
  
  // We always use the Collapsed pixmap for the calculation of the button
  // if the Collapsed and the Expanded pixmap are different in size...,
  // well life is tough, isn't it?

  if (buttonHeight()>0)
   {
     items++;
     h=buttonHeight();
   }
  
  // Calculate the maximum pixmap width and height
  int pw=0,ph=0;
  calculatePixmapSize(pixmap(node_),pw,ph);
  pixmapHeight=(ph>pixmapHeight)?ph:pixmapHeight;
  pixmapWidth=(pw>pixmapWidth)?pw:pixmapWidth;

  calculatePixmapSize(insensitivePixmap(node_),pw,ph);
  pixmapHeight=(ph>pixmapHeight)?ph:pixmapHeight;
  pixmapWidth=(pw>pixmapWidth)?pw:pixmapWidth;

  calculatePixmapSize(selectedPixmap(node_),pw,ph);
  pixmapHeight=(ph>pixmapHeight)?ph:pixmapHeight;
  pixmapWidth=(pw>pixmapWidth)?pw:pixmapWidth;

  if (pixmapWidth>0)
   {
     items++;
     h=(pixmapHeight>h)?pixmapHeight:h;
   }

  int level=node_->level();
  level=(level>0)?level:0;
  // Calculate the text width and height
  MSString buffer;
  formatOutput(buffer,node_->modelCursor());
  if (buffer.length()>0)
   {
     items+=2; //If text output is non-empty, add 2 to number of items
     // Only do this expensive algorithm for tabs if tab spacing is greater than zero
     if (tabSpacing()>0)
      {
	buffer.change('\t','\n');
	MSStringVector strings(buffer);
	int firstSegmentWidth=buttonWidth()+pixmapWidth+fontObject().textWidth(strings(0))
	+items*rowHorizontalSpacing()+level*levelOffset();
	if (maxWidths_.length()>0) maxWidths_[0]=(firstSegmentWidth>maxWidths_(0))?firstSegmentWidth:maxWidths_(0);
	else maxWidths_<<firstSegmentWidth;
	for (unsigned i=1;i<strings.length();i++)
	 {
	   int textWidth=fontObject().textWidth(strings(i));
	   if (maxWidths_.length()>i) maxWidths_[i]=(textWidth>maxWidths_(i))?textWidth:maxWidths_(i);
	   else maxWidths_<<textWidth;
	 }
      }
     else
      {
	int firstSegmentWidth=buttonWidth()+pixmapWidth+fontObject().textWidth(buffer)
	+items*rowHorizontalSpacing()+level*levelOffset();
	if (maxWidths_.length()>0) maxWidths_[0]=(firstSegmentWidth>maxWidths_(0))?firstSegmentWidth:maxWidths_(0);
	else maxWidths_<<firstSegmentWidth;
      }
     int textHeight=fontObject().textHeight()+selectedHighlightThickness()*2+highlightGap()*2; 
     h=(textHeight>h)?textHeight:h;
   }
  else
   {
     items++;
     int firstSegmentWidth=buttonWidth()+pixmapWidth+items*rowHorizontalSpacing()+level*levelOffset();
     if (maxWidths_.length()>0) maxWidths_[0]=(firstSegmentWidth>maxWidths_(0))?firstSegmentWidth:maxWidths_(0);
     else maxWidths_<<firstSegmentWidth;
   }

  h+=rowVerticalSpacing();
  maxHeight_=(h>maxHeight_)?h:maxHeight_;
}

template <class Element>
void MSTreeListView<Element>::calculatePixmapSize(const PixmapList &pixmapList_,
								    int &w_,int &h_)
{
  w_=h_=0;
  unsigned long nElements=pixmapList_.length();
  for (unsigned long i=0;i<nElements;i++)
   {
     w_+=pixmapList_.elementAt(i).width();
     if (i!=nElements-1) w_+=rowHorizontalSpacing();
     int pHeight=pixmapList_.elementAt(i).height();
     h_=(pHeight>h_)?pHeight:h_;
   }
}

template <class Element>
void MSTreeListView<Element>::firstMapNotify(void)
{
  if (MSView::model()!=0) rebuildScreen(MSTrue);
}

template <class Element>
void MSTreeListView<Element>::focusIn(void)
{
  hasFocus(MSTrue);
  highlight();
  if (editor()->mapped()==MSTrue) takeFocusNotify(editor());
  else if (_selectedNode!=0) updateRow(*_selectedNode);
}

template <class Element>
void MSTreeListView<Element>::focusOut(void)
{
  hasFocus(MSFalse);
  unHighlight();
  if (editor()->mapped()==MSTrue) loseFocusNotify(editor());
  else if (_selectedNode!=0) updateRow(*_selectedNode);
}

template <class Element>
void MSTreeListView<Element>::receiveEvent(MSEvent &event_)
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
void MSTreeListView<Element>::updateData(void)
{
  screenVector().removeAll();
  resourceTree().removeAll();
  resetFirstRowColumn();
  resetSelectedNode();
  if (firstMap()==MSTrue) rebuildScreen(MSFalse);
}

template <class Element>
void MSTreeListView<Element>::resetFirstRowColumn(void)
{
  _firstRow=0;
  vsb()->valueChange(0);
  hsb()->valueChange(0);
}

template <class Element>
void MSTreeListView<Element>::resetSelectedNode(void)
{
  if (_selectedNode!=0)
   {
     delete _selectedNode;
     _selectedNode=0;
   }
}

template <class Element>
int MSTreeListView<Element>::branchLength(void)
{
  int loffset=levelOffset();
  int indent=buttonWidth()/2;
  indent=(indent>0)?indent:fontObject().charWidth()/2;
  int len=loffset-indent-rowHorizontalSpacing();
  return (len>0)?len:0;
}

template <class Element>
int MSTreeListView<Element>::rowFromY(int y_)
{
  y_-=(highlightThickness()+shadowThickness());
  if (y_>0&&maxRowHeight()>0)
   {
     int row=y_/maxRowHeight();
     row+=firstRow();
     return (row<numRows())?row:-1;
   }
  else return -1;
}

template <class Element>
MSTreeListView<Element>::ResourceTreeCursor MSTreeListView<Element>::findResourceCursor(const TreeModelCursor &modelCursor_) const
{
  ResourceTreeCursor resourceCursor(resourceTree());
  for (resourceTree().setToFirst(resourceCursor,MSPreorder);
       resourceCursor.isValid();
       resourceTree().setToNext(resourceCursor,MSPreorder))
   {
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (info->modelCursor()==modelCursor_) break;
   }
  return resourceCursor;
}

template <class Element>
MSTreeListView<Element>::ScreenVectorCursor MSTreeListView<Element>::findScreenCursor(const ResourceTreeCursor &resourceCursor_,unsigned long &pos_) const
{
  ScreenVectorCursor screenCursor(screenVector());
  pos_=0;
  for (screenCursor.setToFirst();screenCursor.isValid();screenCursor.setToNext())
   {
     pos_++;
     const ResourceTreeCursor &rCursor=screenVector().elementAt(screenCursor);
     if (((ResourceTreeCursor&)rCursor)==resourceCursor_) break;
   }
  return screenCursor;
}

template <class Element>
void MSTreeListView<Element>::keyPress(const XEvent *pEvent_,KeySym keySym_,
                                       unsigned int state_,const char *pString_)
{
  MSKeyPress keyPress(keySym_, state_);
  if (MSWidget::sensitive()==MSTrue&&keyTranslate(keyPress)==MSFalse)
   {
     if(editor()->mapped()==MSTrue) keyPressNotify(editor(),pEvent_,keySym_,state_,pString_);
     else
      {
        
        switch(keySym_)
         {
         case XK_Up:	up(); break;
         case XK_Down:	down(); break;
         case XK_Left:	left(); break;
         case XK_Right:	right(); break;
         case XK_Home:
         case XK_F27:	home(); break;
         case XK_End:
         case XK_F33:	end(); break;
         case XK_Prior:
         case XK_F29:	pageUp(); break;
         case XK_Next:
         case XK_F35:	pageDown(); break;
         case XK_Return:
         case XK_KP_Enter:	returnKey(); break;
         case XK_plus:
         case XK_KP_Add:
           if(_selectedNode!=0)
            {
              MSBoolean state= expandedState(*_selectedNode);
              if(state == MSFalse) expandedState(*_selectedNode,MSTrue);
            }
           break;        
         case XK_KP_Subtract:
         case XK_F24:           
         case XK_minus:
           if(_selectedNode!=0)
            {
              MSBoolean state= expandedState(*_selectedNode);
              if(state == MSTrue) expandedState(*_selectedNode,MSFalse);
            }
           break;        
           
         default:
         {
           TreeModelCursor selectedCursor(selectedNode());
           ResourceTreeCursor resourceCursor=findResourceCursor(selectedCursor);
           if (resourceCursor.isValid())
            {
              ManagedInfo &info=resourceTree().elementAt(resourceCursor);
              if(isNodeProtected(info) == MSFalse)
               {
                 if (keySym_==XK_BackSpace)
                  {
                    editor()->string("");
                    mapEditor();
                  }
                 else if (keySym_==XK_Insert)
                  {
                    MSString string;
                    formatOutput(string,selectedCursor);
                    editor()->string(string);
                    editor()->editMode(MSTextField::InsertMode);
                    mapEditor();
                  }
                 else if (strlen(pString_)>0)
                  {
                    editor()->string("");	      
                    keyPressNotify(editor(),pEvent_,keySym_,state_,pString_);
                    if (editor()->length()>0) mapEditor();
                  }
               }
            }
         }
         }
      }
   }
}

template <class Element>
void MSTreeListView<Element>::up(void)
{
  if( screenVector().isEmpty()==False) 
   {
     unsigned long pos;
     ScreenVectorCursor screenCursor=nextUpScreenCursor(pos);
     if (screenCursor.isValid())
      {
	ResourceTreeCursor resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	selectedNode(info->modelCursor());
	int row=pos-1;
	if (row<firstRow()) firstRow(row);
	else if (row>=firstRow()+rows())
	 {
	   firstRow(row-rows()+1);
	 }
	nodeSelectionNotify();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::down(void)
{
  if( screenVector().isEmpty()==False) 
   {
     unsigned long pos;
     ScreenVectorCursor screenCursor=nextDownScreenCursor(pos);
     if (screenCursor.isValid())
      {
	ResourceTreeCursor resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	selectedNode(info->modelCursor());
	int row=pos-1;
	if (row<firstRow()) firstRow(row);
	else if (row>=firstRow()+rows())
	 {
	   firstRow(row-rows()+1);
	 }
	nodeSelectionNotify();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::left(void)
{
  if (hsb()->mapped()==MSTrue)
   {
     if (hsb()->value()>hsb()->min())
      {
	int newValue=hsb()->value()-fontObject().charWidth();
	newValue=(newValue>hsb()->min())?newValue:hsb()->min();
	hsb()->value(newValue);
      }
   }
}

template <class Element>
void MSTreeListView<Element>::right(void)
{
  if (hsb()->mapped()==MSTrue)
   {
     if (hsb()->value()<hsb()->max())
      {
	int newValue=hsb()->value()+fontObject().charWidth();
	newValue=(newValue<hsb()->max())?newValue:hsb()->max();
	hsb()->value(newValue);
      }
   }
}

template <class Element>
void MSTreeListView<Element>::home(void)
{
  int nRows=numRows();
  if (nRows!=0)
   {
     firstRow(0);
     const ResourceTreeCursor &resourceCursor=screenVector().elementAtPosition(1);
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (_selectedNode==0||info->modelCursor()!=selectedNode())
      {
	selectedNode(info->modelCursor());
	nodeSelectionNotify();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::end(void)
{
  int nRows=numRows();
  if (nRows!=0)
   {
     firstRow(numRows()-rows());
     const ResourceTreeCursor &resourceCursor=screenVector().elementAtPosition(nRows);
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (_selectedNode==0||info->modelCursor()!=selectedNode())
      {
	selectedNode(info->modelCursor());
	nodeSelectionNotify();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::pageUp(void)
{
  int nRows=numRows();
  if (nRows!=0&&nRows>rows())
   {
     firstRow(firstRow()-(rows()-1));
     const ResourceTreeCursor &resourceCursor=screenVector().elementAtPosition(firstRow()+rows());
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (_selectedNode==0||info->modelCursor()!=selectedNode())
      {
	selectedNode(info->modelCursor());
	nodeSelectionNotify();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::pageDown(void)
{
  int nRows=numRows();
  if (nRows!=0&&nRows>rows())
   {
     firstRow(firstRow()+(rows()-1));
     const ResourceTreeCursor &resourceCursor=screenVector().elementAtPosition(firstRow()+1);
     const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (_selectedNode==0||info->modelCursor()!=selectedNode())
      {
	selectedNode(info->modelCursor());
	nodeSelectionNotify();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::returnKey(void)
{
  if (_selectedNode!=0) doubleClickNotify();
}

template <class Element>
MSTreeListView<Element>::ScreenVectorCursor MSTreeListView<Element>::nextUpScreenCursor(unsigned long &pos_)
{
  ScreenVectorCursor screenCursor(screenVector());
  if (screenVector().isEmpty()==False)
   {
     if (_selectedNode==0)
      {
	screenCursor.setToLast();
	pos_=screenVector().numberOfElements();
      }
     else
      {
	pos_=0;
	for (screenVector().setToFirst(screenCursor);screenCursor.isValid();screenVector().setToNext(screenCursor))
	 {
	   ResourceTreeCursor &resourceCursor=screenVector().elementAt(screenCursor);
	   ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	   if (info->modelCursor()==*_selectedNode)
	    {
	      if (screenVector().isFirst(screenCursor)==False) screenCursor.setToPrevious();
	      else screenCursor.invalidate();
	      break;
	    }
	   else pos_++;
	 }
      }
     for (;screenCursor.isValid();screenVector().setToPrevious(screenCursor))
      {
	ResourceTreeCursor resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	if (info->sensitive()==MSTrue) break;
	else pos_--;
      }
   }
  return screenCursor;
}

template <class Element>
MSTreeListView<Element>::ScreenVectorCursor MSTreeListView<Element>::nextDownScreenCursor(unsigned long &pos_)
{
  ScreenVectorCursor screenCursor(screenVector());
  if (screenVector().isEmpty()==False)
   {
     if (_selectedNode==0)
      {
	screenCursor.setToFirst();
	pos_=1;
      }
     else
      {
	pos_=1;
	for (screenVector().setToFirst(screenCursor);screenCursor.isValid();screenVector().setToNext(screenCursor))
	 {
	   ResourceTreeCursor &resourceCursor=screenVector().elementAt(screenCursor);
	   ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	   if (info->modelCursor()==*_selectedNode)
	    {
	      if (screenVector().isLast(screenCursor)==False) screenCursor.setToNext();
	      else screenCursor.invalidate();
	      break;
	    }
	   else pos_++;
	 }
      }
     for (;screenCursor.isValid();screenVector().setToNext(screenCursor))
      {
	pos_++;
	ResourceTreeCursor resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	if (info->sensitive()==MSTrue) break;
      }
   }
  return screenCursor;
}


template <class Element>
void MSTreeListView<Element>::buttonPress(const XEvent *event_)
{
  if (MSWidget::sensitive()==MSTrue)
   {
     if (acceptFocus()==MSFalse||traverseFocus(this)==MSTrue)
      {
        if (editor()->mapped()==MSTrue)
         {
           editorActivate();
         }
        if (editor()->mapped()==MSFalse)
         {
           buttonPressNotify(this,event_);
         }
      }
   }
}

template <class Element>
void MSTreeListView<Element>::button1Press(const XEvent *event_)
{
  int row=rowFromY(event_->xbutton.y);
  if (row!=-1)
   {
     unsigned long position=row+1;
     ScreenVectorCursor cursor(screenVector());
     screenVector().setToPosition(position,cursor);
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor);
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (isSensitive(info)==MSTrue)
      {
	if (labelSelected(info,event_->xbutton.x,event_->xbutton.y)==MSTrue)
	 {
	   if (isSelected(info->modelCursor())==MSTrue)
	    {
	      if (isDoubleClick(event_)==MSTrue) doubleClickNotify();
	    }
	   else
	    {
	      eventTime(event_->xbutton.time);
	      selectedNode(info->modelCursor());
	      nodeSelectionNotify();
	    }

	 }
	else if (showButtons()==MSTrue&&isExpandable(info)==MSTrue)
	 {
	   if (buttonSelected(info,event_->xbutton.x,event_->xbutton.y)==MSTrue)
	    {
	      TreeModelCursor activatedCursor=info->modelCursor();
	      _activatedNode=&activatedCursor;

	      if (isExpanded(info)==MSTrue)
	       {
		 info->expandedState(MSFalse);
		 collapseSubTree(cursor,position);
		 subTreeCollapsed(info);
	       }
	      else
	       {
		 info->expandedState(MSTrue);
		 expandSubTree(cursor,position);
		 subTreeExpanded(info);
	       }
	    }
	 }
      }
   }
}

template <class Element>
void MSTreeListView<Element>::button2Press(const XEvent *event_)
{
  int row=rowFromY(event_->xbutton.y);
  if (row!=-1)
   {
     unsigned long position=row+1;
     ScreenVectorCursor cursor(screenVector());
     screenVector().setToPosition(position,cursor);
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor);
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (isSensitive(info)==MSTrue)
      {
	if (labelSelected(info,event_->xbutton.x,event_->xbutton.y)==MSTrue)
	 {
	   if (isSelected(info->modelCursor())==MSFalse)
	    {
	      eventTime(event_->xbutton.time);
	      selectedNode(info->modelCursor());
	      nodeSelectionNotify();
	    }
	   button2SelectionNotify();
	 }
      }
   }
}

template <class Element>
void MSTreeListView<Element>::button3Press(const XEvent *event_)
{
  int row=rowFromY(event_->xbutton.y);
  if (row!=-1)
   {
     unsigned long position=row+1;
     ScreenVectorCursor cursor(screenVector());
     screenVector().setToPosition(position,cursor);
     ResourceTreeCursor &resourceCursor=screenVector().elementAt(cursor);
     ManagedInfo &info=resourceTree().elementAt(resourceCursor);
     if (isSensitive(info)==MSTrue)
      {
	if (labelSelected(info,event_->xbutton.x,event_->xbutton.y)==MSTrue)
	 {
	   if (isSelected(info->modelCursor())==MSFalse)
	    {
	      eventTime(event_->xbutton.time);
	      selectedNode(info->modelCursor());
	      nodeSelectionNotify();
	    }
	   button3SelectionNotify();
	 }
      }
   }
}

template <class Element>
MSBoolean MSTreeListView<Element>::buttonSelected(ManagedInfo &info_,int x_,int)
{
  x_+=hsb()->value();
  int level=info_->level();
  int left=level*levelOffset()+highlightThickness()+shadowThickness()+rowHorizontalSpacing();
  if (x_>=left)
   {
     if (x_<left+buttonWidth()) return MSTrue;
     else return MSFalse;
   }
  else return MSFalse;
}

template <class Element>
MSBoolean MSTreeListView<Element>::labelSelected(ManagedInfo &info_,int x_,int)
{
  x_+=hsb()->value();
  int bWidth=buttonWidth();
  int rhs=(bWidth>0)?rowHorizontalSpacing()*2:rowHorizontalSpacing();
  int level=info_->level();
  int left=level*levelOffset()+bWidth+rhs+highlightThickness()+shadowThickness();
  if (x_>=left) return MSTrue;
  else return MSFalse;
}

template <class Element>
MSBoolean MSTreeListView<Element>::isSelected(TreeModelCursor &_modelCursor)
{
  if (_selectedNode==0) return MSFalse;
  else return (*_selectedNode==_modelCursor)?MSTrue:MSFalse;
}

template <class Element>
MSBoolean MSTreeListView<Element>::isRowOnScreen(int row_)
{
  int lastRow=firstRow()+rows();
  if (row_>=firstRow()&&row_<=lastRow) return MSTrue;
  else return MSFalse;
}

template <class Element>
void MSTreeListView<Element>::subTreeCollapsed(ManagedInfo &info_)
{
  TreeModelCursor activatedCursor=info_->modelCursor();
  _activatedNode=&activatedCursor;
  subTreeCollapsedNotify();
  _activatedNode=0;
}

template <class Element>
void MSTreeListView<Element>::subTreeExpanded(ManagedInfo &info_)
{
  TreeModelCursor activatedCursor=info_->modelCursor();
  _activatedNode=&activatedCursor;
  subTreeExpandedNotify();
  _activatedNode=0;
}

template <class Element>
void MSTreeListView<Element>::subTreeCollapsedNotify(void)
{ activateCallback(MSWidgetCallback::subtreecollapsed); }

template <class Element>
void MSTreeListView<Element>::subTreeExpandedNotify(void)
{ activateCallback(MSWidgetCallback::subtreeexpanded); }

template <class Element>
void MSTreeListView<Element>::doubleClickNotify(void)
{ activateCallback(MSWidgetCallback::doubleclick); }

template <class Element>
void MSTreeListView<Element>::nodeSelectionNotify(void)
{ activateCallback(MSWidgetCallback::selection); }

template <class Element>
void MSTreeListView<Element>::button2SelectionNotify(void)
{ activateCallback(MSWidgetCallback::button2selection);}

template <class Element>
void MSTreeListView<Element>::button3SelectionNotify(void)
{ activateCallback(MSWidgetCallback::button3selection);}

template <class Element>
void MSTreeListView<Element>::updateForeground(unsigned long oldFg_)
{
  MSWidgetCommon::updateForeground(oldFg_);
  XSetForeground(display(),pixmapgc().gc(),foreground());
  if (selectedRowBackground()==oldFg_&&selectedRowForeground()==background()) _selectedRowBackground=foreground();
  hsb()->foreground(foreground());
  vsb()->foreground(foreground());
  clipWindow()->foreground(foreground());
  redrawImmediately();
}

template <class Element>
void MSTreeListView<Element>::updateBackground(unsigned long oldBg_)
{
  MSWidgetCommon::updateBackground(oldBg_);
  XSetBackground(display(),pixmapgc().gc(),background());
  if (selectedRowForeground()==oldBg_&&selectedRowBackground()==foreground()) _selectedRowForeground=background();
  hsb()->background(background());
  vsb()->background(background());
  clipWindow()->background(background());
  redrawImmediately();
}

template <class Element>
void MSTreeListView<Element>::updateFont(Font font_)
{
  MSWidgetCommon::updateFont(font_);
  fontObject().fontStruct(server()->fontStruct(font()));
  hsb()->inc(fontObject().charWidth());
  gc().font(font());
  editor()->font(font()); 
  adjustView();
}

template <class Element>
void MSTreeListView<Element>::levelOffset(int levelOffset_)
{
  if (_levelOffset!=levelOffset_)
   {
     _levelOffset=levelOffset_;
     adjustView();
   }
}

template <class Element>
void MSTreeListView<Element>::tabSpacing(int tabSpacing_)
{
  if (_tabSpacing!=tabSpacing_)
   {
     _tabSpacing=tabSpacing_;
     adjustView();
   }
}

template <class Element>
void MSTreeListView<Element>::showButtons(MSBoolean showButtons_)
{
  if (_showButtons!=showButtons_)
   {
     _showButtons=showButtons_;
     adjustView();
   }
}

template <class Element>
void MSTreeListView<Element>::showLines(MSBoolean showLines_)
{
  if (_showLines!=showLines_)
   {
     _showLines=showLines_;
     redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::showRootNode(MSBoolean showRootNode_)
{
  if (_showRootNode!=showRootNode_)
   {
     _showRootNode=showRootNode_;
     adjustLevels();
     buildScreenVector();
     adjustView();
   }
}

template <class Element>
void MSTreeListView<Element>::lineForeground(const char *color_)
{ lineForeground(server()->pixel(color_)); }

template <class Element>
void MSTreeListView<Element>::lineForeground(unsigned long lineForeground_)
{
  static char MSTreeListView_dash_bits[] = {
   0xc7, 0x01, 0xc7, 0x01, 0xc7, 0x01, 0x38, 0x0e, 0x38, 0x0e, 0x38, 0x0e,
   0xc7, 0x01, 0xc7, 0x01, 0xc7, 0x01, 0x38, 0x0e, 0x38, 0x0e, 0x38, 0x0e};


  if (_lineForeground!=lineForeground_)
   {
     _lineForeground=lineForeground_;
     if (dotPixmap()!=0)
      {
        delete _dotPixmap;
        _dotPixmap=0;
        if (lineStyle()==MSDot)
         {
           _dotPixmap=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,lineForeground(),background());
           linegc().tile(dotPixmap()->pixmap());
         }
      }
     if (dashPixmap()!=0)
      {
        delete _dashPixmap;
        _dashPixmap=0;
        if (lineStyle()==MSDash)
         {
           _dashPixmap=new MSPixmap(server(),MSTreeListViewDashPixmapName,
                                    MSTreeListView_dash_bits,
                                    MSTreeListView_dash_width,
                                    MSTreeListView_dash_height,
                                    lineForeground(),background());
           linegc().tile(dashPixmap()->pixmap());
         }
      }
     linegc().foreground(lineForeground());
     redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::lineStyle(MSLineStyle lineStyle_)
{
  static char MSTreeListView_dash_bits[] = {
   0xc7, 0x01, 0xc7, 0x01, 0xc7, 0x01, 0x38, 0x0e, 0x38, 0x0e, 0x38, 0x0e,
   0xc7, 0x01, 0xc7, 0x01, 0xc7, 0x01, 0x38, 0x0e, 0x38, 0x0e, 0x38, 0x0e};

  if (_lineStyle!=lineStyle_)
   {
     _lineStyle=lineStyle_;
     if (lineStyle()==MSDot)
      {
        if (dotPixmap()==0)
         {
           _dotPixmap=new MSPixmap(server(),MSPixmap::ForegroundFiftyPixmap,lineForeground(),background());
         }
        linegc().tile(dotPixmap()->pixmap());
        linegc().fillStyle(FillTiled);
      }
     else if (lineStyle()==MSDash)
      {
        if (dashPixmap()==0) 
         {
           _dashPixmap=new MSPixmap(server(),MSTreeListViewDashPixmapName,
                                    MSTreeListView_dash_bits,
                                    MSTreeListView_dash_width,
                                    MSTreeListView_dash_height,
                                    lineForeground(),background());
        }
        linegc().tile(dashPixmap()->pixmap());
        linegc().fillStyle(FillTiled);
      }
     else linegc().fillStyle(FillSolid);
     if (showLines()==MSTrue) redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::selectedRowForeground(const char *color_)
{ selectedRowForeground(server()->pixel(color_)); }

template <class Element>
void MSTreeListView<Element>::selectedRowForeground(unsigned long selectedRowForeground_)
{
  if (_selectedRowForeground!=selectedRowForeground_)
   {
     _selectedRowForeground=selectedRowForeground_;
     redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::selectedRowBackground(const char *color_)
{ selectedRowBackground(server()->pixel(color_)); }

template <class Element>
void MSTreeListView<Element>::selectedRowBackground(unsigned long selectedRowBackground_)
{
  if (_selectedRowBackground!=selectedRowBackground_)
   {
     _selectedRowBackground=selectedRowBackground_;
     redrawImmediately();
   }
}

template <class Element>
int MSTreeListView<Element>::buttonWidth(void) const
{
  if (showButtons()==MSTrue) return collapsedButtonPixmap().width()+buttonShadowThickness()*2;
  else return 0;
}

template <class Element>
int MSTreeListView<Element>::buttonHeight(void) const
{
  if (showButtons()==MSTrue) return collapsedButtonPixmap().height()+buttonShadowThickness()*2;
  else return 0;
}

template <class Element>
int MSTreeListView<Element>::numRows(void) const
{
  return screenVector().numberOfElements();
}

template <class Element>
void MSTreeListView<Element>::useBackingStore(MSBoolean useBackingStore_)
{
  if (_useBackingStore!=useBackingStore_)
   {
     _useBackingStore=useBackingStore_;
     if (useBackingStore()==MSTrue)
      {
	// if the server doesn't support backing store, then set the flag back to MSFalse
	if (DoesBackingStore(screen())!=NotUseful) backingStore(WhenMapped);
	else _useBackingStore=MSFalse;
      }
     else backingStore(NotUseful);
   }
}

#if !defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
template <class Element>
MSTreeListView<Element>::NodeAttribute::NodeAttribute(void)
: _sensitive(MSTrue),_expandable(MSFalse),_expandedState(MSFalse)
{}

template <class Element>
MSTreeListView<Element>::NodeAttribute::~NodeAttribute(void)
{}

template <class Element>
MSTreeListView<Element>::NodeInfo::NodeInfo(const MSTreeListView<Element> *treeView_,
                                            const MSTreeListView<Element>::NodeAttribute &nodeAttribute_,
                                            MSTreeListView<Element>::TreeModelCursor &modelCursor_,
                                            NodeInfo *parent_,int level_)
: _modelCursor(modelCursor_),_parent(parent_),_level(level_)
{
  attribute(treeView_,nodeAttribute_);
}

template <class Element>
void MSTreeListView<Element>::NodeInfo::attribute(const MSTreeListView<Element> *treeView_,
                                                  const MSTreeListView<Element>::NodeAttribute &nodeAttribute_)
{
  sensitive(nodeAttribute_.sensitive());
  expandable(nodeAttribute_.expandable());
  expandedState(nodeAttribute_.expandedState());
  buildPixmapList(treeView_->pixmapRegistry(),pixmap(),nodeAttribute_.pixmap());
  buildPixmapList(treeView_->pixmapRegistry(),insensitivePixmap(),nodeAttribute_.insensitivePixmap());
  buildPixmapList(treeView_->pixmapRegistry(),selectedPixmap(),nodeAttribute_.selectedPixmap());
}

template <class Element>
MSTreeListView<Element>::NodeAttribute 
MSTreeListView<Element>::NodeInfo::attribute(void) const
{
  MSTreeListView<Element>::NodeAttribute nodeAttribute;
  nodeAttribute.sensitive(sensitive());
  nodeAttribute.expandable(expandable());
  nodeAttribute.expandedState(expandedState());
  MSStringVector names;
  unsigned long i,nElements;
  nElements=pixmap().length();
  for (i=0;i<nElements;i++)
   {
     names<<pixmap().elementAt(i).name();
   }
  nodeAttribute.pixmap(names);
  names.removeAll();
  nElements=insensitivePixmap().length();
  for (i=0;i<nElements;i++)
   {
     names<<insensitivePixmap().elementAt(i).name();
   }
  nodeAttribute.insensitivePixmap(names);
  names.removeAll();
  nElements=selectedPixmap().length();
  for (i=0;i<nElements;i++)
   {
     names<<selectedPixmap().elementAt(i).name();
   }
  nodeAttribute.selectedPixmap(names);
  return nodeAttribute;
}

template <class Element>
MSTreeListView<Element>::NodeInfo::~NodeInfo(void)
{}

template <class Element>
MSBoolean MSTreeListView<Element>::NodeInfo::isLastChild(void) const
{
  if (parent()==0) return MSTrue;
  else
   {
     TreeModelCursor parentCursor(parent()->modelCursor());
     parentCursor.setToLastExistingChild();
     if (parentCursor==modelCursor()) return MSTrue;
     else return MSFalse;
   }
}

template <class Element>
void MSTreeListView<Element>::NodeInfo::buildPixmapList(const MSTreeListView<Element>::PixmapRegistry &pixmapRegistry_,
                                                        MSTreeListView<Element>::PixmapList &pixmapList_,const MSStringVector& pixmapNames_)
{
  pixmapList_.removeAll();
  PixmapRegistryCursor cursor(pixmapRegistry_);
  for (unsigned i=0;i<pixmapNames_.length();i++)
   {
     pixmapRegistry_.locateElementWithKey(pixmapNames_(i),cursor);
     if (cursor.isValid()) pixmapList_.append(pixmapRegistry_.elementAt(cursor));
   }
}
#endif

template <class Element>
void MSTreeListView<Element>::processReshapeEvent(TreeEvent &treeEvent_)
{
  // If resourceTree hasn't been built yet, that means we haven't been mapped, so we can
  // safely defer the processing
  if (resourceTree().isEmpty()==False)
   {
     ScreenVectorCursor screenCursor(screenVector());
     TreeModelCursor modelCursor(treeEvent_.cursor());
     unsigned long pos=treeEvent_.position();
     ResourceTreeCursor resourceCursor(resourceTree());
     MSBoolean rebuild=MSFalse;
     MSBoolean needRedraw=MSFalse;
     //Traverse the resource tree under we find the parent node that the
     //insertion took place.
     for (resourceTree().setToFirst(resourceCursor,MSPreorder);
	  resourceCursor.isValid();
	  resourceTree().setToNext(resourceCursor,MSPreorder))
      {
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	if (info->modelCursor()==modelCursor)
	 {
	   modelTree().setToChild(pos,modelCursor);
	   NodeAttribute nodeAttr;
	   nodeAttribute(modelCursor,nodeAttr);	   
	   NodeInfo *newNode=new NodeInfo(this,nodeAttr,modelCursor,(NodeInfo*)info,info->level()+1);
	   if (treeEvent_.treeEventType()==MSObservableTreeReplace)
	    {
	      ResourceTreeCursor replaceCursor(resourceCursor);
	      replaceCursor.setToChild(pos);
	      resourceTree().removeSubtree(replaceCursor);
	    }
           resourceTree().addAsChild(resourceCursor,pos,ManagedInfo(newNode,MSInit));
	   resourceTree().setToChild(pos,resourceCursor);
	   if (modelTree().isLeaf(modelCursor)==False)
	    {
	      updateResourceTree(resourceCursor,modelCursor);
	    }
           nodeAttr=info->attribute();
	   MSBoolean nodeExpandable = isExpandable(info);
           nodeModified(info->modelCursor(),nodeAttr);
           info->attribute(this,nodeAttr);
	   if (isExpanded(info)==MSTrue) rebuild=MSTrue;
	   else if(isExpandable(info)!=nodeExpandable) needRedraw=MSTrue;
	   break;
	 }
      }
     if (frozen()==MSFalse)
      {
	if(rebuild==MSTrue)
	  {
	    buildScreenVector(); //Can be optimized
	    adjustView();
	  }
	else if(needRedraw==MSTrue) redrawImmediately();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::processPermuteEvent(TreeEvent &treeEvent_)
{
  // If resourceTree hasn't been built yet, that means we haven't been mapped, so we can
  // safely defer the processing
  if (resourceTree().isEmpty()==False)
   {
     ScreenVectorCursor screenCursor(screenVector());
     TreeModelCursor modelCursor(treeEvent_.cursor());
     const MSIndexVector& index=treeEvent_.index();
     ResourceTreeCursor resourceCursor(resourceTree());
     //Traverse the resource tree under we find the parent node that the
     //insertion took place.
     for (resourceTree().setToFirst(resourceCursor,MSPreorder);
	  resourceCursor.isValid();
	  resourceTree().setToNext(resourceCursor,MSPreorder))
      {
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	if (info->modelCursor()==modelCursor)
	 {
           resourceTree().permuteChildren(resourceCursor,index);
	   break;
	 }
      }
     if (frozen()==MSFalse)
      {
        buildScreenVector(); //Can be optimized
        adjustView();
      }
   }
}

template <class Element>
void MSTreeListView<Element>::processDeleteEvent(TreeEvent &treeEvent_)
{
  // If resourceTree hasn't been built yet, that means we haven't been mapped, so we can
  // safely defer the processing
  if (resourceTree().isEmpty()==False)
   {
     MSBoolean redrawNecessary=MSFalse;
     MSBoolean reconfigNecessary=MSFalse;
     MSBoolean checkParent=MSFalse;
     ResourceTreeCursor deleteCursor(resourceTree());
     ResourceTreeCursor resourceCursor(resourceTree());
     for (resourceTree().setToFirst(resourceCursor,MSPreorder);
	  resourceCursor.isValid();
	  resourceTree().setToNext(resourceCursor,MSPreorder))
      {
        const ManagedInfo &nodeInfo=resourceTree().elementAt(resourceCursor);
	if (nodeInfo->modelCursor()==treeEvent_.cursor())
	 {
	   deleteCursor=resourceCursor;
	   deleteCursor.setToChild(treeEvent_.position());
	   // We'll using two loops here for optimizatioin reason, the first
	   // loop get us to the node to be deleted quickly, there second one
	   // deleted all the necessary nodes
	   ScreenVectorCursor screenCursor(screenVector());
	   unsigned long pos=1;
	   for (screenCursor.setToFirst();screenCursor.isValid();screenCursor.setToNext())
	    {
	      ResourceTreeCursor &rCursor=screenVector().elementAt(screenCursor);
	      if (resourceCursor==rCursor) checkParent=MSTrue;
	      if (screenVector().elementAt(screenCursor)==deleteCursor)
	       {
		 reconfigNecessary=MSTrue;
		 redrawNecessary=MSTrue;
		 const ManagedInfo &info=resourceTree().elementAt(deleteCursor);
		 int level=info->level();
		 screenVector().removeAt(screenCursor);
		 while (pos<=screenVector().numberOfElements())
		  {	
		    screenVector().setToPosition(pos,screenCursor);
		    ResourceTreeCursor &rCursor=screenVector().elementAt(screenCursor);
		    ManagedInfo &node=resourceTree().elementAt(rCursor);
		    if (node->level()>level) screenVector().removeAt(screenCursor);
		    else break;
		  }
		 break;
	       }
	      else pos++;
	    }
           NodeAttribute nodeAttr=nodeInfo->attribute();
	   MSBoolean nodeExpandable = isExpandable(nodeInfo);
           nodeModified(nodeInfo->modelCursor(),nodeAttr);
           nodeInfo->attribute(this,nodeAttr);
	   if(isExpandable(nodeInfo)!=nodeExpandable) redrawNecessary=MSTrue;
	   break;
	 }
      }	
     if (locateSelectedNode(deleteCursor)==MSTrue)
      {
	redrawNecessary=MSTrue;
	resetSelectedNode();
      }
     if (deleteCursor.isValid())
      {
	resourceTree().removeSubtree(deleteCursor);
      }
     if (checkParent==MSTrue)
      {
	if (resourceTree().isLeaf(resourceCursor))
	 {
	   ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	   info->expandedState(MSFalse);
	   redrawNecessary=MSTrue;
	 }
      }
     if (reconfigNecessary==MSTrue) configure();
     if (redrawNecessary==MSTrue) redrawImmediately();
   }
}

template <class Element>
void MSTreeListView<Element>::processCopyEvent(TreeEvent &)
{
  updateData();
}

template <class Element>
void MSTreeListView<Element>::processAssignEvent(TreeEvent &treeEvent_)
{
  // If resourceTree hasn't been built yet, that means we haven't been mapped, so we can
  // safely defer the processing
  if (resourceTree().isEmpty()==False)
   {
     int row=0;
     ScreenVectorCursor screenCursor(screenVector());
     for (screenCursor.setToFirst();screenCursor.isValid();screenCursor.setToNext())
      {
	ResourceTreeCursor &resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	if (info->modelCursor()==treeEvent_.cursor())
	 {
	   int maxWidth=maxRowWidth();
	   int maxHeight=maxRowHeight();
           computeMaxRowSize(screenCursor,screenCursor);
	   if (maxWidth!=maxRowWidth()||maxHeight!=maxRowHeight())
	    {
	      configure();
	      redrawImmediately();
	    }
	   else updateRow(info,row);
	   break;
	 }
	else row++;
      }
   }
}

template <class Element>
void MSTreeListView<Element>::reportError(const char *error_) const
{ MSMessageLog::warningMessage(error_); }

template <class Element>
void MSTreeListView<Element>::set(MSAttrValueList& avList_)
{
  MSWidgetCommon::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="rows")
      {
	rows(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="columns")
      {
	columns(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="firstRow")
      {
	firstRow(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="buttonShadowThickness")
      {
	buttonShadowThickness(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="buttonShadowStyle")
      {
        buttonShadowStyle(MSAttrValue::stringToShadowStyle(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="lineForeground")
      {
	lineForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="lineStyle")
      {
	lineStyle(MSAttrValue::stringToLineStyle(avList_[i].value()));
	index<<i;
      }
     else if (avList_[i].attribute()=="selectedRowForeground")
      {
	selectedRowForeground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="selectedRowBackground")
      {
	selectedRowBackground(avList_[i].value());
	index<<i;
      }
     else if (avList_[i].attribute()=="levelOffset")
      {
	levelOffset(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="tabSpacing")
      {
	tabSpacing(avList_[i].value().asInt());
	index<<i;
      }
     else if (avList_[i].attribute()=="showButtons")
      {
	showButtons(avList_[i].value().asBoolean());
	index<<i;
      }
     else if (avList_[i].attribute()=="showLines")
      {
	showLines(avList_[i].value().asBoolean());
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
MSAttrValueList& MSTreeListView<Element>::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("rows",MSString(rows()));
  avList_<<MSAttrValue("columns",MSString(columns()));
  avList_<<MSAttrValue("firstRow",MSString(firstRow()));
  avList_<<MSAttrValue("buttonShadowThickness",MSString(buttonShadowThickness()));
  avList_<<MSAttrValue("buttonShadowStyle",MSAttrValue::shadowStyleToString(buttonShadowStyle()),
		       MSStringVector("MSEtchedIn\nMSEtchedOut\nMSFlat\nMSRaised\nMSSunken"));
  avList_<<MSAttrValue("lineForeground",_server->colorName(lineForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("lineStyle",MSAttrValue::lineStyleToString(lineStyle()),MSStringVector("MSSolid\nMSDot\nMSDash"));
  avList_<<MSAttrValue("selectedRowForeground",_server->colorName(selectedRowForeground()),MSAttrValue::Color);
  avList_<<MSAttrValue("selectedRowBackground",_server->colorName(selectedRowBackground()),MSAttrValue::Color);
  avList_<<MSAttrValue("levelOffset",MSString(levelOffset()));
  avList_<<MSAttrValue("tabSpacing",MSString(tabSpacing()));
  MSStringVector aBoolVector("MSFalse\nMSTrue");
  avList_<<MSAttrValue("showButtons",showButtons()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("showLines",showLines()==MSTrue?"MSTrue":"MSFalse",aBoolVector);
  avList_<<MSAttrValue("showRootNode",showRootNode()==MSTrue?"MSTrue":"MSFalse",aBoolVector);

  avList_<<MSAttrValue("subtreecollapsed","",MSAttrValue::Callback);
  avList_<<MSAttrValue("subtreeexpanded","",MSAttrValue::Callback);
  avList_<<MSAttrValue("doubleclick","",MSAttrValue::Callback);
  avList_<<MSAttrValue("selection","",MSAttrValue::Callback);
  avList_<<MSAttrValue("button2selection","",MSAttrValue::Callback);
  avList_<<MSAttrValue("button3selection","",MSAttrValue::Callback);

  return MSWidgetCommon::get(avList_);
}

template<class Element>
MSBoolean MSTreeListView<Element>::sensitive(void) const
{
  return MSWidgetCommon::sensitive();
}

template<class Element>
void MSTreeListView<Element>::sensitive(MSBoolean sensitive_)
{
  MSWidgetCommon::sensitive(sensitive_);
}

template <class Element>
class MSTreeListViewDefaultIterator : public MSTreeListView<Element>::Iterator
{
protected:
  MSTreeListView<Element>::TreeModel &_treeModel;
public:
  MSTreeListViewDefaultIterator(MSTreeListView<Element>::TreeModel &treeModel_)
  :_treeModel(treeModel_)
  {}
  virtual void nodeAttribute(const MSTreeListView<Element>::TreeModelCursor&,
                             MSTreeListView<Element>::NodeAttribute &);
};

template<class Element>
void MSTreeListViewDefaultIterator<Element>::nodeAttribute(const MSTreeListView<Element>::TreeModelCursor &cursor_,
                                                       MSTreeListView<Element>::NodeAttribute &nodeAttr_)
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
void MSTreeListView<Element>::setDefaultNodeAttributes(void)
{
  if (MSView::model()!=0)
   {
     MSTreeListViewDefaultIterator<Element> iterator(modelTree());
     setNodeAttributes(iterator);
   }
}


template<class Element>
void MSTreeListView<Element>::makeVisible(const TreeModelCursor &modelCursor_)
{
  if (resourceTree().isEmpty()==True) buildResourceTreeAndScreenVector();
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     MSBoolean redrawNeeded=MSFalse;
     while (resourceTree().setToParent(resourceCursor))
      {
        ManagedInfo &info=resourceTree().elementAt(resourceCursor);
        if (info->expandedState()==MSFalse)
         {
           info->expandedState(MSTrue);
           redrawNeeded=MSTrue;
         }
      }
     if (redrawNeeded==MSTrue)
      {
        buildScreenVector();
        adjustView();
      }
   }
}

template<class Element>
MSBoolean MSTreeListView<Element>::isVisible(const TreeModelCursor &modelCursor_) const
{
  // load the element tree if it hasn't been loaded.  But since rebuildScreen
  // is not a const method, we need to cast away the const-ness.  This is a work around
  // in order to preserve the const-ness of this method.
  if (resourceTree().isEmpty()==True)
   {
     MSTreeListView<Element> *myself=(MSTreeListView<Element> *)this;
     myself->buildResourceTreeAndScreenVector();
   }
  ResourceTreeCursor resourceCursor=findResourceCursor(modelCursor_);
  if (resourceCursor.isValid())
   {
     MSBoolean visible=MSTrue;
     while (resourceTree().setToParent(resourceCursor))
      {
        const ManagedInfo &info=resourceTree().elementAt(resourceCursor);
        if (info->expandedState()==MSFalse ||
            info->expandable()==MSFalse)
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
void MSTreeListView<Element>::editorActivate(void)
{
  TreeModelCursor selectedCursor (selectedNode());
  if (selectedCursor.isValid()==True)
   {
     MSString currentString;
     formatOutput(currentString,selectedCursor);
     if (editor()->text()!=currentString)     
      {
        if (validate(selectedCursor,editor()->string())==MSTrue)
         {
           editor()->unmap();
           modelTree().elementChanged(selectedCursor);
         }
        else server()->bell();
      }
     else editor()->unmap();
   }
}

template<class Element>
void MSTreeListView<Element>::editorEscape(void)
{
  loseFocusNotify(editor());
  editor()->unmap();
}

template<class Element>
void MSTreeListView<Element>::mapEditor(void)
{
  TreeModelCursor selectedCursor (selectedNode());
  if (selectedCursor.isValid()==True)
   {
     int row=0;
     ScreenVectorCursor screenCursor(screenVector());
     for (screenCursor.setToFirst();screenCursor.isValid();screenCursor.setToNext())
      {
	ResourceTreeCursor &resourceCursor=screenVector().elementAt(screenCursor);
	ManagedInfo &info=resourceTree().elementAt(resourceCursor);
	if (info->modelCursor()==selectedCursor)
	 {
           int offset=highlightThickness()+shadowThickness();
           int x=offset-hsb()->value();
           int y=offset+maxRowHeight()*(row-firstRow());
///
           int level=info->level();
           int totalOffset=level*levelOffset();

           x +=totalOffset+ 2*rowHorizontalSpacing()+buttonWidth();
           const PixmapList& pixList=selectedPixmap(info);
           unsigned long nElements=pixList.length();
           for (unsigned long i=0;i<nElements;i++)
            {
              const MSPixmap &pixmap=pixList.elementAt(i);
              x+=pixmap.width()+rowHorizontalSpacing();
            }
           MSString buffer;
           formatOutput(buffer,info->modelCursor());

           int hOffset=2*(selectedHighlightThickness()+highlightGap())+2;
           int h=fontObject().textHeight()+hOffset;

           int w =fontObject().textWidth(buffer.string())+hOffset;
           w=(w>MSTreeListViewMinimumEditorWidth)?w:MSTreeListViewMinimumEditorWidth;
             
           editor()->moveTo(x,y);
           editor()->resize(w,h);
           editor()->foreground(background());
           editor()->background(foreground());
           editor()->map();
           editor()->raise();
           takeFocusNotify(editor());
         }
        else row++;
      }
   }
}

template<class Element>
MSBoolean MSTreeListView<Element>::loseFocus(void)
{
  if (editor()->mapped()==MSTrue) editorActivate();
  if (editor()->mapped()==MSTrue) return MSFalse;
  else 
   {
     focusOut();
     return MSTrue; 
   }
}

template<class Element>
MSBoolean MSTreeListView<Element>::editing(void) const
{
  return _pEditor->mapped();
}

template<class Element>
void MSTreeListView<Element>::edit(void)
{
 if (editor()->mapped()==MSFalse) 
  {
    TreeModelCursor selectedCursor(selectedNode());
    ResourceTreeCursor resourceCursor=findResourceCursor(selectedCursor);
    if (resourceCursor.isValid())
     {
       ManagedInfo &info=resourceTree().elementAt(resourceCursor);
       if(isNodeProtected(info) == MSFalse)
        {
          MSString string;
          formatOutput(string,selectedCursor);
          editor()->string(string);
//       editor()->selectAll();
          mapEditor();
        }
     }
  }
}

template<class Element>
unsigned long MSTreeListView<Element>::addEditorKeyCallback( const char* pString_,MSKeyCallback* keyCallback_)
{ return editor()->addKeyCallback(pString_,keyCallback_);}

template<class Element> 
 void MSTreeListView<Element>::removeEditorKeyCallback(unsigned long id_)
{ editor()->removeKeyCallback(id_); }

template<class Element>
void MSTreeListView<Element>::removeEditorKeyCallback(const char* pString_)
{ editor()->removeKeyCallback(pString_); }

#endif
