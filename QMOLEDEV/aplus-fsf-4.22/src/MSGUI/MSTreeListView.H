#ifndef MSTreeListViewHEADER
#define MSTreeListViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>
#include <MSTypes/MSStringVector.H>
#include <MSGUI/MSWidgetCommon.H>
#include <MSGUI/MSPixmap.H>
#include <MSGUI/MSFontObject.H>
#include <MSGUI/MSDoubleClick.H>
#include <MSGUI/MSVScrollBar.H>
#include <MSGUI/MSHScrollBar.H>
#include <MSGUI/MSBackingStorePixmap.H>
#include <MSTypes/MSObservableTree.H>
#include <MSTypes/MSManagedPointer.H>
#include <MSTypes/MSGenericVector.H>
#include <MSTypes/MSIHashKeySet.H>
#include <MSTypes/MSISequence.H>
#include <MSGUI/MSTextField.H>

template<class Element> class VScrollBar;
template<class Element> class HScrollBar;

template <class Element>
class MSTreeListView : public MSWidgetCommon, public MSDoubleClick
{
friend class VScrollBar<Element>;
friend class HScrollBar<Element>;
public:
  typedef MSObservableTree<Element> TreeModel;
  typedef MSObservableTree<Element>::Cursor TreeModelCursor;

  class NodeAttribute
  {
  protected:
    MSBoolean _sensitive;
    MSBoolean _expandable;
    MSBoolean _expandedState;
    MSStringVector _pixmap;
    MSStringVector _insensitivePixmap;
    MSStringVector _selectedPixmap;
  public:
#if defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
    NodeAttribute(void) 
    : _sensitive(MSTrue),_expandable(MSFalse),_expandedState(MSFalse)
    {}
    ~NodeAttribute(void){}
#else
    NodeAttribute(void);
    ~NodeAttribute(void);
#endif
    MSBoolean sensitive(void) const { return _sensitive; }
    void sensitive(MSBoolean sensitive_) { _sensitive=sensitive_; }

    MSBoolean expandable(void) const { return _expandable; }
    void expandable(MSBoolean expandable_) { _expandable=expandable_; }

    MSBoolean expandedState(void) const { return _expandedState; }
    void expandedState(MSBoolean expandedState_) { _expandedState=expandedState_; }

    const MSStringVector &pixmap(void) const { return _pixmap; }
    void pixmap(const MSStringVector &pixmap_) { _pixmap=pixmap_; }

    const MSStringVector &insensitivePixmap(void) const { return _insensitivePixmap; }
    void insensitivePixmap(const MSStringVector &insensitivePixmap_) { _insensitivePixmap=insensitivePixmap_; }

    const MSStringVector &selectedPixmap(void) const { return _selectedPixmap; }
    void selectedPixmap(const MSStringVector &selectedPixmap_) { _selectedPixmap=selectedPixmap_; }
  };

  class Iterator
  {
  public:
    virtual void nodeAttribute(const TreeModelCursor&,NodeAttribute &)=0;
  };

  class ConstIterator
  {
  public:
    virtual void nodeAttribute(const TreeModelCursor&,const NodeAttribute &)=0;
  };
  
  typedef MSGenericVector<MSPixmap>                PixmapList;
  typedef MSIHashKeySet<MSPixmap,MSString>         PixmapRegistry;
  typedef MSIHashKeySet<MSPixmap,MSString>::Cursor PixmapRegistryCursor;
  
  class NodeInfo;
  friend class NodeInfo;

  class NodeInfo 
  {
  protected:
    int        _level;
    MSBoolean  _sensitive;
    MSBoolean  _expandedState;
    MSBoolean  _expandable;
    PixmapList _pixmap;
    PixmapList _insensitivePixmap;
    PixmapList _selectedPixmap;
    TreeModelCursor _modelCursor;
    NodeInfo *_parent;
    
  public:
#if defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
    NodeInfo(const MSTreeListView<Element> *treeView_,
             const NodeAttribute & nodeAttribute_,
             TreeModelCursor & modelCursor_,NodeInfo * parent_,int level_)
    : _modelCursor(modelCursor_),_parent(parent_),_level(level_)
    {
      attribute(treeView_,nodeAttribute_);
    }
    ~NodeInfo(void){}

    void attribute(const MSTreeListView<Element> *treeView_,const NodeAttribute& nodeAttribute_)
    {
      sensitive(nodeAttribute_.sensitive());
      expandable(nodeAttribute_.expandable());
      expandedState(nodeAttribute_.expandedState());
      buildPixmapList(treeView_->pixmapRegistry(),pixmap(),nodeAttribute_.pixmap());
      buildPixmapList(treeView_->pixmapRegistry(),insensitivePixmap(),nodeAttribute_.insensitivePixmap());
      buildPixmapList(treeView_->pixmapRegistry(),selectedPixmap(),nodeAttribute_.selectedPixmap());
    }

    NodeAttribute attribute(void) const
    {
      MSTreeListView<Element>::NodeAttribute nodeAttribute;
      nodeAttribute.sensitive(sensitive());
      nodeAttribute.expandable(expandable());
      nodeAttribute.expandedState(expandedState());
      MSStringVector names;
      unsigned int i,nElements;
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
#else 
    NodeInfo(const MSTreeListView<Element> *,const NodeAttribute &,TreeModelCursor &,NodeInfo *,int);
    ~NodeInfo(void);

    void attribute(const MSTreeListView<Element> *,const NodeAttribute&);
    NodeAttribute attribute(void) const;
#endif
    int level(void) const { return _level; }
    void level(int level_) { _level=level_; }

    MSBoolean sensitive(void) const { return _sensitive; }
    void sensitive(MSBoolean sensitive_) { _sensitive=sensitive_; }

    MSBoolean expandedState(void) const { return _expandedState; }
    void expandedState(MSBoolean expandedState_) { _expandedState=expandedState_; }

    MSBoolean expandable(void) const { return _expandable; }
    void expandable(MSBoolean expandable_) { _expandable=expandable_; }

    PixmapList &pixmap(void) { return _pixmap; }
    const PixmapList &pixmap(void) const { return _pixmap; }
    PixmapList &insensitivePixmap(void) { return _insensitivePixmap; }
    const PixmapList &insensitivePixmap(void) const { return _insensitivePixmap; }
    PixmapList &selectedPixmap(void) { return _selectedPixmap; }
    const PixmapList &selectedPixmap(void) const { return _selectedPixmap; }

    const TreeModelCursor &modelCursor(void) const { return _modelCursor; }
    TreeModelCursor &modelCursor(void) { return _modelCursor; }

    void parent(NodeInfo *parent_) {_parent=parent_;}
    NodeInfo *parent(void) const {return _parent;}
#if !defined(MS_INLINE_TEMPLATE_NESTED_CLASS) 
    MSBoolean isLastChild(void) const;
    void buildPixmapList(const PixmapRegistry&,PixmapList&,const MSStringVector&);
#else
    MSBoolean isLastChild(void) const
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
    void buildPixmapList(const PixmapRegistry& pixmapRegistry_,PixmapList& pixmapList_,
                         const MSStringVector& pixmapNames_)
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
  };

  typedef MSObservableTreeEvent<Element> TreeEvent;
  typedef MSManagedPointer<NodeInfo> ManagedInfo;
  typedef MSTabularTree<ManagedInfo> ResourceTree;
  typedef MSTabularTreeCursor<ManagedInfo> ResourceTreeCursor;
  typedef MSISequence<ResourceTreeCursor> ScreenVector;
  typedef MSISequence<ResourceTreeCursor>::Cursor ScreenVectorCursor;

  
protected:
  class Editor : public MSTextField
  {
  public:
#if defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
    Editor(MSWidget * owner_) : MSTextField(owner_)
      {
	_highlightThickness=0;
	selectInput(ExposureMask|ButtonPressMask);
      } 
    ~Editor() {}
    
    virtual void activate(void)
      { ((MSTreeListView<Element> *)(owner()))->editorActivate(); }

    virtual void escape(void)
      { ((MSTreeListView<Element> *)(owner()))->editorEscape(); }
#else
    Editor(MSWidget *);
    ~Editor();
    
    virtual void activate(void);  
    virtual void escape(void);
#endif
  };
friend class Editor;
  
  MSVScrollBar                    *_vsb;
  MSHScrollBar                    *_hsb;
  MSWidgetCommon                  *_clipWindow;
  MSBackingStorePixmap            *_redrawPixmap;
  MSPixmap                        *_collapsedButtonPixmap;
  MSPixmap                        *_expandedButtonPixmap;
  MSPixmap                        *_stipplePixmap;
  MSPixmap                        *_dotPixmap;
  MSPixmap                        *_dashPixmap;
  MSGC                             _gc;
  MSGC                             _linegc;
  MSGC                             _pixmapgc;
  MSFontObject                     _fontObject;
  MSIntVector                      _maxWidths;
  MSIntVector                      _tabStops;
  int                              _tabSpacing;
  int                              _maxRowHeight;
  int                              _maxRowWidth;
  int                              _rows;
  int                              _columns;
  int                              _rowHorizontalSpacing;
  int                              _rowVerticalSpacing;
  int                              _buttonShadowThickness;
  MSShadowStyle                    _buttonShadowStyle;
  int                              _firstRow;
  int                              _levelOffset;
  MSBoolean                        _showButtons;
  MSBoolean                        _showLines;
  MSBoolean                        _showRootNode;
  MSBoolean                        _useBackingStore;
  MSBoolean                        _hasFocus;
  unsigned long                    _lineForeground;
  MSLineStyle                      _lineStyle;
  unsigned long                    _selectedRowForeground;
  unsigned long                    _selectedRowBackground;
  ResourceTree                     _resourceTree;
  ScreenVector                     _screenVector;
  PixmapRegistry                   _pixmapRegistry;
  TreeModelCursor                 *_selectedNode;
  TreeModelCursor                 *_activatedNode;
  MSTextField                     *_pEditor;

  MSHScrollBar *hsb(void) {return _hsb;}
  MSVScrollBar *vsb(void) {return _vsb;}
  MSWidgetCommon *clipWindow(void) {return _clipWindow;}
  MSBackingStorePixmap *redrawPixmap(void) {return _redrawPixmap;}
  MSPixmap *stipplePixmap(void) {return _stipplePixmap;}
  MSPixmap *dotPixmap(void) {return _dotPixmap;}
  MSPixmap *dashPixmap(void) {return _dashPixmap;}

  MSIntVector &maxWidths(void) {return _maxWidths;}
  MSIntVector &tabStops(void) {return _tabStops;}
  int tabPixelSpacing(void) {return _tabSpacing*fontObject().charWidth();}
    
  void maxRowHeight(int maxRowHeight_) {_maxRowHeight=maxRowHeight_;}
  int maxRowHeight(void) const {return _maxRowHeight;}

  void maxRowWidth(int maxRowWidth_) {_maxRowWidth=maxRowWidth_;}
  int maxRowWidth(void) const {return _maxRowWidth;}

  void rowHorizontalSpacing(int rowHorizontalSpacing_) {_rowHorizontalSpacing=rowHorizontalSpacing_;}
  int rowHorizontalSpacing(void) const {return _rowHorizontalSpacing;}

  void rowVerticalSpacing(int rowVerticalSpacing_) {_rowVerticalSpacing=rowVerticalSpacing_;}
  int rowVerticalSpacing(void) const {return _rowVerticalSpacing;}

  int selectedHighlightThickness(void) const {return 1;}
  int highlightGap(void) const {return 1;}

  MSBoolean useBackingStore(void) {return _useBackingStore;}
  void useBackingStore(MSBoolean);

  MSBoolean hasFocus(void) {return _hasFocus;}
  void hasFocus(MSBoolean hasFocus_) {_hasFocus=hasFocus_;}

  int buttonWidth(void) const;
  int buttonHeight(void) const;
  
  MSGC &gc(void) {return _gc;}
  MSGC &linegc(void) {return _linegc;}
  MSGC &pixmapgc(void) {return _pixmapgc;}

  MSFontObject &fontObject(void) {return _fontObject; }

  TreeModel &modelTree(void) {return *(TreeModel *)_model;}
  const TreeModel &modelTree(void) const {return *(TreeModel *)_model;}

  ResourceTree &resourceTree(void) {return _resourceTree;}
  const ResourceTree &resourceTree(void) const {return _resourceTree;}

  ScreenVector &screenVector(void) {return _screenVector;}
  const ScreenVector &screenVector(void) const {return _screenVector;}

  PixmapRegistry &pixmapRegistry(void) {return _pixmapRegistry;}
  const PixmapRegistry &pixmapRegistry(void) const {return _pixmapRegistry;}

  virtual void configure(void);
  virtual void computeSize(void);
  virtual void firstMapNotify(void);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual MSBoolean loseFocus(void);

  virtual void receiveEvent(MSEvent &);
  virtual void processPermuteEvent(TreeEvent &);
  virtual void processReshapeEvent(TreeEvent &);
  virtual void processDeleteEvent(TreeEvent &);
  virtual void processCopyEvent(TreeEvent &);
  virtual void processAssignEvent(TreeEvent &);
  
  virtual void updateData(void);

  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char *);
  virtual void buttonPress(const XEvent *);
  virtual void button1Press(const XEvent *);
  virtual void button2Press(const XEvent *);
  virtual void button3Press(const XEvent *);
  
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);
  virtual void home(void);
  virtual void end(void);
  virtual void pageUp(void);
  virtual void pageDown(void);
  virtual void returnKey(void);
  
  virtual ScreenVectorCursor nextUpScreenCursor(unsigned long &);
  virtual ScreenVectorCursor nextDownScreenCursor(unsigned long &);

  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateFont(Font);

  void init(void);
  void setGeometryDefaults(void);
  int rowFromY(int);
  int branchLength(void);
  void drawHighlight(Window);
  void clearDrawingArea(Window);
  void drawingAreaShadow(Window);
  void clearRow(Window,int);
  void buildCollapsedButtonPixmap(void);
  void buildExpandedButtonPixmap(void);

  void computeMaxRowSize(void);
  void computeMaxRowSize(ScreenVectorCursor &,ScreenVectorCursor &);
  void calculateRowSize(const ManagedInfo &,MSIntVector&,int&);
  void calculatePixmapSize(const PixmapList&,int&,int&);

  void rebuildScreen(MSBoolean);

  void buildResourceTreeAndScreenVector(Iterator *iterator_=0);
  void buildScreenVector(void);

  // The following are all recursive methods and build either the resource tree,
  // the screen vector or both
  void updateResourceTreeAndScreenVector(ResourceTreeCursor&,ScreenVectorCursor&,MSBoolean,
					 Iterator *iterator_=0);
  void updateResourceTree(ResourceTreeCursor,TreeModelCursor);
  void updateScreenVector(ScreenVectorCursor &,ResourceTreeCursor);

  // The following method set/get all node attributes without rebuilding the resource tree
  void setAttributes(Iterator &);
  void getAttributes(ConstIterator &) const;
  
  ScreenVectorCursor findScreenCursor(const ResourceTreeCursor&,unsigned long &) const;
  ResourceTreeCursor findResourceCursor(const TreeModelCursor&) const;
  MSBoolean buttonSelected(ManagedInfo &,int,int);
  MSBoolean labelSelected(ManagedInfo &,int,int);
  MSBoolean isSelected(TreeModelCursor &);
  MSBoolean isRowOnScreen(int);
  MSBoolean locateSelectedNode(ResourceTreeCursor);

  virtual void reportError(const char *) const;
  virtual void vsbChanged(void);
  virtual void hsbChanged(void);
  virtual void updateVsb(void);
  virtual void updateHsb(void);

  virtual void adjustRowColumn(void);
  virtual void adjustFirstRow(void);
  virtual void adjustFirstColumn(void);
  virtual void adjustLevels(void);
  virtual void adjustSize(void);
  virtual void adjustView(void);
  virtual void resetFirstRowColumn(void);
  virtual void resetSelectedNode(void);
  
  virtual void redrawImmediately(void);
  virtual void updateRow(TreeModelCursor&);
  virtual void updateRow(ManagedInfo&,int);
  virtual void drawRows(Window,int,int);
  virtual void drawRow(Window,const ManagedInfo&,int);
  virtual void drawRow(Window,const ManagedInfo&,int,int);
  virtual void drawPixmaps(Window,const PixmapList&,int&,int&);
  virtual void drawString(Window,const ManagedInfo&,int,int);
  virtual void drawTabbedString(Window,const ManagedInfo&,int,int);
  virtual void scrollUp(int);
  virtual void scrollDown(int);
  
  virtual void expandSubTree(ScreenVectorCursor &,unsigned long);
  virtual void collapseSubTree(ScreenVectorCursor &,unsigned long);

  virtual void subTreeCollapsed(ManagedInfo &);
  virtual void subTreeExpanded(ManagedInfo &);

  //The followings can be overriden by subclasses to respond to different types of event
  //instead of receiving a callback.  The default implementation of these methods just
  //trigger the appropriate callback.
  virtual void subTreeCollapsedNotify(void);
  virtual void subTreeExpandedNotify(void);
  virtual void doubleClickNotify(void);
  virtual void nodeSelectionNotify(void);
  virtual void button2SelectionNotify(void);
  virtual void button3SelectionNotify(void);

  //The followings can be overriden by subclasses to control the attribute of each node
  //The following 
  virtual void nodeModified(const TreeModelCursor &,NodeAttribute &);
  virtual void nodeAttribute(const TreeModelCursor &,NodeAttribute &);
  virtual MSBoolean isSensitive(const NodeInfo *);
  virtual MSBoolean isExpandable(const NodeInfo *);
  virtual MSBoolean isExpanded(const NodeInfo *);
  virtual MSBoolean isNodeProtected(const NodeInfo *);
  virtual const PixmapList &pixmap(const NodeInfo *);
  virtual const PixmapList &selectedPixmap(const NodeInfo *);
  virtual const PixmapList &insensitivePixmap(const NodeInfo *);
  virtual unsigned long nodeForeground(const NodeInfo *);
  virtual const char *formatOutput(MSString &, TreeModelCursor &);
  virtual MSBoolean validate(TreeModelCursor &,const char *);

  virtual void editorEscape(void);
  virtual void editorActivate(void);
  virtual void mapEditor(void);

  MSTextField *editor(void)                 {return _pEditor;}
  
public:
  MSTreeListView(MSWidget *owner_,TreeModel &tree_);
  MSTreeListView(MSWidget *owner_);
  ~MSTreeListView(void);

  void model(TreeModel &);
  void model(const TreeModel &);

  TreeModel& viewModel(void)             { return modelTree(); }
  const TreeModel& viewModel(void) const { return modelTree(); }

  void rows(int);
  int rows(void) const {return _rows;}

  void columns(int);
  int columns(void) const {return _columns;}

  void firstRow(int);
  int firstRow(void) const {return _firstRow;}

  void firstNode(const TreeModelCursor &);
  TreeModelCursor firstNode(void) const;
  
  void selectedNode(const TreeModelCursor &);
  TreeModelCursor selectedNode(void) const;

  TreeModelCursor activatedNode(void) const;

  void collapsedButtonPixmap(const MSPixmap &);
  const MSPixmap &collapsedButtonPixmap(void) const {return *_collapsedButtonPixmap;}

  void expandedButtonPixmap(const MSPixmap &);
  const MSPixmap &expandedButtonPixmap(void) const {return *_expandedButtonPixmap;}

  void buttonShadowThickness(int buttonShadowThickness_);
  int buttonShadowThickness(void) const {return _buttonShadowThickness;}

  void buttonShadowStyle(MSShadowStyle buttonShadowStyle_);
  MSShadowStyle buttonShadowStyle(void) const {return _buttonShadowStyle;}

  void sensitive(const TreeModelCursor &,MSBoolean);
  MSBoolean sensitive(const TreeModelCursor &) const;

  void expandedState(const TreeModelCursor &,MSBoolean);
  MSBoolean expandedState(const TreeModelCursor &) const;

  void expandable(const TreeModelCursor &,MSBoolean);
  MSBoolean expandable(const TreeModelCursor &) const;

  void pixmap(const TreeModelCursor &,const MSStringVector &);
  MSStringVector pixmap(const TreeModelCursor &) const;

  void insensitivePixmap(const TreeModelCursor &,const MSStringVector &);
  MSStringVector insensitivePixmap(const TreeModelCursor &) const;

  void selectedPixmap(const TreeModelCursor &,const MSStringVector &);
  MSStringVector selectedPixmap(const TreeModelCursor &) const;

  void lineForeground(const char *);
  void lineForeground(unsigned long);
  unsigned long lineForeground(void) const {return _lineForeground;}

  void lineStyle(MSLineStyle);
  MSLineStyle lineStyle(void) const {return _lineStyle;}

  void selectedRowForeground(const char *);
  void selectedRowForeground(unsigned long);
  unsigned long selectedRowForeground(void) const {return _selectedRowForeground;}

  void selectedRowBackground(const char *);
  void selectedRowBackground(unsigned long);
  unsigned long selectedRowBackground(void) const {return _selectedRowBackground;}

  void levelOffset(int);
  int levelOffset(void) const {return _levelOffset;}

  void tabSpacing(int);
  int tabSpacing(void) const {return _tabSpacing;}

  void showButtons(MSBoolean);
  MSBoolean showButtons(void) const {return _showButtons;}

  void showLines(MSBoolean);
  MSBoolean showLines(void) const {return _showLines;}

  void showRootNode(MSBoolean);
  MSBoolean showRootNode(void) const {return _showRootNode;} 

  void registerPixmap(const MSPixmap &);
  const MSPixmap *pixmap(const MSString &) const;

  void setNodeAttributes(Iterator &);
  void getNodeAttributes(ConstIterator &) const;

  void setDefaultNodeAttributes(void);
  
  int numRows(void) const;

  virtual void redraw(void);
  virtual void naturalSize(void);
  virtual void unfreeze(void);

  //Make MSWidgets' sensitive visible, because
  //we also have node specific sensitive functions.
  void sensitive(MSBoolean);
  MSBoolean sensitive(void) const;

  void makeVisible(const TreeModelCursor &);
  MSBoolean isVisible(const TreeModelCursor &) const;

  void edit(void);
  MSBoolean editing(void) const;

  unsigned long addEditorKeyCallback( const char*,MSKeyCallback*);
  void removeEditorKeyCallback(unsigned long);
  void removeEditorKeyCallback(const char*);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);
};

#ifndef MSTreeViewHEADER
DECLARE_ELEMENT_FOR_OPS_TYPE(MSPixmap)
inline MSString const &key(const MSPixmap &pixmap_)
{
  return pixmap_.name();
}
#endif

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSGUILIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSIHashKeySet<MSPixmap,MSString>
#pragma do_not_instantiate MSIHashKeySetNode<MSPixmap>
#pragma do_not_instantiate MSIHashKeySetCursor<MSPixmap,MSString>

#pragma do_not_instantiate MSGenericVector<MSPixmap>
#pragma do_not_instantiate MSGenericVectorOps<MSPixmap>
#pragma do_not_instantiate MSGenericData<MSPixmap>
#endif // !__MSGUILIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
