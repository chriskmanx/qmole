#ifndef MSTreeViewHEADER
#define MSTreeViewHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTextField.H>
#include <MSGUI/MSPopupMenu.H>
#include <MSGUI/MSDoubleClick.H>
#include <MSGUI/MSFontObject.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSTypes/MSObservableTree.H>
#include <MSGUI/MSScrollableWidget.H>
#include <MSTypes/MSGenericVector.H>
#include <MSTypes/MSIHashKeySet.H>

class MSPixmap;
template<class Element>
class MSTreeView : public MSScrollableWidget, public MSDoubleClick
{
public:
  typedef MSObservableTree<Element> TreeModel;
  typedef typename MSObservableTree<Element>::Cursor TreeModelCursor;
#ifdef MS_CLASS_ENUM_OVERLOADING_BUG
  enum Orientation2 {Vertical=0,Horizontal=1};
  typedef int Orientation;
#else
  enum Orientation {Vertical=0,Horizontal=1};
#endif

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
        : _sensitive(MSTrue),_expandable(MSFalse),_expandedState(MSFalse) {}

    NodeAttribute(const NodeAttribute &nodeAttr_)
        : _sensitive(nodeAttr_.sensitive()), 
_expandable(nodeAttr_.expandable()),
          _expandedState(nodeAttr_.expandedState()),_pixmap(nodeAttr_.pixmap()),
          _insensitivePixmap(nodeAttr_.insensitivePixmap()), _selectedPixmap(nodeAttr_.selectedPixmap())
    {}

    ~NodeAttribute(void) {}
#else
    NodeAttribute(void);
    NodeAttribute(const NodeAttribute &);
    ~NodeAttribute(void);
#endif
    MSBoolean sensitive(void) const      { return _sensitive;     }
    void sensitive(MSBoolean sensitive_) { _sensitive=sensitive_; }

    MSBoolean expandable(void) const       { return _expandable;      }
    void expandable(MSBoolean expandable_) { _expandable=expandable_; }

    MSBoolean expandedState(void) const          { return _expandedState;         }
    void expandedState(MSBoolean expandedState_) { _expandedState=expandedState_; }

    const MSStringVector &pixmap(void) const    { return _pixmap;  }
    void pixmap(const MSStringVector & pixmap_) { _pixmap=pixmap_; }

    const MSStringVector &insensitivePixmap(void) const    { return _insensitivePixmap;  }
    void insensitivePixmap(const MSStringVector & pixmap_) { _insensitivePixmap=pixmap_; }

    const MSStringVector &selectedPixmap(void) const           { return _selectedPixmap;          }
    void selectedPixmap(const MSStringVector& selectedPixmap_) { _selectedPixmap=selectedPixmap_; }


#if defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
    NodeAttribute & operator=(const NodeAttribute &nodeAttr_)
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

    MSBoolean operator==(const NodeAttribute &nodeAttr_)
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
#else
    NodeAttribute &operator=(const NodeAttribute &nodeAttr_);
    MSBoolean operator==(const NodeAttribute &nodeAttr_);
#endif

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

  MSTreeView(MSWidget *owner_,TreeModel &);
  MSTreeView(MSWidget *owner_);
  ~MSTreeView(void);

  void model(TreeModel &);
  void model(const TreeModel &);

  TreeModel& viewModel(void)             { return modelTree(); }
  const TreeModel& viewModel(void) const { return modelTree(); }

  void selectedNode(const TreeModelCursor &);
  TreeModelCursor selectedNode(void) const;
  
  void startNode(const TreeModelCursor &);
  TreeModelCursor startNode(void) const;

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
  
  void makeVisible(const TreeModelCursor &);
  MSBoolean isVisible(const TreeModelCursor &) const;

  MSPopupMenu *popupMenu(void)             {return _pPopupMenu;}  
  const MSPopupMenu *popupMenu(void) const {return _pPopupMenu;}  
  void popupMenu(MSPopupMenu *);

  void collapsedButtonPixmap(const MSPixmap &);
  const MSPixmap &collapsedButtonPixmap(void) const {return *_collapsedButtonPixmap;}

  void expandedButtonPixmap(const MSPixmap &);
  const MSPixmap &expandedButtonPixmap(void) const {return *_expandedButtonPixmap;}

  void showButtons(MSBoolean);
  MSBoolean showButtons(void) const {return _showButtons;}

  void showLabels(MSBoolean);
  MSBoolean showLabels(void) const {return _showLabels;}

  void showPixmaps(MSBoolean);
  MSBoolean showPixmaps(void) const {return _showPixmaps;}

  void buttonShadowThickness(int buttonShadowThickness_);
  int buttonShadowThickness(void) const {return _buttonShadowThickness;}

  void lineForeground(const char *);
  void lineForeground(unsigned long);
  unsigned long lineForeground(void) const {return _lineForeground;}

  void nodeForeground(const char *);
  void nodeForeground(unsigned long);
  unsigned long nodeForeground(void) const {return _nodeForeground;}

  void nodeBackground(const char *);
  void nodeBackground(unsigned long);
  unsigned long nodeBackground(void) const {return _nodeBackground;}

  void selectedNodeForeground(const char *);
  void selectedNodeForeground(unsigned long);
  unsigned long selectedNodeForeground(void) const {return _selectedNodeForeground;}

  void selectedNodeBackground(const char *);
  void selectedNodeBackground(unsigned long);
  unsigned long selectedNodeBackground(void) const {return _selectedNodeBackground;}  

  void orientation(Orientation);
  Orientation orientation(void) const {return _orientation;}

  void verticalSpacing(int);
  int verticalSpacing(void) const {return _verticalSpacing;}

  void horizontalSpacing(int);
  int horizontalSpacing(void) const {return _horizontalSpacing;}

  void showRootNode(MSBoolean);
  MSBoolean showRootNode(void) const {return _showRootNode;}

  void setNodeAttributes(Iterator &);
  void getNodeAttributes(ConstIterator &) const;

  void setDefaultNodeAttributes(void);

  void registerPixmap(const MSPixmap &);
  const MSPixmap *pixmap(const MSString &) const;

  TreeModelCursor activatedNode(void) const;

  
  virtual void activate(void);
  virtual void up(void);
  virtual void down(void);
  virtual void left(void);
  virtual void right(void);
  
  virtual void set(MSAttrValueList& avList_);
  virtual MSAttrValueList& get(MSAttrValueList& avList_);

  virtual void print(const char *file_=0);
  virtual void unfreeze(void);
  virtual void show(void);

  void sensitive(MSBoolean);
  MSBoolean sensitive(void) const;

  void edit(void);
  MSBoolean editing(void) const;

  unsigned long addEditorKeyCallback( const char*,MSKeyCallback*);
  void removeEditorKeyCallback(unsigned long);
  void removeEditorKeyCallback(const char*);
protected:
  typedef MSGenericVector<MSPixmap>        PixmapList;
  typedef MSIHashKeySet<MSPixmap,MSString> PixmapRegistry;
  typedef MSIHashKeySet<MSPixmap,MSString>::Cursor PixmapRegistryCursor;

  class Editor : public MSTextField
  {
  public:
#if defined(MS_INLINE_TEMPLATE_NESTED_CLASS)
    Editor(MSWidget * owner_) : MSTextField(owner_)
      {
	_highlightThickness=0;
	color(owner()->background(),owner()->foreground());
	selectInput(ExposureMask|ButtonPressMask);
      } 
    ~Editor() {}
    
    virtual void activate(void)
      { ((MSTreeView<Element> *)(owner()->owner()))->editorActivate(); }

    virtual void escape(void)
      { ((MSTreeView<Element> *)(owner()->owner()))->editorEscape(); }
#else
    Editor(MSWidget *);
    ~Editor();
    
    virtual void activate(void);  
    virtual void escape(void);
#endif
  };
friend class Editor;
public:
class TreeNode;
friend class TreeNode;

  class TreeNode 
  {
  private:
    int _x;
    int _y;
    int _width;
    int _height;
    MSBoolean _sensitive;
    MSBoolean _expandedState;
    MSBoolean _expandable;
    PixmapList _pixmap;
    PixmapList _insensitivePixmap;
    PixmapList _selectedPixmap;
    TreeModelCursor _cursor;
  public:
    TreeNode(const MSTreeView<Element> *treeView_,
             const TreeModelCursor &cursor_,
             const NodeAttribute &nodeAttribute_) :
        _cursor(cursor_),
        _x(0),
        _y(0),
        _width(0),
        _height(0),
        _sensitive(nodeAttribute_.sensitive()),
        _expandedState(nodeAttribute_.expandedState()),
        _expandable(nodeAttribute_.expandable())
    {
      buildPixmapList(treeView_->pixmapRegistry(),pixmap(),nodeAttribute_.pixmap());
      buildPixmapList(treeView_->pixmapRegistry(),insensitivePixmap(),nodeAttribute_.insensitivePixmap());
      buildPixmapList(treeView_->pixmapRegistry(),selectedPixmap(),nodeAttribute_.selectedPixmap());
    }
    TreeNode& operator=(const TreeNode& aTreeNode_)
    {
      if (this!=&aTreeNode_)
       {
         _cursor=aTreeNode_._cursor;
       }
      return *this;
    }
    MSBoolean operator==(const TreeNode& aTreeNode_)
    {
      if (cursor()==aTreeNode_.cursor()) return MSTrue;
      else return MSFalse;
    }
    ~TreeNode(void) {}    

    int x(void) const {return _x;}
    void x(int x_) {_x=x_;}

    int y(void) const {return _y;}
    void y(int y_) {_y=y_;}

    int width(void) const {return _width;}
    void width(int width_) {_width=width_;}

    int height(void) const {return _height;}
    void height(int height_) {_height=height_;}

    MSBoolean sensitive(void) const {return _sensitive;}
    void sensitive(MSBoolean sensitive_) {_sensitive=sensitive_;}

    MSBoolean expandedState(void) const {return _expandedState;}
    void expandedState(MSBoolean expandedState_) {_expandedState=expandedState_;}

    MSBoolean expandable(void) const {return _expandable;}
    void expandable(MSBoolean expandable_) {_expandable=expandable_;}
  
    PixmapList &pixmap(void) { return _pixmap; }
    const PixmapList &pixmap(void) const { return _pixmap; }

    PixmapList &insensitivePixmap(void) { return _insensitivePixmap; }
    const PixmapList &insensitivePixmap(void) const { return _insensitivePixmap; }

    PixmapList &selectedPixmap(void) { return _selectedPixmap; }
    const PixmapList &selectedPixmap(void) const { return _selectedPixmap; }

    const TreeModelCursor &cursor(void) const {return _cursor;}
    TreeModelCursor &cursor(void) {return _cursor;}

    void configuration(int x_,int y_,int w_,int h_) { _x=x_;_y=y_;_width=w_;_height=h_; }

    void attribute(const MSTreeView<Element> *treeView_,const NodeAttribute& nodeAttribute_)
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
      NodeAttribute nodeAttr;
      nodeAttr.sensitive(sensitive());
      nodeAttr.expandable(expandable());
      nodeAttr.expandedState(expandedState());
      MSStringVector names;
      unsigned long i,nElements;
      nElements=pixmap().length();
      for (i=0;i<nElements;i++)
       {
         names<<pixmap().elementAt(i).name();
       }
      nodeAttr.pixmap(names);
      names.removeAll();
      nElements=insensitivePixmap().length();
      for (i=0;i<nElements;i++)
       {
         names<<insensitivePixmap().elementAt(i).name();
       }
      nodeAttr.insensitivePixmap(names);
      names.removeAll();
      nElements=selectedPixmap().length();
      for (i=0;i<nElements;i++)
       {
         names<<selectedPixmap().elementAt(i).name();
       }
      nodeAttr.selectedPixmap(names);
      return nodeAttr;
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
  };
protected:
  typedef MSTabularTree<TreeNode> ElementTree;
  typedef typename ElementTree::Cursor ElementTreeCursor;
  typedef MSObservableTreeEvent<Element> TreeEvent;

  ElementTree                 _elementTree;
  ElementTreeCursor           _selectedCursor;
  TreeModelCursor            *_activatedNode;
  int                         _verticalSpacing;
  int                         _horizontalSpacing;
  int                         _maxWidth;
  int                         _maxHeight;
  int                         _margin;
  int                         _nodeMargin;
  MSBoolean                   _showRootNode;
  unsigned long               _lineForeground;
  unsigned long               _nodeForeground;
  unsigned long               _nodeBackground;  
  unsigned long               _selectedNodeForeground;
  unsigned long               _selectedNodeBackground;
  Orientation                 _orientation;
  MSIntVector                 _verticalPositions;
  MSIntVector                 _horizontalPositions;
  MSShadow                    _nodeShadow;
  MSShadow                    _selectedShadow;
  MSTextField                *_pEditor;
  MSPopupMenu                *_pPopupMenu;
  MSBoolean                   _showButtons;
  MSBoolean                   _showLabels;
  MSBoolean                   _showPixmaps;
  int                         _buttonShadowThickness;
  int                         _buttonNodeMargin;
  MSPixmap                   *_collapsedButtonPixmap;
  MSPixmap                   *_expandedButtonPixmap;
  MSFontObject                _fontObject;
  GC                          _textGC;
  GC                          _pixmapGC;
  MSPixmap                   *_redrawPixmap;
  MSPixmap                   *_stipplePixmap;
  PixmapRegistry              _pixmapRegistry;
  ElementTreeCursor           _startCursor;

  virtual TreeModel &modelTree(void); 
  virtual const TreeModel &modelTree(void) const;

  ElementTreeCursor &selectedCursor(void) {return _selectedCursor;}
  const ElementTreeCursor &selectedCursor(void) const {return _selectedCursor;}

  ElementTreeCursor &startCursor(void) {return _startCursor;}
  const ElementTreeCursor &startCursor(void) const {return _startCursor;}

  TreeNode& nodeAt(ElementTreeCursor& aCursor_)             {return _elementTree.elementAt(aCursor_);}
  const TreeNode& nodeAt(ElementTreeCursor& aCursor_) const {return _elementTree.elementAt(aCursor_);}  
 
  ElementTree& elementTree(void)             {return _elementTree;}
  const ElementTree& elementTree(void) const {return _elementTree;}
 
  MSTextField *editor(void)                 {return _pEditor;}
  MSIntVector& verticalPositions(void)      {return _verticalPositions;}
  MSIntVector& horizontalPositions(void)    {return _horizontalPositions;}
  MSShadow& nodeShadow(void)                {return _nodeShadow;}
  MSShadow& selectedShadow(void)            {return _selectedShadow;}

  int maxWidth(void)                  const {return _maxWidth;}
  int maxHeight(void)                 const {return _maxHeight;}
  int margin(void)                    const {return _margin;}    
  int nodeMargin(void)                const {return _nodeMargin;}  
  int buttonNodeMargin(void)          const {return _buttonNodeMargin;}
  int verticalPosition(int depth_)    const {return _verticalPositions(depth_);} 
  int horizontalPosition(int depth_)  const {return _horizontalPositions(depth_);} 
  MSFontObject &fontObject(void) {return _fontObject;}
  GC textGC(void) {return _textGC;}
  GC pixmapGC(void) {return _pixmapGC;}
  MSPixmap *redrawPixmap(void) {return _redrawPixmap;}
  MSPixmap *stipplePixmap(void) {return _stipplePixmap;}
  PixmapRegistry &pixmapRegistry(void) {return _pixmapRegistry;}
  const PixmapRegistry &pixmapRegistry(void) const {return _pixmapRegistry;}

  virtual void adjustSize(void);
  virtual void recompVectors(void);
  
  void verticalPosition(int depth_,int value_)   {_verticalPositions.set(depth_,value_);} 
  void horizontalPosition(int depth_,int value_) {_horizontalPositions.set(depth_,value_);} 

  int computeTreeDepth(void);
  int computeTreeDepth(ElementTreeCursor,int);
  void computePositions(void);
  void setPositions(void);

  void rebuildScreen(Iterator *iterator_=0);
  void updateElementTree(TreeModelCursor,ElementTreeCursor &,Iterator *iterator_=0);
  void buildCollapsedButtonPixmap(void);
  void buildExpandedButtonPixmap(void);
  int computeVerticalPositions(ElementTreeCursor,int depth_);
  int computeHorizontalPositions(ElementTreeCursor,int depth_);  
  void setPositions(ElementTreeCursor,int depth_);
  void computeMaxExtents(ElementTreeCursor);
  void computeMaxExtents(void);  
  void drawTree(void);  
  void drawTree(ElementTreeCursor,Window);
  void drawNode(ElementTreeCursor&,Window,MSBoolean clear_=MSTrue);
  void drawPixmaps(Window,const PixmapList &,int &,int &,int);
  void shiftHorizontalPosition(ElementTreeCursor,int x_);
  void shiftVerticalPosition(ElementTreeCursor,int y_);
  MSBoolean selectNode(ElementTreeCursor&);

  ElementTreeCursor findElementCursor(const TreeModelCursor& cursor_) const;
  
  virtual void buttonPress(const XEvent *);
  virtual void keyPress(const XEvent *,KeySym,unsigned int,const char*);
  virtual void focusIn(void);
  virtual void focusOut(void);
  virtual void firstMapNotify(void);
  virtual void editorActivate(void);
  virtual void editorEscape(void);  
  virtual void mapEditor(void);
  virtual MSBoolean loseFocus(void);
  virtual void backgroundAreaSelection(const XEvent *);

  virtual void updateForeground(unsigned long);
  virtual void updateBackground(unsigned long);
  virtual void updateFont(Font);

  MSBoolean positionToCursor(ElementTreeCursor &cursor_,int x_,int y_); 
  ElementTreeCursor positionToCursor(int x_,int y_);

  MSBoolean locateTreeCursor(ElementTreeCursor,ElementTreeCursor);
  
  void collapseSubTree(ElementTreeCursor &);
  void expandSubTree(ElementTreeCursor &);

  void moveToPreviousNode(void);
  void moveToNextNode(void);
  void moveToParentNode(void);
  void moveToChildNode(void);

  virtual void nodeSelectionNotify(void);
  virtual void doubleClickNotify(void);
  virtual void subTreeCollapsedNotify(void);
  virtual void subTreeExpandedNotify(void);

  virtual void receiveEvent(MSEvent &);
  virtual void processPermuteEvent(TreeEvent &);
  virtual void processReshapeEvent(TreeEvent &);
  virtual void processDeleteEvent(TreeEvent &);
  virtual void processCopyEvent(TreeEvent &);
  virtual void processAssignEvent(TreeEvent &);
  virtual void updateData(void);
  
  void init(void);

  void resizeRedrawPixmap(int,int);
  virtual void moveDrawArea(int,int);
  virtual void redrawViewPortImmediately(void);
  virtual void redrawViewPort(void);
  void drawShadow(Window,const MSRect &,int,GC,GC);
  void calculatePixmapSize(const PixmapList&,int&,int&);
  virtual void calculateNodeSize(TreeNode &,int &,int &);

  virtual void nodeModified(const TreeModelCursor &,NodeAttribute &);
  virtual void nodeAttribute(const TreeModelCursor &,NodeAttribute &);
  virtual MSBoolean validate(TreeModelCursor &,const char *);
  virtual const char *formatOutput(MSString &,TreeModelCursor &);
  virtual MSBoolean isSensitive(const TreeNode *);
  virtual MSBoolean isExpandable(const TreeNode *);
  virtual MSBoolean isExpanded(const TreeNode *);
  virtual const PixmapList &pixmap(const TreeNode *);
  virtual const PixmapList &selectedPixmap(const TreeNode *);
  virtual const PixmapList &insensitivePixmap(const TreeNode *);
  virtual unsigned long nodeForegroundColor(const TreeNode *);
  virtual unsigned long nodeBackgroundColor(const TreeNode *);
  virtual unsigned long lineForegroundColor(const TreeNode *);

  virtual MSBoolean isNodeProtected(const TreeNode *);
};

#ifndef MSTreeListViewHEADER
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
