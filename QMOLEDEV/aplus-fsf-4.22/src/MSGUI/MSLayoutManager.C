///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSGUI/MSLayoutManager.H>

static const int VectorMinimize=0x01;


// Must be set before spanCompare works 
static ValueFunc currentValueFunc;

// Compares items based on span.
static int spanCompare(const void *ap,const void *bp)
{
  MSNodeItem **npa=(MSNodeItem **)ap;
  MSNodeItem **npb=(MSNodeItem **)bp;
  int loc_a,loc_b;
  int span_a,span_b;
  MSBoolean smallFlag;
  
  (*currentValueFunc)((MSLayoutEntry *)npa[0]->data(),&loc_a,&span_a,&smallFlag);
  (*currentValueFunc)((MSLayoutEntry *)npb[0]->data(),&loc_b,&span_b,&smallFlag);
  return span_a-span_b;
}

// Returns the sum of the space in `vec' from `loc' for length `span'.
// Adds in the appropriate padding given by `start',`inter' and `end'.
static int sumVec(int loc_,int span_,MSLayoutVector *vec_,int start_,int inter_,int end_)
{
  int i,sum=0;
  for (i=loc_;i<loc_+span_;i++) sum+=vec_[i].value();
  return sum+start_+end_+((span_>=0)?span_*inter_:0);
}

static void colValues(MSLayoutEntry *entry_,int *loc_,int *span_,MSBoolean *smallFlag_)
{
  *loc_=entry_->at().column();
  *span_=entry_->at().columnSpan();
  *smallFlag_=(entry_->at().constraints()&At::MinimizeWidth)?MSTrue:MSFalse;
}

static void rowValues(MSLayoutEntry *entry_,int *loc_,int *span_,MSBoolean *smallFlag_)
{
  *loc_=entry_->at().row();
  *span_=entry_->at().rowSpan();
  *smallFlag_=(entry_->at().constraints()&At::MinimizeHeight)?MSTrue:MSFalse;
}

static int colSize(MSWidget *widget_) 
{ return widget_->width(); }
static int rowSize(MSWidget *widget_) 
{ return widget_->height(); }

MSLayoutManager::MSLayoutManager(MSWidget *owner_) : MSManager(owner_) 
{ init(); }

MSLayoutManager::~MSLayoutManager(void)
{
  if (_rowHeights!=0)   delete [] _rowHeights;
  if (_columnWidths!=0) delete [] _columnWidths;
  if (_geometry!=0)     delete _geometry;
  _geometry=0;
  
  freeze();
  // remove all children
  MSLayoutEntry *entry;
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np;

  // first remove the node from the list
  // then destroy the widget
  // this will prevent childDestroy/removeChild from causing any damage
  // delete the child list
  while ((np=hp->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();
     delete np;
     if (entry!=0)
      {
	// destroy will cause childDestroy to be invoked
        if (entry->widget()!=0) safeDestroy(entry->widget());  
        delete entry;
      }
     _childCount--;
   }

  // delete the mapped list
  hp=(MSNodeItem *) mappedListHead();
  while ((np=hp->next())!=hp) delete np;
}

void MSLayoutManager::init(void)
{
  _geometry=0;
  _stateFlag=PlacementFlag;
  _vectorState=Invalid;
  currentValueFunc=0;
  _layoutStyle=ColumnMajor;
  _orientation=Unspecified;
  _shadowThickness=2;
  _rows=_columns=0;
  _rowHeights=0;
  _columnWidths=0;
  _rowSpacing=0;
  _columnSpacing=0;
  _margin=0;
  _vectorHeight=0;
  _vectorWidth=0;
  _innerWidth=0;
  _innerHeight=0;
  shadowStyle(MSEtchedIn);
  freeze();
}

void MSLayoutManager::orientation(MSLayoutManager::Orientation orientation_) 
{ 
  if (orientation()!=orientation_)
   {
     _orientation=orientation_;
     updateOrientation();
   }
}

void MSLayoutManager::updateOrientation(void)
{ if (setPositions()==MSTrue) adjustSize(); }

// layout style is not yet implemented
void MSLayoutManager::layoutStyle(MSLayoutManager::LayoutStyle style_) 
{ 
  if (layoutStyle()!=style_)
   {
     _layoutStyle=style_;
     if (setPositions()==MSTrue) adjustSize(); 
   }
}

void MSLayoutManager::geometry(const MSIndexVector& geometry_)
{ 
  if (_geometry==0) _geometry=new MSIndexVector(geometry_);
  else *_geometry=geometry_;
  if (setPositions()==MSTrue) adjustSize(); 
}

void MSLayoutManager::childInsert(MSWidget *widget_)  
{ insertChild(widget_); }
void MSLayoutManager::childRemove(MSWidget *widget_)  
{ removeChild(widget_); }
void MSLayoutManager::childCreate(MSWidget *widget_)  
{ insertChild(widget_); }
void MSLayoutManager::childDestroy(MSWidget *widget_) 
{ removeChild(widget_); }

int MSLayoutManager::innerHeight(void) const
{ return _innerHeight; }
int MSLayoutManager::innerWidth(void) const
{ return _innerWidth; }

int MSLayoutManager::rowHeight(unsigned row_) const
{ return (row_<rows())?_rowHeights[row_].value():0; }
int MSLayoutManager::columnWidth(unsigned col_) const
{ return (col_<columns())?_columnWidths[col_].value():0; }

int MSLayoutManager::row(MSWidget *widget_) const
{ 
  const MSLayoutEntry *entry=getEntry(widget_);
  return (entry!=0)?entry->at().row():0;
}
int MSLayoutManager::column(MSWidget *widget_) const
{ 
  const MSLayoutEntry *entry=getEntry(widget_);
  return (entry!=0)?entry->at().column():0;
}
int MSLayoutManager::columnSpan(MSWidget *widget_) const
{ 
  const MSLayoutEntry *entry=getEntry(widget_);
  return (entry!=0)?entry->at().columnSpan():1;
}
int MSLayoutManager::rowSpan(MSWidget *widget_) const
{ 
  const MSLayoutEntry *entry=getEntry(widget_);
  return (entry!=0)?entry->at().rowSpan():1;
}
unsigned long MSLayoutManager::options(MSWidget *widget_) const
{ 
  const MSLayoutEntry *entry=getEntry(widget_);
  return (entry!=0)?entry->at().constraints():0;
}

//  handle child resize
void MSLayoutManager::childConfigure(MSWidget *widget_) 
{
  if (widget_!=this&&placementFlag()==MSFalse)
   {
     MSLayoutEntry *entry=findMappedEntry(widget_);
     if (entry!=0) adjustSize();
   }
}

//  handle child resizeConstraints change
void MSLayoutManager::childResizeConstraints(MSWidget *widget_) 
{
  MSLayoutEntry *entry=getEntry(widget_);  // get entry from childList
  if (entry!=0&&widget_!=this)
   {
     unsigned long opts=widget_->resizeConstraints();
     if (entry->at().constraints()!=opts)
      {
        entry->at().constraints(opts);
	if (entry->mapped()==MSTrue) adjustSize();
      }
   }
}

// If `size' is larger than the sum of all widths in `vec',
// the extra space is distributed evenly among appropriate
// candidates of `vec'.
void MSLayoutManager::extraColumnSpace(int num_,MSLayoutVector *vec_,int size_)
{ extraSpace(num_,vec_,size_,uniformColumns()); }
void MSLayoutManager::extraRowSpace(int num_,MSLayoutVector *vec_,int size_)
{ extraSpace(num_,vec_,size_,uniformRows()); }

void MSLayoutManager::extraSpace(int num_,MSLayoutVector *vec_,int size_,MSBoolean uniform_)
{
  int i,sum=0;
  for (i=0;i<num_;i++) sum+=vec_[i].value();
  int diff=size_-sum;
  if (diff!=0)
   {
     int ndist=0,amt=0;
     int *dist=(int *) new int[num_];
     if (uniform_==MSTrue) for (i=0;i<num_;i++) dist[ndist++]=i;
     else ndist=findDistribution(0,num_,vec_,dist,MSTrue);
     
     if (diff>0) 
      {
	if (ndist>0)
	 {
	   amt=diff/ndist;
	   for (i=0;i<ndist;i++) 
	    {
	      vec_[dist[i]]._value+=amt;
	    }
	 }
	delete [] dist;
      }
     else if (diff<0) 
      {
	diff=sum-size_;
	if (ndist>0)
	 {
	   amt=diff/ndist;
	   for (i=0;i<ndist;i++) 
	    {
	      vec_[dist[i]]._value-=amt;
	    }
	 }
	delete [] dist;
      }
   }
}

// This routine fills in `result' with a list of indices
// into the spacing vector suitable for distributing required
// space.  Normally, it skips those items marked as
// VectorMinimize.  However, if there aren't any non-VectorMinimize
// spaces, all of them become candidates.
int MSLayoutManager::findDistribution(int loc_,int span_,MSLayoutVector *vec_,int *result_,MSBoolean ignoreZeros_)
{
  int i,count=0,zeros=0;
  for (i=loc_;i<loc_+span_;i++) 
   {
     if (vec_[i].mask()&VectorMinimize) continue; 
     if (vec_[i].value()==0)
      {
        zeros++;
        continue;
      }
     result_[count++]=i;
   }
  if (count==0)  // Add them all back in 
   { 
     for (i=loc_;i<loc_+span_;i++) result_[count++]=i;  
   }
  else if(zeros!=0 && ignoreZeros_==MSFalse)
   {
     count=0;
     for (i=loc_;i<loc_+span_;i++)
      {
        if(vec_[i].mask()&VectorMinimize) continue;
        if(vec_[i].value()==0) result_[count++]=i;
      }
   }
  return count;
}

// If `size' is larger than the current sum of space in `vec'
// specified by `loc' and `span', the difference in space
// is evenly added to each vector entry given by `distrib'.
void MSLayoutManager::doDistribution(int n_,int *distrib_,int loc_,int span_,
				     MSLayoutVector *vec_,int size_,int spacing_)
{
  int diff,amt,i,sum=0;
  
  for (i=loc_;i<loc_+span_;i++) sum+=vec_[i].value();
  if (span_>1)  sum+=(span_-1)*spacing_;
  diff=size_-sum;
  if (diff>0&&n_>0) 
   {
     // Distribution required 
     amt=diff/n_;
     for (i=0;i<n_-1;i++) 
      {
	vec_[distrib_[i]]._value+=amt;
	diff-=amt;
      }
     // Last one deincremented by remaining space 
     vec_[distrib_[i]]._value+=diff;
   }
}

// Steps through the list of widgets.  If the widget is marked
// as having the small flag set, it sets all corresponding
// options in `vec'.
void MSLayoutManager::setVectorOptions(ValueFunc valueFunc,MSLayoutVector *vec_)
{
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;
  int            j;
  int            loc,span;
  MSBoolean      smallFlag;
  
  while ((np=np->next())!=hp) 
   {
     entry=(MSLayoutEntry *) np->data();
     (*valueFunc)(entry,&loc,&span,&smallFlag);
     if (smallFlag) for (j=loc;j<loc+span;j++) vec_[j]._mask=VectorMinimize;
   }
}

// Determines the vector size by examining locations of all
// widgets.  Basically determines the maximum of loc+span.
int MSLayoutManager::vectorSize(ValueFunc valueFunc)
{
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;
  int            loc,span;
  int            result=0;
  MSBoolean      smallFlag;
  
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();
     (*valueFunc)(entry,&loc,&span,&smallFlag);
     if (result<loc+span) result=loc+span;
   }
  return result;
}

// This routine computes the values for either the row or column
// spacing vector.  The strategy is as follows:
//   1. Scan mw and determine number of entrys in result and allocate
//   2. Scan list and set appropriate vector flags.
//   3. Sort the managed widgets in span order (low to high)
//   4. For each item in sorted list:
//      A. Determine distribution locations.
//      B. Distribute any needed space to locations.
// There are some inefficiencies here that could be overcome.
int MSLayoutManager::computeVector(ValueFunc valueFunc,SizeFunc sizeFunc,
				   int spacing_,MSLayoutVector *result_)
{
  MSLayoutEntry *entry;
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np;
  int            resultNum=0;
  int            n_dist,*distrib;
  int            loc,span;
  MSBoolean      smallFlag;
  
  resultNum=vectorSize(valueFunc);
  if (resultNum) 
   {
     setVectorOptions(valueFunc,result_);
     currentValueFunc=valueFunc;
     mappedListHead()->sort(hp,spanCompare);
     
     hp=mappedListHead();
     np=hp;
     distrib=new int[resultNum];
     while ((np=np->next())!=hp) 
      {
        entry=(MSLayoutEntry *) np->data();
	(*valueFunc)(entry,&loc,&span,&smallFlag);
	n_dist=findDistribution(loc,span,result_,distrib,MSFalse);
	doDistribution(n_dist,distrib,loc,span,result_,(*sizeFunc)(entry->widget()),spacing_);
      }
     delete [] distrib;
   } 
  return resultNum;
}

// Recomputes the size vectors in the layout widget by
// examining the preferred sizes of subwidgets.  The
// following fields are modified: num_rows,num_cols,
// rows,cols,vec_height,and vec_width.
void MSLayoutManager::recomputeVectors(void)
{
  unsigned i;
  if (_columnWidths!=0) delete [] _columnWidths;
  _columns=0;
  if (_rowHeights!=0) delete [] _rowHeights;
  _rows=0;
  _vectorWidth=_vectorHeight=0;
  
  _columns=vectorSize(colValues);
  if (columns()>0)
   { 
     _columnWidths=(MSLayoutVector *)new MSLayoutVector[columns()]; 
     _columns=computeVector(colValues,colSize,columnSpacing(),_columnWidths);
     if (uniformColumns()==MSTrue)
      {
        int max=0;
        for (i=0;i<columns();i++) max=(max<_columnWidths[i].value())?_columnWidths[i].value():max;
        for (i=0;i<columns();i++) _columnWidths[i]._value=max; 
      }
   }
  else _columnWidths=0;
  for (i=0;i<columns();i++) _vectorWidth+=_columnWidths[i].value();
  
  _rows=vectorSize(rowValues);
  if (rows()>0)
   {
     _rowHeights= (MSLayoutVector *)new MSLayoutVector[rows()]; 
     _rows=computeVector(rowValues,rowSize,rowSpacing(),_rowHeights);
     if (uniformRows()==MSTrue)
      {
        int max=0;
        for (i=0;i<rows();i++) max=(max<_rowHeights[i].value())?_rowHeights[i].value():max;
        for (i=0;i<rows();i++) _rowHeights[i]._value=max;	
      }
   }
  else _rowHeights=0;
  for (i=0;i<rows();i++) _vectorHeight+=_rowHeights[i].value();
}

// This routine moves the widget `w' inside the space given
// by x,y,width,height.  Its location in this space
// is determined by looking at the resize constraints
void MSLayoutManager::placeWidget(MSWidget *widget_,int x_,int y_,
				  int w_,int h_,int rw_,int rh_,unsigned long opts_)
{
  int rx,ry;

  if (opts_&At::Left) rx=x_;
  else if (opts_&At::Right) rx=x_+w_-rw_;
  else rx=x_+((w_-rw_)>>1);
  
  if (opts_&At::Top) ry=y_;
  else if (opts_&At::Bottom) ry=y_+h_-rh_;
  else ry=y_+((h_-rh_)>>1);
  
  widget_->moveTo(rx,ry);
}

// This routine places each widget in `managed' according to the
// spacing vectors `cvec' and `rvec' and the widget placement
// options (justification and resizing).  First,
// the routine will resize the widget to fit its allocated
// space.  Then it will place the widget paying attention
// to the resize constraints.
void MSLayoutManager::doPlacement(MSLayoutVector *cvec_,MSLayoutVector *rvec_,int vp_,int hp_,int rs_,int cs_)
{
  int r,c,aw,ah,nw,nh;
  int offset=highlightThickness()+shadowThickness()+margin();

  MSLayoutEntry *entry;
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  
  while((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     r=sumVec(0,entry->at().column(),cvec_,hp_,cs_,0)+offset;
     c=sumVec(0,entry->at().row(),rvec_,vp_,rs_,0)+offset;
     aw=sumVec(entry->at().column(),entry->at().columnSpan(),cvec_,0,cs_,-cs_);
     ah=sumVec(entry->at().row(),entry->at().rowSpan(),rvec_,0,rs_,-rs_);
     nw=aw;
     nh=ah;

     if (entry->at().constraints()&At::MaintainWidth) nw=entry->widget()->width(); 
     if (entry->at().constraints()&At::MaintainHeight) nh=entry->widget()->height(); 
     if (((nw!=entry->widget()->width())||(nh!=entry->widget()->height()))&&(nw>0)&&(nh>0))
      { entry->widget()->resize(nw,nh); }

     nw=entry->widget()->width();
     nh=entry->widget()->height();
     placeWidget(entry->widget(),r,c,aw,ah,nw,nh,entry->at().constraints());
   }
}

// Places the children of the layout widget.  There are several steps:
//   1.  Distribute any extra space into local copies of spacing vectors.
//   2.  If the option is set, any extra space is offered to each widget.
//   3.  The final placement is done according to the actual size of
//       each widget and the space allocated for it.
void MSLayoutManager::placement(void)
{
  if (mapped()==MSTrue&&rows()>0&&columns()>0) 
   {
     unsigned i;
     // Make local copies of vectors 
     MSLayoutVector *lrows=(MSLayoutVector *)new MSLayoutVector[rows()];
     MSLayoutVector *lcols=(MSLayoutVector *)new MSLayoutVector[columns()];

     for (i=0;i<rows();i++) lrows[i]=_rowHeights[i];
     for (i=0;i<columns();i++) lcols[i]=_columnWidths[i];
     
     // add or remove space to/from vector (grow/shrink) 
     extraColumnSpace(columns(),lcols,realWidth());
     extraRowSpace(rows(),lrows,realHeight());
     
     placementFlag(MSTrue);
     doPlacement(lcols,lrows,innerHeight(),innerWidth(),rowSpacing(),columnSpacing());
     placementFlag(MSFalse);    
     
     // Free up local resources 
     delete [] lcols;
     delete [] lrows;
  }
}

// This routine registers the position of Widget w.  
// The row and column must be non-negative.  The horizontal and vertical
// span must be positive.  The options are as follows:
//   Left		Horizontally left justified.
//   Right		Horizontally right justified.
//   Top		Vertically top justified.
//   Bottom 	        Vertically bottom justified.
//   MinimizeWidth      Force the width to be as small as possible.
//   MinimizeHeight	Force the height to be as small as possible.
//   MaintainWidth	Don't try to expand the widget horizontally.
//   MaintainHeight     Don't try to expand the widget vertically.
// If `options' is equal to Default, it is filled with 
// the default value for the layout widget. The routine adds the 
// information into a table and recomputes position information.
void MSLayoutManager::childPosition(MSWidget *widget_,const At& at_)
{
  if ((at_.column()>=0)&&(at_.row()>=0)&&(at_.columnSpan()!=0)&&(at_.rowSpan()!=0)) 
   {
     MSLayoutEntry *entry=getEntry(widget_);  
     if (entry!=0)  // set the child position,span info
      {
        if (entry->at()!=at_)
         { 
           entry->at()=at_;
           widget_->resizeConstraints(at_.constraints());
	   if (entry->mapped()==MSTrue) adjustSize();
	 }
      }
   }
}

At MSLayoutManager::childPosition(MSWidget *widget_)
{
  MSLayoutEntry *entry=getEntry(widget_);  
  if (entry!=0) return At(entry->at());
  else return At(0,0,1,1,widget_->resizeConstraints()); 
}

// insert the child into the child list
void MSLayoutManager::insertChild(MSWidget *widget_)
{
  MSLayoutEntry *entry=getEntry(widget_);
  if (entry==0&&widget_!=0) 
   {
     entry=new MSLayoutEntry(widget_);
     entry->at().constraints(widget_->resizeConstraints());
     MSNodeItem *np=new MSNodeItem((void *)entry);
     np->insert(childListHead());    // fifo
     _childCount++;
     setDefaultChildPosition(entry); // allow subclasses to set the default position
     if (widget_->mapped()==MSTrue) childMap(widget_);
   }
}

// remove child from the child list
void MSLayoutManager::removeChild(MSWidget *widget_)
{
  MSLayoutEntry *entry=0;
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;

  // check the child list
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     if (entry->widget()==widget_) 
      {
        delete np;
        if (entry->mapped()==MSTrue) removeMappedEntry(entry);
        delete entry;
        _childCount--;
        break;
      }
     else entry=0; 
   }
  if (entry!=0) adjustSize();
}

// Move the widget from the unmapped nodelist to the mapped nodelist
void MSLayoutManager::childMap(MSWidget *widget_)
{
  if (widget_!=this)
   {
     MSLayoutEntry *entry=findMappedEntry(widget_);
     if (entry==0)
      {
	entry=getEntry(widget_);  // get entry from childList
	entry->mapped(MSTrue);
	insertMappedEntry(entry);
        adjustSize();
      }
   }
}

// a child has been unmapped - move it to the unmapped nodelist
void MSLayoutManager::childUnmap(MSWidget *widget_)
{
  if (widget_!=this)
   {
     MSLayoutEntry *entry=findMappedEntry(widget_);
     if (entry!=0)
      {
	entry->mapped(MSFalse);
	removeMappedEntry(entry);
        adjustSize();
      }
   }
}

void MSLayoutManager::insertMappedEntry(MSLayoutEntry *entry_)
{
  MSNodeItem *hp=mappedListHead();
  MSNodeItem *np=hp;
  MSBoolean   found=MSFalse;

  while ((np=np->next())!=hp)
   {
     if (entry_==(MSLayoutEntry *)np->data())
      {
        found=MSTrue;
        break;
      }
   }
  if (found==MSFalse)
   {
     np=new MSNodeItem((void *)entry_);
     np->insert(hp);  // fifo
     entry_->mapped(MSTrue);
   }
}

void MSLayoutManager::removeMappedEntry(MSLayoutEntry *entry_)
{
  MSNodeItem *hp=mappedListHead();
  MSNodeItem *np=hp;

  while ((np=np->next())!=hp)
   {
     if (entry_==(MSLayoutEntry *)np->data())
      {
        entry_->mapped(MSFalse);
        delete np;
        break;
      }
   }
}

MSLayoutEntry *MSLayoutManager::findMappedEntry(MSWidget *widget_)
{
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;

  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();
     if (entry->widget()==widget_) return entry;
   }
  return 0;
}

// this sucks, but we need a const version 
const MSLayoutEntry *MSLayoutManager::getEntry(MSWidget *widget_) const
{
  MSNodeItem    *hp=(MSNodeItem *)childListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;

  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();
     if (entry->widget()==widget_) return entry;
   }
  return 0;
}

MSLayoutEntry *MSLayoutManager::getEntry(MSWidget *widget_)
{
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;

  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();
     if (entry->widget()==widget_) return entry;
   }
  return 0;
}

MSLayoutEntry *MSLayoutManager::getEntry(int row_,int col_)
{
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;

  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *) np->data();
     if (entry->at().column()==col_&&entry->at().row()==row_) return entry;
   }
  return 0;
}

void MSLayoutManager::show(void)
{
  if (mapped()==MSFalse)
   {
     freeze();
     MSNodeItem    *hp=childListHead();
     MSNodeItem    *np=hp;
     MSLayoutEntry *entry;
     while((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();
	if (entry!=0 && (entry->widget()->firstMap()==MSFalse || entry->mapped()==MSTrue))
         {
           entry->widget()->show();
         }
      }
     map();
   }
}

void MSLayoutManager::firstMapNotify(void)
{ if (geometry().length()>0||orientation()!=Unspecified) setPositions(); }

void MSLayoutManager::map(void)
{
  if (mapped()==MSFalse)
   {
     _mapped=MSTrue;
     if (firstMap()==MSFalse) firstMap(MSTrue);
     unfreeze();
     _mapped=MSFalse;
     MSWidget::map();
   }
}
 
void MSLayoutManager::resize(int w_,int h_)
{
  if (mapped()==MSTrue)
   {
     if (MSRect::width()!=w_||MSRect::height()!=h_)
      {     
        int offset=highlightThickness()+shadowThickness();
	if (w_>offset) MSRect::width(w_);
	if (h_>offset) MSRect::height(h_);
	XResizeWindow(display(),window(),width(),height());	
	placement();
        configure();
	childConfigureNotify();	
      }
     else
      {
	placement();
        configure();
      }
   }
}

void MSLayoutManager::unfreeze(void)
{
  if (firstMap()==MSTrue)
   {
     freezeStatus(MSFalse);
     adjustSize();
   }
}

int MSLayoutManager::realHeight(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (height()-2*innerHeight()-(rows()-1)*rowSpacing()-2*offset);
}

int MSLayoutManager::realWidth(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (width()-2*innerWidth()-(columns()-1)*columnSpacing()-2*offset);
}

int MSLayoutManager::idealHeight(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (vectorHeight()+2*innerHeight()+(rows()-1)*rowSpacing()+2*offset);
}

int MSLayoutManager::idealWidth(void) const
{
  int offset=highlightThickness()+shadowThickness()+margin();
  return (vectorWidth()+2*innerWidth()+(columns()-1)*columnSpacing()+2*offset);
}

void MSLayoutManager::uniformRows(MSBoolean b_)
{
  if (b_!=uniformRows())
   {
     if (b_==MSTrue) _stateFlag|=UniformRows;
     else _stateFlag&=~UniformRows;
     adjustSize();
   }
}

void MSLayoutManager::uniformColumns(MSBoolean b_)
{
  if (b_!=uniformColumns())
   {
     if (b_==MSTrue) _stateFlag|=UniformColumns;
     else _stateFlag&=~UniformColumns;
     adjustSize();
   }
}

void MSLayoutManager::margin(int margin_)
{
  if (margin_!=margin())
   {
     _margin=margin_;
     adjustSize();
   }
}

void MSLayoutManager::rowSpacing(int rs_)
{
  if (rs_!=rowSpacing())
   {
     _rowSpacing=rs_;
     adjustSize();
   }
}

void MSLayoutManager::columnSpacing(int cs_)
{
  if (cs_!=columnSpacing())
   {
     _columnSpacing=cs_;
     adjustSize();
   }
}

void MSLayoutManager::adjustSize(void)
{
  if (firstMap()==MSTrue&&frozen()==MSFalse)
   {
     if (lockPositions()==MSFalse) recomputeVectors();
     _vectorState=Minimum;
     if (lockSize()==MSFalse) resize(idealWidth(),idealHeight());
     else 
      {
	placement();
        configure();
      }
   }
}

void MSLayoutManager::print(const char *file_)
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
     clear();
     redraw();
     MSWidget    *pWidget;
     Window       root,parent,*children=(Window *)0;
     unsigned int nchildren=0;
     (void)XQueryTree(display(),window(),&root,&parent,&children,&nchildren);
     for (int i=0;i<nchildren;i++)
      {
        pWidget=widget(children[i]);
	if (pWidget!=0&&pWidget->mapped()==MSTrue)
	 {
	   displayPrintOriginInc(pWidget);
	   pWidget->print();
	   displayPrintOriginDec(pWidget);
	 }
      }
     XFree((char *)children);
     if (fileOpen==MSTrue) 
      {
	displayPrintClose();
	outputMode(Draw);
      }
   }
}

void MSLayoutManager::visibilityObscured(void)
{
  visible(MSFalse);
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;
  while((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     if (entry!=0) visibilityObscuredNotify(entry->widget());
   }
}

void MSLayoutManager::visibilityUnobscured(void)
{
  visible(MSTrue);
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;
  MSLayoutEntry *entry;
  while((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     if (entry!=0 && entry->mapped()==MSTrue) visibilityUnobscuredNotify(entry->widget());
   }
}

void MSLayoutManager::printChildInfo(void)
{
  MSLayoutEntry *entry;
  MSNodeItem    *hp=childListHead();
  MSNodeItem    *np=hp;
  int            i=0;
  MSString       opts;

  cerr<<"entry\trow\tcol\trow span\tcolumn span\toptions\twidth\theight\tmapped"<<endl;
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();
     opts=entry->at().parsedConstraints();
     cerr<<i<<"\t";
     cerr<<entry->at().row()<<"\t";
     cerr<<entry->at().column()<<"\t";
     cerr<<entry->at().rowSpan()<<"\t";
     cerr<<entry->at().columnSpan()<<"\t";     
     cerr<<opts.string()<<"\t";
     cerr<<entry->widget()->width()<<"\t";     
     cerr<<entry->widget()->height()<<"\t";
     cerr<<int(entry->widget()->mapped())<<endl;     
     i++;
   }
}

// compute the least common multiple of x and y
// from: Wirth,Programming in MODULA-2
static int lcm(int x_,int y_)
{
  int u=x_;
  int v=y_;
  while (x_!=y_)
   {
     if (x_>y_) { x_-=y_; u+=v; }
     else { y_-=x_; v+=u; }
   }
  return (int)(u+v)>>1;  
}

// compute lcm on a vector of numbers
static int lcm(const MSIndexVector& span_)
{
  int n=span_.length();
  if (n>0)
   {
     int i=0;
     int x=((int)span_(0)>0)?(int)span_(0):1;
     for (i=0;i<n-1;i++) x=lcm(x,((int)span_(i+1)>0)?(int)span_(i+1):1);
     return x;
   }
  return 0;
}

static void computeColumnSpan(int index_,int rows_,int cols_,MSIndexVector& m_,int& col_,int& span_)
{
  int r=0,c=0,span=0,col=0,w=cols_;
  for (r=0;r<rows_&&span==0;r++)
   {
     for (c=0;c<cols_;c++)
      {
	if (m_(r*w+c)==index_) 
	 {
	   if (span==0) col=c;
	   span++;          
	 }
      }
   } 
  col_=col;
  span_=span;
}

static void computeRowSpan(int index_,int rows_,int cols_,MSIndexVector& m_,int& row_,int& span_)
{
  int r=0,c=0,span=0,row=0,w=cols_;
  for (c=0;c<cols_&&span==0;c++)
   {
     for (r=0;r<rows_;r++)
      {
	if (m_(r*w+c)==index_) 
	 {
	   if (span==0) row=r;
	   span++;          
	 }
      }
   } 
  row_=row;
  span_=span;
}

void MSLayoutManager::setDefaultChildPosition(MSLayoutEntry *entry_)
{
  if (entry_!=0)
   {
     if (geometry().length()==0&&orientation()!=Unspecified)
      {
	if (orientation()==Vertical)
	 {
	   entry_->at().row(childCount()-1);
	   entry_->at().column(0);
	 }
	else 
	 {
	   entry_->at().row(0);
           entry_->at().column(childCount()-1);
	 }
      }
     else setPositions();
   }
}

MSBoolean MSLayoutManager::setPositions(void)
{
  MSBoolean change=MSFalse;
  if (firstMap()==MSTrue&&childCount()>0)
   {
     if (geometry().length()>0)
      {
	const MSIndexVector& aIndexVector=geometry();
	int span=0,index=0;
	int r=0,c=0,i=0;
	int rows,columns;
	if (aIndexVector.length()==1)
	 {
	   rows=1;
	   columns=aIndexVector(0);
	 } 
	else
	 {
	   rows=aIndexVector.length();
	   columns=lcm(aIndexVector); 	
	 }
	int w=columns;
	int nr=childCount();
	MSIndexVector matrix(rows*columns); // matrix in ravel order
	if (rows==1)
	 {
	   rows=nr/columns;
	   rows+=((nr%columns)>0)?1:0;
	   matrix.series(rows*columns);
	 }
	else if (columns>1) // layout in column major order, row major not yet implemented
	 {
	   for (r=0;r<rows;r++)
	    {
	      span=(int)columns/((aIndexVector(r)>0)?aIndexVector(r):1);
	      for (c=0;c<columns;c+=span)
	       {
		 for (i=0;i<span;i++) matrix.set(r*w+(c+i),index);
		 index++;
	       }
	    }
	 }
	else matrix.series(matrix.length());
	int row=0,col=0,hspan=0,vspan=0;
	int ax,ay,v_span,h_span;
	MSBoolean mstat;
	MSLayoutEntry *entry;
	MSNodeItem    *hp=childListHead(); // not mappedListHead
	MSNodeItem    *np;
	for (r=0,np=hp;(np=np->next())!=hp&&r<nr;r++)
	 {
	   computeRowSpan(r,rows,columns,matrix,row,vspan);
	   computeColumnSpan(r,rows,columns,matrix,col,hspan);
	   entry=(MSLayoutEntry *)np->data();	
	   if (entry!=0)
	    {
	      ax=entry->at().column();
	      ay=entry->at().row();
	      h_span=entry->at().columnSpan();
	      v_span=entry->at().rowSpan();
	      mstat=entry->widget()->mapped();
	      entry->at().column(col);
	      entry->at().row(row);
	      entry->at().columnSpan(hspan);
	      entry->at().rowSpan(vspan);
	      if (hspan==0||vspan==0)
	       {
		 entry->at().columnSpan(1);
		 entry->at().rowSpan(1);
		 entry->widget()->unmap();
	       }
	      if (ax!=entry->at().column()||ay!=entry->at().row()||
		  h_span!=entry->at().columnSpan()||v_span!=entry->at().rowSpan()||
		  mstat!=entry->widget()->mapped()) change=MSTrue;
	    }
	 }
      }
     else change=setDefaultPositions();
   }
  return change;
}

MSBoolean MSLayoutManager::setDefaultPositions(void)
{
  MSBoolean change=MSFalse;
  if (orientation()!=Unspecified)
   {
     int            i=0;
     int            r,c,hs,vs;
     int            ax,ay,h_span,v_span;
     MSBoolean      mstat;
     MSLayoutEntry *entry;
     MSNodeItem    *hp=childListHead(); 
     MSNodeItem    *np=hp;

     while ((np=np->next())!=hp)
      {
	hs=1,vs=1;
	if (orientation()==MSLayoutManager::Vertical) r=i,c=0;
	else r=0,c=i;
	entry=(MSLayoutEntry *)np->data();	
	if (entry!=0)
	 {
	   ax=entry->at().column(),ay=entry->at().row();
	   h_span=entry->at().columnSpan(),v_span=entry->at().rowSpan();
	   mstat=entry->widget()->mapped();
	   entry->at().column(c),entry->at().row(r);
	   entry->at().columnSpan(hs),entry->at().rowSpan(vs);
	   if (ax!=entry->at().column()||ay!=entry->at().row()||
	       h_span!=entry->at().columnSpan()||v_span!=entry->at().rowSpan()||
	       mstat!=entry->widget()->mapped()) change=MSTrue;
	 }
	i++;
      }
   }
  return change;
}
  
MSWidgetVector MSLayoutManager::children(void)
{
  MSWidgetVector vector;
  MSNodeItem *hp=childListHead();
  MSNodeItem *np=hp;
  
  while ((np=np->next())!=hp)
   {
     MSLayoutEntry *entry=(MSLayoutEntry *)np->data();
     vector.append(entry->widget());
   }
  return vector;
}

void MSLayoutManager::set(MSAttrValueList& avList_)
{
  MSManager::set(avList_);
  MSIndexVector index;
  for (unsigned i=0;i<avList_.length();i++)
   {
     if (avList_[i].attribute()=="orientation")
      {
	MSString val(avList_[i].value());
	orientation(((val=="Vertical")?Vertical:((val=="Horizontal")?Horizontal:Unspecified)));
	index<<i;  
      }
     else if (avList_[i].attribute()=="geometry")
      geometry(MSIndexVector(avList_[i].value())),index<<i;
     else if (avList_[i].attribute()=="margin")
      margin(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="rowSpacing")
      rowSpacing(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="columnSpacing")
      columnSpacing(avList_[i].value().asInt()),index<<i;
     else if (avList_[i].attribute()=="uniformColumns")
      uniformColumns(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="uniformRows")
      uniformRows(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="lockSize")
      lockSize(avList_[i].value().asBoolean()),index<<i;
     else if (avList_[i].attribute()=="lockPositions")
      lockPositions(avList_[i].value().asBoolean()),index<<i;
   }
  avList_.remove(index);
}

MSAttrValueList& MSLayoutManager::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("rowSpacing",MSString(rowSpacing()));
  avList_<<MSAttrValue("columnSpacing",MSString(columnSpacing()));
  avList_<<MSAttrValue("margin",MSString(margin()));
  avList_<<MSAttrValue("orientation",
		       (orientation()==Vertical?"Vertical":
			(orientation()==Horizontal?"Horizontal":"Unspecified")),
		       MSStringVector("Horizontal\nVertical\nUnspecified"));
  avList_<<MSAttrValue("geometry",geometry().asString(),MSAttrValue::String);
  MSStringVector aStringVector("MSTrue\nMSFalse");

  avList_<<MSAttrValue("uniformColumns",
		       (uniformColumns()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);
  avList_<<MSAttrValue("uniformRows",
		       (uniformRows()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);		       
  avList_<<MSAttrValue("lockSize",
		       (lockSize()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);		       
  avList_<<MSAttrValue("lockPositions",
		       (lockPositions()==MSTrue?"MSTrue":"MSFalse"),
		       aStringVector);		       
  return MSManager::get(avList_);
}

void MSLayoutManager::lockSize(MSBoolean b_)      
{ 
  if (b_==MSTrue) _stateFlag|=LockSize;
  else _stateFlag&=~LockSize;
}

void MSLayoutManager::lockPositions(MSBoolean b_) 
{ 
  if (b_==MSTrue) _stateFlag|=LockPositions;
  else _stateFlag&=~LockPositions;
}

void MSLayoutManager::placementFlag(MSBoolean b_) 
{ 
  if (b_==MSTrue) _stateFlag|=PlacementFlag;
  else _stateFlag&=~PlacementFlag;
}

const MSIndexVector& MSLayoutManager::geometry(void) const         
{ 
  if (_geometry!=0) return *_geometry;
  else return MSIndexVector::nullVector(); 
}

MSLayoutManager::Orientation MSLayoutManager::orientation(void) const
{ return _orientation; }
MSLayoutManager::LayoutStyle MSLayoutManager::layoutStyle(void) const
{ return _layoutStyle; }

int MSLayoutManager::vectorHeight(void) const
{ return _vectorHeight; }
int MSLayoutManager::vectorWidth(void) const
{ return _vectorWidth; }

MSNodeItem *MSLayoutManager::mappedListHead(void) 
{ return &_mappedListHead; }

int MSLayoutManager::rows(void) const                   
{ return _rows; }
int MSLayoutManager::columns(void) const             
{ return _columns; }
int MSLayoutManager::rowSpacing(void) const             
{ return _rowSpacing; }
int MSLayoutManager::columnSpacing(void) const         
{ return _columnSpacing; }
int MSLayoutManager::margin(void) const                
{ return _margin; }

unsigned long MSLayoutManager::stateFlag(void) const
{ return _stateFlag; }
MSBoolean MSLayoutManager::placementFlag(void) const
{ return MSBoolean((stateFlag()&PlacementFlag)==PlacementFlag); }
MSBoolean MSLayoutManager::uniformColumns(void) const
{ return MSBoolean((stateFlag()&UniformColumns)==UniformColumns); }
MSBoolean MSLayoutManager::uniformRows(void) const
{ return MSBoolean((stateFlag()&UniformRows)==UniformRows); }
MSBoolean MSLayoutManager::lockSize(void) const         
{ return MSBoolean((stateFlag()&LockSize)==LockSize); }
MSBoolean MSLayoutManager::lockPositions(void) const
{ return MSBoolean((stateFlag()&LockPositions)==LockPositions); }

// Virtual methods for ArrowKey Traversal

void MSLayoutManager::up(void)
{
  MSWidget *focusWidget = inputFocus();
  MSLayoutEntry *entry = getEntry(focusWidget);
  if (entry)
   {
     MSLayoutEntry *next = getPrevVerticalEntry(entry);
     if (entry!=next && next!= 0)
       (void) setFocus(next->widget());
   }
}

void MSLayoutManager::down(void)
{
  MSWidget *focusWidget = inputFocus();
  MSLayoutEntry *entry = getEntry(focusWidget);
  if (entry)
   {
     MSLayoutEntry *next = getNextVerticalEntry(entry);
     if (entry!=next && next!= 0)
       (void) setFocus(next->widget());
   }
}

void MSLayoutManager::left(void)
{
  MSWidget *focusWidget = inputFocus();
  MSLayoutEntry *entry = getEntry(focusWidget);
  if (entry)
   {
     MSLayoutEntry *next = getPrevHorizontalEntry(entry);
     if (entry!=next && next!= 0)
       (void) setFocus(next->widget());
   }
}

void MSLayoutManager::right(void)
{
  MSWidget *focusWidget = inputFocus();
  MSLayoutEntry *entry = getEntry(focusWidget);
  if (entry)
   {
     MSLayoutEntry *next = getNextHorizontalEntry(entry);
     if (entry!=next && next!= 0)
       (void) setFocus(next->widget());
   }
}

// Sorting routines

inline void swap(MSLayoutEntry **a_,int i_,int j_)
{ MSLayoutEntry *t=a_[i_];a_[i_]=a_[j_],a_[j_]=t; }
 
void rowSort(MSLayoutEntry **a_,int n_)
{
  int i,j,min,n=n_;
  for (i=0;i<n;i++)
   {
     min=i;
     for (j=i+1;j<n;j++) { if (a_[j]->at().row()<a_[min]->at().row()) min=j; }
     swap(a_,min,i);
   }
}

void columnSort(MSLayoutEntry **a_,int n_)
{
  int i,j,min,n=n_;
  for (i=0;i<n;i++)
   {
     min=i;
     for (j=i+1;j<n;j++) { if (a_[j]->at().column()<a_[min]->at().column()) min=j; }
     swap(a_,min,i);
   }
}



MSLayoutEntry * MSLayoutManager::getNextVerticalEntry(MSLayoutEntry *entry_)
{
  MSLayoutEntry *entry;
  MSLayoutEntry *next = 0;
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  int 		 r = entry_->at().row() + entry_->at().rowSpan();
  int 		 c = entry_->at().column();
  int            i=0, numrows=0;
  
  if (r==rows()) r = 0;
  if (rows()>1)
   {
     MSLayoutEntry **entries = new MSLayoutEntry*[rows()];
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();
	if (entry!=entry_&&entry->at().column()==c) entries[i++]=entry;
      }
     numrows=i;
     rowSort(entries, numrows);
     for (i=0; i<numrows;i++)
      {
	entry=entries[i];
	if (r >= entry->at().row() && r <= entry->at().row() + entry->at().rowSpan())
	 {
	   if (entry->widget()->isProtected()==MSFalse) { next = entry; break; }
	   else { r+=entry->at().rowSpan(); if (r==rows()) r=0; }
	 }
      }
     for (i=0; i < numrows; i++) entries[i]=0;
     delete [] entries;
   }
  return next;
}
MSLayoutEntry * MSLayoutManager::getPrevVerticalEntry(MSLayoutEntry *entry_)
{
  MSLayoutEntry *entry;
  MSLayoutEntry *next = 0;
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  int 		 r = entry_->at().row() - 1;
  int 		 c = entry_->at().column();
  int            i=0, numrows=0;
  
  r = (r<0)? rows()-1:r;
  if (rows()>1)
   {
     MSLayoutEntry **entries = new MSLayoutEntry*[rows()];
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();
	if (entry!=entry_&&entry->at().column()==c) entries[i++]=entry;
      }
     numrows=i;
     rowSort(entries, numrows);
     for (i=numrows-1; i>=0;i--)
      {
	entry=entries[i];
	if (r >= entry->at().row() && r <= entry->at().row() + entry->at().rowSpan())
	 {
	   if (entry->widget()->isProtected()==MSFalse) { next = entry; break; }
	   else { r--; r=(r<0)?rows()-1:r; }
	 }
      }
     for (i=0; i < numrows; i++) entries[i]=0;
     delete [] entries;
   }
  return next;
}

MSLayoutEntry * MSLayoutManager::getNextHorizontalEntry(MSLayoutEntry *entry_)
{
  MSLayoutEntry *entry;
  MSLayoutEntry *next = 0;
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  int 		 r = entry_->at().row();
  int 		 c = entry_->at().column() + entry_->at().columnSpan();
  int            i=0, numcols=0;
  
  if (c==columns()) c=0;
  if (columns()>1)
   {
     MSLayoutEntry **entries = new MSLayoutEntry*[columns()];
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();
	if (entry!=entry_&&entry->at().row()==r) entries[i++]=entry;
      }
     numcols=i;
     columnSort(entries, numcols);
     for (i=0; i<numcols; i++)
      {
	entry=entries[i];
	if (c >= entry->at().column() && c <= entry->at().column() + entry->at().columnSpan())
	 {
	   if (entry->widget()->isProtected()==MSFalse) { next = entry; break; }
	   else { c+=entry->at().columnSpan(); if (c==columns()) c=0; }
	 }
      }
     for (i=0; i < numcols; i++) entries[i]=0;
     delete [] entries;
   }
  return next;
}

MSLayoutEntry * MSLayoutManager::getPrevHorizontalEntry(MSLayoutEntry *entry_)
{
  MSLayoutEntry *entry;
  MSLayoutEntry *next = 0;
  MSNodeItem    *hp=mappedListHead();
  MSNodeItem    *np=hp;
  int 		 r = entry_->at().row();
  int 		 c = entry_->at().column()-1;
  int            i=0, numcols=0;
  
  c=(c<0)?columns()-1:c;
  if (columns()>1)
   {
     MSLayoutEntry **entries = new MSLayoutEntry*[columns()];
     while ((np=np->next())!=hp)
      {
	entry=(MSLayoutEntry *)np->data();
	if (entry!=entry_&&entry->at().row()==r) entries[i++]=entry;
      }
     numcols=i;
     columnSort(entries, numcols);
     for(i=numcols - 1; i >= 0; i--)
      {
	entry=entries[i];
	if (c >= entry->at().column() && c <= entry->at().column() + entry->at().columnSpan())
	 {
	   if (entry->widget()->isProtected()==MSFalse) { next = entry; break; }
	   else { c--; c=(c<0)?columns()-1:c; }
	 }
      }
     for (i=0; i < numcols; i++) entries[i]=0;
     delete [] entries;
   }
  return next;
}

// #########################################################
// default virtual methods - prevents gratuitous inlining
// #########################################################

void MSLayoutManager::configure(void) {}
void MSLayoutManager::computeSize(void)
{
  drawBackground();
  placement();
  drawShadow();  
}

// #########################################################
// MSLayoutEntry
// #########################################################

MSLayoutEntry::MSLayoutEntry(MSWidget *widget_) :
_widget(widget_),_mapped(MSFalse)
{}
MSLayoutEntry::~MSLayoutEntry(void)
{}

MSWidget *MSLayoutEntry::widget(void) const
{ return _widget; }
MSBoolean MSLayoutEntry::mapped(void) const
{ return _mapped; }
At& MSLayoutEntry::at(void)
{ return _at; }
const At& MSLayoutEntry::at(void) const
{ return _at; }
void MSLayoutEntry::mapped(MSBoolean b_)
{ _mapped=b_; }


MSLayoutVector::MSLayoutVector(void) :
_mask(0),_value(0)
{}
MSLayoutVector::~MSLayoutVector(void)
{}

unsigned long MSLayoutVector::mask(void) const
{ return _mask; }
int MSLayoutVector::value(void) const
{ return _value; }	   

void MSLayoutVector::mask(unsigned long mask_)
{ _mask=mask_; }
void MSLayoutVector::value(int value_)
{ _value=value_; }

