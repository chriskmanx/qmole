///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusButtonBox.H>
#include <AplusGUI/AplusCheckBox.H>

extern MSBoolean isSlotFiller(A);
extern long dbg_tmstk;

////////////////////////////////////////////////////////////////////////////////

AplusActionButton::AplusActionButton(MSWidget *xwin_,  const char *title_) : MSActionButton(xwin_, title_)
{
  dynamic(MSTrue);	// make it resize on changes (font changes, etc.)
  highlightColor(AVariableData::defaultHlColor());
}

AplusActionButton::~AplusActionButton(void)
{}

void AplusActionButton::focusIn(void)
{
  if (buttonBox())
    buttonBox()->selectedItem(buttonBox()->buttons().indexOf((unsigned long)(MSWidget *)this));
  MSActionButton::focusIn();
}


MSBoolean AplusActionButton::isProtected(void) const
{
  if (buttonBox()->readOnly(buttonBox()->buttons().indexOf((unsigned long)(MSWidget *)this))==MSFalse &&
      sensitive()==MSTrue)
    {
      return MSFalse;
    }
  else	// readOnly()==MSTrue || sensitive()==MSFalse
    {
      return MSTrue;
    }
}

////////////////////////////////////////////////////////////////////////////////

AplusButtonBox::AplusButtonBox(MSWidget *xwin_) : MSActionBox(xwin_)
{
  _selectedItem=-1;
  _geometry=aplus_nl;
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusButtonBox::~AplusButtonBox(void)
{ if (geometry()!=0) dc(geometry());}

void AplusButtonBox::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *) m_));
}

void AplusButtonBox::receiveEvent(MSEvent& event_)
{
  const MSSymbol& eventType=event_.type();

  if (eventType==AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusButtonBox"  << endl;
      AplusEvent *ave = (AplusEvent *) &event_;
      V v     = ((AplusModel *)model())->aplusVar();
      A index = ave->index();
      A pick  = ave->pick();
      I ravel = ave->ravel();;
      update(v,index, pick, ravel);
    }
  else if (eventType==AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusButtonBox"  << endl;
      
      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  else if (eventType==AplusUpdateTitleEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateTitleEvent in AplusButtonBox"  << endl;
      updateTitle();
    }
}


MSBoolean AplusButtonBox::verifyData(V,A a_)
{ return isSlotFiller(a_); }

extern "C" A gpix(A,A);
void AplusButtonBox::update(V v_,A,A pick_,I)
{ 
  V oldV = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  if (pick_!=0)
   {
     if (QA(pick_))
      {
	A pick=(A)gpix(pick_,(A)v_->a);
	if (pick!=0&&QA(pick)&&pick->t==It&&pick->r<=1)
	 {
	   if (pick->r==1)
	    {
	      if (pick->n>0) for (int i=0;i<pick->n;i+=2)updateValue((int)pick->p[i+1]); 
	      else updateValues();
	    }
	   else 
	    {
	      if (pick->p[0]==1) updateValues();
	      else if (v_==oldV) updateData();
	    }
	 }
	else if (pick==0) cerr<<"action: pick assignment error in update."<<endl;
        else cerr<<"action: pick assignment error in update."<<endl;
	if (pick!=0) dc(pick);
      }
     else cerr<<"action: pick assignment error in update."<<endl;
   }
  else if (v_==oldV) updateData();
}

void AplusButtonBox::updateValue(int row_)
{
  MSWidget *child=children()(row_);
  if (child) child->redraw();
}

void AplusButtonBox::updateValues(void)
{
  if (model() && ((AplusModel*)model())->aplusVar()!=0)
   {
     int nr=numRows();
     for(int i=0;i<nr;i++) updateValue(i);
   }
}

int AplusButtonBox::numRows(void) 
{ int r=0;
  if (model() && ((AplusModel*)model())->aplusVar()!=0)
   { P p; p.i=((AplusModel*)model())->data();
     A p0=p.a[0];
     r=(int)p0->n;
   }
  return r;
}

MSActionButton *AplusButtonBox::newButton(void)
{
  return new AplusActionButton((MSWidget *)this,"");
}


void AplusButtonBox::updateData(void)
{
  if (model() && ((AplusModel*) model())->aplusVar())
   {
     int             nr=numRows();
     MSActionButton  *btn;
     A               str;
     MSBoolean       doMap;
     MSBoolean       change=(childCount()==nr)?MSFalse:MSTrue;

     freeze();

     // Set Managed WidgetVector size
     for (;buttons().length() < nr; buttons()<<(unsigned long)0);
     
     // remove extra buttons 
     if (childCount()-nr>0)
      {
	int extra = childCount()-nr;
	for(int i = nr; i < extra+nr; i++)
	{
	  ((MSWidget *)buttons()(i))->destroy();
	  buttons()[i] = 0;
	}
      }

     P p; p.i=((AplusModel*)model())->data();
     A val=(A)p.a[1];
     P pv; pv.i=val->p;

     for (int r=0;r<nr;r++)
      {
	doMap=MSFalse;
	if (r<buttons().length() && buttons()(r)!=0)
	   btn=(MSActionButton *) (MSWidget *)buttons()(r);
	else
	 {
	   btn = newButton();
	   buttons()[r]=(unsigned long)(MSWidget *)btn;
	   doMap=MSTrue;
	 }

	btn->font(titleFont(r));
	btn->foreground(titleColor(r));
	if (widgetType()==AplusCheckBox::symbol()) ((MSToggleButton *)btn)->selectColor(color(r));

	str = itemLabel(r);
	if (isNull(str)==MSFalse)
         {
           btn->label((char *)str->p);
           dc(str);
         }
	setButtonState(btn,*pv.a[r]->p);
	btn->integerTag(r);
	if (doMap==MSTrue && btn->mapped()==MSFalse) btn->map();
      }
     if (change==MSTrue)  naturalSize();
     unfreeze();
   }
}

A AplusButtonBox::itemLabel(int row_)
{
  A outStr=aplus_nl;
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  int n=(model()==0)?0:((AplusModel*)model())->numElmts();
  if (v!=0&&n==2)
   {
     AOutFunction *titleFunc=AplusModel::getTitleFunc(v);
     if (titleFunc!=0)
      {
	P p; p.i=((AplusModel*)model())->data();
	A as=p.a[0];
	A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
	 {
	   A attr=(A)as->p[row_];  // used as the pick
	   A val =(A)av->p[row_];
	   outStr=(A) titleFunc->invoke(v,(A)val,(A)attr);   
           outStr =(outStr->t==Ct)?outStr:aplus_nl;
	 }
      }
   }
  return outStr;
}

Font AplusButtonBox::titleFont(int row_)
{
  Font fid=font();
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  int n=(model()==0)?0:((AplusModel*)model())->numElmts();

  if (v!=0&&n==2);
   {
     AFontFunction *titleFontFunc=AplusModel::getTitleFontFunc(v);
     if (titleFontFunc!=0)
      {
        P p; p.i=((AplusModel*)model())->data();
        A as=p.a[0];
        A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
	 {
	   A attr=(A)as->p[row_];  // used as the pick
	   A val =(A)av->p[row_];
	   fid=(Font) titleFontFunc->invoke(v,(A)val,(A)attr);
	 }
      }
   }
  return fid;
}

void AplusButtonBox::activate(void)
{
  MSActionButton *entry=activeButton();
  unsigned pos = buttons().indexOf((unsigned long)(MSWidget*)entry);

  if (entry!=0)
   {
     
     P p; p.i=((AplusModel*)model())->data();
     A as=p.a[0];
     A attr=(A)as->p[pos];  // used as the pick
     A val=p.a[1];
     A pick=(A)gs(Et);
     *pick->p=(I)attr;
     A data=(A)val->p[pos];
     (void)callAset(((AplusModel*)model())->aplusVar(),(A)ic(data),0,pick);
     dc(pick);
   }
}

int AplusButtonBox::callAset(V v_,A d_,A i_,A p_)
{
  extern int safeAset(V,A,A,A);
  int r;
  busyOn();
  d_=(A)ic(d_);
  if ((r=safeAset(v_,d_,i_,p_))==0) showError(qs);
  else AplusModel::doneCB(v_,d_,i_,p_);
  dc(d_);
  busyOff();	
  return r;
}


void AplusButtonBox::updateTitle(void)
{
  int nr = childCount();
  MSBoolean changed = MSFalse;
  A str;

  if (model() && ((AplusModel*) model())->aplusVar())
   {
     for(int i=0; i < nr; i++)
      {
	MSActionButton *btn = (MSActionButton *)(MSWidget *)buttons()(i);
	if(btn)
	  {
	    str = itemLabel(i);
	    if (isNull(str)==MSFalse)
	      {
		if (btn->label()!=MSStringVector((char *) str->p))
		  {
		    changed = MSTrue;
		    btn->label((char *)str->p);
		  }
		dc(str);
	      }
	    Font fid = titleFont(i);
	    if (btn->font()!=fid)
	      {
		changed = MSTrue;
		btn->font(fid);
	      }
	    btn->foreground(titleColor(i));
	  }
      }
     if (changed) naturalSize();
   }
  MSActionBox::updateTitle();
}

void AplusButtonBox::naturalSize(void)
{
  freeze();
  setGeometry();
  MSActionBox::naturalSize();
  unfreeze();
}


unsigned long AplusButtonBox::titleColor(int row_)
{
  unsigned long fg=foreground();

  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  int n=(model()==0)?0:((AplusModel*)model())->numElmts();
  if (v!=0&&n==2)
   {
     AColorFunction *titleColorFunc=AplusModel::getTitleColorFunc(v);
     if (titleColorFunc!=0)
      {
        P p; p.i=((AplusModel*)model())->data();
        A as=p.a[0];
        A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
         {
	   A attr=(A)as->p[row_];  // used as the pick
	   A val =(A)av->p[row_];
	   fg=(unsigned long) titleColorFunc->invoke(v,(A)val,(A)attr);
	 }
      }
   }
  return fg;
}

MSBoolean AplusButtonBox::readOnly(int row_) const
{
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  int n=(model()==0)?0:((AplusModel*)model())->numElmts();
  AVariableData *varData = pAVarDataFromV(v);  
  AReadOnlyFunction *roFunc=AplusModel::getReadOnlyFunc(v);
  MSBoolean ro=(varData!=0)?varData->readOnly():MSFalse;

  if (roFunc!=0&&n==2)
   {
     P p; p.i=((AplusModel*)model())->data();
     A as=p.a[0];
     A av=p.a[1];
     if (row_>=0&&row_<(int)av->n)
      {
	A attr=(A)as->p[row_];  // used as the pick
	A val =(A)av->p[row_];
	ro=(MSBoolean) roFunc->invoke(v,(A)val,(A)attr);
      }
   }
  return ro;
}

// compute the least common multiple of x and y
// from: Wirth,Programming in MODULA-2
int lcm(int x_,int y_)
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
int lcm (A span_)
{
  int n=(int)span_->n;
  int i=0;
  P p; p.i=span_->p;
  int x=((int)p.i[0]>0)?(int)p.i[0]:1;
  for (i=0;i<n-1;i++) x=lcm(x,((int)p.i[i+1]>0)?(int)p.i[i+1]:1);
  return x;
}


MSBoolean AplusButtonBox::setGeometry(void)
{
  
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  A a=(model()==0)?0:((AplusModel*)model())->a();
  if (v)
   {
     AGeometryFunction *geoFunc=AplusModel::getGeometryFunc(v);
     if (geoFunc!=0&&numRows()>0)
      {
	A ag=(A)geoFunc->invoke(v,a);
	if (isNull(ag)==MSFalse && ag->t==It)
	 {
           if (compareGeometry(ag)==MSTrue) dc(ag);
           else 
            {
	      geometry(ag); 
              (void) setPositions();
              return MSTrue;
            }	      
	 }
      }
   }
  return MSFalse;
}

MSBoolean AplusButtonBox::compareGeometry(A ag_)
{
  A cg=geometry();
  if (cg->t==It)
   {
     if (cg->t==ag_->t&&cg->n==ag_->n&&cg->r==ag_->r)
      {
        for (int r=0;r<cg->r;r++)
	 {
           if (cg->d[r]!=ag_->d[r]) return MSFalse;
	 }
        for (int i=0;i<cg->n;i++)
	 {
           if (cg->p[i]!=ag_->p[i]) return MSFalse;
	 }
        return MSTrue;
      }
     else return MSFalse;
   }
  else return MSFalse;
}


MSBoolean AplusButtonBox::setPositions(void)
{
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  A a=(model()==0)?0:((AplusModel*)model())->a();
  MSBoolean change=MSFalse;
  if (v!=0)
   {
     AGeometryFunction *geoFunc=AplusModel::getGeometryFunc(v);
     if (geoFunc!=0&&numRows()>0)
      {
       P p; p.i=((AplusModel*)model())->data();
       A ag=(A)geoFunc->invoke(v,a);
       if (isNull(ag)==MSFalse && ag->t==It)
	 {
	   P pg; pg.i=ag->p;
	   A am=ag;
	   P pm;
	   int r=0;
	   int c=0;
	   int nRows=0;
	   int nCols=0;
	   
	   if (ag->r<=1) // vector or scalar form else canonical - matrix
	    {
	      if (ag->r==0)
	       {
		 nRows=1;
		 nCols=(int)pg.i[0];	
	       }
	      else
	       {
		 nRows=(int)ag->n;
		 nCols=lcm(ag);
	       }
	      int w=nCols;
	      int span=0;
	      int i=0;
	      int index=0;
	      am=gm(It,nRows,nCols);
	      P pm; pm.i=am->p;
	      for (r=0;r<nRows;r++)
	       {
		 span=(int) nCols/(((int)pg.i[r]>0)?(int)pg.i[r]:1);
		 for (c=0;c<nCols;c+=span)
		  {
		    for (i=0;i<span;i++) pm.i[r*w+(c+i)]=index;
		    index++;
		  }
	       }
	    }
	   pm.i=am->p;
	   int row=0;
	   int col=0;
	   int hspan=0;
	   int vspan=0;
           int ax,ay,v_span,h_span;
	   MSBoolean mstat;
	   
           int nr=numRows();

	   MSActionButton *btn;
	   for (r=0;r<nr;r++)
	    {
	      rowSpan(r,am,&row,&vspan);
	      colSpan(r,am,&col,&hspan);

	      btn=(MSActionButton *)buttons()(r);
	      if (btn!=0)
	       {
		 At btnGeo = btn->at();
                 ax=btnGeo.row();
                 ay=btnGeo.column();
                 h_span=btnGeo.columnSpan();
                 v_span=btnGeo.rowSpan();
                 mstat=btn->mapped();

		 btn->at(At(row, col, vspan, hspan, btn->resizeConstraints()));	// preserve the constraints
		 if (hspan==0||vspan==0)
		  {
		    btn->unmap();
		    btn->at(At(row, col, 1, 1, btn->resizeConstraints())); // preserve the constraints
		  }
		 else if (btn->mapped()==MSFalse) btn->map();
                 if(ax 	  !=btn->at().row()        ||
		    ay 	  !=btn->at().column()     ||
		    h_span!=btn->at().columnSpan() ||
		    v_span!=btn->at().rowSpan()    ||
                    mstat !=btn->mapped())
		 change=MSTrue;
	       }
	    }
	   if (ag->r<=1) dc(am);			 
	 }
	dc(ag);
      }
   }
  return change;
}

void AplusButtonBox::rowSpan(int index_,A am_,int *row_,int *span_)
{
  int r=0;
  int c=0;
  int w=(int)am_->d[1];
  int span=0;
  int row=0;
  P p; p.i=am_->p;
  
  if (am_->r==2)
   {
     for (c=0;c<am_->d[1]&&span==0;c++)
      {
	for (r=0;r<am_->d[0];r++)
	 {
	   if (p.i[r*w+c]==index_) 
	    {
	      if (span==0) row=r;
	      span++;          
	    }
	 }
      } 
   }  
  *row_=row;
  *span_=span;
}

void AplusButtonBox::colSpan(int index_,A am_,int *col_,int *span_)
{
  int r=0;
  int c=0;
  int w=(int)am_->d[1];
  int span=0;
  int col=0;
  P p; p.i=am_->p;
  
  if (am_->r==2)
   {
     for (r=0;r<am_->d[0]&&span==0;r++)
      {
	for (c=0;c<am_->d[1];c++)
	 {
	   if (p.i[r*w+c]==index_) 
	    {
	      if (span==0) col=c;
	      span++;          
	    }
	 }
      } 
   }  
  *col_=col;
  *span_=span;
}


void AplusButtonBox::valueChange(MSWidget *btn_,MSBoolean value_)
{
  unsigned index = buttons().indexOf((unsigned long)btn_);
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();

  if (btn_)
   {
     P p; p.i=((AplusModel *)model())->data();
     A as=p.a[0];
     A attr=(A)as->p[index];  // used as the pick
     A pick=(A)gs(Et);
     *pick->p=(I)attr;
     A sd=gi((int)value_);
     (void)callAset(v,sd,0,pick);
     dc(pick);
   }
}

unsigned long AplusButtonBox::color(unsigned row_)
{
  unsigned long fg=foreground();

  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  int n=(model()==0)?0:((AplusModel*)model())->numElmts();
  if (v!=0&&n==2)
   {
     AColorFunction *fgFunc=AplusModel::getFgFunc(v);
     if (fgFunc!=0)
      {
        P p; p.i=((AplusModel*)model())->data();
        A as=p.a[0];
        A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
         {
	   A attr=(A)as->p[row_];  // used as the pick
	   A val =(A)av->p[row_];
	   fg=(unsigned long) fgFunc->invoke(v,(A)val,(A)attr);
	 }
      }
   }
  return fg;
}


void AplusButtonBox::buttonHighlightThickness(int t_)
{
  unsigned numButtons=buttons().length();
  if (numButtons>0)
    {
      AplusActionButton *btn;

      for (int i=0; i<numButtons; i++)
	{
	  btn=(AplusActionButton *)(MSWidget *)buttons()(i);
	  if (btn)
	    btn->highlightThickness(t_);
	}
    }
}


void AplusButtonBox::buttonShadowThickness(int t_)
{
  unsigned numButtons=buttons().length();
  if (numButtons>0)
    {
      AplusActionButton *btn;
      
      for (int i=0; i<numButtons; i++)
	{
	  btn=(AplusActionButton *)(MSWidget *)buttons()(i);
	  if (btn)
	    btn->shadowThickness(t_);
	}
    }
}


void AplusButtonBox::highlightColor(unsigned long hlc_)
{
  if (buttons().length()>0)
    {
      if (highlightColor()!=hlc_)
	{
	  AplusActionButton *btn;
	  unsigned numButtons=buttons().length();

	  for (int i=0; i<numButtons; i++)
	    {
	      btn=(AplusActionButton *)(MSWidget *)buttons()(i);
	      if (btn)
		btn->highlightColor(hlc_);
	    }
	}
    }

  MSActionBox::highlightColor(hlc_);
}


int AplusButtonBox::buttonHighlightThickness(void)
{
  if (buttons().length()>0)
    {
      MSWidgetOutput *btn;
      btn=(MSWidgetOutput *)(MSWidget *)buttons()(0);
      if (btn)
	return btn->highlightThickness();
    }
  return MSActionBox::highlightThickness();
}


int AplusButtonBox::buttonShadowThickness(void)
{
  if (buttons().length()>0)
    {
      MSWidgetOutput *btn;
      btn=(MSWidgetOutput *)(MSWidget *)buttons()(0);
      if (btn)
	return btn->shadowThickness();
    }
  return MSActionBox::shadowThickness();
}


unsigned long AplusButtonBox::highlightColor(void)
{
  return MSActionBox::highlightColor();
}


void AplusButtonBox::updateSensitivity(void)
{
  unsigned numButtons=buttons().length();
  if (numButtons>0)
    {
      AplusActionButton *btn;
      MSBoolean sensitivity=sensitive();
      
      for (int i=0; i<numButtons; i++)
	{
	  btn=(AplusActionButton *)(MSWidget *)buttons()(i);
	  if (btn)
	    btn->sensitive(sensitivity);
	}
    }
}


const MSSymbol& AplusButtonBox::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusButtonBox::symbol(void)
{
//   static MSSymbol sym("AplusButtonBox");
//   return sym;
  return  MSActionBox::symbol();
}
