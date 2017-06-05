///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSGUI/MSShadow.H>
#include <AplusGUI/AplusSlot.H>
#include <AplusGUI/AplusShell.H>

extern int lcm(int,int);
extern int lcm (A);
extern MSBoolean isSlotFiller(A);
extern A convertToPixels(const MSWidgetCommon *, A);
extern long dbg_tmstk;

const unsigned long AplusSlotDefaultCycleInterval=1000;

AplusSlotEntryField::AplusSlotEntryField(MSWidget *owner_) 
  : MSStringEntryField(owner_)
{ 
  acceptTab(MSTrue);
  highlightColor(AVariableData::defaultHlColor());
  dynamic(MSTrue);
  supportPasting(MSFalse);
}


AplusSlotEntryField::~AplusSlotEntryField(void)
{
}


MSBoolean AplusSlotEntryField::validate(const char *string_)
{
  return slot()->validate(this, string_);
}


//  Need to override updateForeground so the label does not change color
//
void AplusSlotEntryField::updateForeground(unsigned long oldfg_)
{
  MSComposite::updateForeground(oldfg_);
  _fieldValue->foreground(foreground());
  if (oldfg_==editorBackground()) editorBackground(foreground());
  redraw();
}


void AplusSlotEntryField::up(void)
{
  if (_editor->mapped()==MSTrue)
   {
     if (activateEditor()==MSTrue)
      {
	activateCallback(MSWidgetCallback::activate);
	slot()->up();
      }
   }
  else slot()->up();
}


void AplusSlotEntryField::down(void)
{
  if (_editor->mapped()==MSTrue)
   {
     if (activateEditor()==MSTrue)
      {
	activateCallback(MSWidgetCallback::activate);
	slot()->down();
      }
   }
  else slot()->down();
}

void AplusSlotEntryField::left(void) { slot()->left(); }
void AplusSlotEntryField::right(void) { slot()->right(); }

void AplusSlotEntryField::focusIn(void)
{
  if (slot())
    {
      unsigned long cFld=slot()->fields().indexOf((unsigned long)(MSWidget *)this);
      if (cFld!=slot()->selectedItem())
	{
	  slot()->selectedItem(cFld);
	  activateCallback(slot(),MSWidgetCallback::selection);
	}
      else
	slot()->selectedItem(cFld);
    }

  MSStringEntryField::focusIn();
}


void AplusSlotEntryField::keyPress(const XEvent *event_, KeySym keysym_, unsigned int state_, const char *pString_)
{
  MSStringEntryField::keyPress(event_, keysym_, state_, pString_);
  if (sensitive()==MSTrue && _editor->mapped()==MSFalse)
   {
     // Normally the up/down cursor keys skip over protected fields, however
     // if one is selected with the mouse the following allows the 
     // currsor keys to move off the protected field
     // In version 2 you can't select a protected field with the mouse
     if (isProtected()==MSTrue)
       {
	 if (keysym_==XK_Up) slot()->up();
	 else if (keysym_==XK_Down) slot()->down();
       }

     if (keysym_==XK_Tab)
      {
	unsigned int state=(state_&(ShiftMask|ControlMask|Mod1Mask));
	if (state==ShiftMask) slot()->shiftTab();
	else slot()->tab();
      }
   }
}


MSBoolean AplusSlotEntryField::isProtected(void) const
{
  return slot()->readOnly(slot()->fields().indexOf((unsigned long)(MSWidget *)this));
}


void AplusSlotEntryField::increment(void)
{
  if (slot()->callback(MSWidgetCallback::increment)!=0)
    {
      slot()->activateCallback(MSWidgetCallback::increment);
    }
  else
    {
      slot()->activateCallback(MSWidgetCallback::reference);
    }
}


void AplusSlotEntryField::decrement(void)
{
  if (slot()->callback(MSWidgetCallback::decrement)!=0)
    {	
      slot()->activateCallback(MSWidgetCallback::decrement);
    }
  else
    {
      slot()->activateCallback(MSWidgetCallback::reference);
    }
}


void AplusSlotEntryField::label(const char *pString_)
{
  unsigned vw=fieldValue()->width();
  MSStringEntryField::label(pString_);
  fieldValue()->width(vw);
}

  
///////////////////////////////////////////////////////////////////////////////

AplusSlot::AplusSlot(MSWidget *owner_) : MSCompositeFieldBox(owner_)
{
  _cycleColors=aplus_nl;
  _cycleInterval=AplusSlotDefaultCycleInterval;
  _geometry = aplus_nl;
  _selectedItem = -1;
  _arrowButtons = aplus_nl;

  acceptTab(MSTrue);
  childInFocus(MSFalse);
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);

  backingStore(WhenMapped);
}

AplusSlot::~AplusSlot(void)
{
  removeAllCycles();
  if (isNull(_cycleColors)==MSFalse)  dc(_cycleColors);
  if (isNull(geometry())==MSFalse) dc(geometry());
}

MSBoolean AplusSlot::freezeIfUnfrozen(void)
{
  MSBoolean rc=frozen(); freeze(); return rc;
}

void AplusSlot::unfreezeIfUnfrozen(MSBoolean bool_) 
{
  if(MSFalse==bool_)unfreeze();
}

void AplusSlot::updateFunctionalAttributes(int row_)
{
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  AplusSlotEntryField *fld = (AplusSlotEntryField *)(MSWidget *)fields()(row_);
  if (fld)
  {
    AColorFunction *fgFunc=AplusModel::getFgFunc(v);
    if (fgFunc!=0) fld->foreground(color(row_));

    fgFunc=AplusModel::getTitleColorFunc(v);
    if (fgFunc!=0) fld->labelForeground(titleColor(row_));

    AFontFunction *fontFunc=AplusModel::getFontFunc(v);
    if (fontFunc!=0) fld->font(valueFont(row_));

    fontFunc=AplusModel::getTitleFontFunc(v);
    if (fontFunc!=0) fld->labelFont(titleFont(row_));
  }
}

void AplusSlot::updateData(void)
{
  removeAllCycles();
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v)
   {
     MSBoolean wasFrozen=freezeIfUnfrozen();

     int                     nr=numRows();     
     AplusSlotEntryField    *fld=0;
     A                	     str;
     MSBoolean       	     change=(childCount()==nr)?MSFalse:MSTrue;

     // Set Managed WidgetVector size
     for (;fields().length() < nr; fields()<<(unsigned long)0);

     // remove extra buttons
     if (childCount()-nr>0)
      {
	int extra = childCount()-nr;
	for(int i = nr; i < extra+nr; i++)
	{
	  ((MSWidget *)fields()(i))->destroy();
	  fields()[i] = 0;
	}
	fields().reshape(nr);
      }
     
     MSBoolean doMap;
     unsigned vallen=valueLength();
     for (int r=0;r<nr;r++)
      {
        doMap=MSFalse;
	if (r<fields().length() && fields()(r)!=0)
	   fld=(AplusSlotEntryField *) (MSWidget *)fields()(r);
	else
	 {
	   fld = new AplusSlotEntryField(this);
	   fields()[r]=(unsigned long)(MSWidget *)fld;
	   doMap=MSTrue;
	 }

	if(0!=vallen)
	{
	  if(vallen!=fld->valueWidth())
	  {
	    fld->valueWidth(vallen);
	    change=MSTrue;
	  }
	}
	fld->resizeConstraints(At::Top|At::MaintainHeight);
	str = itemLabel(r);
	if (isNull(str)==MSFalse && str->t==Ct)
         {
           fld->label((char *)str->p);
           dc(str);
         }

	str = itemValue(r);
	if (isNull(str)==MSFalse)
         {
           fld->value((char *)str->p);
           dc(str);
         }
	updateFunctionalAttributes(r);
	if (doMap==MSTrue&&fld->mapped()==MSFalse) fld->map();
      }
     if (change==MSTrue) naturalSize();
     unfreezeIfUnfrozen(wasFrozen);
   }
}

void AplusSlot::updateValue(int row_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  if (v!=0&&row_<numRows()) 
   {
     AplusSlotEntryField *fld;
     A str;

     fld = (AplusSlotEntryField *) (MSWidget *)fields()(row_);
     if (fld)
      {
	str = itemValue(row_);
	if (isNull(str)==MSFalse)
         {
           (void)fld->value((char *)str->p);
	   dc(str);
         }

	str = itemLabel(row_);
        if(isNull(str)==MSFalse) fld->label((char *)str->p),dc(str);
	updateFunctionalAttributes(row_);
      }
   }
}

void AplusSlot::updateValues(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v)
   { 
     AplusSlotEntryField *fld;
     A str;
     
     for (int i = 0; i < fields().length(); i++)
      {
	fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	str = itemValue(i);
	if (isNull(str)==MSFalse)
         {
           (void)fld->value((char *)str->p);
           dc(str);
         }
      }
   }
}

void AplusSlot::updateFont(Font)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v!=0)
   { 
     AplusSlotEntryField *fld;
     MSBoolean wasFrozen=freezeIfUnfrozen();
     for (int i = 0; i < fields().length(); i++)
      {
	fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	fld->font(valueFont(i));
      }
     unfreezeIfUnfrozen(wasFrozen);
   }
}

void AplusSlot::updateSensitivity(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v!=0)
   { 
     AplusSlotEntryField *fld;
     MSBoolean wasFrozen=freezeIfUnfrozen();
     for (int i = 0; i < fields().length(); i++)
      {
	fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	fld->sensitive(sensitive());
      }
     unfreezeIfUnfrozen(wasFrozen);
   }
}

void AplusSlot::highlightThickness(int t_)
{
  if (fields().length()>0)
   {
     if (highlightThickness()!=t_)
      {
	AplusSlotEntryField *fld;
	MSBoolean wasFrozen=freezeIfUnfrozen();
	for (int i = 0; i < fields().length(); i++)
	 {
	   fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	   if (fld->highlightThickness()!=t_) fld->highlightThickness(t_);
	 }
	unfreezeIfUnfrozen(wasFrozen);
	if(MSFalse==wasFrozen)redraw();
      }
   }
}

void AplusSlot::shadowThickness(int t_)
{
  if (fields().length()>0)
   {
     if (shadowThickness()!=t_)
      {
	AplusSlotEntryField *fld;
	MSBoolean wasFrozen=freezeIfUnfrozen();
	for (int i = 0; i < fields().length(); i++)
	 {
	   fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	   if (fld->shadowThickness()!=t_) fld->shadowThickness(t_);
	 }
	unfreezeIfUnfrozen(wasFrozen);
	if(MSFalse==wasFrozen)redraw();
      }
   }
}

void AplusSlot::highlightColor(unsigned long hlc_)
{
  if (fields().length()>0)
   {
     if (highlightColor()!=hlc_)
      {
	AplusSlotEntryField *fld;
	MSBoolean wasFrozen=freezeIfUnfrozen();
	for (int i = 0; i < fields().length(); i++)
	 {
	   fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	   if (fld->highlightColor()!=hlc_) fld->highlightColor(hlc_);
	 }
	unfreezeIfUnfrozen(wasFrozen);
	if(MSFalse==wasFrozen)redraw();
      }
   }
  MSCompositeFieldBox::highlightColor(hlc_);
}

int AplusSlot::highlightThickness(void)
{
  if (fields().length()>0)
   {
     AplusSlotEntryField *fld;
     fld=(AplusSlotEntryField *) (MSWidget *) fields()(0);
     return fld->highlightThickness();
   }
  else return MSCompositeFieldBox::highlightThickness();
}

int AplusSlot::shadowThickness(void)
{
  if (fields().length()>0)
   {
     AplusSlotEntryField *fld;
     fld=(AplusSlotEntryField *) (MSWidget *) fields()(0);
     return fld->shadowThickness();
   }
  else return MSCompositeFieldBox::shadowThickness();
}

unsigned long AplusSlot::highlightColor(void)
{
  return MSCompositeFieldBox::highlightColor();
}


void AplusSlot::firstMapNotify(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  int i;
  unsigned max = 9;
  unsigned vallen=valueLength();
  unsigned len;
  
  if (v)
   { 
     AplusSlotEntryField    *fld;
     A                str;
     
     for (i = 0; i < fields().length(); i++)
      {
	fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	str = itemValue(i);
        if (isNull(str)==MSFalse)
	  fld->value((char *) str->p);
	if(0==vallen)
	{
	  len = strlen((char*)str->p);
	  max=(len>max)?len:max;
	}
	dc(str);
      }
     if(0==vallen)vallen=max;
     for (i = 0; i < fields().length(); i++)
      {
	fld=(AplusSlotEntryField *) (MSWidget *) fields()(i);
	fld->valueWidth(vallen);
	fld->naturalSize();
      }
     alignLabels();  
   }
}

void AplusSlot::naturalSize(void)
{
  setGeometry();
  MSCompositeFieldBox::naturalSize();
}

void AplusSlot::updateTitle(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  
  if (v!=0)
   { 
     AplusSlotEntryField *fld;
     A                str;
     
     MSBoolean wasFrozen=freezeIfUnfrozen();
     for (int i = 0; i < fields().length(); i++)
      {
	fld = (AplusSlotEntryField *)(MSWidget *) fields()(i);
	str = itemLabel(i);
        if(isNull(str)==MSFalse)
	 {
  	   fld->label((char *)str->p);
	   dc(str);
	 }
	fld->labelFont(titleFont(i));
	fld->labelForeground(titleColor(i));
      }
     alignLabels();  
     unfreezeIfUnfrozen(wasFrozen);
   }
  MSCompositeFieldBox::updateTitle();
}

MSBoolean AplusSlot::setGeometry(void)
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

MSBoolean AplusSlot::compareGeometry(A ag_)
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


MSBoolean AplusSlot::setPositions(void)
{
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  A a=(model()==0)?0:((AplusModel*)model())->a();
  MSBoolean change=MSFalse;
  if (v!=0)
    {
      AGeometryFunction *geoFunc=AplusModel::getGeometryFunc(v);
      if (geoFunc!=0&&numRows()>0)
	{
	  A ag=(A)geoFunc->invoke(v,a);
	  if (isNull(ag)==MSFalse && ag->t==It)
	    {
	      P pg; pg.i=ag->p;
	      A am=ag;
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
	      int row=0;
	      int col=0;
	      int hspan=0;
	      int vspan=0;
	      int ax,ay,v_span,h_span;
	      MSBoolean mstat;
	      
	      int nr=numRows();
	      
	      AplusSlotEntryField *fld;
	      for (r=0;r<nr;r++)
		{
		  rowSpan(r,am,&row,&vspan);
		  colSpan(r,am,&col,&hspan);
		  
		  fld=(AplusSlotEntryField *)fields()(r);
		  if (fld!=0)
		    {
		      At fldGeo = fld->at();
		      ax=fldGeo.row();
		      ay=fldGeo.column();
		      h_span=fldGeo.columnSpan();
		      v_span=fldGeo.rowSpan();
		      mstat=fld->mapped();

		      fld->at(At(row, col, vspan, hspan, fld->resizeConstraints())); // preserve the constraints
		      if (hspan==0||vspan==0)
			{
			  fld->unmap();
			  fld->at(At(row, col, 1, 1, fld->resizeConstraints())); // preserve the constraints
			}
		      else if (fld->mapped()==MSFalse) fld->map();
		      if(ax 	  !=fld->at().row()        ||
			 ay 	  !=fld->at().column()     ||
			 h_span!=fld->at().columnSpan() ||
			 v_span!=fld->at().rowSpan()    ||
			 mstat !=fld->mapped())
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

void AplusSlot::rowSpan(int index_,A am_,int *row_,int *span_)
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

void AplusSlot::colSpan(int index_,A am_,int *col_,int *span_)
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

unsigned long AplusSlot::titleColor(int row_)
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

A AplusSlot::itemLabel(int row_)
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

Font AplusSlot::titleFont(int row_)
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

MSBoolean AplusSlot::readOnly(int row_) const
{
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();
  int n=(model()==0)?0:((AplusModel*)model())->numElmts();
  AVariableData *varData = ::pAVarDataFromV(v);  
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


Font AplusSlot::valueFont(int row_)
{
  Font fid=font();
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  int n = (model()!=0)?((AplusModel*)model())->numElmts():0;
  
  if (v!=0&&n==2) 
   {
     AFontFunction *fontFunc=AplusModel::getFontFunc(v);
     if (fontFunc!=0)
      {
        P p; p.i=((AplusModel *)model())->data();
        A as=p.a[0];
        A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
	 {
	   A attr=(A)as->p[row_];  // used as the pick
	   A val =(A)av->p[row_];
	   fid=(Font) fontFunc->invoke(v,(A)val,(A)attr);
	 }
      }
   }
  return fid;
}


unsigned long AplusSlot::editorBackground(void)
{
  if (fields().length()!=0)
   {
     return ((AplusSlotEntryField *)(MSWidget *)fields()(0))->editorBackground();
   }
  else
    return server()->defaultBackground();
  
}
unsigned long AplusSlot::editorForeground(void)
{
  if (fields().length()!=0)
   {
     return ((AplusSlotEntryField *)(MSWidget *)fields()(0))->editorForeground();
   }
  else
  return server()->defaultForeground();
}
void AplusSlot::editorBackground(unsigned long pixel_)
{
  for (int i = 0; i < fields().length(); i++)
   ((AplusSlotEntryField *)(MSWidget *)fields()(i))->editorBackground(pixel_);
}
void AplusSlot::editorForeground(unsigned long pixel_)
{
  for (int i = 0; i < fields().length(); i++)
   ((AplusSlotEntryField *)(MSWidget *)fields()(i))->editorForeground(pixel_);
}


MSBoolean AplusSlot::validate(AplusSlotEntryField *slot_, const char *string_)
{
  extern int safeAset(V,A,A,A);

  MSBoolean success = MSFalse;
  int row = fields().indexOf((unsigned long)(MSWidget *)slot_);
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v!=0&&row!=fields().length())
   {
     busyOn();

     P p; p.i=((AplusModel*)model())->data();
     A as=p.a[0];
     A attr=(A)as->p[row];  // used as the pick
	
     A pick=(A)gs(Et);
     *pick->p=(I)attr;
	
     AInFunction *inFunc;
     A r=aplus_nl;
     if ((inFunc=AplusModel::getInFunc(v))!=0)
     r=inFunc->invoke(v,(char *)string_,pick);
     else r=defaultInFunc(string_,row);          
     if (r!=0 && isNull(r)==MSFalse) 
      {
	r=(A)ic(r);
	if (safeAset(v,r,0,pick)==0) showError(qs);
	else 
	 {
	   AplusModel::doneCB(v,r,0,pick);
	   success=MSTrue;
	 }
	dc(r);
      }
     dc(pick);
   }
  busyOff();

  return success;
}


A AplusSlot::defaultInFunc(const char *string_,int row_)
{
  char   *ptrchar=0;
  long    lnum=0;
  double  dnum=0.0;
  A       r=aplus_nl;

  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v!=0&&row_<numRows()) 
   {
     P p; p.i=((AplusModel*)model())->data();
     A val=p.a[1];
     P pv; pv.i=val->p;
     A scalar=pv.a[row_];
     
     switch(scalar->t)
      {
	case It:
  	  lnum=strtol((char *)string_,&ptrchar,10); // Base 10
	  if (ptrchar==(char *)string_) 
	   {
	     r=aplus_nl;
	     showError("Unknown Number: Integer Expected");
	   }
	  else r=gi((int)lnum);
	  break;
	case Ft:
	  dnum=strtod((char *)string_,&ptrchar);
	  if (ptrchar==(char *)string_) 
	   {
	     r=aplus_nl;
	     showError("Unknown Number: Float Expected");
	   }
	  else r=gf((double)dnum);
	  break;
	case Ct:
	  r=gsv(0,(char *)string_);
	  break;
	case Et: // possible null object
	  if (scalar->n==0) r=gsv(0,(char *)string_);
	  break;
	default:
  	  break;
      }
   }
  return (A) r;
}

A AplusSlot::itemValue(int row_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  A outStr=aplus_nl;
  if (v!=0)
   {
     ACharStrFunction *outFunc=AplusModel::getOutFunc(v);
     if (outFunc!=0)
      {
	P p; p.i=((AplusModel*)model())->data();
	A as=p.a[0];
	A av=p.a[1];
	A attr=(A)as->p[row_];  // used as the pick
	A val=(A)av->p[row_];
	outStr=(A)outFunc->invoke(v,(A)val,(A)attr);   
        outStr=(outStr->t==Ct)?outStr:aplus_nl;
      }
   }
  return outStr;
}

extern "C" A gpix(A,A);
void AplusSlot::updateForeground(unsigned long oldfg_)
{
  AplusSlotEntryField *fld;
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v!=0) updateValues();
  MSLayout::updateForeground(oldfg_);
  for (int i = 0; i < fields().length(); i++)
   {
     fld = (AplusSlotEntryField *) (MSWidget *)fields()(i);
     fld->foreground(color(i));
   }
}

void AplusSlot::updateBackground(unsigned long oldbg_)
{
  AplusSlotEntryField *fld;
  MSLayout::updateBackground(oldbg_);
  for (int i = 0; i < fields().length(); i++)
   {
     fld = (AplusSlotEntryField *) (MSWidget *)fields()(i);
     fld->background(background());
   }
}


void AplusSlot::update(V v_,A,A pick_,I)
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
	       if (pick->n>0) for (int i=0;i<pick->n;i+=2) createCycle((int)pick->p[i+1]); 
	       else createCycle(-1);
	     }
            else 
	     {
               if (pick->p[0]==1) createCycle(-1);
               else if (v_ ==oldV) updateData();
             }
	  }
	else if (pick==0) cerr << "slot: pick assignment error in update." << endl;
        else cerr << "slot: pick assignment error in update." << endl;
	if (pick!=0) dc(pick);
      }
     else cerr << "slot: pick assignment error in update." << endl;
   }
  else if (v_ == oldV)updateData();
}

void AplusSlot::removeAllCycles(void)
{
  // Set each entryfield's cyclecolors to nothing

  AplusSlotEntryField *fld;
  MSUnsignedLongVector empty;

  for (int i = 0; i < fields().length(); i++)
   {
     fld = (AplusSlotEntryField *) (MSWidget *) fields()(i);
     fld->cycleColors(empty);
   }
}

void AplusSlot::createCycle(int row_)
{
  if (dbg_tmstk) cout << "CreateCycle(" << row_ << ")" << endl;
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;

  if (v!=0) 
   {
     A colors;
     int nr=numRows();
     if (row_>=0&&row_<nr)
      {
        colors=(cycleFunc()->func()==0)?cycleColors():cycleColor(row_);
	if (isNull(colors)==MSFalse)
	 {     
           startCycle(row_,colors);
	   if (colors!=0) dc(colors);
	 }
	else updateValue(row_);
      }
     else if (row_==-1)
      {
        if (cycleFunc()->func()==0)
	 {
	   colors=cycleColors();
	   if (isNull(colors)==MSFalse)
	    {     
	      startCycle(row_,colors);
	      if (colors!=0) dc(colors);
	    }
	   else updateValues();
	 }
        else
	 {
           for (int r=0;r<nr;r++)
	    {
	      colors=cycleColor(r);
	      if (isNull(colors)==MSFalse)
	       {     
		 startCycle(r,colors);
		 if (colors!=0) dc(colors);
	       }
              else updateValue(r);
	    }
	 }
      }
   }
}

// For each field, set their cycle interval
void AplusSlot::cycleInterval(unsigned long interval_)
{
  if (dbg_tmstk) cout << "cycleInterval(" << interval_ << ")" << endl;
  
  AplusSlotEntryField *fld;

  for (int i = 0; i < fields().length(); i++)
   {
     fld = (AplusSlotEntryField *) (MSWidget *)fields()(i);
     fld->cycleInterval(interval_);
   }
}

void AplusSlot::startCycle(int row_,A colors_)
{
  MSUnsignedLongVector colors;
  AplusSlotEntryField *fld;

  if (colors_->t == It && colors_->n > 0)
   {
     for (int i = 0; i < colors_->n; i ++)
       colors << (unsigned long) colors_->p[i];
     
     fld = (AplusSlotEntryField *) (MSWidget *)fields()(row_);
     fld->cycleColors(colors);
     updateValue(row_);
   }
}

A AplusSlot::cycleColor(int row_)
{
  A r=aplus_nl;
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;

  if (cycleFunc()->func()!=0&&row_<numRows())
   {
     P p; p.i=((AplusModel*)model())->data();
     A as=p.a[0];
     A av=p.a[1];
     A attr=(A)as->p[row_];  // used as the pick
     A val=(A)av->p[row_];
     r=cycleFunc()->invoke(v,(A)val,(A)attr);   
   }
  return (isNull(r)==MSFalse) ? convertToPixels(this,r) : r;
}

void AplusSlot::cycleColors(A colors_)
{
  if ((colors_->t==It&&colors_->r<=1)||(colors_->t==Et&&colors_->n==0))
   {
     removeAllCycles();
     if (_cycleColors!=0) dc(_cycleColors);
     _cycleColors=(A)ic(colors_);
   }  
}

A AplusSlot::cycleColors(void)
{ return (_cycleColors->n>0)?(A)ic(_cycleColors):aplus_nl; }

void AplusSlot::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel *)m_));
  setClipMode();
  updateTitle();
}

MSBoolean AplusSlot::verifyData(V, A a_)
{
  return isSlotFiller(a_);
}

void AplusSlot::receiveEvent(MSEvent& event_)
{
  const MSSymbol& eventType=event_.type();

  if (eventType==AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusSlot"  << endl;
      AplusEvent *ave = (AplusEvent *) &event_;
      V v     = ((AplusModel *)model())->aplusVar();
      A index = ave->index();
      A pick  = ave->pick();
      I ravel = ave->ravel();;
      update(v,index, pick, ravel);
    }
  else if (eventType==AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusSlot"  << endl;
      
      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  else if (eventType==AplusUpdateDataEvent::symbol())  // Size update Event
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusArray"  << endl;
      setClipMode();
      updateData();
    }
  else if (eventType==AplusUpdateTitleEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateTitleEvent in AplusArray"  << endl;      
      updateTitle();
    }
}


void AplusSlot::setClipMode(void)
{
  AplusModel *pModel=(AplusModel *)model();
  if (pModel!=0)
    {
      V v=pModel->aplusVar();
      if (v!=0)
	{
	  unsigned int numFields=fields().length();
	  AplusSlotEntryField *fld;

	  AVariableData *varData = ::pAVarDataFromV(v);

	  if (varData->stars()==MSTrue)
	    {
	      for (unsigned int i=0; i<numFields; ++i)
		{
		  fld=(AplusSlotEntryField *)(MSWidget *)fields()(i);
		  fld->clipMode(MSClipStars);
		}
	    }
	  else
	    {
	      for (unsigned int i=0; i<numFields; ++i)
		{
		  fld=(AplusSlotEntryField *)(MSWidget *)fields()(i);
		  fld->clipMode(MSNoClipping);
		}
	    }
	}
    }
}


int AplusSlot::numRows(void) 
{ int r=0;
  if (model() && ((AplusModel*)model())->aplusVar()!=0)
   { P p; p.i=((AplusModel*)model())->data();
     A p0=p.a[0];
     r=(int)p0->n;
   }
  return r;
}


int AplusSlot::valueLength(void)
 {
   V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
   if (v)
    {
      AVariableData *varData = ::pAVarDataFromV(v);
      return varData->colWidth();
    }
   return 0;
 }

int AplusSlot::editLength(void)
 {
   V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
   if (v)
    {
      AVariableData *varData = ::pAVarDataFromV(v);
      return varData->editWidth();
    }
   return 0;
 }


unsigned long AplusSlot::color(unsigned row_)
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

MSBoolean AplusSlot::editing(void)
{
  for(int i = 0; i < fields().length(); i++)
   {
     AplusSlotEntryField *f = (AplusSlotEntryField *)(MSWidget *)fields()(i);
     if (f->editing()==MSTrue)
      {
	return MSTrue;
      }
   }
  return MSFalse;
}

void AplusSlot::startEditing(void)
{
  MSWidget *focusWidget = inputFocus();
  for(int i = 0; i < fields().length(); i++)
   {
     if (focusWidget ==(MSWidget *)fields()(i))
      {
	AplusSlotEntryField *f = (AplusSlotEntryField *)(MSWidget *)fields()(i);
	f->editSelection();
	return;
      }
   }
}

void AplusSlot::stopEditing(void)
{
  for(int i = 0; i < fields().length(); i++)
   {
     AplusSlotEntryField *f = (AplusSlotEntryField *)(MSWidget *)fields()(i);
     if (f->editing()==MSTrue)
      {
	f->returnKey();
      }
   }
}

void AplusSlot::tab(void)
{
  const MSUnsignedLongVector& aVector=fields();
  MSWidget *w = inputFocus();
  unsigned i = fields().indexOf((unsigned long) w);
  int len=fields().length();

  if (i < len )
    {
      for(int j=1; j<len; j++)
	{
	  int fld=(i+j)%len;
	  if( MSFalse==((MSWidget *)aVector(fld))->isProtected())
	    {
	      traverseFocus((MSWidget *)aVector(fld));
	      break;
	    }
	}
    }

}

void AplusSlot::shiftTab(void)
{
  const MSUnsignedLongVector& aVector=fields();
  MSWidget *w=inputFocus();
  unsigned i=fields().indexOf((unsigned long)w);
  int len=fields().length();

  if (i < len )
    {
      for(int j=1; j<len; j++)
	{
	  int fld=i-j;
	  if(fld<0) fld=len+fld;
	  if( MSFalse==((MSWidget *)aVector(fld))->isProtected())
	    {
	      traverseFocus((MSWidget *)aVector(fld));
	      break;
	    }
	}
    }

}

void AplusSlot::takeFocus(void)
{
  AplusShell::allowNestedTraversal(MSTrue);
  if (fields().length()>0 && childInFocus()==MSFalse)
   {
     AplusSlotEntryField *f = (AplusSlotEntryField *)(MSWidget *)fields()(0);
     traverseFocus(f);
     childInFocus(MSTrue);
   }
  else
   {
     childInFocus(MSFalse);
     traverseToNext();
   }
  AplusShell::allowNestedTraversal(MSFalse);
}

void AplusSlot::arrowButtons(A states_)
{
  if (QA(states_) && (isNull(states_)==MSTrue || states_->t==It))
   {
     dc(_arrowButtons);
     _arrowButtons = states_;

     for (unsigned i=0;i<fields().length();i++)
      {
	MSBoolean enable = (states_->r?((i<states_->n)?states_->p[i]:0):*states_->p)?MSTrue:MSFalse;
	AplusSlotEntryField *f = (AplusSlotEntryField *)(MSWidget *)fields()(i);
	f->arrowButtons(enable);
      }
   }
}

void AplusSlot::redraw(void)
{
  updateValues();
}


const MSSymbol& AplusSlot::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusSlot::symbol(void)
{
  static MSSymbol sym("AplusSlot");
  return sym;
}
