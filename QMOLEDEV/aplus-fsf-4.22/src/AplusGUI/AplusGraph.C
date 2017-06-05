///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusGraph.H>
#include <AplusGUI/AplusTrace.H>
#include <AplusGUI/EnumTables.H>
#include <AplusGUI/AplusConvert.H>

extern long dbg_tmstk;

const unsigned long EnumHashTableSize = 128;

MSBoolean   AplusGraph::_initialized = MSFalse;
EnumError   AplusGraph::_enumError;
MSHashTable AplusGraph::_enumHashTable(EnumHashTableSize);
MSHashTable AplusGraph::_stringEnumHashTable(EnumHashTableSize);
MSHashTable AplusGraph::_gridEnumHashTable(EnumHashTableSize);
MSHashTable AplusGraph::_legendEnumHashTable(EnumHashTableSize);
AplusFormatter AplusGraph::_outFormat;

////////////////////////////////////////////////////////////////////////////////
EnumError::EnumError(void) 
{
  _lamp    = "\343";
  _attrmsg = ": unknown attributes: `";
  _symmsg  = ": invalid symbol";
}  


AplusGraph::AplusGraph(MSWidget *owner_) : MSGraph(owner_)
{
  if (_initialized == MSFalse)
   {
     _initialized = MSTrue;
     initEnumHashTable();
     initStringEnumHashTable();
     initGridEnumHashTable();
     initLegendEnumHashTable();
   }

  _axisAMode = "std";
  _axisARule = "std";
  _gridAStyle = "dash";
  _zeroAStyle = "dot1";

  for (int i=0; i<2; i++)                              // jmiz
    {				                       // jmiz
     _y_labelFormat[i]=     AplusFormatter::BadFormat; // jmiz
     _x_labelFormat[i]=     AplusFormatter::BadFormat; // jmiz
     _x_subLabelFormat[i]=  AplusFormatter::BadFormat; // jmiz
    }                                                  // jmiz

  foreground(server()->pixel("black"));
  gridForeground(server()->pixel("slategray"));
  zeroAxisForeground(server()->pixel("slategray"));
  
  unsigned long fg = server()->pixel("black");
  axisForeground(fg, MSTop | MSBottom | MSLeft | MSRight);
  legendAlignment(MSG::Inside|MSTop|MSLeft);
  
  AplusModel *am=new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusGraph::~AplusGraph(void)
{}

void AplusGraph::initEnumHashTable( void )
{
  enumHashTable()->notFound((int)0);
  enumHashTable()->add(""     	       	      ,(void*)MSNone);
  enumHashTable()->add("axis" 	       	      ,(void*)_Axis);
  enumHashTable()->add("ymode"	       	      ,(void*)_YAxis);
  enumHashTable()->add("rule" 	       	      ,(void*)_Rule);
  enumHashTable()->add("grid" 	       	      ,(void*)_Grid);
  enumHashTable()->add("gridStyle"     	      ,(void*)_LineStyle);
  enumHashTable()->add("tickStyle"     	      ,(void*)_TickStyle);
  enumHashTable()->add("legendPosition"	      ,(void*)_LegendPosition);
  enumHashTable()->add("legendPlacement"      ,(void*)_LegendPlacement);
  enumHashTable()->add("legendStyle"   	      ,(void*)_LegendStyle);
  enumHashTable()->add("ylabelStyle"   	      ,(void*)_DisplayStyle);
  enumHashTable()->add("standard"             ,(void*)MSGraph::Standard);
  enumHashTable()->add("normalize"            ,(void*)MSGraph::Normalize);
  enumHashTable()->add("optimize"             ,(void*)MSGraph::Optimize);
  enumHashTable()->add("piechart"             ,(void*)MSGraph::PieChart);
  enumHashTable()->add("normal"        	      ,(void*)MSGraph::Normal);
  enumHashTable()->add("addtrace"      	      ,(void*)MSGraph::AddTrace);
  enumHashTable()->add("addtexttrace"  	      ,(void*)MSGraph::AddTextTrace);
  enumHashTable()->add("solid"	       	      ,(void*)Solid);
  enumHashTable()->add("dash" 	       	      ,(void*)Dash3);
  enumHashTable()->add("dash1"	       	      ,(void*)Dash0);
  enumHashTable()->add("dash2"	       	      ,(void*)Dash1);
  enumHashTable()->add("dash3"	       	      ,(void*)Dash2);
  enumHashTable()->add("dash4"	       	      ,(void*)Dash3);
  enumHashTable()->add("dash5"	       	      ,(void*)Dash4);
  enumHashTable()->add("dotdash"       	      ,(void*)DotDash3);
  enumHashTable()->add("dotdash1"      	      ,(void*)DotDash0);
  enumHashTable()->add("dotdash2"      	      ,(void*)DotDash1);
  enumHashTable()->add("dotdash3"      	      ,(void*)DotDash2);
  enumHashTable()->add("dotdash4"      	      ,(void*)DotDash3);
  enumHashTable()->add("dotdash5"      	      ,(void*)DotDash4);
  enumHashTable()->add("dot" 	       	      ,(void*)Dot3);
  enumHashTable()->add("dot1"	       	      ,(void*)Dot0);
  enumHashTable()->add("dot2"	       	      ,(void*)Dot1);
  enumHashTable()->add("dot3"	       	      ,(void*)Dot2);
  enumHashTable()->add("dot4"	       	      ,(void*)Dot3);
  enumHashTable()->add("dot5"	       	      ,(void*)Dot4);
  enumHashTable()->add("ver"	       	      ,(void*)MSG::Vertical);
  enumHashTable()->add("hor"	       	      ,(void*)MSG::Horizontal);
  enumHashTable()->add("lastvalue"     	      ,(void*)MSG::LastValue);
  enumHashTable()->add("none"	       	      ,(void*)MSNone);
  enumHashTable()->add("in"  	       	      ,(void*)MSG::Inside);
  enumHashTable()->add("out"  	       	      ,(void*)MSG::Outside);
  enumHashTable()->add("inout"	       	      ,(void*)(MSG::Inside | MSG::Outside));
  enumHashTable()->add("tl"	       	      ,(void*)(MSTop | MSLeft));
  enumHashTable()->add("tc"	       	      ,(void*)(MSTop | MSCenter));
  enumHashTable()->add("tr"	       	      ,(void*)(MSTop | MSRight));
  enumHashTable()->add("bl"	       	      ,(void*)(MSBottom | MSLeft));
  enumHashTable()->add("bc"	       	      ,(void*)(MSBottom | MSCenter));
  enumHashTable()->add("br"	       	      ,(void*)(MSBottom | MSRight));
  enumHashTable()->add("std"	       	      ,(void*)MSG::Std);
  enumHashTable()->add("box"	       	      ,(void*)MSG::Box);
  enumHashTable()->add("x"	       	      ,(void*)MSBottom);
  enumHashTable()->add("X"	       	      ,(void*)MSTop);
  enumHashTable()->add("y"	       	      ,(void*)MSLeft);
  enumHashTable()->add("Y"	       	      ,(void*)MSRight);
  enumHashTable()->add("XY"	       	      ,(void*)(MSTop | MSRight));
  enumHashTable()->add("xY"	       	      ,(void*)(MSBottom | MSRight));
  enumHashTable()->add("Xy"	       	      ,(void*)(MSTop | MSLeft));
  enumHashTable()->add("xy"	       	      ,(void*)(MSBottom | MSLeft));
  enumHashTable()->add("ascend"        	      ,(void*)MSG::Ascending);
  enumHashTable()->add("descend"       	      ,(void*)MSG::Descending);
  enumHashTable()->add("inside"        	      ,(void*)MSG::Inside);
  enumHashTable()->add("outsidevertical"      ,(void*)(MSG::Outside | MSG::Vertical));
  enumHashTable()->add("outsidehorizontal"    ,(void*)(MSG::Outside | MSG::Horizontal));
		       
}

void AplusGraph::initStringEnumHashTable( void )
{
  stringEnumHashTable()->notFound((int)0);

  // GraphModes
  stringEnumHashTable()->add((unsigned long)MSGraph::Standard 	 ,(void*)"standard");      // 1
  stringEnumHashTable()->add((unsigned long)MSGraph::Normalize 	 ,(void*)"normalize");     // 2
  stringEnumHashTable()->add((unsigned long)MSGraph::Optimize 	 ,(void*)"optimize");      // 4
  stringEnumHashTable()->add((unsigned long)MSGraph::PieChart 	 ,(void*)"piechart");      // 8
  // GraphUIModes
  stringEnumHashTable()->add((unsigned long)MSGraph::Normal  	 	 ,(void*)"normal");       // 16
  stringEnumHashTable()->add((unsigned long)MSGraph::AddTrace	 	 ,(void*)"addtrace");     // 32
  stringEnumHashTable()->add((unsigned long)MSGraph::AddTextTrace   	 ,(void*)"addtexttrace"); // 2048

  // LinesStyles
  stringEnumHashTable()->add(Solid	 	 	 ,(void*)"solid");    // 600
  stringEnumHashTable()->add(Dash0	 	 	 ,(void*)"dash1");    // 601
  stringEnumHashTable()->add(Dash1	 	 	 ,(void*)"dash2");    // 602
  stringEnumHashTable()->add(Dash2	 	 	 ,(void*)"dash");     // 603
  stringEnumHashTable()->add(Dash3	 	 	 ,(void*)"dash4");    // 604
  stringEnumHashTable()->add(Dash4	 	 	 ,(void*)"dash5");    // 605
  stringEnumHashTable()->add(DotDash0	 	 	 ,(void*)"dotdash1"); // 606
  stringEnumHashTable()->add(DotDash1	 	 	 ,(void*)"dotdash2"); // 607
  stringEnumHashTable()->add(DotDash2	 	 	 ,(void*)"dotdash");  // 608
  stringEnumHashTable()->add(DotDash3	 	 	 ,(void*)"dotdash4"); // 609
  stringEnumHashTable()->add(DotDash4	 	 	 ,(void*)"dotdash5"); // 610
  stringEnumHashTable()->add(Dot0	 	 	 ,(void*)"dot1");     // 611
  stringEnumHashTable()->add(Dot1	 	 	 ,(void*)"dot2");     // 612
  stringEnumHashTable()->add(Dot2	 	 	 ,(void*)"dot");      // 613
  stringEnumHashTable()->add(Dot3	 	 	 ,(void*)"dot4");     // 614
  stringEnumHashTable()->add(Dot4	 	 	 ,(void*)"dot5");     // 615

  stringEnumHashTable()->add(MSG::Vertical		 ,(void*)"ver");       // 64
  stringEnumHashTable()->add(MSG::Horizontal		 ,(void*)"hor");       // 128

  // GraphStyles
  stringEnumHashTable()->add(MSG::LastValue	 	 ,(void*)"lastvalue"); // 256
  stringEnumHashTable()->add(MSG::Outside  	 	 ,(void*)"out");       // 512
  stringEnumHashTable()->add(MSG::Inside   	 	 ,(void*)"in");        // 1024
  stringEnumHashTable()->add(MSG::Inside | MSG::Outside  ,(void*)"inout");     // 1536
}

void AplusGraph::initGridEnumHashTable( void )
{
  gridEnumHashTable()->notFound((int)0);

  // Grid
  gridEnumHashTable()->add((unsigned long)MSNone      	 	         ,(void*)"none"); // 1
  gridEnumHashTable()->add((unsigned long)MSBottom	 	      	 ,(void*)"x");    // 32
  gridEnumHashTable()->add((unsigned long)MSTop       	      	         ,(void*)"X"); 	  // 16
  gridEnumHashTable()->add((unsigned long)MSLeft	 	      	 ,(void*)"y"); 	  // 4
  gridEnumHashTable()->add((unsigned long)MSRight	 	      	 ,(void*)"Y"); 	  // 8
  gridEnumHashTable()->add((unsigned long)MSTop | MSLeft	      	 ,(void*)"Xy");   // 20
  gridEnumHashTable()->add((unsigned long)MSTop | MSRight	      	 ,(void*)"XY");   // 24
  gridEnumHashTable()->add((unsigned long)MSBottom | MSLeft        	 ,(void*)"xy");   // 36
  gridEnumHashTable()->add((unsigned long)MSBottom | MSRight	      	 ,(void*)"xY");   // 40
}

void AplusGraph::initLegendEnumHashTable(void)
{
  legendEnumHashTable()->notFound((int)0);

  // Legend Position has been moved to AplusGraph::LegendAlignConverter hash tables
  //

  // Axis Mode
  legendEnumHashTable()->add((unsigned long)MSG::Ascending 	      	 ,(void*)"ascend");  // 0
  legendEnumHashTable()->add((unsigned long)MSG::Descending	      	 ,(void*)"descend"); // 1
}

// Format Labelout Functions (uses AplusLabelOut)

void AplusGraph::y_labelFormatFunc(AFunc func_,AClientData *ac_,int axis_)
{
  y_labelFormatFunc(axis_)->func(func_);
  y_labelFormatFunc(axis_)->arg(ac_);
  AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisLabelOut((axis_==0)?MSLeft:MSRight);
  AplusFuncLabelOut *aflo = new AplusFuncLabelOut(y_labelFormatFunc(axis_), ((AplusModel*)model())->aplusVar(), alo);
  axisLabelOut(aflo, (axis_==0)?MSLeft:MSRight);
  y_labelFormat(axis_==1?YYFunc:YFunc,axis_);
  redrawImmediately();
}

void AplusGraph::x_labelFormatFunc(AFunc func_,AClientData *ac_,int axis_)
{
  x_labelFormatFunc(axis_)->func(func_);
  x_labelFormatFunc(axis_)->arg(ac_);
  AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisLabelOut((axis_==0)?MSBottom:MSTop);
  AplusFuncLabelOut *aflo = new AplusFuncLabelOut(x_labelFormatFunc(axis_), ((AplusModel*)model())->aplusVar(), alo);
  axisLabelOut(aflo, (axis_==0)?MSBottom:MSTop);
  x_labelFormat(axis_==1?XXFunc:XFunc,axis_);
  redrawImmediately();
}


void AplusGraph::x_subLabelFormatFunc(AFunc func_,AClientData *ac_,int axis_)
{
  x_subLabelFormatFunc(axis_)->func(func_);
  x_subLabelFormatFunc(axis_)->arg(ac_);
  AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisSubLabelOut((axis_==0)?MSBottom:MSTop);
  AplusFuncLabelOut *aflo = new AplusFuncLabelOut(x_subLabelFormatFunc(axis_), ((AplusModel*)model())->aplusVar(), alo);
  axisSubLabelOut(aflo, (axis_==0)?MSBottom:MSTop);
  x_subLabelFormat(axis_==1?XXSubFunc:XSubFunc,axis_);
  redrawImmediately();
}

// LabelFuncs uses AplusFuncLabel

void AplusGraph::x_subLabelFunc(AFunc func_,AClientData *ac_,int axis_)
{
  x_subLabelFunc(axis_)->func(func_);
  x_subLabelFunc(axis_)->arg(ac_); 
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v)
   {
     ::A a = x_subLabelFunc(axis_)->invoke(v, (::A)v->a);
     AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisSubLabelOut((axis_==0)?MSBottom:MSTop);
     AplusFuncLabel *afl = new AplusFuncLabel(a, alo);
     axisSubLabelOut(afl, (axis_==0)?MSBottom:MSTop);
   }
}

void AplusGraph::x_labelFunc(AFunc func_,AClientData *ac_,int axis_)
{
  x_labelFunc(axis_)->func(func_);
  x_labelFunc(axis_)->arg(ac_); 
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v)
   {
     ::A a = x_labelFunc(axis_)->invoke(v, (::A)v->a);
     AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisLabelOut((axis_==0)?MSBottom:MSTop);
     AplusFuncLabel *afl = new AplusFuncLabel(a, alo);
     axisLabelOut(afl, (axis_==0)?MSBottom:MSTop);
   }
}

void AplusGraph::y_labelFunc(AFunc func_,AClientData *ac_,int axis_)
{
  y_labelFunc(axis_)->func(func_);
  y_labelFunc(axis_)->arg(ac_);
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v)
   {
     ::A a = y_labelFunc(axis_)->invoke(v, (::A)v->a);
     AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisLabelOut((axis_==0)?MSLeft:MSRight);
     AplusFuncLabel *afl = new AplusFuncLabel(a, alo);
     axisLabelOut(afl, (axis_==0)?MSLeft:MSRight);
   }
}

////////////////////////////////////////////////////////////////////////////////

void AplusGraph::y_labelFormat(::A sym_, int axis_, int precision_)
{
  AplusFormatter::OutputFormat k;
     
  if ((k=outFmt()->format(sym_))!=AplusFormatter::BadFormat)
   {
     _y_labelFormat[axis_]=(int)k;
     _y_labelPrecision[axis_]=precision_;
     AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisLabelOut((axis_==0)?MSLeft:MSRight);
     AplusFormatLabelOut *afl = new AplusFormatLabelOut(k, precision_, alo);
     axisLabelOut(afl, (axis_==0)?MSLeft:MSRight);
     redrawImmediately();
   }
  else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
}

::A AplusGraph::y_labelFormatSym(int axis_) 
{
  if (y_labelFormat(axis_)==YFunc||y_labelFormat(axis_)==YYFunc) return generateFuncSym();
  else return generateSym(outFmt()->formatEnumString(y_labelFormat(axis_)),
			  y_labelFormat(axis_)==outFmt()->Fixed?y_labelPrecision(axis_):-1);
}

void AplusGraph::x_subLabelFormat(::A sym_,int axis_, int precision_) 
{ 
  AplusFormatter::OutputFormat k;
     
  if ((k=outFmt()->format(sym_))!=AplusFormatter::BadFormat)
   {
     _x_subLabelFormat[axis_]=(int)k;
     _x_subLabelPrecision[axis_]=(int)precision_;
     AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisSubLabelOut((axis_==0)?MSBottom:MSTop);
     AplusFormatLabelOut *afl = new AplusFormatLabelOut(k, precision_, alo);
     axisSubLabelOut(afl, (axis_==0)?MSBottom:MSTop);
     redrawImmediately();
   }
  else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
} 

::A AplusGraph::x_subLabelFormatSym(int axis_) 
{
  if (x_subLabelFormat(axis_)==XFunc) return generateFuncSym();
  else return generateSym(outFmt()->formatEnumString(x_subLabelFormat(axis_)),
			  x_subLabelFormat(axis_)==outFmt()->Fixed?x_subLabelPrecision(axis_):-1);
}


void AplusGraph::x_labelFormat(::A sym_,int axis_, int precision_) 
{ 
  AplusFormatter::OutputFormat k;
     
  if ((k=outFmt()->format(sym_))!=AplusFormatter::BadFormat)
   {
     _x_labelFormat[axis_]=(int)k;
     _x_labelPrecision[axis_]=precision_;
     AplusLabelOut *alo = (AplusLabelOut *)(MSLabelOut *)axisLabelOut((axis_==0)?MSBottom:MSTop);
     AplusFormatLabelOut *afl = new AplusFormatLabelOut(k, precision_, alo);
     axisLabelOut(afl, (axis_==0)?MSBottom:MSTop);
     redrawImmediately();
   }
  else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
} 

::A AplusGraph::x_labelFormatSym(int axis_) 
{
  if (x_labelFormat(axis_)==XFunc || x_labelFormat(axis_)==XXFunc) return generateFuncSym();
  else return generateSym(outFmt()->formatEnumString(x_labelFormat(axis_)),
			  x_labelFormat(axis_)==outFmt()->Fixed?x_labelPrecision(axis_):-1);
}

::A AplusGraph::generateFuncSym()
{
  ::A a=gs(Et); 
  *a->p=MS(si((char *)"func"));
  return a; 
}

////////////////////////////////////////////////////////////////////////////////

::A AplusGraph::enumSymbols(const char *str_)  
{ 
  ::A		r=aplus_nl;
  GraphEnum	k;

  if ((k=(GraphEnum)(unsigned long)enumHashTable()->lookup(str_))>0&&
      (k>=_GraphMode&&k<=_DisplayStyle))
   {
     switch(k)
      {
      case _GraphStyle:
	r=gv(Et,2);
	r->p[0]=MS(si("ver"));
	r->p[1]=MS(si("hor"));
	break;
	
      case _GraphMode:
	r=gv(Et,3);
	r->p[0]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSGraph::Normal)));
	r->p[1]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSGraph::AddTrace)));
	r->p[2]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSGraph::AddTextTrace)));
	break;
	
      case _YAxis:
	r=gv(Et,2);
	r->p[0]=MS(si((char *)legendEnumHashTable()->lookup((unsigned long)MSG::Ascending)));
	r->p[1]=MS(si((char *)legendEnumHashTable()->lookup((unsigned long)MSG::Descending)));
	break;

      case _Axis:
      case _Rule:
	r=gv(Et,18);
	r->p[0]=MS(si("x"));
	r->p[1]=MS(si("y"));
	r->p[2]=MS(si("X"));
	r->p[3]=MS(si("Y"));
	r->p[4]=MS(si("yY"));
	r->p[5]=MS(si("xy"));
	r->p[6]=MS(si("xX"));
	r->p[7]=MS(si("xY"));
	r->p[8]=MS(si("Xy"));
	r->p[9]=MS(si("XY"));
	r->p[10]=MS(si("xXy"));
	r->p[11]=MS(si("xXY"));
	r->p[12]=MS(si("xyY"));
	r->p[13]=MS(si("XyY"));
	r->p[14]=MS(si("xXyY"));
	r->p[15]=MS(si(k==_Rule?"axis":"std"));
	r->p[16]=MS(si("box"));
	r->p[17]=MS(si("none"));
	break;
	
      case _Grid:
	r=gv(Et,9);
	r->p[0]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSNone)));
	r->p[1]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSBottom)));
	r->p[2]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSTop)));
	r->p[3]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSLeft)));
	r->p[4]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSRight)));
	r->p[5]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSTop | MSRight)));
	r->p[6]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSBottom | MSRight)));
	r->p[7]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSTop | MSLeft)));
	r->p[8]=MS(si((char *)gridEnumHashTable()->lookup((unsigned long)MSBottom | MSLeft)));
	break;
	
      case _LineStyle:
	r=gv(Et,4);
	r->p[0]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)Solid)));
	r->p[1]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)Dash3)));
	r->p[2]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)DotDash3)));
	r->p[3]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)Dot3)));
	break;
	
      case _TickStyle:
	r=gv(Et,3);
	r->p[0]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSG::Outside)));
	r->p[1]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSG::Inside)));
	r->p[2]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSG::Inside | MSG::Outside)));
	break;
	
      case _LegendStyle:
	r=gv(Et,3);
	r->p[0]=MS(si("ver"));
	r->p[1]=MS(si("hor"));
	r->p[2]=MS(si((char *)stringEnumHashTable()->lookup((unsigned long)MSG::LastValue)));
	break;

      case _DisplayStyle:
	r=gv(Et,2);
	r->p[1]=MS(si("ver"));
	r->p[0]=MS(si("hor"));
	break;
      }
   }
  return r;
}

////////////////////////////////////////////////////////////////////////////////

::A AplusGraph::xExtents(int axis_)
{
  ::A axisData=gv(Ft,3);
  ((F *)axisData->p)[0]=xMinData(axis_);
  ((F *)axisData->p)[1]=xMaxData(axis_);
  ((F *)axisData->p)[2]=xScale(axis_);
  return axisData;
}
::A AplusGraph::yExtents(int axis_)
{
  ::A axisData=gv(Ft,3);
  ((F *)axisData->p)[0]=yMinData(axis_);
  ((F *)axisData->p)[1]=yMaxData(axis_);
  ((F *)axisData->p)[2]=yScale(axis_);
  return axisData;
}
::A AplusGraph::xLabelHeight(int axis_)
{
  ::A h=gs(It);
  XFontStruct  *fontInfo;
  fontInfo=(XFontStruct *)server()->fontStruct(xLabelFont(axis_));
  *h->p=fontInfo->ascent+fontInfo->descent;
  return h;
}
::A AplusGraph::yLabelHeight(int axis_)
{
  ::A h=gs(It); XFontStruct  *fontInfo;
  fontInfo=(XFontStruct *)server()->fontStruct(yLabelFont(axis_));
  *h->p=fontInfo->ascent+fontInfo->descent;
  return h;
}
::A AplusGraph::xLabelWidth(::A text_,int axis_)
{
  XFontStruct *fontInfo=(XFontStruct*)server()->fontStruct(xLabelFont(axis_));
  return labelWidth(text_,fontInfo);
}
::A AplusGraph::yLabelWidth(::A text_,int axis_)
{
  XFontStruct *fontInfo=(XFontStruct*)server()->fontStruct(yLabelFont(axis_));
  return labelWidth(text_,fontInfo);
}
::A AplusGraph::labelWidth(::A text_,XFontStruct *fi_)
{
  ::A		w=aplus_nl;
  P		p;
  int 		i,n,offset;
  
  if (text_!=0&&text_->n!=0)
   {
     if (text_->t==Ct)
      {
	if (text_->r==2)
	 {
	   w=gv(It,(int)text_->d[0]);
	   n=(int)text_->d[1];
	   p.i=text_->p;
	   for (i=0; i<text_->d[0]; i++)
	    {
	      offset=i*n;
	      w->p[i]=XTextWidth(fi_,(char *)p.c+offset,n);
	    }
	 }
	else
	 {
	   w=gs(It); 
	   *w->p=XTextWidth(fi_,(char *)text_->p,(int)text_->n);
	 }
      }
     else if (text_->t==Et) 
      {
	w=gv(It,(int)text_->n);
	p.i=text_->p;
	for (i=0; i<(int)text_->n; i++) 
	 {
	   w->p[i]=XTextWidth(fi_,(char *)p.a[i]->p,(int)p.a[i]->n);
	 }
      }
   }
  return w;
}

::A AplusGraph::coordinate(int axis_)   // Call xCursorValue and yCursorValue  
{ 
  int offset=0;
  double x,y;
  ::A r; P p;
  
  if (graphUIMode()==AddTrace)
   {
     r=gm(Ft,nt(selectLine())->pointCount(),2);
     p.i=r->p;
     for (int i=0; i<nt(selectLine())->pointCount(); i++)
      {
	x = xPixelToValue(nt(selectLine())->points(i)->x, axis_);
	y = yPixelToValue(nt(selectLine())->points(i)->y, axis_);
	p.f[offset++]=x;
	p.f[offset++]=y;
      }
   }
  else
   {
     r=gm(Ft,1,2);
     p.i=r->p; 
     p.f[0]=xCursorValue(axis_==0?MSLeft:MSRight); 
     p.f[1]=yCursorValue(axis_==0?MSBottom:MSTop); 
   }
  return r;
}

::A AplusGraph::generateSym(char *str_,int prec_=-1)
{
  if (str_==0) return aplus_nl;
  ::A a,s=gs(Et); 
  *s->p=MS(si(str_));
  if (prec_>=0)
   {
     a=gv(Et,2); 
     a->p[0]=(I)s;
     a->p[1]=(I)gi(prec_); 
   }
  else a=s;
  return a; 
}

::A AplusGraph::generateSym(MSHashTable *table_, int enumType_)
{
  ::A a=gs(Et); 
  *a->p=MS(si((char *)table_->lookup((unsigned long)enumType_)));
  return a; 
}

::A AplusGraph::generateGraphModeSym(unsigned long mode_)
{
  unsigned i=0,n=0;
  if (mode_==Standard) n++;
  if (mode_&Normalize) n++;
  if (mode_&Optimize) n++;
  if (mode_&PieChart) n++;
  if (n>0)
   {
     ::A a=gv(Et,n);
     if (mode_==Standard) a->p[i++]=MS(si((char *)stringEnumHashTable()->lookup(mode_&Standard)));
     if (mode_&Normalize) a->p[i++]=MS(si((char *)stringEnumHashTable()->lookup(mode_&Normalize)));
     if (mode_&Optimize) a->p[i++]=MS(si((char *)stringEnumHashTable()->lookup(mode_&Optimize)));
     if (mode_&PieChart) a->p[i++]=MS(si((char *)stringEnumHashTable()->lookup(mode_&PieChart)));
     return a;
   }
  return aplus_nl;
}

::A AplusGraph::generateGraphUIModeSym(int enumType_)
{ return generateSym(stringEnumHashTable(), enumType_); }

::A AplusGraph::generateAxisModeSym(AxisMode enumType_)
{ return generateSym(legendEnumHashTable(), enumType_); }

::A AplusGraph::generateGridSym(unsigned long enumType_)
{ return generateSym(gridEnumHashTable(), enumType_); }

::A AplusGraph::generateLegendStyleSym(unsigned long enumType_)
{ return generateSym(stringEnumHashTable(), enumType_); }

::A AplusGraph::generateTickStyleSym(unsigned long enumType_)
{ return generateSym(stringEnumHashTable(), enumType_); }
   

void AplusGraph::graphAMode(::A sym_)   // Need to look up symbol in hashtable and call graphUIMode()
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   {
     unsigned long k,mode=0;
     for (unsigned i=0;i<sym_->n;i++)
      {
        if ((k=(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[i])->n))>0) mode+=k; 
        else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[i])->n); 
      }
     graphMode(mode);
   } 
}

::A AplusGraph::graphAMode(void)         //  See graphModeSym in XGraph.H
{ return generateGraphModeSym(graphMode()); }

void AplusGraph::graphAUIMode(::A sym_)   // Need to look up symbol in hashtable and call graphUIMode()
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     GraphUIMode k;
     if ((k=(GraphUIMode)(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>0&&
         (k>=MSGraph::Normal&&k<=MSGraph::AddTextTrace))
      { 
        graphUIMode(k); 
      } 
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
}

::A AplusGraph::graphAUIMode(void)         //  See graphModeSym in XGraph.H
{ return generateGraphUIModeSym(graphUIMode()); }

void AplusGraph::axisAMode(::A sym_) // Need to look up symbol in hashtable and call axisMode()
{ 
  unsigned long	mode;
  
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   {
     char *style=(char *)XS(sym_->p[0])->n;
     mode=(unsigned long)enumHashTable()->lookup(style);

     if (mode==Std)
      {
	axis(Std);
      }
     else if (mode==MSG::Box)
      {
	axis(MSG::Box);
      }
     else 
      {
	if (strpbrk(style,"x")!=0)  mode |= MSBottom;
	if (strpbrk(style,"X")!=0)  mode |= MSTop;  
	if (strpbrk(style,"y")!=0)  mode |= MSLeft; 
	if (strpbrk(style,"Y")!=0)  mode |= MSRight;
	axis(mode);
      }
     if (mode==MSNone) _axisAMode = "none";
     else if (style) _axisAMode = style;
   }
}

::A AplusGraph::axisAMode(void) //  See axisSym in XGraph.H
{
  ::A out = gs(Et);
  *out->p=MS(si((char *)(const char *)_axisAMode));
  return out;
}


void AplusGraph::yAMode(::A sym_, MSAlignment a_) // Need to look up symbol in hashtable and call yMode()
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     AxisMode k; 
     k=(AxisMode)(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n);
     if (k>=MSG::Ascending && k<=MSG::Descending)
      { 
	axisMode(k, a_);
      } 
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
} 

::A AplusGraph::yAMode(MSAlignment a_)      //  See axisSym in XGraph.H
{ return generateAxisModeSym(axisMode(a_));}


void AplusGraph::axisARule(::A sym_) // Need to look up symbol in hashtable and call axisRule()
{ 
  unsigned long 	mode = axisRule();
  
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   {
     char *style=(char *)XS(sym_->p[0])->n;
     if ((mode=(unsigned long)enumHashTable()->lookup(style))==MSNone)
      { 
	axisRule(MSNone);
      }
     else if (mode==Axis||mode==MSG::Box)
      {
	axisRule(mode);
      }
     else 
      {
	if (strpbrk(style,"x")!=0)  mode |= MSBottom;
	if (strpbrk(style,"X")!=0)  mode |= MSTop;  
	if (strpbrk(style,"y")!=0)  mode |= MSLeft; 
	if (strpbrk(style,"Y")!=0)  mode |= MSRight;
	axisRule(mode);
      }
     if(mode==MSNone) _axisARule = "none";
     else if (style) _axisARule = style;
   }
}

::A AplusGraph::axisARule(void)
{
  ::A out = gs(Et);
  *out->p=MS(si((char *)(const char *)_axisARule));
  return out;
}



void AplusGraph::gridA(::A sym_) // Need to look up symbol in hashtable and call grid()
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     unsigned long k; 
     if ((k=(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>=0&&
         (k == MSNone || k & (MSTop | MSBottom | MSRight | MSLeft)))
      { 
	grid(k);
      } 
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
} 
::A AplusGraph::gridA(void) //  See axisSym in XGraph.H
{ return generateGridSym(grid()); }


MSBoolean AplusGraph::styleConvert(LineStyle s_, unsigned long &lineStyle_, unsigned long &weight_)
{
  if (s_>=Solid&&s_<=Dot5)
    {
     
      switch(s_)
       {
       case Solid:  
         lineStyle_ = MSSolid;
         break;

       case Dash0:
       case Dash1:
       case Dash2:
       case Dash3:
       case Dash4:
         lineStyle_ = MSDash;
         break;
         
       case DotDash0:
       case DotDash1:
       case DotDash2:
       case DotDash3:
       case DotDash4:
         lineStyle_ = (MSDot | MSDash);
         break;
         
       case Dot0:
       case Dot1:
       case Dot2:
       case Dot3:
       case Dot4:
       case Dot5:
         lineStyle_ = MSDot;
         break;

       default:
         lineStyle_ = MSSolid;
         break;
       }

      switch(s_)
       {
       case Solid:  
       case Dash0:
       case DotDash0:
       case Dot0:
         weight_ = 0;
         break;

       case Dash1:
       case DotDash1:
       case Dot1:
         weight_ = 1;
         break;

       case Dash2:
       case DotDash2:
       case Dot2:
         weight_ = 2;
         break;

       case Dash3:
       case DotDash3:
       case Dot3:
         weight_ = 3;
         break;

       case Dash4:
       case DotDash4:
       case Dot4:
       case Dot5:
         weight_ = 4;
         break;

       default:
         weight_ = 0;
         break;
       }
      return MSTrue;
    }
  return MSFalse;
}



void AplusGraph::gridAStyle(::A sym_)
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   { 
     unsigned long lineStyle, weight;
     LineStyle k; 

     if ((k=(LineStyle)(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>0&&
	 styleConvert(k, lineStyle, weight)==MSTrue)
      {
	freeze();
	gridStyle(lineStyle);
	unfreeze();
	gridWeight(weight);
	_gridAStyle = (const char *) XS(sym_->p[0])->n;
      }
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   }
}

::A AplusGraph::gridAStyle(void) // call gridStyle and gridWeight
{
  ::A out = gs(Et);
  *out->p=MS(si((char *)(const char *)_gridAStyle));
  return out;
}



void AplusGraph::zeroA(::A sym_)
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     unsigned long k; 
     if ((k=(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>=0&&
         (k == MSNone || k & (MSTop | MSBottom | MSRight | MSLeft)))
      { 
	zeroAxis(k);
      } 
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
} 

::A AplusGraph::zeroA(void)
{ return generateGridSym( zeroAxis()); }

void AplusGraph::zeroAStyle(::A sym_)
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   { 
     unsigned long lineStyle, weight;
     LineStyle k; 
     if ((k=(LineStyle)(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>0&&
	 styleConvert(k, lineStyle, weight)==MSTrue)
      {
	freeze();
	zeroAxisStyle(lineStyle);
	unfreeze();
	zeroAxisWeight(weight);
	_zeroAStyle = (const char *)XS(sym_->p[0])->n;
      }
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   }
}

::A AplusGraph::zeroAStyle(void)
{
  ::A out = gs(Et);
  *out->p=MS(si((char *)(const char *)_zeroAStyle));
  return out;
}


void AplusGraph::legendAStyle(::A sym_)
{ 
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     unsigned long k; 
     if ((k=(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>0&&
	 (k>=Vertical || k<=LastValue))
      {
	legendStyle(k);
      } 
     else enumError()->showError(((::AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
} 

::A AplusGraph::legendAStyle(void)
{ return generateLegendStyleSym(legendStyle()); }

void AplusGraph::yTitleAStyle(::A sym_, MSAlignment axis_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p)))
   { 
     unsigned long k;
     if ((k=(unsigned long) enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>0&&
	 (k==MSG::Horizontal||k==MSG::Vertical))
      { 
	unsigned long oldStyle = axisTitleAlignment(axis_);
	if (k == MSG::Horizontal)
	 {
	   oldStyle|=(unsigned long) MSG::Horizontal;
	   oldStyle&= ~(unsigned long) MSG::Vertical;
	 }
	else
	 {
	   oldStyle|=(unsigned long) MSG::Vertical;
	   oldStyle&= ~(unsigned long) MSG::Horizontal;
	 }
	axisTitleAlignment(oldStyle, (unsigned long) axis_);
      }
     else enumError()->showError(v,(char *)XS(sym_->p[0])->n); 
   }
}

::A AplusGraph::yTitleAStyle(MSAlignment axis_)
{ 
  ::A r = aplus_nl;
  unsigned long style = axisTitleAlignment(axis_);
  if (style&MSG::Vertical)
  {
    r=gv(Et,1);
    r->p[0] = MS(si("ver"));
  }
  else
  {
    r=gv(Et,1);
    r->p[0] = MS(si("hor"));
  }
  return r;
}

void AplusGraph::subTitleA(::A str_)
{
  MSStringVector sv = AplusConvert::asMSStringVector(str_);
  subtitle(sv);
}

::A AplusGraph::subTitleA(void)
{
  ::A ap = aplus_nl;
  const MSStringVector &t=subtitle();
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  long xrho=t.length();
  d[0]=xrho;
  ap = ga(Et, 1, xrho, d);
  for (long i=0; i<xrho; i++)
  {
    d[0]=t.elementAt(i).length();
    ap->p[i]=(long) gc(Ct, 1, t.elementAt(i).length(), d, (long *)t.elementAt(i).string());
  }
  return ap;
}

void AplusGraph::footnoteA(::A str_) { footnote(AplusConvert::asMSStringVector(str_)); }

::A AplusGraph::footnoteA(void)
{
  ::A ap = aplus_nl;
  const MSStringVector &t=footnote();
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  long xrho=t.length();
  d[0]=xrho;
  ap = ga(Et, 1, xrho, d);
  for (long i=0; i<xrho; i++)
  {
    d[0]=t.elementAt(i).length();
    ap->p[i]=(long) gc(Ct, 1, t.elementAt(i).length(), d, (long *)t.elementAt(i).string());
  }
  return ap;
}
  
void AplusGraph::tickStyleA(::A sym_, MSAlignment axis_)
{
  if (!QS(sym_)&&(sym_->t==Et&&sym_->n>0&&QS(*sym_->p))) 
   { 
     unsigned long k; 
     if ((k=(unsigned long)enumHashTable()->lookup((char *)XS(sym_->p[0])->n))>0&&
	 (k==MSG::Inside || k==MSG::Outside))
      {
	tickStyle(k, axis_);
      } 
     else enumError()->showError(((AplusModel*)model())->aplusVar(),(char *)XS(sym_->p[0])->n); 
   } 
}

::A AplusGraph::tickStyleA(MSAlignment axis_)
{ return generateTickStyleSym(tickStyle(axis_)); }

MSBoolean AplusGraph::verifyTraceSet(::A a_)
{
  return (a_!=0&&(a_->t==Ft||a_->t==It)&&a_->r<3)?MSTrue:MSFalse;
}


MSBoolean AplusGraph::verifyData(V v_,::A a_)
{
  MSBoolean r=MSFalse;
  if (a_!=0&&QA(a_)&&a_->t==Et)
   {
     r=MSTrue;
     ::A as;
     V sv;
     ::A *p=(::A *)a_->p;

     for (int i=0; i<a_->n&&r==MSTrue; i++)
      {
	if (QS(p[i]))
	 {
           sv=(V) getVFromSym(v_->cx,(S)XS(p[i]));
	   as=(::A)gt(sv);
	   r=verifyTraceSet(as);       
	 }
	else r=MSFalse;
      }
   }
  return r;
}

void AplusGraph::addSenderNotify(MSEventSender *m_)
{ INTERNAL_COUPLE(((AplusModel *) m_)); }

void AplusGraph::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusGraph"  << endl;
     AplusEvent *ave = (AplusEvent *) &event_;
     V v     = ((AplusModel *)model())->aplusVar();
     ::A index = ave->index();
     ::A pick  = ave->pick();
     I ravel = ave->ravel();;
     update(v,index, pick, ravel);
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusGraph"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
  if (event_.type() == AplusUpdateDataEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received updateDataEvent in AplusGraph"  << endl;
     updateData();
   }
  
}

void AplusGraph::updateData(void)
{
  AplusModel *pModel=(AplusModel *)model();
  if (pModel==0)
    {
      return;
    }

  V v = pModel->aplusVar();
  if (v!=0)
    {
      ::A av = pModel->a();	// will re-evaluate the variable if necessary
      AplusTraceSet  *traceSet;
      int	     i,j;
      int	     numTraceSets=(int)av->n;  
      V  	    *vt=new V[numTraceSets];  
      ::A  	    *p=(::A *)av->p;
      MSBoolean       found=MSFalse,selectTraceFound=MSFalse;
     
      for (i=0; i<numTraceSets; i++)
	{
	  if (QS(p[i]))
	    {
	      vt[i]=(V) getVFromSym(v->cx,(S)XS(p[i]));
	      (void) gt(vt[i]);	 // ok to call gt() directly, since vt[i] is not bound to a widget yet 
	    }
	}

      MSUnsignedLongVector deleteList;
      for (i = 0; i < traceSetList().count(); i++)
	{
	  found=MSFalse;
	  traceSet=(AplusTraceSet *) graphTraceSet(i);
	  for (j=0; j<numTraceSets; j++)
	    {
	      if (((AplusModel *)traceSet->model())->aplusVar()==vt[j])
		{
		  found=MSTrue;
		  if (traceSet->trace(0)==selectTrace()) selectTraceFound=MSTrue;
		}
	    }
	  if (found==MSFalse)
	    {
	      deleteList << (unsigned long) traceSet;
	    }
	}

      // Remove from delete list
      for (i = 0; i < deleteList.length(); i++)
	{
	  traceSet = (AplusTraceSet *) deleteList(i);
	  // Removing object association from the V of the traceset
	  AplusModel *pOldModel = (AplusModel*)traceSet->model();
	  if (pOldModel && pOldModel->aplusVar() && pOldModel->pAVarData())
	    {
	      pOldModel->pAVarData()->pWidgetView(0);
	      pOldModel->aplusVar()->o = 0;
	      safeDestroy(traceSet);
	    }
	}
     
      for (i=0; i<numTraceSets; i++)
	{
	  traceSet=0;
	  for (j = 0; j < traceSetList().count(); j++)
	    {
	      traceSet=(AplusTraceSet *) graphTraceSet(j);
	      if (((AplusModel*)traceSet->model())->aplusVar()==vt[i]) break;
	      else traceSet=0;
	    }
	  if (traceSet==0)
	    {
	      traceSet=new AplusTraceSet(this);
	      if (visible()==MSTrue) traceSet->visibilityUnobscured();
	      
	      AplusModel *apm = new AplusModel(vt[i]);
	      apm->coupleWidgetView(traceSet);
	      
	      traceSet->lastDataCount(traceSet->dataCount());
	    }
	}  

      updateLegendStatus(MSTrue); 
      redrawImmediately();
      delete [] vt;
    }
}

void AplusGraph::insertChild(MSWidget *widget_) 
{ 
  if (editor()!=0&&dataWin()!=0&&legend()!=0)
   {
     MSTraceSet *ts=(MSTraceSet *)widget_;
     traceSetList().add(ts); 
     MSGraph::updateData();
     updateLegendStatus(MSTrue); 
   }
}


void AplusGraph::update(V v_,::A index_,::A,I ravel_)
{ 
  if(index_==(::A)MP(22)) update(v_,-1,-1,AppendUpdate);
  else if(!index_) update(v_,-1,-1,ShapeUpdate);
  else if(ravel_) // ravel update
   {
     ::A a=(::A)v_->a;
     if(a->r==2&&a->n==1) 
      { 
        int n=(int)a->d[1];
        int k=(int)index_->p[0];
        int j=k/n;
        update(v_,j,k-n*j,ValueUpdate); 
      }
     else update(v_,-1,-1,ValueUpdate);
   }
  else 
   {
     ::A r=index_->t==It?index_:index_->n?(::A)*index_->p:aplus_nl;
     ::A c=index_->t==Et&&index_->n>1?(::A)index_->p[1]:aplus_nl;
     if(isNull(c)==MSTrue) // c is aplus_nl - all cols are updated
      {     
        if(isNull(r)==MSTrue) update(v_,-1,-1,ValueUpdate);
        else 
         {      
           for(int i=0;i<(int)r->n;i++) update(v_,(int)r->p[i],-1,ValueUpdate);
         }
      }
     else if(isNull(r)==MSTrue) // r is aplus_nl - all rows are updated
      { 
        for(int i=0;i<(int)c->n;i++) update(v_,-1,(int)c->p[i],ValueUpdate);
      }
     else 
      {
	for(int j=0;j<(int)r->n;j++)
	 {
	   for(int i=0;i<(int)c->n;i++)
	    {
	      update(v_,(int)r->p[j],(int)c->p[i],ValueUpdate);
	    }
	 }
      }
   }
}


void AplusGraph::removeChild(MSWidget *widget_) 
{ 
  AplusTraceSet *ts=(AplusTraceSet *)widget_;
  unsigned before = traceSetList().count();
  traceSetList().remove(ts);
  unsigned after = traceSetList().count();
  if (before != after)
  {
    ts->deleteTraces();
    if (mapped()==MSTrue&&frozen()==MSFalse) updateData();
    updateLegendStatus(MSTrue); 
  }
}

void AplusGraph::printDebugInfo(const char *str_)
{
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  if (v) cout<<"ã    "<<(char*)v->cx->s->n<<"."<<(char*)v->s->n<<": "<<str_<<endl;
}


const MSSymbol& AplusGraph::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusGraph::symbol(void)
{
  static MSSymbol sym("AplusGraph");
  return sym;
}


const AplusHashTable& AplusGraph::LegendAlignConverter::enumTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(32);

  if (initialized==0)
    {
      table.notFound((unsigned long)0x5f5f5f);
      table.add("none", (void *)0);
      table.add("top", (void *)MSTop);
      table.add("bottom", (void *)MSBottom);
      table.add("left", (void *)MSLeft);
      table.add("right", (void *)MSRight);
      table.add("center", (void *)MSCenter);
      table.add("tl", (void *)(MSTop|MSLeft)); // these abbreviations are for backward compatibility only
      table.add("tr", (void *)(MSTop|MSRight));
      table.add("tc", (void *)(MSTop|MSCenter));
      table.add("bl", (void *)(MSBottom|MSLeft));
      table.add("br", (void *)(MSBottom|MSRight));
      table.add("bc", (void *)(MSBottom|MSCenter));
      table.add("inside", (void *)MSG::Inside); 
      table.add("outside", (void *)MSG::Outside);
      table.add("vertical", (void *)MSG::Vertical);
      table.add("horizontal", (void *)MSG::Horizontal);
      table.add("outsidevertical", (void *)(MSG::Outside|MSG::Vertical));
      table.add("outsidehorizontal", (void *)(MSG::Outside|MSG::Horizontal));

      initialized = 1;
    }

  return table;
}


const AplusHashTable& AplusGraph::LegendAlignConverter::stringTable(void) const
{
  static int initialized=0;
  static AplusHashTable table(32);

  if (initialized==0)
    {
      table.notFound((unsigned long)0);
      table.add((unsigned long)0, (void *)"none");
      table.add(MSTop, (void *)"top");
      table.add(MSBottom, (void *)"bottom");
      table.add(MSLeft, (void *)"left");
      table.add(MSRight, (void *)"right");
      table.add(MSCenter, (void *)"center");
      table.add(MSTop|MSLeft, (void *)"tl");
      table.add(MSTop|MSRight, (void *)"tr");
      table.add(MSTop|MSCenter, (void *)"tc");
      table.add(MSBottom|MSLeft, (void *)"bl");
      table.add(MSBottom|MSRight, (void *)"br");
      table.add(MSBottom|MSCenter, (void *)"bc");
      table.add(MSG::Inside, (void *)"inside");
      table.add(MSG::Outside, (void *)"outside");
      table.add(MSG::Vertical, (void *)"vertical");
      table.add(MSG::Horizontal, (void *)"horizontal");
      table.add(MSG::Outside|MSG::Vertical, (void *)"outsidevertical");
      table.add(MSG::Outside|MSG::Horizontal, (void *)"outsidehorizontal");

      initialized = 1;
    }

  return table;
}


const char *AplusGraph::LegendAlignConverter::type(void) const
{
  return "graphstyle";
}
