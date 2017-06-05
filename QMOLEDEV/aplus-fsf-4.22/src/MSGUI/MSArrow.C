///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSArrow.H>

MSArrow::MSArrow(MSWidgetCommon *owner_)
: _pShadow(0)
{
  _owner=owner_;
  _arrowType=Up;
  init();
}

MSArrow::MSArrow(MSWidgetCommon *owner_,ArrowType type_)
: _pShadow(0)
{
  _owner=owner_;
  _arrowType=type_;
  init();
}

MSArrow::~MSArrow(void)
{
  if (_top!=0)
   {
     delete [] _top;      
     delete [] _center;   
     delete [] _bottom;   
   }
  if(_pShadow !=0) delete _pShadow;
}

void MSArrow::init(void)
{
  _top=0;
  _center=0;
  _bottom=0;
  _topCount=_centerCount=_bottomCount=0;
  _selected=MSFalse;
  MSRect::configuration(0,0,0,0);
}

void MSArrow::type(ArrowType type_)
{
  _arrowType=type_;
  computeSize();  
}

void MSArrow::owner(MSWidgetCommon *owner_)
{
  if (_owner!=owner_)
   {
     _owner=owner_;
     computeSize();
   }
}

void MSArrow::computeSize(void)
{
  if (MSRect::width()!=0&&MSRect::height()!=0)
   {
    int offset=_owner->highlightThickness()+_owner->shadowThickness();
    computeArrowRects();
    offsetArrow(MSRect::x()-offset,MSRect::y()-offset);
  }
}

void MSArrow::draw(void)
{
  if (_top==0) return;

  GC topGC=shadow().topShadowGC();
  GC bottomGC=shadow().bottomShadowGC();

  if (_selected)
   {
     topGC= shadow().bottomShadowGC();
     bottomGC= shadow().topShadowGC();
   }

  Display *dpy=_owner->display();
  Window wid=_owner->window();
  _owner->XBFillRectangles(dpy,wid,topGC,_top,_topCount);
  _owner->XBFillRectangles(dpy,wid,bottomGC,_bottom,_bottomCount);
  _owner->XBFillRectangles(dpy,wid,shadow().backgroundShadowGC(),_center,_centerCount);
}

void MSArrow::computeArrowRects(void)
{
  int ht=_owner->highlightThickness();
  int st=_owner->shadowThickness();
  int w=MSRect::width()+2*(ht+st);
  int h=MSRect::height()+2*(ht+st);
  int size,Width,start;
  int Y;
  int temp;
  short t=0;
  short b=0;
  short c=0;
  int xOffset=0;
  int yOffset=0;
  XRectangle *tmp;

  st-=1;
  
  if (_top!=0)
   {
     delete [] _top;    _top =0;
     delete [] _center; _center=0;
     delete [] _bottom; _bottom =0;
     _topCount=0;
     _centerCount=0;
     _bottomCount=0;
   }

  // Get the size and allocate the rectangle lists  
  if (w>h) 
   {
     size=h-2-((ht+st)<<1);
     xOffset=(w-h)>>1;
   }
  else
   {
     size=w-2-((ht+st)<<1);
     yOffset=(h-w)>>1;
   }
  
  if (size<1) return;
  
  if (_arrowType==Right||_arrowType==Left)
   {
     temp=xOffset;
     xOffset=yOffset;
     yOffset=temp;
   }
  
  int nr=(size>>1)+6;
  _top =(XRectangle *) new XRectangle[nr];
  _center=(XRectangle *) new XRectangle[nr];
  _bottom =(XRectangle *) new XRectangle[nr];
  
  // Set up a loop to generate the segments.  
  Width=size;
  Y=size+ht+st-1+yOffset;
  start=ht+st+1+xOffset;
  
  while (Width>0)
   {
     if (Width==1)
      {
	_top[t].x=start; _top[t].y=Y+1;
	_top[t].width=1; _top[t].height=1;
	t++;
      }
     else if (Width==2)
      {
	if (size==2||(_arrowType==Up||_arrowType==Left))
         {
	   _top[t].x=start; _top[t].y=Y;
	   _top[t].width=2; _top[t].height=1;
	   t++;
	   _top[t].x=start; _top[t].y=Y+1;
	   _top[t].width=1; _top[t].height=1;
	   t++;
	   _bottom[b].x=start+1; _bottom[b].y=Y+1;
	   _bottom[b].width=1; _bottom[b].height=1;
	   b++;
         }
	else if (_arrowType==Up||_arrowType==Left)
         {
	   _top[t].x=start; _top[t].y=Y;
	   _top[t].width=2; _top[t].height=1;
	   t++;
	   _bottom[b].x=start; _bottom[b].y=Y+1;
	   _bottom[b].width=2; _bottom[b].height=1;
	   b++;
         }
      }
     else
      {
	if (start==ht+st+1+xOffset)
         {
	   if (_arrowType==Up||_arrowType==Left)
            {
	      _top[t].x=start; _top[t].y=Y;
	      _top[t].width=2; _top[t].height=1;
	      t++;
	      _top[t].x=start; _top[t].y=Y+1;
	      _top[t].width=1; _top[t].height=1;
	      t++;
	      _bottom[b].x=start+1; _bottom[b].y=Y+1;
	      _bottom[b].width=1; _bottom[b].height=1;
	      b++;
	      _bottom[b].x=start+2; _bottom[b].y=Y;
	      _bottom[b].width=Width-2; _bottom[b].height=2;
	      b++;
            }
	   else
            {
	      _top[t].x=start; _top[t].y=Y;
	      _top[t].width=2; _top[t].height=1;
	      t++;
	      _bottom[b].x=start; _bottom[b].y=Y+1;
	      _bottom[b].width=2; _bottom[b].height=1;
	      b++;
	      _bottom[b].x=start+2; _bottom[b].y=Y;
	      _bottom[b].width=Width-2; _bottom[b].height=2;
	      b++;
            }
         }
	else
         {
	   _top[t].x=start; _top[t].y=Y;
	   _top[t].width=2; _top[t].height=2;
	   t++;
	   _bottom[b].x=start+Width-2; _bottom[b].y=Y;
	   _bottom[b].width=2; _bottom[b].height=2;
	   if (Width==3)
            {
	      _bottom[b].width=1;
	      _bottom[b].x+=1;
            }
	   b++;
	   if (Width>4)
            {
	      _center[c].x=start+2; _center[c].y=Y;
	      _center[c].width=Width-4; _center[c].height=2;
	      c++;
            }
         }
      }
     start++;
     Width-=2;
     Y-=2;
   }
  
  if (_arrowType==Up||_arrowType==Left)
   {
     _topCount=t;
     _centerCount=c;
     _bottomCount=b;
   }
  else
   {   
     tmp=_top;
     _top=_bottom;
     _bottom=tmp;
     _topCount=b;
     _centerCount=c;
     _bottomCount=t;
   }
  
  //  Transform the "up" pointing arrow to the correct _arrowType  
  switch (_arrowType)
   {
   case Left:
     {
       int i=-1; 
       do
	{
	  i++;
	  if (i<_topCount)
	   {
	     temp=_top[i].y; _top[i].y=_top[i].x; _top[i].x=temp;
	     temp=_top[i].width; 
	     _top[i].width=_top[i].height; _top[i].height=temp;
	   }             
	  if (i<_bottomCount)
	   {
	     temp=_bottom[i].y; _bottom[i].y=_bottom[i].x; _bottom[i].x=temp;
	     temp=_bottom[i].width; 
	     _bottom[i].width=_bottom[i].height; _bottom[i].height=temp;
	   }             
	  if (i<_centerCount)
	   {
	     temp=_center[i].y; _center[i].y=_center[i].x; _center[i].x=temp;
	     temp=_center[i].width; 
	     _center[i].width=_center[i].height; _center[i].height=temp;
	   }             
	}
       while (i<_topCount||i<_bottomCount||i<_centerCount);
     }
     break;
     
   case Right:
     {
       int h_right=h-2;
       int w_right=w-2;
       int i=-1; 

       do
	{
	  i++;
	  if (i<_topCount)
	   {
	     temp=_top[i].y; _top[i].y=_top[i].x; 
	     _top[i].x=temp; 
	     temp=_top[i].width; _top[i].width=_top[i].height; 
	     _top[i].height=temp;
	     _top[i].x=w_right-_top[i].x-_top[i].width+2;
	     _top[i].y=h_right-_top[i].y-_top[i].height+2;
	   }             
	  if (i<_bottomCount)
	   {
	     temp=_bottom[i].y; _bottom[i].y=_bottom[i].x; 
	     _bottom[i].x=temp; 
	     temp=_bottom[i].width; _bottom[i].width=_bottom[i].height; 
	     _bottom[i].height=temp;
	     _bottom[i].x=w_right-_bottom[i].x-_bottom[i].width+2;
	     _bottom[i].y=h_right-_bottom[i].y-_bottom[i].height+2;
	   }             
	  if (i<_centerCount)
	   {
	     temp=_center[i].y; _center[i].y=_center[i].x; 
	     _center[i].x=temp; 
	     temp=_center[i].width; _center[i].width=_center[i].height;
	     _center[i].height=temp;
	     _center[i].x=w_right-_center[i].x-_center[i].width+2;
	     _center[i].y=h_right-_center[i].y-_center[i].height+2;
	   }
	}
       while (i<_topCount||i<_bottomCount||i<_centerCount);
     }
     break;
     
   case Up:
     break;
     
   case Down:
     {
       int w_down=w-2;
       int h_down=h-2;
       int i=-1; 

       do
	{
	  i++;
	  if (i<_topCount)
	   {
	     _top[i].x=w_down-_top[i].x-_top[i].width+2;
	     _top[i].y=h_down-_top[i].y-_top[i].height+2;
	   }
	  if (i<_bottomCount)
	   {
	     _bottom[i].x=w_down-_bottom[i].x-_bottom[i].width+2;
	     _bottom[i].y=h_down-_bottom[i].y-_bottom[i].height+2;
	   }
	  if (i<_centerCount)
	   {
	     _center[i].x=w_down-_center[i].x-_center[i].width+2;
	     _center[i].y=h_down-_center[i].y-_center[i].height+2;
	   }
	}
       while (i<_topCount||i<_bottomCount||i<_centerCount);
     }
     break;
   }
}

void MSArrow::offsetArrow(int diff_x,int diff_y)
{
  int i;
  if (diff_x!=0||diff_y!=0)
   {
     for (i=0;i<_topCount;i++)
      {
        _top[i].x+=diff_x;
        _top[i].y+=diff_y;
      }
     for (i=0;i<_centerCount;i++)
      {
        _center[i].x+=diff_x;
        _center[i].y+=diff_y;
      }
     for (i=0;i<_bottomCount;i++)
      {
        _bottom[i].x+=diff_x;
        _bottom[i].y+=diff_y;
      }
   }
}

void MSArrow::moveTo(int x_,int y_)
{
  int i;
  int diff_y=y_-MSRect::y();
  int diff_x=x_-MSRect::x();

  if (diff_x!=0||diff_y!=0)
   {
     MSRect::y(y_);
     MSRect::x(x_);     
     for (i=0;i<_topCount;i++)
      {
        _top[i].x+=diff_x;
        _top[i].y+=diff_y;
      }
     for (i=0; i<_centerCount;i++)
      {
        _center[i].x+=diff_x;
        _center[i].y+=diff_y;
      }
     for (i=0;i<_bottomCount;i++)
      {
        _bottom[i].x+=diff_x;
        _bottom[i].y+=diff_y;
      }
   }
}

void MSArrow::y_origin(int y_)
{
  int i;
  int diff_y=y_-MSRect::y();

  if (diff_y!=0)
  {
     MSRect::y(y_);
     for (i=0;i<_topCount;i++)    _top[i].y+=diff_y;
     for (i=0;i<_centerCount;i++) _center[i].y+=diff_y;
     for (i=0;i<_bottomCount;i++) _bottom[i].y+=diff_y;
  }
}


void MSArrow::x_origin(int x_)
{
  int i;
  int diff_x=x_-MSRect::x();

  if (diff_x!=0)
   {
     MSRect::x(x_);     
     for (i=0;i<_topCount;i++)    _top[i].x+=diff_x;
     for (i=0;i<_centerCount;i++) _center[i].x+=diff_x;
     for (i=0;i<_bottomCount;i++) _bottom[i].x+=diff_x;
   }
}

void MSArrow::resize(int w_,int h_) 
{ 
  if (w_!=width()||h_!=height())
   {
     MSRect::width(w_); 
     MSRect::height(h_); 
     computeSize();
   } 
}

void MSArrow::configure(int x_,int y_,int w_,int h_)
{ 
  if (x_!=x()||y_!=y()||w_!=width()||h_!=height())
   {
     configuration(x_,y_,w_,h_); 
     computeSize(); 
   }
}

MSShadow& MSArrow::shadow(void)
{
  if(_pShadow == 0) return _owner->shadow();
  else  return *_pShadow;
}

const MSShadow& MSArrow::shadow(void) const
{
  if(_pShadow == 0) return _owner->shadow();
  else  return *_pShadow;
}

unsigned long MSArrow::color(void) const
{ return shadow().background(); }

void MSArrow::color(const char * color_)
{ color(owner()->server()->pixel(color_)); }

void MSArrow::color(unsigned long pixel_)
{
  if( _pShadow == 0) _pShadow = new MSShadow(_owner);
  _pShadow->color(pixel_);
}
