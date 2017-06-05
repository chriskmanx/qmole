/********************************************************************************
*                                                                               *
*                     W U   C o l o r   Q u a n t i z a t i o n                 *
*                                                                               *
*********************************************************************************
* Copyright (C) 2004,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: fxwuquantize.cpp,v 1.9.2.2 2006/08/02 01:31:11 fox Exp $                     *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxpriv.h"

/*
  Notes:

  - This code is due to: Xiaolin Wu, Dept. of Computer Science, Univ. of
    Western Ontario, London, Ontario N6A 5B7 (wu@csd.uwo.ca).
    Original algorithm can be found in Graphics Gems vol. II, pp. 126-133.

  - Algorithm: Greedy orthogonal bipartition of RGB space for variance
    minimization aided by inclusion-exclusion tricks.
    For speed no nearest neighbor search is done. Slightly
    better performance can be expected by more sophisticated
    but more expensive versions.

  - Modified by Jeroen for FOX; don't blame the original author if I
    broke it.

*/

#define MAXCOLOR  256
#define	RED       2
#define	GREEN     1
#define BLUE      0


using namespace FX;

/*******************************************************************************/

namespace FX {


// Sub bpx
struct box {
  FXint r0;                     // Min value, exclusive
  FXint r1;                     // Max value, inclusive
  FXint g0;
  FXint g1;
  FXint b0;
  FXint b1;
  FXint vol;                    // Volume
  };


// To pass around
struct WU {
  FXfloat m2[33][33][33];       // Histogram is in elements 1..HISTSIZE along each axis,
  FXint   wt[33][33][33];       // element 0 is for base or marginal value
  FXint   mr[33][33][33];       // NB: these must start out 0!
  FXint   mg[33][33][33];
  FXint   mb[33][33][33];
  };



// At conclusion of the histogram step, we can interpret
//   wt[r][g][b] = sum over voxel of P(c)
//   mr[r][g][b] = sum over voxel of r*P(c)  ,  similarly for mg, mb
//   m2[r][g][b] = sum over voxel of c^2*P(c)
// Actually each of these should be divided by 'size' to give the usual
// interpretation of P() as ranging from 0 to 1, but we needn't do that here.

// Build 3-D color histogram of counts, r/g/b, c^2
static void histogram(WU& wu,const FXColor *data,FXint size){
  register FXint r,g,b,inr,ing,inb,i;

  // Clear counters
  memset(&wu,0,sizeof(wu));

  // Build histogram
  for(i=0; i<size; ++i){
    r=((const FXuchar*)(data+i))[0];
    g=((const FXuchar*)(data+i))[1];
    b=((const FXuchar*)(data+i))[2];
    inr=(r>>3)+1;
    ing=(g>>3)+1;
    inb=(b>>3)+1;
    wu.wt[inr][ing][inb]+=1;
    wu.mr[inr][ing][inb]+=r;
    wu.mg[inr][ing][inb]+=g;
    wu.mb[inr][ing][inb]+=b;
    wu.m2[inr][ing][inb]+=(FXfloat)(r*r+g*g+b*b);
    }
  }


// Compute cumulative moments
static void moments(WU& wu){
  register FXint linet,liner,lineg,lineb,i,r,g,b;
  FXint areat[33],arear[33],areag[33],areab[33];
  FXfloat line2,area2[33];
  for(r=1; r<=32; ++r){
    for(i=0; i<=32; ++i){
      areat[i]=0;
      arear[i]=0;
      areag[i]=0;
      areab[i]=0;
      area2[i]=0.0f;
      }
    for(g=1; g<=32; ++g){
      linet=0;
      liner=0;
      lineg=0;
      lineb=0;
      line2=0.0f;
      for(b=1; b<=32; ++b){
        linet+=wu.wt[r][g][b];
        liner+=wu.mr[r][g][b];
        lineg+=wu.mg[r][g][b];
        lineb+=wu.mb[r][g][b];
        line2+=wu.m2[r][g][b];
        areat[b]+=linet;
        arear[b]+=liner;
        areag[b]+=lineg;
        areab[b]+=lineb;
        area2[b]+=line2;
        wu.wt[r][g][b]=wu.wt[r-1][g][b]+areat[b];
        wu.mr[r][g][b]=wu.mr[r-1][g][b]+arear[b];
        wu.mg[r][g][b]=wu.mg[r-1][g][b]+areag[b];
        wu.mb[r][g][b]=wu.mb[r-1][g][b]+areab[b];
        wu.m2[r][g][b]=wu.m2[r-1][g][b]+area2[b];
        }
      }
    }
  }


// Compute sum over a box of any given statistic
static int volume(box& cube,FXint mmt[33][33][33]){
  return  mmt[cube.r1][cube.g1][cube.b1]
         -mmt[cube.r1][cube.g1][cube.b0]
         -mmt[cube.r1][cube.g0][cube.b1]
         +mmt[cube.r1][cube.g0][cube.b0]
         -mmt[cube.r0][cube.g1][cube.b1]
         +mmt[cube.r0][cube.g1][cube.b0]
         +mmt[cube.r0][cube.g0][cube.b1]
         -mmt[cube.r0][cube.g0][cube.b0];
  }


// The next two routines allow a slightly more efficient calculation
// of Vol() for a proposed subbox of a given box.  The sum of Top()
// and Bottom() is the Vol() of a subbox split in the given direction
// and with the specified new upper bound.

// Compute part of Vol(cube, mmt) that doesn't depend
// on r1, g1, or b1 (depending on dir)
static FXint bottom(box& cube,FXuchar dir,FXint mmt[33][33][33]){
  register FXint result=0;
  switch(dir){
    case RED:
      result= -mmt[cube.r0][cube.g1][cube.b1]
              +mmt[cube.r0][cube.g1][cube.b0]
              +mmt[cube.r0][cube.g0][cube.b1]
              -mmt[cube.r0][cube.g0][cube.b0];
      break;
    case GREEN:
      result= -mmt[cube.r1][cube.g0][cube.b1]
              +mmt[cube.r1][cube.g0][cube.b0]
              +mmt[cube.r0][cube.g0][cube.b1]
              -mmt[cube.r0][cube.g0][cube.b0];
      break;
    case BLUE:
      result= -mmt[cube.r1][cube.g1][cube.b0]
              +mmt[cube.r1][cube.g0][cube.b0]
              +mmt[cube.r0][cube.g1][cube.b0]
              -mmt[cube.r0][cube.g0][cube.b0];
      break;
    }
  return result;
  }


// Compute remainder of Vol(cube, mmt), substituting pos
// for r1, g1, or b1 (depending on dir)
static FXint top(box& cube,FXuchar dir,FXint pos,FXint mmt[33][33][33]){
  register FXint result=0;
  switch(dir){
    case RED:
      result= mmt[pos][cube.g1][cube.b1]
             -mmt[pos][cube.g1][cube.b0]
             -mmt[pos][cube.g0][cube.b1]
             +mmt[pos][cube.g0][cube.b0];
      break;
    case GREEN:
      result= mmt[cube.r1][pos][cube.b1]
             -mmt[cube.r1][pos][cube.b0]
             -mmt[cube.r0][pos][cube.b1]
             +mmt[cube.r0][pos][cube.b0];
      break;
    case BLUE:
      result= mmt[cube.r1][cube.g1][pos]
             -mmt[cube.r1][cube.g0][pos]
             -mmt[cube.r0][cube.g1][pos]
             +mmt[cube.r0][cube.g0][pos];
      break;
    }
  return result;
  }


// Compute the weighted variance of a box
// NB: as with the raw statistics, this is really the variance * size
static FXfloat variance(WU& wu,box& cube){
  register FXfloat dr,dg,db,xx;

  dr = (FXfloat)volume(cube,wu.mr);
  dg = (FXfloat)volume(cube,wu.mg);
  db = (FXfloat)volume(cube,wu.mb);

  xx =  wu.m2[cube.r1][cube.g1][cube.b1]
       -wu.m2[cube.r1][cube.g1][cube.b0]
       -wu.m2[cube.r1][cube.g0][cube.b1]
       +wu.m2[cube.r1][cube.g0][cube.b0]
       -wu.m2[cube.r0][cube.g1][cube.b1]
       +wu.m2[cube.r0][cube.g1][cube.b0]
       +wu.m2[cube.r0][cube.g0][cube.b1]
       -wu.m2[cube.r0][cube.g0][cube.b0];

  return xx-(dr*dr+dg*dg+db*db)/(FXfloat)volume(cube,wu.wt);
  }


// We want to minimize the sum of the variances of two subboxes.
// The sum(c^2) terms can be ignored since their sum over both subboxes
// is the same (the sum for the whole box) no matter where we split.
// The remaining terms have a minus sign in the variance formula,
// so we drop the minus sign and MAXIMIZE the sum of the two terms.
static FXfloat maximize(WU& wu,box& cube,FXuchar dir,FXint first,FXint last,FXint *cut,FXint whole_r,FXint whole_g,FXint whole_b,FXint whole_w){
  register FXint half_r,half_g,half_b,half_w,base_r,base_g,base_b,base_w,i;
  register FXfloat temp,max;

  base_r=bottom(cube,dir,wu.mr);
  base_g=bottom(cube,dir,wu.mg);
  base_b=bottom(cube,dir,wu.mb);
  base_w=bottom(cube,dir,wu.wt);

  max=0.0f;
  *cut=-1;
  for(i=first; i<last; ++i){

    half_r=base_r+top(cube,dir,i,wu.mr);
    half_g=base_g+top(cube,dir,i,wu.mg);
    half_b=base_b+top(cube,dir,i,wu.mb);
    half_w=base_w+top(cube,dir,i,wu.wt);

    // Now half_x is sum over lower half of box, if split at i

    // Subbox could be empty of pixels; never split into an empty box
    if(half_w==0) continue;

    temp=((FXfloat)half_r*half_r + (FXfloat)half_g*half_g + (FXfloat)half_b*half_b)/half_w;

    half_r=whole_r-half_r;
    half_g=whole_g-half_g;
    half_b=whole_b-half_b;
    half_w=whole_w-half_w;

    // Subbox could be empty of pixels; never split into an empty box
    if(half_w==0) continue;

    temp += ((FXfloat)half_r*half_r + (FXfloat)half_g*half_g + (FXfloat)half_b*half_b)/half_w;

    if(temp>max){ max=temp; *cut=i; }
    }
  return max;
  }


// Find best split
static FXint cut(WU& wu,box& set1,box& set2){
  FXint cutr, cutg, cutb;
  FXfloat maxr, maxg, maxb;
  FXint whole_r, whole_g, whole_b, whole_w;
  FXuchar dir;

  // Totals
  whole_r=volume(set1,wu.mr);
  whole_g=volume(set1,wu.mg);
  whole_b=volume(set1,wu.mb);
  whole_w=volume(set1,wu.wt);

  // Find most beneficial split direction
  maxr=maximize(wu,set1,  RED,set1.r0+1,set1.r1,&cutr,whole_r,whole_g,whole_b,whole_w);
  maxg=maximize(wu,set1,GREEN,set1.g0+1,set1.g1,&cutg,whole_r,whole_g,whole_b,whole_w);
  maxb=maximize(wu,set1, BLUE,set1.b0+1,set1.b1,&cutb,whole_r,whole_g,whole_b,whole_w);

  // Direction of split?
  if((maxr>=maxg) && (maxr>=maxb)){
    if(cutr<0) return 0;        // Can't split the box
    dir=RED;
    }
  else if((maxg>=maxr) && (maxg>=maxb)){
    dir=GREEN;
    }
  else{
    dir=BLUE;
    }

  set2.r1=set1.r1;
  set2.g1=set1.g1;
  set2.b1=set1.b1;

  switch(dir){
    case RED:
      set2.r0=set1.r1=cutr;
      set2.g0=set1.g0;
      set2.b0=set1.b0;
      break;
    case GREEN:
      set2.g0=set1.g1=cutg;
      set2.r0=set1.r0;
      set2.b0=set1.b0;
      break;
    case BLUE:
      set2.b0=set1.b1=cutb;
      set2.r0=set1.r0;
      set2.g0=set1.g0;
      break;
    }

  set1.vol=(set1.r1-set1.r0)*(set1.g1-set1.g0)*(set1.b1-set1.b0);
  set2.vol=(set2.r1-set2.r0)*(set2.g1-set2.g0)*(set2.b1-set2.b0);
  return 1;
  }


// Each entry in box maps to label
static void mark(box& cube,FXint label,FXuchar map[33][33][33]){
  register FXint r,g,b;
  for(r=cube.r0+1; r<=cube.r1; ++r){
    for(g=cube.g0+1; g<=cube.g1; ++g){
      for(b=cube.b0+1; b<=cube.b1; ++b){
        map[r][g][b]=label;
        }
      }
    }
  }


// Wu's quantization method based on recursive partitioning
FXbool fxwuquantize(FXuchar* dst,const FXColor* src,FXColor* colormap,FXint& actualcolors,FXint w,FXint h,FXint maxcolors){
  register FXint i,k,weight,next,size,r,g,b;
  register FXfloat temp;
  FXuchar  map[33][33][33];
  FXfloat  vv[MAXCOLOR];
  box      cube[MAXCOLOR];
  WU       wu;

  // Size of image
  size=w*h;

  // Compute histogram
  histogram(wu,src,size);
  //FXTRACE((100,"done hist\n"));

  // Compute moments
  moments(wu);

  // Recursively split boxes
  next=0;
  cube[0].r0=cube[0].g0=cube[0].b0=0;
  cube[0].r1=cube[0].g1=cube[0].b1=32;
  for(i=1; i<maxcolors; ++i){
    if(cut(wu,cube[next],cube[i])){
      vv[next]=(cube[next].vol>1)?variance(wu,cube[next]):0.0f; // Volume test ensures we won't try to cut one-cell box
      vv[i]=(cube[i].vol>1)?variance(wu,cube[i]):0.0f;
      }
    else{
      vv[next]=0.0f;                                            // Don't try to split this box again
      i--;                                                      // Didn't create box i
      }
    next=0;
    temp=vv[0];
    for(k=1; k<=i; ++k){
      if(vv[k]>temp){
        temp=vv[k];
        next=k;
        }
      }
    if(temp<=0.0f){
      maxcolors=i+1;
      //FXTRACE((100,"Only got %d boxes\n",maxcolors));
      break;
      }
    }

  //FXTRACE((100,"done partition\n"));

  // Construct colormap
  for(k=0; k<maxcolors; ++k){
    mark(cube[k],k,map);
    weight=volume(cube[k],wu.wt);
    if(weight){
      ((FXuchar*)(colormap+k))[0]=volume(cube[k],wu.mr)/weight;
      ((FXuchar*)(colormap+k))[1]=volume(cube[k],wu.mg)/weight;
      ((FXuchar*)(colormap+k))[2]=volume(cube[k],wu.mb)/weight;
      ((FXuchar*)(colormap+k))[3]=255;
      }
    else{
      //FXTRACE((1,"bogus box %d\n",k));
      ((FXuchar*)(colormap+k))[0]=0;
      ((FXuchar*)(colormap+k))[1]=0;
      ((FXuchar*)(colormap+k))[2]=0;
      ((FXuchar*)(colormap+k))[3]=0;
      }
    }

  //FXTRACE((100,"done mapping\n"));

  // Build histogram
  for(i=0; i<size; ++i){
    r=((const FXuchar*)(src+i))[0];
    g=((const FXuchar*)(src+i))[1];
    b=((const FXuchar*)(src+i))[2];
    dst[i]=map[(r>>3)+1][(g>>3)+1][(b>>3)+1];
    }

  //FXTRACE((100,"done transform\n"));

  // Return actual number of colors
  actualcolors=maxcolors;

  return TRUE;
  }

}
