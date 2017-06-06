/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                        ZZZZZ   OOO    OOO   M   M                           %
%                           ZZ  O   O  O   O  MM MM                           %
%                         ZZZ   O   O  O   O  M M M                           %
%                        ZZ     O   O  O   O  M   M                           %
%                        ZZZZZ   OOO    OOO   M   M                           %
%                                                                             %
%                                                                             %
%                      ImageMagick Image Zoom Methods                         %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                                 July 1992                                   %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   B e s s e l O r d e r O n e                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method BesselOrderOne computes the Bessel function of x of the first kind
%  of order 0:
%
%    Reduce x to |x| since j1(x)= -j1(-x), and for x in (0,8]
%
%       j1(x) = x*j1(x);
%
%    For x in (8,inf)
%
%       j1(x) = sqrt(2/(pi*x))*(p1(x)*cos(x1)-q1(x)*sin(x1))
%
%    where x1 = x-3*pi/4. Compute sin(x1) and cos(x1) as follow:
%
%       cos(x1) =  cos(x)cos(3pi/4)+sin(x)sin(3pi/4)
%               =  1/sqrt(2) * (sin(x) - cos(x))
%       sin(x1) =  sin(x)cos(3pi/4)-cos(x)sin(3pi/4)
%               = -1/sqrt(2) * (sin(x) + cos(x))
%
%  The format of the BesselOrderOne method is:
%
%      Image *MagnifyImage(Image *image)
%
%  A description of each parameter follows:
%
%    o value: Method BesselOrderOne returns the Bessel function of x of the
%      first kind of orders 1.
%
%    o x: double value.
%
%
*/

static double J1(double x)
{
  double
    p,
    q;

  register int
    i;

  static const double
    Pone[] =
    {
       0.581199354001606143928050809e+21,
      -0.6672106568924916298020941484e+20,
       0.2316433580634002297931815435e+19,
      -0.3588817569910106050743641413e+17,
       0.2908795263834775409737601689e+15,
      -0.1322983480332126453125473247e+13,
       0.3413234182301700539091292655e+10,
      -0.4695753530642995859767162166e+7,
       0.270112271089232341485679099e+4
    },
    Qone[] =
    {
      0.11623987080032122878585294e+22,
      0.1185770712190320999837113348e+20,
      0.6092061398917521746105196863e+17,
      0.2081661221307607351240184229e+15,
      0.5243710262167649715406728642e+12,
      0.1013863514358673989967045588e+10,
      0.1501793594998585505921097578e+7,
      0.1606931573481487801970916749e+4,
      0.1e+1
    };

  p=Pone[8];
  q=Qone[8];
  for (i=7; i >= 0; i--)
  {
    p=p*x*x+Pone[i];
    q=q*x*x+Qone[i];
  }
  return(p/q);
}

static double P1(double x)
{
  double
    p,
    q;

  register int
    i;

  static const double
    Pone[] =
    {
      0.352246649133679798341724373e+5,
      0.62758845247161281269005675e+5,
      0.313539631109159574238669888e+5,
      0.49854832060594338434500455e+4,
      0.2111529182853962382105718e+3,
      0.12571716929145341558495e+1
    },
    Qone[] =
    {
      0.352246649133679798068390431e+5,
      0.626943469593560511888833731e+5,
      0.312404063819041039923015703e+5,
      0.4930396490181088979386097e+4,
      0.2030775189134759322293574e+3,
      0.1e+1
    };

  p=Pone[5];
  q=Qone[5];
  for (i=4; i >= 0; i--)
  {
    p=p*(8.0/x)*(8.0/x)+Pone[i];
    q=q*(8.0/x)*(8.0/x)+Qone[i];
  }
  return(p/q);
}

static double Q1(double x)
{
  double
    p,
    q;

  register int
    i;

  static const double
    Pone[] =
    {
      0.3511751914303552822533318e+3,
      0.7210391804904475039280863e+3,
      0.4259873011654442389886993e+3,
      0.831898957673850827325226e+2,
      0.45681716295512267064405e+1,
      0.3532840052740123642735e-1
    },
    Qone[] =
    {
      0.74917374171809127714519505e+4,
      0.154141773392650970499848051e+5,
      0.91522317015169922705904727e+4,
      0.18111867005523513506724158e+4,
      0.1038187585462133728776636e+3,
      0.1e+1
    };

  p=Pone[5];
  q=Qone[5];
  for (i=4; i >= 0; i--)
  {
    p=p*(8.0/x)*(8.0/x)+Pone[i];
    q=q*(8.0/x)*(8.0/x)+Qone[i];
  }
  return(p/q);
}

static double BesselOrderOne(double x)
{
  double
    p,
    q;

  if (x == 0.0)
    return(0.0);
  p=x;
  if (x < 0.0)
    x=(-x);
  if (x < 8.0)
    return(p*J1(x));
  q=sqrt(2.0/(M_PI*x))*(P1(x)*(1.0/sqrt(2.0)*(sin(x)-cos(x)))-8.0/x*Q1(x)*
    (-1.0/sqrt(2.0)*(sin(x)+cos(x))));
  if (p < 0.0)
    q=(-q);
  return(q);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M a g n i f y I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MagnifyImage creates a new image that is a integral size greater
%  than an existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  MagnifyImage scans the reference image to create a magnified image by
%  bilinear interpolation.  The magnified image columns and rows become:
%
%    number_columns << 1
%    number_rows << 1
%
%  The format of the MagnifyImage method is:
%
%      magnified_image=MagnifyImage(image)
%
%  A description of each parameter follows:
%
%    o magnified_image: Method MagnifyImage returns a pointer to the image
%      after magnification.  A null image is returned if there is a memory
%      shortage.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export Image *MagnifyImage(Image *image)
{
#define MagnifyImageText  "  Magnifying the image...  "

  Image
    *magnified_image;

  int
    y;

  register int
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *r;

  /*
    Initialize magnified image attributes.
  */
  assert(image != (Image *) NULL);
  magnified_image=CloneImage(image,image->columns << 1,image->rows << 1,False);
  if (magnified_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to zoom image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  magnified_image->class=DirectClass;
  /*
    Initialize zoom image pixels.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=magnified_image->pixels;
  for (y=0; y < (int) image->rows; y++)
  {
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *q=(*p);
      q->length=0;
      q++;
    }
    q+=image->columns;
  }
  /*
    Magnify each row.
  */
  for (y=0; y < (int) image->rows; y++)
  {
    p=magnified_image->pixels+(image->rows-1-y)*magnified_image->columns+
      (image->columns-1);
    q=magnified_image->pixels+((image->rows-1-y) << 1)*magnified_image->columns+
      ((image->columns-1) << 1);
    *q=(*p);
    *(q+1)=(*(p));
    for (x=1; x < (int) image->columns; x++)
    {
      p--;
      q-=2;
      *q=(*p);
      (q+1)->red=(((int) p->red)+((int) (p+1)->red)+1) >> 1;
      (q+1)->green=(((int) p->green)+((int) (p+1)->green)+1) >> 1;
      (q+1)->blue=(((int) p->blue)+((int) (p+1)->blue)+1) >> 1;
      (q+1)->index=(((int) p->index)+((int) (p+1)->index)+1) >> 1;
      (q+1)->length=0;
    }
  }
  for (y=0; y < (int) (image->rows-1); y++)
  {
    p=magnified_image->pixels+(y << 1)*magnified_image->columns;
    q=p+magnified_image->columns;
    r=q+magnified_image->columns;
    for (x=0; x < (int) (image->columns-1); x++)
    {
      q->red=(((int) p->red)+((int) r->red)+1) >> 1;
      q->green=(((int) p->green)+((int) r->green)+1) >> 1;
      q->blue=(((int) p->blue)+((int) r->blue)+1) >> 1;
      q->index=(((int) p->index)+((int) r->index)+1) >> 1;
      q->length=0;
      (q+1)->red=(((int) p->red)+((int) (p+2)->red)+((int) r->red)+
        ((int) (r+2)->red)+2) >> 2;
      (q+1)->green=(((int) p->green)+((int) (p+2)->green)+((int) r->green)+
        ((int) (r+2)->green)+2) >> 2;
      (q+1)->blue=(((int) p->blue)+((int) (p+2)->blue)+((int) r->blue)+
        ((int) (r+2)->blue)+2) >> 2;
      (q+1)->index=(((int) p->index)+((int) (p+2)->index)+((int) r->index)+
        ((int) (r+2)->index)+2) >> 2;
      (q+1)->length=0;
      q+=2;
      p+=2;
      r+=2;
    }
    q->red=(((int) p->red)+((int) r->red)+1) >> 1;
    q->green=(((int) p->green)+((int) r->green)+1) >> 1;
    q->blue=(((int) p->blue)+((int) r->blue)+1) >> 1;
    q->index=(((int) p->index)+((int) r->index)+1) >> 1;
    q->length=0;
    p++;
    q++;
    r++;
    q->red=(((int) p->red)+((int) r->red)+1) >> 1;
    q->green=(((int) p->green)+((int) r->green)+1) >> 1;
    q->blue=(((int) p->blue)+((int) r->blue)+1) >> 1;
    q->index=(((int) p->index)+((int) r->index)+1) >> 1;
    q->length=0;
    p++;
    q++;
    r++;
    if (QuantumTick(y,image->rows))
      ProgressMonitor(MagnifyImageText,y,image->rows);
  }
  p=magnified_image->pixels+(2*image->rows-2)*magnified_image->columns;
  q=magnified_image->pixels+(2*image->rows-1)*magnified_image->columns;
  for (x=0; x < (int) image->columns; x++)
  {
    *q++=(*p++);
    *q++=(*p++);
  }
  return(magnified_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M i n i f y I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method MinifyImage creates a new image that is a integral size less than
%  an existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.
%
%  MinifyImage scans the reference image to create a minified image by computing
%  the weighted average of a 4x4 cell centered at each reference pixel.  The
%  target pixel requires two columns and two rows of the reference pixels.
%  Therefore the minified image columns and rows become:
%
%    number_columns/2
%    number_rows/2
%
%  Weights assume that the importance of neighboring pixels is negately
%  proportional to the square of their distance from the target pixel.
%
%  The scan only processes pixels that have a full set of neighbors.  Pixels
%  in the top, bottom, left, and right pairs of rows and columns are omitted
%  from the scan.
%
%  The format of the MinifyImage method is:
%
%      Image *MinifyImage(Image *image)
%
%  A description of each parameter follows:
%
%    o minified_image: Method MinifyImage returns a pointer to the image
%      after reducing.  A null image is returned if there is a memory
%      shortage or if the image size is less than IconSize*2.
%
%    o image: The address of a structure of type Image.
%
%
*/
Export Image *MinifyImage(Image *image)
{
#define Minify(weight) \
  total_red+=(weight)*(s->red); \
  total_green+=(weight)*(s->green); \
  total_blue+=(weight)*(s->blue); \
  total_matte+=(weight)*(s->index); \
  s++;
#define MinifyImageText  "  Minifying image...  "

  Image
    *minified_image;

  int
    y;

  register int
    runlength,
    x;

  register long
    packets;

  register RunlengthPacket
    *p,
    *q,
    *s,
    *s0,
    *s1,
    *s2,
    *s3;

  RunlengthPacket
    *scanline;

  unsigned int
    blue,
    green,
    red;

  unsigned long
    max_packets,
    total_matte,
    total_blue,
    total_green,
    total_red;

  unsigned short
    index;

  assert(image != (Image *) NULL);
  if ((image->columns < 4) || (image->rows < 4))
    return((Image *) NULL);
  /*
    Initialize minified image attributes.
  */
  max_packets=Max(image->packets >> 2,1);
  minified_image=CloneImage(image,max_packets,1,False);
  if (minified_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reduce image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  minified_image->class=DirectClass;
  minified_image->columns=image->columns >> 1;
  minified_image->rows=image->rows >> 1;
  packets=0;
  /*
    Allocate image buffer and scanline buffer for 4 rows of the image.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(4*(image->columns+1)*sizeof(RunlengthPacket));
  if (scanline == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to reduce image",
        "Memory allocation failed");
      DestroyImage(minified_image);
      return((Image *) NULL);
    }
  /*
    Preload the first 2 rows of the image.
  */
  p=image->pixels;
  runlength=p->length+1;
  for (x=0; x < (int) (4*(image->columns+1)); x++)
    scanline[x]=(*p);
  s=scanline;
  for (x=0; x < (int) (image->columns << 1); x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s++;
  }
  /*
    Reduce each row.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=minified_image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < (int) (image->rows-1); y+=2)
  {
    /*
      Initialize sliding window pointers.
    */
    s0=scanline+image->columns*((y+0) % 4);
    s1=scanline+image->columns*((y+1) % 4);
    s2=scanline+image->columns*((y+2) % 4);
    s3=scanline+image->columns*((y+3) % 4);
    /*
      Read another scan line.
    */
    s=s2;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    /*
      Read another scan line.
    */
    s=s3;
    for (x=0; x < (int) image->columns; x++)
    {
      if (runlength != 0)
        runlength--;
      else
        {
          p++;
          runlength=p->length;
        }
      *s=(*p);
      s++;
    }
    for (x=0; x < (int) (image->columns-1); x+=2)
    {
      /*
        Compute weighted average of target pixel color components.

        These particular coefficients total to 128.  Use 128/2-1 or 63 to
        insure correct round off.
      */
      total_red=0;
      total_green=0;
      total_blue=0;
      total_matte=0;
      s=s0;
      Minify(3); Minify(7);  Minify(7);  Minify(3);
      s=s1;
      Minify(7); Minify(15); Minify(15); Minify(7);
      s=s2;
      Minify(7); Minify(15); Minify(15); Minify(7);
      s=s3;
      Minify(3); Minify(7);  Minify(7);  Minify(3);
      s0+=2;
      s1+=2;
      s2+=2;
      s3+=2;
      red=(Quantum) ((total_red+63) >> 7);
      green=(Quantum) ((total_green+63) >> 7);
      blue=(Quantum) ((total_blue+63) >> 7);
      index=(unsigned short) ((total_matte+63) >> 7);
      if ((red == q->red) && (green == q->green) && (blue == q->blue) &&
          (index == q->index) && ((int) q->length < MaxRunlength))
        q->length++;
      else
        {
          if (packets != 0)
            q++;
          packets++;
          if (packets == (int) max_packets)
            {
              max_packets<<=1;
              minified_image->pixels=(RunlengthPacket *) ReallocateMemory(
                (char *) minified_image->pixels,max_packets*
                sizeof(RunlengthPacket));
              if (minified_image->pixels == (RunlengthPacket *) NULL)
                {
                  MagickWarning(ResourceLimitWarning,"Unable to reduce image",
                    "Memory allocation failed");
                  DestroyImage(minified_image);
                  return((Image *) NULL);
                }
              q=minified_image->pixels+packets-1;
            }
          q->red=red;
          q->green=green;
          q->blue=blue;
          q->index=index;
          q->length=0;
        }
    }
    if (QuantumTick(y,image->rows))
      ProgressMonitor(MinifyImageText,y,image->rows-1);
  }
  minified_image->packets=packets;
  minified_image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
    minified_image->pixels,minified_image->packets*sizeof(RunlengthPacket));
  FreeMemory((char *) scanline);
  return(minified_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S a m p l e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SampleImage creates a new image that is a scaled size of an
%  existing one using pixel sampling.  It allocates the memory necessary
%  for the new Image structure and returns a pointer to the new image.
%
%  The format of the SampleImage method is:
%
%      Image *SampleImage(const Image *image,const unsigned int columns,
%        const unsigned int rows)
%
%  A description of each parameter follows:
%
%    o sampled_image: Method SampleImage returns a pointer to the image after
%      scaling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the sampled
%      image.
%
%    o rows: An integer that specifies the number of rows in the sampled
%      image.
%
%
*/
Export Image *SampleImage(const Image *image,const unsigned int columns,
  const unsigned int rows)
{
#define SampleImageText  "  Sampling image...  "

  double
    scale_factor;

  Image
    *sampled_image;

  int
    y;

  register int
    i,
    runlength,
    x;

  register RunlengthPacket
    *p,
    *q,
    *s;

  RunlengthPacket
    *scanline;

  unsigned int
    *x_offset,
    *y_offset;

  assert(image != (Image *) NULL);
  if ((columns == 0) || (rows == 0))
    {
      MagickWarning(OptionWarning,"Unable to resize image",
        "image dimensions are zero");
      return((Image *) NULL);
    }
  if ((columns == image->columns) && (rows == image->rows))
    return(CloneImage(image,columns,rows,True));
  /*
    Initialize sampled image attributes.
  */
  sampled_image=CloneImage(image,columns,rows,False);
  if (sampled_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to sample image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  /*
    Allocate scan line buffer and column offset buffers.
  */
  scanline=(RunlengthPacket *)
    AllocateMemory(image->columns*sizeof(RunlengthPacket));
  x_offset=(unsigned int *)
    AllocateMemory(sampled_image->columns*sizeof(unsigned int));
  y_offset=(unsigned int *)
    AllocateMemory(sampled_image->rows*sizeof(unsigned int));
  if ((scanline == (RunlengthPacket *) NULL) ||
      (x_offset == (unsigned int *) NULL) ||
      (y_offset == (unsigned int *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to sample image",
        "Memory allocation failed");
      DestroyImage(sampled_image);
      return((Image *) NULL);
    }
  /*
    Initialize column pixel offsets.
  */
  scale_factor=(double) image->columns/sampled_image->columns;
  i=0;
  for (x=0; x < (int) sampled_image->columns; x++)
  {
    x_offset[x]=(unsigned int) ((x+1)*scale_factor-i);
    i+=x_offset[x];
  }
  /*
    Initialize row pixel offsets.
  */
  scale_factor=(double) image->rows/sampled_image->rows;
  i=0;
  for (y=0; y < (int) sampled_image->rows; y++)
  {
    y_offset[y]=(unsigned int) ((y+1)*scale_factor-i);
    i+=y_offset[y];
  }
  /*
    Preload first scanline.
  */
  p=image->pixels;
  runlength=p->length+1;
  s=scanline;
  for (x=0; x < (int) image->columns; x++)
  {
    if (runlength != 0)
      runlength--;
    else
      {
        p++;
        runlength=p->length;
      }
    *s=(*p);
    s->length=0;
    s++;
  }
  /*
    Sample each row.
  */
  p=image->pixels;
  runlength=p->length+1;
  q=sampled_image->pixels;
  for (y=0; y < (int) sampled_image->rows; y++)
  {
    for (i=0; i < (int) y_offset[y]; i++)
    {
      /*
        Read a scan line.
      */
      s=scanline;
      for (x=0; x < (int) image->columns; x++)
      {
        if (runlength != 0)
          runlength--;
        else
          {
            p++;
            runlength=p->length;
          }
        *s=(*p);
        s->length=0;
        s++;
      }
    }
    /*
      Sample each column.
    */
    s=scanline;
    for (x=0; x < (int) sampled_image->columns; x++)
    {
      *q=(*s);
      q++;
      s+=x_offset[x];
    }
    if (QuantumTick(y,sampled_image->rows))
      ProgressMonitor(SampleImageText,y,sampled_image->rows);
  }
  FreeMemory((char *) scanline);
  FreeMemory((char *) x_offset);
  FreeMemory((char *) y_offset);
  return(sampled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   S c a l e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ScaleImage creates a new image that is a scaled size of an
%  existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.  To scale a scanline
%  from x pixels to y pixels, each new pixel represents x/y old pixels.  To
%  read x/y pixels, read (x/y rounded up) pixels but only count the required
%  fraction of the last old pixel read in your new pixel.  The remainder
%  of the old pixel will be counted in the next new pixel.
%
%  The scaling algorithm was suggested by rjohnson@shell.com and is adapted
%  from pnmscale(1) of PBMPLUS by Jef Poskanzer.
%
%  The format of the ScaleImage method is:
%
%      Image *ScaleImage(const Image *image,const unsigned int columns,
%        const unsigned int rows)
%
%  A description of each parameter follows:
%
%    o scaled_image: Method ScaleImage returns a pointer to the image after
%      scaling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the scaled
%      image.
%
%    o rows: An integer that specifies the number of rows in the scaled
%      image.
%
%
*/
Export Image *ScaleImage(const Image *image,const unsigned int columns,
  const unsigned int rows)
{
#define ScaleImageText  "  Scaling image...  "

  typedef struct ScaledPacket
  {
    long
      red,
      green,
      blue,
      index;
  } ScaledPacket;

  Image
    *scaled_image;

  int
    next_row,
    number_rows;

  long
    x_scale,
    x_span;

  register int
    runlength,
    x;

  register long
    index,
    packets;

  register RunlengthPacket
    *p,
    *q;

  register ScaledPacket
    *s,
    *t;

  ScaledPacket
    *scaled_scanline,
    *scanline,
    *y_vector,
    *x_vector;

  unsigned int
    y;

  unsigned long
    blue,
    green,
    max_packets,
    red,
    scale_factor;

  assert(image != (Image *) NULL);
  if ((columns == 0) || (rows == 0))
    return((Image *) NULL);
  /*
    Initialize scaled image attributes.
  */
  scale_factor=UpShift(columns*rows)/(image->columns*image->rows);
  max_packets=Max(DownShift(image->packets*scale_factor),1);
  ((Image *) image)->orphan=True;
  scaled_image=CloneImage(image,max_packets,1,False);
  ((Image *) image)->orphan=False;
  if (scaled_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to scale image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  scaled_image->class=DirectClass;
  scaled_image->columns=columns;
  scaled_image->rows=rows;
  packets=0;
  /*
    Allocate memory.
  */
  x_vector=(ScaledPacket *) AllocateMemory(image->columns*sizeof(ScaledPacket));
  scanline=x_vector;
  if (scaled_image->rows != image->rows)
    scanline=(ScaledPacket *)
      AllocateMemory(image->columns*sizeof(ScaledPacket));
  scaled_scanline=(ScaledPacket *)
    AllocateMemory(scaled_image->columns*sizeof(ScaledPacket));
  y_vector=(ScaledPacket *) AllocateMemory(image->columns*sizeof(ScaledPacket));
  if ((x_vector == (ScaledPacket *) NULL) ||
      (scanline == (ScaledPacket *) NULL) ||
      (scaled_scanline == (ScaledPacket *) NULL) ||
      (y_vector == (ScaledPacket *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Unable to scale image",
        "Memory allocation failed");
      DestroyImage(scaled_image);
      return((Image *) NULL);
    }
  /*
    Scale image.
  */
  index=0;
  number_rows=0;
  next_row=True;
  x_scale=UpShift(scaled_image->rows)/image->rows;
  x_span=UpShift(1);
  for (x=0; x < (int) image->columns; x++)
  {
    y_vector[x].red=0;
    y_vector[x].green=0;
    y_vector[x].blue=0;
    y_vector[x].index=0;
  }
  p=image->pixels;
  runlength=p->length+1;
  q=scaled_image->pixels;
  SetRunlengthEncoder(q);
  for (y=0; y < scaled_image->rows; y++)
  {
    if (scaled_image->rows == image->rows)
      for (x=0; x < (int) image->columns; x++)
      {
        /*
          Read a new scanline.
        */
        if (runlength != 0)
          runlength--;
        else
          {
            p++;
            runlength=p->length;
          }
        x_vector[x].red=p->red;
        x_vector[x].green=p->green;
        x_vector[x].blue=p->blue;
        x_vector[x].index=p->index;
      }
    else
      {
        /*
          Scale Y direction.
        */
        while (x_scale < x_span)
        {
          if (next_row && (number_rows < (int) image->rows))
            {
              /*
                Read a new scanline.
              */
              for (x=0; x < (int) image->columns; x++)
              {
                if (runlength != 0)
                  runlength--;
                else
                  {
                    p++;
                    runlength=p->length;
                  }
                x_vector[x].red=p->red;
                x_vector[x].green=p->green;
                x_vector[x].blue=p->blue;
                x_vector[x].index=p->index;
              }
              number_rows++;
            }
          for (x=0; x < (int) image->columns; x++)
          {
            y_vector[x].red+=x_scale*x_vector[x].red;
            y_vector[x].green+=x_scale*x_vector[x].green;
            y_vector[x].blue+=x_scale*x_vector[x].blue;
            y_vector[x].index+=x_scale*x_vector[x].index;
          }
          x_span-=x_scale;
          x_scale=UpShift(scaled_image->rows)/image->rows;
          next_row=True;
        }
        if (next_row && (number_rows < (int) image->rows))
          {
            /*
              Read a new scanline.
            */
            for (x=0; x < (int) image->columns; x++)
            {
              if (runlength != 0)
                runlength--;
              else
                {
                  p++;
                  runlength=p->length;
                }
              x_vector[x].red=p->red;
              x_vector[x].green=p->green;
              x_vector[x].blue=p->blue;
              x_vector[x].index=p->index;
            }
            number_rows++;
            next_row=False;
          }
        s=scanline;
        for (x=0; x < (int) image->columns; x++)
        {
          red=DownShift(y_vector[x].red+x_span*x_vector[x].red);
          green=DownShift(y_vector[x].green+x_span*x_vector[x].green);
          blue=DownShift(y_vector[x].blue+x_span*x_vector[x].blue);
          index=DownShift(y_vector[x].index+x_span*x_vector[x].index);
          s->red=(Quantum) (red > MaxRGB ? MaxRGB : red);
          s->green=(Quantum) (green > MaxRGB ? MaxRGB : green);
          s->blue=(Quantum) (blue > MaxRGB ? MaxRGB : blue);
          s->index=(unsigned short)
            (index > MaxColormapSize ? MaxColormapSize : index);
          s++;
          y_vector[x].red=0;
          y_vector[x].green=0;
          y_vector[x].blue=0;
          y_vector[x].index=0;
        }
        x_scale-=x_span;
        if (x_scale == 0)
          {
            x_scale=UpShift(scaled_image->rows)/image->rows;
            next_row=True;
          }
        x_span=UpShift(1);
      }
    if (scaled_image->columns == image->columns)
      {
        /*
          Transfer scanline to scaled image.
        */
        s=scanline;
        for (x=0; x < (int) scaled_image->columns; x++)
        {
          if ((s->red == q->red) && (s->green == q->green) &&
              (s->blue == q->blue) && (s->index == q->index) &&
              ((int) q->length < MaxRunlength))
            q->length++;
          else
            {
              if (packets != 0)
                q++;
              packets++;
              if (packets == (int) max_packets)
                {
                  max_packets<<=1;
                  scaled_image->pixels=(RunlengthPacket *) ReallocateMemory(
                    (char *) scaled_image->pixels,max_packets*
                    sizeof(RunlengthPacket));
                  if (scaled_image->pixels == (RunlengthPacket *) NULL)
                    {
                      MagickWarning(ResourceLimitWarning,
                        "Unable to scale image","Memory allocation failed");
                      DestroyImage(scaled_image);
                      return((Image *) NULL);
                    }
                  q=scaled_image->pixels+packets-1;
                }
              q->red=(Quantum) s->red;
              q->green=(Quantum) s->green;
              q->blue=(Quantum) s->blue;
              q->index=(unsigned short) s->index;
              q->length=0;
            }
          s++;
        }
      }
    else
      {
        int
          next_column;

        long
          y_scale,
          y_span;

        /*
          Scale X direction.
        */
        red=0;
        green=0;
        blue=0;
        next_column=False;
        y_span=UpShift(1);
        s=scanline;
        t=scaled_scanline;
        for (x=0; x < (int) image->columns; x++)
        {
          y_scale=UpShift(scaled_image->columns)/image->columns;
          while (y_scale >= y_span)
          {
            if (next_column)
              {
                red=0;
                green=0;
                blue=0;
                index=0;
                t++;
              }
            red=DownShift(red+y_span*s->red);
            green=DownShift(green+y_span*s->green);
            blue=DownShift(blue+y_span*s->blue);
            index=DownShift(index+y_span*s->index);
            t->red=(Quantum) (red > MaxRGB ? MaxRGB : red);
            t->green=(Quantum) (green > MaxRGB ? MaxRGB : green);
            t->blue=(Quantum) (blue > MaxRGB ? MaxRGB : blue);
            t->index=(unsigned short)
              (index > MaxColormapSize ? MaxColormapSize : index);
            y_scale-=y_span;
            y_span=UpShift(1);
            next_column=True;
          }
        if (y_scale > 0)
          {
            if (next_column)
              {
                red=0;
                green=0;
                blue=0;
                index=0;
                next_column=False;
                t++;
              }
            red+=y_scale*s->red;
            green+=y_scale*s->green;
            blue+=y_scale*s->blue;
            index+=y_scale*s->index;
            y_span-=y_scale;
          }
        s++;
      }
      if (y_span > 0)
        {
          s--;
          red+=y_span*s->red;
          green+=y_span*s->green;
          blue+=y_span*s->blue;
          index+=y_span*s->index;
        }
      if (!next_column)
        {
          red=DownShift(red);
          green=DownShift(green);
          blue=DownShift(blue);
          index=DownShift(index);
          t->red=(Quantum) (red > MaxRGB ? MaxRGB : red);
          t->green=(Quantum) (green > MaxRGB ? MaxRGB : green);
          t->blue=(Quantum) (blue > MaxRGB ? MaxRGB : blue);
          t->index=(unsigned short) (index > MaxRGB ? MaxRGB : index);
        }
      /*
        Transfer scanline to scaled image.
      */
      t=scaled_scanline;
      for (x=0; x < (int) scaled_image->columns; x++)
      {
        if ((t->red == q->red) && (t->green == q->green) &&
            (t->blue == q->blue) && (t->index == q->index) &&
            ((int) q->length < MaxRunlength))
          q->length++;
        else
          {
            if (packets != 0)
              q++;
            packets++;
            if (packets == (int) max_packets)
              {
                max_packets<<=1;
                scaled_image->pixels=(RunlengthPacket *) ReallocateMemory(
                  (char *) scaled_image->pixels,max_packets*
                  sizeof(RunlengthPacket));
                if (scaled_image->pixels == (RunlengthPacket *) NULL)
                  {
                    MagickWarning(ResourceLimitWarning,"Unable to scale image",
                      "Memory allocation failed");
                    DestroyImage(scaled_image);
                    return((Image *) NULL);
                  }
                q=scaled_image->pixels+packets-1;
              }
            q->red=(Quantum) t->red;
            q->green=(Quantum) t->green;
            q->blue=(Quantum) t->blue;
            q->index=(unsigned short) t->index;
            q->length=0;
          }
        t++;
      }
    }
    if (QuantumTick(y,scaled_image->rows))
      ProgressMonitor(ScaleImageText,y,scaled_image->rows);
  }
  scaled_image->packets=packets;
  scaled_image->pixels=(RunlengthPacket *) ReallocateMemory((char *)
    scaled_image->pixels,scaled_image->packets*sizeof(RunlengthPacket));
  /*
    Free allocated memory.
  */
  FreeMemory((char *) y_vector);
  FreeMemory((char *) scaled_scanline);
  if (scanline != x_vector)
    FreeMemory((char *) scanline);
  FreeMemory((char *) x_vector);
  return(scaled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   Z o o m I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ZoomImage creates a new image that is a scaled size of an
%  existing one.  It allocates the memory necessary for the new Image
%  structure and returns a pointer to the new image.  The Point filter gives
%  fast pixel replication, Triangle is equivalent to bi-linear interpolation,
%  and Mitchel giver slower, very high-quality results.  See Graphic Gems III
%  for details on this algorithm.
%
%  The filter member of the Image structure specifies which image filter to
%  use. Blur specifies the blur factor where > 1 is blurry, < 1 is sharp.
%
%  The format of the ZoomImage method is:
%
%      Image *ZoomImage(Image *image,const unsigned int columns,
%        const unsigned int rows)
%
%  A description of each parameter follows:
%
%    o zoomed_image: Method ZoomImage returns a pointer to the image after
%      scaling.  A null image is returned if there is a memory shortage.
%
%    o image: The address of a structure of type Image.
%
%    o columns: An integer that specifies the number of columns in the zoomed
%      image.
%
%    o rows: An integer that specifies the number of rows in the scaled
%      image.
%
%
*/

#define ZoomImageText  "  Zooming image...  "

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

static double Box(double x)
{
  if ((x >= -0.5) && (x < 0.5))
    return(1.0);
  return(0.0);
}

static double Bessel(double x)
{
  if (x == 0.0)
    return(M_PI/4.0);
  return(BesselOrderOne(M_PI*x)/(2.0*x));
}

static double Blackman(double x)
{
  return(0.42+0.50*cos(M_PI*x)+0.08*cos(2.0*M_PI*x));
}

static double Catrom(double x)
{
  if (x < 0)
    x=(-x);
  if (x < 1.0)
    return(0.5*(2.0+x*x*(-5.0+x*3.0)));
  if (x < 2.0)
    return(0.5*(4.0+x*(-8.0+x*(5.0-x))));
  return(0.0);
}

static double Cubic(double x)
{
  if (x < 0)
    x=(-x);
  if (x < 1.0)
    return((0.5*x*x*x)-x*x+(2.0/3.0));
  if (x < 2.0)
    {
      x=2.0-x;
      return((1.0/6.0)*x*x*x);
    }
  return(0.0);
}

static double Gaussian(double x)
{
  return(exp(-2.0*x*x)*sqrt(2.0/M_PI));
}

static double Hanning(double x)
{
  return(0.5+0.5*cos(M_PI*x));
}

static double Hamming(double x)
{
  return(0.54+0.46*cos(M_PI*x));
}

static double Hermite(double x)
{
  if (x < 0)
    x=(-x);
  if (x < 1.0)
    return((2.0*x-3.0)*x*x+1.0);
  return(0.0);
}

static double Sinc(double x)
{
  x*=M_PI;
  if (x != 0.0)
    return(sin(x)/x);
  return(1.0);
}

static double Lanczos(double x)
{
  if (x < 0)
    x=(-x);
  if (x < 3.0)
   return(Sinc(x)*Sinc(x/3.0));
  return(0.0);
}

static double Mitchell(double x)
{
  double
    b,
    c;

  b=1.0/3.0;
  c=1.0/3.0;
  if (x < 0)
    x=(-x);
  if (x < 1.0)
    {
      x=((12.0-9.0*b-6.0*c)*(x*x*x))+((-18.0+12.0*b+6.0*c)*x*x)+(6.0-2.0*b);
      return(x/6.0);
    }
 if (x < 2.0)
   {
     x=((-1.0*b-6.0*c)*(x*x*x))+((6.0*b+30.0*c)*x*x)+((-12.0*b-48.0*c)*x)+
       (8.0*b+24.0*c);
     return(x/6.0);
   }
  return(0.0);
}

static double Quadratic(double x)
{
  if (x < 0)
    x=(-x);
  if (x < 0.5)
    return(0.75-x*x);
  if (x < 1.5)
    {
      x-=1.5;
      return(0.5*x*x);
    }
  return(0.0);
}

static double Triangle(double x)
{
  if (x < 0.0)
    x=(-x);
  if (x < 1.0)
    return(1.0-x);
  return(0.0);
}

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

static void HorizontalFilter(Image *source,Image *destination,double x_factor,
  const FilterInfo *filter_info,ContributionInfo *contribution_info,
  const Quantum *range_limit,const unsigned int span,unsigned int *quantum)
{
  double
    blue_weight,
    center,
    density,
    green_weight,
    index_weight,
    red_weight,
    scale_factor,
    support;

  int
    end,
    n,
    start,
    x;

  register int
    i,
    y;

  register RunlengthPacket
    *p,
    *q;

  /*
    Apply filter to zoom horizontally from source to destination.
  */
  scale_factor=source->blur*Max(1.0/x_factor,1.0);
  support=Max(scale_factor*filter_info->support,0.5);
  destination->class=source->class;
  if (support > 0.5)
    destination->class=DirectClass;
  else
    {
      /*
        Reduce to point sampling.
      */
      support=0.5;
      scale_factor=1.0;
    }
  support+=1.0e-7;
  for (x=0; x < (int) destination->columns; x++)
  {
    density=0.0;
    n=0;
    center=(double) x/x_factor;
    start=(int) (center-support+0.5);
    end=(int) (center+support+0.5);
    for (i=Max(start,0); i < Min(end,(int) source->columns); i++)
    {
      contribution_info[n].pixel=i;
      contribution_info[n].weight=
        filter_info->function(((double) i-center+0.5)/scale_factor);
      contribution_info[n].weight/=scale_factor;
      density+=contribution_info[n].weight;
      n++;
    }
    if ((density != 0.0) && (density != 1.0))
      for (i=0; i < n; i++)
        contribution_info[i].weight/=density;  /* normalize */
    q=destination->pixels+x;
    for (y=0; y < (int) destination->rows; y++)
    {
      blue_weight=0.0;
      green_weight=0.0;
      red_weight=0.0;
      index_weight=0.0;
      for (i=0; i < n; i++)
      {
        p=source->pixels+(y*source->columns)+contribution_info[i].pixel;
        red_weight+=contribution_info[i].weight*p->red;
        green_weight+=contribution_info[i].weight*p->green;
        blue_weight+=contribution_info[i].weight*p->blue;
        index_weight+=contribution_info[i].weight*p->index;
      }
      q->red=range_limit[(int) (red_weight+0.5)];
      q->green=range_limit[(int) (green_weight+0.5)];
      q->blue=range_limit[(int) (blue_weight+0.5)];
      if (index_weight > Opaque)
        q->index=Opaque;
      else
        if (index_weight < Transparent)
          q->index=Transparent;
        else
          q->index=(unsigned short) (index_weight+0.5);
      q->length=0;
      q+=destination->columns;
    }
    if (QuantumTick(*quantum,span))
      ProgressMonitor(ZoomImageText,*quantum,span);
    (*quantum)++;
  }
}

static void VerticalFilter(Image *source,Image *destination,double y_factor,
  const FilterInfo *filter_info,ContributionInfo *contribution_info,
  const Quantum *range_limit,const unsigned int span,unsigned int *quantum)
{
  double
    blue_weight,
    center,
    density,
    green_weight,
    index_weight,
    red_weight,
    scale_factor,
    support;

  int
    end,
    n,
    start,
    y;

  register int
    i,
    x;

  register RunlengthPacket
    *p,
    *q;

  /*
    Apply filter to zoom vertically from source to destination.
  */
  scale_factor=source->blur*Max(1.0/y_factor,1.0);
  support=Max(scale_factor*filter_info->support,0.5);
  destination->class=source->class;
  if (support > 0.5)
    destination->class=DirectClass;
  else
    {
      /*
        Reduce to point sampling.
      */
      support=0.5;
      scale_factor=1.0;
    }
  support+=1.0e-7;
  q=destination->pixels;
  for (y=0; y < (int) destination->rows; y++)
  {
    density=0.0;
    n=0;
    center=(double) y/y_factor;
    start=(int) (center-support+0.5);
    end=(int) (center+support+0.5);
    for (i=Max(start,0); i < Min(end,(int) source->rows); i++)
    {
      contribution_info[n].pixel=i;
      contribution_info[n].weight=
        filter_info->function(((double) i-center+0.5)/scale_factor);
      contribution_info[n].weight/=scale_factor;
      density+=contribution_info[n].weight;
      n++;
    }
    if ((density != 0.0) && (density != 1.0))
      for (i=0; i < n; i++)
        contribution_info[i].weight/=density;  /* normalize */
    for (x=0; x < (int) destination->columns; x++)
    {
      blue_weight=0.0;
      green_weight=0.0;
      red_weight=0.0;
      index_weight=0.0;
      for (i=0; i < n; i++)
      {
        p=source->pixels+(contribution_info[i].pixel*source->columns)+x;
        red_weight+=contribution_info[i].weight*p->red;
        green_weight+=contribution_info[i].weight*p->green;
        blue_weight+=contribution_info[i].weight*p->blue;
        index_weight+=contribution_info[i].weight*p->index;
      }
      q->red=range_limit[(int) (red_weight+0.5)];
      q->green=range_limit[(int) (green_weight+0.5)];
      q->blue=range_limit[(int) (blue_weight+0.5)];
      if (index_weight > Opaque)
        q->index=Opaque;
      else
        if (index_weight < Transparent)
          q->index=Transparent;
        else
          q->index=(unsigned short) (index_weight+0.5);
      q->length=0;
      q++;
    }
    if (QuantumTick(*quantum,span))
      ProgressMonitor(ZoomImageText,*quantum,span);
    (*quantum)++;
  }
}

Export Image *ZoomImage(Image *image,const unsigned int columns,
  const unsigned int rows)
{
  ContributionInfo
    *contribution_info;

  double
    support,
    x_factor,
    y_factor;

  Image
    *source_image,
    *zoomed_image;

  Quantum
    *range_table;

  register int
     i;

  register Quantum
    *range_limit;

  static const FilterInfo
    filters[SincFilter+1] =
    {
      { Box, 0.0 },
      { Box, 0.0 },
      { Box, 0.5 },
      { Triangle, 1.0 },
      { Hermite, 1.0 },
      { Hanning, 1.0 },
      { Hamming, 1.0 },
      { Blackman, 1.0 },
      { Gaussian, 1.25 },
      { Quadratic, 1.5 },
      { Cubic, 2.0 },
      { Catrom, 2.0 },
      { Mitchell, 2.0 },
      { Lanczos, 3.0 },
      { Bessel, 3.2383 },
      { Sinc, 4.0 }
    };

  unsigned int
    quantum,
    span;

  assert(image != (Image *) NULL);
  assert((image->filter >= 0) && (image->filter <= SincFilter));
  if ((columns == 0) || (rows == 0))
    {
      MagickWarning(OptionWarning,"Unable to resize image",
        "image dimensions are zero");
      return((Image *) NULL);
    }
  if ((columns == image->columns) && (rows == image->rows))
    return(CloneImage(image,columns,rows,True));
  /*
    Image must be uncompressed.
  */
  if (!UncondenseImage(image))
    return((Image *) NULL);
  /*
    Initialize zoomed image attributes.
  */
  zoomed_image=CloneImage(image,columns,rows,False);
  if (zoomed_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to zoom image",
        "Memory allocation failed");
      return((Image *) NULL);
    }
  image->orphan=True;
  if (zoomed_image->rows >= image->rows)
    source_image=CloneImage(image,zoomed_image->columns,image->rows,False);
  else
    source_image=CloneImage(image,image->columns,zoomed_image->rows,False);
  image->orphan=False;
  if (source_image == (Image *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to zoom image",
        "Memory allocation failed");
      DestroyImage(zoomed_image);
      return((Image *) NULL);
    }
  /*
    Allocate the range table.
  */
  range_table=(Quantum *) AllocateMemory(3*(MaxRGB+1)*sizeof(Quantum));
  if (range_table == (Quantum *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to zoom image",
        "Memory allocation failed");
      DestroyImage(source_image);
      DestroyImage(zoomed_image);
      return((Image *) NULL);
    }
  /*
    Pre-compute conversion tables.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    range_table[i]=0;
    range_table[i+(MaxRGB+1)]=(Quantum) i;
    range_table[i+(MaxRGB+1)*2]=MaxRGB;
  }
  range_limit=range_table+(MaxRGB+1);
  /*
    Allocate filter info list.
  */
  x_factor=(double) zoomed_image->columns/(double) image->columns;
  y_factor=(double) zoomed_image->rows/(double) image->rows;
  support=Max(filters[image->filter].support/x_factor,
    filters[image->filter].support/y_factor);
  if (support < filters[image->filter].support)
    support=filters[image->filter].support;
  contribution_info=(ContributionInfo *)
    AllocateMemory((int) (support*2+3)*sizeof(ContributionInfo));
  if (contribution_info == (ContributionInfo *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to zoom image",
        "Memory allocation failed");
      FreeMemory((char *) range_table);
      DestroyImage(source_image);
      DestroyImage(zoomed_image);
      return((Image *) NULL);
    }
  /*
    Zoom image.
  */
  quantum=0;
  if (zoomed_image->rows >= image->rows)
    {
      span=source_image->columns+zoomed_image->rows;
      HorizontalFilter(image,source_image,x_factor,&filters[image->filter],
        contribution_info,range_limit,span,&quantum);
      VerticalFilter(source_image,zoomed_image,y_factor,&filters[image->filter],
        contribution_info,range_limit,span,&quantum);
    }
  else
    {
      span=zoomed_image->columns+source_image->columns;
      VerticalFilter(image,source_image,y_factor,&filters[image->filter],
        contribution_info,range_limit,span,&quantum);
      HorizontalFilter(source_image,zoomed_image,x_factor,
        &filters[image->filter],contribution_info,range_limit,span,&quantum);
    }
  /*
    Free allocated memory.
  */
  FreeMemory((char *) contribution_info);
  FreeMemory((char *) range_table);
  DestroyImage(source_image);
  return(zoomed_image);
}
