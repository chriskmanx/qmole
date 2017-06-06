/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%               SSSSS  EEEEE   GGGG  M   M  EEEEE  N   N  TTTTT               %
%               SS     E      G      MM MM  E      NN  N    T                 %
%                SSS   EEE    G GGG  M M M  EEE    N N N    T                 %
%                  SS  E      G   G  M   M  E      N  NN    T                 %
%               SSSSS  EEEEE   GGGG  M   M  EEEEE  N   N    T                 %
%                                                                             %
%                                                                             %
%        Methods to Segment an Image with Thresholding Fuzzy c-Means          %
%                                                                             %
%                                                                             %
%                              Software Design                                %
%                                John Cristy                                  %
%                                April 1993                                   %
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
%  Segment segments an image by analyzing the histograms of the color
%  components and identifying units that are homogeneous with the fuzzy
%  c-means technique.  The scale-space filter analyzes the histograms of
%  the three color components of the image and identifies a set of classes.
%  The extents of each class is used to coarsely segment the image with
%  thresholding.  The color associated with each class is determined by
%  the mean color of all pixels within the extents of a particular class.
%  Finally, any unclassified pixels are assigned to the closest class with
%  the fuzzy c-means technique.
%
%  The fuzzy c-Means algorithm can be summarized as follows:
%
%    o Build a histogram, one for each color component of the image.
%
%    o For each histogram, successively apply the scale-space
%      filter and build an interval tree of zero crossings in
%      the second derivative at each scale.  Analyze this
%      scale-space ``fingerprint'' to determine which peaks and
%      valleys in the histogram are most predominant.
%
%    o The fingerprint defines intervals on the axis of the
%      histogram.  Each interval contains either a minima or a
%      maxima in the original signal.  If each color component
%      lies within the maxima interval, that pixel is considered
%      ``classified'' and is assigned an unique class number.
%
%    o Any pixel that fails to be classified in the above
%      thresholding pass is classified using the fuzzy
%      c-Means technique.  It is assigned to one of the classes
%      discovered in the histogram analysis phase.
%
%  The fuzzy c-Means technique attempts to cluster a pixel by finding
%  the local minima of the generalized within group sum of squared error
%  objective function.  A pixel is assigned to the closest class of which
%  the fuzzy membership has a maximum value.
%
%  Segment is strongly based on software written by Andy Gallo, University
%  of Delaware.
%
%  The following reference was used in creating this program:
%
%    Young Won Lim, Sang Uk Lee, "On The Color Image Segmentation Algorithm
%    Based on the Thresholding and the Fuzzy c-Means Techniques", Pattern
%    Recognition, Volume 23, Number 9, pages 935-952, 1990.
%
%
*/

#include "magick.h"
#include "defines.h"

/*
  Define declarations.
*/
#define  Dimension  3

/*
  Typedef declarations.
*/
typedef struct _ExtentPacket
{
  int
    index,
    left,
    right;

  long
    center;
} ExtentPacket;

typedef struct _IntervalTree
{
  double
    tau;

  int
    left,
    right;

  double
    mean_stability,
    stability;

  struct _IntervalTree
    *sibling,
    *child;
} IntervalTree;

typedef struct _ZeroCrossing
{
  double
    tau,
    histogram[MaxRGB+1];

  short
    crossings[MaxRGB+1];
} ZeroCrossing;

/*
  Constant declarations.
*/
const int
  Blue = 2,
  Green = 1,
  Red = 0,
  SafeMargin = 3,
  TreeLength = 600;

/*
  Method prototypes.
*/
static int
  DefineRegion(const short *,ExtentPacket *);

static void
  ScaleSpace(const long *,const double,double *),
  ZeroCrossHistogram(double *,const double,short *);

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   C l a s s i f y                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method Classify defines on ore more classes.  Each pixel is thresholded
%  to determine which class it belongs to.  If not class is identified it
%  is assigned to the closest class based on the fuzzy c-Means technique.
%
%  The format of the Classify method is:
%
%      unsigned int SegmentImage(Image *image,const ColorspaceType colorspace,
%        const unsigned int verbose,const double cluster_threshold,
%        const double smoothing_threshold)
%
%  A description of each parameter follows.
%
%    o image: Specifies a pointer to an Image structure;  returned from
%      ReadImage.
%
%    o extrema:  Specifies a pointer to an array of integers.  They
%      represent the peaks and valleys of the histogram for each color
%      component.
%
%    o cluster_threshold:  This double represents the minimum number of pixels
%      contained in a hexahedra before it can be considered valid (expressed
%      as a percentage).
%
%    o weighting_exponent: Specifies the membership weighting exponent.
%
%    o verbose:  A value greater than zero prints detailed information about
%      the identified classes.
%
%
*/
static unsigned int Classify(Image *image,short **extrema,
  const double cluster_threshold,const double weighting_exponent,
  const unsigned int verbose)
{
#define SegmentImageText  "  Segmenting image...  "

  typedef struct _Cluster
  {
    short
      id;

    ExtentPacket
      red,
      green,
      blue;

    long
      count;

    struct _Cluster
      *next;
  } Cluster;

  Cluster
    *cluster,
    *head,
    *last_cluster,
    *next_cluster;

  ColorPacket
    *colormap;

  double
    local_minima,
    numerator,
    ratio,
    sum;

  ExtentPacket
    blue,
    green,
    red;

  int
    count;

  register double
    distance_squared,
    *squares;

  register int
    i,
    j,
    k;

  register RunlengthPacket
    *p,
    *q;

  unsigned int
    number_clusters;

  /*
    Form clusters.
  */
  cluster=(Cluster *) NULL;
  head=(Cluster *) NULL;
  red.index=0;
  while (DefineRegion(extrema[Red],&red))
  {
    green.index=0;
    while (DefineRegion(extrema[Green],&green))
    {
      blue.index=0;
      while (DefineRegion(extrema[Blue],&blue))
      {
        /*
          Allocate a new class.
        */
        if (head != (Cluster *) NULL)
          {
            cluster->next=(Cluster *) AllocateMemory(sizeof(Cluster));
            cluster=cluster->next;
          }
        else
          {
            cluster=(Cluster *) AllocateMemory(sizeof(Cluster));
            head=cluster;
          }
        if (cluster == (Cluster *) NULL)
          {
            MagickWarning(ResourceLimitWarning,"Memory allocation failed",
              (char *) NULL);
            return(False);
          }
        /*
          Initialize a new class.
        */
        cluster->count=0;
        cluster->red=red;
        cluster->green=green;
        cluster->blue=blue;
        cluster->next=(Cluster *) NULL;
      }
    }
  }
  if (head == (Cluster *) NULL)
    {
      /*
        No classes were identified-- create one.
      */
      cluster=(Cluster *) AllocateMemory(sizeof(Cluster));
      if (cluster == (Cluster *) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Memory allocation failed",
            (char *) NULL);
          return(False);
        }
      /*
        Initialize a new class.
      */
      cluster->count=0;
      cluster->red=red;
      cluster->green=green;
      cluster->blue=blue;
      cluster->next=(Cluster *) NULL;
      head=cluster;
    }
  /*
    Count the pixels for each cluster.
  */
  count=0;
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    for (cluster=head; cluster != (Cluster *) NULL; cluster=cluster->next)
      if (((int) p->red >= (cluster->red.left-SafeMargin)) &&
          ((int) p->red <= (cluster->red.right+SafeMargin)) &&
          ((int) p->green >= (cluster->green.left-SafeMargin)) &&
          ((int) p->green <= (cluster->green.right+SafeMargin)) &&
          ((int) p->blue >= (cluster->blue.left-SafeMargin)) &&
          ((int) p->blue <= (cluster->blue.right+SafeMargin)))
        {
          /*
            Count this pixel.
          */
          count+=(p->length+1);
          cluster->count+=(p->length+1);
          cluster->red.center+=(p->red*(p->length+1));
          cluster->green.center+=(p->green*(p->length+1));
          cluster->blue.center+=(p->blue*(p->length+1));
          break;
        }
    p++;
    if (QuantumTick(i,image->packets))
      ProgressMonitor(SegmentImageText,i,image->packets << 1);
  }
  /*
    Remove clusters that do not meet minimum cluster threshold.
  */
  count=0;
  last_cluster=head;
  next_cluster=head;
  for (cluster=head; cluster != (Cluster *) NULL; cluster=next_cluster)
  {
    next_cluster=cluster->next;
    if ((double) cluster->count >= (cluster_threshold*count*0.01))
      {
        /*
          Initialize cluster.
        */
        cluster->id=count;
        cluster->red.center=(cluster->red.center+(cluster->count >> 1))/
          cluster->count;
        cluster->green.center=(cluster->green.center+(cluster->count >> 1))/
          cluster->count;
        cluster->blue.center=(cluster->blue.center+(cluster->count >> 1))/
          cluster->count;
        count++;
        last_cluster=cluster;
      }
    else
      {
        /*
          Delete cluster.
        */
        if (cluster == head)
          head=next_cluster;
        else
          last_cluster->next=next_cluster;
        FreeMemory((char *) cluster);
      }
  }
  number_clusters=count;
  if (verbose)
    {
      /*
        Print cluster statistics.
      */
      (void) fprintf(stderr,"Fuzzy c-Means Statistics\n");
      (void) fprintf(stderr,"===================\n\n");
      (void) fprintf(stderr,"\tTotal Number of Clusters = %u\n\n",
        number_clusters);
      /*
        Print the total number of points per cluster.
      */
      (void) fprintf(stderr,"\n\nNumber of Vectors Per Cluster\n");
      (void) fprintf(stderr,"=============================\n\n");
      for (cluster=head; cluster != (Cluster *) NULL; cluster=cluster->next)
        (void) fprintf(stderr,"Cluster #%d = %ld\n",cluster->id,cluster->count);
      /*
        Print the cluster extents.
      */
      (void) fprintf(stderr,
        "\n\n\nCluster Extents:        (Vector Size: %d)\n",Dimension);
      (void) fprintf(stderr, "================");
      for (cluster=head; cluster != (Cluster *) NULL; cluster=cluster->next)
      {
        (void) fprintf(stderr,"\n\nCluster #%d\n\n",cluster->id);
        (void) fprintf(stderr,"%d-%d  %d-%d  %d-%d\n",
          cluster->red.left,cluster->red.right,
          cluster->green.left,cluster->green.right,
          cluster->blue.left,cluster->blue.right);
      }
      /*
        Print the cluster center values.
      */
      (void) fprintf(stderr,
        "\n\n\nCluster Center Values:        (Vector Size: %d)\n",Dimension);
      (void) fprintf(stderr, "=====================");
      for (cluster=head; cluster != (Cluster *) NULL; cluster=cluster->next)
      {
        (void) fprintf(stderr,"\n\nCluster #%d\n\n",cluster->id);
        (void) fprintf(stderr,"%ld  %ld  %ld\n",cluster->red.center,
          cluster->green.center,cluster->blue.center);
      }
      (void) fprintf(stderr,"\n");
    }
  /*
    Speed up distance calculations.
  */
  squares=(double *) AllocateMemory((MaxRGB+MaxRGB+1)*sizeof(double));
  if (squares == (double *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  squares+=MaxRGB;
  for (i=(-MaxRGB); i <= MaxRGB; i++)
    squares[i]=i*i;
  /*
    Allocate image colormap.
  */
  colormap=(ColorPacket *)
    AllocateMemory((unsigned int) number_clusters*sizeof(ColorPacket));
  if (colormap == (ColorPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(False);
    }
  image->matte=False;
  image->class=PseudoClass;
  if (image->colormap != (ColorPacket *) NULL)
    FreeMemory((char *) image->colormap);
  image->colormap=colormap;
  image->colors=number_clusters;
  i=0;
  for (cluster=head; cluster != (Cluster *) NULL; cluster=cluster->next)
  {
    image->colormap[i].red=(Quantum) cluster->red.center;
    image->colormap[i].green=(Quantum) cluster->green.center;
    image->colormap[i].blue=(Quantum) cluster->blue.center;
    i++;
  }
  /*
    Do course grain classification.
  */
  image->tainted=True;
  q=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    for (cluster=head; cluster != (Cluster *) NULL; cluster=cluster->next)
      if (((int) q->red >= (cluster->red.left-SafeMargin)) &&
          ((int) q->red <= (cluster->red.right+SafeMargin)) &&
          ((int) q->green >= (cluster->green.left-SafeMargin)) &&
          ((int) q->green <= (cluster->green.right+SafeMargin)) &&
          ((int) q->blue >= (cluster->blue.left-SafeMargin)) &&
          ((int) q->blue <= (cluster->blue.right+SafeMargin)))
        {
          /*
            Classify this pixel.
          */
          q->index=cluster->id;
          break;
        }
    if (cluster == (Cluster *) NULL)
      {
        /*
          Compute fuzzy membership.
        */
        local_minima=0.0;
        for (j=0; j < (int) image->colors; j++)
        {
          sum=0.0;
          distance_squared=squares[(int) q->red-(int) image->colormap[j].red]+
            squares[(int) q->green-(int) image->colormap[j].green]+
            squares[(int) q->blue-(int) image->colormap[j].blue];
          numerator=sqrt(distance_squared);
          for (k=0; k < (int) image->colors; k++)
          {
            distance_squared=squares[(int) q->red-(int) image->colormap[k].red]+
              squares[(int) q->green-(int) image->colormap[k].green]+
              squares[(int) q->blue-(int) image->colormap[k].blue];
            ratio=numerator/sqrt(distance_squared);
            sum+=pow(ratio,(double) (2.0/(weighting_exponent-1.0)));
          }
          if ((1.0/sum) > local_minima)
            {
              /*
                Classify this pixel.
              */
              local_minima=1.0/sum;
              q->index=j;
            }
        }
      }
    q++;
    if (QuantumTick(i,image->packets))
      ProgressMonitor(SegmentImageText,i+image->packets,image->packets << 1);
  }
  /*
    Free memory.
  */
  for (cluster=head; cluster != (Cluster *) NULL; cluster=next_cluster)
  {
    next_cluster=cluster->next;
    FreeMemory((char *) cluster);
  }
  squares-=MaxRGB;
  FreeMemory((char *) squares);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   C o n s o l i d a t e C r o s s i n g s                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method ConsolidateCrossings guarantees that an even number of zero
%  crossings always lie between two crossings.
%
%  The format of the ConsolidateCrossings method is:
%
%      ConsolidateCrossings(zero_crossing,number_crossings)
%
%  A description of each parameter follows.
%
%    o zero_crossing: Specifies an array of structures of type ZeroCrossing.
%
%    o number_crossings: This unsigned int specifies the number of elements
%      in the zero_crossing array.
%
%
*/
static void ConsolidateCrossings(ZeroCrossing *zero_crossing,
  const unsigned int number_crossings)
{
  int
    center,
    correct,
    count,
    left,
    right;

  register int
    i,
    j,
    k,
    l;

  /*
    Consolidate zero crossings.
  */
  for (i=number_crossings-1; i >= 0; i--)
    for (j=0; j <= MaxRGB; j++)
    {
      if (zero_crossing[i].crossings[j] == 0)
        continue;
      /*
        Find the entry that is closest to j and still preserves the
        property that there are an even number of crossings between
        intervals.
      */
      for (k=j-1; k > 0; k--)
        if (zero_crossing[i+1].crossings[k] != 0)
          break;
      left=Max(k,0);
      center=j;
      for (k=j+1; k < MaxRGB; k++)
        if (zero_crossing[i+1].crossings[k] != 0)
          break;
      right=Min(k,MaxRGB);
      /*
        K is the zero crossing just left of j.
      */
      for (k=j-1; k > 0; k--)
        if (zero_crossing[i].crossings[k] != 0)
          break;
      if (k < 0)
        k=0;
      /*
        Check center for an even number of crossings between k and j.
      */
      correct=(-1);
      if (zero_crossing[i+1].crossings[j] != 0)
        {
          count=0;
          for (l=k+1; l < center; l++)
            if (zero_crossing[i+1].crossings[l] != 0)
              count++;
          if ((count % 2) == 0)
            if (center != k)
              correct=center;
        }
      /*
        Check left for an even number of crossings between k and j.
      */
      if (correct == -1)
        {
          count=0;
          for (l=k+1; l < left; l++)
            if (zero_crossing[i+1].crossings[l] != 0)
              count++;
          if ((count % 2) == 0)
            if (left != k)
              correct=left;
        }
      /*
        Check right for an even number of crossings between k and j.
      */
      if (correct == -1)
        {
          count=0;
          for (l=k+1; l < right; l++)
            if (zero_crossing[i+1].crossings[l] != 0)
              count++;
          if ((count % 2) == 0)
            if (right != k)
              correct=right;
        }
      l=zero_crossing[i].crossings[j];
      zero_crossing[i].crossings[j]=0;
      if (correct != -1)
        zero_crossing[i].crossings[correct]=l;
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   D e f i n e R e g i o n                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method DefineRegion defines the left and right boundaries of a peak
%  region.
%
%  The format of the DefineRegion method is:
%
%      status=DefineRegion(extrema,extents)
%
%  A description of each parameter follows.
%
%    o extrema:  Specifies a pointer to an array of integers.  They
%      represent the peaks and valleys of the histogram for each color
%      component.
%
%    o extents:  This pointer to an ExtentPacket represent the extends
%      of a particular peak or valley of a color component.
%
%
*/
static int DefineRegion(const short *extrema,ExtentPacket *extents)
{
  /*
    Initialize to default values.
  */
  extents->left=0;
  extents->center=0;
  extents->right=MaxRGB;
  /*
    Find the left side (maxima).
  */
  for ( ; extents->index <= MaxRGB; extents->index++)
    if (extrema[extents->index] > 0)
      break;
  if (extents->index > MaxRGB)
    return(False);  /* no left side - no region exists */
  extents->left=extents->index;
  /*
    Find the right side (minima).
  */
  for ( ; extents->index <= MaxRGB; extents->index++)
    if (extrema[extents->index] < 0)
      break;
  extents->right=extents->index-1;
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   D e r i v a t i v e H i s t o g r a m                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method DerivativeHistogram determines the derivative of the histogram
%  using central differencing.
%
%  The format of the DerivativeHistogram method is:
%
%      DerivativeHistogram(histogram,derivative)
%
%  A description of each parameter follows.
%
%    o histogram: Specifies an array of doubles representing the number of
%      pixels for each intensity of a particular color component.
%
%    o derivative: This array of doubles is initialized by DerivativeHistogram
%      to the derivative of the histogram using central differencing.
%
%
*/
static void DerivativeHistogram(const double *histogram,double *derivative)
{
  register int
    i,
    n;

  /*
    Compute endpoints using second order polynomial interpolation.
  */
  n=MaxRGB;
  derivative[0]=(-1.5*histogram[0]+2.0*histogram[1]-0.5*histogram[2]);
  derivative[n]=(0.5*histogram[n-2]-2.0*histogram[n-1]+1.5*histogram[n]);
  /*
    Compute derivative using central differencing.
  */
  for (i=1; i < n; i++)
    derivative[i]=(histogram[i+1]-histogram[i-1])/2;
  return;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+  I n i t i a l i z e H i s t o g r a m                                      %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method InitializeHistogram computes the histogram for an image.
%
%  The format of the InitializeHistogram method is:
%
%      InitializeHistogram(image,histogram)
%
%  A description of each parameter follows.
%
%    o image: Specifies a pointer to an Image structure;  returned from
%      ReadImage.
%
%    o histogram: Specifies an array of integers representing the number
%      of pixels for each intensity of a particular color component.
%
%
*/
static void InitializeHistogram(const Image *image,long **histogram)
{
  register int
    i;

  register RunlengthPacket
    *p;

  /*
    Initialize histogram.
  */
  for (i=0; i <= MaxRGB; i++)
  {
    histogram[Red][i]=0;
    histogram[Green][i]=0;
    histogram[Blue][i]=0;
  }
  p=image->pixels;
  for (i=0; i < (int) image->packets; i++)
  {
    histogram[Red][p->red]+=(p->length+1);
    histogram[Green][p->green]+=(p->length+1);
    histogram[Blue][p->blue]+=(p->length+1);
    p++;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   I n i t i a l i z e I n t e r v a l T r e e                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method InitializeIntervalTree initializes an interval tree from the
%  lists of zero crossings.
%
%  The format of the InitializeIntervalTree method is:
%
%      InitializeIntervalTree(zero_crossing,number_crossings)
%
%  A description of each parameter follows.
%
%    o zero_crossing: Specifies an array of structures of type ZeroCrossing.
%
%    o number_crossings: This unsigned int specifies the number of elements
%      in the zero_crossing array.
%
%
*/

static void InitializeList(IntervalTree **list,int *number_nodes,
  IntervalTree *node)
{
  if (node == (IntervalTree *) NULL)
    return;
  if (node->child == (IntervalTree *) NULL)
    list[(*number_nodes)++]=node;
  InitializeList(list,number_nodes,node->sibling);
  InitializeList(list,number_nodes,node->child);
}

static void MeanStability(register IntervalTree *node)
{
  register IntervalTree
    *child;

  if (node == (IntervalTree *) NULL)
    return;
  node->mean_stability=0.0;
  child=node->child;
  if (child != (IntervalTree *) NULL)
    {
      register double
        sum;

      register int
        count;

      sum=0.0;
      count=0;
      for ( ; child != (IntervalTree *) NULL; child=child->sibling)
      {
        sum+=child->stability;
        count++;
      }
      node->mean_stability=sum/(double) count;
    }
  MeanStability(node->sibling);
  MeanStability(node->child);
}

static void Stability(register IntervalTree *node)
{
  if (node == (IntervalTree *) NULL)
    return;
  if (node->child == (IntervalTree *) NULL)
    node->stability=0.0;
  else
    node->stability=node->tau-(node->child)->tau;
  Stability(node->sibling);
  Stability(node->child);
}

static IntervalTree *InitializeIntervalTree(const ZeroCrossing *zero_crossing,
  const unsigned int number_crossings)
{
  int
    left,
    number_nodes;

  IntervalTree
    *head,
    **list,
    *node,
    *root;

  register int
    i,
    j,
    k;

  /*
    Allocate interval tree.
  */
  list=(IntervalTree **) AllocateMemory(TreeLength*sizeof(IntervalTree *));
  if (list == (IntervalTree **) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return((IntervalTree *) NULL);
    }
  /*
    The root is the entire histogram.
  */
  root=(IntervalTree *) AllocateMemory(sizeof(IntervalTree));
  root->child=(IntervalTree *) NULL;
  root->sibling=(IntervalTree *) NULL;
  root->tau=0.0;
  root->left=0;
  root->right=MaxRGB;
  for (i=(-1); i < (int) number_crossings; i++)
  {
    /*
      Initialize list with all nodes with no children.
    */
    number_nodes=0;
    InitializeList(list,&number_nodes,root);
    /*
      Split list.
    */
    for (j=0; j < number_nodes; j++)
    {
      head=list[j];
      left=head->left;
      node=head;
      for (k=head->left+1; k < head->right; k++)
      {
        if (zero_crossing[i+1].crossings[k] != 0)
          {
            if (node == head)
              {
                node->child=(IntervalTree *)
                  AllocateMemory(sizeof(IntervalTree));
                node=node->child;
              }
            else
              {
                node->sibling=(IntervalTree *)
                  AllocateMemory(sizeof(IntervalTree));
                node=node->sibling;
              }
            node->tau=zero_crossing[i+1].tau;
            node->child=(IntervalTree *) NULL;
            node->sibling=(IntervalTree *) NULL;
            node->left=left;
            node->right=k;
            left=k;
          }
        }
        if (left != head->left)
          {
            node->sibling=(IntervalTree *) AllocateMemory(sizeof(IntervalTree));
            node=node->sibling;
            node->tau=zero_crossing[i+1].tau;
            node->child=(IntervalTree *) NULL;
            node->sibling=(IntervalTree *) NULL;
            node->left=left;
            node->right=head->right;
          }
      }
    }
  /*
    Determine the stability: difference between a nodes tau and its child.
  */
  Stability(root->child);
  MeanStability(root->child);
  FreeMemory((char *) list);
  return(root);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   O p t i m a l T a u                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method OptimalTau finds the optimal tau for each band of the histogram.
%
%  The format of the OptimalTau method is:
%
%      OptimalTau(histogram,max_tau,min_tau,delta_tau,smoothing_threshold,
%        extrema)
%
%  A description of each parameter follows.
%
%    o histogram: Specifies an array of integers representing the number
%      of pixels for each intensity of a particular color component.
%
%    o extrema:  Specifies a pointer to an array of integers.  They
%      represent the peaks and valleys of the histogram for each color
%      component.
%
%
*/

static void ActiveNodes(IntervalTree **list,int *number_nodes,
  IntervalTree *node)
{
  if (node == (IntervalTree *) NULL)
    return;
  if (node->stability >= node->mean_stability)
    {
      list[(*number_nodes)++]=node;
      ActiveNodes(list,number_nodes,node->sibling);
    }
  else
    {
      ActiveNodes(list,number_nodes,node->sibling);
      ActiveNodes(list,number_nodes,node->child);
    }
}

static void FreeNodes(const IntervalTree *node)
{
  if (node == (IntervalTree *) NULL)
    return;
  FreeNodes(node->sibling);
  FreeNodes(node->child);
  FreeMemory((char *) node);
}

static double OptimalTau(const long *histogram,const double max_tau,
  const double min_tau,const double delta_tau,const double smoothing_threshold,
  short *extrema)
{
  double
    average_tau,
    derivative[MaxRGB+1],
    second_derivative[MaxRGB+1],
    tau,
    value;

  int
    index,
    number_nodes,
    peak,
    x;

  IntervalTree
    **list,
    *node,
    *root;

  register int
    i,
    j,
    k;

  unsigned int
    count,
    number_crossings;

  ZeroCrossing
    *zero_crossing;

  /*
    Allocate interval tree.
  */
  list=(IntervalTree **) AllocateMemory(TreeLength*sizeof(IntervalTree *));
  if (list == (IntervalTree **) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(0.0);
    }
  /*
    Allocate zero crossing list.
  */
  count=(unsigned int) ((max_tau-min_tau)/delta_tau)+2;
  zero_crossing=(ZeroCrossing *) AllocateMemory(count*sizeof(ZeroCrossing));
  if (zero_crossing == (ZeroCrossing *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        (char *) NULL);
      return(0.0);
    }
  for (i=0; i < (int) count; i++)
    zero_crossing[i].tau=(-1);
  /*
    Initialize zero crossing list.
  */
  i=0;
  for (tau=max_tau; tau >= min_tau; tau-=delta_tau)
  {
    zero_crossing[i].tau=tau;
    ScaleSpace(histogram,tau,zero_crossing[i].histogram);
    DerivativeHistogram(zero_crossing[i].histogram,derivative);
    DerivativeHistogram(derivative,second_derivative);
    ZeroCrossHistogram(second_derivative,smoothing_threshold,
      zero_crossing[i].crossings);
    i++;
  }
  /*
    Add an entry for the original histogram.
  */
  zero_crossing[i].tau=0.0;
  for (j=0; j <= MaxRGB; j++)
    zero_crossing[i].histogram[j]=(double) histogram[j];
  DerivativeHistogram(zero_crossing[i].histogram,derivative);
  DerivativeHistogram(derivative,second_derivative);
  ZeroCrossHistogram(second_derivative,smoothing_threshold,
    zero_crossing[i].crossings);
  number_crossings=i;
  /*
    Ensure the scale-space fingerprints form lines in scale-space, not loops.
  */
  ConsolidateCrossings(zero_crossing,number_crossings);
  /*
    Force endpoints to be included in the interval.
  */
  for (i=0; i <= (int) number_crossings; i++)
  {
    for (j=0; j < MaxRGB; j++)
      if (zero_crossing[i].crossings[j] != 0)
        break;
    zero_crossing[i].crossings[0]=(-zero_crossing[i].crossings[j]);
    for (j=MaxRGB; j > 0; j--)
      if (zero_crossing[i].crossings[j] != 0)
        break;
    zero_crossing[i].crossings[MaxRGB]=(-zero_crossing[i].crossings[j]);
  }
  /*
    Initialize interval tree.
  */
  root=InitializeIntervalTree(zero_crossing,number_crossings);
  if (root == (IntervalTree *) NULL)
    return(0.0);
  /*
    Find active nodes:  stability is greater (or equal) to the mean stability of
    its children.
  */
  number_nodes=0;
  ActiveNodes(list,&number_nodes,root->child);
  /*
    Initialize extrema.
  */
  for (i=0; i <= MaxRGB; i++)
    extrema[i]=0;
  for (i=0; i < number_nodes; i++)
  {
    /*
      Find this tau in zero crossings list.
    */
    k=0;
    node=list[i];
    for (j=0; j <= (int) number_crossings; j++)
      if (zero_crossing[j].tau == node->tau)
        k=j;
    /*
      Find the value of the peak.
    */
    peak=zero_crossing[k].crossings[node->right] == -1;
    index=node->left;
    value=zero_crossing[k].histogram[index];
    for (x=node->left; x <= node->right; x++)
    {
      if (peak)
        {
          if (zero_crossing[k].histogram[x] > value)
            {
              value=zero_crossing[k].histogram[x];
              index=x;
            }
        }
      else
        if (zero_crossing[k].histogram[x] < value)
          {
            value=zero_crossing[k].histogram[x];
            index=x;
          }
    }
    for (x=node->left; x <= node->right; x++)
    {
      if (index == 0)
        index=MaxRGB+1;
      if (peak)
        extrema[x]=index;
      else
        extrema[x]=(-index);
    }
  }
  /*
    Determine the average tau.
  */
  average_tau=0.0;
  for (i=0; i < number_nodes; i++)
    average_tau+=list[i]->tau;
  average_tau/=(double) number_nodes;
  /*
    Free memory.
  */
  FreeNodes(root);
  FreeMemory((char *) zero_crossing);
  FreeMemory((char *) list);
  return(average_tau);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   S c a l e S p a c e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method ScaleSpace performs a scale-space filter on the 1D histogram.
%
%  The format of the ScaleSpace method is:
%
%      ScaleSpace(histogram,tau,scaled_histogram)
%
%  A description of each parameter follows.
%
%    o histogram: Specifies an array of doubles representing the number of
%      pixels for each intensity of a particular color component.
%
%
*/
static void ScaleSpace(const long *histogram,const double tau,
  double *scaled_histogram)
{
  double
    alpha,
    beta,
    gamma[MaxRGB+1],
    sum;

  register int
    u,
    x;

  alpha=1.0/(tau*sqrt((double) (2.0*M_PI)));
  beta=(-1.0/(2.0*tau*tau));
  for (x=0; x <= MaxRGB; x++)
    gamma[x]=exp((double) (beta*x*x));
  for (x=0; x <= MaxRGB; x++)
  {
    sum=0.0;
    for (u=0; u <= MaxRGB; u++)
      sum+=(double) histogram[u]*gamma[AbsoluteValue(x-u)];
    scaled_histogram[x]=alpha*sum;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   Z e r o C r o s s H i s t o g r a m                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%  Method ZeroCrossHistogram find the zero crossings in a histogram and
%  marks directions as:  1 is negative to positive; 0 is zero crossing; and
%  -1 is positive to negative.
%
%  The format of the ZeroCrossHistogram method is:
%
%      ZeroCrossHistogram(second_derivative,smoothing_threshold,crossings)
%
%  A description of each parameter follows.
%
%    o second_derivative: Specifies an array of doubles representing the
%      second derivative of the histogram of a particular color component.
%
%    o crossings:  This array of integers is initialized with
%      -1, 0, or 1 representing the slope of the first derivative of the
%      of a particular color component.
%
%
*/
static void ZeroCrossHistogram(double *second_derivative,
  const double smoothing_threshold,short *crossings)
{
  int
    parity;

  register int
    i;

  /*
    Merge low numbers to zero to help prevent noise.
  */
  for (i=0; i <= MaxRGB; i++)
    if ((second_derivative[i] < smoothing_threshold) &&
        (second_derivative[i] >= -smoothing_threshold))
      second_derivative[i]=0.0;
  /*
    Mark zero crossings.
  */
  parity=0;
  for (i=0; i <= MaxRGB; i++)
  {
    crossings[i]=0;
    if (second_derivative[i] < 0.0)
      {
        if (parity > 0)
          crossings[i]=(-1);
        parity=1;
      }
    else
      if (second_derivative[i] > 0.0)
        {
          if (parity < 0)
            crossings[i]=1;
          parity=(-1);
        }
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  S e g m e n t I m a g e                                                    %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method SegmentImage segment an image by analyzing the histograms of the
%  color components and identifying units that are homogeneous with the fuzzy
%  c-means technique.
%
%  Specify Icluster threshold as the number of pixels in each cluster must
%  exceed the the cluster threshold to be considered valid.  Smoothing
%  threshold eliminates noise in the second derivative of the histogram.
%  As the value is increased, you can expect a smoother second derivative.
%  The default is 1.5.
%
%  The format of the SegmentImage method is:
%
%      status=SegmentImage(image,colorspace,verbose)
%
%  A description of each parameter follows.
%
%    o colors: The SegmentImage function returns this integer
%      value.  It is the actual number of colors allocated in the
%      colormap.
%
%    o image: Specifies a pointer to an Image structure;  returned from
%      ReadImage.
%
%    o colorspace: An unsigned integer value that indicates the colorspace.
%      Empirical evidence suggests that distances in YUV or YIQ correspond to
%      perceptual color differences more closely than do distances in RGB
%      space.  The image is then returned to RGB colorspace after color
%      reduction.
%
%    o verbose:  A value greater than zero prints detailed information about
%      the identified classes.
%
%
*/
Export unsigned int SegmentImage(Image *image,const ColorspaceType colorspace,
  const unsigned int verbose,const double cluster_threshold,
  const double smoothing_threshold)
{
#define DeltaTau  0.5
#define Tau  5.2
#define WeightingExponent  2.0

  long
    *histogram[Dimension];

  register int
    i;

  short
    *extrema[Dimension];

  unsigned int
    status;

  /*
    Allocate histogram and extrema.
  */
  assert(image != (Image *) NULL);
  for (i=0; i < Dimension; i++)
  {
    histogram[i]=(long *) AllocateMemory((MaxRGB+1)*sizeof(long));
    extrema[i]=(short *) AllocateMemory((MaxRGB+1)*sizeof(short));
    if ((histogram[i] == (long *) NULL) || (extrema[i] == (short *) NULL))
      {
        MagickWarning(ResourceLimitWarning,"Memory allocation failed",
          (char *) NULL);
        for (i-- ; i >= 0; i--)
        {
          FreeMemory((char *) extrema[i]);
          FreeMemory((char *) histogram[i]);
        }
        return(False);
      }
  }
  if (colorspace != RGBColorspace)
    RGBTransformImage(image,colorspace);
  /*
    Initialize histogram.
  */
  InitializeHistogram(image,histogram);
  (void) OptimalTau(histogram[Red],Tau,0.2,DeltaTau,smoothing_threshold,
    extrema[Red]);
  (void) OptimalTau(histogram[Green],Tau,0.2,DeltaTau,smoothing_threshold,
    extrema[Green]);
  (void) OptimalTau(histogram[Blue],Tau,0.2,DeltaTau,smoothing_threshold,
    extrema[Blue]);
  /*
    Classify using the fuzzy c-Means technique.
  */
  status=Classify(image,extrema,cluster_threshold,WeightingExponent,verbose);
  if (colorspace != RGBColorspace)
    TransformRGBImage(image,colorspace);
  /*
    Free memory.
  */
  for (i=0; i < Dimension; i++)
  {
    FreeMemory((char *) extrema[i]);
    FreeMemory((char *) histogram[i]);
  }
  return(status);
}
