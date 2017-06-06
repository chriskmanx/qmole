/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            PPPP   DDDD   FFFFF                              %
%                            P   P  D   D  F                                  %
%                            PPPP   D   D  FFF                                %
%                            P      D   D  F                                  %
%                            P      DDDD   F                                  %
%                                                                             %
%                                                                             %
%                    Read/Write ImageMagick Image Format.                     %
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
*/

/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"
#if defined(HasTIFF)
#define CCITTParam  "-1"
#else
#define CCITTParam  "0"
#endif

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d P D F I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadPDFImage reads a Portable Document Format image file and
%  returns it.  It allocates the memory necessary for the new Image structure
%  and returns a pointer to the new image.
%
%  The format of the ReadPDFImage method is:
%
%      Image *ReadPDFImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadPDFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      if the image cannot be read.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadPDFImage(const ImageInfo *image_info)
{
#define MediaBox  "/MediaBox ["

  char
    density[MaxTextExtent],
    command[MaxTextExtent],
    filename[MaxTextExtent],
    geometry[MaxTextExtent],
    options[MaxTextExtent],
    postscript_filename[MaxTextExtent];

  DelegateInfo
    delegate_info;

  double
    dx_resolution,
    dy_resolution;

  FILE
    *file;

  Image
    *image,
    *next_image;

  ImageInfo
    *local_info;

  int
    count,
    status;

  long int
    filesize;

  RectangleInfo
    box,
    page_info;

  register char
    *p;

  register int
    c;

  SegmentInfo
    bounding_box;

  unsigned int
    height,
    portrait,
    width;

  if (image_info->monochrome)
    {
      if (!GetDelegateInfo("gs-mono",(char *) NULL,&delegate_info))
        return((Image *) NULL);
    }
  else
    if (!GetDelegateInfo("gs-color",(char *) NULL,&delegate_info))
      return((Image *) NULL);
  /*
    Allocate image structure.
  */
  image=AllocateImage(image_info);
  if (image == (Image *) NULL)
    return((Image *) NULL);
  /*
    Open image file.
  */
  status=OpenBlob(image_info,image,ReadBinaryType);
  if (status == False)
    ReaderExit(FileOpenWarning,"Unable to open file",image);
  /*
    Open temporary output file.
  */
  TemporaryFilename(postscript_filename);
  file=fopen(postscript_filename,WriteBinaryType);
  if (file == (FILE *) NULL)
    ReaderExit(FileOpenWarning,"Unable to write file",image);
  /*
    Set the page geometry.
  */
  dx_resolution=72.0;
  dy_resolution=72.0;
  if ((image->x_resolution == 0.0) || (image->y_resolution == 0.0))
    {
     (void) strcpy(density,PSDensityGeometry);
      count=sscanf(density,"%lfx%lf",&image->x_resolution,&image->y_resolution);
      if (count != 2)
        image->y_resolution=image->x_resolution;
    }
  FormatString(density,"%gx%g",image->x_resolution,image->y_resolution);
  page_info.width=612;
  page_info.height=792;
  page_info.x=0;
  page_info.y=0;
  (void) ParseImageGeometry(PSPageGeometry,&page_info.x,&page_info.y,
    &page_info.width,&page_info.height);
  portrait=True;
  /*
    Determine page geometry from the PDF media box.
  */
  box.width=0;
  box.height=0;
  for (p=command; ; )
  {
    c=ReadByte(image);
    if (c == EOF)
      break;
    (void) fputc(c,file);
    *p++=(char) c;
    if ((c != '\n') && (c != '\r') && ((p-command) < (MaxTextExtent-1)))
      continue;
    *p='\0';
    p=command;
    /*
      Continue unless this is a MediaBox statement.
    */
    if (strncmp(command,"/Rotate 90",10) == 0)
      portrait=False;
    if (strncmp(MediaBox,command,Extent(MediaBox)) != 0)
      continue;
    count=sscanf(command,"/MediaBox [ %lf %lf %lf %lf",&bounding_box.x1,
      &bounding_box.y1,&bounding_box.x2,&bounding_box.y2);
    if (count != 4)
      continue;
    if ((bounding_box.x1 > bounding_box.x2) ||
        (bounding_box.y1 > bounding_box.y2))
      continue;
    /*
      Set Postscript render geometry.
    */
    width=(unsigned int) (bounding_box.x2-bounding_box.x1);
    if ((float) ((int) bounding_box.x2) != bounding_box.x2)
      width++;
    height=(unsigned int) (bounding_box.y2-bounding_box.y1);
    if ((float) ((int) bounding_box.y2) != bounding_box.y2)
      height++;
    if ((width <= box.width) && (height <= box.height))
      continue;
    page_info.width=width;
    page_info.height=height;
    box=page_info;
  }
  if (image_info->page != (char *) NULL)
    (void) ParseImageGeometry(image_info->page,&page_info.x,&page_info.y,
      &page_info.width,&page_info.height);
  FormatString(geometry,"%ux%u",
    (unsigned int) ((page_info.width*image->x_resolution+0.5)/dx_resolution),
    (unsigned int) ((page_info.height*image->y_resolution+0.5)/dy_resolution));
  if (ferror(file))
    {
      MagickWarning(FileOpenWarning,"An error has occurred writing to file",
        postscript_filename);
      (void) fclose(file);
      return((Image *) NULL);
    }
  (void) fclose(file);
  CloseBlob(image);
  filesize=image->filesize;
  DestroyImage(image);
  /*
    Use Ghostscript to convert Postscript image.
  */
  *options='\0';
  if (image_info->subrange != 0)
    FormatString(options,"-dFirstPage=%u -dLastPage=%u",
      image_info->subimage+1,image_info->subimage+image_info->subrange);
  (void) strcpy(filename,image_info->filename);
  TemporaryFilename((char *) image_info->filename);
  FormatString(command,delegate_info.commands,image_info->antialias ? 4 : 1,
    image_info->antialias ? 4 : 1,geometry,density,options,image_info->filename,
    postscript_filename);
  ProgressMonitor(RenderPostscriptText,0,8);
  status=SystemCommand(image_info->verbose,command);
  ProgressMonitor(RenderPostscriptText,7,8);
  if (status)
    {
      MagickWarning(CorruptImageWarning,"Portable Document delegation failed",
        image_info->filename);
      (void) remove(postscript_filename);
      return((Image *) NULL);
    }
  local_info=CloneImageInfo(image_info);
  GetBlobInfo(&local_info->blob);
  image=ReadPNMImage(local_info);
  DestroyImageInfo(local_info);
  (void) remove(postscript_filename);
  (void) remove(image_info->filename);
  if (image == (Image *) NULL)
    {
      MagickWarning(CorruptImageWarning,"Portable Document delegation failed",
        image_info->filename);
      return((Image *) NULL);
    }
  (void) strcpy((char *) image_info->filename,filename);
  do
  {
    (void) strcpy(image->magick,"PDF");
    (void) strcpy(filename,image_info->filename);
    image->filesize=filesize;
    if (!portrait)
      {
        Image
          *rotated_image;

        /*
          Rotate image.
        */
        image->orphan=True;
        rotated_image=RotateImage(image,90,False,True);
        image->orphan=False;
        if (rotated_image != (Image *) NULL)
          {
            DestroyImage(image);
            image=rotated_image;
          }
      }
    next_image=image->next;
    if (next_image != (Image *) NULL)
      image=next_image;
  } while (next_image != (Image *) NULL);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e P D F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WritePDFImage writes an image in the Portable Document image
%  format.
%
%  The format of the WritePDFImage method is:
%
%      unsigned int WritePDFImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WritePDFImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WritePDFImage(const ImageInfo *image_info,Image *image)
{
#define ObjectsPerImage  12

  char
    buffer[MaxTextExtent],
    date[MaxTextExtent],
    density[MaxTextExtent],
    geometry[MaxTextExtent],
    **labels;

  CompressionType
    compression;

  double
    dx_resolution,
    dy_resolution,
    x_resolution,
    x_scale,
    y_resolution,
    y_scale;

  int
    count,
    status,
    x,
    y;

  Image
    encode_image,
    *tile_image;

  RectangleInfo
    media_info;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  register int
    i,
    j;

  time_t
    timer;

  unsigned char
    *pixels;

  unsigned int
    height,
    info_id,
    object,
    pages_id,
    root_id,
    scene,
    text_size,
    width;

  unsigned long
    length,
    number_packets,
    *xref;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  if ((image->file == stdout) || image->pipe)
    {
      /*
        Write standard output or pipe to temporary file.
      */
      encode_image=(*image);
      TemporaryFilename(image->filename);
      image->temporary=True;
      status=OpenBlob(image_info,image,WriteBinaryType);
      if (status == False)
        WriterExit(FileOpenWarning,"Unable to open file",image);
    }
  compression=image_info->compression;
#if defined(HasZLIB)
  if (compression == UndefinedCompression)
    compression=ZipCompression;
#else
#if defined(HasLZW)
  if (compression == UndefinedCompression)
    compression=LZWCompression;
#endif
#endif
  /*
    Allocate X ref memory.
  */
  xref=(unsigned long *) AllocateMemory(2048*sizeof(unsigned long));
  if (xref == (unsigned long *) NULL)
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  /*
    Write Info object.
  */
  object=0;
  (void) strcpy(buffer,"%PDF-1.1 \n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  xref[object++]=TellBlob(image);
  info_id=object;
  (void) sprintf(buffer,"%u 0 obj\n",object);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"<<\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  timer=time((time_t *) NULL);
  (void) localtime(&timer);
  (void) strcpy(date,ctime(&timer));
  date[Extent(date)-1]='\0';
  (void) sprintf(buffer,"/CreationDate (%.1024s)\n",date);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Producer (%.1024s)\n",MagickVersion);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,">>\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"endobj\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  /*
    Write Catalog object.
  */
  xref[object++]=TellBlob(image);
  root_id=object;
  (void) sprintf(buffer,"%u 0 obj\n",object);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"<<\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"/Type /Catalog\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Pages %u 0 R\n",object+1);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,">>\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"endobj\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  /*
    Write Pages object.
  */
  xref[object++]=TellBlob(image);
  pages_id=object;
  (void) sprintf(buffer,"%u 0 obj\n",object);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"<<\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"/Type /Pages\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Kids [ %u 0 R ",object+1);
  (void) WriteBlob(image,strlen(buffer),buffer);
  count=pages_id+ObjectsPerImage+1;
  if (image_info->adjoin)
    {
      Image
        *kid_image;

      /*
        Predict page object id's.
      */
      kid_image=image;
      for ( ; kid_image->next != (Image *) NULL; count+=ObjectsPerImage)
      {
        (void) sprintf(buffer,"%d 0 R ",count);
        (void) WriteBlob(image,strlen(buffer),buffer);
        kid_image=kid_image->next;
      }
      xref=(unsigned long *)
        ReallocateMemory((char *) xref,(count+2048)*sizeof(unsigned long));
      if (xref == (unsigned long *) NULL)
        WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    }
  (void) strcpy(buffer,"]\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Count %u\n",(count-pages_id)/ObjectsPerImage);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,">>\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"endobj\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  scene=0;
  do
  {
    /*
      Scale image to size of Portable Document page.
    */
    text_size=0;
    if (image->label != (char *) NULL)
      text_size=MultilineCensus(image->label)*image_info->pointsize+12;
    width=image->columns;
    height=image->rows;
    x=0;
    y=text_size;
    FormatString(geometry,"%ux%u",image->columns,image->rows);
    if (image_info->page != (char *) NULL)
      (void) strcpy(geometry,image_info->page);
    else
      if (image->page != (char *) NULL)
        (void) strcpy(geometry,image->page);
      else
        if (Latin1Compare(image_info->magick,"PDF") == 0)
          (void) strcpy(geometry,PSPageGeometry);
    (void) ParseImageGeometry(geometry,&x,&y,&width,&height);
    (void) GetGeometry(geometry,&media_info.x,&media_info.y,
      &media_info.width,&media_info.height);
    /*
      Scale relative to dots-per-inch.
    */
    dx_resolution=72.0;
    dy_resolution=72.0;
    x_resolution=72.0;
    (void) strcpy(density,PSDensityGeometry);
    count=sscanf(density,"%lfx%lf",&x_resolution,&y_resolution);
    if (count != 2)
      y_resolution=x_resolution;
    if (image_info->density != (char *) NULL)
      {
        count=sscanf(image_info->density,"%lfx%lf",&x_resolution,&y_resolution);
        if (count != 2)
          y_resolution=x_resolution;
      }
    x_scale=(width*dx_resolution)/x_resolution;
    width=(unsigned int) (x_scale+0.5);
    y_scale=(height*dy_resolution)/y_resolution;
    height=(unsigned int) (y_scale+0.5);
    /*
      Write Page object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"<<\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Type /Page\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Parent %u 0 R\n",pages_id);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Resources <<\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Font << /F%u %u 0 R >>\n",image->scene,
      object+4);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/XObject << /Im%u %u 0 R >>\n",image->scene,
      object+5);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/ProcSet %u 0 R >>\n",object+3);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/MediaBox [ %d %d %d %d ]\n",0,0,
      media_info.width,media_info.height);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Contents %u 0 R\n",object+1);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Thumb %u 0 R\n",object+8);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,">>\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Contents object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"<<\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Length %u 0 R\n",object+1);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,">>\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"stream\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    length=TellBlob(image);
    (void) strcpy(buffer,"q\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    labels=StringToList(image->label);
    if (labels != (char **) NULL)
      {
        for (i=0; labels[i] != (char *) NULL; i++)
        {
          (void) strcpy(buffer,"BT\n");
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) sprintf(buffer,"/F%u %u Tf\n",image->scene,
            image_info->pointsize);
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) sprintf(buffer,"%d %u Td\n",x,y+height+
            i*image_info->pointsize+12);
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) sprintf(buffer,"(%.1024s) Tj\n",labels[i]);
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) strcpy(buffer,"ET\n");
          (void) WriteBlob(image,strlen(buffer),buffer);
          FreeMemory(labels[i]);
        }
        FreeMemory((char *) labels);
      }
    (void) sprintf(buffer,"%g 0 0 %g %d %d cm\n",x_scale,y_scale,x,y);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Im%u Do\n",image->scene);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"Q\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    length=TellBlob(image)-length;
    (void) strcpy(buffer,"endstream\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Length object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"%lu\n",length);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Procset object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      (void) strcpy(buffer,"[ /PDF /Text /ImageC");
    else
      if (IsFaxImage(image))
        (void) strcpy(buffer,"[ /PDF /Text /ImageB");
      else
        (void) strcpy(buffer,"[ /PDF /Text /ImageI");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer," ]\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Font object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"<<\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Type /Font\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Subtype /Type1\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Name /F%u\n",image->scene);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/BaseFont /Helvetica\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Encoding /MacRomanEncoding\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,">>\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write XObject object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"<<\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Type /XObject\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"/Subtype /Image\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Name /Im%u\n",image->scene);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (compression == NoCompression)
      (void) strcpy(buffer,"/Filter /ASCII85Decode\n");
    else
      if (!IsFaxImage(image))
        {
          (void) sprintf(buffer,"/Filter [ /ASCII85Decode /%.1024s ]\n",
            compression == ZipCompression ? "FlateDecode" :
            compression == LZWCompression ? "LZWDecode" : "RunLengthDecode");
          (void) WriteBlob(image,strlen(buffer),buffer);
        }
      else
        {
          (void) strcpy(buffer,
            "/Filter [ /ASCII85Decode /CCITTFaxDecode ]\n");
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) sprintf(buffer,
            "/DecodeParms [ << >> << /K %.1024s /Columns %d /Rows %d >> ]\n",
            CCITTParam,image->columns,image->rows);
          (void) WriteBlob(image,strlen(buffer),buffer);
        }
    (void) sprintf(buffer,"/Width %u\n",image->columns);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Height %u\n",image->rows);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/ColorSpace %u 0 R\n",object+2);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/BitsPerComponent %d\n",
      IsFaxImage(image) ? 1 : 8);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Length %u 0 R\n",object+1);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,">>\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"stream\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    length=TellBlob(image);
    p=image->pixels;
    if (!IsPseudoClass(image) && !IsGrayImage(image))
      switch (compression)
      {
        case RunlengthEncodedCompression:
        default:
        {
          /*
            Allocate pixel array.
          */
          number_packets=(image->colorspace == CMYKColorspace ? 4 : 3)*
            image->columns*image->rows;
          pixels=(unsigned char *)
            AllocateMemory(number_packets*sizeof(unsigned char));
          if (pixels == (unsigned char *) NULL)
            WriterExit(ResourceLimitWarning,"Memory allocation failed",
              image);
          /*
            Dump runlength encoded pixels.
          */
          q=pixels;
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              if (image->matte && (p->index == Transparent))
                {
                  *q++=DownScale(MaxRGB);
                  *q++=DownScale(MaxRGB);
                  *q++=DownScale(MaxRGB);
                }
              else
                if (image->colorspace != CMYKColorspace)
                  {
                    *q++=DownScale(p->red);
                    *q++=DownScale(p->green);
                    *q++=DownScale(p->blue);
                  }
                else
                  {
                    *q++=DownScale(p->red);
                    *q++=DownScale(p->green);
                    *q++=DownScale(p->blue);
                    *q++=DownScale(p->index);
                  }
            }
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
          if (compression == ZipCompression)
            status=
              ZLIBEncodeImage(image,number_packets,image_info->quality,pixels);
          else
            if (compression == LZWCompression)
              status=LZWEncodeImage(image,number_packets,pixels);
            else
              status=PackbitsEncodeImage(image,number_packets,pixels);
          FreeMemory((char *) pixels);
          if (!status)
            {
              CloseBlob(image);
              return(False);
            }
          break;
        }
        case NoCompression:
        {
          /*
            Dump uncompressed DirectColor packets.
          */
          Ascii85Initialize();
          for (i=0; i < (int) image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              if (image->matte && (p->index == Transparent))
                {
                  Ascii85Encode(image,DownScale(MaxRGB));
                  Ascii85Encode(image,DownScale(MaxRGB));
                  Ascii85Encode(image,DownScale(MaxRGB));
                }
              else
                if (image->colorspace != CMYKColorspace)
                  {
                    Ascii85Encode(image,DownScale(p->red));
                    Ascii85Encode(image,DownScale(p->green));
                    Ascii85Encode(image,DownScale(p->blue));
                  }
                else
                  {
                    Ascii85Encode(image,DownScale(p->red));
                    Ascii85Encode(image,DownScale(p->green));
                    Ascii85Encode(image,DownScale(p->blue));
                    Ascii85Encode(image,DownScale(p->index));
                  }
            }
            p++;
            if (image->previous == (Image *) NULL)
              if (QuantumTick(i,image->packets))
                ProgressMonitor(SaveImageText,i,image->packets);
          }
          Ascii85Flush(image);
          break;
        }
      }
    else
      if (IsFaxImage(image))
        {
          register unsigned char
            bit,
            byte,
            polarity;

          polarity=Intensity(image->colormap[0]) > (MaxRGB >> 1);
          if (image->colors == 2)
            polarity=
              Intensity(image->colormap[0]) < Intensity(image->colormap[1]);
          bit=0;
          byte=0;
          x=0;
          switch (compression)
          {
            case RunlengthEncodedCompression:
            default:
            {
              if (Latin1Compare(CCITTParam,"0") == 0)
                (void) HuffmanEncodeImage((ImageInfo *) image_info,image);
              else
                (void) Huffman2DEncodeImage((ImageInfo *) image_info,image);
              break;
            }
            case NoCompression:
            {
              /*
                Dump uncompressed PseudoColor packets.
              */
              Ascii85Initialize();
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                {
                  byte<<=1;
                  if (p->index == polarity)
                    byte|=0x01;
                  bit++;
                  if (bit == 8)
                    {
                      Ascii85Encode(image,byte);
                      bit=0;
                      byte=0;
                    }
                  x++;
                  if (x == (int) image->columns)
                    {
                      /*
                        Advance to the next scanline.
                      */
                      if (bit != 0)
                        Ascii85Encode(image,byte << (8-bit));
                      if (image->previous == (Image *) NULL)
                        if (QuantumTick(y,image->rows))
                          ProgressMonitor(SaveImageText,y,image->rows);
                      bit=0;
                      byte=0;
                      x=0;
                      y++;
                   }
                }
                p++;
              }
              Ascii85Flush(image);
              break;
            }
          }
        }
      else
        {
          /*
            Dump number of colors and colormap.
          */
          switch (compression)
          {
            case RunlengthEncodedCompression:
            default:
            {
              /*
                Allocate pixel array.
              */
              number_packets=image->columns*image->rows;
              pixels=(unsigned char *)
                AllocateMemory(number_packets*sizeof(unsigned char));
              if (pixels == (unsigned char *) NULL)
                WriterExit(ResourceLimitWarning,"Memory allocation failed",
                  image);
              /*
                Dump Runlength encoded pixels.
              */
              q=pixels;
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                  *q++=(unsigned char) p->index;
                p++;
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(i,image->packets))
                    ProgressMonitor(SaveImageText,i,image->packets);
              }
              if (compression == ZipCompression)
                status=ZLIBEncodeImage(image,number_packets,image_info->quality,
                  pixels);
              else
                if (compression == LZWCompression)
                  status=LZWEncodeImage(image,number_packets,pixels);
                else
                  status=PackbitsEncodeImage(image,number_packets,pixels);
              FreeMemory((char *) pixels);
              if (!status)
                {
                  CloseBlob(image);
                  return(False);
                }
              break;
            }
            case NoCompression:
            {
              /*
                Dump uncompressed PseudoColor packets.
              */
              Ascii85Initialize();
              for (i=0; i < (int) image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                  Ascii85Encode(image,(unsigned char) p->index);
                p++;
                if (image->previous == (Image *) NULL)
                  if (QuantumTick(i,image->packets))
                    ProgressMonitor(SaveImageText,i,image->packets);
              }
              Ascii85Flush(image);
              break;
            }
          }
        }
    length=TellBlob(image)-length;
    (void) strcpy(buffer,"\nendstream\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Length object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"%lu\n",length);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Colorspace object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (image->colorspace == CMYKColorspace)
      (void) strcpy(buffer,"/DeviceCMYK\n");
    else
      if (!IsPseudoClass(image) && !IsGrayImage(image))
        (void) strcpy(buffer,"/DeviceRGB\n");
      else
        if (IsFaxImage(image))
          (void) strcpy(buffer,"/DeviceGray\n");
        else
          (void) sprintf(buffer,"[ /Indexed /DeviceRGB %u %u 0 R ]\n",
            image->colors-1,object+3);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Thumb object.
    */
    width=image->columns;
    height=image->rows;
    x=0;
    y=0;
    image->orphan=True;
    (void) ParseImageGeometry("106x106+0+0>",&x,&y,&width,&height);
    if (image->class == PseudoClass)
      tile_image=SampleImage(image,width,height);
    else
      tile_image=ZoomImage(image,width,height);
    image->orphan=False;
    if (tile_image == (Image *) NULL)
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"<<\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if (compression == NoCompression)
      (void) strcpy(buffer,"/Filter /ASCII85Decode\n");
    else
      if (!IsFaxImage(image))
        {
          (void) sprintf(buffer,"/Filter [ /ASCII85Decode /%.1024s ]\n",
            compression == ZipCompression ? "FlateDecode" :
            compression == LZWCompression ? "LZWDecode" : "RunLengthDecode");
          (void) WriteBlob(image,strlen(buffer),buffer);
        }
      else
        {
          (void) strcpy(buffer,
            "/Filter [ /ASCII85Decode /CCITTFaxDecode ]\n");
          (void) WriteBlob(image,strlen(buffer),buffer);
          (void) sprintf(buffer,
            "/DecodeParms [ << >> << /Columns %d /Rows %d >> ]\n",
            tile_image->columns,tile_image->rows);
          (void) WriteBlob(image,strlen(buffer),buffer);
        }
    (void) sprintf(buffer,"/Width %u\n",tile_image->columns);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Height %u\n",tile_image->rows);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/ColorSpace %u 0 R\n",object-1);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/BitsPerComponent %d\n",
      IsFaxImage(tile_image) ? 1 : 8);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"/Length %u 0 R\n",object+1);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,">>\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"stream\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    length=TellBlob(image);
    p=tile_image->pixels;
    if (!IsPseudoClass(tile_image) && !IsGrayImage(tile_image))
      switch (compression)
      {
        case RunlengthEncodedCompression:
        default:
        {
          /*
            Allocate pixel array.
          */
          number_packets=(image->colorspace == CMYKColorspace ? 4 : 3)*
            image->columns*image->rows;
          pixels=(unsigned char *)
            AllocateMemory(number_packets*sizeof(unsigned char));
          if (pixels == (unsigned char *) NULL)
            {
              DestroyImage(tile_image);
              WriterExit(ResourceLimitWarning,"Memory allocation failed",
                image);
            }
          /*
            Dump runlength encoded pixels.
          */
          q=pixels;
          for (i=0; i < (int) tile_image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              if (tile_image->matte && (p->index == Transparent))
                {
                  *q++=DownScale(MaxRGB);
                  *q++=DownScale(MaxRGB);
                  *q++=DownScale(MaxRGB);
                }
              else
                {
                  *q++=DownScale(p->red);
                  *q++=DownScale(p->green);
                  *q++=DownScale(p->blue);
                  if (image->colorspace == CMYKColorspace)
                    *q++=DownScale(p->index);
                }
            }
            p++;
          }
          if (compression == ZipCompression)
            status=
              ZLIBEncodeImage(image,number_packets,image_info->quality,pixels);
          else
            if (compression == LZWCompression)
              status=LZWEncodeImage(image,number_packets,pixels);
            else
              status=PackbitsEncodeImage(image,number_packets,pixels);
          FreeMemory((char *) pixels);
          if (!status)
            {
              CloseBlob(image);
              return(False);
            }
          break;
        }
        case NoCompression:
        {
          /*
            Dump uncompressed DirectColor packets.
          */
          Ascii85Initialize();
          for (i=0; i < (int) tile_image->packets; i++)
          {
            for (j=0; j <= ((int) p->length); j++)
            {
              if (tile_image->matte && (p->index == Transparent))
                {
                  Ascii85Encode(image,DownScale(MaxRGB));
                  Ascii85Encode(image,DownScale(MaxRGB));
                  Ascii85Encode(image,DownScale(MaxRGB));
                }
              else
                {
                  Ascii85Encode(image,DownScale(p->red));
                  Ascii85Encode(image,DownScale(p->green));
                  Ascii85Encode(image,DownScale(p->blue));
                  if (image->colorspace == CMYKColorspace)
                    Ascii85Encode(image,DownScale(p->index));
                }
            }
            p++;
          }
          Ascii85Flush(image);
          break;
        }
      }
    else
      if (IsFaxImage(tile_image))
        {
          register unsigned char
            bit,
            byte,
            polarity;

          polarity=Intensity(tile_image->colormap[0]) > (MaxRGB >> 1);
          if (image->colors == 2)
            polarity=Intensity(tile_image->colormap[0]) <
              Intensity(tile_image->colormap[1]);
          bit=0;
          byte=0;
          x=0;
          switch (compression)
          {
            case RunlengthEncodedCompression:
            default:
            {
              /*
                Allocate pixel array.
              */
              number_packets=((tile_image->columns+7) >> 3)*tile_image->rows;
              pixels=(unsigned char *)
                AllocateMemory(number_packets*sizeof(unsigned char));
              if (pixels == (unsigned char *) NULL)
                {
                  DestroyImage(tile_image);
                  WriterExit(ResourceLimitWarning,"Memory allocation failed",
                    image);
                }
              /*
                Dump Runlength encoded pixels.
              */
              q=pixels;
              for (i=0; i < (int) tile_image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                {
                  byte<<=1;
                  if (p->index == polarity)
                    byte|=0x01;
                  bit++;
                  if (bit == 8)
                    {
                      *q++=byte;
                      bit=0;
                      byte=0;
                    }
                  x++;
                  if (x == (int) tile_image->columns)
                    {
                      /*
                        Advance to the next scanline.
                      */
                      if (bit != 0)
                        *q++=byte << (8-bit);
                      if (image->previous == (Image *) NULL)
                        if (QuantumTick(y,tile_image->rows))
                          ProgressMonitor(SaveImageText,y,tile_image->rows);
                      bit=0;
                      byte=0;
                      x=0;
                      y++;
                   }
                }
                p++;
              }
              if (compression == ZipCompression)
                status=ZLIBEncodeImage(image,number_packets,image_info->quality,
                  pixels);
              else
                if (compression == LZWCompression)
                  status=LZWEncodeImage(image,number_packets,pixels);
                else
                  status=PackbitsEncodeImage(image,number_packets,pixels);
              FreeMemory((char *) pixels);
              if (!status)
                {
                  CloseBlob(image);
                  return(False);
                }
              break;
            }
            case NoCompression:
            {
              /*
                Dump uncompressed PseudoColor packets.
              */
              Ascii85Initialize();
              for (i=0; i < (int) tile_image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                {
                  byte<<=1;
                  if (p->index == polarity)
                    byte|=0x01;
                  bit++;
                  if (bit == 8)
                    {
                      Ascii85Encode(image,byte);
                      bit=0;
                      byte=0;
                    }
                  x++;
                  if (x == (int) tile_image->columns)
                    {
                      /*
                        Advance to the next scanline.
                      */
                      if (bit != 0)
                        Ascii85Encode(image,byte << (8-bit));
                      if (image->previous == (Image *) NULL)
                        if (QuantumTick(y,tile_image->rows))
                          ProgressMonitor(SaveImageText,y,tile_image->rows);
                      bit=0;
                      byte=0;
                      x=0;
                      y++;
                   }
                }
                p++;
              }
              Ascii85Flush(image);
              break;
            }
          }
        }
      else
        {
          /*
            Dump number of colors and colormap.
          */
          switch (compression)
          {
            case RunlengthEncodedCompression:
            default:
            {
              /*
                Allocate pixel array.
              */
              number_packets=tile_image->columns*tile_image->rows;
              pixels=(unsigned char *)
                AllocateMemory(number_packets*sizeof(unsigned char));
              if (pixels == (unsigned char *) NULL)
                {
                  DestroyImage(tile_image);
                  WriterExit(ResourceLimitWarning,
                    "Memory allocation failed",image);
                }
              /*
                Dump Runlength encoded pixels.
              */
              q=pixels;
              for (i=0; i < (int) tile_image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                  *q++=(unsigned char) p->index;
                p++;
              }
              if (compression == ZipCompression)
                status=ZLIBEncodeImage(image,number_packets,image_info->quality,
                  pixels);
              else
                if (compression == LZWCompression)
                  status=LZWEncodeImage(image,number_packets,pixels);
                else
                  status=PackbitsEncodeImage(image,number_packets,pixels);
              FreeMemory((char *) pixels);
              if (!status)
                {
                  CloseBlob(image);
                  return(False);
                }
              break;
            }
            case NoCompression:
            {
              /*
                Dump uncompressed PseudoColor packets.
              */
              Ascii85Initialize();
              for (i=0; i < (int) tile_image->packets; i++)
              {
                for (j=0; j <= ((int) p->length); j++)
                  Ascii85Encode(image,(unsigned char) p->index);
                p++;
              }
              Ascii85Flush(image);
              break;
            }
          }
        }
    DestroyImage(tile_image);
    length=TellBlob(image)-length;
    (void) strcpy(buffer,"\nendstream\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    /*
      Write Length object.
    */
    xref[object++]=TellBlob(image);
    (void) sprintf(buffer,"%u 0 obj\n",object);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) sprintf(buffer,"%lu\n",length);
    (void) WriteBlob(image,strlen(buffer),buffer);
    (void) strcpy(buffer,"endobj\n");
    (void) WriteBlob(image,strlen(buffer),buffer);
    if ((image->class == DirectClass) || IsFaxImage(image))
      {
        xref[object++]=0;
        xref[object++]=0;
      }
    else
      {
        /*
          Write Colormap object.
        */
        xref[object++]=TellBlob(image);
        (void) sprintf(buffer,"%u 0 obj\n",object);
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"<<\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"/Filter /ASCII85Decode \n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) sprintf(buffer,"/Length %u 0 R\n",object+1);
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,">>\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"stream\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        length=TellBlob(image);
        Ascii85Initialize();
        for (i=0; i < (int) image->colors; i++)
        {
          Ascii85Encode(image,DownScale(image->colormap[i].red));
          Ascii85Encode(image,DownScale(image->colormap[i].green));
          Ascii85Encode(image,DownScale(image->colormap[i].blue));
        }
        Ascii85Flush(image);
        length=TellBlob(image)-length;
        (void) strcpy(buffer,"\nendstream\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"endobj\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
        /*
          Write Length object.
        */
        xref[object++]=TellBlob(image);
        (void) sprintf(buffer,"%u 0 obj\n",object);
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) sprintf(buffer,"%lu\n",length);
        (void) WriteBlob(image,strlen(buffer),buffer);
        (void) strcpy(buffer,"endobj\n");
        (void) WriteBlob(image,strlen(buffer),buffer);
      }
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  /*
    Write Xref object.
  */
  length=TellBlob(image)-xref[0]+10;
  (void) strcpy(buffer,"xref\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"0 %u\n",object+1);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"0000000000 65535 f \n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  for (i=0; i < (int) object; i++)
  {
    (void) sprintf(buffer,"%010lu 00000 n \n",xref[i]);
    (void) WriteBlob(image,strlen(buffer),buffer);
  }
  (void) strcpy(buffer,"trailer\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"<<\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Size %u\n",object+1);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Info %u 0 R\n",info_id);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"/Root %u 0 R\n",root_id);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,">>\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"startxref\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) sprintf(buffer,"%lu\n",length);
  (void) WriteBlob(image,strlen(buffer),buffer);
  (void) strcpy(buffer,"%%EOF\n");
  (void) WriteBlob(image,strlen(buffer),buffer);
  FreeMemory((char *) xref);
  CloseBlob(image);
  if (image->temporary)
    {
      FILE
        *file;

      int
        c;

      /*
        Copy temporary file to standard output or pipe.
      */
      file=fopen(image->filename,ReadBinaryType);
      if (file == (FILE *) NULL)
        WriterExit(FileOpenWarning,"Unable to open file",image);
      for (c=fgetc(file); c != EOF; c=fgetc(file))
        (void) putc(c,encode_image.file);
      (void) fclose(file);
      (void) remove(image->filename);
      image->temporary=False;
      CloseBlob(&encode_image);
    }
  return(True);
}
