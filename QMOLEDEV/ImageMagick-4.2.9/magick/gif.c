/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                             GGGG  IIIII  FFFFF                              %
%                            G        I    F                                  %
%                            G  GG    I    FFF                                %
%                            G   G    I    F                                  %
%                             GGG   IIIII  F                                  %
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

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   D e c o d e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method DecodeImage uncompresses an image via GIF-coding.
%
%  The format of the DecodeImage method is:
%
%      Image *ReadGIFImage(const ImageInfo *image_info)
%
%  A description of each parameter follows:
%
%    o status:  Method DecodeImage returns True if all the pixels are
%      uncompressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%
*/
static unsigned int DecodeImage(Image *image)
{
#define MaxStackSize  4096
#define NullCode  (-1)

  int
    available,
    clear,
    code_mask,
    code_size,
    end_of_information,
    in_code,
    old_code;

  register int
    bits,
    code,
    count,
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *c,
    *p;

  register unsigned int
    datum;

  short
    *prefix;

  unsigned char
    data_size,
    first,
    *packet,
    *pixels,
    *pixel_stack,
    *suffix,
    *top_stack;

  unsigned long
    packets;

  unsigned short
    index;

  /*
    Allocate decoder tables.
  */
  assert(image != (Image *) NULL);
  pixels=(unsigned char *)
    AllocateMemory(image->columns*image->rows*sizeof(unsigned char));
  packet=(unsigned char *) AllocateMemory(256*sizeof(unsigned char));
  prefix=(short *) AllocateMemory(MaxStackSize*sizeof(short));
  suffix=(unsigned char *) AllocateMemory(MaxStackSize*sizeof(unsigned char));
  pixel_stack=(unsigned char *)
    AllocateMemory((MaxStackSize+1)*sizeof(unsigned char));
  if ((pixels == (unsigned char *) NULL) ||
      (packet == (unsigned char *) NULL) ||
      (prefix == (short *) NULL) ||
      (suffix == (unsigned char *) NULL) ||
      (pixel_stack == (unsigned char *) NULL))
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        image->filename);
      return(False);
    }
  /*
    Initialize GIF data stream decoder.
  */
  data_size=ReadByte(image);
  clear=1 << data_size;
  end_of_information=clear+1;
  available=clear+2;
  old_code=NullCode;
  code_size=data_size+1;
  code_mask=(1 << code_size)-1;
  for (code=0; code < clear; code++)
  {
    prefix[code]=0;
    suffix[code]=(unsigned char) code;
  }
  /*
    Decode GIF pixel stream.
  */
  datum=0;
  bits=0;
  c=0;
  count=0;
  first=0;
  top_stack=pixel_stack;
  p=pixels;
  for (i=0; i < (int) (image->columns*image->rows); )
  {
    if (top_stack == pixel_stack)
      {
        if (bits < code_size)
          {
            /*
              Load bytes until there is enough bits for a code.
            */
            if (count == 0)
              {
                /*
                  Read a new data block.
                */
                count=ReadBlobBlock(image,(char *) packet);
                if (count <= 0)
                  break;
                c=packet;
              }
            datum+=(*c) << bits;
            bits+=8;
            c++;
            count--;
            continue;
          }
        /*
          Get the next code.
        */
        code=datum & code_mask;
        datum>>=code_size;
        bits-=code_size;
        /*
          Interpret the code
        */
        if ((code > available) || (code == end_of_information))
          break;
        if (code == clear)
          {
            /*
              Reset decoder.
            */
            code_size=data_size+1;
            code_mask=(1 << code_size)-1;
            available=clear+2;
            old_code=NullCode;
            continue;
          }
        if (old_code == NullCode)
          {
            *top_stack++=suffix[code];
            old_code=code;
            first=(unsigned char) code;
            continue;
          }
        in_code=code;
        if (code == available)
          {
            *top_stack++=first;
            code=old_code;
          }
        while (code > clear)
        {
          *top_stack++=suffix[code];
          code=prefix[code];
        }
        first=suffix[code];
        /*
          Add a new string to the string table,
        */
        if (available >= MaxStackSize)
          break;
        *top_stack++=first;
        prefix[available]=old_code;
        suffix[available]=first;
        available++;
        if (((available & code_mask) == 0) && (available < MaxStackSize))
          {
            code_size++;
            code_mask+=available;
          }
        old_code=in_code;
      }
    /*
      Pop a pixel off the pixel stack.
    */
    top_stack--;
    *p=(*top_stack);
    p++;
    i++;
  }
  if (i < (int) (image->columns*image->rows))
    {
      for ( ; i < (int) (image->columns*image->rows); i++)
        *p++=0;
      MagickWarning(CorruptImageWarning,"Corrupt GIF image",image->filename);
    }
  /*
    Free decoder memory.
  */
  FreeMemory((char *) pixel_stack);
  FreeMemory((char *) suffix);
  FreeMemory((char *) prefix);
  FreeMemory((char *) packet);
  if (image->interlace != NoInterlace)
    {
      int
        pass,
        y;

      register int
        x;

      register unsigned char
        *q;

      static const int
        interlace_rate[4] = { 8, 8, 4, 2 },
        interlace_start[4] = { 0, 4, 2, 1 };

      unsigned char
        *interlaced_pixels;

      /*
        Interlace image.
      */
      interlaced_pixels=pixels;
      pixels=(unsigned char *)
        AllocateMemory(image->columns*image->rows*sizeof(unsigned char));
      if (pixels == (unsigned char *) NULL)
        {
          FreeMemory(interlaced_pixels);
          MagickWarning(ResourceLimitWarning,"Memory allocation failed",
            image->filename);
          return(False);
        }
      p=interlaced_pixels;
      q=pixels;
      for (pass=0; pass < 4; pass++)
      {
        y=interlace_start[pass];
        while (y < (int) image->rows)
        {
          q=pixels+(y*image->columns);
          for (x=0; x < (int) image->columns; x++)
          {
            *q=(*p);
            p++;
            q++;
          }
          y+=interlace_rate[pass];
        }
      }
      FreeMemory(interlaced_pixels);
    }
  /*
    Convert GIF pixels to runlength-encoded packets.
  */
  packets=0;
  image->pixels=(RunlengthPacket *)
    AllocateMemory(image->columns*image->rows*sizeof(RunlengthPacket));
  if (image->pixels == (RunlengthPacket *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Memory allocation failed",
        image->filename);
      return(False);
    }
  p=pixels;
  q=image->pixels;
  SetRunlengthEncoder(q);
  for (i=0; i < (int) (image->columns*image->rows); i++)
  {
    index=(*p++);
    if ((index == q->index) && ((int) q->length < MaxRunlength))
       q->length++;
     else
       {
         if (packets != 0)
           q++;
         packets++;
         q->index=index;
         q->length=0;
       }
    if (image->previous == (Image *) NULL)
      if (QuantumTick(i,image->columns*image->rows))
        ProgressMonitor(LoadImageText,i,image->columns*image->rows);
  }
  SetRunlengthPackets(image,packets);
  SyncImage(image);
  image->compression=LZWCompression;
  FreeMemory(pixels);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E n c o d e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method EncodeImage compresses an image via GIF-coding.
%
%  The format of the EncodeImage method is:
%
%      status=EncodeImage(image,data_size)
%
%  A description of each parameter follows:
%
%    o status:  Method EncodeImage returns True if all the pixels are
%      compressed without error, otherwise False.
%
%    o image: The address of a structure of type Image.
%
%
*/
static unsigned int EncodeImage(Image *image,const unsigned int data_size)
{
#define MaxCode(number_bits)  ((1 << (number_bits))-1)
#define MaxHashTable  5003
#define MaxGIFBits  12
#if defined(HasLZW)
#define MaxGIFTable  (1 << MaxGIFBits)
#else
#define MaxGIFTable  max_code
#endif
#define GIFOutputCode(code) \
{ \
  /*  \
    Emit a code. \
  */ \
  if (bits > 0) \
    datum|=((long) code << bits); \
  else \
    datum=(long) code; \
  bits+=number_bits; \
  while (bits >= 8) \
  { \
    /*  \
      Add a character to current packet. \
    */ \
    packet[byte_count++]=(unsigned char) (datum & 0xff); \
    if (byte_count >= 254) \
      { \
        (void) WriteByte(image,byte_count); \
        (void) WriteBlob(image,byte_count,(char *) packet); \
        byte_count=0; \
      } \
    datum>>=8; \
    bits-=8; \
  } \
  if (free_code > max_code)  \
    { \
      number_bits++; \
      if (number_bits == MaxGIFBits) \
        max_code=MaxGIFTable; \
      else \
        max_code=MaxCode(number_bits); \
    } \
}

  int
    bits,
    byte_count,
    i,
    next_pixel,
    number_bits;

  long
    datum;

  register int
    displacement,
    j,
    k;

  register RunlengthPacket
    *p;

  short
    clear_code,
    end_of_information_code,
    free_code,
    *hash_code,
    *hash_prefix,
    index,
    max_code,
    waiting_code;

  unsigned char
    *packet,
    *hash_suffix;

  /*
    Allocate encoder tables.
  */
  assert(image != (Image *) NULL);
  packet=(unsigned char *) AllocateMemory(256*sizeof(unsigned char));
  hash_code=(short *) AllocateMemory(MaxHashTable*sizeof(short));
  hash_prefix=(short *) AllocateMemory(MaxHashTable*sizeof(short));
  hash_suffix=(unsigned char *)
    AllocateMemory(MaxHashTable*sizeof(unsigned char));
  if ((packet == (unsigned char *) NULL) || (hash_code == (short *) NULL) ||
      (hash_prefix == (short *) NULL) ||
      (hash_suffix == (unsigned char *) NULL))
    return(False);
  /*
    Initialize GIF encoder.
  */
  number_bits=data_size;
  max_code=MaxCode(number_bits);
  clear_code=((short) 1 << (data_size-1));
  end_of_information_code=clear_code+1;
  free_code=clear_code+2;
  byte_count=0;
  datum=0;
  bits=0;
  for (i=0; i < MaxHashTable; i++)
    hash_code[i]=0;
  GIFOutputCode(clear_code);
  /*
    Encode pixels.
  */
  p=image->pixels;
  waiting_code=p->index;
  for (i=0; i < (int) image->packets; i++)
  {
    for (j=(i == 0) ? 1 : 0; j <= ((int) p->length); j++)
    {
      /*
        Probe hash table.
      */
      index=p->index & 0xff;
      k=(int) ((int) index << (MaxGIFBits-8))+waiting_code;
      if (k >= MaxHashTable)
        k-=MaxHashTable;
#if defined(HasLZW)
      if (hash_code[k] > 0)
        {
          if ((hash_prefix[k] == waiting_code) && (hash_suffix[k] == index))
            {
              waiting_code=hash_code[k];
              continue;
            }
          if (k == 0)
            displacement=1;
          else
            displacement=MaxHashTable-k;
          next_pixel=False;
          for ( ; ; )
          {
            k-=displacement;
            if (k < 0)
              k+=MaxHashTable;
            if (hash_code[k] == 0)
              break;
            if ((hash_prefix[k] == waiting_code) && (hash_suffix[k] == index))
              {
                waiting_code=hash_code[k];
                next_pixel=True;
                break;
              }
          }
          if (next_pixel == True)
            continue;
        }
#endif
      GIFOutputCode(waiting_code);
      if (free_code < MaxGIFTable)
        {
          hash_code[k]=free_code++;
          hash_prefix[k]=waiting_code;
          hash_suffix[k]=(unsigned char) index;
        }
      else
        {
          /*
            Fill the hash table with empty entries.
          */
          for (k=0; k < MaxHashTable; k++)
            hash_code[k]=0;
          /*
            Reset compressor and issue a clear code.
          */
          free_code=clear_code+2;
          GIFOutputCode(clear_code);
          number_bits=data_size;
          max_code=MaxCode(number_bits);
        }
      waiting_code=index;
      if (image->previous == (Image *) NULL)
        if (QuantumTick(i,image->packets))
          ProgressMonitor(SaveImageText,i,image->packets);
    }
    p++;
  }
  /*
    Flush out the buffered code.
  */
  GIFOutputCode(waiting_code);
  GIFOutputCode(end_of_information_code);
  if (bits > 0)
    {
      /*
        Add a character to current packet.
      */
      packet[byte_count++]=(unsigned char) (datum & 0xff);
      if (byte_count >= 254)
        {
          (void) WriteByte(image,byte_count);
          (void) WriteBlob(image,byte_count,(char *) packet);
          byte_count=0;
        }
    }
  /*
    Flush accumulated data.
  */
  if (byte_count > 0)
    {
      (void) WriteByte(image,byte_count);
      (void) WriteBlob(image,byte_count,(char *) packet);
    }
  /*
    Free encoder memory.
  */
  FreeMemory((char *) hash_suffix);
  FreeMemory((char *) hash_prefix);
  FreeMemory((char *) hash_code);
  FreeMemory((char *) packet);
  if (i < (int) image->packets)
    return(False);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e a d G I F I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ReadGIFImage reads a Compuserve Graphics image file and returns it.
%  It allocates the memory necessary for the new Image structure and returns a
%  pointer to the new image.
%
%  The format of the ReadGIFImage method is:
%
%      image=ReadGIFImage(image_info)
%
%  A description of each parameter follows:
%
%    o image:  Method ReadGIFImage returns a pointer to the image after
%      reading.  A null image is returned if there is a memory shortage or
%      an error occurs.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%
*/
Export Image *ReadGIFImage(const ImageInfo *image_info)
{
#define BitSet(byte,bit)  (((byte) & (bit)) == (bit))
#define LSBFirstOrder(x,y)  (((y) << 8) | (x))

  char
    geometry[MaxTextExtent];

  Image
    *image;

  int
    status;

  RectangleInfo
    page_info;

  register int
    i;

  register RunlengthPacket
    *q;

  register unsigned char
    *p;

  short int
    transparency_index;

  unsigned char
    background,
    c,
    flag,
    *global_colormap,
    header[MaxTextExtent],
    magick[12];

  unsigned int
    delay,
    dispose,
    global_colors,
    image_count,
    iterations;

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
    Determine if this is a GIF file.
  */
  status=ReadBlob(image,6,(char *) magick);
  if ((status == False) || ((strncmp((char *) magick,"GIF87",5) != 0) &&
      (strncmp((char *) magick,"GIF89",5) != 0)))
    ReaderExit(CorruptImageWarning,"Not a GIF image file",image);
  global_colors=0;
  global_colormap=(unsigned char *) NULL;
  page_info.width=LSBFirstReadShort(image);
  page_info.height=LSBFirstReadShort(image);
  flag=ReadByte(image);
  background=ReadByte(image);
  c=ReadByte(image);  /* reserved */
  if (BitSet(flag,0x80))
    {
      /*
        Read global colormap.
      */
      global_colors=1 << ((flag & 0x07)+1);
      global_colormap=(unsigned char *)
        AllocateMemory(3*global_colors*sizeof(unsigned char));
      if (global_colormap == (unsigned char *) NULL)
        ReaderExit(ResourceLimitWarning,"Unable to read image colormap file",
          image);
      (void) ReadBlob(image,3*global_colors,(char *) global_colormap);
    }
  delay=0;
  dispose=0;
  iterations=1;
  transparency_index=(-1);
  image_count=0;
  for ( ; ; )
  {
    status=ReadBlob(image,1,(char *) &c);
    if (status == False)
      break;
    if (c == ';')
      break;  /* terminator */
    if (c == '!')
      {
        /*
          GIF Extension block.
        */
        status=ReadBlob(image,1,(char *) &c);
        if (status == False)
          ReaderExit(CorruptImageWarning,"Unable to read extension block",
            image);
        switch (c)
        {
          case 0xf9:
          {
            /*
              Read Graphics Control extension.
            */
            while (ReadBlobBlock(image,(char *) header) > 0);
            dispose=header[0] >> 2;
            delay=(header[2] << 8) | header[1];
            if ((header[0] & 0x01) == 1)
              transparency_index=header[3];
            break;
          }
          case 0xfe:
          {
            int
              length;

            /*
              Read Comment extension.
            */
            for ( ; ; )
            {
              length=ReadBlobBlock(image,(char *) header);
              if (length <= 0)
                break;
              if (image->comments != (char *) NULL)
                {
                  image->comments=(char *) ReallocateMemory((char *)
                    image->comments,(Extent(image->comments)+length+1)*
                    sizeof(char));
                }
              else
                {
                  image->comments=(char *)
                    AllocateMemory((length+1)*sizeof(char));
                  if (image->comments != (char *) NULL)
                    *image->comments='\0';
                }
              if (image->comments == (char *) NULL)
                ReaderExit(ResourceLimitWarning,"Memory allocation failed",
                  image);
              header[length]='\0';
              (void) strcat(image->comments,(char *) header);
            }
            break;
          }
          case 0xff:
          {
            /*
              Read Netscape Loop extension.
            */
            int
              found_netscape_loop=False;

            if (ReadBlobBlock(image,(char *) header) > 0)
              found_netscape_loop=!strncmp((char *) header,"NETSCAPE2.0",11);
            while (ReadBlobBlock(image,(char *) header) > 0)
            if (found_netscape_loop)
              {
                /*
                  Look for terminator.
                */
                iterations=(header[2] << 8) | header[1];
              }
            break;
          }
          default:
          {
            while (ReadBlobBlock(image,(char *) header) > 0);
            break;
          }
        }
      }
    if (c != ',')
      continue;
    if (image_count != 0)
      {
        /*
          Allocate next image structure.
        */
        AllocateNextImage(image_info,image);
        if (image->next == (Image *) NULL)
          {
            DestroyImages(image);
            return((Image *) NULL);
          }
        image=image->next;
        ProgressMonitor(LoadImagesText,(unsigned int) TellBlob(image),
          (unsigned int) image->filesize);
      }
    image_count++;
    /*
      Read image attributes.
    */
    image->class=PseudoClass;
    page_info.x=LSBFirstReadShort(image);
    page_info.y=LSBFirstReadShort(image);
    image->columns=LSBFirstReadShort(image);
    image->rows=LSBFirstReadShort(image);
    flag=ReadByte(image);
    image->interlace=BitSet(flag,0x40) ? PlaneInterlace : NoInterlace;
    image->colors=!BitSet(flag,0x80) ? global_colors : 1 << ((flag & 0x07)+1);
    FormatString(geometry,"%ux%u%+d%+d",page_info.width,page_info.height,
      page_info.x,page_info.y);
    if (image_info->page == (char *) NULL)
      image->page=PostscriptGeometry(geometry);
    if (image_info->delay == (char *) NULL)
      image->delay=delay;
    if (image_info->dispose == (char *) NULL)
      image->dispose=dispose;
    if (image_info->iterations == (char *) NULL)
      image->iterations=iterations;
    delay=0;
    dispose=0;
    iterations=1;
    if (image_info->ping)
      {
        if (transparency_index >= 0)
          image->class=DirectClass;
        CloseBlob(image);
        return(image);
      }
    if ((image->columns == 0) || (image->rows == 0))
      ReaderExit(CorruptImageWarning,"image size is 0",image);
    /*
      Inititialize colormap.
    */
    if (image->pixels != (RunlengthPacket *) NULL)
      FreeMemory((char *) image->pixels);
    image->pixels=(RunlengthPacket *) NULL;
    image->colormap=(ColorPacket *)
      AllocateMemory(image->colors*sizeof(ColorPacket));
    if (image->colormap == (ColorPacket *) NULL)
      ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
    if (!BitSet(flag,0x80))
      {
        /*
          Use global colormap.
        */
        p=global_colormap;
        for (i=0; i < (int) image->colors; i++)
        {
          image->colormap[i].red=UpScale(*p++);
          image->colormap[i].green=UpScale(*p++);
          image->colormap[i].blue=UpScale(*p++);
        }
        image->background_color=
          image->colormap[Min(background,image->colors-1)];
      }
    else
      {
        unsigned char
          *colormap;

        /*
          Read local colormap.
        */
        colormap=(unsigned char *)
          AllocateMemory(3*image->colors*sizeof(unsigned char));
        if (colormap == (unsigned char *) NULL)
          ReaderExit(ResourceLimitWarning,"Memory allocation failed",image);
        (void) ReadBlob(image,3*image->colors,(char *) colormap);
        p=colormap;
        for (i=0; i < (int) image->colors; i++)
        {
          image->colormap[i].red=UpScale(*p++);
          image->colormap[i].green=UpScale(*p++);
          image->colormap[i].blue=UpScale(*p++);
        }
        FreeMemory((char *) colormap);
      }
    /*
      Decode image.
    */
    status=DecodeImage(image);
    if (transparency_index >= 0)
      {
        /*
          Create matte channel.
        */
        ColorPacket
          transparent_color;

        transparent_color=
          image->colormap[Min(transparency_index,image->colors-1)];
        q=image->pixels;
        for (i=0; i < (int) image->packets; i++)
        {
          if (q->index != (unsigned short) transparency_index)
            q->index=Opaque;
          else
            {
              q->index=Transparent;
              q->red=transparent_color.red;
              q->green=transparent_color.green;
              q->blue=transparent_color.blue;
            }
          q++;
        }
        transparency_index=(-1);
        image->class=DirectClass;
        image->matte=True;
      }
    if (status == False)
      {
        MagickWarning(CorruptImageWarning,"Corrupt GIF image",image->filename);
        break;
      }
    if (image_info->subrange != 0)
      {
        if (image->scene < image_info->subimage)
          {
            Image
              subimage;

            /*
              Destroy image.
            */
            subimage=(*image);
            image->file=(FILE *) NULL;
            DestroyImage(image);
            image=AllocateImage(image_info);
            if (image == (Image *) NULL)
              return((Image *) NULL);
            image->file=subimage.file;
            image->scene=subimage.scene+1;
            image_count=0;
          }
        else
          if (image->scene >= (image_info->subimage+image_info->subrange-1))
            break;
      }
  }
  if (global_colormap != (unsigned char *) NULL)
    FreeMemory((char *) global_colormap);
  if (image->pixels == (RunlengthPacket *) NULL)
    ReaderExit(CorruptImageWarning,"Corrupt GIF image or subimage not found",
      image);
  while (image->previous != (Image *) NULL)
    image=image->previous;
  CloseBlob(image);
  return(image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   W r i t e G I F I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method WriteGIFImage writes an image to a file in the Compuserve Graphics
%  image format.
%
%  The format of the WriteGIFImage method is:
%
%      unsigned int WriteGIFImage(const ImageInfo *image_info,Image *image)
%
%  A description of each parameter follows.
%
%    o status: Method WriteGIFImage return True if the image is written.
%      False is returned is there is a memory shortage or if the image file
%      fails to write.
%
%    o image_info: Specifies a pointer to an ImageInfo structure.
%
%    o image:  A pointer to a Image structure.
%
%
*/
Export unsigned int WriteGIFImage(const ImageInfo *image_info,Image *image)
{
  Image
    *next_image;

  RectangleInfo
    page_info;

  register int
    i,
    j,
    x;

  register RunlengthPacket
    *p;

  register unsigned char
    *q;

  unsigned char
    bits_per_pixel,
    c,
    *colormap,
    *global_colormap,
    *matte;

  unsigned int
    colors,
    height,
    interlace,
    scene,
    status,
    transparency,
    width;

  /*
    Open output image file.
  */
  status=OpenBlob(image_info,image,WriteBinaryType);
  if (status == False)
    WriterExit(FileOpenWarning,"Unable to open file",image);
  /*
    Determine image bounding box.
  */
  page_info.width=image->columns;
  page_info.height=image->rows;
  page_info.x=0;
  page_info.y=0;
  next_image=image;
  for (next_image=image; next_image != (Image *) NULL; )
  {
    (void) ParseGeometry(next_image->page,&page_info.x,&page_info.y,
      &width,&height);
    if ((next_image->columns+page_info.x) > page_info.width)
      page_info.width=next_image->columns+page_info.x;
    if ((next_image->rows+page_info.y) > page_info.height)
      page_info.height=next_image->rows+page_info.y;
    next_image=next_image->next;
  }
  /*
    Allocate colormap.
  */
  matte=(unsigned char *)
    AllocateMemory(page_info.width*page_info.height*sizeof(unsigned char));
  global_colormap=(unsigned char *) AllocateMemory(768*sizeof(unsigned char));
  colormap=(unsigned char *) AllocateMemory(768*sizeof(unsigned char));
  if ((matte == (unsigned char *) NULL) ||
      (global_colormap == (unsigned char *) NULL) ||
      (colormap == (unsigned char *) NULL))
    WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
  for (i=0; i < 768; i++)
    colormap[i]=0;
  /*
    Write GIF header.
  */
  if ((image->comments == (char *) NULL) && !image_info->adjoin &&
      !image->matte)
    (void) WriteBlob(image,6,"GIF87a");
  else
    if (Latin1Compare(image_info->magick,"GIF87") == 0)
      (void) WriteBlob(image,6,"GIF87a");
    else
      (void) WriteBlob(image,6,"GIF89a");
  if (image_info->page != (char *) NULL)
    (void) ParseGeometry(image_info->page,&page_info.x,&page_info.y,
      &page_info.width,&page_info.height);
  LSBFirstWriteShort(image,page_info.width);
  LSBFirstWriteShort(image,page_info.height);
  page_info.x=0;
  page_info.y=0;
  /*
    Write images to file.
  */
  interlace=image_info->interlace;
  if (image_info->adjoin && (image->next != (Image *) NULL))
    interlace=NoInterlace;
  scene=0;
  do
  {
    TransformRGBImage(image,RGBColorspace);
    transparency=False;
    if (IsPseudoClass(image))
      colors=image->colors;
    else
      {
        QuantizeInfo
          quantize_info;

        if (image->matte)
          {
            /*
              Track all the transparent pixels.
            */
            if (!UncondenseImage(image))
              return(False);
            p=image->pixels;
            for (i=0; i < (int) image->packets; i++)
            {
              matte[i]=p->index == Transparent;
              if (p->index == Transparent)
                transparency=True;
              p++;
            }
          }
        GetQuantizeInfo(&quantize_info);
        quantize_info.number_colors=transparency ? 255 : 256;
        quantize_info.dither=image_info->dither;
        (void) QuantizeImage(&quantize_info,image);
        SyncImage(image);
        colors=image->colors;
        if (transparency)
          {
            /*
              Set the transparent pixel index.
            */
            if (!UncondenseImage(image))
              return(False);
            p=image->pixels;
            image->class=DirectClass;
            image->matte=True;
            for (i=0; i < (int) image->packets; i++)
            {
              if (matte[i])
                p->index=image->colors;
              p++;
            }
            colors++;
          }
      }
    for (bits_per_pixel=1; bits_per_pixel < 8; bits_per_pixel++)
      if ((1 << bits_per_pixel) >= (int) colors)
        break;
    q=colormap;
    for (i=0; i < (int) image->colors; i++)
    {
      *q++=DownScale(image->colormap[i].red);
      *q++=DownScale(image->colormap[i].green);
      *q++=DownScale(image->colormap[i].blue);
    }
    if (transparency)
      {
        *q++=DownScale(image->background_color.red);
        *q++=DownScale(image->background_color.green);
        *q++=DownScale(image->background_color.blue);
        i++;
      }
    for ( ; i < (int) (1 << bits_per_pixel); i++)
    {
      *q++=(Quantum) 0x0;
      *q++=(Quantum) 0x0;
      *q++=(Quantum) 0x0;
    }
    if ((image->previous == (Image *) NULL) || !image_info->adjoin)
      {
        /*
          Write global colormap.
        */
        c=0x80;
        c|=(8-1) << 4;  /* color resolution */
        c|=(bits_per_pixel-1);   /* size of global colormap */
        (void) WriteByte(image,(char) c);
        for (j=0; j < (int) (image->colors-1); j++)
          if (ColorMatch(image->background_color,image->colormap[j],0))
            break;
        (void) WriteByte(image,j);  /* background color */
        (void) WriteByte(image,0x0);  /* reserved */
        (void) WriteBlob(image,3*(1 << bits_per_pixel),(char *) colormap);
        for (j=0; j < 768; j++)
          global_colormap[j]=colormap[j];
      }
    if (Latin1Compare(image_info->magick,"GIF87") != 0)
      {
        /*
          Write Graphics Control extension.
        */
        (void) WriteByte(image,0x21);
        (void) WriteByte(image,0xf9);
        (void) WriteByte(image,0x04);
        c=image->dispose << 2;
        if (transparency)
          c|=0x01;
        (void) WriteByte(image,c);
        LSBFirstWriteShort(image,image->delay);
        (void) WriteByte(image,(char) image->colors);
        (void) WriteByte(image,0x00);
        if (image->comments != (char *) NULL)
          {
            register char
              *p;

            register unsigned int
              count;

            /*
              Write Comment extension.
            */
            (void) WriteByte(image,0x21);
            (void) WriteByte(image,0xfe);
            p=image->comments;
            while (Extent(p) > 0)
            {
              count=Min(Extent(p),255);
              (void) WriteByte(image,count);
              for (i=0; i < (int) count; i++)
                (void) WriteByte(image,*p++);
            }
            (void) WriteByte(image,0x0);
          }
        if ((image->previous == (Image *) NULL) &&
            (image->next != (Image *) NULL) && (image->iterations != 1))
          {
            /*
              Write Netscape Loop extension.
            */
            (void) WriteByte(image,0x21);
            (void) WriteByte(image,0xff);
            (void) WriteByte(image,0x0b);
            (void) WriteBlob(image,11,"NETSCAPE2.0");
            (void) WriteByte(image,0x03);
            (void) WriteByte(image,0x01);
            LSBFirstWriteShort(image,image->iterations);
            (void) WriteByte(image,0x00);
          }
      }
    (void) WriteByte(image,',');  /* image separator */
    /*
      Write the image header.
    */
    if (image->page != (char *) NULL)
      (void) ParseGeometry(image->page,&page_info.x,&page_info.y,
        &page_info.width,&page_info.height);
    LSBFirstWriteShort(image,page_info.x);
    LSBFirstWriteShort(image,page_info.y);
    LSBFirstWriteShort(image,image->columns);
    LSBFirstWriteShort(image,image->rows);
    c=0x00;
    if (interlace != NoInterlace)
      c|=0x40;  /* pixel data is interlaced */
    for (j=0; j < (int) (3*colors); j++)
      if (colormap[j] != global_colormap[j])
        break;
    if (j == (int) (3*colors))
      (void) WriteByte(image,(char) c);
    else
      {
        c|=0x80;
        c|=(bits_per_pixel-1);   /* size of local colormap */
        (void) WriteByte(image,(char) c);
        (void) WriteBlob(image,3*(1 << bits_per_pixel),(char *) colormap);
      }
    /*
      Write the image data.
    */
    c=Max(bits_per_pixel,2);
    (void) WriteByte(image,(char) c);
    if (interlace == NoInterlace)
      status=EncodeImage(image,Max(bits_per_pixel,2)+1);
    else
      {
        Image
          *interlaced_image;

        int
          pass,
          y;

        register RunlengthPacket
          *q;

        static const int
          interlace_rate[4] = { 8, 8, 4, 2 },
          interlace_start[4] = { 0, 4, 2, 1 };

        /*
          Interlace image.
        */
        if (!UncondenseImage(image))
          return(False);
        image->orphan=True;
        interlaced_image=CloneImage(image,image->columns,image->rows,False);
        image->orphan=False;
        if (interlaced_image == (Image *) NULL)
          WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
        p=image->pixels;
        q=interlaced_image->pixels;
        for (pass=0; pass < 4; pass++)
        {
          y=interlace_start[pass];
          while (y < (int) image->rows)
          {
            p=image->pixels+(y*image->columns);
            for (x=0; x < (int) image->columns; x++)
            {
              *q=(*p);
              p++;
              q++;
            }
            y+=interlace_rate[pass];
          }
        }
        interlaced_image->file=image->file;
        status=EncodeImage(interlaced_image,Max(bits_per_pixel,2)+1);
        interlaced_image->file=(FILE *) NULL;
        DestroyImage(interlaced_image);
      }
    if (status == False)
      WriterExit(ResourceLimitWarning,"Memory allocation failed",image);
    (void) WriteByte(image,0x0);
    if (image->next == (Image *) NULL)
      break;
    image->next->file=image->file;
    image=image->next;
    ProgressMonitor(SaveImagesText,scene++,GetNumberScenes(image));
  } while (image_info->adjoin);
  (void) WriteByte(image,';'); /* terminator */
  FreeMemory((char *) global_colormap);
  FreeMemory((char *) colormap);
  FreeMemory((char *) matte);
  if (image_info->adjoin)
    while (image->previous != (Image *) NULL)
      image=image->previous;
  CloseBlob(image);
  return(True);
}
