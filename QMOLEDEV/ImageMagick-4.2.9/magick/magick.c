/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                  M   M   AAA    GGGG  IIIII   CCCC  K   K                   %
%                  MM MM  A   A  G        I    C      K  K                    %
%                  M M M  AAAAA  G GGG    I    C      KKK                     %
%                  M   M  A   A  G   G    I    C      K  K                    %
%                  M   M  A   A   GGGG  IIIII   CCCC  K   K                   %
%                                                                             %
%                                                                             %
%               Methods to Read or List ImageMagick Image formats             %
%                                                                             %
%                                                                             %
%                            Software Design                                  %
%                            Bob Friesenhahn                                  %
%                              John Cristy                                    %
%                             November 1998                                   %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentations ("ImageMagick"),       %
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
  Global declarations.
*/
static MagickInfo
  *magick_info = (MagickInfo *) NULL;

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   G e t M a g i c k I n f o                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method GetMagickInfo returns a pointer MagickInfo structure that matches
%  the specified tag.  If tag is NULL, the head of the image format list is
%  returned.
%
%  The format of the GetMagickInfo method is:
%
%      MagickInfo *GetMagickInfo(const char *tag)
%
%  A description of each parameter follows:
%
%    o magick_info: Method GetMagickInfo returns a pointer MagickInfo
%      structure that matches the specified tag.
%
%    o tag: a character string that represents the image format we are
%      looking for.
%
%
*/

static unsigned int IsBMP(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if (strncmp((char *) magick,"BM",2) == 0)
    return(True);
  if (strncmp((char *) magick,"IC",2) == 0)
    return(True);
  if (strncmp((char *) magick,"PI",2) == 0)
    return(True);
  if (strncmp((char *) magick,"CI",2) == 0)
    return(True);
  if (strncmp((char *) magick,"CP",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsDCM(const unsigned char *magick,unsigned int length)
{
  if (length < 132)
    return(False);
  if (strncmp((char *) (magick+128),"DICM",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsDCX(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"\261\150\336\72",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsEPT(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"\305\320\323\306",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsFAX(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"DFAX",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsFITS(const unsigned char *magick,unsigned int length)
{
  if (length < 6)
    return(False);
  if (strncmp((char *) magick,"IT0",3) == 0)
    return(True);
  if (strncmp((char *) magick,"SIMPLE",6) == 0)
    return(True);
  return(False);
}

static unsigned int IsGIF(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"GIF8",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsHDF(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"\016\003\023\001",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsHTML(const unsigned char *magick,unsigned int length)
{
  if (length < 5)
    return(False);
  if (strncmp((char *) magick,"<HTML",5) == 0)
    return(True);
  if (strncmp((char *) magick,"<html",5) == 0)
    return(True);
  return(False);
}

static unsigned int IsJPEG(const unsigned char *magick,unsigned int length)
{
  if (length < 3)
    return(False);
  if (strncmp((char *) magick,"\377\330\377",3) == 0)
    return(True);
  return(False);
}

static unsigned int IsMIFF(const unsigned char *magick,unsigned int length)
{
  if (length < 14)
    return(False);
  if (strncmp((char *) magick,"id=ImageMagick",14) == 0)
    return(True);
  return(False);
}

static unsigned int IsMNG(const unsigned char *magick,unsigned int length)
{
  if (length < 8)
    return(False);
  if (strncmp((char *) magick,"\212MNG\r\n\032\n",8) == 0)
    return(True);
  return(False);
}

static unsigned int IsPCD(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"PCD_",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsPCL(const unsigned char *magick,unsigned int length)
{
  if (length < 3)
    return(False);
  if (strncmp((char *) magick,"\033E\033",3) == 0)
    return(True);
  return(False);
}

static unsigned int IsPCX(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if (strncmp((char *) magick,"\12\2",2) == 0)
    return(True);
  if (strncmp((char *) magick,"\12\5",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsPDF(const unsigned char *magick,unsigned int length)
{
  if (length < 5)
    return(False);
  if (strncmp((char *) magick,"%PDF-",5) == 0)
    return(True);
  return(False);
}

static unsigned int IsPNM(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if ((*magick == 'P') && isdigit((int) magick[1]))
    return(True);
  return(False);
}

static unsigned int IsPNG(const unsigned char *magick,unsigned int length)
{
  if (length < 8)
    return(False);
  if (strncmp((char *) magick,"\211PNG\r\n\032\n",8) == 0)
    return(True);
  return(False);
}

static unsigned int IsPS(const unsigned char *magick,unsigned int length)
{
  if (length < 3)
    return(False);
  if (strncmp((char *) magick,"\004%!",3) == 0)
    return(True);
  if (strncmp((char *) magick,"%!",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsPSD(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"8BPS",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsPWP(const unsigned char *magick,unsigned int length)
{
  if (length < 5)
    return(False);
  if (strncmp((char *) magick,"SFW95",5) == 0)
    return(True);
  return(False);
}

static unsigned int IsRLE(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if (strncmp((char *) magick,"\122\314",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsSCT(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if (strncmp((char *) magick,"CT",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsSFW(const unsigned char *magick,unsigned int length)
{
  if (length < 5)
    return(False);
  if (strncmp((char *) magick,"SFW94",5) == 0)
    return(True);
  return(False);
}

static unsigned int IsSGI(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if (strncmp((char *) magick,"\001\332",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsSUN(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if (strncmp((char *) magick,"\131\246\152\225",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsTIFF(const unsigned char *magick,unsigned int length)
{
  if (length < 4)
    return(False);
  if ((magick[0] == 0x4D) && (magick[1] == 0x4D))
    if ((magick[2] == 0x00) && (magick[3] == 0x2A))
      return(True);
  if (strncmp((char *) magick,"\111\111\052\000",4) == 0)
    return(True);
  return(False);
}

static unsigned int IsVICAR(const unsigned char *magick,unsigned int length)
{
  if (length < 7)
    return(False);
  if (strncmp((char *) magick,"LBLSIZE",7) == 0)
    return(True);
  if (strncmp((char *) magick,"NJPL1I",6) == 0)
    return(True);
  return(False);
}

static unsigned int IsVIFF(const unsigned char *magick,unsigned int length)
{
  if (length < 2)
    return(False);
  if (strncmp((char *) magick,"\253\1",2) == 0)
    return(True);
  return(False);
}

static unsigned int IsXBM(const unsigned char *magick,unsigned int length)
{
  if (length < 7)
    return(False);
  if (strncmp((char *) magick,"#define",7) == 0)
    return(True);
  return(False);
}

static unsigned int IsXPM(const unsigned char *magick,unsigned int length)
{
  if (length < 9)
    return(False);
  if (strncmp((char *) magick,"/* XPM */",9) == 0)
    return(True);
  return(False);
}

static unsigned int IsXWD(const unsigned char *magick,unsigned int length)
{
  if (length < 8)
    return(False);
  if ((magick[1] == 0x00) && (magick[2] == 0x00))
    if ((magick[5] == 0x00) && (magick[6] == 0x00))
      if ((magick[4] == 0x07) || (magick[7] == 0x07))
        return(True);
  return(False);
}

Export MagickInfo *GetMagickInfo(const char *tag)
{
  register MagickInfo
    *p;

  if (magick_info == (MagickInfo *) NULL)
    {
      (void) RegisterMagickInfo("AVS",ReadAVSImage,WriteAVSImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"AVS X image");
#if defined(HasJBIG)
      (void) RegisterMagickInfo("BIE",ReadJBIGImage,WriteJBIGImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"Joint Bi-level Image experts Group interchange format");
#endif
      (void) RegisterMagickInfo("BMP",ReadBMPImage,WriteBMPImage,IsBMP,True,
        True,"Microsoft Windows bitmap image");
      (void) RegisterMagickInfo("BMP24",ReadBMPImage,WriteBMPImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Microsoft Windows 24-bit bitmap image");
      (void) RegisterMagickInfo("CMYK",ReadCMYKImage,WriteCMYKImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Raw cyan, magenta, yellow, and black bytes");
      (void) RegisterMagickInfo("DCM",ReadDCMImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,IsDCM,False,True,
        "Digital Imaging and Communications in Medicine image");
      (void) RegisterMagickInfo("DCX",ReadPCXImage,WritePCXImage,IsDCX,True,
        True,"ZSoft IBM PC multi-page Paintbrush");
      (void) RegisterMagickInfo("DIB",ReadBMPImage,WriteBMPImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Microsoft Windows bitmap image");
      (void) RegisterMagickInfo("EPDF",ReadPDFImage,WritePDFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Encapsulated Portable Document Format");
      (void) RegisterMagickInfo("EPI",ReadPSImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Adobe Encapsulated PostScript Interchange format");
      (void) RegisterMagickInfo("EPS",ReadPSImage,WritePSImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Adobe Encapsulated PostScript");
      (void) RegisterMagickInfo("EPS2",ReadPSImage,WritePS2Image,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Adobe Level II Encapsulated PostScript");
      (void) RegisterMagickInfo("EPSF",ReadPSImage,WritePSImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Adobe Encapsulated PostScript");
      (void) RegisterMagickInfo("EPSI",ReadPSImage,WritePSImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Adobe Encapsulated PostScript Interchange format");
      (void) RegisterMagickInfo("EPT",ReadPSImage,WriteEPTImage,IsEPT,False,
        False,"Adobe Encapsulated PostScript with TIFF preview");
      (void) RegisterMagickInfo("FAX",ReadFAXImage,WriteFAXImage,IsFAX,True,
        True,"Group 3 FAX");
      (void) RegisterMagickInfo("FITS",ReadFITSImage,WriteFITSImage,IsFITS,
        False,True,"Flexible Image Transport System");
#if defined(HasFPX)
      (void) RegisterMagickInfo("FPX",ReadFPXImage,WriteFPXImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"FlashPix Format");
#endif
      (void) RegisterMagickInfo("G3",ReadFAXImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Group 3 FAX");
      (void) RegisterMagickInfo("GIF",ReadGIFImage,WriteGIFImage,IsGIF,True,
        True,"CompuServe graphics interchange format");
      (void) RegisterMagickInfo("GIF87",ReadGIFImage,WriteGIFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"CompuServe graphics interchange format (version 87a)");
      (void) RegisterMagickInfo("GRADATION",ReadGRADATIONImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Gradual passing from one shade to another");
      (void) RegisterMagickInfo("GRANITE",ReadLOGOImage,WriteLOGOImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"Granite texture");
      (void) RegisterMagickInfo("GRAY",ReadGRAYImage,WriteGRAYImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Raw gray bytes");
      (void) RegisterMagickInfo("H",ReadLOGOImage,WriteLOGOImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"Internal format");
      (void) RegisterMagickInfo("HDF",ReadHDFImage,WriteHDFImage,IsHDF,True,
        False,"Hierarchical Data Format");
      (void) RegisterMagickInfo("HISTOGRAM",ReadHISTOGRAMImage,
        WriteHISTOGRAMImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Histogram of the image");
      (void) RegisterMagickInfo("HTM",(Image *(*)(const ImageInfo *)) NULL,
        WriteHTMLImage,IsHTML,False,True,
        "Hypertext Markup Language with a client-side image map");
      (void) RegisterMagickInfo("HTML",(Image *(*)(const ImageInfo *)) NULL,
        WriteHTMLImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"Hypertext Markup Language with a client-side image map");
      (void) RegisterMagickInfo("ICB",ReadTGAImage,WriteTGAImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Truevision Targa image");
      (void) RegisterMagickInfo("ICC",ReadICCImage,WriteICCImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"ICC Color Profile");
      (void) RegisterMagickInfo("ICO",ReadICONImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Microsoft icon");
      (void) RegisterMagickInfo("IMPLICIT",(Image *(*)(const ImageInfo *)) NULL,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Internal format");
      (void) RegisterMagickInfo("IPTC",ReadIPTCImage,WriteIPTCImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"IPTC Newsphoto");
#if defined(HasJBIG)
      (void) RegisterMagickInfo("JBG",ReadJBIGImage,WriteJBIGImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,False,"Joint Bi-level Image experts Group interchange format");
      (void) RegisterMagickInfo("JBIG",ReadJBIGImage,WriteJBIGImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
         True,False,"Joint Bi-level Image experts Group interchange format");
#endif
#if defined(HasJPEG)
      (void) RegisterMagickInfo("JPG",ReadJPEGImage,WriteJPEGImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Joint Photographic Experts Group JFIF format");
      (void) RegisterMagickInfo("JPEG",ReadJPEGImage,WriteJPEGImage,IsJPEG,
        False,True,"Joint Photographic Experts Group JFIF format");
      (void) RegisterMagickInfo("JPEG24",ReadJPEGImage,WriteJPEGImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Joint Photographic Experts Group JFIF format");
#endif
      (void) RegisterMagickInfo("LABEL",ReadLABELImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Text image format");
      (void) RegisterMagickInfo("LOGO",ReadLOGOImage,WriteLOGOImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"ImageMagick Logo");
      (void) RegisterMagickInfo("MAP",ReadMAPImage,WriteMAPImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Colormap intensities and indices");
      (void) RegisterMagickInfo("MATTE",ReadMIFFImage,WriteMATTEImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Matte format");
      (void) RegisterMagickInfo("MIFF",ReadMIFFImage,WriteMIFFImage,IsMIFF,True,
        True,"Magick image format");
#if defined(HasPNG)
      (void) RegisterMagickInfo("MNG",ReadPNGImage,WritePNGImage,IsMNG,True,
        True,"Multiple-image Network Graphics");
#endif
      (void) RegisterMagickInfo("MONO",ReadMONOImage,WriteMONOImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Bi-level bitmap in least-significant-byte first order");
      (void) RegisterMagickInfo("MTV",ReadMTVImage,WriteMTVImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"MTV Raytracing image format");
      (void) RegisterMagickInfo("NETSCAPE",ReadLOGOImage,WriteLOGOImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"Netscape 216 color cube");
      (void) RegisterMagickInfo("NULL",ReadNULLImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"NULL image");
      (void) RegisterMagickInfo("P7",ReadPNMImage,WritePNMImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Xv thumbnail format");
      (void) RegisterMagickInfo("PBM",ReadPNMImage,WritePNMImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Portable bitmap format (black and white)");
      (void) RegisterMagickInfo("PCD",ReadPCDImage,WritePCDImage,IsPCD,False,
        False,"Photo CD");
      (void) RegisterMagickInfo("PCDS",ReadPCDImage,WritePCDImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"Photo CD");
      (void) RegisterMagickInfo("PCL",ReadPCLImage,WritePCLImage,IsPCL,False,
        True,"Page Control Language");
      (void) RegisterMagickInfo("PCT",
        (Image *(*)(const ImageInfo *)) ReadPICTImage,WritePICTImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Apple Macintosh QuickDraw/PICT");
      (void) RegisterMagickInfo("PCX",ReadPCXImage,WritePCXImage,IsPCX,False,
        True,"ZSoft IBM PC Paintbrush");
      (void) RegisterMagickInfo("PDF",ReadPDFImage,WritePDFImage,IsPDF,True,
        True,"Portable Document Format");
      (void) RegisterMagickInfo("PIC",ReadPICTImage,WritePICTImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Apple Macintosh QuickDraw/PICT");
      (void) RegisterMagickInfo("PICT",ReadPICTImage,WritePICTImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Apple Macintosh QuickDraw/PICT");
      (void) RegisterMagickInfo("PICT24",ReadPICTImage,WritePICTImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"24-bit Apple Macintosh QuickDraw/PICT");
      (void) RegisterMagickInfo("PIX",ReadPIXImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Alias/Wavefront RLE image format");
      (void) RegisterMagickInfo("PLASMA",ReadPLASMAImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Plasma fractal image");
      (void) RegisterMagickInfo("PGM",ReadPNMImage,WritePNMImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Portable graymap format (gray scale)");
      (void) RegisterMagickInfo("PM",ReadXPMImage,WriteXPMImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"X Windows system pixmap (color)");
#if defined(HasPNG)
      (void) RegisterMagickInfo("PNG",ReadPNGImage,WritePNGImage,IsPNG,False,
        True,"Portable Network Graphics");
#endif
      (void) RegisterMagickInfo("PNM",ReadPNMImage,WritePNMImage,IsPNM,True,
        True,"Portable anymap");
      (void) RegisterMagickInfo("PPM",ReadPNMImage,WritePNMImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Portable pixmap format (color)");
      (void) RegisterMagickInfo("PREVIEW",(Image *(*)(const ImageInfo *)) NULL,
        WritePREVIEWImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Show a preview an image enhancement, effect, or f/x");
      (void) RegisterMagickInfo("PS",ReadPSImage,WritePSImage,IsPS,True,True,
        "Adobe PostScript");
      (void) RegisterMagickInfo("PS2",(Image *(*)(const ImageInfo *)) NULL,
        WritePS2Image,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,True,
        True,"Adobe Level II PostScript");
      (void) RegisterMagickInfo("PS3",(Image *(*)(const ImageInfo *)) NULL,
        WritePS3Image,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Adobe Level III PostScript");
      (void) RegisterMagickInfo("PSD",ReadPSDImage,WritePSDImage,IsPSD,False,
        True,"Adobe Photoshop bitmap");
#if defined(HasTIFF)
      (void) RegisterMagickInfo("PTIF",ReadTIFFImage,WriteTIFFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,False,"Pyramid encoded TIFF");
#endif
      (void) RegisterMagickInfo("PWP",ReadPWPImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,IsPWP,True,True,
        "Seattle Film Works");
      (void) RegisterMagickInfo("RAS",ReadSUNImage,WriteSUNImage,IsSUN,True,
        True,"SUN Rasterfile");
      (void) RegisterMagickInfo("RGB",ReadRGBImage,WriteRGBImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Raw red, green, and blue bytes");
      (void) RegisterMagickInfo("RGBA",ReadRGBImage,WriteRGBImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Raw red, green, blue, and matte bytes");
      (void) RegisterMagickInfo("RLA",ReadRLAImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Alias/Wavefront image");
      (void) RegisterMagickInfo("RLE",ReadRLEImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,IsRLE,False,False,
        "Utah Run length encoded image");
      (void) RegisterMagickInfo("SCT",ReadSCTImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,IsSCT,False,True,
        "Scitex HandShake");
      (void) RegisterMagickInfo("SFW",ReadSFWImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,IsSFW,False,True,
        "Seattle Film Works");
      (void) RegisterMagickInfo("SGI",ReadSGIImage,WriteSGIImage,IsSGI,True,
        True,"Irix RGB image");
      (void) RegisterMagickInfo("SHTML",(Image *(*)(const ImageInfo *)) NULL,
        WriteHTMLImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,False,
        False,"Hypertext Markup Language with a client-side image map");
      (void) RegisterMagickInfo("STEGANO",ReadSTEGANOImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Steganographic image");
      (void) RegisterMagickInfo("SUN",ReadSUNImage,WriteSUNImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"SUN Rasterfile");
      (void) RegisterMagickInfo("TEXT",ReadTXTImage,WriteTXTImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Raw text");
      (void) RegisterMagickInfo("TGA",ReadTGAImage,WriteTGAImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Truevision Targa image");
#if defined(HasTIFF)
      (void) RegisterMagickInfo("TIF",ReadTIFFImage,WriteTIFFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,False,"Tagged Image File Format");
      (void) RegisterMagickInfo("TIFF",ReadTIFFImage,WriteTIFFImage,IsTIFF,
        True,False,"Tagged Image File Format");
      (void) RegisterMagickInfo("TIFF24",ReadTIFFImage,WriteTIFFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,False,"24-bit Tagged Image File Format");
#endif
      (void) RegisterMagickInfo("TILE",ReadTILEImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Tile image with a texture");
      (void) RegisterMagickInfo("TIM",ReadTIMImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"PSX TIM");
#if defined(HasTTF)
      (void) RegisterMagickInfo("TTF",ReadTTFImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"TrueType font");
#endif
      (void) RegisterMagickInfo("TXT",ReadTXTImage,WriteTXTImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Raw text");
      (void) RegisterMagickInfo("UIL",ReadUILImage,WriteUILImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"X-Motif UIL table");
      (void) RegisterMagickInfo("UYVY",ReadUYVYImage,WriteUYVYImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"16bit/pixel interleaved YUV");
      (void) RegisterMagickInfo("VDA",ReadTGAImage,WriteTGAImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Truevision Targa image");
      (void) RegisterMagickInfo("VICAR",ReadVICARImage,WriteVICARImage,IsVICAR,
        False,True,"VICAR rasterfile format");
      (void) RegisterMagickInfo("VID",ReadVIDImage,WriteMIFFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Visual Image Directory");
      (void) RegisterMagickInfo("VIFF",ReadVIFFImage,WriteVIFFImage,IsVIFF,True,
        True,"Khoros Visualization image");
      (void) RegisterMagickInfo("VST",ReadTGAImage,WriteTGAImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Truevision Targa image");
#if defined(HasX11)
      (void) RegisterMagickInfo("X",ReadXImage,WriteXImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"X Image");
#endif
      (void) RegisterMagickInfo("XBM",ReadXBMImage,WriteXBMImage,IsXBM,False,
        True,"X Windows system bitmap (black and white)");
      (void) RegisterMagickInfo("XC",ReadXCImage,
        (unsigned int (*)(const ImageInfo *,Image *)) NULL,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,True,"Constant image of X server color");
      (void) RegisterMagickInfo("XPM",ReadXPMImage,WriteXPMImage,IsXPM,False,
        True,"X Windows system pixmap (color)");
      (void) RegisterMagickInfo("XV",ReadVIFFImage,WriteVIFFImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        True,True,"Khoros Visualization image");
#if defined(HasX11)
      (void) RegisterMagickInfo("XWD",ReadXWDImage,WriteXWDImage,IsXWD,False,
        True,"X Windows system window dump (color)");
#endif
      (void) RegisterMagickInfo("YUV",ReadYUVImage,WriteYUVImage,
        (unsigned int (*)(const unsigned char *,const unsigned int)) NULL,
        False,False,"CCIR 601 4:1:1");
    }
  if (tag == (char *) NULL)
    return(magick_info);
  for (p=magick_info; p != (MagickInfo *) NULL; p=p->next)
    if (Latin1Compare(tag,p->tag) == 0)
      return(p);
  return((MagickInfo *) NULL);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%  L i s t M a g i c k I n f o                                                %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method ListMagickInfo lists the image formats to a file.
%
%  The format of the ListMagickInfo method is:
%
%      void ListMagickInfo(FILE *file)
%
%  A description of each parameter follows.
%
%    o file: A pointer to a FILE structure.
%
%
*/
Export void ListMagickInfo(FILE *file)
{
  register MagickInfo
    *p;

  if (file == (FILE *) NULL)
    file=stdout;
  (void) fprintf(file,"\nHere is a list of image formats recognized by "
    "ImageMagick.  Mode 'rw+'\nmeans ImageMagick can read, write, and "
    "save more than one image of a\nsequence to the same blob or file.\n\n");
  (void) fprintf(file,"    Format  Mode  Description\n");
  (void) fprintf(file,"--------------------------------------------------------"
    "-----------------\n");
  for (p=GetMagickInfo((char *) NULL); p != (MagickInfo *) NULL; p=p->next)
    (void) fprintf(file,"%10s%c  %c%c%c   %s\n",p->tag ? p->tag : "",
      p->blob_support ? '*' : ' ',p->decoder ? 'r' : '-',p->encoder ? 'w' : '-',
      p->encoder && p->adjoin ? '+' : '-',p->description ? p->description : "");
  (void) fprintf(file,"\n* native blob support\n\n");
  (void) fflush(file);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e g i s t e r M a g i c k I n f o                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method RegisterMagickInfo adds attributes for a particular image format to
%  the list of supported formats.  The attributes include the image format tag,
%  a method to read and/or write the format, whether the format supports
%  the saving of more than one frame to the same file or blob, whether the
%  format supports native in-memory I/O, and a brief description of the format.
%
%  The format of the RegisterMagickInfo method is:
%
%      MagickInfo *RegisterMagickInfo(const char *tag,
%        Image *(*decoder)(const ImageInfo *),
%        unsigned int (*encoder)(const ImageInfo *,Image *),
%        unsigned int (*magick)(const unsigned char *,const unsigned int),
%        const unsigned int adjoin,const unsigned int blob_support,
%        const char *description)
%
%  A description of each parameter follows:
%
%    o magick_info: Method RegisterMagickInfo returns a pointer MagickInfo
%      structure that contains the specified tag info.
%
%    o tag: a character string that represents the image format we are
%      looking for.
%
%    o decoder: a method that is used to read the image format.
%
%    o encoder: a method that is used to write the image format.
%
%    o magick: this method returns True if the image format signature
%      matches a subset of the first few bytes of the file or blob.
%
%    o adjoin: a value greater than 0 means the image format can save more
%      than one frame to the file or blob.
%
%    o blob_support: a value greater than 0 means the format supports
%      native in-memory I/O.
%
%    o description: a character string that describes the image format.
%
%
*/
Export MagickInfo *RegisterMagickInfo(const char *tag,
  Image *(*decoder)(const ImageInfo *),
  unsigned int (*encoder)(const ImageInfo *,Image *),
  unsigned int (*magick)(const unsigned char *,const unsigned int),
  const unsigned int adjoin,const unsigned int blob_support,
  const char *description)
{
  MagickInfo
    *entry;

  register MagickInfo
    *p;

  /*
    Add tag info to the end of the image format list.
  */
  entry=(MagickInfo *) AllocateMemory(sizeof(MagickInfo));
  if (entry == (MagickInfo *) NULL)
    MagickError(ResourceLimitWarning,"Unable to allocate image",
      "Memory allocation failed");
  entry->tag=AllocateString(tag);
  entry->decoder=decoder;
  entry->encoder=encoder;
  entry->magick=magick;
  entry->adjoin=adjoin;
  entry->blob_support=blob_support;
  entry->description=AllocateString(description);
  entry->data=(void *) NULL;
  entry->previous=(MagickInfo *) NULL;
  entry->next=(MagickInfo *) NULL;
  if (magick_info == (MagickInfo *) NULL)
    {
      magick_info=entry;
      return(entry);
    }
  for (p=magick_info; p->next != (MagickInfo *) NULL; p=p->next);
  p->next=entry;
  entry->previous=p;
  return(entry);
}
