/*
  ImageMagick defines.
*/
#ifndef _DEFINES_H
#define _DEFINES_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/*
  Review these platform specific definitions.
*/
#if defined(sgi)
#if !defined(RGBColorDatabase)
#define RGBColorDatabase  "/usr/lib/X11/rgb.txt"
#endif
#if !defined(ApplicationDefaults)
#define ApplicationDefaults  "/usr/lib/X11/app-defaults/"
#endif
#endif
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
#if !defined(ApplicationDefaults)
#define ApplicationDefaults  "/usr/X11R6/lib/X11/app-defaults/"
#endif
#if !defined(DelegatePath)
#define DelegatePath  "/usr/local/share/ImageMagick/"
#endif
#define DirectorySeparator  "/"
#define DirectoryListSeparator  ':'
#define EditorOptions  " -title \"Edit Image Comment\" -e vi"
#define Exit  exit
#define IsBasenameSeparator(c)  ((c) == '/')
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL) || \
   (strchr(text,'[') != (char *) NULL) || \
   (strchr(text,']') != (char *) NULL))
#define PreferencesDefaults  "~/."
#define ProcessPendingEvents(text)
#define ReadCommandlLine(argc,argv)
#if !defined(RGBColorDatabase)
#define RGBColorDatabase  "/usr/X11R6/lib/X11/rgb.txt"
#endif
#define SetNotifyHandlers
#define TemporaryTemplate  "magick"
#else /* specific platforms */
#if defined(vms)
#define ApplicationDefaults  "decw$system_defaults:"
#define DelegatePath  "sys$login:"
#define DirectorySeparator  ""
#define DirectoryListSeparator  ';'
#define EditorOptions  ""
#define Exit  exit
#define IsBasenameSeparator(c)  ((c) == ']')
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL))
#define j1(x)  x
#define PreferencesDefaults  "decw$user_defaults:"
#define ProcessPendingEvents(text)
#define ReadCommandlLine(argc,argv)
#if !defined(RGBColorDatabase)
#define RGBColorDatabase  "sys$common:[sysmgr]decw$rgb.dat"
#endif
#define SetNotifyHandlers
#endif /* vms */
#if defined(macintosh)
#define ApplicationDefaults  "/usr/lib/X11/app-defaults/"
#define DelegatePath  ""
#define DirectorySeparator  ":"
#define DirectoryListSeparator  ';'
#define EditorOptions ""
#define IsBasenameSeparator(c)  ((c) == ':')
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL) || \
   (strchr(text,'[') != (char *) NULL) || \
   (strchr(text,']') != (char *) NULL))
#define j1(x)  x
#define PreferencesDefaults  "~/."
#define ReadCommandlLine(argc,argv)  argc=ccommand(argv); puts(MagickVersion);
#if !defined(RGBColorDatabase)
#define RGBColorDatabase  "../xlib/lib/X11/rgb.txt"
#endif
#define SetNotifyHandlers \
  SetErrorHandler(MACErrorHandler); \
  SetWarningHandler(MACWarningHandler)
#endif /* macintosh */
#if defined(WIN32)
#define ApplicationDefaults  "c:\\ImageMagick\\"
#define DelegatePath  "c:\\ImageMagick\\"
#define DirectorySeparator  "\\"
#define DirectoryListSeparator  ';'
#define EditorOptions ""
#undef isatty
#define isatty(filedes)  1
#define IsBasenameSeparator(c)  (((c) == '/') || ((c) == '\\'))
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL) || \
   (strchr(text,'[') != (char *) NULL) || \
   (strchr(text,']') != (char *) NULL))
#define j1(x)  x
#define PreferencesDefaults  "~/."
#define ProcessPendingEvents(text)
#define ReadCommandlLine(argc,argv)
#if !defined(RGBColorDatabase)
#define RGBColorDatabase  "c:\\ImageMagick\\rgb.txt"
#endif
#define SetNotifyHandlers \
  SetErrorHandler(NTErrorHandler); \
  SetWarningHandler(NTWarningHandler)
#undef sleep
#define sleep(seconds)  Sleep(seconds*1000)
#endif /* WIN32 */
#endif /* end of platform specific definitions */

/*
  Define declarations.
*/
#define AbsoluteValue(x)  ((x) < 0 ? -(x) : (x))
#define CloseImage(image)  CloseBlob(image)
#define ColorMatch(color,target,distance) \
  (((distance) == 0) ? \
   (((color).red == (target).red) && \
    ((color).green == (target).green) && \
    ((color).blue == (target).blue)) : \
   ((unsigned long) ((((int) (color).red-(int) (target).red)* \
      ((int) (color).red-(int) (target).red))+ \
     (((int) (color).green-(int) (target).green)* \
      ((int) (color).green-(int) (target).green))+ \
     (((int) (color).blue-(int) (target).blue)* \
      ((int) (color).blue-(int) (target).blue))) <= \
      (unsigned long) (distance*distance)))
#define DownShift(x) (((unsigned long) ((x)+(1L << 13))) >> 14)
#define Extent(string)  ((int) strlen(string))
#define False  0
#define DegreesToRadians(x) ((x)*M_PI/180.0)
#define Intensity(color)  \
  ((unsigned long) ((color).red*77+(color).green*150+(color).blue*29) >> 8)
#define IsFaxImage(color)  \
  (IsMonochromeImage(image) && ((image)->columns <= 2560))
#define IsGray(color)  \
  (((color).red == (color).green) && ((color).green == (color).blue))
#define Max(x,y)  (((x) > (y)) ? (x) : (y))
#define Min(x,y)  (((x) < (y)) ? (x) : (y))
#if !defined(M_PI)
#define M_PI  3.14159265358979323846
#endif
#define Opaque  MaxRGB
#define OpenImage(image_info,image,type)  OpenBlob(image,image,type)
#define PixelOffset(image,x,y) \
  ((image)->pixels+(((int) (y))*(image)->columns+((int) (x))))
#define QuantumTick(i,span) \
  (((~((span)-i-1) & ((span)-i-2))+1) == ((span)-i-1))
#define RadiansToDegrees(x) ((x)*180/M_PI)
#define ReaderExit(warning,message,image) \
{ \
  MagickWarning(warning,message,image->filename); \
  DestroyImages(image); \
  return((Image *) NULL); \
}
#define ReadQuantum(quantum,p)  \
{  \
  if (image->depth <= 8) \
    quantum=UpScale(*p++); \
  else \
    { \
      value=(*p++) << 8;  \
      value|=(*p++);  \
      quantum=value >> (image->depth-QuantumDepth); \
    } \
}
#define ReadQuantumFile(quantum)  \
{  \
  if (image->depth <= 8) \
    quantum=UpScale(ReadByte(image)); \
  else \
    quantum=MSBFirstReadShort(image) >> (image->depth-QuantumDepth); \
}
#define RenderPostscriptText  "  Rendering postscript...  "
#define Swap(x,y) ((x)^=(y), (y)^=(x), (x)^=(y))
#if !defined(STDIN_FILENO)
#define STDIN_FILENO  0
#endif
#define Transparent  0
#define True  1
#define UpShift(x) ((int) (x) << 14)
#define UpShifted(x) ((int) ((x)*(1L << 14)+0.5))
#define WriteQuantum(quantum,q)  \
{  \
  if (image->depth <= 8) \
    *q++=DownScale(quantum); \
  else \
    { \
      value=(quantum); \
      if ((QuantumDepth-image->depth) > 0) \
        value*=257; \
      *q++=value >> 8; \
      *q++=value; \
    } \
}
#define WriteQuantumFile(quantum)  \
{  \
  if (image->depth <= 8) \
    (void) WriteByte(image,DownScale(quantum)); \
  else \
    if ((QuantumDepth-image->depth) > 0) \
      MSBFirstWriteShort(image,(quantum)*257); \
    else \
      MSBFirstWriteShort(image,quantum); \
}
#define WriterExit(error,message,image) \
{ \
  MagickWarning(error,message,image->filename); \
  if (image_info->adjoin) \
    while (image->previous != (Image *) NULL) \
      image=image->previous; \
  CloseBlob(image); \
  return(False); \
}

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
