/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%              AAA   N   N  IIIII  M   M   AAA   TTTTT  EEEEE                 %
%             A   A  NN  N    I    MM MM  A   A    T    E                     %
%             AAAAA  N N N    I    M M M  AAAAA    T    EEE                   %
%             A   A  N  NN    I    M   M  A   A    T    E                     %
%             A   A  N   N  IIIII  M   M  A   A    T    EEEEE                 %
%                                                                             %
%                                                                             %
%            Methods to Interactively Animate an Image Sequence               %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              July 1992                                      %
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

#if defined(HasX11)
#include "xwindows.h"
#include "animate.h"
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X M a g i c k C o m m a n d                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XMagickCommand makes a transform to the image or Image window as
%  specified by a user menu button or keyboard command.
%
%  The format of the XMagickCommand method is:
%
%      void XAnimateBackgroundImage(Display *display,
%        XResourceInfo *resource_info, Image *image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  XMagickCommand
%      may transform the image and return a new image pointer.
%
%    o state: Specifies an unsigned int;  XMagickCommand may return a
%      modified state.
%
%
*/
static Image *XMagickCommand(Display *display,XResourceInfo *resource_info,
  XWindows *windows,const CommandType command_type,Image **image,
  unsigned int *state)
{
#define LoadImageText  "  Loading images...  "

  Image
    *loaded_image;

  int
    status;

  XTextProperty
    window_name;

  /*
    Process user command.
  */
  loaded_image=(Image *) NULL;
  switch (command_type)
  {
    case OpenCommand:
    {
      char
        **filelist;

      Image
        *image,
        *next_image;

      ImageInfo
        *local_info;

      int
        number_files;

      MonitorHandler
        handler;

      register int
        i;

      static char
        filenames[MaxTextExtent] = "*";

      unsigned int
        status;

      if (resource_info->immutable)
        break;
      /*
        Request file name from user.
      */
      XFileBrowserWidget(display,windows,"Animate",filenames);
      if (*filenames == '\0')
        return((Image *) NULL);
      /*
        Expand the filenames.
      */
      filelist=(char **) AllocateMemory(sizeof(char *));
      if (filelist == (char **) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Memory allocation failed",
            (char *) NULL);
          return((Image *) NULL);
        }
      number_files=1;
      filelist[0]=filenames;
      status=ExpandFilenames(&number_files,&filelist);
      if ((status == False) || (number_files == 0))
        {
          MagickWarning(OptionWarning,"No image files were found",filenames);
          return((Image *) NULL);
        }
      local_info=CloneImageInfo(resource_info->image_info);
      if (local_info == (ImageInfo *) NULL)
        break;
      image=(Image *) NULL;
      handler=(MonitorHandler) NULL;
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      for (i=0; i < number_files; i++)
      {
        if (number_files > 5)
          handler=SetMonitorHandler((MonitorHandler) NULL);
        (void) strcpy(local_info->filename,filelist[i]);
        *local_info->magick='\0';
        next_image=ReadImage(local_info);
#if 0
        /* Something's wrong here.  No matching AllocMemory except for
           filelist[0] .. glennrp Jun 1999 */
        if (filelist[i] != filenames)
          FreeMemory((char *) filelist[i]);
#endif
        if (next_image != (Image *) NULL)
          {
            if (image == (Image *) NULL)
              image=next_image;
            else
              {
                image->next=next_image;
                image->next->previous=image;
                image=image->next;
              }
          }
        if (number_files <= 5)
          continue;
        (void) SetMonitorHandler(handler);
        ProgressMonitor(LoadImageText,i,number_files);
      }
#if 1
      FreeMemory(filelist[0]);
#endif
      DestroyImageInfo(local_info);
      if (image == (Image *) NULL)
        {
          XSetCursorState(display,windows,False);
          MagickWarning(OptionWarning,"No images were loaded",filenames);
          return((Image *) NULL);
        }
      while (image->previous != (Image *) NULL)
        image=image->previous;
      loaded_image=image;
      *state|=ExitState;
      break;
    }
    case PlayCommand:
    {
      *state|=PlayAnimationState;
      *state&=(~AutoReverseAnimationState);
      /*
        Window name is the base of the filename.
      */
      FormatString(windows->image.name,"ImageMagick: %.1024s",
        BaseFilename((*image)->filename));
      if (resource_info->title != (char *) NULL)
        (void) strcpy(windows->image.name,(*image)->label);
      status=XStringListToTextProperty(&windows->image.name,1,&window_name);
      if (status == 0)
        break;
      XSetWMName(display,windows->image.id,&window_name);
      XFree((void *) window_name.value);
      break;
    }
    case StepCommand:
    case StepBackwardCommand:
    case StepForwardCommand:
    {
      *state|=StepAnimationState;
      *state&=(~PlayAnimationState);
      if (command_type == StepBackwardCommand)
        *state&=(~ForwardAnimationState);
      if (command_type == StepForwardCommand)
        *state|=ForwardAnimationState;
      if (resource_info->title != (char *) NULL)
        break;
      break;
    }
    case RepeatCommand:
    {
      *state|=RepeatAnimationState;
      *state&=(~AutoReverseAnimationState);
      *state|=PlayAnimationState;
      break;
    }
    case AutoReverseCommand:
    {
      *state|=AutoReverseAnimationState;
      *state&=(~RepeatAnimationState);
      *state|=PlayAnimationState;
      break;
    }
    case SlowerCommand:
    {
      resource_info->delay<<=1;
      if (resource_info->delay == 0)
        resource_info->delay=1;
      break;
    }
    case FasterCommand:
    {
      resource_info->delay>>=1;
      break;
    }
    case ForwardCommand:
    {
      *state=ForwardAnimationState;
      *state&=(~AutoReverseAnimationState);
      break;
    }
    case ReverseCommand:
    {
      *state&=(~ForwardAnimationState);
      *state&=(~AutoReverseAnimationState);
      break;
    }
    case InfoCommand:
    {
      XDisplayImageInfo(display,resource_info,windows,(Image *) NULL,*image);
      break;
    }
    case HelpCommand:
    {
      /*
        User requested help.
      */
      XTextViewWidget(display,resource_info,windows,False,
        "Help Viewer - Animate",AnimateHelp);
      break;
    }
    case BrowseDocumentationCommand:
    {
      Atom
        mozilla_atom;

      Window
        mozilla_window,
        root_window;

      /*
        Browse the ImageMagick documentation.
      */
      root_window=XRootWindow(display,XDefaultScreen(display));
      mozilla_atom=XInternAtom(display,"_MOZILLA_VERSION",False);
      mozilla_window=XWindowByProperty(display,root_window,mozilla_atom);
      if (mozilla_window != (Window) NULL)
        {
          char
            command[MaxTextExtent];

          /*
            Display documentation using Netscape remote control.
          */
          FormatString(command,"openURL(%.1024s,new-window,noraise)",
            "http://www.wizards.dupont.com/cristy/ImageMagick.html");
          mozilla_atom=XInternAtom(display,"_MOZILLA_COMMAND",False);
          XChangeProperty(display,mozilla_window,mozilla_atom,XA_STRING,8,
            PropModeReplace,(unsigned char *) command,Extent(command));
          XSetCursorState(display,windows,False);
          break;
        }
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      status=InvokeDelegate(resource_info->image_info,*image,"browse",
        (char *) NULL);
      if (status != False)
        XNoticeWidget(display,windows,"Unable to browse documentation",
          (char *) NULL);
      XDelay(display,1500);
      XSetCursorState(display,windows,False);
      break;
    }
    case VersionCommand:
    {
      XNoticeWidget(display,windows,MagickVersion,MagickCopyright);
      break;
    }
    case QuitCommand:
    {
      /*
        Exit program
      */
      if (!resource_info->confirm_exit)
        XClientMessage(display,windows->image.id,windows->im_protocols,
          windows->im_exit,CurrentTime);
      else
        {
          /*
            Confirm program exit.
          */
          status=XConfirmWidget(display,windows,"Do you really want to exit",
            resource_info->client_name);
          if (status > 0)
            XClientMessage(display,windows->image.id,windows->im_protocols,
              windows->im_exit,CurrentTime);
        }
      break;
    }
    default:
      break;
  }
  return(loaded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X A n i m a t e B a c k g r o u n d I m a g e                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XAnimateBackgroundImage animates an image sequence in the
%  background of a window.
%
%  The format of the XAnimateBackgroundImage method is:
%
%      XAnimateBackgroundImage(display,resource_info,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%
*/

static int SceneCompare(const void *x,const void *y)
{
  Image
    **image_1,
    **image_2;

  image_1=(Image **) x;
  image_2=(Image **) y;
  return((int) (*image_1)->scene-(int) (*image_2)->scene);
}

Export void XAnimateBackgroundImage(Display *display,
  XResourceInfo *resource_info, Image *image)
{
  char
    geometry[MaxTextExtent],
    visual_type[MaxTextExtent];

  static XPixelInfo
    pixel_info;

  static XStandardColormap
    *map_info;

  static XVisualInfo
    *visual_info = (XVisualInfo *) NULL;

  static XWindowInfo
    window_info;

  Image
    *displayed_image,
    **images;

  int
    i,
    scene;

  unsigned int
    height,
    number_scenes,
    status,
    width;

  Window
    root_window;

  XEvent
    event;

  XGCValues
    context_values;

  XPixelInfo
    scene_info;

  XResourceInfo
    resources;

  XWindowAttributes
    window_attributes;

  /*
    Determine target window.
  */
  resources=(*resource_info);
  window_info.id=(Window) NULL;
  root_window=XRootWindow(display,XDefaultScreen(display));
  if (Latin1Compare(resources.window_id,"root") == 0)
    window_info.id=root_window;
  else
    {
      if (isdigit((int) (*resources.window_id)))
        window_info.id=XWindowByID(display,root_window,
          (Window) strtol((char *) resources.window_id,(char **) NULL,0));
      if (window_info.id == (Window) NULL)
        window_info.id=
          XWindowByName(display,root_window,resources.window_id);
    }
  if (window_info.id == (Window) NULL)
    {
      MagickWarning(OptionWarning,"No window with specified id exists",
        resources.window_id);
      return;
    }
  /*
    Determine window visual id.
  */
  window_attributes.width=XDisplayWidth(display,XDefaultScreen(display));
  window_attributes.height=XDisplayHeight(display,XDefaultScreen(display));
  (void) strcpy(visual_type,"default");
  status=XGetWindowAttributes(display,window_info.id,&window_attributes);
  if (status != False)
    FormatString(visual_type,"0x%lx",
      XVisualIDFromVisual(window_attributes.visual));
  if (visual_info == (XVisualInfo *) NULL)
    {
      /*
        Allocate standard colormap.
      */
      map_info=XAllocStandardColormap();
      if (map_info == (XStandardColormap *) NULL)
        MagickError(XServerError,"Unable to create standard colormap",
          "Memory allocation failed");
      map_info->colormap=(Colormap) NULL;
      pixel_info.pixels=(unsigned long *) NULL;
      pixel_info.gamma_map=(XColor *) NULL;
      /*
        Initialize visual info.
      */
      resources.map_type=(char *) NULL;
      resources.visual_type=visual_type;
      visual_info=XBestVisualInfo(display,map_info,&resources);
      if (visual_info == (XVisualInfo *) NULL)
        MagickError(XServerError,"Unable to get visual",resources.visual_type);
      /*
        Initialize window info.
      */
      window_info.ximage=(XImage *) NULL;
      window_info.matte_image=(XImage *) NULL;
      window_info.pixmap=(Pixmap) NULL;
      window_info.matte_pixmap=(Pixmap) NULL;
    }
  /*
    Free previous root colors.
  */
  if (window_info.id == root_window)
    XDestroyWindowColors(display,root_window);
  if (image->next != (Image *)NULL)
    CoalesceImages(image);
  if (resources.map_type == (char *) NULL)
    if ((visual_info->class != TrueColor) &&
        (visual_info->class != DirectColor))
      {
        Image
          *next_image;

        unsigned int
          global_colormap;

        /*
          Determine if the sequence of images has the identical colormap.
        */
        global_colormap=True;
        next_image=image;
        for ( ; next_image != (Image *) NULL; next_image=next_image->next)
        {
          next_image->matte=False;
          if ((next_image->class == DirectClass) ||
              (next_image->colors != image->colors) ||
              ((int) next_image->colors > visual_info->colormap_size))
            {
              global_colormap=False;
              break;
            }
          for (i=0; i < (int) image->colors; i++)
            if (!ColorMatch(next_image->colormap[i],image->colormap[i],0))
              {
                global_colormap=False;
                break;
              }
        }
        if (!global_colormap)
          (void) MapImages(image,(Image *) NULL,
            resources.quantize_info->dither);
      }
  /*
    Sort images by increasing scene number.
  */
  images=ListToGroupImage(image,&number_scenes);
  if (images == (Image **) NULL)
    MagickError(ResourceLimitError,"Unable to animate images",
      "Memory allocation failed");
  for (scene=0; scene < (int) number_scenes; scene++)
    if (images[scene]->scene == 0)
      break;
  if (scene == (int) number_scenes)
    qsort((void *) images,number_scenes,sizeof(Image *),
      (int (*)(const void *, const void *)) SceneCompare);
  /*
    Initialize Standard Colormap.
  */
  resources.colormap=SharedColormap;
  displayed_image=images[0];
  for (scene=0; scene < (int) number_scenes; scene++)
  {
    if ((resource_info->map_type != (char *) NULL) ||
        (visual_info->class == TrueColor) ||
        (visual_info->class == DirectColor))
      images[scene]->class=DirectClass;
    if ((displayed_image->columns < images[scene]->columns) &&
        (displayed_image->rows < images[scene]->rows))
      displayed_image=images[scene];
  }
  if ((resource_info->map_type != (char *) NULL) ||
      (visual_info->class == TrueColor) ||
      (visual_info->class == DirectColor))
    displayed_image->class=DirectClass;
  XMakeStandardColormap(display,visual_info,&resources,displayed_image,map_info,
    &pixel_info);
  /*
    Graphic context superclass.
  */
  context_values.background=pixel_info.background_color.pixel;
  context_values.foreground=pixel_info.foreground_color.pixel;
  pixel_info.annotate_context=XCreateGC(display,window_info.id,GCBackground |
    GCForeground,&context_values);
  if (pixel_info.annotate_context == (GC) NULL)
    MagickError(XServerError,"Unable to create graphic context",(char *) NULL);
  /*
    Initialize Image window attributes.
  */
  XGetWindowInfo(display,visual_info,map_info,&pixel_info,(XFontStruct *) NULL,
    &resources,&window_info);
  /*
    Create the X image.
  */
  window_info.width=images[0]->columns;
  window_info.height=images[0]->rows;
  FormatString(geometry,"%ux%u+0+0>",window_attributes.width,
    window_attributes.height);
  (void) ParseImageGeometry(geometry,&window_info.x,&window_info.y,
    &window_info.width,&window_info.height);
  status=XMakeImage(display,&resources,&window_info,images[0],window_info.width,
    window_info.height);
  if (status == False)
    MagickError(XServerError,"Unable to create X image",(char *) NULL);
  window_info.x=0;
  window_info.y=0;
  if (resources.debug)
    {
      (void) fprintf(stderr,"Image: %.1024s[%u] %ux%u ",images[0]->filename,
        images[0]->scene,images[0]->columns,images[0]->rows);
      if (images[0]->colors != 0)
        (void) fprintf(stderr,"%uc ",images[0]->colors);
      (void) fprintf(stderr,"%.1024s\n",images[0]->magick);
    }
  /*
    Adjust image dimensions as specified by backdrop or geometry options.
  */
  width=window_info.width;
  height=window_info.height;
  if (resources.backdrop)
    {
      /*
        Center image on window.
      */
      window_info.x=(window_attributes.width >> 1)-
        (window_info.ximage->width >> 1);
      window_info.y=(window_attributes.height >> 1)-
        (window_info.ximage->height >> 1);
      width=window_attributes.width;
      height=window_attributes.height;
    }
  if (resources.image_geometry != (char *) NULL)
    {
      char
        default_geometry[MaxTextExtent];

      int
        flags,
        gravity;

      XSizeHints
        *size_hints;

      /*
        User specified geometry.
      */
      size_hints=XAllocSizeHints();
      if (size_hints == (XSizeHints *) NULL)
        MagickError(ResourceLimitError,"Unable to display on window",
          "Memory allocation failed");
      size_hints->flags=(long) NULL;
      FormatString(default_geometry,"%ux%u",width,height);
      flags=XWMGeometry(display,visual_info->screen,resources.image_geometry,
        default_geometry,window_info.border_width,size_hints,&window_info.x,
        &window_info.y,(int *) &width,(int *) &height,&gravity);
      if (flags & (XValue | YValue))
        {
          width=window_attributes.width;
          height=window_attributes.height;
        }
      XFree((void *) size_hints);
    }
  /*
    Create the X pixmap.
  */
  window_info.pixmap=
    XCreatePixmap(display,window_info.id,width,height,window_info.depth);
  if (window_info.pixmap == (Pixmap) NULL)
    MagickError(XServerError,"Unable to create X pixmap",(char *) NULL);
  /*
    Display pixmap on the window.
  */
  if ((width > window_info.width) || (height > window_info.height))
    XFillRectangle(display,window_info.pixmap,window_info.annotate_context,
      0,0,width,height);
  XPutImage(display,window_info.pixmap,window_info.annotate_context,
    window_info.ximage,0,0,window_info.x,window_info.y,window_info.width,
    window_info.height);
  XSetWindowBackgroundPixmap(display,window_info.id,window_info.pixmap);
  XClearWindow(display,window_info.id);
  /*
    Initialize image pixmaps structure.
  */
  window_info.pixmaps=(Pixmap *) AllocateMemory(number_scenes*sizeof(Pixmap));
  window_info.matte_pixmaps=(Pixmap *)
    AllocateMemory(number_scenes*sizeof(Pixmap));
  if ((window_info.pixmaps == (Pixmap *) NULL) ||
      (window_info.matte_pixmaps == (Pixmap *) NULL))
    MagickError(ResourceLimitError,"Unable to animate images",
      "Memory allocation failed");
  window_info.pixmaps[0]=window_info.pixmap;
  window_info.matte_pixmaps[0]=window_info.pixmap;
  scene_info.pixels=(unsigned long *) NULL;
  scene_info.gamma_map=(XColor *) NULL;
  for (scene=1; scene < (int) number_scenes; scene++)
  {
    /*
      Create X image.
    */
    window_info.pixmap=(Pixmap) NULL;
    window_info.matte_pixmap=(Pixmap) NULL;
    if ((resources.map_type != (char *) NULL) ||
        (visual_info->class == TrueColor) ||
        (visual_info->class == DirectColor))
      if (images[scene]->class == PseudoClass)
        {
          /*
            Get pixel info for this scene.
          */
          XGetPixelInfo(display,visual_info,map_info,&resources,images[scene],
            &scene_info);
          window_info.pixel_info=(&scene_info);
        }
    status=XMakeImage(display,&resources,&window_info,images[scene],
      images[scene]->columns,images[scene]->rows);
    if (status == False)
      MagickError(XServerError,"Unable to create X image",(char *) NULL);
    if (resources.debug)
      {
        (void) fprintf(stderr,"Image: [%u] %.1024s %ux%u ",images[scene]->scene,
          images[scene]->filename,images[scene]->columns,images[scene]->rows);
        if (images[scene]->colors != 0)
          (void) fprintf(stderr,"%uc ",images[scene]->colors);
        (void) fprintf(stderr,"%.1024s\n",images[scene]->magick);
      }
    /*
      Create the X pixmap.
    */
    window_info.pixmap=
      XCreatePixmap(display,window_info.id,width,height,window_info.depth);
    if (window_info.pixmap == (Pixmap) NULL)
      MagickError(XServerError,"Unable to create X pixmap",(char *) NULL);
    /*
      Display pixmap on the window.
    */
    if ((width > window_info.width) || (height > window_info.height))
      XFillRectangle(display,window_info.pixmap,window_info.annotate_context,
        0,0,width,height);
    XPutImage(display,window_info.pixmap,window_info.annotate_context,
      window_info.ximage,0,0,window_info.x,window_info.y,window_info.width,
      window_info.height);
    XSetWindowBackgroundPixmap(display,window_info.id,window_info.pixmap);
    XClearWindow(display,window_info.id);
    window_info.pixmaps[scene]=window_info.pixmap;
    window_info.matte_pixmaps[scene]=window_info.matte_pixmap;
    if (images[scene]->matte)
      XClearWindow(display,window_info.id);
    if (resources.delay != 0)
      XDelay(display,(unsigned long) resources.delay*10);
    else
      XDelay(display,(unsigned long) image->delay*10);
  }
  window_info.pixel_info=(&pixel_info);
  /*
    Display pixmap on the window.
  */
  XSelectInput(display,window_info.id,SubstructureNotifyMask);
  event.type=Expose;
  do
  {
    for (scene=0; scene < (int) number_scenes; scene++)
    {
      if (XEventsQueued(display,QueuedAfterFlush) > 0)
        {
          XNextEvent(display,&event);
          if (event.type == DestroyNotify)
            break;
        }
      window_info.pixmap=window_info.pixmaps[scene];
      window_info.matte_pixmap=window_info.matte_pixmaps[scene];
      XSetWindowBackgroundPixmap(display,window_info.id,window_info.pixmap);
      XClearWindow(display,window_info.id);
      XSync(display,False);
      if (resources.delay != 0)
        XDelay(display,(unsigned long) resources.delay*10);
      else
        XDelay(display,(unsigned long) images[scene]->delay*10);
    }
  } while (event.type != DestroyNotify);
  XSync(display,False);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X A n i m a t e I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XAnimateImages displays an image via X11.
%
%  The format of the XAnimateImages method is:
%
%      Image *XAnimateImages(Display *display,XResourceInfo *resource_info,
%        char **argv,const int argc,Image *image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o argv: Specifies the application's argument list.
%
%    o argc: Specifies the number of arguments.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%
*/
Export Image *XAnimateImages(Display *display,XResourceInfo *resource_info,
  char **argv,const int argc,Image *image)
{
#define MagickMenus  4
#define MaxWindows  8
#define MagickTitle  "Commands"

  static const char
    *CommandMenu[]=
    {
      "Animate",
      "Speed",
      "Direction",
      "Help",
      "Image Info",
      "Quit",
      (char *) NULL
    },
    *AnimateMenu[]=
    {
      "Open",
      "Play",
      "Step",
      "Repeat",
      "Auto Reverse",
      (char *) NULL
    },
    *SpeedMenu[]=
    {
      "Faster",
      "Slower",
      (char *) NULL
    },
    *DirectionMenu[]=
    {
      "Forward",
      "Reverse",
      (char *) NULL
    },
    *HelpMenu[]=
    {
      "Overview",
      "Browse Documentation",
      "About Animate",
      (char *) NULL
    };


  static const char
    **Menus[MagickMenus]=
    {
      AnimateMenu,
      SpeedMenu,
      DirectionMenu,
      HelpMenu
    };

  static const CommandType
    CommandMenus[]=
    {
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      InfoCommand,
      QuitCommand
    },
    CommandTypes[]=
    {
      OpenCommand,
      PlayCommand,
      StepCommand,
      RepeatCommand,
      AutoReverseCommand
    },
    SpeedCommands[]=
    {
      FasterCommand,
      SlowerCommand
    },
    DirectionCommands[]=
    {
      ForwardCommand,
      ReverseCommand
    },
    HelpCommands[]=
    {
      HelpCommand,
      BrowseDocumentationCommand,
      VersionCommand
    };

  static const CommandType
    *Commands[MagickMenus]=
    {
      CommandTypes,
      SpeedCommands,
      DirectionCommands,
      HelpCommands
    };

  char
    command[MaxTextExtent],
    resource_name[MaxTextExtent];

  CommandType
    command_type;

  ErrorHandler
    warning_handler;

  Image
    *displayed_image,
    **images,
    *loaded_image;

  int
    first_scene,
    scene,
    status;

  KeySym
    key_symbol;

  MonitorHandler
    monitor_handler;

  register char
    *p;

  register int
    i;

  static char
    working_directory[MaxTextExtent];

  static unsigned int
    number_windows;

  static XWindowInfo
    *magick_windows[MaxWindows];

  time_t
    timestamp;

  unsigned int
    context_mask,
    number_scenes,
    state;

  Window
    root_window;

  XClassHint
    *class_hints;

  XEvent
    event;

  XFontStruct
    *font_info;

  XGCValues
    context_values;

  XPixelInfo
    *icon_pixel,
    *pixel_info,
    scene_info;

  XResourceInfo
    *icon_resources;

  XStandardColormap
    *icon_map,
    *map_info;

  XTextProperty
    window_name;

  XVisualInfo
    *icon_visual,
    *visual_info;

  XWindows
    *windows;

  XWMHints
    *manager_hints;

  monitor_handler=(MonitorHandler) NULL;
  warning_handler=(ErrorHandler) NULL;
  windows=XSetWindows((XWindows *) ~0);
  if (windows != (XWindows *) NULL)
    {
      (void) chdir(working_directory);
      monitor_handler=SetMonitorHandler(XProgressMonitor);
      warning_handler=resource_info->display_warnings ?
        SetWarningHandler(XWarning) : SetWarningHandler((ErrorHandler) NULL);
      (void) signal(SIGINT,XSignalHandler);
      (void) signal(SIGSEGV,XSignalHandler);
    }
  else
    {
      register Image
        *p;

      /*
        Initialize window structure.
      */
      for (p=image; p != (Image *) NULL; p=p->next)
      {
        if (p->class == DirectClass)
          {
            resource_info->colors=0;
            break;
          }
        if (p->colors > resource_info->colors)
          resource_info->colors=p->colors;
      }
      windows=XSetWindows(XInitializeWindows(display,resource_info));
      if (windows == (XWindows *) NULL)
        MagickError(XServerError,"Unable to create X windows",
          "Memory allocation failed");
      /*
        Initialize window id's.
      */
      number_windows=0;
      magick_windows[number_windows++]=(&windows->icon);
      magick_windows[number_windows++]=(&windows->backdrop);
      magick_windows[number_windows++]=(&windows->image);
      magick_windows[number_windows++]=(&windows->info);
      magick_windows[number_windows++]=(&windows->command);
      magick_windows[number_windows++]=(&windows->widget);
      magick_windows[number_windows++]=(&windows->popup);
      for (i=0; i < (int) number_windows; i++)
        magick_windows[i]->id=(Window) NULL;
    }
  /*
    Initialize font info.
  */
  if (windows->font_info != (XFontStruct *) NULL)
    XFreeFont(display,windows->font_info);
  windows->font_info=XBestFont(display,resource_info,False);
  if (windows->font_info == (XFontStruct *) NULL)
    MagickError(XServerError,"Unable to load font",resource_info->font);
  /*
    Initialize Standard Colormap.
  */
  map_info=windows->map_info;
  icon_map=windows->icon_map;
  visual_info=windows->visual_info;
  icon_visual=windows->icon_visual;
  pixel_info=windows->pixel_info;
  icon_pixel=windows->icon_pixel;
  font_info=windows->font_info;
  icon_resources=windows->icon_resources;
  class_hints=windows->class_hints;
  manager_hints=windows->manager_hints;
  root_window=XRootWindow(display,visual_info->screen);
  if (image->next != (Image *)NULL)
    CoalesceImages(image);
  if (resource_info->map_type == (char *) NULL)
    if ((visual_info->class != TrueColor) &&
        (visual_info->class != DirectColor))
      {
        Image
          *next_image;

        unsigned int
          global_colormap;

        /*
          Determine if the sequence of images has the identical colormap.
        */
        global_colormap=True;
        next_image=image;
        for ( ; next_image != (Image *) NULL; next_image=next_image->next)
        {
          TransformRGBImage(next_image,RGBColorspace);
          next_image->matte=False;
          if ((next_image->class == DirectClass) ||
              (next_image->colors != image->colors) ||
              ((int) next_image->colors > visual_info->colormap_size))
            {
              global_colormap=False;
              break;
            }
          for (i=0; i < (int) image->colors; i++)
            if (!ColorMatch(next_image->colormap[i],image->colormap[i],0))
              {
                global_colormap=False;
                break;
              }
        }
        if (!global_colormap)
          (void) MapImages(image,(Image *) NULL,
            resource_info->quantize_info->dither);
      }
  /*
    Sort images by increasing scene number.
  */
  images=ListToGroupImage(image,&number_scenes);
  if (images == (Image **) NULL)
    MagickError(ResourceLimitError,"Unable to animate images",
      "Memory allocation failed");
  for (scene=0; scene < (int) number_scenes; scene++)
    if (images[scene]->scene == 0)
      break;
  if (scene == (int) number_scenes)
    qsort((void *) images,number_scenes,sizeof(Image *),
      (int (*)(const void *, const void *)) SceneCompare);
  /*
    Initialize Standard Colormap.
  */
  loaded_image=(Image *) NULL;
  displayed_image=images[0];
  TransformRGBImage(displayed_image,RGBColorspace);
  for (scene=0; scene < (int) number_scenes; scene++)
  {
    if ((resource_info->map_type != (char *) NULL) ||
        (visual_info->class == TrueColor) ||
        (visual_info->class == DirectColor))
      images[scene]->class=DirectClass;
    if ((displayed_image->columns < images[scene]->columns) &&
        (displayed_image->rows < images[scene]->rows))
      displayed_image=images[scene];
  }
  if (resource_info->debug)
    {
      (void) fprintf(stderr,"Image: %.1024s[%u] %ux%u ",
        displayed_image->filename,displayed_image->scene,
        displayed_image->columns,displayed_image->rows);
      if (displayed_image->colors != 0)
        (void) fprintf(stderr,"%uc ",displayed_image->colors);
      (void) fprintf(stderr,"%.1024s\n",displayed_image->magick);
    }
  XMakeStandardColormap(display,visual_info,resource_info,displayed_image,
    map_info,pixel_info);
  /*
    Initialize graphic context.
  */
  windows->context.id=(Window) NULL;
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->context);
  class_hints->res_name="superclass";
  class_hints->res_class="Display";
  manager_hints->flags=InputHint | StateHint;
  manager_hints->input=False;
  manager_hints->initial_state=WithdrawnState;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->context);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (context)\n",windows->context.id);
  context_values.background=pixel_info->background_color.pixel;
  context_values.font=font_info->fid;
  context_values.foreground=pixel_info->foreground_color.pixel;
  context_values.graphics_exposures=False;
  context_mask=GCBackground | GCFont | GCForeground | GCGraphicsExposures;
  if (pixel_info->annotate_context != (GC) NULL)
    XFreeGC(display,pixel_info->annotate_context);
  pixel_info->annotate_context=
    XCreateGC(display,windows->context.id,context_mask,&context_values);
  if (pixel_info->annotate_context == (GC) NULL)
    MagickError(XServerError,"Unable to create graphic context",(char *) NULL);
  context_values.background=pixel_info->depth_color.pixel;
  if (pixel_info->widget_context != (GC) NULL)
    XFreeGC(display,pixel_info->widget_context);
  pixel_info->widget_context=
    XCreateGC(display,windows->context.id,context_mask,&context_values);
  if (pixel_info->widget_context == (GC) NULL)
    MagickError(XServerError,"Unable to create graphic context",(char *) NULL);
  context_values.background=pixel_info->foreground_color.pixel;
  context_values.foreground=pixel_info->background_color.pixel;
  context_values.plane_mask=
    context_values.background ^ context_values.foreground;
  if (pixel_info->highlight_context != (GC) NULL)
    XFreeGC(display,pixel_info->highlight_context);
  pixel_info->highlight_context=XCreateGC(display,windows->context.id,
    context_mask | GCPlaneMask,&context_values);
  if (pixel_info->highlight_context == (GC) NULL)
    MagickError(XServerError,"Unable to create graphic context",(char *) NULL);
  XDestroyWindow(display,windows->context.id);
  /*
    Initialize icon window.
  */
  XGetWindowInfo(display,icon_visual,icon_map,icon_pixel,(XFontStruct *) NULL,
    icon_resources,&windows->icon);
  windows->icon.geometry=resource_info->icon_geometry;
  XBestIconSize(display,&windows->icon,displayed_image);
  windows->icon.attributes.colormap=
    XDefaultColormap(display,icon_visual->screen);
  windows->icon.attributes.event_mask=ExposureMask | StructureNotifyMask;
  class_hints->res_name="icon";
  manager_hints->flags=InputHint | StateHint;
  manager_hints->input=False;
  manager_hints->initial_state=IconicState;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->icon);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (icon)\n",windows->icon.id);
  /*
    Initialize graphic context for icon window.
  */
  if (icon_pixel->annotate_context != (GC) NULL)
    XFreeGC(display,icon_pixel->annotate_context);
  context_values.background=icon_pixel->background_color.pixel;
  context_values.foreground=icon_pixel->foreground_color.pixel;
  icon_pixel->annotate_context=XCreateGC(display,windows->icon.id,
    GCBackground | GCForeground,&context_values);
  if (icon_pixel->annotate_context == (GC) NULL)
    MagickError(XServerError,"Unable to create graphic context",(char *) NULL);
  windows->icon.annotate_context=icon_pixel->annotate_context;
  /*
    Initialize Image window.
  */
  if (windows->image.id != (Window) NULL)
    {
      FreeMemory((char *) windows->image.name);
      FreeMemory((char *) windows->image.icon_name);
    }
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->image);
  windows->image.shape=True;  /* non-rectangular shape hint */
  windows->image.shared_memory=resource_info->use_shared_memory;
  windows->image.name=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
  windows->image.icon_name=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
  if ((windows->image.name == NULL) || (windows->image.icon_name == NULL))
    MagickError(ResourceLimitError,"Unable to create Image window",
      "Memory allocation failed");
  if (resource_info->title != (char *) NULL)
    {
      /*
        User specified window name.
      */
      LabelImage(displayed_image,resource_info->title);
      (void) strcpy(windows->image.name,displayed_image->label);
      (void) strcpy(windows->image.icon_name,displayed_image->label);
    }
  else
    {
      /*
        Window name is the base of the filename.
      */
      p=displayed_image->filename+Extent(displayed_image->filename)-1;
      while ((p > displayed_image->filename) && !IsBasenameSeparator(*(p-1)))
        p--;
      FormatString(windows->image.name,"ImageMagick: %.1024s[%u of %u]",p,
        displayed_image->scene,number_scenes);
      (void) strcpy(windows->image.icon_name,p);
    }
  if (resource_info->immutable)
    windows->image.immutable=True;
  windows->image.shape=True;
  windows->image.geometry=resource_info->image_geometry;
  windows->image.width=displayed_image->columns;
  if ((int) windows->image.width > XDisplayWidth(display,visual_info->screen))
    windows->image.width=XDisplayWidth(display,visual_info->screen);
  windows->image.height=displayed_image->rows;
  if ((int) windows->image.height > XDisplayHeight(display,visual_info->screen))
    windows->image.height=XDisplayHeight(display,visual_info->screen);
  windows->image.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | LeaveWindowMask | OwnerGrabButtonMask |
    PropertyChangeMask | StructureNotifyMask | SubstructureNotifyMask;
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->backdrop);
  if ((resource_info->backdrop) || (windows->backdrop.id != (Window) NULL))
    {
      /*
        Initialize backdrop window.
      */
      windows->backdrop.x=0;
      windows->backdrop.y=0;
      windows->backdrop.name="ImageMagick Backdrop";
      windows->backdrop.flags=USSize | USPosition;
      windows->backdrop.width=XDisplayWidth(display,visual_info->screen);
      windows->backdrop.height=XDisplayHeight(display,visual_info->screen);
      windows->backdrop.border_width=0;
      windows->backdrop.immutable=True;
      windows->backdrop.attributes.do_not_propagate_mask=ButtonPressMask |
        ButtonReleaseMask;
      windows->backdrop.attributes.event_mask=ButtonPressMask | KeyPressMask |
        StructureNotifyMask;
      windows->backdrop.attributes.override_redirect=True;
      class_hints->res_name="backdrop";
      manager_hints->flags=IconWindowHint | InputHint | StateHint;
      manager_hints->icon_window=windows->icon.id;
      manager_hints->input=True;
      manager_hints->initial_state=
        resource_info->iconic ? IconicState : NormalState;
      XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
        &windows->backdrop);
      if (resource_info->debug)
        (void) fprintf(stderr,"Window id: 0x%lx (backdrop)\n",
          windows->backdrop.id);
      XMapWindow(display,windows->backdrop.id);
      XClearWindow(display,windows->backdrop.id);
      if (windows->image.id != (Window) NULL)
        {
          XDestroyWindow(display,windows->image.id);
          windows->image.id=(Window) NULL;
        }
      /*
        Position image in the center the backdrop.
      */
      windows->image.flags|=USPosition;
      windows->image.x=(XDisplayWidth(display,visual_info->screen) >> 1)-
        (windows->image.width >> 1);
      windows->image.y=(XDisplayHeight(display,visual_info->screen) >> 1)-
        (windows->image.height >> 1);
    }
  if (resource_info->name == (char *) NULL)
    class_hints->res_name=resource_info->client_name;
  else
    class_hints->res_name=resource_info->name;
  manager_hints->flags=IconWindowHint | InputHint | StateHint;
  manager_hints->icon_window=windows->icon.id;
  manager_hints->input=True;
  manager_hints->initial_state=
    resource_info->iconic ? IconicState : NormalState;
  if (windows->group_leader.id != (Window) NULL)
    {
      /*
        Follow the leader.
      */
      manager_hints->flags|=WindowGroupHint;
      manager_hints->window_group=windows->group_leader.id;
      XSelectInput(display,windows->group_leader.id,StructureNotifyMask);
      if (resource_info->debug)
        (void) fprintf(stderr,"Window id: 0x%lx (group leader)\n",
          windows->group_leader.id);
    }
  XMakeWindow(display,
    (Window) (resource_info->backdrop ? windows->backdrop.id : root_window),
    argv,argc,class_hints,manager_hints,&windows->image);
  XChangeProperty(display,windows->image.id,windows->im_protocols,XA_STRING,8,
    PropModeReplace,(unsigned char *) NULL,0);
  if (windows->group_leader.id != (Window) NULL)
    XSetTransientForHint(display,windows->image.id,windows->group_leader.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (image)\n",windows->image.id);
  /*
    Initialize Info widget.
  */
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->info);
  windows->info.name="Info";
  windows->info.icon_name="Info";
  windows->info.border_width=1;
  windows->info.x=2;
  windows->info.y=2;
  windows->info.flags|=PPosition;
  windows->info.attributes.win_gravity=UnmapGravity;
  windows->info.attributes.event_mask=
    ButtonPressMask | ExposureMask | StructureNotifyMask;
  class_hints->res_name="info";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=False;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,windows->image.id,argv,argc,class_hints,manager_hints,
    &windows->info);
  windows->info.highlight_stipple=XCreateBitmapFromData(display,
    windows->info.id,(char *) HighlightBitmap,HighlightWidth,HighlightHeight);
  windows->info.shadow_stipple=XCreateBitmapFromData(display,
    windows->info.id,(char *) ShadowBitmap,ShadowWidth,ShadowHeight);
  XSetTransientForHint(display,windows->info.id,windows->image.id);
  if (windows->image.mapped)
    XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (info)\n",windows->info.id);
  /*
    Initialize Command widget.
  */
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->command);
  windows->command.data=MagickMenus;
  (void) XCommandWidget(display,windows,CommandMenu,(XEvent *) NULL);
  FormatString(resource_name,"%.1024s.command",resource_info->client_name);
  windows->command.geometry=XGetResourceClass(resource_info->resource_database,
    resource_name,"geometry",(char *) NULL);
  windows->command.name=MagickTitle;
  windows->command.border_width=0;
  windows->command.flags|=PPosition;
  windows->command.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | LeaveWindowMask |
    OwnerGrabButtonMask | StructureNotifyMask;
  class_hints->res_name="command";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=False;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->command);
  windows->command.highlight_stipple=windows->info.highlight_stipple;
  windows->command.shadow_stipple=windows->info.shadow_stipple;
  XSetTransientForHint(display,windows->command.id,windows->image.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (command)\n",windows->command.id);
  /*
    Initialize Widget window.
  */
  if (windows->widget.id != (Window) NULL)
    FreeMemory((char *) windows->widget.name);
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->widget);
  FormatString(resource_name,"%.1024s.widget",resource_info->client_name);
  windows->widget.geometry=XGetResourceClass(resource_info->resource_database,
    resource_name,"geometry",(char *) NULL);
  windows->widget.name=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
  if (windows->widget.name == NULL)
    MagickError(ResourceLimitError,"Unable to create Image window",
      "Memory allocation failed");
  *windows->widget.name='\0';
  windows->widget.border_width=0;
  windows->widget.flags|=PPosition;
  windows->widget.attributes.backing_store=WhenMapped;
  windows->widget.attributes.save_under=True;
  windows->widget.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | LeaveWindowMask | OwnerGrabButtonMask |
    StructureNotifyMask;
  class_hints->res_name="widget";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=True;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->widget);
  windows->widget.highlight_stipple=windows->info.highlight_stipple;
  windows->widget.shadow_stipple=windows->info.shadow_stipple;
  XSetTransientForHint(display,windows->widget.id,windows->image.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (widget)\n",windows->widget.id);
  /*
    Initialize popup window.
  */
  if (windows->popup.id != (Window) NULL)
    FreeMemory((char *) windows->popup.name);
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->popup);
  windows->popup.name=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
  if (windows->popup.name == NULL)
    MagickError(ResourceLimitError,"Unable to create Image window",
      "Memory allocation failed");
  *windows->popup.name='\0';
  windows->popup.border_width=0;
  windows->popup.flags|=PPosition;
  windows->popup.attributes.backing_store=WhenMapped;
  windows->popup.attributes.save_under=True;
  windows->popup.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | EnterWindowMask | ExposureMask | KeyPressMask |
    KeyReleaseMask | LeaveWindowMask | StructureNotifyMask;
  class_hints->res_name="popup";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=True;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->popup);
  windows->popup.highlight_stipple=windows->info.highlight_stipple;
  windows->popup.shadow_stipple=windows->info.shadow_stipple;
  XSetTransientForHint(display,windows->popup.id,windows->image.id);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (pop up)\n",windows->popup.id);
  if (!windows->image.mapped || (windows->backdrop.id != (Window) NULL))
    XMapWindow(display,windows->image.id);
  /*
    Set out progress and warning handlers.
  */
  if (monitor_handler == (MonitorHandler) NULL)
    monitor_handler=SetMonitorHandler(XProgressMonitor);
  if (warning_handler == (ErrorHandler) NULL)
    warning_handler=resource_info->display_warnings ?
      SetWarningHandler(XWarning) : SetWarningHandler((ErrorHandler) NULL);
  (void) signal(SIGINT,XSignalHandler);
  (void) signal(SIGSEGV,XSignalHandler);
  /*
    Initialize X image structure.
  */
  windows->image.x=0;
  windows->image.y=0;
  status=XMakeImage(display,resource_info,&windows->image,displayed_image,
    displayed_image->columns,displayed_image->rows);
  if (status == False)
    MagickError(XServerError,"Unable to create X image",(char *) NULL);
  if (windows->image.mapped)
    XRefreshWindow(display,&windows->image,(XEvent *) NULL);
  /*
    Initialize image pixmaps structure.
  */
  XMapWindow(display,windows->image.id);
  windows->image.pixmaps=(Pixmap *)
    AllocateMemory(number_scenes*sizeof(Pixmap));
  windows->image.matte_pixmaps=(Pixmap *)
    AllocateMemory(number_scenes*sizeof(Pixmap));
  if ((windows->image.pixmaps == (Pixmap *) NULL) ||
      (windows->image.matte_pixmaps == (Pixmap *) NULL))
    MagickError(ResourceLimitError,"Unable to animate images",
      "Memory allocation failed");
  windows->image.pixmaps[0]=windows->image.pixmap;
  windows->image.matte_pixmaps[0]=windows->image.matte_pixmap;
  scene_info.pixels=(unsigned long *) NULL;
  scene_info.gamma_map=(XColor *) NULL;
  for (scene=1; scene < (int) number_scenes; scene++)
  {
    /*
      Create X image.
    */
    TransformRGBImage(images[scene],RGBColorspace);
    windows->image.pixmap=(Pixmap) NULL;
    windows->image.matte_pixmap=(Pixmap) NULL;
    if ((resource_info->map_type != (char *) NULL) ||
        (visual_info->class == TrueColor) ||
        (visual_info->class == DirectColor))
      if (images[scene]->class == PseudoClass)
        {
          /*
            Get pixel info for this scene.
          */
          XGetPixelInfo(display,visual_info,map_info,resource_info,
            images[scene],&scene_info);
          windows->image.pixel_info=(&scene_info);
        }
    status=XMakeImage(display,resource_info,&windows->image,images[scene],
      images[scene]->columns,images[scene]->rows);
    if (status == False)
      MagickError(XServerError,"Unable to create X image",(char *) NULL);
    if (resource_info->debug)
      {
        (void) fprintf(stderr,"Image: [%u] %.1024s %ux%u ",images[scene]->scene,
          images[scene]->filename,images[scene]->columns,images[scene]->rows);
        if (images[scene]->colors != 0)
          (void) fprintf(stderr,"%uc ",images[scene]->colors);
        (void) fprintf(stderr,"%.1024s\n",images[scene]->magick);
      }
    /*
      Window name is the base of the filename.
    */
    if (resource_info->title != (char *) NULL)
      {
        LabelImage(images[scene],resource_info->title);
        (void) strcpy(windows->image.name,images[scene]->label);
      }
    else
      {
        p=images[scene]->filename+Extent(images[scene]->filename)-1;
        while ((p > images[scene]->filename) && (*(p-1) != '/'))
          p--;
        FormatString(windows->image.name,"ImageMagick: %.1024s[%u of %u]",p,
          scene,number_scenes);
      }
    status=XStringListToTextProperty(&windows->image.name,1,&window_name);
    if (status != 0)
      {
        XSetWMName(display,windows->image.id,&window_name);
        XFree((void *) window_name.value);
      }
    windows->image.pixmaps[scene]=windows->image.pixmap;
    windows->image.matte_pixmaps[scene]=windows->image.matte_pixmap;
    event.xexpose.x=0;
    event.xexpose.y=0;
    event.xexpose.width=images[scene]->columns;
    event.xexpose.height=images[scene]->rows;
    XRefreshWindow(display,&windows->image,&event);
  }
  if (windows->command.mapped)
    XMapRaised(display,windows->command.id);
  /*
    Respond to events.
  */
  loaded_image=(Image *) NULL;
  scene=0;
  first_scene=0;
  image=images[0];
  state=ForwardAnimationState | RepeatAnimationState;
  (void) XMagickCommand(display,resource_info,windows,PlayCommand,&image,
    &state);
  do
  {
    if (XEventsQueued(display,QueuedAfterFlush) == 0)
      if ((state & PlayAnimationState) || (state & StepAnimationState))
        {
          if (state & ForwardAnimationState)
            {
              /*
                Forward animation:  increment scene number.
              */
              if (scene < ((int) number_scenes-1))
                scene++;
              else
                if (state & AutoReverseAnimationState)
                  {
                    state&=(~ForwardAnimationState);
                    scene--;
                  }
                else
                  {
                    if (!(state & RepeatAnimationState))
                      state&=(~PlayAnimationState);
                    scene=first_scene;
                    (void) sleep(resource_info->pause);
                  }
            }
          else
            {
              /*
                Reverse animation:  decrement scene number.
              */
              if (scene > first_scene)
                scene--;
              else
                if (state & AutoReverseAnimationState)
                  {
                    state|=ForwardAnimationState;
                    scene=first_scene;
                    (void) sleep(resource_info->pause);
                  }
                else
                  {
                    if (!(state & RepeatAnimationState))
                      state&=(~PlayAnimationState);
                    scene=number_scenes-1;
                  }
            }
          image=images[scene];
          if ((image != (Image *) NULL) && image->restart_animation_here)
            first_scene=scene;
          if ((state & StepAnimationState) ||
              (resource_info->title != (char *) NULL))
            {
              /*
                Update window title.
              */
              p=images[scene]->filename+Extent(images[scene]->filename)-1;
              while ((p > images[scene]->filename) && (*(p-1) != '/'))
                p--;
              FormatString(windows->image.name,
                "ImageMagick: %.1024s[%u of %u]",p,scene,number_scenes);
              if (resource_info->title != (char *) NULL)
                (void) strcpy(windows->image.name,image->label);
              status=
                XStringListToTextProperty(&windows->image.name,1,&window_name);
              if (status != 0)
                {
                  XSetWMName(display,windows->image.id,&window_name);
                  XFree((void *) window_name.value);
                }
            }
          /*
            Copy X pixmap to Image window.
          */
          XGetPixelInfo(display,visual_info,map_info,resource_info,
          images[scene],&scene_info);
          windows->image.pixel_info=(&scene_info);
          windows->image.ximage->width=image->columns;
          windows->image.ximage->height=image->rows;
          windows->image.pixmap=windows->image.pixmaps[scene];
          windows->image.matte_pixmap=windows->image.matte_pixmaps[scene];
          event.xexpose.x=0;
          event.xexpose.y=0;
          event.xexpose.width=image->columns;
          event.xexpose.height=image->rows;
          XRefreshWindow(display,&windows->image,&event);
          XSync(display,False);
          state&=(~StepAnimationState);
          if (resource_info->delay != 0)
            XDelay(display,(unsigned long) resource_info->delay*10);
          else
            XDelay(display,(unsigned long) image->delay*10);
          continue;
        }
    /*
      Handle a window event.
    */
    timestamp=time((time_t *) NULL);
    XNextEvent(display,&event);
    if (!windows->image.stasis)
      windows->image.stasis=(time((time_t *) NULL)-timestamp) > 0;
    if (event.xany.window == windows->command.id)
      {
        int
          id;

        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,CommandMenu,&event);
        if (id < 0)
          continue;
        (void) strcpy(command,CommandMenu[id]);
        command_type=CommandMenus[id];
        if (id < MagickMenus)
          {
            int
              entry;

            /*
              Select a command from a pop-up menu.
            */
            entry=XMenuWidget(display,windows,CommandMenu[id],Menus[id],
              command);
            if (entry < 0)
              continue;
            (void) strcpy(command,Menus[id][entry]);
            command_type=Commands[id][entry];
          }
        if (command_type != NullCommand)
          loaded_image=XMagickCommand(display,resource_info,windows,
            command_type,&image,&state);
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Button Press: 0x%lx %u +%d+%d\n",
            event.xbutton.window,event.xbutton.button,event.xbutton.x,
            event.xbutton.y);
        if ((event.xbutton.button == Button3) &&
            (event.xbutton.state & Mod1Mask))
          {
            /*
              Convert Alt-Button3 to Button2.
            */
            event.xbutton.button=Button2;
            event.xbutton.state&=(~Mod1Mask);
          }
        if (event.xbutton.window == windows->backdrop.id)
          {
            XSetInputFocus(display,event.xbutton.window,RevertToParent,
              event.xbutton.time);
            break;
          }
        if (event.xbutton.window == windows->image.id)
          {
            if (resource_info->immutable)
              {
                state|=ExitState;
                break;
              }
            /*
              Map/unmap Command widget.
            */
            if (windows->command.mapped)
              XWithdrawWindow(display,windows->command.id,
                windows->command.screen);
            else
              {
                (void) XCommandWidget(display,windows,CommandMenu,
                  (XEvent *) NULL);
                XMapRaised(display,windows->command.id);
              }
          }
        break;
      }
      case ButtonRelease:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Button Release: 0x%lx %u +%d+%d\n",
            event.xbutton.window,event.xbutton.button,event.xbutton.x,
            event.xbutton.y);
        break;
      }
      case ClientMessage:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Client Message: 0x%lx 0x%lx %d 0x%lx\n",
            event.xclient.window,event.xclient.message_type,
            event.xclient.format,(unsigned long) event.xclient.data.l[0]);
        if (event.xclient.message_type == windows->im_protocols)
          {
            if (*event.xclient.data.l == (int) windows->im_update_colormap)
              {
                /*
                  Update graphic context and window colormap.
                */
                for (i=0; i < (int) number_windows; i++)
                {
                  if (magick_windows[i]->id == windows->icon.id)
                    continue;
                  context_values.background=pixel_info->background_color.pixel;
                  context_values.foreground=pixel_info->foreground_color.pixel;
                  XChangeGC(display,magick_windows[i]->annotate_context,
                    context_mask,&context_values);
                  XChangeGC(display,magick_windows[i]->widget_context,
                    context_mask,&context_values);
                  context_values.background=pixel_info->foreground_color.pixel;
                  context_values.foreground=pixel_info->background_color.pixel;
                  context_values.plane_mask=
                    context_values.background ^ context_values.foreground;
                  XChangeGC(display,magick_windows[i]->highlight_context,
                    context_mask | GCPlaneMask,&context_values);
                  magick_windows[i]->attributes.background_pixel=
                    pixel_info->background_color.pixel;
                  magick_windows[i]->attributes.border_pixel=
                    pixel_info->border_color.pixel;
                  magick_windows[i]->attributes.colormap=map_info->colormap;
                  XChangeWindowAttributes(display,magick_windows[i]->id,
                    magick_windows[i]->mask,&magick_windows[i]->attributes);
                }
                if (windows->backdrop.id != (Window) NULL)
                  XInstallColormap(display,map_info->colormap);
                break;
              }
            if (*event.xclient.data.l == (int) windows->im_exit)
              {
                state|=ExitState;
                break;
              }
            break;
          }
        if (event.xclient.message_type == windows->dnd_protocols)
          {
            Atom
              selection,
              type;

            int
              format;

            unsigned char
              *data;

            unsigned long
              after,
              length;

            /*
              Display image named by the Drag-and-Drop selection.
            */
            if (((int) (*event.xclient.data.l) != 2) &&
                ((int) (*event.xclient.data.l) != 128))
              break;
            selection=XInternAtom(display,"DndSelection",False);
            status=XGetWindowProperty(display,root_window,selection,0L,2047L,
              False,(Atom) AnyPropertyType,&type,&format,&length,&after,&data);
            if ((status != Success) || (length == 0))
              break;
            if ((int) (*event.xclient.data.l) == 2)
              {
                /*
                  Offix DND.
                */
                (void) strcpy(resource_info->image_info->filename,
                  (char *) data);
              }
            else
              {
                /*
                  XDND.
                */
                if (strncmp((char *) data, "file:", 5) != 0)
                  {
                    XFree((void *) data);
                    break;
                  }
                (void) strcpy(resource_info->image_info->filename,
                  ((char *) data)+5);
              }
            loaded_image=ReadImage(resource_info->image_info);
            if (loaded_image != (Image *) NULL)
              state|=ExitState;
            XFree((void *) data);
            break;
          }
        /*
          If client window delete message, exit.
        */
        if (event.xclient.message_type != windows->wm_protocols)
          break;
        if (*event.xclient.data.l == (int) windows->wm_take_focus)
          {
            XSetInputFocus(display,event.xclient.window,RevertToParent,
              event.xclient.data.l[1]);
            break;
          }
        if (*event.xclient.data.l != (int) windows->wm_delete_window)
          break;
        XWithdrawWindow(display,event.xclient.window,visual_info->screen);
        if (event.xclient.window == windows->image.id)
          {
            state|=ExitState;
            break;
          }
        break;
      }
      case ConfigureNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Configure Notify: 0x%lx %dx%d+%d+%d %d\n",
            event.xconfigure.window,event.xconfigure.width,
            event.xconfigure.height,event.xconfigure.x,event.xconfigure.y,
            event.xconfigure.send_event);
        if (event.xconfigure.window == windows->image.id)
          {
            if (event.xconfigure.send_event != 0)
              {
                XWindowChanges
                  window_changes;

                /*
                  Position the transient windows relative of the Image window.
                */
                if (windows->command.geometry == (char *) NULL)
                  if (!windows->command.mapped)
                    {
                       windows->command.x=
                          event.xconfigure.x-windows->command.width-25;
                        windows->command.y=event.xconfigure.y;
                        XConstrainWindowPosition(display,&windows->command);
                        window_changes.x=windows->command.x;
                        window_changes.y=windows->command.y;
                        XReconfigureWMWindow(display,windows->command.id,
                          windows->command.screen,CWX | CWY,&window_changes);
                    }
                if (windows->widget.geometry == (char *) NULL)
                  if (!windows->widget.mapped)
                    {
                      windows->widget.x=
                        event.xconfigure.x+event.xconfigure.width/10;
                      windows->widget.y=
                        event.xconfigure.y+event.xconfigure.height/10;
                      XConstrainWindowPosition(display,&windows->widget);
                      window_changes.x=windows->widget.x;
                      window_changes.y=windows->widget.y;
                      XReconfigureWMWindow(display,windows->widget.id,
                        windows->widget.screen,CWX | CWY,&window_changes);
                    }
              }
            /*
              Image window has a new configuration.
            */
            windows->image.width=event.xconfigure.width;
            windows->image.height=event.xconfigure.height;
            break;
          }
        if (event.xconfigure.window == windows->icon.id)
          {
            /*
              Icon window has a new configuration.
            */
            windows->icon.width=event.xconfigure.width;
            windows->icon.height=event.xconfigure.height;
            break;
          }
        break;
      }
      case DestroyNotify:
      {
        /*
          Group leader has exited.
        */
        if (resource_info->debug)
          (void) fprintf(stderr,"Destroy Notify: 0x%lx\n",
            event.xdestroywindow.window);
        if (event.xdestroywindow.window == windows->group_leader.id)
          {
            state|=ExitState;
            break;
          }
        break;
      }
      case EnterNotify:
      {
        /*
          Selectively install colormap.
        */
        if (map_info->colormap != XDefaultColormap(display,visual_info->screen))
          if (event.xcrossing.mode != NotifyUngrab)
            XInductColormap(display,map_info->colormap);
        break;
      }
      case Expose:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Expose: 0x%lx %dx%d+%d+%d\n",
            event.xexpose.window,event.xexpose.width,event.xexpose.height,
            event.xexpose.x,event.xexpose.y);
        /*
          Repaint windows that are now exposed.
        */
        if (event.xexpose.window == windows->image.id)
          {
            windows->image.pixmap=windows->image.pixmaps[scene];
            windows->image.matte_pixmap=windows->image.matte_pixmaps[scene];
            XRefreshWindow(display,&windows->image,&event);
            break;
          }
        if (event.xexpose.window == windows->icon.id)
          if (event.xexpose.count == 0)
            {
              XRefreshWindow(display,&windows->icon,&event);
              break;
            }
        break;
      }
      case KeyPress:
      {
        static int
          length;

        /*
          Respond to a user key press.
        */
        length=XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        *(command+length)='\0';
        if (resource_info->debug)
          (void) fprintf(stderr,"Key press: 0x%lx (%c)\n",key_symbol,*command);
        command_type=NullCommand;
        switch (key_symbol)
        {
          case XK_o:
          {
            if (!(event.xkey.state & ControlMask))
              break;
            command_type=OpenCommand;
          }
          case XK_BackSpace:
          {
            command_type=StepBackwardCommand;
            break;
          }
          case XK_space:
          {
            command_type=StepForwardCommand;
            break;
          }
          case XK_less:
          {
            command_type=FasterCommand;
            break;
          }
          case XK_greater:
          {
            command_type=SlowerCommand;
            break;
          }
          case XK_F1:
          {
            command_type=HelpCommand;
            break;
          }
          case XK_Find:
          {
            command_type=BrowseDocumentationCommand;
            break;
          }
          case XK_question:
          {
            command_type=InfoCommand;
            break;
          }
          case XK_q:
          case XK_Cancel:
          {
            if (!(event.xkey.state & ControlMask))
              break;
            command_type=QuitCommand;
            break;
          }
          default:
            break;
        }
        if (command_type != NullCommand)
          loaded_image=XMagickCommand(display,resource_info,windows,
            command_type,&image,&state);
        break;
      }
      case KeyRelease:
      {
        /*
          Respond to a user key release.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        if (resource_info->debug)
          (void) fprintf(stderr,"Key release: 0x%lx (%c)\n",key_symbol,
            *command);
        break;
      }
      case LeaveNotify:
      {
        /*
          Selectively uninstall colormap.
        */
        if (map_info->colormap != XDefaultColormap(display,visual_info->screen))
          if (event.xcrossing.mode != NotifyUngrab)
            XUninductColormap(display,map_info->colormap);
        break;
      }
      case MapNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Map Notify: 0x%lx\n",event.xmap.window);
        if (event.xmap.window == windows->backdrop.id)
          {
            XSetInputFocus(display,event.xmap.window,RevertToParent,
              CurrentTime);
            windows->backdrop.mapped=True;
            break;
          }
        if (event.xmap.window == windows->image.id)
          {
            if (windows->backdrop.id != (Window) NULL)
              XInstallColormap(display,map_info->colormap);
            if (Latin1Compare(images[0]->magick,"LOGO") == 0)
              {
                if (Latin1Compare(displayed_image->filename,"Untitled") == 0)
                  loaded_image=XMagickCommand(display,resource_info,windows,
                    OpenCommand,&image,&state);
                else
                  state|=ExitState;
              }
            windows->image.mapped=True;
            break;
          }
        if (event.xmap.window == windows->info.id)
          {
            windows->info.mapped=True;
            break;
          }
        if (event.xmap.window == windows->icon.id)
          {
            /*
              Create an icon image.
            */
            XMakeStandardColormap(display,icon_visual,icon_resources,
              displayed_image,icon_map,icon_pixel);
            (void) XMakeImage(display,icon_resources,&windows->icon,
              displayed_image,windows->icon.width,windows->icon.height);
            XSetWindowBackgroundPixmap(display,windows->icon.id,
              windows->icon.pixmap);
            XClearWindow(display,windows->icon.id);
            XWithdrawWindow(display,windows->info.id,windows->info.screen);
            windows->icon.mapped=True;
            break;
          }
        if (event.xmap.window == windows->command.id)
          {
            windows->command.mapped=True;
            break;
          }
        if (event.xmap.window == windows->popup.id)
          {
            windows->popup.mapped=True;
            break;
          }
        if (event.xmap.window == windows->widget.id)
          {
            windows->widget.mapped=True;
            break;
          }
        break;
      }
      case MappingNotify:
      {
        XRefreshKeyboardMapping(&event.xmapping);
        break;
      }
      case NoExpose:
        break;
      case PropertyNotify:
      {
        Atom
          type;

        int
          format;

        unsigned char
          *data;

        unsigned long
          after,
          length;

        if (resource_info->debug)
          (void) fprintf(stderr,"Property Notify: 0x%lx 0x%lx %d\n",
            event.xproperty.window,event.xproperty.atom,event.xproperty.state);
        if (event.xproperty.atom != windows->im_remote_command)
          break;
        /*
          Display image named by the remote command protocol.
        */
        status=XGetWindowProperty(display,event.xproperty.window,
          event.xproperty.atom,0L,MaxTextExtent-1,False,(Atom) AnyPropertyType,
          &type,&format,&length,&after,&data);
        if ((status != Success) || (length == 0))
          break;
        (void) strcpy(resource_info->image_info->filename,(char *) data);
        loaded_image=ReadImage(resource_info->image_info);
        if (loaded_image != (Image *) NULL)
          state|=ExitState;
        XFree((void *) data);
        break;
      }
      case ReparentNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Reparent Notify: 0x%lx=>0x%lx\n",
            event.xreparent.parent,event.xreparent.window);
        break;
      }
      case UnmapNotify:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Unmap Notify: 0x%lx\n",event.xunmap.window);
        if (event.xunmap.window == windows->backdrop.id)
          {
            windows->backdrop.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->image.id)
          {
            windows->image.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->info.id)
          {
            windows->info.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->icon.id)
          {
            if (map_info->colormap == icon_map->colormap)
              XConfigureImageColormap(display,resource_info,windows,
                displayed_image);
            XFreeStandardColormap(display,icon_visual,icon_map,icon_pixel);
            windows->icon.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->command.id)
          {
            windows->command.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->popup.id)
          {
            if (windows->backdrop.id != (Window) NULL)
              XSetInputFocus(display,windows->image.id,RevertToParent,
                CurrentTime);
            windows->popup.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->widget.id)
          {
            if (windows->backdrop.id != (Window) NULL)
              XSetInputFocus(display,windows->image.id,RevertToParent,
                CurrentTime);
            windows->widget.mapped=False;
            break;
          }
        break;
      }
      default:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Event type: %d\n",event.type);
        break;
      }
    }
  }
  while (!(state & ExitState));
  if ((windows->visual_info->class == GrayScale) ||
      (windows->visual_info->class == PseudoColor) ||
      (windows->visual_info->class == DirectColor))
    {
      /*
        Withdraw windows.
      */
      if (windows->info.mapped)
        XWithdrawWindow(display,windows->info.id,windows->info.screen);
      if (windows->command.mapped)
        XWithdrawWindow(display,windows->command.id,windows->command.screen);
    }
  if (!resource_info->backdrop)
    if (windows->backdrop.mapped)
      {
        XWithdrawWindow(display,windows->backdrop.id,windows->backdrop.screen);
        XDestroyWindow(display,windows->backdrop.id);
        windows->backdrop.id=(Window) NULL;
        XWithdrawWindow(display,windows->image.id,windows->image.screen);
        XDestroyWindow(display,windows->image.id);
        windows->image.id=(Window) NULL;
      }
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  for (scene=1; scene < (int) number_scenes; scene++)
  {
    if (windows->image.pixmaps[scene] != (Pixmap) NULL)
      XFreePixmap(display,windows->image.pixmaps[scene]);
    windows->image.pixmaps[scene]=(Pixmap) NULL;
    if (windows->image.matte_pixmaps[scene] != (Pixmap) NULL)
      XFreePixmap(display,windows->image.matte_pixmaps[scene]);
    windows->image.matte_pixmaps[scene]=(Pixmap) NULL;
  }
  FreeMemory((char *) windows->image.pixmaps);
  windows->image.pixmaps=(Pixmap *) NULL;
  FreeMemory((char *) windows->image.matte_pixmaps);
  windows->image.matte_pixmaps=(Pixmap *) NULL;
  if (loaded_image == (Image *) NULL)
    {
      /*
        Destroy X windows.
      */
      if (windows->image.mapped)
        XWithdrawWindow(display,windows->image.id,windows->image.screen);
      XDelay(display,SuspendTime);
      for (i=0; i < (int) number_windows; i++)
      {
        if (magick_windows[i]->id != (Window) NULL)
          {
            magick_windows[i]->shared_memory=False;
            (void) XMakeImage(display,resource_info,magick_windows[i],
              (Image *) NULL,1,1);
            XDestroyWindow(display,magick_windows[i]->id);
          }
        if (magick_windows[i]->ximage != (XImage *) NULL)
          XDestroyImage(magick_windows[i]->ximage);
        if (magick_windows[i]->pixmap != (Pixmap) NULL)
          XFreePixmap(display,magick_windows[i]->pixmap);
      }
      /*
        Free Standard Colormap.
      */
      XFreeStandardColormap(display,icon_visual,icon_map,icon_pixel);
      if (resource_info->map_type == (char *) NULL)
        XFreeStandardColormap(display,visual_info,map_info,pixel_info);
      /*
        Free X resources.
      */
      if (resource_info->backdrop)
        XFreeCursor(display,windows->backdrop.cursor);
      if (windows->widget.highlight_stipple != (Pixmap) NULL)
        XFreePixmap(display,windows->widget.highlight_stipple);
      if (windows->widget.highlight_stipple != (Pixmap) NULL)
        XFreePixmap(display,windows->widget.shadow_stipple);
      XFreeGC(display,pixel_info->widget_context);
      XFreeGC(display,pixel_info->highlight_context);
      XFreeGC(display,pixel_info->annotate_context);
      XFreeGC(display,icon_pixel->annotate_context);
      XFreeFont(display,font_info);
      XFree((void *) class_hints);
      XFree((void *) manager_hints);
      XFree((void *) icon_visual);
      XFree((void *) visual_info);
      XFree((void *) icon_map);
      XFree((void *) map_info);
      FreeMemory((char *) windows->popup.name);
      FreeMemory((char *) windows->widget.name);
      FreeMemory((char *) windows->image.icon_name);
      FreeMemory((char *) windows->image.name);
      FreeMemory((char *) windows->icon_resources);
      FreeMemory((char *) windows->icon_pixel);
      FreeMemory((char *) windows->pixel_info);
      (void) signal(SIGSEGV,SIG_DFL);
      (void) signal(SIGINT,SIG_DFL);
      (void) XSetWindows((XWindows *) NULL);
    }
  XSync(display,False);
  /*
    Restore our progress monitor and warning handlers.
  */
  (void) SetMonitorHandler(monitor_handler);
  (void) SetWarningHandler(warning_handler);
  /*
    Change to home directory.
  */
  (void) getcwd(working_directory,MaxTextExtent-1);
  (void) chdir(resource_info->home_directory);
  return(loaded_image);
}
#endif
