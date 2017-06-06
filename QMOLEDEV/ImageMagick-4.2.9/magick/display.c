/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%             DDDD   IIIII  SSSSS  PPPP   L       AAA   Y   Y                 %
%             D   D    I    SS     P   P  L      A   A   Y Y                  %
%             D   D    I     SSS   PPPP   L      AAAAA    Y                   %
%             D   D    I       SS  P      L      A   A    Y                   %
%             DDDD   IIIII  SSSSS  P      LLLLL  A   A    Y                   %
%                                                                             %
%                                                                             %
%            Methods to Interactively Display and Edit an Image               %
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

/*
  Constant declaration.
*/
const int
  RoiDelta = 8;

#if defined(HasX11)
#include "xwindows.h"
#include "display.h"
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X A n n o t a t e E d i t I m a g e                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XAnnotateEditImage annotates the image with text.
%
%  The format of the XAnnotateEditImage method is:
%
%      unsigned int XDisplayBackgroundImage(Display *display,
%        XResourceInfo *resource_info,Image *image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
*/
static unsigned int XAnnotateEditImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image *image)
{
  static const char
    *AnnotateMenu[]=
    {
      "Font Name",
      "Font Color",
      "Box Color",
      "Rotate Text",
      "Help",
      "Dismiss",
      (char *) NULL
    },
    *TextMenu[]=
    {
      "Help",
      "Apply",
      (char *) NULL
    };

  static double
    degrees = 0.0;

  static const ModeType
    AnnotateCommands[]=
    {
      AnnotateNameCommand,
      AnnotateFontColorCommand,
      AnnotateBackgroundColorCommand,
      AnnotateRotateCommand,
      AnnotateHelpCommand,
      AnnotateDismissCommand
    },
    TextCommands[]=
    {
      TextHelpCommand,
      TextApplyCommand
    };

  static unsigned int
    box_id = MaxNumberPens-2,
    font_id = 0,
    pen_id = 0,
    transparent_box = True,
    transparent_pen = False;

  char
    *ColorMenu[MaxNumberPens+1],
    command[MaxTextExtent],
    text[MaxTextExtent];

  Cursor
    cursor;

  GC
    annotate_context;

  int
    id,
    pen_number,
    x,
    y;

  KeySym
    key_symbol;

  register char
    *p;

  register int
    i;

  unsigned int
    height,
    status,
    width;

  unsigned long
    state;

  XAnnotateInfo
    *annotate_info,
    *previous_info;

  XColor
    color;

  XFontStruct
    *font_info;

  XEvent
    event,
    text_event;

  /*
    Map Command widget.
  */
  windows->command.name="Annotate";
  windows->command.data=4;
  (void) XCommandWidget(display,windows,AnnotateMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  cursor=XCreateFontCursor(display,XC_left_side);
  XDefineCursor(display,windows->image.id,cursor);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",x+windows->image.x,y+windows->image.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,AnnotateMenu,&event);
        XDefineCursor(display,windows->image.id,cursor);
        if (id < 0)
          continue;
        switch (AnnotateCommands[id])
        {
          case AnnotateNameCommand:
          {
            char
              *FontMenu[MaxNumberFonts];

            int
              font_number;

            /*
              Initialize menu selections.
            */
            for (i=0; i < MaxNumberFonts; i++)
              FontMenu[i]=resource_info->font_name[i];
            FontMenu[MaxNumberFonts-2]="Browser...";
            FontMenu[MaxNumberFonts-1]=(char *) NULL;
            /*
              Select a font name from the pop-up menu.
            */
            font_number=XMenuWidget(display,windows,AnnotateMenu[id],
              (const char **) FontMenu,command);
            if (font_number < 0)
              break;
            if (font_number == (MaxNumberFonts-2))
              {
                static char
                  font_name[MaxTextExtent]="fixed";

                /*
                  Select a font name from a browser.
                */
                resource_info->font_name[font_number]=font_name;
                XFontBrowserWidget(display,windows,"Select",font_name);
                if (*font_name == '\0')
                  break;
              }
            /*
              Initialize font info.
            */
            font_info=
              XLoadQueryFont(display,resource_info->font_name[font_number]);
            if (font_info == (XFontStruct *) NULL)
              {
                XNoticeWidget(display,windows,"Unable to load font:",
                  resource_info->font_name[font_number]);
                break;
              }
            font_id=font_number;
            XFreeFont(display,font_info);
            break;
          }
          case AnnotateFontColorCommand:
          {
            /*
              Initialize menu selections.
            */
            for (i=0; i < (int) (MaxNumberPens-2); i++)
              ColorMenu[i]=resource_info->pen_colors[i];
            ColorMenu[MaxNumberPens-2]="transparent";
            ColorMenu[MaxNumberPens-1]="Browser...";
            ColorMenu[MaxNumberPens]=(char *) NULL;
            /*
              Select a pen color from the pop-up menu.
            */
            pen_number=XMenuWidget(display,windows,AnnotateMenu[id],
              (const char **) ColorMenu,command);
            if (pen_number < 0)
              break;
            transparent_pen=pen_number == (MaxNumberPens-2);
            if (transparent_pen)
              break;
            if (pen_number == (MaxNumberPens-1))
              {
                static char
                  color_name[MaxTextExtent] = "gray";

                /*
                  Select a pen color from a dialog.
                */
                resource_info->pen_colors[pen_number]=color_name;
                XColorBrowserWidget(display,windows,"Select",color_name);
                if (*color_name == '\0')
                  break;
              }
            /*
              Set pen color.
            */
            (void) XParseColor(display,windows->map_info->colormap,
              resource_info->pen_colors[pen_number],&color);
            XBestPixel(display,windows->map_info->colormap,(XColor *) NULL,
              (unsigned int) MaxColors,&color);
            windows->pixel_info->pen_colors[pen_number]=color;
            pen_id=pen_number;
            break;
          }
          case AnnotateBackgroundColorCommand:
          {
            /*
              Initialize menu selections.
            */
            for (i=0; i < (int) (MaxNumberPens-2); i++)
              ColorMenu[i]=resource_info->pen_colors[i];
            ColorMenu[MaxNumberPens-2]="transparent";
            ColorMenu[MaxNumberPens-1]="Browser...";
            ColorMenu[MaxNumberPens]=(char *) NULL;
            /*
              Select a pen color from the pop-up menu.
            */
            pen_number=XMenuWidget(display,windows,AnnotateMenu[id],
              (const char **) ColorMenu,command);
            if (pen_number < 0)
              break;
            transparent_box=pen_number == (MaxNumberPens-2);
            if (transparent_box)
              break;
            if (pen_number == (MaxNumberPens-1))
              {
                static char
                  color_name[MaxTextExtent] = "gray";

                /*
                  Select a pen color from a dialog.
                */
                resource_info->pen_colors[pen_number]=color_name;
                XColorBrowserWidget(display,windows,"Select",color_name);
                if (*color_name == '\0')
                  break;
              }
            /*
              Set pen color.
            */
            (void) XParseColor(display,windows->map_info->colormap,
              resource_info->pen_colors[pen_number],&color);
            XBestPixel(display,windows->map_info->colormap,(XColor *) NULL,
              (unsigned int) MaxColors,&color);
            windows->pixel_info->pen_colors[pen_number]=color;
            box_id=pen_number;
            break;
          }
          case AnnotateRotateCommand:
          {
            int
              entry;

            static char
              angle[MaxTextExtent] = "30.0";

            static const char
              *RotateMenu[]=
              {
                "-90",
                "-45",
                "-30",
                "0",
                "30",
                "45",
                "90",
                "180",
                (char *) NULL,
                (char *) NULL,
              };

            /*
              Select a command from the pop-up menu.
            */
            RotateMenu[8]="Dialog...";
            entry=XMenuWidget(display,windows,AnnotateMenu[id],RotateMenu,
              command);
            if (entry < 0)
              break;
            if (entry != 8)
              {
                degrees=atof(RotateMenu[entry]);
                break;
              }
            (void) XDialogWidget(display,windows,"OK","Enter rotation angle:",
              angle);
            if (*angle == '\0')
              break;
            degrees=atof(angle);
            break;
          }
          case AnnotateHelpCommand:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Annotation",ImageAnnotateHelp);
            break;
          }
          case AnnotateDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        /*
          Change to text entering mode.
        */
        x=event.xbutton.x;
        y=event.xbutton.y;
        state|=ExitState;
        break;
      }
      case ButtonRelease:
        break;
      case Expose:
        break;
      case KeyPress:
      {
        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Annotation",ImageAnnotateHelp);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        break;
      }
      default:
        break;
    }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (state & EscapeState)
    return(True);
  /*
    Set font info and check boundary conditions.
  */
  font_info=XLoadQueryFont(display,resource_info->font_name[font_id]);
  if (font_info == (XFontStruct *) NULL)
    {
      XNoticeWidget(display,windows,"Unable to load font:",
        resource_info->font_name[font_id]);
      font_info=windows->font_info;
    }
  if ((x+font_info->max_bounds.width) >= (int) windows->image.width)
    x=windows->image.width-font_info->max_bounds.width;
  if (y < (int) (font_info->ascent+font_info->descent))
    y=font_info->ascent+font_info->descent;
  if ((font_info->max_bounds.width > (int) windows->image.width) ||
      ((font_info->ascent+font_info->descent) >= (int) windows->image.height))
    return(False);
  /*
    Initialize annotate structure.
  */
  annotate_info=(XAnnotateInfo *) AllocateMemory(sizeof(XAnnotateInfo));
  if (annotate_info == (XAnnotateInfo *) NULL)
    return(False);
  XGetAnnotateInfo(annotate_info);
  annotate_info->x=x;
  annotate_info->y=y;
  if (!transparent_box && !transparent_pen)
    annotate_info->stencil=OpaqueStencil;
  else
    if (!transparent_box)
      annotate_info->stencil=BackgroundStencil;
    else
      annotate_info->stencil=ForegroundStencil;
  annotate_info->height=font_info->ascent+font_info->descent;
  annotate_info->degrees=degrees;
  annotate_info->font_info=font_info;
  annotate_info->text=(char *) AllocateMemory(
    (windows->image.width/Max(font_info->min_bounds.width,1)+2)*sizeof(char));
  if (annotate_info->text == (char *) NULL)
    return(False);
  /*
    Create cursor and set graphic context.
  */
  cursor=XCreateFontCursor(display,XC_pencil);
  XDefineCursor(display,windows->image.id,cursor);
  annotate_context=windows->image.annotate_context;
  XSetFont(display,annotate_context,font_info->fid);
  XSetBackground(display,annotate_context,
    windows->pixel_info->pen_colors[box_id].pixel);
  XSetForeground(display,annotate_context,
    windows->pixel_info->pen_colors[pen_id].pixel);
  /*
    Begin annotating the image with text.
  */
  windows->command.name="Text";
  windows->command.data=0;
  (void) XCommandWidget(display,windows,TextMenu,(XEvent *) NULL);
  state=DefaultState;
  XDrawString(display,windows->image.id,annotate_context,x,y,"_",1);
  text_event.xexpose.width=(unsigned int) font_info->max_bounds.width;
  text_event.xexpose.height=font_info->max_bounds.ascent+
    font_info->max_bounds.descent;
  p=annotate_info->text;
  do
  {
    /*
      Display text cursor.
    */
    *p='\0';
    XDrawString(display,windows->image.id,annotate_context,x,y,"_",1);
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        XSetBackground(display,annotate_context,
          windows->pixel_info->background_color.pixel);
        XSetForeground(display,annotate_context,
          windows->pixel_info->foreground_color.pixel);
        id=XCommandWidget(display,windows,AnnotateMenu,&event);
        XSetBackground(display,annotate_context,
          windows->pixel_info->pen_colors[box_id].pixel);
        XSetForeground(display,annotate_context,
          windows->pixel_info->pen_colors[pen_id].pixel);
        if (id < 0)
          continue;
        switch (TextCommands[id])
        {
          case TextHelpCommand:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Annotation",ImageAnnotateHelp);
            XDefineCursor(display,windows->image.id,cursor);
            break;
          }
          case TextApplyCommand:
          {
            /*
              Finished annotating.
            */
            annotate_info->width=XTextWidth(font_info,annotate_info->text,
              Extent(annotate_info->text));
            XRefreshWindow(display,&windows->image,&text_event);
            state|=ExitState;
            break;
          }
          default:
            break;
        }
        continue;
      }
    /*
      Erase text cursor.
    */
    text_event.xexpose.x=x;
    text_event.xexpose.y=y-font_info->max_bounds.ascent;
    XClearArea(display,windows->image.id,x,text_event.xexpose.y,
      text_event.xexpose.width,text_event.xexpose.height,False);
    XRefreshWindow(display,&windows->image,&text_event);
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.window != windows->image.id)
          break;
        if (event.xbutton.button == Button2)
          {
            /*
              Request primary selection.
            */
            XConvertSelection(display,XA_PRIMARY,XA_STRING,XA_STRING,
              windows->image.id,CurrentTime);
            break;
          }
        break;
      }
      case Expose:
      {
        if (event.xexpose.count == 0)
          {
            XAnnotateInfo
              *text_info;

            /*
              Refresh Image window.
            */
            XRefreshWindow(display,&windows->image,(XEvent *) NULL);
            text_info=annotate_info;
            while (text_info != (XAnnotateInfo *) NULL)
            {
              if (annotate_info->stencil == ForegroundStencil)
                XDrawString(display,windows->image.id,annotate_context,
                  text_info->x,text_info->y,text_info->text,
                  Extent(text_info->text));
              else
                XDrawImageString(display,windows->image.id,annotate_context,
                  text_info->x,text_info->y,text_info->text,
                  Extent(text_info->text));
              text_info=text_info->previous;
            }
            XDrawString(display,windows->image.id,annotate_context,x,y,"_",1);
          }
        break;
      }
      case KeyPress:
      {
        int
          length;

        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        length=XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        *(command+length)='\0';
        if ((event.xkey.state & ControlMask) || (event.xkey.state & Mod1Mask))
          state|=ModifierState;
        if (state & ModifierState)
          switch (key_symbol)
          {
            case XK_u:
            case XK_U:
            {
              key_symbol=DeleteCommand;
              break;
            }
            default:
              break;
          }
        switch (key_symbol)
        {
          case XK_BackSpace:
          {
            /*
              Erase one character.
            */
            if (p == annotate_info->text)
              {
                if (annotate_info->previous == (XAnnotateInfo *) NULL)
                  break;
                else
                  {
                    /*
                      Go to end of the previous line of text.
                    */
                    annotate_info=annotate_info->previous;
                    p=annotate_info->text;
                    x=annotate_info->x+annotate_info->width;
                    y=annotate_info->y;
                    if (annotate_info->width != 0)
                      p+=Extent(annotate_info->text);
                    break;
                  }
              }
            p--;
            x-=XTextWidth(font_info,p,1);
            text_event.xexpose.x=x;
            text_event.xexpose.y=y-font_info->max_bounds.ascent;
            XRefreshWindow(display,&windows->image,&text_event);
            break;
          }
          case XK_bracketleft:
          {
            key_symbol=XK_Escape;
            break;
          }
          case DeleteCommand:
          {
            /*
              Erase the entire line of text.
            */
            while (p != annotate_info->text)
            {
              p--;
              x-=XTextWidth(font_info,p,1);
              text_event.xexpose.x=x;
              XRefreshWindow(display,&windows->image,&text_event);
            }
            break;
          }
          case XK_Escape:
          case XK_F20:
          {
            /*
              Finished annotating.
            */
            annotate_info->width=XTextWidth(font_info,annotate_info->text,
              Extent(annotate_info->text));
            XRefreshWindow(display,&windows->image,&text_event);
            state|=ExitState;
            break;
          }
          default:
          {
            /*
              Draw a single character on the Image window.
            */
            if (state & ModifierState)
              break;
            if (*command == '\0')
              break;
            *p=(*command);
            if (annotate_info->stencil == ForegroundStencil)
              XDrawString(display,windows->image.id,annotate_context,x,y,p,1);
            else
              XDrawImageString(display,windows->image.id,annotate_context,x,y,
                p,1);
            x+=XTextWidth(font_info,p,1);
            p++;
            if ((x+font_info->max_bounds.width) < (int) windows->image.width)
              break;
          }
          case XK_Return:
          case XK_KP_Enter:
          {
            /*
              Advance to the next line of text.
            */
            *p='\0';
            annotate_info->width=XTextWidth(font_info,annotate_info->text,
              Extent(annotate_info->text));
            if (annotate_info->next != (XAnnotateInfo *) NULL)
              {
                /*
                  Line of text already exists.
                */
                annotate_info=annotate_info->next;
                x=annotate_info->x;
                y=annotate_info->y;
                p=annotate_info->text;
                break;
              }
            annotate_info->next=(XAnnotateInfo *)
              AllocateMemory(sizeof(XAnnotateInfo));
            if (annotate_info->next == (XAnnotateInfo *) NULL)
              return(False);
            *annotate_info->next=(*annotate_info);
            annotate_info->next->previous=annotate_info;
            annotate_info=annotate_info->next;
            annotate_info->text=(char *) AllocateMemory((windows->image.width/
              Max(font_info->min_bounds.width,1)+2)*sizeof(char));
            if (annotate_info->text == (char *) NULL)
              return(False);
            annotate_info->y+=annotate_info->height;
            if (annotate_info->y > (int) windows->image.height)
              annotate_info->y=annotate_info->height;
            annotate_info->next=(XAnnotateInfo *) NULL;
            x=annotate_info->x;
            y=annotate_info->y;
            p=annotate_info->text;
            break;
          }
        }
        break;
      }
      case KeyRelease:
      {
        /*
          Respond to a user key release.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        state&=(~ModifierState);
        break;
      }
      case SelectionNotify:
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

        /*
          Obtain response from primary selection.
        */
        if (event.xselection.property == (Atom) None)
          break;
        status=XGetWindowProperty(display,event.xselection.requestor,
          event.xselection.property,0L,MaxTextExtent-1,True,XA_STRING,&type,
          &format,&length,&after,&data);
        if ((status != Success) || (type != XA_STRING) || (format == 32) ||
            (length == 0))
          break;
        /*
          Annotate Image window with primary selection.
        */
        for (i=0; i < (int) length; i++)
        {
          if (data[i] != '\n')
            {
              /*
                Draw a single character on the Image window.
              */
              *p=data[i];
              XDrawString(display,windows->image.id,annotate_context,x,y,p,1);
              x+=XTextWidth(font_info,p,1);
              p++;
              if ((x+font_info->max_bounds.width) < (int) windows->image.width)
                continue;
            }
          /*
            Advance to the next line of text.
          */
          *p='\0';
          annotate_info->width=XTextWidth(font_info,annotate_info->text,
            Extent(annotate_info->text));
          if (annotate_info->next != (XAnnotateInfo *) NULL)
            {
              /*
                Line of text already exists.
              */
              annotate_info=annotate_info->next;
              x=annotate_info->x;
              y=annotate_info->y;
              p=annotate_info->text;
              continue;
            }
          annotate_info->next=(XAnnotateInfo *)
            AllocateMemory(sizeof(XAnnotateInfo));
          if (annotate_info->next == (XAnnotateInfo *) NULL)
            return(False);
          *annotate_info->next=(*annotate_info);
          annotate_info->next->previous=annotate_info;
          annotate_info=annotate_info->next;
          annotate_info->text=(char *) AllocateMemory((windows->image.width/
            Max(font_info->min_bounds.width,1)+2)*sizeof(char));
          if (annotate_info->text == (char *) NULL)
            return(False);
          annotate_info->y+=annotate_info->height;
          if (annotate_info->y > (int) windows->image.height)
            annotate_info->y=annotate_info->height;
          annotate_info->next=(XAnnotateInfo *) NULL;
          x=annotate_info->x;
          y=annotate_info->y;
          p=annotate_info->text;
        }
        XFree((void *) data);
        break;
      }
      default:
        break;
    }
  } while (!(state & ExitState));
  XFreeCursor(display,cursor);
  /*
    Annotation is relative to image configuration.
  */
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  /*
    Initialize annotated image.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  while (annotate_info != (XAnnotateInfo *) NULL)
  {
    if (annotate_info->width == 0)
      {
        /*
          No text on this line--  go to the next line of text.
        */
        previous_info=annotate_info->previous;
        FreeMemory((char *) annotate_info->text);
        FreeMemory((char *) annotate_info);
        annotate_info=previous_info;
        continue;
      }
    /*
      Determine pixel index for box and pen color.
    */
    windows->pixel_info->box_color=windows->pixel_info->pen_colors[box_id];
    if (windows->pixel_info->colors != 0)
      for (i=0; i < (int) windows->pixel_info->colors; i++)
        if (windows->pixel_info->pixels[i] ==
            windows->pixel_info->pen_colors[box_id].pixel)
          {
            windows->pixel_info->box_index=(unsigned short) i;
            break;
          }
    windows->pixel_info->pen_color=windows->pixel_info->pen_colors[pen_id];
    if (windows->pixel_info->colors != 0)
      for (i=0; i < (int) windows->pixel_info->colors; i++)
        if (windows->pixel_info->pixels[i] ==
            windows->pixel_info->pen_colors[pen_id].pixel)
          {
            windows->pixel_info->pen_index=(unsigned short) i;
            break;
          }
    /*
      Define the annotate geometry string.
    */
    annotate_info->x=
      width*(annotate_info->x+windows->image.x)/windows->image.ximage->width;
    annotate_info->y=height*(annotate_info->y-font_info->ascent+
      windows->image.y)/windows->image.ximage->height;
    FormatString(annotate_info->geometry,"%ux%u%+d%+d",
      width*annotate_info->width/windows->image.ximage->width,
      height*annotate_info->height/windows->image.ximage->height,
      annotate_info->x+x,annotate_info->y+y);
    /*
      Annotate image with text.
    */
    status=XAnnotateImage(display,windows->pixel_info,annotate_info,image);
    if (status == 0)
      return(False);
    /*
      Free up memory.
    */
    previous_info=annotate_info->previous;
    FreeMemory((char *) annotate_info->text);
    FreeMemory((char *) annotate_info);
    annotate_info=previous_info;
  }
  XSetForeground(display,annotate_context,
    windows->pixel_info->foreground_color.pixel);
  XSetBackground(display,annotate_context,
    windows->pixel_info->background_color.pixel);
  XSetFont(display,annotate_context,windows->font_info->fid);
  XSetCursorState(display,windows,False);
  XFreeFont(display,font_info);
  /*
    Update image configuration.
  */
  XConfigureImageColormap(display,resource_info,windows,image);
  (void) XConfigureImage(display,resource_info,windows,image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X B a c k g r o u n d I m a g e                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XBackgroundImage displays the image in the background of a window.
%
%  The format of the XBackgroundImage method is:
%
%    status=XBackgroundImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XBackgroundImage return True if the image is
%      printed.  False is returned is there is a memory shortage or if the
%      image fails to print.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XBackgroundImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image **image)
{
#define BackgroundImageText  "  Backgrounding the image...  "

  static char
    window_id[MaxTextExtent] = "root";

  XResourceInfo
    background_resources;

  unsigned int
    status;

  /*
    Put image in background.
  */
  status=XDialogWidget(display,windows,"Background",
    "Enter window id (id 0x00 selects window with pointer):",window_id);
  if (*window_id == '\0')
    return(False);
  (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
  XInfoWidget(display,windows,BackgroundImageText);
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  background_resources=(*resource_info);
  background_resources.window_id=window_id;
  background_resources.backdrop=status;
  status=XDisplayBackgroundImage(display,&background_resources,*image);
  if (status)
    XClientMessage(display,windows->image.id,windows->im_protocols,
      windows->im_retain_colors,CurrentTime);
  XSetCursorState(display,windows,False);
  (void) XMagickCommand(display,resource_info,windows,UndoCommand,image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X C h o p I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XChopImage chops the X image.
%
%  The format of the XChopImage method is:
%
%    status=XChopImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XChopImage return True if the image is
%      cut.  False is returned is there is a memory shortage or if the
%      image fails to cut.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XChopImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image **image)
{
  static const char
    *ChopMenu[]=
    {
      "Direction",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static ModeType
    direction = HorizontalChopCommand;

  static const ModeType
    ChopCommands[]=
    {
      ChopDirectionCommand,
      ChopHelpCommand,
      ChopDismissCommand
    },
    DirectionCommands[]=
    {
      HorizontalChopCommand,
      VerticalChopCommand
    };

  char
    text[MaxTextExtent];

  Image
    *chop_image;

  int
    id,
    x,
    y;

  RectangleInfo
    chop_info;

  unsigned int
    distance,
    height,
    width;

  unsigned long
    scale_factor,
    state;

  XEvent
    event;

  XSegment
    segment_info;

  /*
    Map Command widget.
  */
  windows->command.name="Chop";
  windows->command.data=1;
  (void) XCommandWidget(display,windows,ChopMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",x+windows->image.x,y+windows->image.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,ChopMenu,&event);
        if (id < 0)
          continue;
        switch (ChopCommands[id])
        {
          case ChopDirectionCommand:
          {
            char
              command[MaxTextExtent];

            static const char
              *Directions[]=
              {
                "horizontal",
                "vertical",
                (char *) NULL,
              };

            /*
              Select a command from the pop-up menu.
            */
            id=
              XMenuWidget(display,windows,ChopMenu[id],Directions,command);
            if (id >= 0)
              direction=DirectionCommands[id];
            break;
          }
          case ChopHelpCommand:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Chopping",ImageChopHelp);
            break;
          }
          case ChopDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        /*
          User has committed to start point of chopping line.
        */
        segment_info.x1=event.xbutton.x;
        segment_info.x2=event.xbutton.x;
        segment_info.y1=event.xbutton.y;
        segment_info.y2=event.xbutton.y;
        state|=ExitState;
        break;
      }
      case ButtonRelease:
        break;
      case Expose:
        break;
      case KeyPress:
      {
        char
          command[MaxTextExtent];

        KeySym
          key_symbol;

        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,
          sizeof(command),&key_symbol,(XComposeStatus *) NULL);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Chopping",ImageChopHelp);
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as text cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
      }
    }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (state & EscapeState)
    return(True);
  /*
    Draw line as pointer moves until the mouse button is released.
  */
  chop_info.width=0;
  chop_info.height=0;
  chop_info.x=0;
  chop_info.y=0;
  distance=0;
  XSetFunction(display,windows->image.highlight_context,GXinvert);
  state=DefaultState;
  do
  {
    if (distance > 9)
      {
        /*
          Display info and draw chopping line.
        */
        if (!windows->info.mapped)
          XMapWindow(display,windows->info.id);
        FormatString(text," %ux%u%+d%+d",chop_info.width,chop_info.height,
          chop_info.x,chop_info.y);
        XInfoWidget(display,windows,text);
        XHighlightLine(display,windows->image.id,
          windows->image.highlight_context,&segment_info);
      }
    else
      if (windows->info.mapped)
        XWithdrawWindow(display,windows->info.id,windows->info.screen);
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (distance > 9)
      XHighlightLine(display,windows->image.id,
        windows->image.highlight_context,&segment_info);
    switch (event.type)
    {
      case ButtonPress:
      {
        segment_info.x2=event.xmotion.x;
        segment_info.y2=event.xmotion.y;
        break;
      }
      case ButtonRelease:
      {
        /*
          User has committed to chopping line.
        */
        segment_info.x2=event.xbutton.x;
        segment_info.y2=event.xbutton.y;
        state|=ExitState;
        break;
      }
      case Expose:
        break;
      case MotionNotify:
      {
        segment_info.x2=event.xmotion.x;
        segment_info.y2=event.xmotion.y;
      }
      default:
        break;
    }
    /*
      Check boundary conditions.
    */
    if (segment_info.x2 < 0)
      segment_info.x2=0;
    else
      if (segment_info.x2 > windows->image.ximage->width)
        segment_info.x2=windows->image.ximage->width;
    if (segment_info.y2 < 0)
      segment_info.y2=0;
    else
      if (segment_info.y2 > windows->image.ximage->height)
        segment_info.y2=windows->image.ximage->height;
    distance=
      ((segment_info.x2-segment_info.x1)*(segment_info.x2-segment_info.x1))+
      ((segment_info.y2-segment_info.y1)*(segment_info.y2-segment_info.y1));
    /*
      Compute chopping geometry.
    */
    if (direction == HorizontalChopCommand)
      {
        chop_info.width=segment_info.x2-segment_info.x1+1;
        chop_info.x=windows->image.x+segment_info.x1;
        chop_info.height=0;
        chop_info.y=0;
        if (segment_info.x1 > (int) segment_info.x2)
          {
            chop_info.width=segment_info.x1-segment_info.x2+1;
            chop_info.x=windows->image.x+segment_info.x2;
          }
      }
    else
      {
        chop_info.width=0;
        chop_info.height=segment_info.y2-segment_info.y1+1;
        chop_info.x=0;
        chop_info.y=windows->image.y+segment_info.y1;
        if (segment_info.y1 > segment_info.y2)
          {
            chop_info.height=segment_info.y1-segment_info.y2+1;
            chop_info.y=windows->image.y+segment_info.y2;
          }
      }
  } while (!(state & ExitState));
  XSetFunction(display,windows->image.highlight_context,GXcopy);
  XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (distance <= 9)
    return(True);
  /*
    Image chopping is relative to image configuration.
  */
  (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  windows->image.window_changes.width=
    windows->image.ximage->width-chop_info.width;
  windows->image.window_changes.height=
    windows->image.ximage->height-chop_info.height;
  width=(*image)->columns;
  height=(*image)->rows;
  x=0;
  y=0;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  scale_factor=UpShift(width)/windows->image.ximage->width;
  chop_info.x+=x;
  chop_info.x=DownShift(chop_info.x*scale_factor);
  chop_info.width=DownShift(chop_info.width*scale_factor);
  scale_factor=UpShift(height)/windows->image.ximage->height;
  chop_info.y+=y;
  chop_info.y=DownShift(chop_info.y*scale_factor);
  chop_info.height=DownShift(chop_info.height*scale_factor);
  /*
    Chop image.
  */
  chop_image=ChopImage(*image,&chop_info);
  XSetCursorState(display,windows,False);
  if (chop_image == (Image *) NULL)
    return(False);
  DestroyImage(*image);
  *image=chop_image;
  /*
    Update image configuration.
  */
  XConfigureImageColormap(display,resource_info,windows,*image);
  (void) XConfigureImage(display,resource_info,windows,*image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X C o l o r E d i t I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XColorEditImage allows the user to interactively change
%  the color of one pixel for a DirectColor image or one colormap entry for
%  a PseudoClass image.
%
%  The format of the XColorEditImage method is:
%
%    XColorEditImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
*/
static unsigned int XColorEditImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image **image)
{
  static const char
    *ColorEditMenu[]=
    {
      "Method",
      "Pixel Color",
      "Border Color",
      "Fuzz",
      "Undo",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static const ModeType
    ColorEditCommands[]=
    {
      ColorEditMethodCommand,
      ColorEditColorCommand,
      ColorEditBorderCommand,
      ColorEditFuzzCommand,
      ColorEditUndoCommand,
      ColorEditHelpCommand,
      ColorEditDismissCommand
    };

  static PaintMethod
    method = PointMethod;

  static unsigned int
    pen_id = 0;

  static XColor
    border_color = { 0, 0, 0, 0, 0 };

  char
    command[MaxTextExtent],
    text[MaxTextExtent];

  Cursor
    cursor;

  int
    entry,
    id,
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    height,
    width;

  unsigned long
    state;

  XColor
    color;

  XEvent
    event;

  /*
    Map Command widget.
  */
  windows->command.name="Color Edit";
  windows->command.data=4;
  (void) XCommandWidget(display,windows,ColorEditMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Make cursor.
  */
  cursor=XMakeCursor(display,windows->image.id,windows->map_info->colormap,
    resource_info->background_color,resource_info->foreground_color);
  XDefineCursor(display,windows->image.id,cursor);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",x+windows->image.x,y+windows->image.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,ColorEditMenu,&event);
        if (id < 0)
          {
            XDefineCursor(display,windows->image.id,cursor);
            continue;
          }
        switch (ColorEditCommands[id])
        {
          case ColorEditMethodCommand:
          {
            static const char
              *MethodMenu[]=
              {
                "point",
                "replace",
                "floodfill",
                "filltoborder",
                "reset",
                (char *) NULL,
              };

            /*
              Select a method from the pop-up menu.
            */
            entry=
              XMenuWidget(display,windows,ColorEditMenu[id],MethodMenu,command);
            if (entry >= 0)
              method=(PaintMethod) entry;
            break;
          }
          case ColorEditColorCommand:
          {
            char
              *ColorMenu[MaxNumberPens];

            int
              pen_number;

            /*
              Initialize menu selections.
            */
            for (i=0; i < (int) (MaxNumberPens-2); i++)
              ColorMenu[i]=resource_info->pen_colors[i];
            ColorMenu[MaxNumberPens-2]="Browser...";
            ColorMenu[MaxNumberPens-1]=(char *) NULL;
            /*
              Select a pen color from the pop-up menu.
            */
            pen_number=XMenuWidget(display,windows,ColorEditMenu[id],
              (const char **) ColorMenu,command);
            if (pen_number < 0)
              break;
            if (pen_number == (MaxNumberPens-2))
              {
                static char
                  color_name[MaxTextExtent] = "gray";

                /*
                  Select a pen color from a dialog.
                */
                resource_info->pen_colors[pen_number]=color_name;
                XColorBrowserWidget(display,windows,"Select",color_name);
                if (*color_name == '\0')
                  break;
              }
            /*
              Set pen color.
            */
            (void) XParseColor(display,windows->map_info->colormap,
              resource_info->pen_colors[pen_number],&color);
            XBestPixel(display,windows->map_info->colormap,(XColor *) NULL,
              (unsigned int) MaxColors,&color);
            windows->pixel_info->pen_colors[pen_number]=color;
            pen_id=pen_number;
            break;
          }
          case ColorEditBorderCommand:
          {
            char
              *ColorMenu[MaxNumberPens];

            int
              pen_number;

            /*
              Initialize menu selections.
            */
            for (i=0; i < (int) (MaxNumberPens-2); i++)
              ColorMenu[i]=resource_info->pen_colors[i];
            ColorMenu[MaxNumberPens-2]="Browser...";
            ColorMenu[MaxNumberPens-1]=(char *) NULL;
            /*
              Select a pen color from the pop-up menu.
            */
            pen_number=XMenuWidget(display,windows,ColorEditMenu[id],
              (const char **) ColorMenu,command);
            if (pen_number < 0)
              break;
            if (pen_number == (MaxNumberPens-2))
              {
                static char
                  color_name[MaxTextExtent] = "gray";

                /*
                  Select a pen color from a dialog.
                */
                resource_info->pen_colors[pen_number]=color_name;
                XColorBrowserWidget(display,windows,"Select",color_name);
                if (*color_name == '\0')
                  break;
              }
            /*
              Set border color.
            */
            (void) XParseColor(display,windows->map_info->colormap,
              resource_info->pen_colors[pen_number],&border_color);
            break;
          }
          case ColorEditFuzzCommand:
          {
            static char
              fuzz[MaxTextExtent];

            static const char
              *FuzzMenu[]=
              {
                "0",
                "2",
                "4",
                "8",
                "16",
                (char *) NULL,
                (char *) NULL,
              };

            /*
              Select a command from the pop-up menu.
            */
            FuzzMenu[5]="Dialog...";
            entry=XMenuWidget(display,windows,ColorEditMenu[id],FuzzMenu,
              command);
            if (entry < 0)
              break;
            if (entry != 5)
              {
                (*image)->fuzz=atoi(FuzzMenu[entry]);
                break;
              }
            (void) sprintf(fuzz,"%d",(*image)->fuzz);
            (void) XDialogWidget(display,windows,"Ok","Enter fuzz factor:",
              fuzz);
            if (*fuzz == '\0')
              break;
            (*image)->fuzz=atoi(fuzz);
            break;
          }
          case ColorEditUndoCommand:
          {
            (void) XMagickCommand(display,resource_info,windows,UndoCommand,
              image);
            break;
          }
          case ColorEditHelpCommand:
          default:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Annotation",ImageColorEditHelp);
            break;
          }
          case ColorEditDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
        }
        XDefineCursor(display,windows->image.id,cursor);
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.button != Button1)
          break;
        if ((event.xbutton.window != windows->image.id) &&
            (event.xbutton.window != windows->magnify.id))
          break;
        /*
          Exit loop.
        */
        x=event.xbutton.x;
        y=event.xbutton.y;
        (void) XMagickCommand(display,resource_info,windows,
          SaveToUndoBufferCommand,image);
        state|=UpdateConfigurationState;
        break;
      }
      case ButtonRelease:
      {
        if (event.xbutton.button != Button1)
          break;
        if ((event.xbutton.window != windows->image.id) &&
            (event.xbutton.window != windows->magnify.id))
          break;
        /*
          Update colormap information.
        */
        x=event.xbutton.x;
        y=event.xbutton.y;
        XConfigureImageColormap(display,resource_info,windows,*image);
        (void) XConfigureImage(display,resource_info,windows,*image);
        XInfoWidget(display,windows,text);
        XDefineCursor(display,windows->image.id,cursor);
        state&=(~UpdateConfigurationState);
        break;
      }
      case Expose:
        break;
      case KeyPress:
      {
        KeySym
          key_symbol;

        if (event.xkey.window == windows->magnify.id)
          {
            Window
              window;

            window=windows->magnify.id;
            while (XCheckWindowEvent(display,window,KeyPressMask,&event));
          }
        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Annotation",ImageColorEditHelp);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        break;
      }
      default:
        break;
    }
    if (event.xany.window == windows->magnify.id)
      {
        x=windows->magnify.x-windows->image.x;
        y=windows->magnify.y-windows->image.y;
      }
    x_offset=x;
    y_offset=y;
    if (state & UpdateConfigurationState)
      {
        int
          x,
          y;

        /*
          Pixel edit is relative to image configuration.
        */
        XClearArea(display,windows->image.id,x_offset,y_offset,1,1,True);
        color=windows->pixel_info->pen_colors[pen_id];
        XPutPixel(windows->image.ximage,x_offset,y_offset,color.pixel);
        width=(*image)->columns;
        height=(*image)->rows;
        x=0;
        y=0;
        if (windows->image.crop_geometry != (char *) NULL)
          (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
            &width,&height);
        x_offset=
          width*(windows->image.x+x_offset)/windows->image.ximage->width+x;
        y_offset=
          height*(windows->image.y+y_offset)/windows->image.ximage->height+y;
        if ((x_offset < 0) || (y_offset < 0))
          continue;
        if ((x_offset >= (int) (*image)->columns) ||
            (y_offset >= (int) (*image)->rows))
          continue;
        switch (method)
        {
          case PointMethod:
          default:
          {
            /*
              Update color information using point algorithm.
            */
            (*image)->class=DirectClass;
            if (!UncondenseImage(*image))
              break;
            p=(*image)->pixels+(y_offset*(*image)->columns+x_offset);
            p->red=XDownScale(color.red);
            p->green=XDownScale(color.green);
            p->blue=XDownScale(color.blue);
            break;
          }
          case ReplaceMethod:
          {
            RunlengthPacket
              target;

            /*
              Update color information using replace algorithm.
            */
            x=0;
            p=(*image)->pixels;
            for (i=0; i < (int) (*image)->packets; i++)
            {
              x+=(p->length+1);
              if (x > (int) (y_offset*(*image)->columns+x_offset))
                break;
              p++;
            }
            target=(*image)->pixels[i];
            if ((*image)->class == DirectClass)
              {
                p=(*image)->pixels;
                for (i=0; i < (int) (*image)->packets; i++)
                {
                  if (ColorMatch(*p,target,(*image)->fuzz))
                    {
                      p->red=XDownScale(color.red);
                      p->green=XDownScale(color.green);
                      p->blue=XDownScale(color.blue);
                    }
                  p++;
                }
              }
            else
              {
                for (i=0; i < (int) (*image)->colors; i++)
                  if (ColorMatch((*image)->colormap[i],target,(*image)->fuzz))
                    {
                      (*image)->colormap[i].red=XDownScale(color.red);
                      (*image)->colormap[i].green=XDownScale(color.green);
                      (*image)->colormap[i].blue=XDownScale(color.blue);
                    }
                SyncImage(*image);
              }
            break;
          }
          case FloodfillMethod:
          case FillToBorderMethod:
          {
            AnnotateInfo
              annotate_info;

            ImageInfo
              *image_info;

            RunlengthPacket
              target;

            /*
              Update color information using floodfill algorithm.
            */
            (*image)->class=DirectClass;
            if (!UncondenseImage(*image))
              break;
            target=(*image)->pixels[y_offset*(*image)->columns+x_offset];
            if (method == FillToBorderMethod)
              {
                target.red=XDownScale(border_color.red);
                target.green=XDownScale(border_color.green);
                target.blue=XDownScale(border_color.blue);
              }
            image_info=CloneImageInfo(resource_info->image_info);
            (void) CloneString(&image_info->pen,
              resource_info->pen_colors[pen_id]);
            GetAnnotateInfo(image_info,&annotate_info);
            ColorFloodfillImage(*image,&target,annotate_info.tile,x_offset,
              y_offset,method);
            DestroyAnnotateInfo(&annotate_info);
            DestroyImageInfo(image_info);
            break;
          }
          case ResetMethod:
          {
            /*
              Update color information using reset algorithm.
            */
            (*image)->class=DirectClass;
            p=(*image)->pixels;
            for (i=0; i < (int) (*image)->packets; i++)
            {
              p->red=XDownScale(color.red);
              p->green=XDownScale(color.green);
              p->blue=XDownScale(color.blue);
              p++;
            }
            break;
          }
        }
        state&=(~UpdateConfigurationState);
      }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  XSetCursorState(display,windows,False);
  XFreeCursor(display,cursor);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X C o m p o s i t e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XCompositeImage requests an image name from the user, reads
%  the image and composites it with the X window image at a location the user
%  chooses with the pointer.
%
%  The format of the XCompositeImage method is:
%
%    status=XCompositeImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XCompositeImage returns True if the image is
%      composited.  False is returned is there is a memory shortage or if the
%      image fails to be composited.
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
*/
static unsigned int XCompositeImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image *image)
{
  static char
    displacement_geometry[MaxTextExtent] = "30x30",
    filename[MaxTextExtent] = "\0";

  static const char
    *CompositeMenu[]=
    {
      "Operators",
      "Blend",
      "Displace",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static CompositeOperator
    compose = ReplaceCompositeOp;

  static const ModeType
    CompositeCommands[]=
    {
      CompositeOperatorsCommand,
      CompositeBlendCommand,
      CompositeDisplaceCommand,
      CompositeHelpCommand,
      CompositeDismissCommand
    };

  char
    text[MaxTextExtent];

  Cursor
    cursor;

  double
    blend;

  Image
    *composite_image;

  int
    id,
    x,
    y;

  RectangleInfo
    highlight_info,
    composite_info;

  unsigned int
    height,
    width;

  unsigned long
    scale_factor,
    state;

  XEvent
    event;

  /*
    Request image file name from user.
  */
  XFileBrowserWidget(display,windows,"Composite",filename);
  if (*filename == '\0')
    return(True);
  /*
    Read image.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  (void) strcpy(resource_info->image_info->filename,filename);
  composite_image=ReadImage(resource_info->image_info);
  XSetCursorState(display,windows,False);
  if (composite_image == (Image *) NULL)
    {
      XNoticeWidget(display,windows,"Unable to read image:",filename);
      return(False);
    }
  if (!composite_image->matte)
    {
      /*
        Request mask image file name from user.
      */
      XNoticeWidget(display,windows,
        "Your image does not have the required matte information.",
        "Press dismiss and choose an image to use as a mask.");
      XFileBrowserWidget(display,windows,"Composite",filename);
      if (*filename != '\0')
        {
          char
            size[MaxTextExtent];

          Image
            *mask_image;

          ImageInfo
            image_info;

          /*
            Read image.
          */
          XSetCursorState(display,windows,True);
          XCheckRefreshWindows(display,windows);
          GetImageInfo(&image_info);
          (void) strcpy(image_info.filename,filename);
          (void) CloneString(&image_info.size,size);
          FormatString(image_info.size,"%ux%u",composite_image->columns,
            composite_image->rows);
          mask_image=ReadImage(&image_info);
          XSetCursorState(display,windows,False);
          if (mask_image == (Image *) NULL)
            {
              XNoticeWidget(display,windows,"Unable to read image:",filename);
              return(False);
            }
          CompositeImage(composite_image,ReplaceMatteCompositeOp,mask_image,
            0,0);
          DestroyImage(mask_image);
          DestroyImageInfo(&image_info);
        }
    }
  /*
    Map Command widget.
  */
  windows->command.name="Composite";
  windows->command.data=1;
  (void) XCommandWidget(display,windows,CompositeMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  composite_info.x=windows->image.x+x;
  composite_info.y=windows->image.y+y;
  composite_info.width=0;
  composite_info.height=0;
  cursor=XCreateFontCursor(display,XC_ul_angle);
  XSetFunction(display,windows->image.highlight_context,GXinvert);
  blend=0.0;
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",composite_info.x,composite_info.y);
        XInfoWidget(display,windows,text);
      }
    highlight_info=composite_info;
    highlight_info.x=composite_info.x-windows->image.x;
    highlight_info.y=composite_info.y-windows->image.y;
    XHighlightRectangle(display,windows->image.id,
      windows->image.highlight_context,&highlight_info);
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    XHighlightRectangle(display,windows->image.id,
      windows->image.highlight_context,&highlight_info);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,CompositeMenu,&event);
        if (id < 0)
          continue;
        switch (CompositeCommands[id])
        {
          case CompositeOperatorsCommand:
          {
            char
              command[MaxTextExtent];

            static const char
              *OperatorMenu[]=
              {
                "Over",
                "In",
                "Out",
                "Atop",
                "Xor",
                "Plus",
                "Minus",
                "Add",
                "Subtract",
                "Difference",
                "Bumpmap",
                "Replace",
                "ReplaceRed",
                "ReplaceGreen",
                "ReplaceBlue",
                "ReplaceMatte",
                (char *) NULL,
              };

            /*
              Select a command from the pop-up menu.
            */
            compose=(CompositeOperator) (XMenuWidget(display,windows,
              CompositeMenu[id],OperatorMenu,command)+1);
            break;
          }
          case CompositeBlendCommand:
          {
            static char
              factor[MaxTextExtent] = "20.0";

            /*
              Blend the two images a given percent.
            */
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            (void) XDialogWidget(display,windows,"Blend",
              "Enter the blend factor (0.0 - 99.9%):",factor);
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            if (*factor == '\0')
              break;
            blend=atof(factor);
            compose=BlendCompositeOp;
            break;
          }
          case CompositeDisplaceCommand:
          {
            /*
              Get horizontal and vertical scale displacement geometry.
            */
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            (void) XDialogWidget(display,windows,"Displace",
              "Enter the horizontal and vertical scale:",displacement_geometry);
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            if (*displacement_geometry == '\0')
              break;
            compose=DisplaceCompositeOp;
            break;
          }
          case CompositeHelpCommand:
          {
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Compositing",ImageCompositeHelp);
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            break;
          }
          case CompositeDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
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
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        /*
          Change cursor.
        */
        composite_info.width=composite_image->columns;
        composite_info.height=composite_image->rows;
        XDefineCursor(display,windows->image.id,cursor);
        composite_info.x=windows->image.x+event.xbutton.x;
        composite_info.y=windows->image.y+event.xbutton.y;
        break;
      }
      case ButtonRelease:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Button Release: 0x%lx %u +%d+%d\n",
            event.xbutton.window,event.xbutton.button,event.xbutton.x,
            event.xbutton.y);
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        if ((composite_info.width != 0) && (composite_info.height != 0))
          {
            /*
              User has selected the location of the composite image.
            */
            composite_info.x=windows->image.x+event.xbutton.x;
            composite_info.y=windows->image.y+event.xbutton.y;
            state|=ExitState;
          }
        break;
      }
      case Expose:
        break;
      case KeyPress:
      {
        char
          command[MaxTextExtent];

        KeySym
          key_symbol;

        int
          length;

        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        length=XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        *(command+length)='\0';
        if (resource_info->debug)
          (void) fprintf(stderr,"Key press: 0x%lx (%.1024s)\n",key_symbol,
            command);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            DestroyImage(composite_image);
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Compositing",ImageCompositeHelp);
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as text cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        composite_info.x=windows->image.x+x;
        composite_info.y=windows->image.y+y;
        break;
      }
      default:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Event type: %d\n",event.type);
        break;
      }
    }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  XSetFunction(display,windows->image.highlight_context,GXcopy);
  XSetCursorState(display,windows,False);
  XFreeCursor(display,cursor);
  if (state & EscapeState)
    return(True);
  /*
    Image compositing is relative to image configuration.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  scale_factor=UpShift(width)/windows->image.ximage->width;
  composite_info.x+=x;
  composite_info.x=DownShift(composite_info.x*scale_factor);
  composite_info.width=DownShift(composite_info.width*scale_factor);
  scale_factor=UpShift(height)/windows->image.ximage->height;
  composite_info.y+=y;
  composite_info.y=DownShift(composite_info.y*scale_factor);
  composite_info.height=DownShift(composite_info.height*scale_factor);
  if ((composite_info.width != composite_image->columns) ||
      (composite_info.height != composite_image->rows))
    {
      Image
        *zoomed_image;

      /*
        Scale composite image.
      */
      zoomed_image=ZoomImage(composite_image,composite_info.width,
        composite_info.height);
      DestroyImage(composite_image);
      if (zoomed_image == (Image *) NULL)
        {
          XSetCursorState(display,windows,False);
          return(False);
        }
      composite_image=zoomed_image;
    }
  if (compose == DisplaceCompositeOp)
    composite_image->geometry=displacement_geometry;
  if (blend != 0.0)
    {
      register int
        i;

      register RunlengthPacket
        *p;

      unsigned short
        index;

      /*
        Create mattes for blending.
      */
      index=(unsigned short) (((int) DownScale(MaxRGB)*blend)/100);
      MatteImage(composite_image);
      index=(unsigned short)
        ((int) DownScale(MaxRGB)-((int) DownScale(MaxRGB)*blend)/100);
      image->class=DirectClass;
      image->matte=True;
      p=image->pixels;
      for (i=0; i < (int) image->packets; i++)
      {
        p->index=index;
        p++;
      }
    }
  /*
    Composite image with X Image window.
  */
  CompositeImage(image,compose,composite_image,composite_info.x,
    composite_info.y);
  DestroyImage(composite_image);
  XSetCursorState(display,windows,False);
  /*
    Update image configuration.
  */
  XConfigureImageColormap(display,resource_info,windows,image);
  (void) XConfigureImage(display,resource_info,windows,image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X C o n f i g u r e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XConfigureImage creates a new X image.  It also notifies the
%  window manager of the new image size and configures the transient widows.
%
%  The format of the XConfigureImage method is:
%
%    status=XConfigureImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XConfigureImage returns True if the window is
%      resized.  False is returned is there is a memory shortage or if the
%      window fails to resize.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XConfigureImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image *image)
{
  char
    geometry[MaxTextExtent];

  int
    x,
    y;

  unsigned int
    height,
    mask,
    stasis,
    status,
    width;

  XSizeHints
    *size_hints;

  XWindowChanges
    window_changes;

  /*
    Dismiss if window dimensions are zero.
  */
  width=windows->image.window_changes.width;
  height=windows->image.window_changes.height;
  if (resource_info->debug)
    (void) fprintf(stderr,"Configure Image: %dx%d=>%ux%u\n",
      windows->image.ximage->width,windows->image.ximage->height,width,height);
  if ((width*height) == 0)
    return(True);
  /*
    Resize image to fit Image window dimensions.
  */
  XSetCursorState(display,windows,True);
  XFlush(display);
  stasis=((int) width == windows->image.ximage->width) &&
    ((int) height == windows->image.ximage->height);
  windows->magnify.x=width*windows->magnify.x/windows->image.ximage->width;
  windows->magnify.y=height*windows->magnify.y/windows->image.ximage->height;
  windows->image.x=width*windows->image.x/windows->image.ximage->width;
  windows->image.y=height*windows->image.y/windows->image.ximage->height;
  status=XMakeImage(display,resource_info,&windows->image,image,width,height);
  if (status == False)
    XNoticeWidget(display,windows,"Unable to configure X image:",
      windows->image.name);
  /*
    Notify window manager of the new configuration.
  */
  FormatString(geometry,"%ux%u+0+0>!",
    XDisplayWidth(display,windows->image.screen),
    XDisplayHeight(display,windows->image.screen));
  (void) ParseImageGeometry(geometry,&x,&y,&width,&height);
  window_changes.width=width;
  window_changes.height=height;
  mask=CWWidth | CWHeight;
  if (resource_info->backdrop)
    {
      mask|=CWX | CWY;
      window_changes.x=
        (XDisplayWidth(display,windows->image.screen) >> 1)-(width >> 1);
      window_changes.y=
        (XDisplayHeight(display,windows->image.screen) >> 1)-(height >> 1);
    }
  XReconfigureWMWindow(display,windows->image.id,windows->image.screen,mask,
    &window_changes);
  if (image->matte)
    XClearWindow(display,windows->image.id);
  if (stasis)
    XRefreshWindow(display,&windows->image,(XEvent *) NULL);
  /*
    Update Magnify window configuration.
  */
  if (windows->magnify.mapped)
    XMakeMagnifyImage(display,windows);
  /*
    Update pan window configuration.
  */
  windows->pan.crop_geometry=windows->image.crop_geometry;
  XBestIconSize(display,&windows->pan,image);
  while ((windows->pan.width < 96) && (windows->pan.height < 96))
  {
    windows->pan.width<<=1;
    windows->pan.height<<=1;
  }
  if (windows->pan.geometry != (char *) NULL)
    (void) XParseGeometry(windows->pan.geometry,&windows->pan.x,&windows->pan.y,
      &windows->pan.width,&windows->pan.height);
  window_changes.width=windows->pan.width;
  window_changes.height=windows->pan.height;
  size_hints=XAllocSizeHints();
  if (size_hints != (XSizeHints *) NULL)
    {
      /*
        Set new size hints.
      */
      size_hints->flags=PSize | PMinSize | PMaxSize;
      size_hints->width=window_changes.width;
      size_hints->height=window_changes.height;
      size_hints->min_width=size_hints->width;
      size_hints->min_height=size_hints->height;
      size_hints->max_width=size_hints->width;
      size_hints->max_height=size_hints->height;
      XSetNormalHints(display,windows->pan.id,size_hints);
      XFree((void *) size_hints);
    }
  XReconfigureWMWindow(display,windows->pan.id,windows->pan.screen,CWWidth |
    CWHeight,&window_changes);
  if (windows->pan.mapped)
    XMakePanImage(display,resource_info,windows,image);
  /*
    Update icon window configuration.
  */
  windows->icon.crop_geometry=windows->image.crop_geometry;
  XBestIconSize(display,&windows->icon,image);
  window_changes.width=windows->icon.width;
  window_changes.height=windows->icon.height;
  XReconfigureWMWindow(display,windows->icon.id,windows->icon.screen,
    CWWidth | CWHeight,&window_changes);
  XSetCursorState(display,windows,False);
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X C r o p I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XCropImage allows the user to select a region of the image and
%  crop, copy, or cut it.  For copy or cut, the image can subsequently be
%  composited onto the image with XPasteImage.
%
%  The format of the XCropImage method is:
%
%    status=XCropImage(display,resource_info,windows,image,mode)
%
%  A description of each parameter follows:
%
%    o status: Method XCropImage returns True if the image is
%      copied.  False is returned is there is a memory shortage or if the
%      image fails to be copied.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%    o mode: This unsigned value specified whether the image should be
%      cropped, copied, or cut.
%
%
*/
static unsigned int XCropImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image,const ClipboardMode mode)
{
  static const char
    *CropModeMenu[]=
    {
      "Help",
      "Dismiss",
      (char *) NULL
    },
    *RectifyModeMenu[]=
    {
      "Crop",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static const ModeType
    CropCommands[]=
    {
      CropHelpCommand,
      CropDismissCommand
    },
    RectifyCommands[]=
    {
      RectifyCopyCommand,
      RectifyHelpCommand,
      RectifyDismissCommand
    };

  char
    command[MaxTextExtent],
    text[MaxTextExtent];

  Cursor
    cursor;

  int
    id,
    x,
    y;

  KeySym
    key_symbol;

  Image
    *crop_image;

  RectangleInfo
    crop_info,
    highlight_info;

  register RunlengthPacket
    *p;

  unsigned int
    height,
    width;

  unsigned long
    scale_factor,
    state;

  XEvent
    event;

  /*
    Map Command widget.
  */
  switch (mode)
  {
    case CopyMode:
    {
      windows->command.name="Copy";
      break;
    }
    case CropMode:
    {
      windows->command.name="Crop";
      break;
    }
    case CutMode:
    {
      windows->command.name="Cut";
      break;
    }
  }
  RectifyModeMenu[0]=windows->command.name;
  windows->command.data=0;
  (void) XCommandWidget(display,windows,CropModeMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  crop_info.x=windows->image.x+x;
  crop_info.y=windows->image.y+y;
  crop_info.width=0;
  crop_info.height=0;
  cursor=XCreateFontCursor(display,XC_fleur);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",crop_info.x,crop_info.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,CropModeMenu,&event);
        if (id < 0)
          continue;
        switch (CropCommands[id])
        {
          case CropHelpCommand:
          {
            switch (mode)
            {
              case CopyMode:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Copying",ImageCopyHelp);
                break;
              }
              case CropMode:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Cropping",ImageCropHelp);
                break;
              }
              case CutMode:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Cutting",ImageCutHelp);
                break;
              }
            }
            break;
          }
          case CropDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        /*
          Note first corner of cropping rectangle-- exit loop.
        */
        XDefineCursor(display,windows->image.id,cursor);
        crop_info.x=windows->image.x+event.xbutton.x;
        crop_info.y=windows->image.y+event.xbutton.y;
        state|=ExitState;
        break;
      }
      case ButtonRelease:
        break;
      case Expose:
        break;
      case KeyPress:
      {
        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            switch (mode)
            {
              case CopyMode:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Copying",ImageCopyHelp);
                break;
              }
              case CropMode:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Cropping",ImageCropHelp);
                break;
              }
              case CutMode:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Cutting",ImageCutHelp);
                break;
              }
            }
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as text cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        crop_info.x=windows->image.x+x;
        crop_info.y=windows->image.y+y;
        break;
      }
      default:
        break;
    }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  if (state & EscapeState)
    {
      /*
        User want to exit without cropping.
      */
      XWithdrawWindow(display,windows->info.id,windows->info.screen);
      XFreeCursor(display,cursor);
      return(True);
    }
  XSetFunction(display,windows->image.highlight_context,GXinvert);
  do
  {
    /*
      Size rectangle as pointer moves until the mouse button is released.
    */
    x=crop_info.x;
    y=crop_info.y;
    crop_info.width=0;
    crop_info.height=0;
    state=DefaultState;
    do
    {
      highlight_info=crop_info;
      highlight_info.x=crop_info.x-windows->image.x;
      highlight_info.y=crop_info.y-windows->image.y;
      if ((highlight_info.width > 3) && (highlight_info.height > 3))
        {
          /*
            Display info and draw cropping rectangle.
          */
          if (!windows->info.mapped)
            XMapWindow(display,windows->info.id);
          FormatString(text," %ux%u%+d%+d",crop_info.width,crop_info.height,
            crop_info.x,crop_info.y);
          XInfoWidget(display,windows,text);
          XHighlightRectangle(display,windows->image.id,
            windows->image.highlight_context,&highlight_info);
        }
      else
        if (windows->info.mapped)
          XWithdrawWindow(display,windows->info.id,windows->info.screen);
      /*
        Wait for next event.
      */
      XScreenEvent(display,windows,&event);
      if ((highlight_info.width > 3) && (highlight_info.height > 3))
        XHighlightRectangle(display,windows->image.id,
          windows->image.highlight_context,&highlight_info);
      switch (event.type)
      {
        case ButtonPress:
        {
          crop_info.x=windows->image.x+event.xbutton.x;
          crop_info.y=windows->image.y+event.xbutton.y;
          break;
        }
        case ButtonRelease:
        {
          /*
            User has committed to cropping rectangle.
          */
          crop_info.x=windows->image.x+event.xbutton.x;
          crop_info.y=windows->image.y+event.xbutton.y;
          XSetCursorState(display,windows,False);
          state|=ExitState;
          if (Latin1Compare(windows->command.name,"Rectify") == 0)
            break;
          windows->command.name="Rectify";
          windows->command.data=0;
          (void) XCommandWidget(display,windows,RectifyModeMenu,
            (XEvent *) NULL);
          break;
        }
        case Expose:
          break;
        case MotionNotify:
        {
          crop_info.x=windows->image.x+event.xmotion.x;
          crop_info.y=windows->image.y+event.xmotion.y;
        }
        default:
          break;
      }
      if (((crop_info.x != x) && (crop_info.y != y)) || (state & ExitState))
        {
          /*
            Check boundary conditions.
          */
          if (crop_info.x < 0)
            crop_info.x=0;
          else
            if (crop_info.x > windows->image.ximage->width)
              crop_info.x=windows->image.ximage->width;
          if (crop_info.x < x)
            crop_info.width=(unsigned int) (x-crop_info.x);
          else
            {
              crop_info.width=(unsigned int) (crop_info.x-x);
              crop_info.x=x;
            }
          if (crop_info.y < 0)
            crop_info.y=0;
          else
            if (crop_info.y > windows->image.ximage->height)
              crop_info.y=windows->image.ximage->height;
          if (crop_info.y < y)
            crop_info.height=(unsigned int) (y-crop_info.y);
          else
            {
              crop_info.height=(unsigned int) (crop_info.y-y);
              crop_info.y=y;
            }
        }
    } while (!(state & ExitState));
    /*
      Wait for user to grab a corner of the rectangle or press return.
    */
    state=DefaultState;
    do
    {
      if (windows->info.mapped)
        {
          /*
            Display pointer position.
          */
          FormatString(text," %ux%u%+d%+d",crop_info.width,crop_info.height,
            crop_info.x,crop_info.y);
          XInfoWidget(display,windows,text);
        }
      highlight_info=crop_info;
      highlight_info.x=crop_info.x-windows->image.x;
      highlight_info.y=crop_info.y-windows->image.y;
      if ((highlight_info.width <= 3) || (highlight_info.height <= 3))
        {
          state|=EscapeState;
          state|=ExitState;
          break;
        }
      XHighlightRectangle(display,windows->image.id,
        windows->image.highlight_context,&highlight_info);
      XScreenEvent(display,windows,&event);
      if (event.xany.window == windows->command.id)
        {
          /*
            Select a command from the Command widget.
          */
          XSetFunction(display,windows->image.highlight_context,GXcopy);
          id=XCommandWidget(display,windows,RectifyModeMenu,&event);
          XSetFunction(display,windows->image.highlight_context,GXinvert);
          XHighlightRectangle(display,windows->image.id,
            windows->image.highlight_context,&highlight_info);
          if (id >= 0)
            switch (RectifyCommands[id])
            {
              case RectifyCopyCommand:
              {
                state|=ExitState;
                break;
              }
              case RectifyHelpCommand:
              {
                XSetFunction(display,windows->image.highlight_context,GXcopy);
                switch (mode)
                {
                  case CopyMode:
                  {
                    XTextViewWidget(display,resource_info,windows,False,
                      "Help Viewer - Image Copying",ImageCopyHelp);
                    break;
                  }
                  case CropMode:
                  {
                    XTextViewWidget(display,resource_info,windows,False,
                      "Help Viewer - Image Cropping",ImageCropHelp);
                    break;
                  }
                  case CutMode:
                  {
                    XTextViewWidget(display,resource_info,windows,False,
                      "Help Viewer - Image Cutting",ImageCutHelp);
                    break;
                  }
                }
                XSetFunction(display,windows->image.highlight_context,GXinvert);
                break;
              }
              case RectifyDismissCommand:
              {
                /*
                  Prematurely exit.
                */
                state|=EscapeState;
                state|=ExitState;
                break;
              }
              default:
                break;
            }
          continue;
        }
      XHighlightRectangle(display,windows->image.id,
        windows->image.highlight_context,&highlight_info);
      switch (event.type)
      {
        case ButtonPress:
        {
          if (event.xbutton.button != Button1)
            break;
          if (event.xbutton.window != windows->image.id)
            break;
          x=windows->image.x+event.xbutton.x;
          y=windows->image.y+event.xbutton.y;
          if ((x < (int) (crop_info.x+RoiDelta)) &&
              (x > (int) (crop_info.x-RoiDelta)) &&
              (y < (int) (crop_info.y+RoiDelta)) &&
              (y > (int) (crop_info.y-RoiDelta)))
            {
              crop_info.x=crop_info.x+crop_info.width;
              crop_info.y=crop_info.y+crop_info.height;
              state|=UpdateConfigurationState;
              break;
            }
          if ((x < (int) (crop_info.x+RoiDelta)) &&
              (x > (int) (crop_info.x-RoiDelta)) &&
              (y < (int) (crop_info.y+crop_info.height+RoiDelta)) &&
              (y > (int) (crop_info.y+crop_info.height-RoiDelta)))
            {
              crop_info.x=crop_info.x+crop_info.width;
              state|=UpdateConfigurationState;
              break;
            }
          if ((x < (int) (crop_info.x+crop_info.width+RoiDelta)) &&
              (x > (int) (crop_info.x+crop_info.width-RoiDelta)) &&
              (y < (int) (crop_info.y+RoiDelta)) &&
              (y > (int) (crop_info.y-RoiDelta)))
            {
              crop_info.y=crop_info.y+crop_info.height;
              state|=UpdateConfigurationState;
              break;
            }
          if ((x < (int) (crop_info.x+crop_info.width+RoiDelta)) &&
              (x > (int) (crop_info.x+crop_info.width-RoiDelta)) &&
              (y < (int) (crop_info.y+crop_info.height+RoiDelta)) &&
              (y > (int) (crop_info.y+crop_info.height-RoiDelta)))
            {
              state|=UpdateConfigurationState;
              break;
            }
        }
        case ButtonRelease:
        {
          if (event.xbutton.window == windows->pan.id)
            if ((highlight_info.x != crop_info.x-windows->image.x) ||
                (highlight_info.y != crop_info.y-windows->image.y))
              XHighlightRectangle(display,windows->image.id,
                windows->image.highlight_context,&highlight_info);
          break;
        }
        case Expose:
        {
          if (event.xexpose.window == windows->image.id)
            if (event.xexpose.count == 0)
              {
                event.xexpose.x=highlight_info.x;
                event.xexpose.y=highlight_info.y;
                event.xexpose.width=highlight_info.width;
                event.xexpose.height=highlight_info.height;
                XRefreshWindow(display,&windows->image,&event);
              }
          if (event.xexpose.window == windows->info.id)
            if (event.xexpose.count == 0)
              XInfoWidget(display,windows,text);
          break;
        }
        case KeyPress:
        {
          if (event.xkey.window != windows->image.id)
            break;
          /*
            Respond to a user key press.
          */
          (void) XLookupString((XKeyEvent *) &event.xkey,command,
            sizeof(command),&key_symbol,(XComposeStatus *) NULL);
          switch (key_symbol)
          {
            case XK_Escape:
            case XK_F20:
              state|=EscapeState;
            case XK_Return:
            {
              state|=ExitState;
              break;
            }
            case XK_F1:
            case XK_Help:
            {
              XSetFunction(display,windows->image.highlight_context,GXcopy);
              switch (mode)
              {
                case CopyMode:
                {
                  XTextViewWidget(display,resource_info,windows,False,
                    "Help Viewer - Image Copying",ImageCopyHelp);
                  break;
                }
                case CropMode:
                {
                  XTextViewWidget(display,resource_info,windows,False,
                    "Help Viewer - Image Cropping",ImageCropHelp);
                  break;
                }
                case CutMode:
                {
                  XTextViewWidget(display,resource_info,windows,False,
                    "Help Viewer - Image Cutting",ImageCutHelp);
                  break;
                }
              }
              XSetFunction(display,windows->image.highlight_context,GXinvert);
              break;
            }
            default:
            {
              XBell(display,0);
              break;
            }
          }
          break;
        }
        case KeyRelease:
          break;
        case MotionNotify:
        {
          /*
            Map and unmap Info widget as text cursor crosses its boundaries.
          */
          x=event.xmotion.x;
          y=event.xmotion.y;
          if (windows->info.mapped)
            {
              if ((x < (int) (windows->info.x+windows->info.width)) &&
                  (y < (int) (windows->info.y+windows->info.height)))
                XWithdrawWindow(display,windows->info.id,windows->info.screen);
            }
          else
            if ((x > (int) (windows->info.x+windows->info.width)) ||
                (y > (int) (windows->info.y+windows->info.height)))
              XMapWindow(display,windows->info.id);
          break;
        }
        default:
          break;
      }
      if (state & UpdateConfigurationState)
        {
          XPutBackEvent(display,&event);
          XDefineCursor(display,windows->image.id,cursor);
          break;
        }
    } while (!(state & ExitState));
  } while (!(state & ExitState));
  XSetFunction(display,windows->image.highlight_context,GXcopy);
  XSetCursorState(display,windows,False);
  if (state & EscapeState)
    return(True);
  if (mode == CropMode)
    if (((int) crop_info.width != windows->image.ximage->width) ||
        ((int) crop_info.height != windows->image.ximage->height))
      {
        /*
          Reconfigure Image window as defined by cropping rectangle.
        */
        XSetCropGeometry(display,windows,&crop_info,image);
        windows->image.window_changes.width=crop_info.width;
        windows->image.window_changes.height=crop_info.height;
        (void) XConfigureImage(display,resource_info,windows,image);
        return(True);
      }
  /*
    Copy image before applying image transforms.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  scale_factor=UpShift(width)/windows->image.ximage->width;
  crop_info.x+=x;
  crop_info.x=DownShift(crop_info.x*scale_factor);
  crop_info.width=DownShift(crop_info.width*scale_factor);
  scale_factor=UpShift(height)/windows->image.ximage->height;
  crop_info.y+=y;
  crop_info.y=DownShift(crop_info.y*scale_factor);
  crop_info.height=DownShift(crop_info.height*scale_factor);
  crop_image=CropImage(image,&crop_info);
  XSetCursorState(display,windows,False);
  if (crop_image == (Image *) NULL)
    return(False);
  if (resource_info->copy_image != (Image *) NULL)
    DestroyImage(resource_info->copy_image);
  resource_info->copy_image=crop_image;
  if (mode == CopyMode)
    {
      (void) XConfigureImage(display,resource_info,windows,image);
      return(True);
    }
  /*
    Cut image.
  */
  image->class=DirectClass;
  if (!image->matte)
    MatteImage(image);
  if (UncondenseImage(image))
    for (y=0; y < (int) crop_info.height; y++)
    {
      p=image->pixels+(crop_info.y+y)*image->columns+crop_info.x;
      for (x=0; x < (int) crop_info.width; x++)
      {
        p->index=Transparent;
        p++;
      }
    }
  /*
    Update image configuration.
  */
  XConfigureImageColormap(display,resource_info,windows,image);
  (void) XConfigureImage(display,resource_info,windows,image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X D r a w I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XDrawEditImage draws a graphic primitive (point, line, rectangle,
%  etc.) on the image.
%
%  The format of the XDrawEditImage method is:
%
%    status=XDrawEditImage(display,resource_info,windows,degrees,image)
%
%  A description of each parameter follows:
%
%    o status: Method XDrawEditImage return True if the image is drawn
%      upon.  False is returned is there is a memory shortage or if the
%      image cannot be drawn on.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XDrawEditImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image **image)
{
  static const char
    *DrawMenu[]=
    {
      "Primitive",
      "Color",
      "Stipple",
      "Width",
      "Undo",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static PrimitiveType
    primitive = PointPrimitive;

  static const ModeType
    DrawCommands[]=
    {
      DrawPrimitiveCommand,
      DrawColorCommand,
      DrawStippleCommand,
      DrawWidthCommand,
      DrawUndoCommand,
      DrawHelpCommand,
      DrawDismissCommand
    };

  static Pixmap
    stipple = (Pixmap) NULL;

  static unsigned int
    pen_id = 0,
    line_width = 1;

  char
    command[MaxTextExtent],
    text[MaxTextExtent];

  Cursor
    cursor;

  double
    degrees;

  int
    entry,
    id,
    number_coordinates,
    x,
    y;

  RectangleInfo
    rectangle_info;

  register int
    i;

  unsigned int
    distance,
    height,
    max_coordinates,
    status,
    width;

  unsigned long
    state;

  Window
    root_window;

  XDrawInfo
    draw_info;

  XEvent
    event;

  XPoint
    *coordinate_info;

  XSegment
    line_info;

  /*
    Allocate polygon info.
  */
  max_coordinates=2048;
  coordinate_info=(XPoint *) AllocateMemory(max_coordinates*sizeof(XPoint));
  if (coordinate_info == (XPoint *) NULL)
    {
      MagickWarning(ResourceLimitWarning,"Unable to draw on image",
        "Memory allocation failed");
      return(False);
    }
  /*
    Map Command widget.
  */
  windows->command.name="Draw";
  windows->command.data=4;
  (void) XCommandWidget(display,windows,DrawMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Wait for first button press.
  */
  root_window=XRootWindow(display,XDefaultScreen(display));
  draw_info.stencil=OpaqueStencil;
  status=True;
  cursor=XCreateFontCursor(display,XC_tcross);
  for ( ; ; )
  {
    XQueryPosition(display,windows->image.id,&x,&y);
    XSelectInput(display,windows->image.id,
      windows->image.attributes.event_mask | PointerMotionMask);
    XDefineCursor(display,windows->image.id,cursor);
    state=DefaultState;
    do
    {
      if (windows->info.mapped)
        {
          /*
            Display pointer position.
          */
          FormatString(text," %+d%+d ",x+windows->image.x,y+windows->image.y);
          XInfoWidget(display,windows,text);
        }
      /*
        Wait for next event.
      */
      XScreenEvent(display,windows,&event);
      if (event.xany.window == windows->command.id)
        {
          /*
            Select a command from the Command widget.
          */
          id=XCommandWidget(display,windows,DrawMenu,&event);
          if (id < 0)
            continue;
          switch (DrawCommands[id])
          {
            case DrawPrimitiveCommand:
            {
              static const char
                *Primitives[]=
                {
                  "point",
                  "line",
                  "rectangle",
                  "fill rectangle",
                  "circle",
                  "fill circle",
                  "ellipse",
                  "fill ellipse",
                  "polygon",
                  "fill polygon",
                  (char *) NULL,
                };

              /*
                Select a command from the pop-up menu.
              */
              primitive=(PrimitiveType) (XMenuWidget(display,windows,
                DrawMenu[id],Primitives,command)+1);
              break;
            }
            case DrawColorCommand:
            {
              char
                *ColorMenu[MaxNumberPens+1];

              int
                pen_number;

              unsigned int
                transparent;

              XColor
                color;

              /*
                Initialize menu selections.
              */
              for (i=0; i < (int) (MaxNumberPens-2); i++)
                ColorMenu[i]=resource_info->pen_colors[i];
              ColorMenu[MaxNumberPens-2]="transparent";
              ColorMenu[MaxNumberPens-1]="Browser...";
              ColorMenu[MaxNumberPens]=(char *) NULL;
              /*
                Select a pen color from the pop-up menu.
              */
              pen_number=XMenuWidget(display,windows,DrawMenu[id],
                (const char **) ColorMenu,command);
              if (pen_number < 0)
                break;
              transparent=pen_number == (MaxNumberPens-2);
              if (transparent)
                {
                  draw_info.stencil=TransparentStencil;
                  break;
                }
              if (pen_number == (MaxNumberPens-1))
                {
                  static char
                    color_name[MaxTextExtent] = "gray";

                  /*
                    Select a pen color from a dialog.
                  */
                  resource_info->pen_colors[pen_number]=color_name;
                  XColorBrowserWidget(display,windows,"Select",color_name);
                  if (*color_name == '\0')
                    break;
                }
              /*
                Set pen color.
              */
              (void) XParseColor(display,windows->map_info->colormap,
                resource_info->pen_colors[pen_number],&color);
              XBestPixel(display,windows->map_info->colormap,(XColor *) NULL,
                (unsigned int) MaxColors,&color);
              windows->pixel_info->pen_colors[pen_number]=color;
              pen_id=pen_number;
              draw_info.stencil=OpaqueStencil;
              break;
            }
            case DrawStippleCommand:
            {
              Image
                *stipple_image;

              ImageInfo
                image_info;

              static char
                filename[MaxTextExtent] = "\0";

              static const char
                *StipplesMenu[]=
                {
                  "Brick",
                  "Diagonal",
                  "Scales",
                  "Vertical",
                  "Wavy",
                  "Translucent",
                  "Opaque",
                  (char *) NULL,
                  (char *) NULL,
                };

              /*
                Select a command from the pop-up menu.
              */
              StipplesMenu[7]="Open...";
              entry=XMenuWidget(display,windows,DrawMenu[id],StipplesMenu,
                command);
              if (entry < 0)
                break;
              if (stipple != (Pixmap) NULL)
                XFreePixmap(display,stipple);
              stipple=(Pixmap) NULL;
              if (entry == 6)
                break;
              if (entry != 7)
                {
                  switch (entry)
                  {
                    case 0:
                    {
                      stipple=XCreateBitmapFromData(display,root_window,
                        (char *) BricksBitmap,BricksWidth,BricksHeight);
                      break;
                    }
                    case 1:
                    {
                      stipple=XCreateBitmapFromData(display,root_window,
                        (char *) DiagonalBitmap,DiagonalWidth,DiagonalHeight);
                      break;
                    }
                    case 2:
                    {
                      stipple=XCreateBitmapFromData(display,root_window,
                        (char *) ScalesBitmap,ScalesWidth,ScalesHeight);
                      break;
                    }
                    case 3:
                    {
                      stipple=XCreateBitmapFromData(display,root_window,
                        (char *) VerticalBitmap,VerticalWidth,VerticalHeight);
                      break;
                    }
                    case 4:
                    {
                      stipple=XCreateBitmapFromData(display,root_window,
                        (char *) WavyBitmap,WavyWidth,WavyHeight);
                      break;
                    }
                    case 5:
                    default:
                    {
                      stipple=XCreateBitmapFromData(display,root_window,
                        (char *) HighlightBitmap,HighlightWidth,
                        HighlightHeight);
                      break;
                    }
                  }
                  break;
                }
              XFileBrowserWidget(display,windows,"Stipple",filename);
              if (*filename == '\0')
                break;
              /*
                Read image.
              */
              XSetCursorState(display,windows,True);
              XCheckRefreshWindows(display,windows);
              GetImageInfo(&image_info);
              (void) strcpy(image_info.filename,filename);
              stipple_image=ReadImage(&image_info);
              XSetCursorState(display,windows,False);
              if (stipple_image == (Image *) NULL)
                {
                  XNoticeWidget(display,windows,"Unable to read image:",
                    filename);
                  break;
                }
              TemporaryFilename(filename);
              FormatString(stipple_image->filename,"xbm:%.1024s",filename);
              status=WriteImage(&image_info,stipple_image);
              DestroyImage(stipple_image);
              DestroyImageInfo(&image_info);
              status=XReadBitmapFile(display,root_window,filename,&width,
                &height,&stipple,&x,&y);
              (void) remove(filename);
              if (status != BitmapSuccess)
                XNoticeWidget(display,windows,"Unable to read X bitmap image:",
                  filename);
              break;
            }
            case DrawWidthCommand:
            {
              static char
                width[MaxTextExtent] = "3";

              static const char
                *WidthsMenu[]=
                {
                  "1",
                  "2",
                  "4",
                  "8",
                  "16",
                  (char *) NULL,
                  (char *) NULL,
                };

              /*
                Select a command from the pop-up menu.
              */
              WidthsMenu[5]="Dialog...";
              entry=XMenuWidget(display,windows,DrawMenu[id],WidthsMenu,
                command);
              if (entry < 0)
                break;
              if (entry != 5)
                {
                  line_width=atoi(WidthsMenu[entry]);
                  break;
                }
              (void) XDialogWidget(display,windows,"Ok","Enter line width:",
                width);
              if (*width == '\0')
                break;
              line_width=atoi(width);
              break;
            }
            case DrawUndoCommand:
            {
              (void) XMagickCommand(display,resource_info,windows,UndoCommand,
                image);
              break;
            }
            case DrawHelpCommand:
            {
              XTextViewWidget(display,resource_info,windows,False,
                "Help Viewer - Image Rotation",ImageDrawHelp);
              XDefineCursor(display,windows->image.id,cursor);
              break;
            }
            case DrawDismissCommand:
            {
              /*
                Prematurely exit.
              */
              state|=EscapeState;
              state|=ExitState;
              break;
            }
            default:
              break;
          }
          XDefineCursor(display,windows->image.id,cursor);
          continue;
        }
      switch (event.type)
      {
        case ButtonPress:
        {
          if (event.xbutton.button != Button1)
            break;
          if (event.xbutton.window != windows->image.id)
            break;
          /*
            Exit loop.
          */
          x=event.xbutton.x;
          y=event.xbutton.y;
          state|=ExitState;
          break;
        }
        case ButtonRelease:
          break;
        case Expose:
          break;
        case KeyPress:
        {
          KeySym
            key_symbol;

          if (event.xkey.window != windows->image.id)
            break;
          /*
            Respond to a user key press.
          */
          (void) XLookupString((XKeyEvent *) &event.xkey,command,
            sizeof(command),&key_symbol,(XComposeStatus *) NULL);
          switch (key_symbol)
          {
            case XK_Escape:
            case XK_F20:
            {
              /*
                Prematurely exit.
              */
              state|=EscapeState;
              state|=ExitState;
              break;
            }
            case XK_F1:
            case XK_Help:
            {
              XTextViewWidget(display,resource_info,windows,False,
                "Help Viewer - Image Rotation",ImageDrawHelp);
              break;
            }
            default:
            {
              XBell(display,0);
              break;
            }
          }
          break;
        }
        case MotionNotify:
        {
          /*
            Map and unmap Info widget as text cursor crosses its boundaries.
          */
          x=event.xmotion.x;
          y=event.xmotion.y;
          if (windows->info.mapped)
            {
              if ((x < (int) (windows->info.x+windows->info.width)) &&
                  (y < (int) (windows->info.y+windows->info.height)))
                XWithdrawWindow(display,windows->info.id,windows->info.screen);
            }
          else
            if ((x > (int) (windows->info.x+windows->info.width)) ||
                (y > (int) (windows->info.y+windows->info.height)))
              XMapWindow(display,windows->info.id);
          break;
        }
      }
    } while (!(state & ExitState));
    XSelectInput(display,windows->image.id,
      windows->image.attributes.event_mask);
    XWithdrawWindow(display,windows->info.id,windows->info.screen);
    if (state & EscapeState)
      break;
    /*
      Draw primitive as pointer moves until the button is released.
    */
    distance=0;
    degrees=0.0;
    line_info.x1=x;
    line_info.y1=y;
    line_info.x2=x;
    line_info.y2=y;
    rectangle_info.x=x;
    rectangle_info.y=y;
    rectangle_info.width=0;
    rectangle_info.height=0;
    number_coordinates=1;
    coordinate_info->x=x;
    coordinate_info->y=y;
    XSetFunction(display,windows->image.highlight_context,GXinvert);
    state=DefaultState;
    do
    {
      switch (primitive)
      {
        case PointPrimitive:
        default:
        {
          if (number_coordinates > 1)
            {
              XDrawLines(display,windows->image.id,
                windows->image.highlight_context,coordinate_info,
                number_coordinates,CoordModeOrigin);
              FormatString(text," %+d%+d",
                coordinate_info[number_coordinates-1].x,
                coordinate_info[number_coordinates-1].y);
              XInfoWidget(display,windows,text);
            }
          break;
        }
        case LinePrimitive:
        {
          if (distance > 9)
            {
              /*
                Display angle of the line.
              */
              degrees=RadiansToDegrees(-atan2((double) (line_info.y2-
                line_info.y1),(double) (line_info.x2-line_info.x1)));
              FormatString(text," %.2f",degrees);
              XInfoWidget(display,windows,text);
              XHighlightLine(display,windows->image.id,
                windows->image.highlight_context,&line_info);
            }
          else
            if (windows->info.mapped)
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          break;
        }
        case RectanglePrimitive:
        case FillRectanglePrimitive:
        {
          if ((rectangle_info.width > 3) && (rectangle_info.height > 3))
            {
              /*
                Display info and draw drawing rectangle.
              */
              FormatString(text," %ux%u%+d%+d",rectangle_info.width,
                rectangle_info.height,rectangle_info.x,rectangle_info.y);
              XInfoWidget(display,windows,text);
              XHighlightRectangle(display,windows->image.id,
                windows->image.highlight_context,&rectangle_info);
            }
          else
            if (windows->info.mapped)
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          break;
        }
        case CirclePrimitive:
        case FillCirclePrimitive:
        case EllipsePrimitive:
        case FillEllipsePrimitive:
        {
          if ((rectangle_info.width > 3) && (rectangle_info.height > 3))
            {
              /*
                Display info and draw drawing rectangle.
              */
              FormatString(text," %ux%u%+d%+d",rectangle_info.width,
                rectangle_info.height,rectangle_info.x,rectangle_info.y);
              XInfoWidget(display,windows,text);
              XHighlightEllipse(display,windows->image.id,
                windows->image.highlight_context,&rectangle_info);
            }
          else
            if (windows->info.mapped)
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          break;
        }
        case PolygonPrimitive:
        case FillPolygonPrimitive:
        {
          if (number_coordinates > 1)
            XDrawLines(display,windows->image.id,
              windows->image.highlight_context,coordinate_info,
              number_coordinates,CoordModeOrigin);
          if (distance > 9)
            {
              /*
                Display angle of the line.
              */
              degrees=RadiansToDegrees(-atan2((double) (line_info.y2-
                line_info.y1),(double) (line_info.x2-line_info.x1)));
              FormatString(text," %.2f",degrees);
              XInfoWidget(display,windows,text);
              XHighlightLine(display,windows->image.id,
                windows->image.highlight_context,&line_info);
            }
          else
            if (windows->info.mapped)
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          break;
        }
      }
      /*
        Wait for next event.
      */
      XScreenEvent(display,windows,&event);
      switch (primitive)
      {
        case PointPrimitive:
        default:
        {
          if (number_coordinates > 1)
            XDrawLines(display,windows->image.id,
              windows->image.highlight_context,coordinate_info,
              number_coordinates,CoordModeOrigin);
          break;
        }
        case LinePrimitive:
        {
          if (distance > 9)
            XHighlightLine(display,windows->image.id,
              windows->image.highlight_context,&line_info);
          break;
        }
        case RectanglePrimitive:
        case FillRectanglePrimitive:
        {
          if ((rectangle_info.width > 3) && (rectangle_info.height > 3))
            XHighlightRectangle(display,windows->image.id,
              windows->image.highlight_context,&rectangle_info);
          break;
        }
        case CirclePrimitive:
        case FillCirclePrimitive:
        case EllipsePrimitive:
        case FillEllipsePrimitive:
        {
          if ((rectangle_info.width > 3) && (rectangle_info.height > 3))
            XHighlightEllipse(display,windows->image.id,
              windows->image.highlight_context,&rectangle_info);
          break;
        }
        case PolygonPrimitive:
        case FillPolygonPrimitive:
        {
          if (number_coordinates > 1)
            XDrawLines(display,windows->image.id,
              windows->image.highlight_context,coordinate_info,
              number_coordinates,CoordModeOrigin);
          if (distance > 9)
            XHighlightLine(display,windows->image.id,
              windows->image.highlight_context,&line_info);
          break;
        }
      }
      switch (event.type)
      {
        case ButtonPress:
          break;
        case ButtonRelease:
        {
          /*
            User has committed to primitive.
          */
          line_info.x2=event.xbutton.x;
          line_info.y2=event.xbutton.y;
          rectangle_info.x=event.xbutton.x;
          rectangle_info.y=event.xbutton.y;
          coordinate_info[number_coordinates].x=event.xbutton.x;
          coordinate_info[number_coordinates].y=event.xbutton.y;
          if (((primitive != PolygonPrimitive) &&
               (primitive != FillPolygonPrimitive)) || (distance <= 9))
            {
              state|=ExitState;
              break;
            }
          number_coordinates++;
          if (number_coordinates < (int) max_coordinates)
            {
              line_info.x1=event.xbutton.x;
              line_info.y1=event.xbutton.y;
              break;
            }
          max_coordinates<<=1;
          coordinate_info=(XPoint *)
            ReallocateMemory(coordinate_info,max_coordinates*sizeof(XPoint));
          if (coordinate_info == (XPoint *) NULL)
            MagickWarning(ResourceLimitWarning,"Unable to draw on image",
              "Memory allocation failed");
          break;
        }
        case Expose:
          break;
        case MotionNotify:
        {
          if (event.xmotion.window != windows->image.id)
            break;
          if (primitive != PointPrimitive)
            {
              line_info.x2=event.xmotion.x;
              line_info.y2=event.xmotion.y;
              rectangle_info.x=event.xmotion.x;
              rectangle_info.y=event.xmotion.y;
              break;
            }
          coordinate_info[number_coordinates].x=event.xbutton.x;
          coordinate_info[number_coordinates].y=event.xbutton.y;
          number_coordinates++;
          if (number_coordinates < (int) max_coordinates)
            break;
          max_coordinates<<=1;
          coordinate_info=(XPoint *)
            ReallocateMemory(coordinate_info,max_coordinates*sizeof(XPoint));
          if (coordinate_info == (XPoint *) NULL)
            MagickWarning(ResourceLimitWarning,"Unable to draw on image",
              "Memory allocation failed");
          break;
        }
        default:
          break;
      }
      /*
        Check boundary conditions.
      */
      if (line_info.x2 < 0)
        line_info.x2=0;
      else
        if (line_info.x2 > (int) windows->image.width)
          line_info.x2=windows->image.width;
      if (line_info.y2 < 0)
        line_info.y2=0;
      else
        if (line_info.y2 > (int) windows->image.height)
          line_info.y2=windows->image.height;
      distance=
        ((line_info.x2-line_info.x1+1)*(line_info.x2-line_info.x1+1))+
        ((line_info.y2-line_info.y1+1)*(line_info.y2-line_info.y1+1));
      if (((rectangle_info.x != x) && (rectangle_info.y != y)) ||
          (state & ExitState))
        {
          if (rectangle_info.x < 0)
            rectangle_info.x=0;
          else
            if (rectangle_info.x > (int) windows->image.width)
              rectangle_info.x=windows->image.width;
          if (rectangle_info.x < x)
            rectangle_info.width=(unsigned int) (x-rectangle_info.x);
          else
            {
              rectangle_info.width=(unsigned int) (rectangle_info.x-x);
              rectangle_info.x=x;
            }
          if (rectangle_info.y < 0)
            rectangle_info.y=0;
          else
            if (rectangle_info.y > (int) windows->image.height)
              rectangle_info.y=windows->image.height;
          if (rectangle_info.y < y)
            rectangle_info.height=(unsigned int) (y-rectangle_info.y);
          else
            {
              rectangle_info.height=(unsigned int) (rectangle_info.y-y);
              rectangle_info.y=y;
            }
        }
    } while (!(state & ExitState));
    XSetFunction(display,windows->image.highlight_context,GXcopy);
    if ((primitive == PointPrimitive) || (primitive == PolygonPrimitive) ||
        (primitive == FillPolygonPrimitive))
      {
        /*
          Determine polygon bounding box.
        */
        rectangle_info.x=coordinate_info->x;
        rectangle_info.y=coordinate_info->y;
        x=coordinate_info->x;
        y=coordinate_info->y;
        for (i=1; i < number_coordinates; i++)
        {
          if (coordinate_info[i].x > x)
            x=coordinate_info[i].x;
          if (coordinate_info[i].y > y)
            y=coordinate_info[i].y;
          if (coordinate_info[i].x < rectangle_info.x)
            rectangle_info.x=Max(coordinate_info[i].x,0);
          if (coordinate_info[i].y < rectangle_info.y)
            rectangle_info.y=Max(coordinate_info[i].y,0);
        }
        rectangle_info.width=x-rectangle_info.x;
        rectangle_info.height=y-rectangle_info.y;
        for (i=0; i < number_coordinates; i++)
        {
          coordinate_info[i].x-=rectangle_info.x;
          coordinate_info[i].y-=rectangle_info.y;
        }
      }
    else
      if (distance <= 9)
        continue;
      else
        if ((primitive == RectanglePrimitive) ||
            (primitive == CirclePrimitive) || (primitive == EllipsePrimitive))
          {
            rectangle_info.width--;
            rectangle_info.height--;
          }
    /*
      Drawing is relative to image configuration.
    */
    draw_info.x=rectangle_info.x;
    draw_info.y=rectangle_info.y;
    (void) XMagickCommand(display,resource_info,windows,SaveToUndoBufferCommand,
      image);
    width=(*image)->columns;
    height=(*image)->rows;
    x=0;
    y=0;
    if (windows->image.crop_geometry != (char *) NULL)
      (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
    draw_info.x+=windows->image.x-(line_width >> 1);
    if (draw_info.x < 0)
      draw_info.x=0;
    draw_info.x=width*draw_info.x/windows->image.ximage->width;
    draw_info.y+=windows->image.y-(line_width >> 1);
    if (draw_info.y < 0)
      draw_info.y=0;
    draw_info.y=height*draw_info.y/windows->image.ximage->height;
    draw_info.width=rectangle_info.width+(line_width << 1);
    if (draw_info.width > (*image)->columns)
      draw_info.width=(*image)->columns;
    draw_info.height=rectangle_info.height+(line_width << 1);
    if (draw_info.height > (*image)->rows)
      draw_info.height=(*image)->rows;
    FormatString(draw_info.geometry,"%ux%u%+d%+d",
      width*draw_info.width/windows->image.ximage->width,
      height*draw_info.height/windows->image.ximage->height,
      draw_info.x+x,draw_info.y+y);
    /*
      Initialize drawing attributes.
    */
    draw_info.degrees=0.0;
    draw_info.primitive=primitive;
    draw_info.stipple=stipple;
    draw_info.line_width=line_width;
    draw_info.line_info=line_info;
    if (line_info.x1 > (int) (line_width >> 1))
      draw_info.line_info.x1=line_width >> 1;
    if (line_info.y1 > (int) (line_width >> 1))
      draw_info.line_info.y1=line_width >> 1;
    draw_info.line_info.x2=line_info.x2-line_info.x1+(line_width >> 1);
    draw_info.line_info.y2=line_info.y2-line_info.y1+(line_width >> 1);
    if ((draw_info.line_info.x2 < 0) && (draw_info.line_info.y2 < 0))
      {
        draw_info.line_info.x2=(-draw_info.line_info.x2);
        draw_info.line_info.y2=(-draw_info.line_info.y2);
      }
    if (draw_info.line_info.x2 < 0)
      {
        draw_info.line_info.x2=(-draw_info.line_info.x2);
        Swap(draw_info.line_info.x1,draw_info.line_info.x2);
      }
    if (draw_info.line_info.y2 < 0)
      {
        draw_info.line_info.y2=(-draw_info.line_info.y2);
        Swap(draw_info.line_info.y1,draw_info.line_info.y2);
      }
    draw_info.rectangle_info=rectangle_info;
    if (draw_info.rectangle_info.x > (int) (line_width >> 1))
      draw_info.rectangle_info.x=line_width >> 1;
    if (draw_info.rectangle_info.y > (int) (line_width >> 1))
      draw_info.rectangle_info.y=line_width >> 1;
    draw_info.number_coordinates=number_coordinates;
    draw_info.coordinate_info=coordinate_info;
    windows->pixel_info->pen_color=windows->pixel_info->pen_colors[pen_id];
    /*
      Draw primitive on image.
    */
    XSetCursorState(display,windows,True);
    XCheckRefreshWindows(display,windows);
    status=XDrawImage(display,windows->pixel_info,&draw_info,*image);
    XSetCursorState(display,windows,False);
    /*
      Update image colormap and return to image drawing.
    */
    XConfigureImageColormap(display,resource_info,windows,*image);
    (void) XConfigureImage(display,resource_info,windows,*image);
  }
  XSetCursorState(display,windows,False);
  FreeMemory((char *) coordinate_info);
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X D r a w P a n R e c t a n g l e                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XDrawPanRectangle draws a rectangle in the pan window.  The pan
%  window displays a zoomed image and the rectangle shows which portion of
%  the image is displayed in the Image window.
%
%  The format of the XDrawPanRectangle method is:
%
%    XDrawPanRectangle(display,windows)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%
*/
static void XDrawPanRectangle(Display *display,XWindows *windows)
{
  unsigned long
    scale_factor;

  RectangleInfo
    highlight_info;

  /*
    Determine dimensions of the panning rectangle.
  */
  scale_factor=(unsigned long)
    (UpShift(windows->pan.width)/windows->image.ximage->width);
  highlight_info.x=DownShift(windows->image.x*scale_factor);
  highlight_info.width=DownShift(windows->image.width*scale_factor);
  scale_factor=(unsigned long)
    (UpShift(windows->pan.height)/windows->image.ximage->height);
  highlight_info.y=DownShift(windows->image.y*scale_factor);
  highlight_info.height=DownShift(windows->image.height*scale_factor);
  /*
    Display the panning rectangle.
  */
  XClearWindow(display,windows->pan.id);
  XHighlightRectangle(display,windows->pan.id,windows->pan.annotate_context,
    &highlight_info);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X I m a g e C a c h e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XImageCache handles the creation, manipulation, and destruction of
%  the image cache (undo and redo buffers).
%
%  The format of the XImageCache method is:
%
%    XImageCache(display,resource_info,windows,command,image)
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
%    o command: Specifies a command to perform.
%
%    o image: Specifies a pointer to a Image structure;  XImageCache
%      may transform the image and return a new image pointer.
%
%
*/
static void XImageCache(Display *display,XResourceInfo *resource_info,
  XWindows *windows,const CommandType command,Image **image)
{
  Image
    *cache_image;

  static Image
    *redo_image = (Image *) NULL,
    *undo_image = (Image *) NULL;

  switch (command)
  {
    case FreeBuffersCommand:
    {
      /*
        Free memory from the undo and redo cache.
      */
      while (undo_image != (Image *) NULL)
      {
        cache_image=undo_image;
        undo_image=undo_image->previous;
        DestroyImage(cache_image->list);
        DestroyImage(cache_image);
      }
      undo_image=(Image *) NULL;
      if (redo_image != (Image *) NULL)
        DestroyImage(redo_image);
      redo_image=(Image *) NULL;
      return;
    }
    case UndoCommand:
    {
      /*
        Undo the last image transformation.
      */
      if (undo_image == (Image *) NULL)
        {
          XBell(display,0);
          return;
        }
      cache_image=undo_image;
      undo_image=undo_image->previous;
      windows->image.window_changes.width=cache_image->columns;
      windows->image.window_changes.height=cache_image->rows;
      if (windows->image.crop_geometry != (char *) NULL)
        FreeMemory((char *) windows->image.crop_geometry);
      windows->image.crop_geometry=cache_image->geometry;
      if (redo_image != (Image *) NULL)
        DestroyImage(redo_image);
      redo_image=(*image);
      *image=cache_image->list;
      DestroyImage(cache_image);
      if (windows->image.orphan)
        return;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      return;
    }
    case CutCommand:
    case PasteCommand:
    case ApplyCommand:
    case HalfSizeCommand:
    case OriginalSizeCommand:
    case DoubleSizeCommand:
    case ResizeCommand:
    case TrimCommand:
    case CropCommand:
    case ChopCommand:
    case FlipCommand:
    case FlopCommand:
    case RotateRightCommand:
    case RotateLeftCommand:
    case RotateCommand:
    case ShearCommand:
    case RollCommand:
    case NegateCommand:
    case EqualizeCommand:
    case NormalizeCommand:
    case HueCommand:
    case SaturationCommand:
    case BrightnessCommand:
    case GammaCommand:
    case SpiffCommand:
    case DullCommand:
    case GrayscaleCommand:
    case MapCommand:
    case QuantizeCommand:
    case DespeckleCommand:
    case EmbossCommand:
    case ReduceNoiseCommand:
    case AddNoiseCommand:
    case SharpenCommand:
    case BlurCommand:
    case ThresholdCommand:
    case EdgeDetectCommand:
    case SpreadCommand:
    case ShadeCommand:
    case RaiseCommand:
    case SegmentCommand:
    case SolarizeCommand:
    case SwirlCommand:
    case ImplodeCommand:
    case WaveCommand:
    case OilPaintCommand:
    case CharcoalDrawingCommand:
    case AnnotateCommand:
    case AddBorderCommand:
    case AddFrameCommand:
    case CompositeCommand:
    case CommentCommand:
    case LaunchCommand:
    case RegionofInterestCommand:
    case SaveToUndoBufferCommand:
    case RedoCommand:
    {
      Image
        *previous_image;

      unsigned int
        bytes;

      bytes=(unsigned int) ((*image)->packets*sizeof(RunlengthPacket));
      if (undo_image != (Image *) NULL)
        {
          /*
            Ensure the undo cache has enough memory available.
          */
          previous_image=undo_image;
          while (previous_image != (Image *) NULL)
          {
            bytes+=previous_image->list->packets*sizeof(RunlengthPacket);
            if (bytes <= (resource_info->undo_cache << 20))
              {
                previous_image=previous_image->previous;
                continue;
              }
            bytes-=previous_image->list->packets*sizeof(RunlengthPacket);
            if (previous_image == undo_image)
              undo_image=(Image *) NULL;
            else
              previous_image->next->previous=(Image *) NULL;
            break;
          }
          while (previous_image != (Image *) NULL)
          {
            /*
              Delete any excess memory from undo cache.
            */
            cache_image=previous_image;
            previous_image=previous_image->previous;
            cache_image->file=(FILE *) NULL;
            DestroyImage(cache_image->list);
            DestroyImage(cache_image);
          }
        }
      if (bytes > (resource_info->undo_cache << 20))
        break;
      /*
        Save image before transformations are applied.
      */
      cache_image=AllocateImage((ImageInfo *) NULL);
      if (cache_image == (Image *) NULL)
        break;
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      (*image)->orphan=True;
      cache_image->list=
        CloneImage(*image,(*image)->columns,(*image)->rows,True);
      (*image)->orphan=False;
      XSetCursorState(display,windows,False);
      if (cache_image->list == (Image *) NULL)
        {
          DestroyImage(cache_image);
          break;
        }
      cache_image->columns=windows->image.ximage->width;
      cache_image->rows=windows->image.ximage->height;
      cache_image->geometry=windows->image.crop_geometry;
      if (windows->image.crop_geometry != (char *) NULL)
        {
          cache_image->geometry=(char *)
            AllocateMemory(MaxTextExtent*sizeof(char));
          if (cache_image->geometry != (char *) NULL)
            (void) strcpy(cache_image->geometry,windows->image.crop_geometry);
        }
      if (undo_image == (Image *) NULL)
        {
          undo_image=cache_image;
          break;
        }
      undo_image->next=cache_image;
      undo_image->next->previous=undo_image;
      undo_image=undo_image->next;
      break;
    }
    default:
      break;
  }
  if (command == RedoCommand)
    {
      /*
        Redo the last image transformation.
      */
      if (redo_image == (Image *) NULL)
        {
          XBell(display,0);
          return;
        }
      windows->image.window_changes.width=redo_image->columns;
      windows->image.window_changes.height=redo_image->rows;
      if (windows->image.crop_geometry != (char *) NULL)
        FreeMemory((char *) windows->image.crop_geometry);
      windows->image.crop_geometry=redo_image->geometry;
      DestroyImage(*image);
      *image=redo_image;
      redo_image=(Image *) NULL;
      if (windows->image.orphan)
        return;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      return;
    }
  if (command != InfoCommand)
    return;
  /*
    Display image info.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  XDisplayImageInfo(display,resource_info,windows,undo_image,*image);
  XSetCursorState(display,windows,False);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X I m a g e W i n d o w C o m m a n d                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XImageWindowCommand makes a transform to the image or Image window
%  as specified by a user menu button or keyboard command.
%
%  The format of the XMagickCommand method is:
%
%    loaded_image=XImageWindowCommand(display,resource_info,windows,state,
%      key_symbol,image)
%
%  A description of each parameter follows:
%
%    o loaded_image:  Method XImageWindowCommand returns an image when the
%      user chooses 'Open Image' from the command menu.  Otherwise a null
%      image is returned.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o state: key mask.
%
%    o key_symbol: Specifies a command to perform.
%
%    o image: Specifies a pointer to a Image structure;  XImageWIndowCommand
%      may transform the image and return a new image pointer.
%
%
*/
static CommandType XImageWindowCommand(Display *display,
  XResourceInfo *resource_info,XWindows *windows,const unsigned int state,
  KeySym key_symbol,Image **image)
{
  static char
    delta[MaxTextExtent] = "";

  static const char
    Digits[]="01234567890";

  static KeySym
    last_symbol = XK_0;

  if ((key_symbol >= XK_0) && (key_symbol <= XK_9))
    {
      if (!((last_symbol >= XK_0) && (last_symbol <= XK_9)))
        {
          *delta='\0';
          resource_info->quantum=1;
        }
      last_symbol=key_symbol;
      delta[Extent(delta)+1]='\0';
      delta[Extent(delta)]=Digits[key_symbol-XK_0];
      resource_info->quantum=atoi(delta);
      return(NullCommand);
    }
  last_symbol=key_symbol;
  if (resource_info->immutable)
    {
      /*
        Immutable image window has a restricted command set.
      */
      switch(key_symbol)
      {
        case XK_question:
          return(InfoCommand);
        case XK_space:
          return(NextCommand);
        case XK_q:
        {
          if (!(state & ControlMask))
            break;
          return(QuitCommand);
        }
        default:
          break;
      }
      return(NullCommand);
    }
  switch (key_symbol)
  {
    case XK_o:
    {
      if (!(state & ControlMask))
        break;
      return(OpenCommand);
    }
    case XK_space:
      return(NextCommand);
    case XK_BackSpace:
      return(FormerCommand);
    case XK_s:
    {
      if (state & Mod1Mask)
        return(SwirlCommand);
      if (!(state & ControlMask))
        return(ShearCommand);
      return(SaveCommand);
    }
    case XK_p:
    case XK_Print:
    {
      if (state & Mod1Mask)
        return(OilPaintCommand);
      if (state & Mod4Mask)
        return(ColorCommand);
      if (!(state & ControlMask))
        return(NullCommand);
      return(PrintCommand);
    }
    case XK_d:
    {
      if (state & Mod4Mask)
        return(DrawCommand);
      if (!(state & ControlMask))
        return(NullCommand);
      return(DeleteCommand);
    }
    case XK_Select:
    {
      if (!(state & ControlMask))
        return(NullCommand);
      return(SelectCommand);
    }
    case XK_n:
    {
      if (!(state & ControlMask))
        return(NullCommand);
      return(NewCommand);
    }
    case XK_q:
    case XK_Cancel:
    {
      if (!(state & ControlMask))
        return(NullCommand);
      return(QuitCommand);
    }
    case XK_z:
    case XK_Undo:
    {
      if (!(state & ControlMask))
        return(NullCommand);
      return(UndoCommand);
    }
    case XK_r:
    case XK_Redo:
    {
      if (!(state & ControlMask))
        return(RollCommand);
      return(RedoCommand);
    }
    case XK_x:
    {
      if (!(state & ControlMask))
        return(NullCommand);
      return(CutCommand);
    }
    case XK_c:
    {
      if (state & Mod1Mask)
        return(CharcoalDrawingCommand);
      if (!(state & ControlMask))
        return(CropCommand);
      return(CopyCommand);
    }
    case XK_v:
    case XK_Insert:
    {
      if (state & Mod4Mask)
        return(CompositeCommand);
      if (!(state & ControlMask))
        return(FlipCommand);
      return(PasteCommand);
    }
    case XK_less:
      return(HalfSizeCommand);
    case XK_minus:
      return(OriginalSizeCommand);
    case XK_greater:
      return(DoubleSizeCommand);
    case XK_percent:
      return(ResizeCommand);
    case XK_at:
      return(RefreshCommand);
    case XK_bracketleft:
      return(ChopCommand);
    case XK_h:
      return(FlopCommand);
    case XK_slash:
      return(RotateRightCommand);
    case XK_backslash:
      return(RotateLeftCommand);
    case XK_asterisk:
      return(RotateCommand);
    case XK_t:
      return(TrimCommand);
    case XK_H:
      return(HueCommand);
    case XK_S:
      return(SaturationCommand);
    case XK_L:
      return(BrightnessCommand);
    case XK_G:
      return(GammaCommand);
    case XK_C:
      return(SpiffCommand);
    case XK_Z:
      return(DullCommand);
    case XK_equal:
      return(EqualizeCommand);
    case XK_N:
      return(NormalizeCommand);
    case XK_asciitilde:
      return(NegateCommand);
    case XK_period:
      return(GrayscaleCommand);
    case XK_numbersign:
      return(QuantizeCommand);
    case XK_F2:
      return(DespeckleCommand);
    case XK_F3:
      return(EmbossCommand);
    case XK_F4:
      return(ReduceNoiseCommand);
    case XK_F5:
      return(AddNoiseCommand);
    case XK_F6:
      return(SharpenCommand);
    case XK_F7:
      return(BlurCommand);
    case XK_F8:
      return(ThresholdCommand);
    case XK_F9:
      return(EdgeDetectCommand);
    case XK_F10:
      return(SpreadCommand);
    case XK_F11:
      return(ShadeCommand);
    case XK_F12:
      return(RaiseCommand);
    case XK_F13:
      return(SegmentCommand);
    case XK_i:
    {
      if (!(state & Mod1Mask))
        return(NullCommand);
      return(ImplodeCommand);
    }
    case XK_w:
    {
      if (!(state & Mod1Mask))
        return(NullCommand);
      return(WaveCommand);
    }
    case XK_m:
    {
      if (!(state & Mod4Mask))
        return(NullCommand);
      return(MatteCommand);
    }
    case XK_b:
    {
      if (!(state & Mod4Mask))
        return(NullCommand);
      return(AddBorderCommand);
    }
    case XK_f:
    {
      if (!(state & Mod4Mask))
        return(NullCommand);
      return(AddFrameCommand);
    }
    case XK_exclam:
    {
      if (!(state & Mod4Mask))
        return(NullCommand);
      return(CommentCommand);
    }
    case XK_a:
    {
      if (state & Mod1Mask)
        return(ApplyCommand);
      if (state & Mod4Mask)
        return(AnnotateCommand);
      if (!(state & ControlMask))
        return(NullCommand);
      return(RegionofInterestCommand);
    }
    case XK_question:
      return(InfoCommand);
    case XK_plus:
      return(ZoomCommand);
    case XK_P:
    {
      if (!(state & ShiftMask))
        return(NullCommand);
      return(ShowPreviewCommand);
    }
    case XK_Execute:
      return(LaunchCommand);
    case XK_F1:
      return(HelpCommand);
    case XK_Find:
      return(BrowseDocumentationCommand);
    case XK_Menu:
    {
      XMapRaised(display,windows->command.id);
      return(NullCommand);
    }
    case XK_Next:
    case XK_Prior:
    case XK_Home:
    case XK_KP_Home:
    {
      XTranslateImage(display,windows,*image,key_symbol);
      return(NullCommand);
    }
    case XK_Up:
    case XK_KP_Up:
    case XK_Down:
    case XK_KP_Down:
    case XK_Left:
    case XK_KP_Left:
    case XK_Right:
    case XK_KP_Right:
    {
      if (state & Mod1Mask)
        {
          RectangleInfo
            crop_info;

          /*
            Trim one pixel from edge of image.
          */
          crop_info.x=0;
          crop_info.y=0;
          crop_info.width=windows->image.ximage->width;
          crop_info.height=windows->image.ximage->height;
          if ((key_symbol == XK_Up) || (key_symbol == XK_KP_Up))
            {
              if (resource_info->quantum >= (int) crop_info.height)
                resource_info->quantum=crop_info.height-1;
              crop_info.height-=resource_info->quantum;
            }
          if ((key_symbol == XK_Down) || (key_symbol == XK_KP_Down))
            {
              if (resource_info->quantum >= (int) (crop_info.height-crop_info.y))
                resource_info->quantum=crop_info.height-crop_info.y-1;
              crop_info.y+=resource_info->quantum;
              crop_info.height-=resource_info->quantum;
            }
          if ((key_symbol == XK_Left) || (key_symbol == XK_KP_Left))
            {
              if (resource_info->quantum >= (int) crop_info.width)
                resource_info->quantum=crop_info.width-1;
              crop_info.width-=resource_info->quantum;
            }
          if ((key_symbol == XK_Right) || (key_symbol == XK_KP_Right))
            {
              if (resource_info->quantum >= (int) (crop_info.width-crop_info.x))
                resource_info->quantum=crop_info.width-crop_info.x-1;
              crop_info.x+=resource_info->quantum;
              crop_info.width-=resource_info->quantum;
            }
          if ((int) (windows->image.x+windows->image.width) >
              (int) crop_info.width)
            windows->image.x=crop_info.width-windows->image.width;
          if ((int) (windows->image.y+windows->image.height) >
              (int) crop_info.height)
            windows->image.y=crop_info.height-windows->image.height;
          XSetCropGeometry(display,windows,&crop_info,*image);
          windows->image.window_changes.width=crop_info.width;
          windows->image.window_changes.height=crop_info.height;
          XSetWindowBackgroundPixmap(display,windows->image.id,None);
          (void) XConfigureImage(display,resource_info,windows,*image);
          return(NullCommand);
        }
      XTranslateImage(display,windows,*image,key_symbol);
      return(NullCommand);
    }
    default:
      return(NullCommand);
  }
  return(NullCommand);
}

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
%  Method XMagickCommand makes a transform to the image or Image window
%  as specified by a user menu button or keyboard command.
%
%  The format of the XMagickCommand method is:
%
%    loaded_image=XMagickCommand(display,resource_info,windows,command,image)
%
%  A description of each parameter follows:
%
%    o loaded_image:  Method XMagickCommand returns an image when the
%      user chooses 'Load Image' from the command menu.  Otherwise a null
%      image is returned.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o command: Specifies a command to perform.
%
%    o image: Specifies a pointer to a Image structure;  XMagickCommand
%      may transform the image and return a new image pointer.
%
%
*/

static Image *XMagickCommand(Display *display,XResourceInfo *resource_info,
  XWindows *windows,const CommandType command,Image **image)
{
  char
    *argv[10],
    geometry[MaxTextExtent],
    modulate_factors[MaxTextExtent];

  Image
    *loaded_image;

  ImageInfo
    image_info;

  int
    status,
    x,
    y;

  static char
    color[MaxTextExtent] = "gray";

  unsigned int
    height,
    width;

  /*
    Process user command.
  */
  XCheckRefreshWindows(display,windows);
  XImageCache(display,resource_info,windows,command,image);
  argv[0]=resource_info->client_name;
  loaded_image=(Image *) NULL;
  windows->image.window_changes.width=windows->image.ximage->width;
  windows->image.window_changes.height=windows->image.ximage->height;
  GetImageInfo(&image_info);
  switch (command)
  {
    case OpenCommand:
    {
      /*
        Load image.
      */
      loaded_image=XOpenBlob(display,resource_info,windows,False);
      break;
    }
    case NextCommand:
    {
      /*
        Display next image.
      */
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_next_image,CurrentTime);
      break;
    }
    case FormerCommand:
    {
      /*
        Display former image.
      */
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_former_image,CurrentTime);
      break;
    }
    case SelectCommand:
    {
      /*
        Select image.
      */
      (void) chdir(resource_info->home_directory);
      loaded_image=XOpenBlob(display,resource_info,windows,True);
      break;
    }
    case SaveCommand:
    {
      /*
        Save image.
      */
      status=XSaveImage(display,resource_info,windows,*image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to write X image:",
            (*image)->filename);
          break;
        }
      break;
    }
    case PrintCommand:
    {
      /*
        Print image.
      */
      status=XPrintImage(display,resource_info,windows,*image);
      if (status != False)
        {
          XNoticeWidget(display,windows,"Unable to print X image:",
            (*image)->filename);
          break;
        }
      break;
    }
    case DeleteCommand:
    {
      static char
        filename[MaxTextExtent] = "\0";

      /*
        Delete image file.
      */
      XFileBrowserWidget(display,windows,"Delete",filename);
      if (*filename == '\0')
        break;
      status=remove(filename);
      if (status != False)
        XNoticeWidget(display,windows,"Unable to delete image file:",filename);
      break;
    }
    case NewCommand:
    {
      static char
        *format = "gradation",
        color[MaxTextExtent] = "gray",
        geometry[MaxTextExtent] = "640x480";

      /*
        Query user for canvas geometry.
      */
      status=XDialogWidget(display,windows,"New","Enter image geometry:",
        geometry);
      if (*geometry == '\0')
        break;
      if (!status)
        format="xc";
      XColorBrowserWidget(display,windows,"Select",color);
      if (*color == '\0')
        break;
      /*
        Create canvas.
      */
      FormatString(image_info.filename,"%.1024s:%.1024s",format,color);
      (void) CloneString(&image_info.size,geometry);
      loaded_image=ReadImage(&image_info);
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_next_image,CurrentTime);
      break;
    }
    case VisualDirectoryCommand:
    {
      /*
        Visual Image directory.
      */
      loaded_image=XVisualDirectoryImage(display,resource_info,windows);
      break;
    }
    case QuitCommand:
    {
      /*
        Exit program.
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
    case CutCommand:
    {
      /*
        Cut image.
      */
      (void) XCropImage(display,resource_info,windows,*image,CutMode);
      break;
    }
    case CopyCommand:
    {
      /*
        Copy image.
      */
      (void) XCropImage(display,resource_info,windows,*image,CopyMode);
      break;
    }
    case PasteCommand:
    {
      /*
        Paste image.
      */
      status=XPasteImage(display,resource_info,windows,*image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to paste X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case HalfSizeCommand:
    {
      /*
        Half image size.
      */
      windows->image.window_changes.width=windows->image.ximage->width >> 1;
      windows->image.window_changes.height=windows->image.ximage->height >> 1;
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case OriginalSizeCommand:
    {
      /*
        Original image size.
      */
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case DoubleSizeCommand:
    {
      /*
        Double the image size.
      */
      windows->image.window_changes.width=windows->image.ximage->width << 1;
      windows->image.window_changes.height=windows->image.ximage->height << 1;
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case ResizeCommand:
    {
      unsigned int
        height,
        width;

      /*
        Resize image.
      */
      width=windows->image.ximage->width;
      height=windows->image.ximage->height;
      x=0;
      y=0;
      FormatString(geometry,"%ux%u+0+0",width,height);
      status=XDialogWidget(display,windows,"Resize",
        "Enter resize geometry (e.g. 640x480, 200%):",geometry);
      if (*geometry == '\0')
        break;
      if (!status)
        (void) strcat(geometry,"!");
      (void) ParseImageGeometry(geometry,&x,&y,&width,&height);
      windows->image.window_changes.width=width;
      windows->image.window_changes.height=height;
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case ApplyCommand:
    {
      char
        image_geometry[MaxTextExtent];

      if ((windows->image.crop_geometry == (char *) NULL) &&
          ((int) (*image)->columns == windows->image.ximage->width) &&
          ((int) (*image)->rows == windows->image.ximage->height) &&
          (resource_info->quantize_info->number_colors == 0))
        break;
      /*
        Apply size transforms to image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      /*
        Crop and/or scale displayed image.
      */
      FormatString(image_geometry,"%dx%d!",windows->image.ximage->width,
        windows->image.ximage->height);
      TransformImage(image,windows->image.crop_geometry,image_geometry);
      if (windows->image.crop_geometry != (char *) NULL)
        {
          FreeMemory((char *) windows->image.crop_geometry);
          windows->image.crop_geometry=(char *) NULL;
        }
      windows->image.x=0;
      windows->image.y=0;
      if (resource_info->quantize_info->number_colors != 0)
        {
          /*
            Reduce the number of colors in the image.
          */
          if (((*image)->class == DirectClass) ||
              ((*image)->colors > resource_info->quantize_info->number_colors) ||
              (resource_info->quantize_info->colorspace == GRAYColorspace))
            (void) QuantizeImage(resource_info->quantize_info,*image);
          SyncImage(*image);
        }
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case RefreshCommand:
    {
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case RestoreCommand:
    {
      /*
        Restore Image window to its original size.
      */
      if ((windows->image.width == (*image)->columns) &&
          (windows->image.height == (*image)->rows) &&
          (windows->image.crop_geometry == (char *) NULL))
        {
          XBell(display,0);
          break;
        }
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      if (windows->image.crop_geometry != (char *) NULL)
        {
          FreeMemory((char *) windows->image.crop_geometry);
          windows->image.crop_geometry=(char *) NULL;
          windows->image.x=0;
          windows->image.y=0;
        }
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case CropCommand:
    {
      /*
        Crop image.
      */
      (void) XCropImage(display,resource_info,windows,*image,CropMode);
      break;
    }
    case ChopCommand:
    {
      /*
        Chop image.
      */
      status=XChopImage(display,resource_info,windows,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to cut X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case FlopCommand:
    {
      /*
        Flop image scanlines.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-flop";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.crop_geometry != (char *) NULL)
        {
          /*
            Flop crop geometry.
          */
          width=(*image)->columns;
          height=(*image)->rows;
          (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
            &width,&height);
          FormatString(windows->image.crop_geometry,"%ux%u%+d%+d",width,
            height,(int) (*image)->columns-(int) width-x,y);
        }
      if (windows->image.orphan)
        break;
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case FlipCommand:
    {
      /*
        Flip image scanlines.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-flip";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.crop_geometry != (char *) NULL)
        {
          /*
            Flip crop geometry.
          */
          width=(*image)->columns;
          height=(*image)->rows;
          (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
            &width,&height);
          FormatString(windows->image.crop_geometry,"%ux%u%+d%+d",width,
            height,x,(int) (*image)->rows-(int) height-y);
        }
      if (windows->image.orphan)
        break;
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case RotateRightCommand:
    {
      /*
        Rotate image 90 degrees clockwise.
      */
      status=XRotateImage(display,resource_info,windows,90.0,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to rotate X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case RotateLeftCommand:
    {
      /*
        Rotate image 90 degrees counter-clockwise.
      */
      status=XRotateImage(display,resource_info,windows,-90.0,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to rotate X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case RotateCommand:
    {
      /*
        Rotate image.
      */
      status=XRotateImage(display,resource_info,windows,0.0,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to rotate X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case ShearCommand:
    {
      static char
        geometry[MaxTextExtent] = "45.0x45.0";

      /*
        Query user for shear color and geometry.
      */
      XColorBrowserWidget(display,windows,"Select",color);
      if (*color == '\0')
        break;
      (void) XDialogWidget(display,windows,"Shear","Enter shear geometry:",
        geometry);
      if (*geometry == '\0')
        break;
      /*
        Shear image.
      */
      (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-bordercolor";
      argv[2]=color;
      argv[3]="-shear";
      argv[4]=geometry;
      MogrifyImage(resource_info->image_info,5,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case RollCommand:
    {
      static char
        geometry[MaxTextExtent] = "+2+2";

      /*
        Query user for the roll geometry.
      */
      (void) XDialogWidget(display,windows,"Roll","Enter roll geometry:",
        geometry);
      if (*geometry == '\0')
        break;
      /*
        Roll image.
      */
      (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-roll";
      argv[2]=geometry;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case TrimCommand:
    {
      /*
        Trim image.
      */
      status=XTrimImage(display,resource_info,windows,*image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to trim X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case HueCommand:
    {
      static char
        hue_percent[MaxTextExtent] = "3";

      /*
        Query user for percent hue change.
      */
      (void) XDialogWidget(display,windows,"Apply",
        "Enter percent change in image hue:",hue_percent);
      if (*hue_percent == '\0')
        break;
      /*
        Vary the image hue.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      (void) strcpy(modulate_factors,"0.0/0.0/");
      (void) strcat(modulate_factors,hue_percent);
      argv[1]="-modulate";
      argv[2]=modulate_factors;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SaturationCommand:
    {
      static char
        saturation_percent[MaxTextExtent] = "10";

      /*
        Query user for percent saturation change.
      */
      (void) XDialogWidget(display,windows,"Apply",
        "Enter percent change in color saturation:",saturation_percent);
      if (*saturation_percent == '\0')
        break;
      /*
        Vary color saturation.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      (void) strcpy(modulate_factors,"0.0/");
      (void) strcat(modulate_factors,saturation_percent);
      argv[1]="-modulate";
      argv[2]=modulate_factors;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case BrightnessCommand:
    {
      static char
        brightness_percent[MaxTextExtent] = "3";

      /*
        Query user for percent brightness change.
      */
      (void) XDialogWidget(display,windows,"Apply",
        "Enter percent change in color brightness:",brightness_percent);
      if (*brightness_percent == '\0')
        break;
      /*
        Vary the color brightness.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      (void) strcpy(modulate_factors,brightness_percent);
      argv[1]="-modulate";
      argv[2]=modulate_factors;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case GammaCommand:
    {
      static char
        factor[MaxTextExtent] = "1.6";

      /*
        Query user for gamma value.
      */
      (void) XDialogWidget(display,windows,"Gamma",
        "Enter gamma value (e.g. 1.0/1.0/1.6):",factor);
      if (*factor == '\0')
        break;
      /*
        Gamma correct image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-gamma";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SpiffCommand:
    {
      /*
        Sharpen the image contrast.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-contrast";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case DullCommand:
    {
      /*
        Dull the image contrast.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="+contrast";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case EqualizeCommand:
    {
      /*
        Perform histogram equalization on the image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-equalize";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case NormalizeCommand:
    {
      /*
        Perform histogram normalization on the image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-normalize";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case NegateCommand:
    {
      /*
        Negate colors in image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-negate";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case GrayscaleCommand:
    {
      /*
        Convert image to grayscale.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-colorspace";
      argv[2]="gray";
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case MapCommand:
    {
      static char
        filename[MaxTextExtent] = "\0";

      /*
        Request image file name from user.
      */
      XFileBrowserWidget(display,windows,"Map",filename);
      if (*filename == '\0')
        break;
      /*
        Map image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-map";
      argv[2]=filename;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case QuantizeCommand:
    {
      static char
        colors[MaxTextExtent] = "256";

      /*
        Query user for maximum number of colors.
      */
      status=XDialogWidget(display,windows,"Quantize",
        "Maximum number of colors:",colors);
      if (*colors == '\0')
        break;
      /*
        Color reduce the image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-colors";
      argv[2]=colors;
      argv[3]=status ? "-dither" : "+dither";
      MogrifyImage(resource_info->image_info,4,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case DespeckleCommand:
    {
      /*
        Despeckle image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-despeckle";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case EmbossCommand:
    {
      /*
        Emboss image scanlines.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-emboss";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case ReduceNoiseCommand:
    {
      /*
        Reduce noise in the image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-noise";
      MogrifyImage(resource_info->image_info,2,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case AddNoiseCommand:
    {
      static char
        noise_type[MaxTextExtent] = "Gaussian";

      /*
        Add noise to the image.
      */
      XListBrowserWidget(display,windows,&windows->widget,NoiseTypes,
        "Add Noise","Select a type of noise to add to your image:",noise_type);
      if (*noise_type == '\0')
        break;
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="+noise";
      argv[2]=noise_type;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SharpenCommand:
    {
      static char
        factor[MaxTextExtent] = "60.0";

      /*
        Query user for sharpen factor.
      */
      (void) XDialogWidget(display,windows,"Sharpen",
        "Enter the sharpening factor (0.0 - 99.9%):",factor);
      if (*factor == '\0')
        break;
      /*
        Sharpen image scanlines.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-sharpen";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case BlurCommand:
    {
      static char
        factor[MaxTextExtent] = "60.0";

      /*
        Query user for blur factor.
      */
      (void) XDialogWidget(display,windows,"Blur",
        "Enter the blurring factor (0.0 - 99.9%):",factor);
      if (*factor == '\0')
        break;
      /*
        Blur an image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-blur";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case ThresholdCommand:
    {
      static char
        factor[MaxTextExtent] = "128";

      /*
        Query user for threshold value.
      */
      (void) XDialogWidget(display,windows,"Threshold",
        "Enter threshold value:",factor);
      if (*factor == '\0')
        break;
      /*
        Gamma correct image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-threshold";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case EdgeDetectCommand:
    {
      static char
        factor[MaxTextExtent] = "60.0";

      /*
        Query user for edge factor.
      */
      (void) XDialogWidget(display,windows,"Detect Edges",
        "Enter the edge detect factor (0.0 - 99.9%):",factor);
      if (*factor == '\0')
        break;
      /*
        Detect edge in image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-edge";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SpreadCommand:
    {
      static char
        amount[MaxTextExtent] = "2";

      /*
        Query user for spread amount.
      */
      (void) XDialogWidget(display,windows,"Spread",
        "Enter the displacement amount:",amount);
      if (*amount == '\0')
        break;
      /*
        Displace image pixels by a random amount.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-spread";
      argv[2]=amount;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case ShadeCommand:
    {
      static char
        geometry[MaxTextExtent] = "30x30";

      /*
        Query user for the shade geometry.
      */
      status=XDialogWidget(display,windows,"Shade",
        "Enter the azimuth and elevation of the light source:",geometry);
      if (*geometry == '\0')
        break;
      /*
        Shade image pixels.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]=status ? "-shade" : "+shade";
      argv[2]=geometry;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case RaiseCommand:
    {
      static char
        bevel_width[MaxTextExtent] = "10";

      /*
        Query user for bevel width.
      */
      (void) XDialogWidget(display,windows,"Raise","Bevel width:",bevel_width);
      if (*bevel_width == '\0')
        break;
      /*
        Raise an image.
      */
      (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-raise";
      argv[2]=bevel_width;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SegmentCommand:
    {
      static char
        threshold[MaxTextExtent] = "1.5";

      /*
        Query user for smoothing threshold.
      */
      (void) XDialogWidget(display,windows,"Segment","Smoothing threshold:",
        threshold);
      if (*threshold == '\0')
        break;
      /*
        Segment an image.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-segment";
      argv[2]=threshold;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SolarizeCommand:
    {
      static char
        factor[MaxTextExtent] = "60";

      /*
        Query user for solarize factor.
      */
      (void) XDialogWidget(display,windows,"Solarize",
        "Enter the solarize factor (0 - 99.9%):",factor);
      if (*factor == '\0')
        break;
      /*
        Solarize image pixels.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-solarize";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case SwirlCommand:
    {
      static char
        degrees[MaxTextExtent] = "60";

      /*
        Query user for swirl angle.
      */
      (void) XDialogWidget(display,windows,"Swirl","Enter the swirl angle:",
        degrees);
      if (*degrees == '\0')
        break;
      /*
        Swirl image pixels about the center.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-swirl";
      argv[2]=degrees;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case ImplodeCommand:
    {
      static char
        factor[MaxTextExtent] = "30.0";

      /*
        Query user for implode factor.
      */
      (void) XDialogWidget(display,windows,"Implode",
        "Enter the implosion/explosion factor (-99.9 - 99.9%):",factor);
      if (*factor == '\0')
        break;
      /*
        Implode image pixels about the center.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-implode";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case WaveCommand:
    {
      static char
        geometry[MaxTextExtent] = "25x150";

      /*
        Query user for the shade geometry.
      */
      (void) XDialogWidget(display,windows,"Wave",
        "Enter the amplitude and length of the wave:",geometry);
      if (*geometry == '\0')
        break;
      /*
        Shade image pixels.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-wave";
      argv[2]=geometry;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case OilPaintCommand:
    {
      static char
        radius[MaxTextExtent] = "3";

      /*
        Query user for circular neighborhood radius.
      */
      (void) XDialogWidget(display,windows,"Oil Paint",
        "Enter the mask radius:",radius);
      if (*radius == '\0')
        break;
      /*
        OilPaint image scanlines.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-paint";
      argv[2]=radius;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case CharcoalDrawingCommand:
    {
      static char
        factor[MaxTextExtent] = "50";

      /*
        Query user for bevel width.
      */
      (void) XDialogWidget(display,windows,"Charcoal Drawing",
        "Enter the charcoal factor (0 - 99.9%):",factor);
      if (*factor == '\0')
        break;
      /*
        Raise an image.
      */
      (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-charcoal";
      argv[2]=factor;
      MogrifyImage(resource_info->image_info,3,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case AnnotateCommand:
    {
      /*
        Annotate the image with text.
      */
      status=XAnnotateEditImage(display,resource_info,windows,*image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to annotate X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case DrawCommand:
    {
      /*
        Draw image.
      */
      status=XDrawEditImage(display,resource_info,windows,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to draw on the X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case ColorCommand:
    {
      /*
        Color edit.
      */
      status=XColorEditImage(display,resource_info,windows,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to pixel edit X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case MatteCommand:
    {
      /*
        Matte edit.
      */
      status=XMatteEditImage(display,resource_info,windows,image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to matte edit X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case CompositeCommand:
    {
      /*
        Composite image.
      */
      status=XCompositeImage(display,resource_info,windows,*image);
      if (status == False)
        {
          XNoticeWidget(display,windows,"Unable to composite X image",
            (*image)->filename);
          break;
        }
      break;
    }
    case AddBorderCommand:
    {
      static char
        geometry[MaxTextExtent] = "6x6";

      /*
        Query user for border color and geometry.
      */
      XColorBrowserWidget(display,windows,"Select",color);
      if (*color == '\0')
        break;
      (void) XDialogWidget(display,windows,"Add Border",
        "Enter border geometry:",geometry);
      if (*geometry == '\0')
        break;
      /*
        Add a border to the image.
      */
      (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-bordercolor";
      argv[2]=color;
      argv[3]="-border";
      argv[4]=geometry;
      MogrifyImage(resource_info->image_info,5,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case AddFrameCommand:
    {
      static char
        geometry[MaxTextExtent] = "6x6";

      /*
        Query user for frame color and geometry.
      */
      XColorBrowserWidget(display,windows,"Select",color);
      if (*color == '\0')
        break;
      (void) XDialogWidget(display,windows,"Add Frame","Enter frame geometry:",
        geometry);
      if (*geometry == '\0')
        break;
      /*
        Surround image with an ornamental border.
      */
      (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      argv[1]="-mattecolor";
      argv[2]=color;
      argv[3]="-frame";
      argv[4]=geometry;
      MogrifyImage(resource_info->image_info,5,argv,image);
      XSetCursorState(display,windows,False);
      if (windows->image.orphan)
        break;
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      XConfigureImageColormap(display,resource_info,windows,*image);
      (void) XConfigureImage(display,resource_info,windows,*image);
      break;
    }
    case CommentCommand:
    {
      /*
        Edit image comment.
      */
      TemporaryFilename(image_info.filename);
      if ((*image)->comments != (char *) NULL)
        {
          FILE
            *file;

          register char
            *p;

          file=fopen(image_info.filename,WriteBinaryType);
          if (file == (FILE *) NULL)
            {
              XNoticeWidget(display,windows,"Unable to edit image comment",
                image_info.filename);
              break;
            }
          for (p=(*image)->comments; *p != '\0'; p++)
            (void) putc((int) *p,file);
          (void) putc('\n',file);
          (void) fclose(file);
        }
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      status=InvokeDelegate(&image_info,*image,"edit",(char *) NULL);
      if (status != False)
        XNoticeWidget(display,windows,"Unable to edit image comment",
          (char *) NULL);
      else
        {
          char
            command[MaxTextExtent];

          FormatString(command,"@%.1024s",image_info.filename);
          CommentImage(*image,command);
        }
      (void) remove(image_info.filename);
      XSetCursorState(display,windows,False);
      break;
    }
    case LaunchCommand:
    {
      /*
        Launch program.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      (void) strcpy((*image)->magick,"LAUNCH");
      TemporaryFilename((*image)->filename);
      status=WriteImage(&image_info,*image);
      if (status != False)
        XNoticeWidget(display,windows,"Unable to launch image editor",
          (char *) NULL);
      else
        {
          loaded_image=ReadImage(resource_info->image_info);
          XClientMessage(display,windows->image.id,windows->im_protocols,
            windows->im_next_image,CurrentTime);
        }
      (void) remove((*image)->filename);
      XSetCursorState(display,windows,False);
      break;
    }
    case RegionofInterestCommand:
    {
      /*
        Apply an image processing technique to a region of interest.
      */
      (void) XROIImage(display,resource_info,windows,image);
      break;
    }
    case InfoCommand:
      break;
    case ZoomCommand:
    {
      /*
        Zoom image.
      */
      if (windows->magnify.mapped)
        XRaiseWindow(display,windows->magnify.id);
      else
        {
          /*
            Make magnify image.
          */
          XSetCursorState(display,windows,True);
          XMapRaised(display,windows->magnify.id);
          XSetCursorState(display,windows,False);
        }
      break;
    }
    case ShowPreviewCommand:
    {
      static char
        preview_type[MaxTextExtent] = "Gamma";

      register int
        i;

      /*
        Select preview type from menu.
      */
      XListBrowserWidget(display,windows,&windows->widget,PreviewTypes,
        "Preview","Select an enhancement, effect, or F/X:",preview_type);
      if (*preview_type == '\0')
        break;
      for (i=0; PreviewTypes[i] != (char *) NULL; i++)
        if (Latin1Compare(PreviewTypes[i],preview_type) == 0)
          break;
      if (PreviewTypes[i] == (char *) NULL)
        {
          XNoticeWidget(display,windows,"unknown preview type",preview_type);
          break;
        }
      /*
        Show image preview.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      image_info.preview_type=(PreviewType) (i+1);
      image_info.group=windows->image.id;
      LabelImage(*image,"Preview");
      TemporaryFilename((*image)->filename);
      status=WritePREVIEWImage(&image_info,*image);
      (void) strcpy((*image)->magick,"SHOW");
      status=WriteImage(&image_info,*image);
      if (status)
        XNoticeWidget(display,windows,"Unable to show image preview",
          (*image)->filename);
      XDelay(display,1500);
      XSetCursorState(display,windows,False);
      break;
    }
    case ShowHistogramCommand:
    {
      /*
        Show image histogram.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      image_info.group=windows->image.id;
      LabelImage(*image,"Histogram");
      TemporaryFilename((*image)->filename);
      status=WriteHISTOGRAMImage(&image_info,*image);
      (void) strcpy((*image)->magick,"SHOW");
      status=WriteImage(&image_info,*image);
      if (status)
        XNoticeWidget(display,windows,"Unable to show histogram",
          (*image)->filename);
      XDelay(display,1500);
      XSetCursorState(display,windows,False);
      break;
    }
    case ShowMatteCommand:
    {
      if (!(*image)->matte)
        {
          XNoticeWidget(display,windows,
            "Image does not have any matte information",(*image)->filename);
          break;
        }
      /*
        Show image matte.
      */
      XSetCursorState(display,windows,True);
      XCheckRefreshWindows(display,windows);
      image_info.group=windows->image.id;
      LabelImage(*image,"Matte");
      TemporaryFilename((*image)->filename);
      status=WriteMATTEImage(&image_info,*image);
      (void) strcpy((*image)->magick,"SHOW");
      status=WriteImage(&image_info,*image);
      if (status)
        XNoticeWidget(display,windows,"Unable to show histogram",
          (*image)->filename);
      XDelay(display,1500);
      XSetCursorState(display,windows,False);
      break;
    }
    case BackgroundCommand:
    {
      /*
        Background image.
      */
      status=XBackgroundImage(display,resource_info,windows,image);
      if (status == False)
        break;
      (*image)->orphan=True;
      loaded_image=CloneImage(*image,(*image)->columns,(*image)->rows,True);
      (*image)->orphan=False;
      if (loaded_image != (Image *) NULL)
        XClientMessage(display,windows->image.id,windows->im_protocols,
          windows->im_next_image,CurrentTime);
      break;
    }
    case SlideShowCommand:
    {
      static char
        delay[MaxTextExtent] = "5";

      /*
        Display next image after pausing.
      */
      resource_info->delay=0;
      (void) XDialogWidget(display,windows,"Slide Show",
        "Pause how many 1/100ths of a second between images:",delay);
      if (*delay == '\0')
        break;
      resource_info->delay=atoi(delay);
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_next_image,CurrentTime);
      break;
    }
    case PreferencesCommand:
    {
      /*
        Set user preferences.
      */
      status=XPreferencesWidget(display,resource_info,windows);
      if (status == False)
        break;
      (*image)->orphan=True;
      loaded_image=CloneImage(*image,(*image)->columns,(*image)->rows,True);
      (*image)->orphan=False;
      if (loaded_image != (Image *) NULL)
        XClientMessage(display,windows->image.id,windows->im_protocols,
          windows->im_next_image,CurrentTime);
      break;
    }
    case HelpCommand:
    {
      /*
        User requested help.
      */
      XTextViewWidget(display,resource_info,windows,False,
        "Help Viewer - Display",DisplayHelp);
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
      status=InvokeDelegate(&image_info,*image,"browse",(char *) NULL);
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
    case SaveToUndoBufferCommand:
      break;
    default:
    {
      XBell(display,0);
      break;
    }
  }
  return(loaded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X M a g n i f y I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XMagnifyImage magnifies portions of the image as indicated
%  by the pointer.  The magnified portion is displayed in a separate window.
%
%  The format of the XMagnifyImage method is:
%
%    XMagnifyImage(display,windows,event)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o event: Specifies a pointer to a XEvent structure.  If it is NULL,
%      the entire image is refreshed.
%
%
*/
static void XMagnifyImage(Display *display,XWindows *windows,XEvent *event)
{
  char
    text[MaxTextExtent];

  register int
    x,
    y;

  unsigned long
    state;

  /*
    Update magnified image until the mouse button is released.
  */
  XDefineCursor(display,windows->image.id,windows->magnify.cursor);
  state=DefaultState;
  x=event->xbutton.x;
  y=event->xbutton.y;
  windows->magnify.x=windows->image.x+x;
  windows->magnify.y=windows->image.y+y;
  do
  {
    /*
      Map and unmap Info widget as text cursor crosses its boundaries.
    */
    if (windows->info.mapped)
      {
        if ((x < (int) (windows->info.x+windows->info.width)) &&
            (y < (int) (windows->info.y+windows->info.height)))
          XWithdrawWindow(display,windows->info.id,windows->info.screen);
      }
    else
      if ((x > (int) (windows->info.x+windows->info.width)) ||
          (y > (int) (windows->info.y+windows->info.height)))
        XMapWindow(display,windows->info.id);
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",windows->magnify.x,windows->magnify.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,event);
    switch (event->type)
    {
      case ButtonPress:
        break;
      case ButtonRelease:
      {
        /*
          User has finished magnifying image.
        */
        x=event->xbutton.x;
        y=event->xbutton.y;
        state|=ExitState;
        break;
      }
      case Expose:
        break;
      case MotionNotify:
      {
        x=event->xmotion.x;
        y=event->xmotion.y;
        break;
      }
      default:
        break;
    }
    /*
      Check boundary conditions.
    */
    if (x < 0)
      x=0;
    else
      if (x >= (int) windows->image.width)
        x=windows->image.width-1;
    if (y < 0)
      y=0;
    else
     if (y >= (int) windows->image.height)
       y=windows->image.height-1;
  } while (!(state & ExitState));
  /*
    Display magnified image.
  */
  XSetCursorState(display,windows,False);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X M a g n i f y W i n d o w C o m m a n d                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XMagnifyWindowCommand moves the image within an Magnify window by
%  one pixel as specified by the key symbol.
%
%  The format of the XMagnifyWindowCommand method is:
%
%    XMagnifyWindowCommand(display,windows,state,key_symbol)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o state: key mask.
%
%    o key_symbol: Specifies a KeySym which indicates which side of the image
%      to trim.
%
%
*/
static void XMagnifyWindowCommand(Display *display,XWindows *windows,
  const unsigned int state,const KeySym key_symbol)
{
  unsigned int
    quantum;

  /*
    User specified a magnify factor or position.
  */
  quantum=1;
  if (state & Mod1Mask)
    quantum=10;
  switch (key_symbol)
  {
    case QuitCommand:
    {
      XWithdrawWindow(display,windows->magnify.id,windows->magnify.screen);
      break;
    }
    case XK_Home:
    case XK_KP_Home:
    {
      windows->magnify.x=windows->image.width >> 1;
      windows->magnify.y=windows->image.height >> 1;
      break;
    }
    case XK_Left:
    case XK_KP_Left:
    {
      if (windows->magnify.x > 0)
        windows->magnify.x-=quantum;
      break;
    }
    case XK_Up:
    case XK_KP_Up:
    {
      if (windows->magnify.y > 0)
        windows->magnify.y-=quantum;
      break;
    }
    case XK_Right:
    case XK_KP_Right:
    {
      if (windows->magnify.x < (int) (windows->image.width-1))
        windows->magnify.x+=quantum;
      break;
    }
    case XK_Down:
    case XK_KP_Down:
    {
      if (windows->magnify.y < (int) (windows->image.height-1))
        windows->magnify.y+=quantum;
      break;
    }
    case XK_0:
    case XK_1:
    case XK_2:
    case XK_3:
    case XK_4:
    case XK_5:
    case XK_6:
    case XK_7:
    case XK_8:
    case XK_9:
    {
      windows->magnify.data=(unsigned int) (key_symbol-XK_0);
      break;
    }
    case XK_KP_0:
    case XK_KP_1:
    case XK_KP_2:
    case XK_KP_3:
    case XK_KP_4:
    case XK_KP_5:
    case XK_KP_6:
    case XK_KP_7:
    case XK_KP_8:
    case XK_KP_9:
    {
      windows->magnify.data=(unsigned int) (key_symbol-XK_KP_0);
      break;
    }
    default:
      break;
  }
  XMakeMagnifyImage(display,windows);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X M a k e P a n I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XMakePanImage creates a thumbnail of the image and displays it in
%  the Pan icon window.
%
%  The format of the XMakePanImage method is:
%
%      XMakePanImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static void XMakePanImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image)
{
  unsigned int
    status;

  /*
    Create and display image for panning icon.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  windows->pan.x=windows->image.x;
  windows->pan.y=windows->image.y;
  status=XMakeImage(display,resource_info,&windows->pan,image,
    windows->pan.width,windows->pan.height);
  if (status == False)
    MagickWarning(XServerWarning,"Unable to create Pan icon image",
      (char *) NULL);
  XSetWindowBackgroundPixmap(display,windows->pan.id,windows->pan.pixmap);
  XClearWindow(display,windows->pan.id);
  XDrawPanRectangle(display,windows);
  XSetCursorState(display,windows,False);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X M a t t a E d i t I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XMatteEditImage allows the user to interactively change
%  the Matte channel of an image.  If the image is PseudoClass it is promoted
%  to DirectClass before the matte information is stored.
%
%  The format of the XMatteEditImage method is:
%
%    XMatteEditImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
*/
static unsigned int XMatteEditImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows,Image **image)
{
  static char
    matte[MaxTextExtent] = "0";

  static const char
    *MatteEditMenu[]=
    {
      "Method",
      "Border Color",
      "Fuzz",
      "Matte Value",
      "Undo",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static const ModeType
    MatteEditCommands[]=
    {
      MatteEditMethod,
      MatteEditBorderCommand,
      MatteEditFuzzCommand,
      MatteEditValueCommand,
      MatteEditUndoCommand,
      MatteEditHelpCommand,
      MatteEditDismissCommand
    };

  static PaintMethod
    method = PointMethod;

  static XColor
    border_color = { 0, 0, 0, 0, 0 };

  char
    command[MaxTextExtent],
    text[MaxTextExtent];

  Cursor
    cursor;

  int
    entry,
    id,
    x,
    x_offset,
    y,
    y_offset;

  register int
    i;

  register RunlengthPacket
    *p;

  unsigned int
    height,
    width;

  unsigned long
    state;

  XEvent
    event;

  /*
    Map Command widget.
  */
  windows->command.name="Matte Edit";
  windows->command.data=3;
  (void) XCommandWidget(display,windows,MatteEditMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Make cursor.
  */
  cursor=XMakeCursor(display,windows->image.id,windows->map_info->colormap,
    resource_info->background_color,resource_info->foreground_color);
  XDefineCursor(display,windows->image.id,cursor);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",x+windows->image.x,y+windows->image.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,MatteEditMenu,&event);
        if (id < 0)
          {
            XDefineCursor(display,windows->image.id,cursor);
            continue;
          }
        switch (MatteEditCommands[id])
        {
          case MatteEditMethod:
          {
            static const char
              *MethodMenu[]=
              {
                "point",
                "replace",
                "floodfill",
                "filltoborder",
                "reset",
                (char *) NULL,
              };

            /*
              Select a method from the pop-up menu.
            */
            entry=
              XMenuWidget(display,windows,MatteEditMenu[id],MethodMenu,command);
            if (entry >= 0)
              method=(PaintMethod) entry;
            break;
          }
          case MatteEditBorderCommand:
          {
            char
              *ColorMenu[MaxNumberPens];

            int
              pen_number;

            /*
              Initialize menu selections.
            */
            for (i=0; i < (int) (MaxNumberPens-2); i++)
              ColorMenu[i]=resource_info->pen_colors[i];
            ColorMenu[MaxNumberPens-2]="Browser...";
            ColorMenu[MaxNumberPens-1]=(char *) NULL;
            /*
              Select a pen color from the pop-up menu.
            */
            pen_number=XMenuWidget(display,windows,MatteEditMenu[id],
              (const char **) ColorMenu,command);
            if (pen_number < 0)
              break;
            if (pen_number == (MaxNumberPens-2))
              {
                static char
                  color_name[MaxTextExtent] = "gray";

                /*
                  Select a pen color from a dialog.
                */
                resource_info->pen_colors[pen_number]=color_name;
                XColorBrowserWidget(display,windows,"Select",color_name);
                if (*color_name == '\0')
                  break;
              }
            /*
              Set border color.
            */
            (void) XParseColor(display,windows->map_info->colormap,
              resource_info->pen_colors[pen_number],&border_color);
            break;
          }
          case MatteEditFuzzCommand:
          {
            static char
              fuzz[MaxTextExtent];

            static const char
              *FuzzMenu[]=
              {
                "0",
                "2",
                "4",
                "8",
                "16",
                (char *) NULL,
                (char *) NULL,
              };

            /*
              Select a command from the pop-up menu.
            */
            FuzzMenu[5]="Dialog...";
            entry=XMenuWidget(display,windows,MatteEditMenu[id],FuzzMenu,
              command);
            if (entry < 0)
              break;
            if (entry != 5)
              {
                (*image)->fuzz=atoi(FuzzMenu[entry]);
                break;
              }
            (void) sprintf(fuzz,"%d",(*image)->fuzz);
            (void) XDialogWidget(display,windows,"Ok","Enter fuzz factor:",
              fuzz);
            if (*fuzz == '\0')
              break;
            (*image)->fuzz=atoi(fuzz);
            break;
          }
          case MatteEditValueCommand:
          {
            /*
              Request matte value from the user.
            */
            (void) XDialogWidget(display,windows,"Matte","Enter matte value:",
              matte);
            break;
          }
          case MatteEditUndoCommand:
          {
            (void) XMagickCommand(display,resource_info,windows,UndoCommand,
              image);
            break;
          }
          case MatteEditHelpCommand:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Matte Edit",ImageMatteEditHelp);
            break;
          }
          case MatteEditDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
        XDefineCursor(display,windows->image.id,cursor);
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.button != Button1)
          break;
        if ((event.xbutton.window != windows->image.id) &&
            (event.xbutton.window != windows->magnify.id))
          break;
        /*
          Update matte data.
        */
        x=event.xbutton.x;
        y=event.xbutton.y;
        (void) XMagickCommand(display,resource_info,windows,
          SaveToUndoBufferCommand,image);
        state|=UpdateConfigurationState;
        break;
      }
      case ButtonRelease:
      {
        if (event.xbutton.button != Button1)
          break;
        if ((event.xbutton.window != windows->image.id) &&
            (event.xbutton.window != windows->magnify.id))
          break;
        /*
          Update colormap information.
        */
        x=event.xbutton.x;
        y=event.xbutton.y;
        XConfigureImageColormap(display,resource_info,windows,*image);
        (void) XConfigureImage(display,resource_info,windows,*image);
        XInfoWidget(display,windows,text);
        XDefineCursor(display,windows->image.id,cursor);
        state&=(~UpdateConfigurationState);
        break;
      }
      case Expose:
        break;
      case KeyPress:
      {
        char
          command[MaxTextExtent];

        KeySym
          key_symbol;

        if (event.xkey.window == windows->magnify.id)
          {
            Window
              window;

            window=windows->magnify.id;
            while (XCheckWindowEvent(display,window,KeyPressMask,&event));
          }
        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Matte Edit",ImageMatteEditHelp);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        break;
      }
      default:
        break;
    }
    if (event.xany.window == windows->magnify.id)
      {
        x=windows->magnify.x-windows->image.x;
        y=windows->magnify.y-windows->image.y;
      }
    x_offset=x;
    y_offset=y;
    if (state & UpdateConfigurationState)
      {
        int
          x,
          y;

        /*
          Matte edit is relative to image configuration.
        */
        XClearArea(display,windows->image.id,x_offset,y_offset,1,1,True);
        XPutPixel(windows->image.ximage,x_offset,y_offset,
          windows->pixel_info->background_color.pixel);
        width=(*image)->columns;
        height=(*image)->rows;
        x=0;
        y=0;
        if (windows->image.crop_geometry != (char *) NULL)
          (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
            &width,&height);
        x_offset=
          width*(windows->image.x+x_offset)/windows->image.ximage->width+x;
        y_offset=
          height*(windows->image.y+y_offset)/windows->image.ximage->height+y;
        if ((x_offset < 0) || (y_offset < 0))
          continue;
        if ((x_offset >= (int) (*image)->columns) ||
            (y_offset >= (int) (*image)->rows))
          continue;
        (*image)->class=DirectClass;
        if (!(*image)->matte)
          MatteImage(*image);
        switch (method)
        {
          case PointMethod:
          default:
          {
            /*
              Update matte information using point algorithm.
            */
            if (!UncondenseImage(*image))
              break;
            p=(*image)->pixels+(y_offset*(*image)->columns+x_offset);
            p->index=atoi(matte) & 0xff;
            break;
          }
          case ReplaceMethod:
          {
            RunlengthPacket
              target;

            /*
              Update matte information using replace algorithm.
            */
            x=0;
            p=(*image)->pixels;
            for (i=0; i < (int) (*image)->packets; i++)
            {
              x+=(p->length+1);
              if (x > (int) (y_offset*(*image)->columns+x_offset))
                break;
              p++;
            }
            target=(*image)->pixels[i];
            p=(*image)->pixels;
            for (i=0; i < (int) (*image)->packets; i++)
            {
              if (ColorMatch(*p,target,(*image)->fuzz))
                p->index=atoi(matte) & 0xff;
              p++;
            }
            break;
          }
          case FloodfillMethod:
          case FillToBorderMethod:
          {
            RunlengthPacket
              target;

            /*
              Update matte information using floodfill algorithm.
            */
            if (!UncondenseImage(*image))
              break;
            target=(*image)->pixels[y_offset*(*image)->columns+x_offset];
            if (method == FillToBorderMethod)
              {
                target.red=XDownScale(border_color.red);
                target.green=XDownScale(border_color.green);
                target.blue=XDownScale(border_color.blue);
              }
            MatteFloodfillImage(*image,&target,atoi(matte) & 0xff,x_offset,
              y_offset,method);
            break;
          }
          case ResetMethod:
          {
            /*
              Update matte information using reset algorithm.
            */
            p=(*image)->pixels;
            for (i=0; i < (int) (*image)->packets; i++)
            {
              p->index=atoi(matte) & 0xff;
              p++;
            }
            if ((atoi(matte) & 0xff) == Opaque)
              (*image)->matte=False;
            break;
          }
        }
        state&=(~UpdateConfigurationState);
      }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  XSetCursorState(display,windows,False);
  XFreeCursor(display,cursor);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X O p e n I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XOpenBlob loads an image from a file.
%
%  The format of the XOpenBlob method is:
%
%    loaded_image=XOpenBlob(display,resource_info,windows,command)
%
%  A description of each parameter follows:
%
%    o loaded_image: Method XOpenBlob returns an image if can be loaded
%      successfully.  Otherwise a null image is returned.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o command: A value other than zero indicates that the file is selected
%      from the command line argument list.
%
%
*/
static Image *XOpenBlob(Display *display,XResourceInfo *resource_info,
  XWindows *windows,const unsigned int command)
{
  Image
    *loaded_image;

  ImageInfo
    image_info;

  MonitorHandler
    handler;

  static char
    filename[MaxTextExtent] = "\0";

  /*
    Request file name from user.
  */
  if (!command)
    XFileBrowserWidget(display,windows,"Open",filename);
  else
    {
      char
        **filelist,
        **files;

      int
        count,
        status;

      register int
        i,
        j;

      /*
        Select next image from the command line.
      */
      status=XGetCommand(display,windows->image.id,&files,&count);
      if (!status)
        {
          MagickWarning(XServerWarning,"Unable to select image",
            "XGetCommand failed");
          return((Image *) NULL);
        }
      filelist=(char **) AllocateMemory(count*sizeof(char *));
      if (filelist == (char **) NULL)
        {
          MagickWarning(ResourceLimitWarning,"Unable to select image",
            "Memory allocation failed");
          XFreeStringList(files);
          return((Image *) NULL);
        }
      j=0;
      for (i=1; i < count; i++)
        if (*files[i] != '-')
          filelist[j++]=files[i];
      filelist[j]=(char *) NULL;
      XListBrowserWidget(display,windows,&windows->widget,
        (const char **) filelist,"Load","Select Image to Load:",filename);
      FreeMemory((char *) filelist);
      XFreeStringList(files);
    }
  if (*filename == '\0')
    return((Image *) NULL);
  GetImageInfo(&image_info);
  (void) strcpy(image_info.filename,filename);
  SetImageInfo(&image_info,False);
  if (Latin1Compare(image_info.magick,"X") == 0)
    {
      char
        seconds[MaxTextExtent];

      /*
        User may want to delay the X server screen grab.
      */
      (void) strcpy(seconds,"0");
      (void) XDialogWidget(display,windows,"Grab","Enter any delay in seconds:",
        seconds);
      if (*seconds == '\0')
        return((Image *) NULL);
      XDelay(display,1000*atoi(seconds));
    }
  if ((Latin1Compare(image_info.magick,"CMYK") == 0) ||
      (Latin1Compare(image_info.magick,"GRAY") == 0) ||
      (Latin1Compare(image_info.magick,"MAP") == 0) ||
      (Latin1Compare(image_info.magick,"MATTE") == 0) ||
      (Latin1Compare(image_info.magick,"RGB") == 0) ||
      (Latin1Compare(image_info.magick,"TEXT") == 0) ||
      (Latin1Compare(image_info.magick,"TILE") == 0) ||
      (Latin1Compare(image_info.magick,"UYVY") == 0) ||
      (Latin1Compare(image_info.magick,"XC") == 0) ||
      (Latin1Compare(image_info.magick,"YUV") == 0))
    {
      static char
        geometry[MaxTextExtent] = "512x512";

      /*
        Request image size from the user.
      */
      if (resource_info->image_info->size != (char *) NULL)
        (void) strcpy(geometry,resource_info->image_info->size);
      (void) CloneString(&resource_info->image_info->size,geometry);
      (void) XDialogWidget(display,windows,"Load",
        "Enter the image geometry:",geometry);
    }
  /*
    Load the image.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  (void) strcpy(resource_info->image_info->filename,filename);
  handler=(MonitorHandler) NULL;
  if (Latin1Compare(image_info.magick,"X") == 0)
    handler=SetMonitorHandler((MonitorHandler) NULL);
  loaded_image=ReadImage(resource_info->image_info);
  if (Latin1Compare(image_info.magick,"X") == 0)
    (void) SetMonitorHandler(handler);
  XSetCursorState(display,windows,False);
  if (loaded_image != (Image *) NULL)
    XClientMessage(display,windows->image.id,windows->im_protocols,
      windows->im_next_image,CurrentTime);
  else
    {
      char
        *text,
        **textlist;

      FILE
        *file;

      int
        c;

      register char
        *p;

      unsigned int
        length;

      /*
        Unknown image format.
      */
      file=(FILE *) fopen(filename,"r");
      if (file == (FILE *) NULL)
        return((Image *) NULL);
      length=MaxTextExtent;
      text=(char *) AllocateMemory(length*sizeof(char));
      for (p=text ; text != (char *) NULL; p++)
      {
        c=fgetc(file);
        if (c == EOF)
          break;
        if ((p-text+1) >= (int) length)
          {
            *p='\0';
            length<<=1;
            text=(char *) ReallocateMemory((char *) text,length*sizeof(char));
            if (text == (char *) NULL)
              break;
            p=text+Extent(text);
          }
        *p=(unsigned char) c;
      }
      (void) fclose(file);
      if (text == (char *) NULL)
        return((Image *) NULL);
      *p='\0';
      textlist=StringToList(text);
      if (textlist != (char **) NULL)
        {
          char
            title[MaxTextExtent];

          register int
            i;

          FormatString(title,"Unknown format: %.1024s",filename);
          XTextViewWidget(display,resource_info,windows,True,title,
            (const char **) textlist);
          for (i=0; textlist[i] != (char *) NULL; i++)
            FreeMemory((char *) textlist[i]);
          FreeMemory((char *) textlist);
        }
      FreeMemory((char *) text);
    }
  return(loaded_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X P a n I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XPanImage pans the image until the mouse button is released.
%
%  The format of the XPanImage method is:
%
%    XPanImage(display,windows,event)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o event: Specifies a pointer to a XEvent structure.  If it is NULL,
%      the entire image is refreshed.
%
*/
static void XPanImage(Display *display,XWindows *windows,XEvent *event)
{
  char
    text[MaxTextExtent];

  Cursor
    cursor;

  RectangleInfo
    pan_info;

  unsigned long
    state,
    x_factor,
    y_factor;

  /*
    Define cursor.
  */
  if ((windows->image.ximage->width > (int) windows->image.width) &&
      (windows->image.ximage->height > (int) windows->image.height))
    cursor=XCreateFontCursor(display,XC_fleur);
  else
    if (windows->image.ximage->width > (int) windows->image.width)
      cursor=XCreateFontCursor(display,XC_sb_h_double_arrow);
    else
      if (windows->image.ximage->height > (int) windows->image.height)
        cursor=XCreateFontCursor(display,XC_sb_v_double_arrow);
      else
        cursor=XCreateFontCursor(display,XC_arrow);
  XDefineCursor(display,windows->pan.id,cursor);
  /*
    Pan image as pointer moves until the mouse button is released.
  */
  x_factor=(unsigned long)
    UpShift(windows->image.ximage->width)/windows->pan.width;
  y_factor=(unsigned long)
    UpShift(windows->image.ximage->height)/windows->pan.height;
  pan_info.width=
    windows->pan.width*windows->image.width/windows->image.ximage->width;
  pan_info.height=
    windows->pan.height*windows->image.height/windows->image.ximage->height;
  state=UpdateConfigurationState;
  do
  {
    switch (event->type)
    {
      case ButtonPress:
      {
        /*
          User choose an initial pan location.
        */
        pan_info.x=event->xbutton.x;
        pan_info.y=event->xbutton.y;
        state|=UpdateConfigurationState;
        break;
      }
      case ButtonRelease:
      {
        /*
          User has finished panning the image.
        */
        pan_info.x=event->xbutton.x;
        pan_info.y=event->xbutton.y;
        state|=UpdateConfigurationState | ExitState;
        break;
      }
      case MotionNotify:
      {
        pan_info.x=event->xmotion.x;
        pan_info.y=event->xmotion.y;
        state|=UpdateConfigurationState;
      }
      default:
        break;
    }
    if (state & UpdateConfigurationState)
      {
        /*
          Check boundary conditions.
        */
        if (pan_info.x < (int) (pan_info.width >> 1))
          pan_info.x=0;
        else
          pan_info.x=DownShift((pan_info.x-(pan_info.width >> 1))*x_factor);
        if (pan_info.x < 0)
          pan_info.x=0;
        else
          if ((int) (pan_info.x+windows->image.width) >
              windows->image.ximage->width)
            pan_info.x=windows->image.ximage->width-windows->image.width;
        if (pan_info.y < (int) (pan_info.height >> 1))
          pan_info.y=0;
        else
          pan_info.y=DownShift((pan_info.y-(pan_info.height >> 1))*y_factor);
        if (pan_info.y < 0)
          pan_info.y=0;
        else
          if ((int) (pan_info.y+windows->image.height) >
              windows->image.ximage->height)
            pan_info.y=windows->image.ximage->height-windows->image.height;
        if ((windows->image.x != pan_info.x) ||
            (windows->image.y != pan_info.y))
          {
            /*
              Display image pan offset.
            */
            windows->image.x=pan_info.x;
            windows->image.y=pan_info.y;
            FormatString(text," %ux%u%+d%+d ",windows->image.width,
              windows->image.height,windows->image.x,windows->image.y);
            XInfoWidget(display,windows,text);
            /*
              Refresh Image window.
            */
            XDrawPanRectangle(display,windows);
            XRefreshWindow(display,&windows->image,(XEvent *) NULL);
          }
        state&=(~UpdateConfigurationState);
      }
    /*
      Wait for next event.
    */
    if (!(state & ExitState))
      XScreenEvent(display,windows,event);
  } while (!(state & ExitState));
  /*
    Restore cursor.
  */
  XDefineCursor(display,windows->pan.id,windows->pan.cursor);
  XFreeCursor(display,cursor);
  XWithdrawWindow(display,windows->info.id,windows->info.screen);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X P a s t e I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XPasteImage pastes an image previously saved with XCropImage
%  in the X window image at a location the user chooses with the pointer.
%
%  The format of the XPasteImage method is:
%
%    status=XPasteImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XPasteImage returns True if the image is
%      pasted.  False is returned is there is a memory shortage or if the
%      image fails to be pasted.
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
*/
static unsigned int XPasteImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image)
{
  static const char
    *PasteMenu[]=
    {
      "Operator",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static const ModeType
    PasteCommands[]=
    {
      PasteOperatorsCommand,
      PasteHelpCommand,
      PasteDismissCommand
    };

  static CompositeOperator
    operation = ReplaceCompositeOp;

  char
    text[MaxTextExtent];

  Cursor
    cursor;

  Image
    *paste_image;

  int
    id,
    x,
    y;

  RectangleInfo
    highlight_info,
    paste_info;

  unsigned int
    height,
    width;

  unsigned long
    scale_factor,
    state;

  XEvent
    event;

  /*
    Copy image.
  */
  if (resource_info->copy_image == (Image *) NULL)
    return(False);
  resource_info->copy_image->orphan=True;
  paste_image=CloneImage(resource_info->copy_image,resource_info->copy_image->columns,resource_info->copy_image->rows,True);
  resource_info->copy_image->orphan=False;
  /*
    Map Command widget.
  */
  windows->command.name="Paste";
  windows->command.data=1;
  (void) XCommandWidget(display,windows,PasteMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Track pointer until button 1 is pressed.
  */
  XSetCursorState(display,windows,False);
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  paste_info.x=windows->image.x+x;
  paste_info.y=windows->image.y+y;
  paste_info.width=0;
  paste_info.height=0;
  cursor=XCreateFontCursor(display,XC_ul_angle);
  XSetFunction(display,windows->image.highlight_context,GXinvert);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",paste_info.x,paste_info.y);
        XInfoWidget(display,windows,text);
      }
    highlight_info=paste_info;
    highlight_info.x=paste_info.x-windows->image.x;
    highlight_info.y=paste_info.y-windows->image.y;
    XHighlightRectangle(display,windows->image.id,
      windows->image.highlight_context,&highlight_info);
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    XHighlightRectangle(display,windows->image.id,
      windows->image.highlight_context,&highlight_info);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,PasteMenu,&event);
        if (id < 0)
          continue;
        switch (PasteCommands[id])
        {
          case PasteOperatorsCommand:
          {
            char
              command[MaxTextExtent];

            static const char
              *OperatorMenu[]=
              {
                "Over",
                "In",
                "Out",
                "Atop",
                "Xor",
                "Plus",
                "Minus",
                "Add",
                "Subtract",
                "Difference",
                "Bumpmap",
                "Replace",
                "ReplaceRed",
                "ReplaceGreen",
                "ReplaceBlue",
                "ReplaceMatte",
                (char *) NULL,
              };

            /*
              Select a command from the pop-up menu.
            */
            operation=(CompositeOperator) (XMenuWidget(display,windows,
              PasteMenu[id],OperatorMenu,command)+1);
            break;
          }
          case PasteHelpCommand:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Compositing",ImagePasteHelp);
            break;
          }
          case PasteDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
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
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        /*
          Paste rectangle is relative to image configuration.
        */
        width=image->columns;
        height=image->rows;
        x=0;
        y=0;
        if (windows->image.crop_geometry != (char *) NULL)
          (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
            &width,&height);
        scale_factor=UpShift(windows->image.ximage->width)/width;
        paste_info.width=DownShift(paste_image->columns*scale_factor);
        scale_factor=UpShift(windows->image.ximage->height)/height;
        paste_info.height=DownShift(paste_image->rows*scale_factor);
        XDefineCursor(display,windows->image.id,cursor);
        paste_info.x=windows->image.x+event.xbutton.x;
        paste_info.y=windows->image.y+event.xbutton.y;
        break;
      }
      case ButtonRelease:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Button Release: 0x%lx %u +%d+%d\n",
            event.xbutton.window,event.xbutton.button,event.xbutton.x,
            event.xbutton.y);
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        if ((paste_info.width != 0) && (paste_info.height != 0))
          {
            /*
              User has selected the location of the paste image.
            */
            paste_info.x=windows->image.x+event.xbutton.x;
            paste_info.y=windows->image.y+event.xbutton.y;
            state|=ExitState;
          }
        break;
      }
      case Expose:
        break;
      case KeyPress:
      {
        char
          command[MaxTextExtent];

        KeySym
          key_symbol;

        int
          length;

        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        length=XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        *(command+length)='\0';
        if (resource_info->debug)
          (void) fprintf(stderr,"Key press: 0x%lx (%.1024s)\n",key_symbol,command);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            DestroyImage(paste_image);
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Image Compositing",ImagePasteHelp);
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as text cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        paste_info.x=windows->image.x+x;
        paste_info.y=windows->image.y+y;
        break;
      }
      default:
      {
        if (resource_info->debug)
          (void) fprintf(stderr,"Event type: %d\n",event.type);
        break;
      }
    }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  XSetFunction(display,windows->image.highlight_context,GXcopy);
  XSetCursorState(display,windows,False);
  XFreeCursor(display,cursor);
  if (state & EscapeState)
    return(True);
  /*
    Image pasting is relative to image configuration.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  scale_factor=UpShift(width)/windows->image.ximage->width;
  paste_info.x+=x;
  paste_info.x=DownShift(paste_info.x*scale_factor);
  paste_info.width=DownShift(paste_info.width*scale_factor);
  scale_factor=UpShift(height)/windows->image.ximage->height;
  paste_info.y+=y;
  paste_info.y=DownShift(paste_info.y*scale_factor);
  paste_info.height=DownShift(paste_info.height*scale_factor);
  /*
    Paste image with X Image window.
  */
  CompositeImage(image,operation,paste_image,paste_info.x,paste_info.y);
  DestroyImage(paste_image);
  XSetCursorState(display,windows,False);
  /*
    Update image colormap.
  */
  XConfigureImageColormap(display,resource_info,windows,image);
  (void) XConfigureImage(display,resource_info,windows,image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X P r i n t I m a g e                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XPrintImage prints an image to a Postscript printer.
%
%  The format of the XPrintImage method is:
%
%    status=XPrintImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XPrintImage return False if the image is
%      printed.  True is returned is there is a memory shortage or if the
%      image fails to print.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XPrintImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image)
{
  char
    geometry[MaxTextExtent];

  Image
    *print_image;

  ImageInfo
    *image_info;

  unsigned int
    status;

  /*
    Request Postscript page geometry from user.
  */
  image_info=CloneImageInfo(resource_info->image_info);
  FormatString(geometry,"Letter");
  if (image_info->page != (char *) NULL)
    (void) strcpy(geometry,image_info->page);
  XListBrowserWidget(display,windows,&windows->widget,PageSizes,"Select",
    "Select Postscript Page Geometry:",geometry);
  if (*geometry == '\0')
    return(False);
  image_info->page=PostscriptGeometry(geometry);
  /*
    Apply image transforms.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  image->orphan=True;
  print_image=CloneImage(image,image->columns,image->rows,True);
  image->orphan=False;
  if (print_image == (Image *) NULL)
    return(True);
  FormatString(geometry,"%dx%d!",windows->image.ximage->width,
    windows->image.ximage->height);
  TransformImage(&print_image,windows->image.crop_geometry,geometry);
  if (resource_info->quantize_info->number_colors != 0)
    {
      /*
        Reduce the number of colors in the image.
      */
      if ((print_image->class == DirectClass) ||
          (print_image->colors > resource_info->quantize_info->number_colors) ||
          (resource_info->quantize_info->colorspace == GRAYColorspace))
        (void) QuantizeImage(resource_info->quantize_info,print_image);
      SyncImage(print_image);
    }
  /*
    Print image.
  */
  TemporaryFilename(print_image->magick_filename);
  (void) strcpy(print_image->filename,"print:");
  TemporaryFilename(print_image->filename+strlen("print:"));
  status=WriteImage(image_info,print_image);
  DestroyImage(print_image);
  DestroyImageInfo(image_info);
  XSetCursorState(display,windows,False);
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X R O I I m a g e                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XROIImage applies an image processing technique to a region
%  of interest.
%
%  The format of the XROIImage method is:
%
%    status=XROIImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XROIImage returns True if the image is
%      cropped.  False is returned is there is a memory shortage or if the
%      image fails to be cropped.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%
*/
static unsigned int XROIImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image **image)
{
#define ApplyMenus  7

  static const char
    *ROIMenu[]=
    {
      "Help",
      "Dismiss",
      (char *) NULL
    },
    *ApplyMenu[]=
    {
      "File",
      "Edit",
      "Transform",
      "Enhance",
      "Effects",
      "F/X",
      "Miscellany",
      "Help",
      "Dismiss",
      (char *) NULL
    },
    *FileMenu[]=
    {
      "Save...",
      "Print...",
      (char *) NULL
    },
    *EditMenu[]=
    {
      "Undo",
      "Redo",
      (char *) NULL
    },
    *TransformMenu[]=
    {
      "Flop",
      "Flip",
      "Rotate Right",
      "Rotate Left",
      (char *) NULL
    },
    *EnhanceMenu[]=
    {
      "Hue...",
      "Saturation...",
      "Brightness...",
      "Gamma...",
      "Spiff",
      "Dull",
      "Equalize",
      "Normalize",
      "Negate",
      "Grayscale",
      "Map...",
      "Quantize...",
      (char *) NULL
    },
    *EffectsMenu[]=
    {
      "Despeckle",
      "Emboss",
      "Reduce Noise",
      "Add Noise",
      "Sharpen...",
      "Blur...",
      "Threshold...",
      "Edge Detect...",
      "Spread...",
      "Shade...",
      "Raise...",
      "Segment...",
      (char *) NULL
    },
    *FXMenu[]=
    {
      "Solarize...",
      "Swirl...",
      "Implode...",
      "Wave...",
      "Oil Painting...",
      "Charcoal Drawing...",
      (char *) NULL
    },
    *MiscellanyMenu[]=
    {
      "Image Info",
      "Zoom Image",
      "Show Preview...",
      "Show Histogram",
      "Show Matte",
      (char *) NULL
    };

  static const char
    **Menus[ApplyMenus]=
    {
      FileMenu,
      EditMenu,
      TransformMenu,
      EnhanceMenu,
      EffectsMenu,
      FXMenu,
      MiscellanyMenu
    };

  static const CommandType
    ApplyCommands[]=
    {
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      HelpCommand,
      QuitCommand
    },
    FileCommands[]=
    {
      SaveCommand,
      PrintCommand
    },
    EditCommands[]=
    {
      UndoCommand,
      RedoCommand
    },
    TransformCommands[]=
    {
      FlopCommand,
      FlipCommand,
      RotateRightCommand,
      RotateLeftCommand
    },
    EnhanceCommands[]=
    {
      HueCommand,
      SaturationCommand,
      BrightnessCommand,
      GammaCommand,
      SpiffCommand,
      DullCommand,
      EqualizeCommand,
      NormalizeCommand,
      NegateCommand,
      GrayscaleCommand,
      MapCommand,
      QuantizeCommand
    },
    EffectsCommands[]=
    {
      DespeckleCommand,
      EmbossCommand,
      ReduceNoiseCommand,
      AddNoiseCommand,
      SharpenCommand,
      BlurCommand,
      EdgeDetectCommand,
      SpreadCommand,
      ShadeCommand,
      RaiseCommand,
      SegmentCommand
    },
    FXCommands[]=
    {
      SolarizeCommand,
      SwirlCommand,
      ImplodeCommand,
      WaveCommand,
      OilPaintCommand,
      CharcoalDrawingCommand
    },
    MiscellanyCommands[]=
    {
      InfoCommand,
      ZoomCommand,
      ShowPreviewCommand,
      ShowHistogramCommand,
      ShowMatteCommand
    },
    ROICommands[]=
    {
      ROIHelpCommand,
      ROIDismissCommand
    };

  static const CommandType
    *Commands[ApplyMenus]=
    {
      FileCommands,
      EditCommands,
      TransformCommands,
      EnhanceCommands,
      EffectsCommands,
      FXCommands,
      MiscellanyCommands
    };

  char
    command[MaxTextExtent],
    text[MaxTextExtent];

  CommandType
    command_type;

  Cursor
    cursor;

  Image
    *roi_image;

  int
    entry,
    id,
    x,
    y;

  MonitorHandler
    handler;

  RectangleInfo
    crop_info,
    highlight_info,
    roi_info;

  unsigned int
    height,
    width;

  unsigned long
    scale_factor,
    state;

  XEvent
    event;

  /*
    Map Command widget.
  */
  windows->command.name="ROI";
  windows->command.data=0;
  (void) XCommandWidget(display,windows,ROIMenu,(XEvent *) NULL);
  XMapRaised(display,windows->command.id);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_update_widget,CurrentTime);
  /*
    Track pointer until button 1 is pressed.
  */
  XQueryPosition(display,windows->image.id,&x,&y);
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask |
    PointerMotionMask);
  roi_info.x=windows->image.x+x;
  roi_info.y=windows->image.y+y;
  roi_info.width=0;
  roi_info.height=0;
  cursor=XCreateFontCursor(display,XC_fleur);
  state=DefaultState;
  do
  {
    if (windows->info.mapped)
      {
        /*
          Display pointer position.
        */
        FormatString(text," %+d%+d ",roi_info.x,roi_info.y);
        XInfoWidget(display,windows,text);
      }
    /*
      Wait for next event.
    */
    XScreenEvent(display,windows,&event);
    if (event.xany.window == windows->command.id)
      {
        /*
          Select a command from the Command widget.
        */
        id=XCommandWidget(display,windows,ROIMenu,&event);
        if (id < 0)
          continue;
        switch (ROICommands[id])
        {
          case ROIHelpCommand:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Region of Interest",ImageROIHelp);
            break;
          }
          case ROIDismissCommand:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          default:
            break;
        }
        continue;
      }
    switch (event.type)
    {
      case ButtonPress:
      {
        if (event.xbutton.button != Button1)
          break;
        if (event.xbutton.window != windows->image.id)
          break;
        /*
          Note first corner of region of interest rectangle-- exit loop.
        */
        XDefineCursor(display,windows->image.id,cursor);
        roi_info.x=windows->image.x+event.xbutton.x;
        roi_info.y=windows->image.y+event.xbutton.y;
        state|=ExitState;
        break;
      }
      case ButtonRelease:
        break;
      case Expose:
        break;
      case KeyPress:
      {
        KeySym
          key_symbol;

        if (event.xkey.window != windows->image.id)
          break;
        /*
          Respond to a user key press.
        */
        (void) XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        switch (key_symbol)
        {
          case XK_Escape:
          case XK_F20:
          {
            /*
              Prematurely exit.
            */
            state|=EscapeState;
            state|=ExitState;
            break;
          }
          case XK_F1:
          case XK_Help:
          {
            XTextViewWidget(display,resource_info,windows,False,
              "Help Viewer - Region of Interest",ImageROIHelp);
            break;
          }
          default:
          {
            XBell(display,0);
            break;
          }
        }
        break;
      }
      case MotionNotify:
      {
        /*
          Map and unmap Info widget as text cursor crosses its boundaries.
        */
        x=event.xmotion.x;
        y=event.xmotion.y;
        if (windows->info.mapped)
          {
            if ((x < (int) (windows->info.x+windows->info.width)) &&
                (y < (int) (windows->info.y+windows->info.height)))
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
          }
        else
          if ((x > (int) (windows->info.x+windows->info.width)) ||
              (y > (int) (windows->info.y+windows->info.height)))
            XMapWindow(display,windows->info.id);
        roi_info.x=windows->image.x+x;
        roi_info.y=windows->image.y+y;
        break;
      }
      default:
        break;
    }
  } while (!(state & ExitState));
  XSelectInput(display,windows->image.id,windows->image.attributes.event_mask);
  if (state & EscapeState)
    {
      /*
        User want to exit without region of interest.
      */
      XWithdrawWindow(display,windows->info.id,windows->info.screen);
      XFreeCursor(display,cursor);
      return(True);
    }
  XSetFunction(display,windows->image.highlight_context,GXinvert);
  do
  {
    /*
      Size rectangle as pointer moves until the mouse button is released.
    */
    x=roi_info.x;
    y=roi_info.y;
    roi_info.width=0;
    roi_info.height=0;
    state=DefaultState;
    do
    {
      highlight_info=roi_info;
      highlight_info.x=roi_info.x-windows->image.x;
      highlight_info.y=roi_info.y-windows->image.y;
      if ((highlight_info.width > 3) && (highlight_info.height > 3))
        {
          /*
            Display info and draw region of interest rectangle.
          */
          if (!windows->info.mapped)
            XMapWindow(display,windows->info.id);
          FormatString(text," %ux%u%+d%+d",roi_info.width,roi_info.height,
            roi_info.x,roi_info.y);
          XInfoWidget(display,windows,text);
          XHighlightRectangle(display,windows->image.id,
            windows->image.highlight_context,&highlight_info);
        }
      else
        if (windows->info.mapped)
          XWithdrawWindow(display,windows->info.id,windows->info.screen);
      /*
        Wait for next event.
      */
      XScreenEvent(display,windows,&event);
      if ((highlight_info.width > 3) && (highlight_info.height > 3))
        XHighlightRectangle(display,windows->image.id,
          windows->image.highlight_context,&highlight_info);
      switch (event.type)
      {
        case ButtonPress:
        {
          roi_info.x=windows->image.x+event.xbutton.x;
          roi_info.y=windows->image.y+event.xbutton.y;
          break;
        }
        case ButtonRelease:
        {
          /*
            User has committed to region of interest rectangle.
          */
          roi_info.x=windows->image.x+event.xbutton.x;
          roi_info.y=windows->image.y+event.xbutton.y;
          XSetCursorState(display,windows,False);
          state|=ExitState;
          if (Latin1Compare(windows->command.name,"Apply") == 0)
            break;
          windows->command.name="Apply";
          windows->command.data=ApplyMenus;
          (void) XCommandWidget(display,windows,ApplyMenu,(XEvent *) NULL);
          break;
        }
        case Expose:
          break;
        case MotionNotify:
        {
          roi_info.x=windows->image.x+event.xmotion.x;
          roi_info.y=windows->image.y+event.xmotion.y;
        }
        default:
          break;
      }
      if (((roi_info.x != x) && (roi_info.y != y)) || (state & ExitState))
        {
          /*
            Check boundary conditions.
          */
          if (roi_info.x < 0)
            roi_info.x=0;
          else
            if (roi_info.x > windows->image.ximage->width)
              roi_info.x=windows->image.ximage->width;
          if (roi_info.x < x)
            roi_info.width=(unsigned int) (x-roi_info.x);
          else
            {
              roi_info.width=(unsigned int) (roi_info.x-x);
              roi_info.x=x;
            }
          if (roi_info.y < 0)
            roi_info.y=0;
          else
            if (roi_info.y > windows->image.ximage->height)
              roi_info.y=windows->image.ximage->height;
          if (roi_info.y < y)
            roi_info.height=(unsigned int) (y-roi_info.y);
          else
            {
              roi_info.height=(unsigned int) (roi_info.y-y);
              roi_info.y=y;
            }
        }
    } while (!(state & ExitState));
    /*
      Wait for user to grab a corner of the rectangle or press return.
    */
    state=DefaultState;
    command_type=NullCommand;
    do
    {
      if (windows->info.mapped)
        {
          /*
            Display pointer position.
          */
          FormatString(text," %ux%u%+d%+d",roi_info.width,roi_info.height,
            roi_info.x,roi_info.y);
          XInfoWidget(display,windows,text);
        }
      highlight_info=roi_info;
      highlight_info.x=roi_info.x-windows->image.x;
      highlight_info.y=roi_info.y-windows->image.y;
      if ((highlight_info.width <= 3) || (highlight_info.height <= 3))
        {
          state|=EscapeState;
          state|=ExitState;
          break;
        }
      if (state & UpdateRegionState)
        {
          XSetFunction(display,windows->image.highlight_context,GXcopy);
          switch (command_type)
          {
            case UndoCommand:
            case RedoCommand:
            {
              (void) XMagickCommand(display,resource_info,windows,command_type,
                image);
              break;
            }
            default:
            {
              /*
                Region of interest is relative to image configuration.
              */
              handler=SetMonitorHandler((MonitorHandler) NULL);
              crop_info=roi_info;
              width=(*image)->columns;
              height=(*image)->rows;
              x=0;
              y=0;
              if (windows->image.crop_geometry != (char *) NULL)
                (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
                  &width,&height);
              scale_factor=UpShift(width)/windows->image.ximage->width;
              crop_info.x+=x;
              crop_info.x=DownShift(crop_info.x*scale_factor);
              crop_info.width=DownShift(crop_info.width*scale_factor);
              scale_factor=UpShift(height)/windows->image.ximage->height;
              crop_info.y+=y;
              crop_info.y=DownShift(crop_info.y*scale_factor);
              crop_info.height=DownShift(crop_info.height*scale_factor);
              roi_image=CropImage(*image,&crop_info);
              (void) SetMonitorHandler(handler);
              if (roi_image == (Image *) NULL)
                continue;
              /*
                Apply image processing technique to the region of interest.
              */
              windows->image.orphan=True;
              (void) XMagickCommand(display,resource_info,windows,command_type,
                &roi_image);
              handler=SetMonitorHandler((MonitorHandler) NULL);
              (void) XMagickCommand(display,resource_info,windows,
                SaveToUndoBufferCommand,image);
              windows->image.orphan=False;
              CompositeImage(*image,ReplaceCompositeOp,roi_image,
                crop_info.x,crop_info.y);
              DestroyImage(roi_image);
              (void) SetMonitorHandler(handler);
              break;
            }
          }
          if (command_type != InfoCommand)
            {
              XConfigureImageColormap(display,resource_info,windows,*image);
              (void) XConfigureImage(display,resource_info,windows,*image);
            }
          XCheckRefreshWindows(display,windows);
          XInfoWidget(display,windows,text);
          XSetFunction(display,windows->image.highlight_context,GXinvert);
          state&=(~UpdateRegionState);
        }
      XHighlightRectangle(display,windows->image.id,
        windows->image.highlight_context,&highlight_info);
      XScreenEvent(display,windows,&event);
      if (event.xany.window == windows->command.id)
        {
          /*
            Select a command from the Command widget.
          */
          XSetFunction(display,windows->image.highlight_context,GXcopy);
          command_type=NullCommand;
          id=XCommandWidget(display,windows,ApplyMenu,&event);
          if (id >= 0)
            {
              (void) strcpy(command,ApplyMenu[id]);
              command_type=ApplyCommands[id];
              if (id < ApplyMenus)
                {
                  /*
                    Select a command from a pop-up menu.
                  */
                  entry=XMenuWidget(display,windows,ApplyMenu[id],
                    (const char **) Menus[id],command);
                  if (entry >= 0)
                    {
                      (void) strcpy(command,Menus[id][entry]);
                      command_type=Commands[id][entry];
                    }
                }
            }
          XSetFunction(display,windows->image.highlight_context,GXinvert);
          XHighlightRectangle(display,windows->image.id,
            windows->image.highlight_context,&highlight_info);
          if (command_type == HelpCommand)
            {
              XSetFunction(display,windows->image.highlight_context,GXcopy);
              XTextViewWidget(display,resource_info,windows,False,
                "Help Viewer - Region of Interest",ImageROIHelp);
              XSetFunction(display,windows->image.highlight_context,GXinvert);
              continue;
            }
          if (command_type == QuitCommand)
            {
              /*
                Exit.
              */
              state|=EscapeState;
              state|=ExitState;
              continue;
            }
          if (command_type != NullCommand)
            state|=UpdateRegionState;
          continue;
        }
      XHighlightRectangle(display,windows->image.id,
        windows->image.highlight_context,&highlight_info);
      switch (event.type)
      {
        case ButtonPress:
        {
          x=windows->image.x;
          y=windows->image.y;
          if (event.xbutton.button != Button1)
            break;
          if (event.xbutton.window != windows->image.id)
            break;
          x=windows->image.x+event.xbutton.x;
          y=windows->image.y+event.xbutton.y;
          if ((x < (int) (roi_info.x+RoiDelta)) &&
              (x > (int) (roi_info.x-RoiDelta)) &&
              (y < (int) (roi_info.y+RoiDelta)) &&
              (y > (int) (roi_info.y-RoiDelta)))
            {
              roi_info.x=roi_info.x+roi_info.width;
              roi_info.y=roi_info.y+roi_info.height;
              state|=UpdateConfigurationState;
              break;
            }
          if ((x < (int) (roi_info.x+RoiDelta)) &&
              (x > (int) (roi_info.x-RoiDelta)) &&
              (y < (int) (roi_info.y+roi_info.height+RoiDelta)) &&
              (y > (int) (roi_info.y+roi_info.height-RoiDelta)))
            {
              roi_info.x=roi_info.x+roi_info.width;
              state|=UpdateConfigurationState;
              break;
            }
          if ((x < (int) (roi_info.x+roi_info.width+RoiDelta)) &&
              (x > (int) (roi_info.x+roi_info.width-RoiDelta)) &&
              (y < (int) (roi_info.y+RoiDelta)) &&
              (y > (int) (roi_info.y-RoiDelta)))
            {
              roi_info.y=roi_info.y+roi_info.height;
              state|=UpdateConfigurationState;
              break;
            }
          if ((x < (int) (roi_info.x+roi_info.width+RoiDelta)) &&
              (x > (int) (roi_info.x+roi_info.width-RoiDelta)) &&
              (y < (int) (roi_info.y+roi_info.height+RoiDelta)) &&
              (y > (int) (roi_info.y+roi_info.height-RoiDelta)))
            {
              state|=UpdateConfigurationState;
              break;
            }
        }
        case ButtonRelease:
        {
          if (event.xbutton.window == windows->pan.id)
            if ((highlight_info.x != crop_info.x-windows->image.x) ||
                (highlight_info.y != crop_info.y-windows->image.y))
              XHighlightRectangle(display,windows->image.id,
                windows->image.highlight_context,&highlight_info);
          break;
        }
        case Expose:
        {
          if (event.xexpose.window == windows->image.id)
            if (event.xexpose.count == 0)
              {
                event.xexpose.x=highlight_info.x;
                event.xexpose.y=highlight_info.y;
                event.xexpose.width=highlight_info.width;
                event.xexpose.height=highlight_info.height;
                XRefreshWindow(display,&windows->image,&event);
              }
          if (event.xexpose.window == windows->info.id)
            if (event.xexpose.count == 0)
              XInfoWidget(display,windows,text);
          break;
        }
        case KeyPress:
        {
          KeySym
            key_symbol;

          if (event.xkey.window != windows->image.id)
            break;
          /*
            Respond to a user key press.
          */
          (void) XLookupString((XKeyEvent *) &event.xkey,command,
            sizeof(command),&key_symbol,(XComposeStatus *) NULL);
          switch (key_symbol)
          {
            case XK_Shift_L:
            case XK_Shift_R:
              break;
            case XK_Escape:
            case XK_F20:
              state|=EscapeState;
            case XK_Return:
            {
              state|=ExitState;
              break;
            }
            case XK_F1:
            case XK_Help:
            {
              XSetFunction(display,windows->image.highlight_context,GXcopy);
              XTextViewWidget(display,resource_info,windows,False,
                "Help Viewer - Region of Interest",ImageROIHelp);
              XSetFunction(display,windows->image.highlight_context,GXinvert);
              break;
            }
            default:
            {
              command_type=XImageWindowCommand(display,resource_info,windows,
                event.xkey.state,key_symbol,image);
              if (command_type != NullCommand)
                state|=UpdateRegionState;
              break;
            }
          }
          break;
        }
        case KeyRelease:
          break;
        case MotionNotify:
        {
          if (event.xbutton.window != windows->image.id)
            break;
          /*
            Map and unmap Info widget as text cursor crosses its boundaries.
          */
          x=event.xmotion.x;
          y=event.xmotion.y;
          if (windows->info.mapped)
            {
              if ((x < (int) (windows->info.x+windows->info.width)) &&
                  (y < (int) (windows->info.y+windows->info.height)))
                XWithdrawWindow(display,windows->info.id,windows->info.screen);
            }
          else
            if ((x > (int) (windows->info.x+windows->info.width)) ||
                (y > (int) (windows->info.y+windows->info.height)))
              XMapWindow(display,windows->info.id);
          break;
        }
        default:
          break;
      }
      if (state & UpdateConfigurationState)
        {
          XPutBackEvent(display,&event);
          XDefineCursor(display,windows->image.id,cursor);
          break;
        }
    } while (!(state & ExitState));
  } while (!(state & ExitState));
  XSetFunction(display,windows->image.highlight_context,GXcopy);
  XSetCursorState(display,windows,False);
  if (state & EscapeState)
    return(True);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X R o t a t e I m a g e                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XRotateImage rotates the X image.  If the degrees parameter
%  if zero, the rotation angle is computed from the slope of a line drawn by
%  the user.
%
%  The format of the XRotateImage method is:
%
%    status=XRotateImage(display,resource_info,windows,degrees,image)
%
%  A description of each parameter follows:
%
%    o status: Method XRotateImage return True if the image is
%      rotated.  False is returned is there is a memory shortage or if the
%      image fails to rotate.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o degrees: Specifies the number of degrees to rotate the image.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XRotateImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,double degrees,Image **image)
{
  static const char
    *RotateMenu[]=
    {
      "Pixel Color",
      "Direction",
      "Crop",
      "Sharpen",
      "Help",
      "Dismiss",
      (char *) NULL
    };

  static ModeType
    direction = HorizontalRotateCommand;

  static const ModeType
    DirectionCommands[]=
    {
      HorizontalRotateCommand,
      VerticalRotateCommand
    },
    RotateCommands[]=
    {
      RotateColorCommand,
      RotateDirectionCommand,
      RotateCropCommand,
      RotateSharpenCommand,
      RotateHelpCommand,
      RotateDismissCommand
    };

  static unsigned int
    crop = False,
    pen_id = 0,
    sharpen = True;

  char
    command[MaxTextExtent],
    text[MaxTextExtent];

  double
    normalized_degrees;

  Image
    *rotated_image;

  int
    id,
    x,
    y;

  register int
    i;

  unsigned int
    height,
    rotations,
    width;

  if (degrees == 0.0)
    {
      unsigned int
        distance;

      unsigned long
        state;

      XEvent
        event;

      XSegment
        rotate_info;

      /*
        Map Command widget.
      */
      windows->command.name="Rotate";
      windows->command.data=4;
      (void) XCommandWidget(display,windows,RotateMenu,(XEvent *) NULL);
      XMapRaised(display,windows->command.id);
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_update_widget,CurrentTime);
      /*
        Wait for first button press.
      */
      XSetFunction(display,windows->image.highlight_context,GXinvert);
      XQueryPosition(display,windows->image.id,&x,&y);
      rotate_info.x1=x;
      rotate_info.y1=y;
      rotate_info.x2=x;
      rotate_info.y2=y;
      state=DefaultState;
      do
      {
        XHighlightLine(display,windows->image.id,
          windows->image.highlight_context,&rotate_info);
        /*
          Wait for next event.
        */
        XScreenEvent(display,windows,&event);
        XHighlightLine(display,windows->image.id,
          windows->image.highlight_context,&rotate_info);
        if (event.xany.window == windows->command.id)
          {
            /*
              Select a command from the Command widget.
            */
            id=XCommandWidget(display,windows,RotateMenu,&event);
            if (id < 0)
              continue;
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            switch (RotateCommands[id])
            {
              case RotateColorCommand:
              {
                char
                  *ColorMenu[MaxNumberPens];

                int
                  pen_number;

                XColor
                  color;

                /*
                  Initialize menu selections.
                */
                for (i=0; i < (int) (MaxNumberPens-2); i++)
                  ColorMenu[i]=resource_info->pen_colors[i];
                ColorMenu[MaxNumberPens-2]="Browser...";
                ColorMenu[MaxNumberPens-1]=(char *) NULL;
                /*
                  Select a pen color from the pop-up menu.
                */
                pen_number=XMenuWidget(display,windows,RotateMenu[id],
                  (const char **) ColorMenu,command);
                if (pen_number < 0)
                  break;
                if (pen_number == (MaxNumberPens-2))
                  {
                    static char
                      color_name[MaxTextExtent] = "gray";

                    /*
                      Select a pen color from a dialog.
                    */
                    resource_info->pen_colors[pen_number]=color_name;
                    XColorBrowserWidget(display,windows,"Select",color_name);
                    if (*color_name == '\0')
                      break;
                  }
                /*
                  Set pen color.
                */
                (void) XParseColor(display,windows->map_info->colormap,
                  resource_info->pen_colors[pen_number],&color);
                XBestPixel(display,windows->map_info->colormap,(XColor *) NULL,
                  (unsigned int) MaxColors,&color);
                windows->pixel_info->pen_colors[pen_number]=color;
                pen_id=pen_number;
                break;
              }
              case RotateDirectionCommand:
              {
                static const char
                  *Directions[]=
                  {
                    "horizontal",
                    "vertical",
                    (char *) NULL,
                  };

                /*
                  Select a command from the pop-up menu.
                */
                id=XMenuWidget(display,windows,RotateMenu[id],
                  Directions,command);
                if (id >= 0)
                  direction=DirectionCommands[id];
                break;
              }
              case RotateCropCommand:
              {
                static const char
                  *Options[]=
                  {
                    "false",
                    "true",
                    (char *) NULL,
                  };

                /*
                  Select a command from the pop-up menu.
                */
                crop=XMenuWidget(display,windows,RotateMenu[id],
                  Options,command);
                break;
              }
              case RotateSharpenCommand:
              {
                static const char
                  *Options[]=
                  {
                    "false",
                    "true",
                    (char *) NULL,
                  };

                /*
                  Select a command from the pop-up menu.
                */
                sharpen=XMenuWidget(display,windows,RotateMenu[id],
                  Options,command);
                break;
              }
              case RotateHelpCommand:
              {
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Rotation",ImageRotateHelp);
                break;
              }
              case RotateDismissCommand:
              {
                /*
                  Prematurely exit.
                */
                state|=EscapeState;
                state|=ExitState;
                break;
              }
              default:
                break;
            }
            XSetFunction(display,windows->image.highlight_context,GXinvert);
            continue;
          }
        switch (event.type)
        {
          case ButtonPress:
          {
            if (event.xbutton.button != Button1)
              break;
            if (event.xbutton.window != windows->image.id)
              break;
            /*
              Exit loop.
            */
            XSetFunction(display,windows->image.highlight_context,GXcopy);
            rotate_info.x1=event.xbutton.x;
            rotate_info.y1=event.xbutton.y;
            state|=ExitState;
            break;
          }
          case ButtonRelease:
            break;
          case Expose:
            break;
          case KeyPress:
          {
            char
              command[MaxTextExtent];

            KeySym
              key_symbol;

            if (event.xkey.window != windows->image.id)
              break;
            /*
              Respond to a user key press.
            */
            (void) XLookupString((XKeyEvent *) &event.xkey,command,
              sizeof(command),&key_symbol,(XComposeStatus *) NULL);
            switch (key_symbol)
            {
              case XK_Escape:
              case XK_F20:
              {
                /*
                  Prematurely exit.
                */
                state|=EscapeState;
                state|=ExitState;
                break;
              }
              case XK_F1:
              case XK_Help:
              {
                XSetFunction(display,windows->image.highlight_context,GXcopy);
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Rotation",ImageRotateHelp);
                XSetFunction(display,windows->image.highlight_context,GXinvert);
                break;
              }
              default:
              {
                XBell(display,0);
                break;
              }
            }
            break;
          }
          case MotionNotify:
          {
            rotate_info.x1=event.xmotion.x;
            rotate_info.y1=event.xmotion.y;
          }
        }
        rotate_info.x2=rotate_info.x1;
        rotate_info.y2=rotate_info.y1;
        if (direction == HorizontalRotateCommand)
          rotate_info.x2+=32;
        else
          rotate_info.y2-=32;
      } while (!(state & ExitState));
      XSetFunction(display,windows->image.highlight_context,GXcopy);
      XWithdrawWindow(display,windows->info.id,windows->info.screen);
      if (state & EscapeState)
        return(True);
      /*
        Draw line as pointer moves until the mouse button is released.
      */
      distance=0;
      XSetFunction(display,windows->image.highlight_context,GXinvert);
      state=DefaultState;
      do
      {
        if (distance > 9)
          {
            /*
              Display info and draw rotation line.
            */
            if (!windows->info.mapped)
              XMapWindow(display,windows->info.id);
            FormatString(text," %.2f",
              direction == VerticalRotateCommand ? degrees-90.0 : degrees);
            XInfoWidget(display,windows,text);
            XHighlightLine(display,windows->image.id,
              windows->image.highlight_context,&rotate_info);
          }
        else
          if (windows->info.mapped)
            XWithdrawWindow(display,windows->info.id,windows->info.screen);
        /*
          Wait for next event.
        */
        XScreenEvent(display,windows,&event);
        if (distance > 9)
          XHighlightLine(display,windows->image.id,
            windows->image.highlight_context,&rotate_info);
        switch (event.type)
        {
          case ButtonPress:
            break;
          case ButtonRelease:
          {
            /*
              User has committed to rotation line.
            */
            rotate_info.x2=event.xbutton.x;
            rotate_info.y2=event.xbutton.y;
            state|=ExitState;
            break;
          }
          case Expose:
            break;
          case MotionNotify:
          {
            rotate_info.x2=event.xmotion.x;
            rotate_info.y2=event.xmotion.y;
          }
          default:
            break;
        }
        /*
          Check boundary conditions.
        */
        if (rotate_info.x2 < 0)
          rotate_info.x2=0;
        else
          if (rotate_info.x2 > (int) windows->image.width)
            rotate_info.x2=windows->image.width;
        if (rotate_info.y2 < 0)
          rotate_info.y2=0;
        else
          if (rotate_info.y2 > (int) windows->image.height)
            rotate_info.y2=windows->image.height;
        /*
          Compute rotation angle from the slope of the line.
        */
        degrees=0.0;
        distance=
          ((rotate_info.x2-rotate_info.x1+1)*(rotate_info.x2-rotate_info.x1+1))+
          ((rotate_info.y2-rotate_info.y1+1)*(rotate_info.y2-rotate_info.y1+1));
        if (distance > 9)
          degrees=RadiansToDegrees(-atan2((double) (rotate_info.y2-
            rotate_info.y1),(double) (rotate_info.x2-rotate_info.x1)));
      } while (!(state & ExitState));
      XSetFunction(display,windows->image.highlight_context,GXcopy);
      XWithdrawWindow(display,windows->info.id,windows->info.screen);
      if (distance <= 9)
        return(True);
    }
  if (direction == VerticalRotateCommand)
    degrees-=90.0;
  if (degrees == 0.0)
    return(True);
  /*
    Rotate image.
  */
  normalized_degrees=degrees;
  while (normalized_degrees < -45.0)
    normalized_degrees+=360.0;
  for (rotations=0; normalized_degrees > 45.0; rotations++)
    normalized_degrees-=90.0;
  if (normalized_degrees != 0.0)
    (void) XMagickCommand(display,resource_info,windows,ApplyCommand,image);
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  (*image)->border_color.red=
    XDownScale(windows->pixel_info->pen_colors[pen_id].red);
  (*image)->border_color.green=
    XDownScale(windows->pixel_info->pen_colors[pen_id].green);
  (*image)->border_color.blue=
    XDownScale(windows->pixel_info->pen_colors[pen_id].blue);
  (*image)->border_color.index=0;
  rotated_image=RotateImage(*image,degrees,crop,sharpen);
  XSetCursorState(display,windows,False);
  if (rotated_image == (Image *) NULL)
    return(False);
  DestroyImage(*image);
  *image=rotated_image;
  if (windows->image.crop_geometry != (char *) NULL)
    {
      /*
        Rotate crop geometry.
      */
      width=(*image)->columns;
      height=(*image)->rows;
      (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
      switch (rotations % 4)
      {
        default:
        case 0:
          break;
        case 1:
        {
          /*
            Rotate 90 degrees.
          */
          FormatString(windows->image.crop_geometry,"%ux%u%+d%+d",
            height,width,(int) (*image)->columns-(int) height-y,x);
          break;
        }
        case 2:
        {
          /*
            Rotate 180 degrees.
          */
          FormatString(windows->image.crop_geometry,"%ux%u%+d%+d",
            width,height,(int) width-x,(int) height-y);
          break;
        }
        case 3:
        {
          /*
            Rotate 270 degrees.
          */
          FormatString(windows->image.crop_geometry,"%ux%u%+d%+d",
            height,width,y,(int) (*image)->rows-(int) width-x);
          break;
        }
      }
    }
  if (windows->image.orphan)
    return(True);
  if (normalized_degrees != 0.0)
    {
      /*
        Update image colormap.
      */
      windows->image.window_changes.width=(*image)->columns;
      windows->image.window_changes.height=(*image)->rows;
      if (windows->image.crop_geometry != (char *) NULL)
        {
          /*
            Obtain dimensions of image from crop geometry.
          */
          (void) XParseGeometry(windows->image.crop_geometry,&x,&y,
            &width,&height);
          windows->image.window_changes.width=width;
          windows->image.window_changes.height=height;
        }
      XConfigureImageColormap(display,resource_info,windows,*image);
    }
  else
    if (((rotations % 4) == 1) || ((rotations % 4) == 3))
      {
        windows->image.window_changes.width=windows->image.ximage->height;
        windows->image.window_changes.height=windows->image.ximage->width;
      }
  /*
    Update image configuration.
  */
  (void) XConfigureImage(display,resource_info,windows,*image);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X S a v e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XSaveImage saves an image to a file.
%
%  The format of the XSaveImage method is:
%
%    status=XSaveImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XSaveImage return True if the image is
%      written.  False is returned is there is a memory shortage or if the
%      image fails to write.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%
*/
static unsigned int XSaveImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image)
{
  char
    filename[MaxTextExtent],
    geometry[MaxTextExtent];

  Image
    *save_image;

  ImageInfo
    *image_info;

  int
    status;

  /*
    Request file name from user.
  */
  if (resource_info->write_filename != (char *) NULL)
    (void) strcpy(filename,resource_info->write_filename);
  else
    {
      char
        working_directory[MaxTextExtent];

      register char
        *p;

      p=image->filename+Extent(image->filename)-1;
      while ((p > image->filename) && !IsBasenameSeparator(*(p-1)))
        p--;
      (void) strcpy(filename,p);
      (void) strcpy(working_directory,image->filename);
      working_directory[p-image->filename]='\0';
      if (p != image->filename)
        (void) chdir(working_directory);
    }
  XFileBrowserWidget(display,windows,"Save",filename);
  if (*filename == '\0')
    return(True);
  if (IsAccessible(filename))
    {
      /*
        File exists-- seek user's permission before overwriting.
      */
      status=XConfirmWidget(display,windows,"Overwrite",filename);
      if (status <= 0)
        return(True);
    }
  image_info=CloneImageInfo(resource_info->image_info);
  (void) strcpy(image_info->filename,filename);
  SetImageInfo(image_info,False);
  if ((Latin1Compare(image_info->magick,"JPEG") == 0) ||
      (Latin1Compare(image_info->magick,"JPG") == 0))
    {
      char
        quality[MaxTextExtent];

      /*
        Request JPEG quality from user.
      */
      FormatString(quality,"%u",image_info->quality);
      status=XDialogWidget(display,windows,"Save","Enter JPEG quality:",
        quality);
      if (*quality == '\0')
        return(True);
      image_info->quality=atoi(quality);
      image_info->interlace=status ? NoInterlace : PlaneInterlace;
    }
  if ((Latin1Compare(image_info->magick,"EPS") == 0) ||
      (Latin1Compare(image_info->magick,"PDF") == 0) ||
      (Latin1Compare(image_info->magick,"PS") == 0) ||
      (Latin1Compare(image_info->magick,"PS2") == 0))
    {
      char
        geometry[MaxTextExtent];

      /*
        Request page geometry from user.
      */
      FormatString(geometry,PSPageGeometry);
      if (Latin1Compare(image_info->magick,"PDF") == 0)
        FormatString(geometry,PSPageGeometry);
      if (image_info->page != (char *) NULL)
        (void) strcpy(geometry,image_info->page);
      XListBrowserWidget(display,windows,&windows->widget,PageSizes,"Select",
        "Select page geometry:",geometry);
      if (*geometry != '\0')
        image_info->page=PostscriptGeometry(geometry);
    }
  /*
    Apply image transforms.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  image->orphan=True;
  save_image=CloneImage(image,image->columns,image->rows,True);
  image->orphan=False;
  if (save_image == (Image *) NULL)
    return(False);
  FormatString(geometry,"%dx%d!",windows->image.ximage->width,
    windows->image.ximage->height);
  TransformImage(&save_image,windows->image.crop_geometry,geometry);
  if (resource_info->quantize_info->number_colors != 0)
    {
      /*
        Reduce the number of colors in the image.
      */
      if ((save_image->class == DirectClass) ||
          (save_image->colors > resource_info->quantize_info->number_colors) ||
          (resource_info->quantize_info->colorspace == GRAYColorspace))
        (void) QuantizeImage(resource_info->quantize_info,save_image);
      SyncImage(save_image);
    }
  /*
    Write image.
  */
  (void) strcpy(save_image->filename,filename);
  status=WriteImage(image_info,save_image);
  if (status == True)
    image->tainted=False;
  DestroyImage(save_image);
  DestroyImageInfo(image_info);
  XSetCursorState(display,windows,False);
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X S c r e e n E v e n t                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XScreenEvent handles global events associated with the Pan and
%  Magnify windows.
%
%  The format of the XScreenEvent function is:
%
%      XScreenEvent(display,windows,event)
%
%  A description of each parameter follows:
%
%    o display: Specifies a pointer to the Display structure;  returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o event: Specifies a pointer to a X11 XEvent structure.
%
%
*/

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

static int XPredicate(Display *display,XEvent *event,char *data)
{
  register XWindows
    *windows;

  windows=(XWindows *) data;
  if ((event->type == ClientMessage) &&
      (event->xclient.window == windows->image.id))
    return(False);
  return(True);
}

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

static void XScreenEvent(Display *display,XWindows *windows,XEvent *event)
{
  MonitorHandler
    handler;

  register int
    x,
    y;

  XIfEvent(display,event,XPredicate,(char *) windows);
  if (event->xany.window == windows->command.id)
    return;
  switch (event->type)
  {
    case ButtonPress:
    case ButtonRelease:
    {
      if ((event->xbutton.button == Button3) &&
          (event->xbutton.state & Mod1Mask))
        {
          /*
            Convert Alt-Button3 to Button2.
          */
          event->xbutton.button=Button2;
          event->xbutton.state&=(~Mod1Mask);
        }
      if (event->xbutton.window == windows->backdrop.id)
        {
          XSetInputFocus(display,event->xbutton.window,RevertToParent,
            event->xbutton.time);
          break;
        }
      if (event->xbutton.window == windows->pan.id)
        {
          XPanImage(display,windows,event);
          break;
        }
      if (event->xbutton.window == windows->image.id)
        if (event->xbutton.button == Button2)
          {
            /*
              Update magnified image.
            */
            x=event->xbutton.x;
            y=event->xbutton.y;
            if (x < 0)
              x=0;
            else
              if (x >= (int) windows->image.width)
                x=windows->image.width-1;
            windows->magnify.x=windows->image.x+x;
            if (y < 0)
              y=0;
            else
             if (y >= (int) windows->image.height)
               y=windows->image.height-1;
            windows->magnify.y=windows->image.y+y;
            if (!windows->magnify.mapped)
              XMapRaised(display,windows->magnify.id);
            handler=SetMonitorHandler((MonitorHandler) NULL);
            XMakeMagnifyImage(display,windows);
            (void) SetMonitorHandler(handler);
            if (event->type == ButtonRelease)
              XWithdrawWindow(display,windows->info.id,windows->info.screen);
            break;
          }
      break;
    }
    case ClientMessage:
    {
      /*
        If client window delete message, exit.
      */
      if (event->xclient.message_type != windows->wm_protocols)
        break;
      if (*event->xclient.data.l != (int) windows->wm_delete_window)
        break;
      if (event->xclient.window == windows->magnify.id)
        {
          XWithdrawWindow(display,windows->magnify.id,windows->magnify.screen);
          break;
        }
      break;
    }
    case ConfigureNotify:
    {
      if (event->xconfigure.window == windows->magnify.id)
        {
          unsigned int
            magnify;

          /*
            Magnify window has a new configuration.
          */
          windows->magnify.width=event->xconfigure.width;
          windows->magnify.height=event->xconfigure.height;
          if (!windows->magnify.mapped)
            break;
          magnify=1;
          while ((int) magnify <= event->xconfigure.width)
            magnify<<=1;
          while ((int) magnify <= event->xconfigure.height)
            magnify<<=1;
          magnify>>=1;
          if (((int) magnify != event->xconfigure.width) ||
              ((int) magnify != event->xconfigure.height))
            {
              XWindowChanges
                window_changes;

              window_changes.width=magnify;
              window_changes.height=magnify;
              XReconfigureWMWindow(display,windows->magnify.id,
                windows->magnify.screen,CWWidth | CWHeight,&window_changes);
              break;
            }
          XMakeMagnifyImage(display,windows);
          break;
        }
      break;
    }
    case Expose:
    {
      if (event->xexpose.window == windows->image.id)
        {
          XRefreshWindow(display,&windows->image,event);
          break;
        }
      if (event->xexpose.window == windows->pan.id)
        if (event->xexpose.count == 0)
          {
            XDrawPanRectangle(display,windows);
            break;
          }
      if (event->xexpose.window == windows->magnify.id)
        if (event->xexpose.count == 0)
          {
            XMakeMagnifyImage(display,windows);
            break;
          }
      break;
    }
    case KeyPress:
    {
      char
        command[MaxTextExtent];

      KeySym
        key_symbol;

      if (event->xkey.window != windows->magnify.id)
        break;
      /*
        Respond to a user key press.
      */
      (void) XLookupString((XKeyEvent *) &event->xkey,command,sizeof(command),
        &key_symbol,(XComposeStatus *) NULL);
      XMagnifyWindowCommand(display,windows,event->xkey.state,key_symbol);
      break;
    }
    case MapNotify:
    {
      if (event->xmap.window == windows->magnify.id)
        {
          windows->magnify.mapped=True;
          XWithdrawWindow(display,windows->info.id,windows->info.screen);
          break;
        }
      if (event->xmap.window == windows->info.id)
        {
          windows->info.mapped=True;
          break;
        }
      break;
    }
    case MotionNotify:
    {
      while (XCheckMaskEvent(display,ButtonMotionMask,event));
      if (event->xmotion.window == windows->image.id)
        if (windows->magnify.mapped)
          {
            /*
              Update magnified image.
            */
            x=event->xmotion.x;
            y=event->xmotion.y;
            if (x < 0)
              x=0;
            else
              if (x >= (int) windows->image.width)
                x=windows->image.width-1;
            windows->magnify.x=windows->image.x+x;
            if (y < 0)
              y=0;
            else
             if (y >= (int) windows->image.height)
               y=windows->image.height-1;
            windows->magnify.y=windows->image.y+y;
            XMakeMagnifyImage(display,windows);
          }
      break;
    }
    case UnmapNotify:
    {
      if (event->xunmap.window == windows->magnify.id)
        {
          windows->magnify.mapped=False;
          break;
        }
      if (event->xunmap.window == windows->info.id)
        {
          windows->info.mapped=False;
          break;
        }
      break;
    }
    default:
      break;
  }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X S e t C r o p G e o m e t r y                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XSetCropGeometry accepts a cropping geometry relative to the
%  Image window and translates it to a cropping geometry relative to the
%  image.
%
%  The format of the XSetCropGeometry method is:
%
%    XSetCropGeometry(display,windows,crop_info,image)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o crop_info:  A pointer to a RectangleInfo that defines a region of the
%      Image window to crop.
%
%    o image: Specifies a pointer to a Image structure.
%
%
*/
static void XSetCropGeometry(Display *display,XWindows *windows,
  RectangleInfo *crop_info,Image *image)
{
  char
    text[MaxTextExtent];

  int
    x,
    y;

  unsigned int
    height,
    width;

  unsigned long
    scale_factor;

  if (windows->info.mapped)
    {
      /*
        Display info on cropping rectangle.
      */
      FormatString(text," %ux%u%+d%+d",crop_info->width,crop_info->height,
        crop_info->x,crop_info->y);
      XInfoWidget(display,windows,text);
    }
  /*
    Cropping geometry is relative to any previous crop geometry.
  */
  x=0;
  y=0;
  width=image->columns;
  height=image->rows;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  else
    {
      /*
        Allocate crop geometry string.
      */
      windows->image.crop_geometry=(char *)
        AllocateMemory(MaxTextExtent*sizeof(char));
      if (windows->image.crop_geometry == (char *) NULL)
        MagickError(ResourceLimitError,"Unable to crop X image",
          windows->image.name);
    }
  /*
    Define the crop geometry string from the cropping rectangle.
  */
  scale_factor=UpShift(width)/windows->image.ximage->width;
  if (crop_info->x > 0)
    x+=DownShift(crop_info->x*scale_factor);
  width=DownShift(crop_info->width*scale_factor);
  if (width == 0)
    width=1;
  scale_factor=UpShift(height)/windows->image.ximage->height;
  if (crop_info->y > 0)
    y+=DownShift(crop_info->y*scale_factor);
  height=DownShift(crop_info->height*scale_factor);
  if (height == 0)
    height=1;
  FormatString(windows->image.crop_geometry,"%ux%u%+d%+d",width,height,x,y);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X T i l e I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XTileImage loads or deletes a selected tile from a visual
%  image directory.  The load or delete command is chosen from a menu.
%
%  The format of the XTileImage method is:
%
%    tiled_image=XTileImage(display,resource_info,windows,image,event)
%
%  A description of each parameter follows:
%
%    o tiled_image:  XTileImage reads or deletes the tiled image
%      and returns it.  A null image is returned if an error occurs.
%
%    o display: Specifies a connection to an X server;  returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure; returned from
%      ReadImage.
%
%    o event: Specifies a pointer to a XEvent structure.  If it is NULL,
%      the entire image is refreshed.
%
%
*/
static Image *XTileImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image,XEvent *event)
{
  static const char
    *VerbMenu[]=
    {
      "Load",
      "Next",
      "Former",
      "Delete",
      "Update",
      (char *) NULL,
    };

  static const ModeType
    TileCommands[]=
    {
      TileLoadCommand,
      TileNextCommand,
      TileFormerCommand,
      TileDeleteCommand,
      TileUpdateCommand
    };

  char
    command[MaxTextExtent],
    filename[MaxTextExtent];

  Image
    *tiled_image;

  int
    id,
    status,
    tile,
    x,
    y;

  register char
    *p,
    *q;

  register int
    i;

  unsigned int
    height,
    width;

  unsigned long
    scale_factor;

  /*
    Tile image is relative to montage image configuration.
  */
  x=0;
  y=0;
  width=image->columns;
  height=image->rows;
  if (windows->image.crop_geometry != (char *) NULL)
    (void) XParseGeometry(windows->image.crop_geometry,&x,&y,&width,&height);
  scale_factor=UpShift(width)/windows->image.ximage->width;
  event->xbutton.x+=windows->image.x;
  event->xbutton.x=DownShift(event->xbutton.x*scale_factor)+x;
  scale_factor=UpShift(height)/windows->image.ximage->height;
  event->xbutton.y+=windows->image.y;
  event->xbutton.y=DownShift(event->xbutton.y*scale_factor)+y;
  /*
    Determine size and location of each tile in the visual image directory.
  */
  width=image->columns;
  height=image->rows;
  x=0;
  y=0;
  (void) XParseGeometry(image->montage,&x,&y,&width,&height);
  tile=((event->xbutton.y-y)/height)*((image->columns-x)/width)+
    (event->xbutton.x-x)/width;
  if (tile < 0)
    {
      /*
        Button press is outside any tile.
      */
      XBell(display,0);
      return((Image *) NULL);
    }
  /*
    Determine file name from the tile directory.
  */
  p=image->directory;
  for (i=tile; (i != 0) && (*p != '\0'); )
  {
    if (*p == '\n')
      i--;
    p++;
  }
  if (*p == '\0')
    {
      /*
        Button press is outside any tile.
      */
      XBell(display,0);
      return((Image *) NULL);
    }
  /*
    Select a command from the pop-up menu.
  */
  id=XMenuWidget(display,windows,"Tile Verb",VerbMenu,command);
  if (id < 0)
    return((Image *) NULL);
  q=p;
  while ((*q != '\n') && (*q != '\0'))
    q++;
  (void) strncpy(filename,p,q-p);
  filename[q-p]='\0';
  /*
    Perform command for the selected tile.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  tiled_image=(Image *) NULL;
  switch (TileCommands[id])
  {
    case TileLoadCommand:
    {
      /*
        Load tile image.
      */
      XCheckRefreshWindows(display,windows);
      (void) strcpy(resource_info->image_info->magick,"MIFF");
      (void) strcpy(resource_info->image_info->filename,filename);
      tiled_image=ReadImage(resource_info->image_info);
      XWithdrawWindow(display,windows->info.id,windows->info.screen);
      break;
    }
    case TileNextCommand:
    {
      /*
        Display next image.
      */
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_next_image,CurrentTime);
      break;
    }
    case TileFormerCommand:
    {
      /*
        Display former image.
      */
      XClientMessage(display,windows->image.id,windows->im_protocols,
        windows->im_former_image,CurrentTime);
      break;
    }
    case TileDeleteCommand:
    {
      /*
        Delete tile image.
      */
      if (!IsAccessible(filename))
        {
          XNoticeWidget(display,windows,"Image file does not exist:",filename);
          break;
        }
      status=XConfirmWidget(display,windows,"Really delete tile",filename);
      if (status <= 0)
        break;
      status=remove(filename);
      if (status != False)
        {
          XNoticeWidget(display,windows,"Unable to delete image file:",
            filename);
          break;
        }
    }
    case TileUpdateCommand:
    {
      int
        x_offset,
        y_offset;

      register int
        j;

      register RunlengthPacket
        *r;

      /*
        Ensure all the images exist.
      */
      if (!UncondenseImage(image))
        return((Image *) NULL);
      tile=0;
      for (p=image->directory; *p != '\0'; p++)
      {
        q=p;
        while ((*q != '\n') && (*q != '\0'))
          q++;
        (void) strncpy(filename,p,q-p);
        filename[q-p]='\0';
        p=q;
        if (IsAccessible(filename))
          {
            tile++;
            continue;
          }
        /*
          Overwrite tile with background color.
        */
        x_offset=width*(tile % ((image->columns-x)/width))+x;
        y_offset=height*(tile/((image->columns-x)/width))+y;
        for (i=0; i < (int) height; i++)
        {
          r=image->pixels+((y_offset+i)*image->columns+x_offset);
          for (j=0; j < (int) width; j++)
            *r++=(*image->pixels);
        }
        tile++;
      }
      windows->image.window_changes.width=image->columns;
      windows->image.window_changes.height=image->rows;
      XConfigureImageColormap(display,resource_info,windows,image);
      (void) XConfigureImage(display,resource_info,windows,image);
      break;
    }
    default:
      break;
  }
  XSetCursorState(display,windows,False);
  return(tiled_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X T r a n s l a t e I m a g e                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XTranslateImage translates the image within an Image window
%  by one pixel as specified by the key symbol.  If the image has a `montage'
%  string the translation is respect to the width and height contained within
%  the string.
%
%  The format of the XTranslateImage method is:
%
%    XTranslateImage(display,windows,image,key_symbol)
%
%  A description of each parameter follows:
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure;  returned from
%      ReadImage.
%
%    o key_symbol: Specifies a KeySym which indicates which side of the image
%      to trim.
%
%
*/
static void XTranslateImage(Display *display,XWindows *windows,
  Image *image,const KeySym key_symbol)
{
  char
    text[MaxTextExtent];

  int
    x,
    y;

  unsigned int
    x_offset,
    y_offset;

  /*
    User specified a pan position offset.
  */
  x_offset=windows->image.width;
  y_offset=windows->image.height;
  if (image->montage != (char *) NULL)
    (void) XParseGeometry(image->montage,&x,&y,&x_offset,&y_offset);
  switch (key_symbol)
  {
    case XK_Home:
    case XK_KP_Home:
    {
      windows->image.x=windows->image.width >> 1;
      windows->image.y=windows->image.height >> 1;
      break;
    }
    case XK_Left:
    case XK_KP_Left:
    {
      windows->image.x-=x_offset;
      break;
    }
    case XK_Next:
    case XK_Up:
    case XK_KP_Up:
    {
      windows->image.y-=y_offset;
      break;
    }
    case XK_Right:
    case XK_KP_Right:
    {
      windows->image.x+=x_offset;
      break;
    }
    case XK_Prior:
    case XK_Down:
    case XK_KP_Down:
    {
      windows->image.y+=y_offset;
      break;
    }
    default:
      return;
  }
  /*
    Check boundary conditions.
  */
  if (windows->image.x < 0)
    windows->image.x=0;
  else
    if ((int) (windows->image.x+windows->image.width) >
        windows->image.ximage->width)
      windows->image.x=windows->image.ximage->width-windows->image.width;
  if (windows->image.y < 0)
    windows->image.y=0;
  else
    if ((int) (windows->image.y+windows->image.height) >
        windows->image.ximage->height)
      windows->image.y=windows->image.ximage->height-windows->image.height;
  /*
    Refresh Image window.
  */
  FormatString(text," %ux%u%+d%+d ",windows->image.width,
    windows->image.height,windows->image.x,windows->image.y);
  XInfoWidget(display,windows,text);
  XCheckRefreshWindows(display,windows);
  XDrawPanRectangle(display,windows);
  XRefreshWindow(display,&windows->image,(XEvent *) NULL);
  XWithdrawWindow(display,windows->info.id,windows->info.screen);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X T r i m I m a g e                                                       %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XTrimImage trims the edges from the Image window.
%
%  The format of the XTrimImage method is:
%
%    status=XTrimImage(display,resource_info,windows,image)
%
%  A description of each parameter follows:
%
%    o status: Method XTrimImage returns True if the image is
%      cropped.  False is returned is there is a memory shortage or if the
%      image fails to be cropped.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%    o windows: Specifies a pointer to a XWindows structure.
%
%    o image: Specifies a pointer to a Image structure.
%
%
*/
static unsigned int XTrimImage(Display *display,XResourceInfo *resource_info,
  XWindows *windows,Image *image)
{
  RectangleInfo
    trim_info;

  register int
    x,
    y;

  unsigned long
    background,
    pixel;

  /*
    Trim edges from image.
  */
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  /*
    Crop the left edge.
  */
  background=XGetPixel(windows->image.ximage,0,0);
  trim_info.width=windows->image.ximage->width;
  for (x=0; x < windows->image.ximage->width; x++)
  {
    for (y=0; y < windows->image.ximage->height; y++)
    {
      pixel=XGetPixel(windows->image.ximage,x,y);
      if (pixel != background)
        break;
    }
    if (y < windows->image.ximage->height)
      break;
  }
  trim_info.x=x;
  if (trim_info.x == windows->image.ximage->width)
    {
      XSetCursorState(display,windows,False);
      return(False);
    }
  /*
    Crop the right edge.
  */
  background=XGetPixel(windows->image.ximage,windows->image.ximage->width-1,0);
  for (x=windows->image.ximage->width-1; x > 0; x--)
  {
    for (y=0; y < windows->image.ximage->height; y++)
    {
      pixel=XGetPixel(windows->image.ximage,x,y);
      if (pixel != background)
        break;
    }
    if (y < windows->image.ximage->height)
      break;
  }
  trim_info.width=x-trim_info.x+1;
  /*
    Crop the top edge.
  */
  background=XGetPixel(windows->image.ximage,0,0);
  trim_info.height=windows->image.ximage->height;
  for (y=0; y < windows->image.ximage->height; y++)
  {
    for (x=0; x < windows->image.ximage->width; x++)
    {
      pixel=XGetPixel(windows->image.ximage,x,y);
      if (pixel != background)
        break;
    }
    if (x < windows->image.ximage->width)
      break;
  }
  trim_info.y=y;
  /*
    Crop the bottom edge.
  */
  background=XGetPixel(windows->image.ximage,0,windows->image.ximage->height-1);
  for (y=windows->image.ximage->height-1; y > 0; y--)
  {
    for (x=0; x < windows->image.ximage->width; x++)
    {
      pixel=XGetPixel(windows->image.ximage,x,y);
      if (pixel != background)
        break;
    }
    if (x < windows->image.ximage->width)
      break;
  }
  trim_info.height=y-trim_info.y+1;
  if ((trim_info.width != windows->image.width) ||
      (trim_info.height != windows->image.height))
    {
      /*
        Reconfigure Image window as defined by the trimming rectangle.
      */
      XSetCropGeometry(display,windows,&trim_info,image);
      windows->image.window_changes.width=trim_info.width;
      windows->image.window_changes.height=trim_info.height;
      (void) XConfigureImage(display,resource_info,windows,image);
    }
  XSetCursorState(display,windows,False);
  return(True);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
+   X V i s u a l D i r e c t o r y I m a g e                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XVisualDirectoryImage creates a Visual Image Directory.
%
%  The format of the XVisualDirectoryImage method is:
%
%    loaded_image=XVisualDirectoryImage(display,resource_info,windows)
%
%  A description of each parameter follows:
%
%    o loaded_image: Method XVisualDirectoryImage returns a visual image
%      directory if it can be created successfully.  Otherwise a null image
%      is returned.
%
%    o display: Specifies a connection to an X server; returned from
%      XOpenDisplay.
%
%    o resource_info: Specifies a pointer to a X11 XResourceInfo structure.
%
%    o windows: Specifies a pointer to a XWindows structure.
%
%
*/
static Image *XVisualDirectoryImage(Display *display,
  XResourceInfo *resource_info,XWindows *windows)
{
#define LoadImageText  "  Loading images...  "
#define TileImageText  "  Scaling image tiles...  "
#define XClientName  "montage"

  char
    *commands[10],
    **filelist,
    window_id[MaxTextExtent];

  Image
    *image,
    *montage_image,
    *next_image;

  ImageInfo
    *local_info;

  int
    number_files;

  MonitorHandler
    handler;

  MontageInfo
    montage_info;

  register int
    i;

  static char
    filename[MaxTextExtent] = "\0",
    filenames[MaxTextExtent] = "*";

  unsigned int
    backdrop,
    status;

  XResourceInfo
    background_resources;

  /*
    Request file name from user.
  */
  XFileBrowserWidget(display,windows,"Directory",filenames);
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
  /*
    Set image background resources.
  */
  background_resources=(*resource_info);
  background_resources.window_id=window_id;
  FormatString(background_resources.window_id,"0x%lx",windows->image.id);
  background_resources.backdrop=True;
  /*
    Read each image and convert them to a tile.
  */
  backdrop=(windows->visual_info->class == TrueColor) ||
   (windows->visual_info->class == DirectColor);
  local_info=CloneImageInfo(resource_info->image_info);
  if (local_info == (ImageInfo *) NULL)
    return((Image *) NULL);
  image=(Image *) NULL;
  commands[0]=resource_info->client_name;
  commands[1]="-label";
  commands[2]=(char *) DefaultTileLabel;
  commands[3]="-geometry";
  commands[4]=(char *) DefaultTileGeometry;
  XSetCursorState(display,windows,True);
  XCheckRefreshWindows(display,windows);
  for (i=0; i < number_files; i++)
  {
    handler=SetMonitorHandler((MonitorHandler) NULL);
    (void) strcpy(local_info->filename,filelist[i]);
    *local_info->magick='\0';
    (void) CloneString(&local_info->size,DefaultTileGeometry);
    next_image=ReadImage(local_info);
    if (filelist[i] != filenames)
      FreeMemory((char *) filelist[i]);
    if (next_image != (Image *) NULL)
      {
        MogrifyImages(local_info,5,commands,&next_image);
        next_image->matte=False;
        if (backdrop)
          {
            (void) XDisplayBackgroundImage(display,&background_resources,
              next_image);
            XSetCursorState(display,windows,True);
          }
        if (image == (Image *) NULL)
          image=next_image;
        else
          {
            image->next=next_image;
            image->next->previous=image;
            image=image->next;
          }
      }
    (void) SetMonitorHandler(handler);
    ProgressMonitor(LoadImageText,i,number_files);
  }
  DestroyImageInfo(local_info);
  FreeMemory((char *) filelist);
  if (image == (Image *) NULL)
    {
      XSetCursorState(display,windows,False);
      MagickWarning(OptionWarning,"No images were loaded",filenames);
      return((Image *) NULL);
    }
  while (image->previous != (Image *) NULL)
    image=image->previous;
  /*
    Create the Visual Image Directory.
  */
  GetMontageInfo(&montage_info);
  (void) strcpy(montage_info.filename,filename);
  (void) CloneString(&montage_info.font,resource_info->image_info->font);
  montage_info.pointsize=resource_info->image_info->pointsize;
  montage_image=MontageImages(image,&montage_info);
  DestroyMontageInfo(&montage_info);
  DestroyImages(image);
  XSetCursorState(display,windows,False);
  if (montage_image == (Image *) NULL)
    return(montage_image);
  XClientMessage(display,windows->image.id,windows->im_protocols,
    windows->im_next_image,CurrentTime);
  return(montage_image);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X D i s p l a y B a c k g r o u n d I m a g e                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XDisplayBackgroundImage displays an image in the background of a
%  window.
%
%  The format of the XDisplayBackgroundImage method is:
%
%      status=XDisplayBackgroundImage(display,resource_info,image)
%
%  A description of each parameter follows:
%
%    o status: Method XDisplayBackgroundImage returns True if the
%      designated window is the root window.
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
Export unsigned int XDisplayBackgroundImage(Display *display,
  XResourceInfo *resource_info,Image *image)
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

  unsigned int
    height,
    status,
    width;

  Window
    root_window;

  XGCValues
    context_values;

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
      if (isdigit((int) *resources.window_id))
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
      return(False);
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
  /*
    Initialize Standard Colormap.
  */
  resources.colormap=SharedColormap;
  XMakeStandardColormap(display,visual_info,&resources,image,map_info,
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
  window_info.width=image->columns;
  window_info.height=image->rows;
  FormatString(geometry,"%ux%u+0+0>",window_attributes.width,
    window_attributes.height);
  (void) ParseImageGeometry(geometry,&window_info.x,&window_info.y,
    &window_info.width,&window_info.height);
  status=XMakeImage(display,&resources,&window_info,image,window_info.width,
    window_info.height);
  if (status == False)
    MagickError(XServerError,"Unable to create X image",(char *) NULL);
  window_info.x=0;
  window_info.y=0;
  if (resources.debug)
    {
      (void) fprintf(stderr,"Image: %.1024s[%u] %ux%u ",image->filename,
        image->scene,image->columns,image->rows);
      if (image->colors != 0)
        (void) fprintf(stderr,"%uc ",image->colors);
      (void) fprintf(stderr,"%.1024s\n",image->magick);
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
  if (resources.delay != 0)
    XDelay(display,10*resources.delay);
  XSync(display,False);
  return(window_info.id == root_window);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   X D i s p l a y I m a g e                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method XDisplayImage displays an image via X11.  A new image is created
%  and returned if the user interactively transforms the displayed image.
%
%  The format of the XDisplayImage method is:
%
%      Image *XDisplayImage(Display *display,XResourceInfo *resource_info,
%        char **argv,int argc,Image **image,unsigned long *state)
%
%  A description of each parameter follows:
%
%    o loaded_image:  Method XDisplayImage returns an image when the
%      user chooses 'Open Image' from the command menu or picks a tile
%      from the image directory.  Otherwise a null image is returned.
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
%    o image: Specifies an address to an address of an Image structure;
%      returned from ReadImage.
%
%
*/
Export Image *XDisplayImage(Display *display,XResourceInfo *resource_info,
  char **argv,int argc,Image **image,unsigned long *state)
{
#define MagnifySize  256  /* must be a power of 2 */
#define MagickMenus  10
#define MaxWindows  10
#define MagickTitle  "Commands"

  static const char
    *CommandMenu[]=
    {
      "File",
      "Edit",
      "View",
      "Transform",
      "Enhance",
      "Effects",
      "F/X",
      "Image Edit",
      "Miscellany",
      "Help",
      (char *) NULL
    },
    *FileMenu[]=
    {
      "Open...",
      "Next",
      "Former",
      "Select...",
      "Save...",
      "Print...",
      "Delete...",
      "New...",
      "Visual Directory...",
      "Quit",
      (char *) NULL
    },
    *EditMenu[]=
    {
      "Undo",
      "Redo",
      "Cut",
      "Copy",
      "Paste",
      (char *) NULL
    },
    *ViewMenu[]=
    {
      "Half Size",
      "Original Size",
      "Double Size",
      "Resize...",
      "Apply",
      "Refresh",
      "Restore",
      (char *) NULL
    },
    *TransformMenu[]=
    {
      "Crop",
      "Chop",
      "Flop",
      "Flip",
      "Rotate Right",
      "Rotate Left",
      "Rotate...",
      "Shear...",
      "Roll...",
      "Trim Edges",
      (char *) NULL
    },
    *EnhanceMenu[]=
    {
      "Hue...",
      "Saturation...",
      "Brightness...",
      "Gamma...",
      "Spiff",
      "Dull",
      "Equalize",
      "Normalize",
      "Negate",
      "Grayscale",
      "Map...",
      "Quantize...",
      (char *) NULL
    },
    *EffectsMenu[]=
    {
      "Despeckle",
      "Emboss",
      "Reduce Noise",
      "Add Noise...",
      "Sharpen...",
      "Blur...",
      "Threshold...",
      "Edge Detect...",
      "Spread...",
      "Shade...",
      "Raise...",
      "Segment...",
      (char *) NULL
    },
    *FXMenu[]=
    {
      "Solarize...",
      "Swirl...",
      "Implode...",
      "Wave...",
      "Oil Painting...",
      "Charcoal Drawing...",
      (char *) NULL
    },
    *ImageEditMenu[]=
    {
      "Annotate...",
      "Draw...",
      "Color...",
      "Matte...",
      "Composite...",
      "Add Border...",
      "Add Frame...",
      "Comment...",
      "Launch...",
      "Region of Interest...",
      (char *) NULL
    },
    *MiscellanyMenu[]=
    {
      "Image Info",
      "Zoom Image",
      "Show Preview...",
      "Show Histogram",
      "Show Matte",
      "Background...",
      "Slide Show...",
      "Preferences...",
      (char *) NULL
    },
    *HelpMenu[]=
    {
      "Overview",
      "Browse Documentation",
      "About Display",
      (char *) NULL
    },
    *ShortCutsMenu[]=
    {
      "Next",
      "Former",
      "Open...",
      "Save...",
      "Print...",
      "Undo",
      "Restore",
      "Image Info",
      "Quit",
      (char *) NULL
    },
    *ImmutableMenu[]=
    {
      "Image Info",
      "Next",
      "Quit",
      (char *) NULL
    };

  static const char
    **Menus[MagickMenus]=
    {
      FileMenu,
      EditMenu,
      ViewMenu,
      TransformMenu,
      EnhanceMenu,
      EffectsMenu,
      FXMenu,
      ImageEditMenu,
      MiscellanyMenu,
      HelpMenu
    };

  static CommandType
    CommandMenus[]=
    {
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
      NullCommand,
    },
    FileCommands[]=
    {
      OpenCommand,
      NextCommand,
      FormerCommand,
      SelectCommand,
      SaveCommand,
      PrintCommand,
      DeleteCommand,
      NewCommand,
      VisualDirectoryCommand,
      QuitCommand
    },
    EditCommands[]=
    {
      UndoCommand,
      RedoCommand,
      CutCommand,
      CopyCommand,
      PasteCommand
    },
    ViewCommands[]=
    {
      HalfSizeCommand,
      OriginalSizeCommand,
      DoubleSizeCommand,
      ResizeCommand,
      ApplyCommand,
      RefreshCommand,
      RestoreCommand
    },
    TransformCommands[]=
    {
      CropCommand,
      ChopCommand,
      FlopCommand,
      FlipCommand,
      RotateRightCommand,
      RotateLeftCommand,
      RotateCommand,
      ShearCommand,
      RollCommand,
      TrimCommand
    },
    EnhanceCommands[]=
    {
      HueCommand,
      SaturationCommand,
      BrightnessCommand,
      GammaCommand,
      SpiffCommand,
      DullCommand,
      EqualizeCommand,
      NormalizeCommand,
      NegateCommand,
      GrayscaleCommand,
      MapCommand,
      QuantizeCommand
    },
    EffectsCommands[]=
    {
      DespeckleCommand,
      EmbossCommand,
      ReduceNoiseCommand,
      AddNoiseCommand,
      SharpenCommand,
      BlurCommand,
      ThresholdCommand,
      EdgeDetectCommand,
      SpreadCommand,
      ShadeCommand,
      RaiseCommand,
      SegmentCommand
    },
    FXCommands[]=
    {
      SolarizeCommand,
      SwirlCommand,
      ImplodeCommand,
      WaveCommand,
      OilPaintCommand,
      CharcoalDrawingCommand
    },
    ImageEditCommands[]=
    {
      AnnotateCommand,
      DrawCommand,
      ColorCommand,
      MatteCommand,
      CompositeCommand,
      AddBorderCommand,
      AddFrameCommand,
      CommentCommand,
      LaunchCommand,
      RegionofInterestCommand
    },
    MiscellanyCommands[]=
    {
      InfoCommand,
      ZoomCommand,
      ShowPreviewCommand,
      ShowHistogramCommand,
      ShowMatteCommand,
      BackgroundCommand,
      SlideShowCommand,
      PreferencesCommand
    },
    HelpCommands[]=
    {
      HelpCommand,
      BrowseDocumentationCommand,
      VersionCommand
    },
    ShortCutsCommands[]=
    {
      NextCommand,
      FormerCommand,
      OpenCommand,
      SaveCommand,
      PrintCommand,
      UndoCommand,
      RestoreCommand,
      InfoCommand,
      QuitCommand
    },
    ImmutableCommands[]=
    {
      InfoCommand,
      NextCommand,
      QuitCommand
    };

  static CommandType
    *Commands[MagickMenus]=
    {
      FileCommands,
      EditCommands,
      ViewCommands,
      TransformCommands,
      EnhanceCommands,
      EffectsCommands,
      FXCommands,
      ImageEditCommands,
      MiscellanyCommands,
      HelpCommands
    };

  char
    command[MaxTextExtent],
    geometry[MaxTextExtent],
    resource_name[MaxTextExtent];

  CommandType
    command_type;

  ErrorHandler
    warning_handler;

  Image
    *displayed_image,
    *loaded_image;

  int
    entry,
    id,
    status;

  KeySym
    key_symbol;

  MonitorHandler
    handler,
    monitor_handler;

  register int
    i;

  static char
    working_directory[MaxTextExtent];

  static XPoint
    vid_info;

  static XWindowInfo
    *magick_windows[MaxWindows];

  static unsigned int
    number_windows;

  struct stat
    file_info;

  time_t
    timer,
    timestamp,
    update_time;

  unsigned int
    context_mask;

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
    *pixel_info;

  XResourceInfo
    *icon_resources;

  XStandardColormap
    *icon_map,
    *map_info;

  XVisualInfo
    *icon_visual,
    *visual_info;

  XWindowChanges
    window_changes;

  XWindows
    *windows;

  XWMHints
    *manager_hints;

  displayed_image=(*image);
  TransformRGBImage(displayed_image,RGBColorspace);
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
      /*
        Allocate windows structure.
      */
      resource_info->colors=displayed_image->colors;
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
      magick_windows[number_windows++]=(&windows->magnify);
      magick_windows[number_windows++]=(&windows->pan);
      for (i=0; i < (int) number_windows; i++)
        magick_windows[i]->id=(Window) NULL;
      vid_info.x=0;
      vid_info.y=0;
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
  loaded_image=(Image *) NULL;
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
  displayed_image->tainted=False;
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
  if ((resource_info->title != (char *) NULL) && !(*state & MontageImageState))
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
      register char
        *p;

      register Image
        *q;

      unsigned int
        count;

      /*
        Window name is the base of the filename.
      */
      p=displayed_image->filename+Extent(displayed_image->filename)-1;
      while ((p > displayed_image->filename) && !IsBasenameSeparator(*(p-1)))
        p--;
      FormatString(windows->image.name,"ImageMagick: %.1024s[%u]",p,
        displayed_image->scene);
      q=displayed_image;
      while (q->previous != (Image *) NULL)
        q=q->previous;
      for (count=1; q->next != (Image *) NULL; count++)
        q=q->next;
      FormatString(windows->image.name,"ImageMagick: %.1024s[%u of %u]",p,
        displayed_image->scene,count);
      if ((displayed_image->previous == (Image *) NULL) &&
          (displayed_image->next == (Image *) NULL) &&
          (displayed_image->scene == 0))
        FormatString(windows->image.name,"ImageMagick: %.1024s",p);
      (void) strcpy(windows->image.icon_name,p);
    }
  if (resource_info->immutable)
    windows->image.immutable=True;
  windows->image.use_pixmap=resource_info->use_pixmap;
  windows->image.geometry=resource_info->image_geometry;
  windows->image.width=displayed_image->columns;
  windows->image.height=displayed_image->rows;
  FormatString(geometry,"%ux%u+0+0>!",
    XDisplayWidth(display,visual_info->screen),
    XDisplayHeight(display,visual_info->screen));
  (void) ParseImageGeometry(geometry,&windows->image.x,&windows->image.y,
    &windows->image.width,&windows->image.height);
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
  if (windows->command.mapped)
    XMapRaised(display,windows->command.id);
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
  /*
    Initialize Magnify window and cursor.
  */
  if (windows->magnify.id != (Window) NULL)
    FreeMemory((char *) windows->magnify.name);
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->magnify);
  windows->magnify.shared_memory=resource_info->use_shared_memory;
  FormatString(resource_name,"%.1024s.magnify",resource_info->client_name);
  windows->magnify.geometry=XGetResourceClass(resource_info->resource_database,
    resource_name,"geometry",(char *) NULL);
  windows->magnify.name=(char *) AllocateMemory(MaxTextExtent*sizeof(char));
  if (windows->magnify.name == NULL)
    MagickError(ResourceLimitError,"Unable to create Magnify window",
      "Memory allocation failed");
  FormatString(windows->magnify.name,"Magnify %uX",resource_info->magnify);
  windows->magnify.cursor=XMakeCursor(display,windows->image.id,
    map_info->colormap,resource_info->background_color,
    resource_info->foreground_color);
  if (windows->magnify.cursor == (Cursor) NULL)
    MagickError(XServerError,"Unable to create cursor",(char *) NULL);
  windows->magnify.width=MagnifySize;
  windows->magnify.height=MagnifySize;
  windows->magnify.flags|=PPosition;
  windows->magnify.min_width=MagnifySize;
  windows->magnify.min_height=MagnifySize;
  windows->magnify.width_inc=MagnifySize;
  windows->magnify.height_inc=MagnifySize;
  windows->magnify.data=resource_info->magnify;
  windows->magnify.attributes.cursor=windows->magnify.cursor;
  windows->magnify.attributes.event_mask=ButtonPressMask | ButtonReleaseMask |
    ExposureMask | KeyPressMask | KeyReleaseMask | OwnerGrabButtonMask |
    StructureNotifyMask;
  class_hints->res_name="magnify";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=True;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->magnify);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (magnify)\n",windows->magnify.id);
  XSetTransientForHint(display,windows->magnify.id,windows->image.id);
  /*
    Initialize panning window.
  */
  XGetWindowInfo(display,visual_info,map_info,pixel_info,font_info,
    resource_info,&windows->pan);
  windows->pan.name="Pan Icon";
  windows->pan.width=windows->icon.width;
  windows->pan.height=windows->icon.height;
  FormatString(resource_name,"%.1024s.pan",resource_info->client_name);
  windows->pan.geometry=XGetResourceClass(resource_info->resource_database,
    resource_name,"geometry",(char *) NULL);
  (void) XParseGeometry(windows->pan.geometry,&windows->pan.x,&windows->pan.y,
    &windows->pan.width,&windows->pan.height);
  windows->pan.flags|=PPosition;
  windows->pan.immutable=True;
  windows->pan.attributes.event_mask=ButtonMotionMask | ButtonPressMask |
    ButtonReleaseMask | ExposureMask | KeyPressMask | KeyReleaseMask |
    StructureNotifyMask;
  class_hints->res_name="pan";
  manager_hints->flags=InputHint | StateHint | WindowGroupHint;
  manager_hints->input=True;
  manager_hints->initial_state=NormalState;
  manager_hints->window_group=windows->image.id;
  XMakeWindow(display,root_window,argv,argc,class_hints,manager_hints,
    &windows->pan);
  if (resource_info->debug)
    (void) fprintf(stderr,"Window id: 0x%lx (pan)\n",windows->pan.id);
  XSetTransientForHint(display,windows->pan.id,windows->image.id);
  if (windows->info.mapped)
    XWithdrawWindow(display,windows->info.id,windows->info.screen);
  if (!windows->image.mapped || (windows->backdrop.id != (Window) NULL))
    XMapWindow(display,windows->image.id);
  /*
    Set our progress monitor and warning handlers.
  */
  if (monitor_handler == (MonitorHandler) NULL)
    monitor_handler=SetMonitorHandler(XProgressMonitor);
  if (warning_handler == (ErrorHandler) NULL)
    warning_handler=resource_info->display_warnings ?
      SetWarningHandler(XWarning) : SetWarningHandler((ErrorHandler) NULL);
  (void) signal(SIGINT,XSignalHandler);
  (void) signal(SIGSEGV,XSignalHandler);
  /*
    Initialize Image and Magnify X images.
  */
  windows->image.x=0;
  windows->image.y=0;
  status=XMakeImage(display,resource_info,&windows->image,displayed_image,
    displayed_image->columns,displayed_image->rows);
  if (status == False)
    MagickError(XServerError,"Unable to create X image",(char *) NULL);
  if (windows->image.mapped)
    XRefreshWindow(display,&windows->image,(XEvent *) NULL);
  SignatureImage(displayed_image);
  handler=SetMonitorHandler((MonitorHandler) NULL);
  status=XMakeImage(display,resource_info,&windows->magnify,(Image *) NULL,
    windows->magnify.width,windows->magnify.height);
  (void) SetMonitorHandler(handler);
  if (status == False)
    MagickError(XServerError,"Unable to create X magnify image",(char *) NULL);
  if (windows->magnify.mapped)
    XMapRaised(display,windows->magnify.id);
  if (windows->image.mapped)
    if (((int) windows->image.width < windows->image.ximage->width) ||
        ((int) windows->image.height < windows->image.ximage->height))
      XMapRaised(display,windows->pan.id);
  XWithdrawWindow(display,windows->info.id,windows->info.screen);
  XSync(display,False);
  /*
    Respond to events.
  */
  timer=time((time_t *) NULL)+(resource_info->delay/100)+1;
  update_time=0;
  if (resource_info->update)
    {
      /*
        Determine when file data was last modified.
      */
      status=stat(displayed_image->filename,&file_info);
      if (status == 0)
        update_time=file_info.st_mtime;
    }
  *state&=(~FormerImageState);
  *state&=(~MontageImageState);
  *state&=(~NextImageState);
  do
  {
    /*
      Handle a window event.
    */
    if (windows->image.mapped && resource_info->delay)
      {
        if (timer < time((time_t *) NULL))
          {
            if (!resource_info->update)
              *state|=NextImageState | ExitState;
            else
              {
                /*
                  Determine if image file was modified.
                */
                status=stat(displayed_image->filename,&file_info);
                if (status == 0)
                  if (update_time != file_info.st_mtime)
                    {
                      /*
                        Redisplay image.
                      */
                      FormatString(resource_info->image_info->filename,
                        "%.1024s:%.1024s",displayed_image->magick,
                        displayed_image->filename);
                      loaded_image=ReadImage(resource_info->image_info);
                      if (loaded_image != (Image *) NULL)
                        *state|=NextImageState | ExitState;
                    }
                timer=time((time_t *) NULL)+(resource_info->delay/100)+1;
              }
          }
        if (XEventsQueued(display,QueuedAfterFlush) == 0)
          {
            /*
              Do not block if delay > 0.
            */
            XDelay(display,SuspendTime << 2);
            continue;
          }
      }
    timestamp=time((time_t *) NULL);
    XNextEvent(display,&event);
    if (!windows->image.stasis)
      windows->image.stasis=(time((time_t *) NULL)-timestamp) > 0;
    if (event.xany.window == windows->command.id)
      {
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
            command_type,&displayed_image);
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
            switch (event.xbutton.button)
            {
              case Button1:
              {
                if (resource_info->immutable)
                  {
                    /*
                      Select a command from the Immutable menu.
                    */
                    entry=XMenuWidget(display,windows,"Commands",ImmutableMenu,
                      command);
                    if (entry >= 0)
                      loaded_image=XMagickCommand(display,resource_info,
                        windows,ImmutableCommands[entry],&displayed_image);
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
                break;
              }
              case Button2:
              {
                /*
                  User pressed the image magnify button.
                */
                (void) XMagickCommand(display,resource_info,windows,ZoomCommand,
                  &displayed_image);
                XMagnifyImage(display,windows,&event);
                break;
              }
              case Button3:
              {
                if (resource_info->immutable)
                  {
                    /*
                      Select a command from the Immutable menu.
                    */
                    entry=XMenuWidget(display,windows,"Commands",ImmutableMenu,
                      command);
                    if (entry >= 0)
                      loaded_image=XMagickCommand(display,resource_info,
                        windows,ImmutableCommands[entry],&displayed_image);
                    break;
                  }
                if (displayed_image->montage != (char *) NULL)
                  {
                    /*
                      Open or delete a tile from a visual image directory.
                    */
                    loaded_image=XTileImage(display,resource_info,windows,
                      displayed_image,&event);
                    if (loaded_image != (Image *) NULL)
                      *state|=MontageImageState | NextImageState | ExitState;
                    vid_info.x=windows->image.x;
                    vid_info.y=windows->image.y;
                    break;
                  }
                /*
                  Select a command from the Short Cuts menu.
                */
                entry=XMenuWidget(display,windows,"Short Cuts",ShortCutsMenu,
                  command);
                if (entry >= 0)
                  loaded_image=XMagickCommand(display,resource_info,windows,
                    ShortCutsCommands[entry],&displayed_image);
                break;
              }
              default:
                break;
            }
            break;
          }
        if (event.xbutton.window == windows->magnify.id)
          {
            int
              factor;

            static const char
              *MagnifyMenu[]=
              {
                "2",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "3",
                (char *) NULL,
              };

            static KeySym
              MagnifyCommands[]=
              {
                XK_2,
                XK_4,
                XK_5,
                XK_6,
                XK_7,
                XK_8,
                XK_9,
                XK_3
              };

            /*
              Select a magnify factor from the pop-up menu.
            */
            factor=XMenuWidget(display,windows,"Magnify",MagnifyMenu,command);
            if (factor >= 0)
              XMagnifyWindowCommand(display,windows,0,MagnifyCommands[factor]);
            break;
          }
        if (event.xbutton.window == windows->pan.id)
          {
            XPanImage(display,windows,&event);
            break;
          }
        timer=time((time_t *) NULL)+(resource_info->delay/100)+1;
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
            if (*event.xclient.data.l == (int) windows->im_update_widget)
              {
                windows->command.name=MagickTitle;
                windows->command.data=MagickMenus;
                (void) XCommandWidget(display,windows,CommandMenu,
                  (XEvent *) NULL);
                break;
              }
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
                if (windows->pan.mapped)
                  {
                    XSetWindowBackgroundPixmap(display,windows->pan.id,
                      windows->pan.pixmap);
                    XClearWindow(display,windows->pan.id);
                    XDrawPanRectangle(display,windows);
                  }
                if (windows->backdrop.id != (Window) NULL)
                  XInstallColormap(display,map_info->colormap);
                break;
              }
            if (*event.xclient.data.l == (int) windows->im_former_image)
              {
                *state|=FormerImageState | ExitState;
                break;
              }
            if (*event.xclient.data.l == (int) windows->im_next_image)
              {
                *state|=NextImageState | ExitState;
                break;
              }
            if (*event.xclient.data.l == (int) windows->im_retain_colors)
              {
                *state|=RetainColorsState;
                break;
              }
            if (*event.xclient.data.l == (int) windows->im_exit)
              {
                *state|=ExitState;
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
            status=XGetWindowProperty(display,root_window,selection,0L,
              MaxTextExtent-1,False,(Atom) AnyPropertyType,&type,&format,
              &length,&after,&data);
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
              *state|=NextImageState | ExitState;
            XFree((void *) data);
            break;
          }
        /*
          If client window delete message, exit.
        */
        if ((int) event.xclient.message_type != (int) windows->wm_protocols)
          break;
        if (*event.xclient.data.l != (int) windows->wm_delete_window)
          break;
        XWithdrawWindow(display,event.xclient.window,visual_info->screen);
        if (event.xclient.window == windows->image.id)
          {
            *state|=ExitState;
            break;
          }
        if (event.xclient.window == windows->pan.id)
          {
            /*
              Restore original image size when pan window is deleted.
            */
            windows->image.window_changes.width=windows->image.ximage->width;
            windows->image.window_changes.height=windows->image.ximage->height;
            (void) XConfigureImage(display,resource_info,windows,
              displayed_image);
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
            /*
              Image window has a new configuration.
            */
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
                if (windows->magnify.geometry == (char *) NULL)
                  if (!windows->magnify.mapped)
                    {
                      windows->magnify.x=
                        event.xconfigure.x+event.xconfigure.width+25;
                      windows->magnify.y=event.xconfigure.y;
                      XConstrainWindowPosition(display,&windows->magnify);
                      window_changes.x=windows->magnify.x;
                      window_changes.y=windows->magnify.y;
                      XReconfigureWMWindow(display,windows->magnify.id,
                        windows->magnify.screen,CWX | CWY,&window_changes);
                    }
                if (windows->pan.geometry == (char *) NULL)
                  if (!windows->pan.mapped)
                    {
                      windows->pan.x=
                        event.xconfigure.x+event.xconfigure.width+25;
                      windows->pan.y=
                        event.xconfigure.y+windows->magnify.height+50;
                      XConstrainWindowPosition(display,&windows->pan);
                      window_changes.x=windows->pan.x;
                      window_changes.y=windows->pan.y;
                      XReconfigureWMWindow(display,windows->pan.id,
                        windows->pan.screen,CWX | CWY,&window_changes);
                    }
              }
            if ((event.xconfigure.width == (int) windows->image.width) &&
                (event.xconfigure.height == (int) windows->image.height))
              {
                if (windows->image.mapped)
                  XRefreshWindow(display,&windows->image,(XEvent *) NULL);
                break;
              }
            windows->image.width=event.xconfigure.width;
            windows->image.height=event.xconfigure.height;
            windows->image.x=0;
            windows->image.y=0;
            if (displayed_image->montage != (char *) NULL)
              {
                windows->image.x=vid_info.x;
                windows->image.y=vid_info.y;
              }
            if (windows->image.mapped && windows->image.stasis)
              {
                /*
                  Update Image window configuration.
                */
                windows->image.window_changes.width=event.xconfigure.width;
                windows->image.window_changes.height=event.xconfigure.height;
                (void) XConfigureImage(display,resource_info,windows,
                  displayed_image);
              }
            if ((event.xconfigure.width < windows->image.ximage->width) ||
                (event.xconfigure.height < windows->image.ximage->height))
              {
                XMapRaised(display,windows->pan.id);
                XDrawPanRectangle(display,windows);
              }
            else
              if (windows->pan.mapped)
                XWithdrawWindow(display,windows->pan.id,windows->pan.screen);
            break;
          }
        if (event.xconfigure.window == windows->magnify.id)
          {
            unsigned int
              magnify;

            /*
              Magnify window has a new configuration.
            */
            windows->magnify.width=event.xconfigure.width;
            windows->magnify.height=event.xconfigure.height;
            if (!windows->magnify.mapped)
              break;
            magnify=1;
            while ((int) magnify <= event.xconfigure.width)
              magnify<<=1;
            while ((int) magnify <= event.xconfigure.height)
              magnify<<=1;
            magnify>>=1;
            if (((int) magnify != event.xconfigure.width) ||
                ((int) magnify != event.xconfigure.height))
              {
                window_changes.width=magnify;
                window_changes.height=magnify;
                XReconfigureWMWindow(display,windows->magnify.id,
                  windows->magnify.screen,CWWidth | CWHeight,&window_changes);
                break;
              }
            handler=SetMonitorHandler((MonitorHandler) NULL);
            status=XMakeImage(display,resource_info,&windows->magnify,
              displayed_image,windows->magnify.width,windows->magnify.height);
            XMakeMagnifyImage(display,windows);
            (void) SetMonitorHandler(handler);
            break;
          }
        if (event.xconfigure.window == windows->pan.id)
          {
            /*
              Pan icon window has a new configuration.
            */
            if (event.xconfigure.send_event != 0)
              {
                windows->pan.x=event.xconfigure.x;
                windows->pan.y=event.xconfigure.y;
              }
            windows->pan.width=event.xconfigure.width;
            windows->pan.height=event.xconfigure.height;
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
            *state|=ExitState;
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
          Refresh windows that are now exposed.
        */
        if (event.xexpose.window == windows->image.id)
          if (windows->image.mapped)
            {
              XRefreshWindow(display,&windows->image,&event);
              timer=time((time_t *) NULL)+(resource_info->delay/100)+1;
              break;
            }
        if (event.xexpose.window == windows->magnify.id)
          if (event.xexpose.count == 0)
            if (windows->magnify.mapped)
              {
                XMakeMagnifyImage(display,windows);
                break;
              }
        if (event.xexpose.window == windows->pan.id)
          if (event.xexpose.count == 0)
            {
              XDrawPanRectangle(display,windows);
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
        int
          length;

        /*
          Respond to a user key press.
        */
        length=XLookupString((XKeyEvent *) &event.xkey,command,sizeof(command),
          &key_symbol,(XComposeStatus *) NULL);
        *(command+length)='\0';
        if (resource_info->debug)
          (void) fprintf(stderr,"Key press: %d 0x%lx (%.1024s)\n",
            event.xkey.state,key_symbol,command);
        if (event.xkey.window == windows->image.id)
          {
            command_type=XImageWindowCommand(display,resource_info,windows,
              event.xkey.state,key_symbol,&displayed_image);
            if (command_type != NullCommand)
              loaded_image=XMagickCommand(display,resource_info,windows,
                command_type,&displayed_image);
          }
        if (event.xkey.window == windows->magnify.id)
          XMagnifyWindowCommand(display,windows,event.xkey.state,key_symbol);
        if (event.xkey.window == windows->pan.id)
          {
            if (key_symbol == XK_q)
              XWithdrawWindow(display,windows->pan.id,windows->pan.screen);
            else
              if ((key_symbol == XK_F1) || (key_symbol == XK_Help))
                XTextViewWidget(display,resource_info,windows,False,
                  "Help Viewer - Image Panning",ImagePanHelp);
              else
                XTranslateImage(display,windows,*image,key_symbol);
          }
        timer=time((time_t *) NULL)+(resource_info->delay/100)+1;
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
            if (Latin1Compare(displayed_image->magick,"LOGO") == 0)
              {
                if (Latin1Compare(displayed_image->filename,"Untitled") == 0)
                  loaded_image=XOpenBlob(display,resource_info,windows,False);
                else
                  *state|=NextImageState | ExitState;
              }
            if (((int) windows->image.width < windows->image.ximage->width) ||
                ((int) windows->image.height < windows->image.ximage->height))
              XMapRaised(display,windows->pan.id);
            windows->image.mapped=True;
            break;
          }
        if (event.xmap.window == windows->magnify.id)
          {
            XMakeMagnifyImage(display,windows);
            windows->magnify.mapped=True;
            XWithdrawWindow(display,windows->info.id,windows->info.screen);
            break;
          }
        if (event.xmap.window == windows->pan.id)
          {
            XMakePanImage(display,resource_info,windows,displayed_image);
            windows->pan.mapped=True;
            break;
          }
        if (event.xmap.window == windows->info.id)
          {
            windows->info.mapped=True;
            break;
          }
        if (event.xmap.window == windows->icon.id)
          {
            unsigned int
              tainted;

            /*
              Create an icon image.
            */
            tainted=displayed_image->tainted;
            XMakeStandardColormap(display,icon_visual,icon_resources,
              displayed_image,icon_map,icon_pixel);
            (void) XMakeImage(display,icon_resources,&windows->icon,
              displayed_image,windows->icon.width,windows->icon.height);
            displayed_image->tainted=tainted;
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
          *state|=NextImageState | ExitState;
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
        if (event.xunmap.window == windows->magnify.id)
          {
            windows->magnify.mapped=False;
            break;
          }
        if (event.xunmap.window == windows->pan.id)
          {
            windows->pan.mapped=False;
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
  while (!(*state & ExitState));
  if (!(*state & ExitState))
    (void) XMagickCommand(display,resource_info,windows,FreeBuffersCommand,
      &displayed_image);
  else
    {
      /*
        Query user if image has changed.
      */
      SignatureImage(displayed_image);
      if (!resource_info->immutable && displayed_image->tainted)
        {
          status=XConfirmWidget(display,windows,"Your image changed.",
            "Do you want to save it");
          if (status == 0)
            *state&=(~ExitState);
          else
            if (status > 0)
              (void) XMagickCommand(display,resource_info,windows,SaveCommand,
                &displayed_image);
        }
    }
  if ((windows->visual_info->class == GrayScale) ||
      (windows->visual_info->class == PseudoColor) ||
      (windows->visual_info->class == DirectColor))
    {
      /*
        Withdraw pan and Magnify window.
      */
      if (windows->info.mapped)
        XWithdrawWindow(display,windows->info.id,windows->info.screen);
      if (windows->magnify.mapped)
        XWithdrawWindow(display,windows->magnify.id,windows->magnify.screen);
      if (windows->command.mapped)
        XWithdrawWindow(display,windows->command.id,windows->command.screen);
    }
  if (windows->pan.mapped)
    XWithdrawWindow(display,windows->pan.id,windows->pan.screen);
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
  if (!resource_info->immutable || (displayed_image->next != (Image *) NULL))
    if ((*state & FormerImageState) || (*state & NextImageState))
      *state&=(~ExitState);
  if (*state & ExitState)
    {
      /*
        Destroy X windows.
      */
      if (windows->image.mapped)
        XWithdrawWindow(display,windows->image.id,windows->image.screen);
      XDelay(display,SuspendTime);
      for (i=0; i < (int) number_windows; i++)
      {
        if (magick_windows[i]->ximage != (XImage *) NULL)
          {
            magick_windows[i]->shared_memory=False;
            (void) XMakeImage(display,resource_info,magick_windows[i],
              (Image *) NULL,1,1);
            XDestroyImage(magick_windows[i]->ximage);
          }
        if (magick_windows[i]->pixmap != (Pixmap) NULL)
          XFreePixmap(display,magick_windows[i]->pixmap);
        if (magick_windows[i]->id != (Window) NULL)
          XDestroyWindow(display,magick_windows[i]->id);
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
      FreeMemory((char *) windows->magnify.name);
      FreeMemory((char *) windows->image.icon_name);
      FreeMemory((char *) windows->image.name);
      if (resource_info->copy_image != (Image *) NULL)
        DestroyImage(resource_info->copy_image);
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
  *image=displayed_image;
  return(loaded_image);
}
#endif
