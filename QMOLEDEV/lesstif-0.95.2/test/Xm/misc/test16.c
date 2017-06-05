/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test16.c,v 1.23 2002/05/03 12:03:41 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>


#ifdef LesstifVersion
#       define DISPLAY
#       define DESKTOP
#       define PROTOCOL
#       define SCREEN
#       define SHELL
#       define WORLD
#elif XmVERSION > 1
#       define DISPLAY
#       define DESKTOP
#       define SCREEN
#else
#       define DISPLAY
#       define DESKTOP
#       define PROTOCOL
#       define SCREEN
#       define SHELL
#       define WORLD
#endif

#include <Xm/ArrowBP.h>
#include <Xm/ArrowBGP.h>
#include <Xm/BulletinBP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/CommandP.h>
#include <Xm/DesktopP.h>
#include <Xm/DialogSP.h>
#include <Xm/DialogSEP.h>
#ifdef DISPLAY
#include <Xm/DisplayP.h>
#endif
#ifdef SCREEN
#include <Xm/ScreenP.h>
#endif
#ifdef SHELL
#include <Xm/ShellEP.h>
#endif
#include <Xm/DragCP.h>
#include <Xm/DragIconP.h>
#include <Xm/DragOverSP.h>
#include <Xm/DrawingAP.h>
#include <Xm/DrawnBP.h>
#include <Xm/DropSMgrP.h>
#include <Xm/DropTransP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/FileSBP.h>
#include <Xm/FormP.h>
#include <Xm/FrameP.h>
#include <Xm/LabelP.h>
#include <Xm/LabelGP.h>
#include <Xm/ListP.h>
#include <Xm/MainWP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MessageBP.h>
#include <Xm/PanedWP.h>
#include <Xm/ProtocolsP.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/SashP.h>
#include <Xm/ScaleP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/SelectioBP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/SeparatorP.h>
#include <Xm/ShellEP.h>
#include <Xm/TearOffBP.h>
#include <Xm/TextP.h>
#include <Xm/TextFP.h>
#include <Xm/ToggleBP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>
#ifdef WORLD
#include <Xm/WorldP.h>
#endif
#include <Xm/XmP.h>

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test16.c,v 1.23 2002/05/03 12:03:41 amai Exp $";

typedef struct _SavedItem{
        String class;
        String name;
        XtPointer addr;
} SavedItem;

static SavedItem *SavedActions = NULL;
static int NumSavedActions = 0;

/* Motif does not seem to have this
extern WidgetClass xmTextInnerObjectClass;
*/

static char *ClassRec2String(WidgetClass class);

static void SaveItem(String class, String name, XtPointer addr)
{
    NumSavedActions++;
    SavedActions = (SavedItem *)XtRealloc((char *)SavedActions, NumSavedActions * sizeof(SavedItem));
    SavedActions[NumSavedActions - 1].class = XtNewString(class);
    SavedActions[NumSavedActions - 1].name  = XtNewString(name);
    SavedActions[NumSavedActions - 1].addr  = addr;
}

static char *
Resource2String(XtResource *resource)
{
static char buf[1000];

        if (strcmp("CallProc",resource->default_type) == 0)
        {
                sprintf(buf,"_Xm%sDefault", resource->resource_class);
                SaveItem(resource->default_type,
                        buf,
                        resource->default_addr);
                return(buf);
        }
        else if (strcmp("Pointer",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%s", resource->resource_name);
                SaveItem(resource->default_type,
                        buf,
                        resource->default_addr);
                return(buf);
            }
        }
        else if (strcmp("Callback",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%s", resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("MenuWidget",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sMenuWidget", resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("XmString",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sXmString", resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("XmStringTable",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sXmStringTable", resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("StringTable",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sXmStringTable", resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("String",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"\"%s\"",resource->default_addr);
                return(buf);
            }
        }
        else if (strcmp("Boolean",resource->default_type) == 0)
        {
                return(resource->default_addr ? "True" : "False");
        }
        else if (strcmp("FontList",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sFontList /* FIX ME */",resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("Pixmap",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sPixmap /* FIX ME */",resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("Function",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sFunction",resource->resource_class);
                SaveItem(resource->default_type,
                        buf,
                        resource->default_addr);
                return(buf);
            }
        }
        else if (strcmp("WidgetList",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sWidgetList /* FIX ME */",resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("WidgetClass",resource->default_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sWidgetClass /* FIX ME */",resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("Orientation",resource->default_type) == 0)
        {
            if (resource->default_addr == (XtPointer)XmHORIZONTAL)
            {
                    return("XmHORIZONTAL");
            }
            else if (resource->default_addr == (XtPointer)XmVERTICAL)
            {
                    return("XmVERTICAL");
            }
            else
            {
                sprintf(buf,"_Xm%sOrientation /* FIX ME */",resource->resource_name);
                return(buf);
            }
        }
        else if (strcmp("Int",&resource->default_type[strlen(resource->default_type) - strlen("Int")]) == 0)
        {
                sprintf(buf,"%i",resource->default_addr);
                return(buf);
        }
        else if (strcmp("Immediate",resource->default_type) == 0)
        {
                if (strcmp("Boolean",resource->resource_type) == 0)
                {
                        return(resource->default_addr ? "True" : "False");
                }
                else if (strcmp("WidgetClass",resource->resource_type) == 0)
                {
                    if (resource->default_addr == NULL)
                    {
                            return("NULL");
                    }
                    else
                    {
                        sprintf(buf,"&%s /* FIX ME */",ClassRec2String(resource->default_addr));
                        return(buf);
                    }
                }
                else if (strcmp("Proc",resource->resource_type) == 0)
                {
                        sprintf(buf,"_Xm%sProc",resource->resource_class);
                        SaveItem(resource->default_type,
                            buf,
                            resource->default_addr);
                        return(buf);
                }
                else if (strcmp("Position",&resource->resource_type[strlen(resource->resource_type) - strlen("Position")]) == 0)
                {
                        sprintf(buf,"%i",resource->default_addr);
                        return(buf);
                }
                else if (strcmp("Dimension",&resource->resource_type[strlen(resource->resource_type) - strlen("Dimension")]) == 0)
                {
                        sprintf(buf,"%i",resource->default_addr);
                        return(buf);
                }
                else if (strcmp("PreeditType",resource->resource_class) == 0 && strcmp("String",&resource->resource_type[strlen(resource->resource_type) - strlen("String")]) == 0)
                {
                        if (resource->default_addr == XmFONTLIST_DEFAULT_TAG)
                        {
                        return("XmFONTLIST_DEFAULT_TAG");
                        }
                        else
                        {
                        sprintf(buf,"\"%s\"",resource->default_addr);
                        return(buf);
                        }
                }
                else if (strcmp("Pixmap",&resource->resource_type[strlen(resource->resource_type) - strlen("Pixmap")]) == 0)
                {
                    if (resource->default_addr == NULL)
                    {
                            return("NULL");
                    }
                    else if (resource->default_addr == (XtPointer)XmUNSPECIFIED_PIXMAP)
                    {
                            return("XmUNSPECIFIED_PIXMAP");
                    }
                    else
                    {
                            sprintf(buf,"0x%08x /* FIX ME Immediate*/",resource->default_addr);
                            return(buf);
                    }
                }
                else if (strcmp("String",resource->resource_type) == 0)
                {
                    if (resource->default_addr == NULL)
                    {
                        return("NULL");
                    }
                    else
                    {
                        sprintf(buf,"\"%s\"",resource->default_addr);
                        return(buf);
                    }
                }
        else if (strcmp("Function",resource->resource_type) == 0)
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL");
            }
            else
            {
                sprintf(buf,"_Xm%sFunction",resource->resource_class);
                SaveItem(resource->default_type,
                        buf,
                        resource->default_addr);
                return(buf);
            }
        }
                else
                {
                    if (resource->default_addr == NULL)
                    {
                            return("NULL");
                    }
                    else if (resource->default_addr == (XtPointer)XmUNSPECIFIED_PIXMAP)
                    {
                            return("XmUNSPECIFIED_PIXMAP");
                    }
                    else
                    {
                        if (resource->default_addr == NULL)
                        {
                                return("NULL");
                        }
                        else
                        {
                            sprintf(buf,"0x%08x",resource->default_addr);
                        }
                    }
                }
                return(buf);
        }
        else
        {
            if (resource->default_addr == NULL)
            {
                    return("NULL /* FIX ME default type */");
            }
            else
            {
                sprintf(buf,"0x%08x /* FIX ME default type*/",resource->default_addr);
            }
            return(buf);
        }
}

static char *
Class2Part(WidgetClass class)
{
        if (class == (WidgetClass)&xmArrowButtonGadgetClassRec)
        {
                return("arrowbutton");
        }
        else if (class == &coreClassRec)
        {
                return("core");
        }
        else if (class == (WidgetClass)&xmPrimitiveClassRec)
        {
                return("primitive");
        }
        else if (class == (WidgetClass)&xmFormClassRec)
        {
                return("form");
        }
        else if (class == (WidgetClass)&xmArrowButtonClassRec)
        {
                return("arrowbutton");
        }
        else if (class == (WidgetClass)&xmListClassRec)
        {
                return("list");
        }
        else if (class == (WidgetClass)&xmSashClassRec)
        {
                return("sash");
        }
        else if (class == (WidgetClass)&xmScrollBarClassRec)
        {
                return("scrollBar");
        }
        else if (class == (WidgetClass)&xmSeparatorClassRec)
        {
                return("separator");
        }
        else if (class == (WidgetClass)&xmTextFieldClassRec)
        {
                return("text");
        }
        else if (class == (WidgetClass)&xmTextClassRec)
        {
                return("text");
        }
        else if (class == (WidgetClass)&compositeClassRec)
        {
                return("composite");
        }
        else if (class == (WidgetClass)&constraintClassRec)
        {
                return("constraint");
        }
        else if (class == (WidgetClass)&xmBulletinBoardClassRec)
        {
                return("bulletin_board");
        }
        else if (class == (WidgetClass)&xmCascadeButtonGadgetClassRec)
        {
                return("cascade_button");
        }
        else if (class == (WidgetClass)&xmCascadeButtonClassRec)
        {
                return("cascade_button");
        }
        else if (class == (WidgetClass)&xmCommandClassRec)
        {
                return("command");
        }
        else if (class == (WidgetClass)&xmDesktopClassRec)
        {
                return("desktop");
        }
        else if (class == (WidgetClass)&xmDialogShellClassRec)
        {
                return("dialog_shell");
        }
        else if (class == (WidgetClass)&xmDrawingAreaClassRec)
        {
                return("drawing_area");
        }
        else if (class == (WidgetClass)&xmDrawnButtonClassRec)
        {
                return("drawnbutton");
        }
        else if (class == (WidgetClass)&xmScaleClassRec)
        {
                return("scale");
        }
        else if (class == (WidgetClass)&xmRowColumnClassRec)
        {
                return("row_column");
        }
        else if (class == (WidgetClass)&xmPushButtonClassRec)
        {
                return("pushbutton");
        }
        else if (class == (WidgetClass)&xmPanedWindowClassRec)
        {
                return("paned_window_box");
        }
        else if (class == (WidgetClass)&xmMessageBoxClassRec)
        {
                return("message_box");
        }
        else if (class == (WidgetClass)&xmFileSelectionBoxClassRec)
        {
                return("file_selection_box");
        }
        else if (class == (WidgetClass)&xmLabelClassRec)
        {
                return("label");
        }
        else if (class == (WidgetClass)&xmFrameClassRec)
        {
                return("frame");
        }
        else if (class == (WidgetClass)&xmToggleButtonClassRec)
        {
                return("togglebutton");
        }
        else if (class == (WidgetClass)&xmSelectionBoxClassRec)
        {
                return("selection_box");
        }
        else if (class == (WidgetClass)&xmScrolledWindowClassRec)
        {
                return("swindow");
        }
        else if (class == (WidgetClass)&xmManagerClassRec)
        {
                return("manager");
        }
        else if (class == (WidgetClass)&xmMainWindowClassRec)
        {
                return("mwindow");
        }
        else if (class == (WidgetClass)&vendorShellClassRec)
        {
                return("vendor_shell");
        }
        else if (class == (WidgetClass)&xmDragContextClassRec)
        {
                return("drag");
        }
        else if (class == (WidgetClass)&xmDragOverShellClassRec)
        {
                return("dragOver_shell");
        }
        else if (class == (WidgetClass)&xmMenuShellClassRec)
        {
                return("menu_shell");
        }
        else if (class == (WidgetClass)&xmTearOffButtonClassRec)
        {
                return("tearoffbutton");
        }
        else if (class == (WidgetClass)&xmVendorShellExtClassRec)
        {
                return("vendor");
        }
        else if (class == (WidgetClass)&xmDisplayClassRec)
        {
                return("display");
        }
        else if (class == (WidgetClass)&xmScreenClassRec)
        {
                return("screen");
        }
        else if (class == (WidgetClass)&xmExtClassRec)
        {
                return("ext");
        }
        else
        {
                return("unknown");
        }
}

static char *
ResourceName2Variable(char *name)
{
static char result[1000];
char *tmp;

        result[0] = '\0';
        tmp = name;
        while (tmp[0] != '\0')
        {
                if (isupper(tmp[0]))
                {
                        sprintf(&result[strlen(result)],"_%c",tolower(tmp[0]));
                }
                else
                {
                        sprintf(&result[strlen(result)],"%c",tmp[0]);
                }
                tmp = &tmp[1];
        }
        return(result);
}

static char *
CompressExpose2String(XtEnum expose)
{
        switch (expose)
        {
        case XtExposeNoCompress:
                return("XtExposeNoCompress");
        case XtExposeCompressSeries:
                return("XtExposeCompressSeries");
        case XtExposeCompressMultiple:
                return("XtExposeCompressMultiple");
        case XtExposeCompressMaximal:
                return("XtExposeCompressMaximal");
        default:
                return("UNKNOWN");
        }
}

static char *
ClassRec2String(WidgetClass class)
{
        if (class == (WidgetClass)&xmArrowButtonGadgetClassRec)
        {
                return("xmArrowButtonGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmArrowButtonClassRec)
        {
                return("xmArrowButtonClassRec");
        }
        else if (class == (WidgetClass)&xmBulletinBoardClassRec)
        {
                return("xmBulletinBoardClassRec");
        }
        else if (class == (WidgetClass)&xmCascadeButtonGCacheObjClassRec)
        {
                return("xmCascadeButtonGCacheObjClassRec");
        }
        else if (class == (WidgetClass)&xmCascadeButtonGadgetClassRec)
        {
                return("xmCascadeButtonGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmCascadeButtonClassRec)
        {
                return("xmCascadeButtonClassRec");
        }
        else if (class == (WidgetClass)&xmCommandClassRec)
        {
                return("xmCommandClassRec");
        }
        else if (class == (WidgetClass)&xmDesktopClassRec)
        {
                return("xmDesktopClassRec");
        }
        else if (class == (WidgetClass)&xmDialogShellExtClassRec)
        {
                return("xmDialogShellExtClassRec");
        }
#if 1
        else if (class == (WidgetClass)&xmDisplayClassRec)
        {
                return("xmDisplayClassRec");
        }
#endif
        else if (class == (WidgetClass)&xmDragContextClassRec)
        {
                return("xmDragContextClassRec");
        }
        else if (class == (WidgetClass)&xmDragIconClassRec)
        {
                return("xmDragIconClassRec");
        }
        else if (class == (WidgetClass)&xmDragOverShellClassRec)
        {
                return("xmDragOverShellClassRec");
        }
        else if (class == (WidgetClass)&xmDrawingAreaClassRec)
        {
                return("xmDrawingAreaClassRec");
        }
        else if (class == (WidgetClass)&xmDrawnButtonClassRec)
        {
                return("xmDrawnButtonClassRec");
        }
        else if (class == (WidgetClass)&xmDropSiteManagerClassRec)
        {
                return("xmDropSiteManagerClassRec");
        }
        else if (class == (WidgetClass)&xmDropTransferClassRec)
        {
                return("xmDropTransferClassRec");
        }
        else if (class == (WidgetClass)&xmExtClassRec)
        {
                return("xmExtClassRec");
        }
        else if (class == (WidgetClass)&xmFileSelectionBoxClassRec)
        {
                return("xmFileSelectionBoxClassRec");
        }
        else if (class == (WidgetClass)&xmFormClassRec)
        {
                return("xmFormClassRec");
        }
        else if (class == (WidgetClass)&xmFrameClassRec)
        {
                return("xmFrameClassRec");
        }
        else if (class == (WidgetClass)&xmGadgetClassRec)
        {
                return("xmGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmLabelGCacheObjClassRec)
        {
                return("xmLabelGCacheObjClassRec");
        }
        else if (class == (WidgetClass)&xmLabelGadgetClassRec)
        {
                return("xmLabelGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmLabelClassRec)
        {
                return("xmLabelClassRec");
        }
        else if (class == (WidgetClass)&xmListClassRec)
        {
                return("xmListClassRec");
        }
        else if (class == (WidgetClass)&xmMainWindowClassRec)
        {
                return("xmMainWindowClassRec");
        }
        else if (class == (WidgetClass)&xmManagerClassRec)
        {
                return("xmManagerClassRec");
        }
        else if (class == (WidgetClass)&xmMenuShellClassRec)
        {
                return("xmMenuShellClassRec");
        }
        else if (class == (WidgetClass)&xmMessageBoxClassRec)
        {
                return("xmMessageBoxClassRec");
        }
        else if (class == (WidgetClass)&xmPanedWindowClassRec)
        {
                return("xmPanedWindowClassRec");
        }
        else if (class == (WidgetClass)&xmPrimitiveClassRec)
        {
                return("xmPrimitiveClassRec");
        }
        else if (class == (WidgetClass)&xmProtocolClassRec)
        {
                return("xmProtocolClassRec");
        }
        else if (class == (WidgetClass)&xmPushButtonGCacheObjClassRec)
        {
                return("xmPushButtonGCaacheObjClassRec");
        }
        else if (class == (WidgetClass)&xmPushButtonGadgetClassRec)
        {
                return("xmPushButtonGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmPushButtonClassRec)
        {
                return("xmPushButtonClassRec");
        }
        else if (class == (WidgetClass)&xmRowColumnClassRec)
        {
                return("xmRowColumnClassRec");
        }
        else if (class == (WidgetClass)&xmSashClassRec)
        {
                return("xmSashClassRec");
        }
        else if (class == (WidgetClass)&xmScaleClassRec)
        {
                return("xmScaleClassRec");
        }
#if 0
        else if (class == (WidgetClass)&xmScreenClassRec)
        {
                return("xmScreenClassRec");
        }
#endif
        else if (class == (WidgetClass)&xmScrollBarClassRec)
        {
                return("xmScrollBarClassRec");
        }
        else if (class == (WidgetClass)&xmScrolledWindowClassRec)
        {
                return("xmScrolledWindowClassRec");
        }
        else if (class == (WidgetClass)&xmSelectionBoxClassRec)
        {
                return("xmSelectionBoxClassRec");
        }
        else if (class == (WidgetClass)&xmSeparatorGCacheObjClassRec)
        {
                return("xmSeparatorGCacheObjClassRec");
        }
        else if (class == (WidgetClass)&xmSeparatorGadgetClassRec)
        {
                return("xmSeparatorGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmSeparatorClassRec)
        {
                return("xmSeparatorClassRec");
        }
        else if (class == (WidgetClass)&xmShellExtClassRec)
        {
                return("xmShellExtClassRec");
        }
        else if (class == (WidgetClass)&xmTearOffButtonClassRec)
        {
                return("xmTearOffButtonClassRec");
        }
        else if (class == (WidgetClass)&xmTextFieldClassRec)
        {
                return("xmTextFieldClassRec");
        }
        else if (class == (WidgetClass)&xmTextClassRec)
        {
                return("xmTextClassRec");
        }
        else if (class == (WidgetClass)&xmToggleButtonGCacheObjClassRec)
        {
                return("xmToggleButtonGCacheObjClassRec");
        }
        else if (class == (WidgetClass)&xmToggleButtonGadgetClassRec)
        {
                return("xmToggleButtonGadgetClassRec");
        }
        else if (class == (WidgetClass)&xmToggleButtonClassRec)
        {
                return("xmToggleButtonClassRec");
        }
        else if (class == (WidgetClass)&xmVendorShellExtClassRec)
        {
                return("xmVendorShellExtClassRec");
        }
#ifdef WORLD
        else if (class == (WidgetClass)&xmWorldClassRec)
        {
                return("xmWorldClassRec");
        }
#endif
        else if (class == &coreClassRec)
        {
                return("coreClassRec");
        }
        else if (class == (WidgetClass)&compositeClassRec)
        {
                return("compositeClassRec");
        }
        else if (class == (WidgetClass)&constraintClassRec)
        {
                return("constraintClassRec");
        }
        else if (class == (WidgetClass)&widgetClassRec)
        {
                return("widgetClassRec");
        }
        else if (class == (WidgetClass)&objectClassRec)
        {
                return("objectClassRec");
        }
        else if (class == (WidgetClass)&rectObjClassRec)
        {
                return("rectObjClassRec");
        }
        else if (class == (WidgetClass)&shellClassRec)
        {
                return("shellClassRec");
        }
        else if (class == (WidgetClass)&overrideShellClassRec)
        {
                return("overrideShellClassRec");
        }
        else if (class == (WidgetClass)&wmShellClassRec)
        {
                return("wmShellClassRec");
        }
        else if (class == (WidgetClass)&transientShellClassRec)
        {
                return("transientShellClassRec");
        }
        else if (class == (WidgetClass)&topLevelShellClassRec)
        {
                return("topLevelShellClassRec");
        }
        else if (class == (WidgetClass)&applicationShellClassRec)
        {
                return("applicationShellClassRec");
        }
        else if (class == (WidgetClass)&vendorShellClassRec)
        {
                return("vendorShellClassRec");
        }
        else
        {
                return("unknownClassRec");
        }
}

static void
DoExtensions(WidgetClass base, WidgetClass class)
{
    if (class != (WidgetClass)&rectObjClassRec && class != &coreClassRec && class->core_class.superclass != NULL)
    {
        DoExtensions(base, class->core_class.superclass);
    }
    if (class == (WidgetClass)&coreClassRec ||
        class == (WidgetClass)&rectObjClassRec ||
        class == (WidgetClass)&objectClassRec)
    {
    XmBaseClassExtRec *ptr = (((CoreClassRec *)base)->core_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static XmBaseClassExtRec _%sCoreClassExtRec = {\n",base->core_class.class_name);
            printf("\t/* next_extension        */       %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Core Class Extension");
            printf("\t/* record_type           */       NULLQUARK,\n");
            printf("\t/* version               */       XmBaseClassExtVersion,\n");
            printf("\t/* size                  */       sizeof(XmBaseClassExtRec),\n");
            printf("\t/* initialize_prehook    */       %s,\n", ptr->initializePrehook == NULL ? "NULL" : ptr->initializePrehook == XmInheritInitializePrehook ? "XmInheritInitializePrehook" : "initialize_prehook");
            printf("\t/* set_values_prehook    */       %s,\n", ptr->setValuesPrehook == NULL ? "NULL" : ptr->setValuesPrehook == XmInheritSetValuesPrehook ? "XmInheritSetValuesPrehook" : "set_values_prehook");
            printf("\t/* initialize_posthook   */       %s,\n", ptr->initializePosthook == NULL ? "NULL" : ptr->initializePosthook == XmInheritInitializePosthook ? "XmInheritInitializePosthook" : "initialize_posthook");
            printf("\t/* set_values_posthook    */      %s,\n", ptr->setValuesPosthook == NULL ? "NULL" : ptr->setValuesPosthook == XmInheritSetValuesPosthook ? "XmInheritSetValuesPosthook" : "set_values_posthook");
            printf("\t/* secondary_object_class */      %s,\n", ptr->secondaryObjectClass == NULL ? "NULL" : ptr->secondaryObjectClass == XmInheritClass ? "XmInheritClass": ClassRec2String(ptr->secondaryObjectClass));
            printf("\t/* secondary_object_create*/      %s,\n", ptr->secondaryObjectCreate == NULL ? "NULL" : ptr->secondaryObjectCreate == XmInheritSecObjectCreate ? "XmInheritSecObjecCreate" : "secondaryObjectCreate");
            printf("\t/* get_secondary_resources*/      %s,\n", ptr->getSecResData == NULL ? "NULL" : ptr->getSecResData == XmInheritGetSecResData ? " XmInheritGetSecResData" : "getSecResData");
            printf("\t/* fast_subclass          */      { 0 },\n");
            printf("\t/* get_values_prehook     */      %s,\n", ptr->getValuesPrehook == NULL ? "NULL" : ptr->getValuesPrehook == XmInheritGetValuesPrehook ? "XmInheritGetValuesPrehook" : "get_values_prehook");
            printf("\t/* get_values_posthook    */      %s,\n", ptr->getValuesPosthook == NULL ? "NULL" : ptr->getValuesPosthook == XmInheritGetValuesPosthook ? "XmInheritGetValuesPosthook" : "get_values_posthook");
            printf("\t/* class_part_init_prehook*/      %s,\n", ptr->classPartInitPrehook == NULL ? "NULL" : ptr->classPartInitPrehook == XmInheritClassPartInitPrehook ? "XmInheritClassPartInitPrehook" : "class_part_init_prehook");
            printf("\t/* class_part_init_posthook*/     %s,\n", ptr->classPartInitPosthook == NULL ? "NULL" : ptr->classPartInitPosthook == XmInheritClassPartInitPosthook ? "XmInheritClassPartInitPosthook" : "class_part_init_posthook");
            printf("\t/* ext_resources          */      %s,\n", ptr->ext_resources == NULL ? "NULL" : "ext_resources");
            printf("\t/* compiled_ext_resources */      %s,\n", ptr->compiled_ext_resources == NULL ? "NULL" : "compiled_ext_resources");
            printf("\t/* num_ext_resources      */      %s,\n", ptr->ext_resources == NULL ? "0" : "XtNumber(ext_resources)");
            printf("\t/* use_sub_resources     */       %s,\n", ptr->use_sub_resources == False ? "False" : "True");
            printf("\t/* widget_navigable       */      %s,\n", ptr->widgetNavigable == NULL ? "NULL" : ptr->widgetNavigable == XmInheritWidgetNavigable ? "XmInheritWidgetNavigable" : "widgetNavigable");
            printf("\t/* focus_change           */      %s,\n", ptr->focusChange == NULL ? "NULL" : ptr->focusChange == XmInheritFocusChange ? "XmInheritFocusChange" : "focusChange");
            printf("\t/* wrapper_data           */      %s,\n", ptr->wrapperData == NULL ? "NULL" : "wrapperData");

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    else if (class == (WidgetClass)&xmPrimitiveClassRec)
    {
    XmPrimitiveClassExtRec *ptr = (((XmPrimitiveClassRec *)base)->primitive_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static %sClassExtRec _%sPrimClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);
            printf("\t/* next_extension        */       %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Primitive Class Extension");
            printf("\t/* record_type           */       NULLQUARK,\n");
            printf("\t/* version               */       %sClassExtVersion,\n",class->core_class.class_name);
            printf("\t/* record_size           */       sizeof(%sClassExtRec),\n",class->core_class.class_name);
            if (ptr->widget_baseline == NULL)
            {
                printf("\t/* widget_baseline       */   NULL,\n");
            }
            else if (ptr->widget_baseline == XmInheritBaselineProc)
            {
                printf("\t/* widget_baseline       */   XmInheritBaselineProc,\n");
            }
            else
            {
                printf("\t/* widget_baseline       */   _%sGetBaselines,\n", base->core_class.class_name);
            }
            if (ptr->widget_display_rect == NULL)
            {
                printf("\t/* widget_display_rect   */   NULL,\n");
            }
            else if (ptr->widget_display_rect == XmInheritDisplayRectProc)
            {
                printf("\t/* widget_display_rect   */   XmInheritDisplayRectProc,\n");
            }
            else
            {
                printf("\t/* widget_display_rect   */   _%sGetDisplayRect,\n", base->core_class.class_name);
            }
            if (ptr->widget_margins == NULL)
            {
                printf("\t/* widget_margins        */   NULL,\n");
            }
            else if (ptr->widget_margins == XmInheritMarginsProc)
            {
                printf("\t/* widget_margins        */   XmInheritMarginsProc,\n");
            }
            else
            {
                printf("\t/* widget_margins        */   _%sGetMargins,\n", base->core_class.class_name);
            }

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    else if (class == (WidgetClass)&xmManagerClassRec)
    {
    XmManagerClassExtRec *ptr = (((XmManagerClassRec *)base)->manager_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static %sClassExtRec _%sManagerClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);
            printf("\t/* next_extension        */       %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Primitive Class Extension");
            printf("\t/* record_type           */       NULLQUARK,\n");
            printf("\t/* version               */       %sClassExtVersion,\n",class->core_class.class_name);
            printf("\t/* record_size           */       sizeof(%sClassExtRec),\n",class->core_class.class_name);
            printf("\t/* traversal_children    */       %s,\n", ptr->traversal_children == NULL ? "NULL" : ptr->traversal_children == XmInheritTraversalChildrenProc ? "XmInheritTraversalChildrenProc" : "traversal_children");

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    else if (class == (WidgetClass)&xmGadgetClassRec)
    {
    XmGadgetClassExtRec *ptr = (((XmGadgetClassRec *)base)->gadget_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static %sClassExtRec _%sGadgetClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);
            printf("\t/* next_extension        */       %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Gadget Class Extension");
            printf("\t/* record_type           */       NULLQUARK,\n");
            printf("\t/* version               */       %sClassExtVersion,\n",class->core_class.class_name);
            printf("\t/* record_size           */       sizeof(%sClassExtRec),\n",class->core_class.class_name);
            printf("\t/* widget_baseline       */       %s,\n", ptr->widget_baseline == NULL ? "NULL" : ptr->widget_baseline == XmInheritBaselineProc ? "XmInheritBaselineProc" : "widget_baseline");
            printf("\t/* widget_display_rect   */       %s,\n", ptr->widget_display_rect == NULL ? "NULL" : ptr->widget_display_rect == XmInheritDisplayRectProc ? "XmInheritDisplayRectProc" : "widget_display_rect");

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    /* Motif does not seem to have this
    else if (class == (WidgetClass)&xmTextInnerClassRec)
    {
    }
    */
    else if (class == (WidgetClass)&rectObjClassRec)
    {
    }
    else if (class == (WidgetClass)&xmLabelClassRec)
    {
    }
    else if (class == (WidgetClass)&xmLabelGadgetClassRec)
    {
    }
    else if (class == (WidgetClass)&xmArrowButtonClassRec)
    {
    }
    else if (class == (WidgetClass)&xmArrowButtonGadgetClassRec)
    {
    }
    else if (class == (WidgetClass)&xmListClassRec)
    {
    }
    else if (class == (WidgetClass)&xmSashClassRec)
    {
    }
    else if (class == (WidgetClass)&xmScrollBarClassRec)
    {
    }
    else if (class == (WidgetClass)&xmSeparatorClassRec)
    {
    }
    else if (class == (WidgetClass)&xmTextFieldClassRec)
    {
    }
    else if (class == (WidgetClass)&xmTextClassRec)
    {
    }
    else if (class == (WidgetClass)&xmCascadeButtonClassRec)
    {
    }
    else if (class == (WidgetClass)&xmCascadeButtonGadgetClassRec)
    {
    }
    else if (class == (WidgetClass)&xmBulletinBoardClassRec)
    {
    }
    else if (class == (WidgetClass)&xmSelectionBoxClassRec)
    {
    }
    else if (class == (WidgetClass)&xmCommandClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDrawingAreaClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDrawnButtonClassRec)
    {
    }
    else if (class == (WidgetClass)&xmFileSelectionBoxClassRec)
    {
    }
    else if (class == (WidgetClass)&xmFormClassRec)
    {
    }
    else if (class == (WidgetClass)&xmFrameClassRec)
    {
    }
    else if (class == (WidgetClass)&xmMainWindowClassRec)
    {
    }
    else if (class == (WidgetClass)&xmScrolledWindowClassRec)
    {
    }
    else if (class == (WidgetClass)&xmMessageBoxClassRec)
    {
    }
    else if (class == (WidgetClass)&xmPanedWindowClassRec)
    {
    }
    else if (class == (WidgetClass)&xmPushButtonClassRec)
    {
    }
    else if (class == (WidgetClass)&xmRowColumnClassRec)
    {
    }
    else if (class == (WidgetClass)&xmScaleClassRec)
    {
    }
    else if (class == (WidgetClass)&xmToggleButtonClassRec)
    {
    }
    else if (class == (WidgetClass)&wmShellClassRec)
    {
    }
    else if (class == (WidgetClass)&vendorShellClassRec)
    {
    }
    else if (class == (WidgetClass)&objectClassRec)
    {
    }
    else if (class == (WidgetClass)&transientShellClassRec)
    {
    }
    else if (class == (WidgetClass)&overrideShellClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDesktopClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDropSiteManagerClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDialogShellClassRec)
    {
    }
    else if (class == (WidgetClass)&xmShellExtClassRec)
    {
    }
    else if (class == (WidgetClass)&xmVendorShellExtClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDialogShellExtClassRec)
    {
    }
    else if (class == (WidgetClass)&xmMenuShellClassRec)
    {
    }
    else if (class == (WidgetClass)&xmPushButtonGadgetClassRec)
    {
    }
#ifdef WORLD
    else if (class == (WidgetClass)&xmWorldClassRec)
    {
    }
#endif
    else if (class == (WidgetClass)&xmSeparatorGadgetClassRec)
    {
    }
    else if (class == (WidgetClass)&xmTearOffButtonClassRec)
    {
    }
    else if (class == (WidgetClass)&xmToggleButtonGadgetClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDragContextClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDragIconClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDragOverShellClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDragOverShellClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDropTransferClassRec)
    {
    }
    else if (class == (WidgetClass)&xmExtClassRec)
    {
    }
    else if (class == (WidgetClass)&xmDisplayClassRec)
    {
    }
    else if (class == (WidgetClass)&xmScreenClassRec)
    {
    }
    else if (class == (WidgetClass)&xmProtocolClassRec)
    {
    }
    else if (class == (WidgetClass)&topLevelShellClassRec)
    {
    }
    else if (class == (WidgetClass)&applicationShellClassRec)
    {
    }
    else if (class == (WidgetClass)&compositeClassRec)
    {
    CompositeClassExtensionRec *ptr = (((CompositeClassRec *)base)->composite_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static %sClassExtRec _%sCompClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);
            printf("\t/* next_extension            */   %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Primitive Class Extension");
            printf("\t/* record_type               */   NULLQUARK,\n");
            printf("\t/* version                   */   %sClassExtVersion,\n",class->core_class.class_name);
            printf("\t/* record_size               */   sizeof(%sClassExtRec),\n",class->core_class.class_name);
            printf("\t/* accepts_objects           */   %s,\n",ptr->accepts_objects ? "True" : "False");
printf("#if XtSpecificationRelease >= 6\n");
#if XtSpecificationRelease >= 6
            printf("\t/* allows_change_managed_set */   %s,\n",ptr->allows_change_managed_set ? "True" : "False");
#else
            printf("\t/* allows_change_managed_set */   False,\n");
#endif
printf("#endif\n");

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    else if (class == (WidgetClass)&constraintClassRec)
    {
    ConstraintClassExtensionRec *ptr = (((ConstraintClassRec *)base)->constraint_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static %sClassExtRec _%sConstraintClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);
            printf("\t/* next_extension        */       %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Primitive Class Extension");
            printf("\t/* record_type           */       NULLQUARK,\n");
            printf("\t/* version               */       %sClassExtVersion,\n",class->core_class.class_name);
            printf("\t/* record_size           */       sizeof(%sClassExtRec),\n",class->core_class.class_name);
            printf("\t/* get_values_hook       */       %s,\n",ptr->get_values_hook == NULL ? "NULL" : "constraint_get_values_hook");

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    else if (class == (WidgetClass)&shellClassRec)
    {
    ShellClassExtensionRec *ptr = (((ShellClassRec *)base)->shell_class.extension);

        if (ptr != NULL)
        {
            printf("/* *INDENT-OFF* */\n");
            printf("static %sClassExtRec _%sShellClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);
            printf("\t/* next_extension        */       %s,\n", ptr->next_extension == NULL ? "NULL" : "2nd Shell Class Extension");
            printf("\t/* record_type           */       NULLQUARK,\n");
            printf("\t/* version               */       %sClassExtVersion,\n",class->core_class.class_name);
            printf("\t/* record_size           */       sizeof(%sClassExtRec),\n",class->core_class.class_name);
            printf("\t/* root_geometry_manager */       %s,\n",ptr->root_geometry_manager == NULL ? "NULL" : ptr->root_geometry_manager == XtInheritRootGeometryManager ? "XtInheritRootGeometryManager" : "root_geometry_manager");

            printf("};\n");
            printf("/* *INDENT-ON* */\n\n");
        }
    }
    else
    {
        printf("/* *INDENT-OFF* */\n");
        printf("static %sClassExtRec _%sXXXClassExtRec = {\n",class->core_class.class_name,base->core_class.class_name);

        printf("DoExtensionRecord(%s) - UNKNOWN\n",class->core_class.class_name);

        printf("};\n");
        printf("/* *INDENT-ON* */\n\n");
    }
}

static void
DoClass(WidgetClass base, WidgetClass class)
{
        if (class != (WidgetClass)&rectObjClassRec && class != &coreClassRec && class->core_class.superclass != NULL)
        {
            DoClass(base, class->core_class.superclass);
        }
        if (class == (WidgetClass)&coreClassRec ||
            class == (WidgetClass)&rectObjClassRec ||
            class == (WidgetClass)&objectClassRec)
        {
        CoreClassPart *ptr = &(base->core_class);

                printf("\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* superclass            */   (WidgetClass) &%s,\n", ClassRec2String(ptr->superclass));
                printf("\t/* class_name            */   \"%s\",\n", ptr->class_name);
                printf("\t/* widget_size           */   sizeof(%sRec),\n", ptr->class_name);
                printf("\t/* class_initialize      */   %s,\n", ptr->class_initialize == NULL ? "NULL" : "class_initialize");
                printf("\t/* class_part_initialize */   %s,\n", ptr->class_part_initialize == NULL ? "NULL" : "class_part_initialize");
                printf("\t/* class_inited          */   %s,\n", ptr->class_inited == False ? "False" : "True");
                printf("\t/* initialize            */   %s,\n", ptr->initialize == NULL ? "NULL" : "initialize");
                printf("\t/* initialize_hook       */   %s,\n", ptr->initialize_hook == NULL ? "NULL" : "initialize_hook");
                printf("\t/* realize               */   %s,\n", ptr->realize == XtInheritRealize ? "XtInheritRealize" : "realize");
                printf("\t/* actions               */   %s,\n", ptr->actions == NULL ? "NULL" : "actions");
                printf("\t/* num_actions           */   %s,\n", ptr->actions == NULL ? "0" : "XtNumber(actions)");
                printf("\t/* resources             */   %s,\n", ptr->resources == NULL ? "NULL" : "resources");
                printf("\t/* num_resources         */   %s,\n", ptr->resources == NULL ? "0" : "XtNumber(resources)");
                printf("\t/* xrm_class             */   NULLQUARK,\n");
                printf("\t/* compress_motion       */   %s,\n", ptr->compress_motion == False ? "False" : "True");
                printf("\t/* compress_exposure     */   %s,\n", CompressExpose2String(ptr->compress_exposure));
                printf("\t/* compress_enterleave   */   %s,\n", ptr->compress_enterleave == False ? "False" : "True");
                printf("\t/* visible_interest      */   %s,\n", ptr->visible_interest == False ? "False" : "True");
                printf("\t/* destroy               */   %s,\n", ptr->destroy == NULL ? "NULL" : "destroy");
                printf("\t/* resize                */   %s,\n", ptr->resize == NULL ? "NULL" : ptr->resize == XtInheritResize ? "XtInheritResize" : "resize");
                printf("\t/* expose                */   %s,\n", ptr->expose == NULL ? "NULL" : ptr->expose == XtInheritExpose ? "XtInheritExpose" : "expose");
                printf("\t/* set_values            */   %s,\n", ptr->set_values == NULL ? "NULL" : "set_values");
                printf("\t/* set_values_hook       */   %s,\n", ptr->set_values_hook == NULL ? "NULL" : "set_values_hook");
                printf("\t/* set_values_almost     */   %s,\n", ptr->set_values_almost == NULL ? "NULL" : ptr->set_values_almost == XtInheritSetValuesAlmost ? "XtInheritSetValuesAlmost" : "set_values_almost");
                printf("\t/* get_values_hook       */   %s,\n", ptr->get_values_hook == NULL ? "NULL" : "get_values_hook");
                printf("\t/* accept_focus          */   %s,\n", ptr->accept_focus == NULL ? "NULL" : ptr->accept_focus == XtInheritAcceptFocus ? "XtInheritAcceptFocus" : "accept_focus");
                printf("\t/* version               */   %s,\n", ptr->version == XtVersion ? "XtVersion" : "XtVersionDontCheck");
                printf("\t/* callback_private      */   NULL,\n");
                printf("\t/* tm_table              */   %s,\n", ptr->tm_table == NULL ? "NULL" : ptr->tm_table == XtInheritTranslations ? "XtInheritTranslations" : "tm_table");
                printf("\t/* query_geometry        */   %s,\n", ptr->query_geometry == NULL ? "NULL" : ptr->query_geometry == XtInheritQueryGeometry ? "XtInheritQueryGeometry" : "query_geometry");
                printf("\t/* display_accelerator   */   %s,\n", ptr->display_accelerator == NULL ? "NULL" : ptr->display_accelerator == XtInheritDisplayAccelerator ? "XtInheritDisplayAccelerator" : "display_accelerator");
                printf("\t/* extension             */   %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : ptr->class_name,ptr->extension == NULL ? "" : "CoreClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmPrimitiveClassRec)
        {
        XmPrimitiveClassPart *ptr = &(((XmPrimitiveClassRec *)base)->primitive_class);

                printf(",\n\t/* Primitive class part */\n\t{\n");
                printf("\t/* border_highlight      */   %s,\n", ptr->border_highlight == NULL ? "NULL" : ptr->border_highlight == XmInheritBorderHighlight ? "XmInheritBorderHighlight" : "border_highlight");
                printf("\t/* border_unhighlight      */ %s,\n", ptr->border_unhighlight == NULL ? "NULL" : ptr->border_unhighlight == XmInheritBorderUnhighlight ? "XmInheritBorderUnhighlight" : "border_unhighlight");
                printf("\t/* translations            */ %s,\n", ptr->translations == NULL ? "NULL" : ptr->translations == XtInheritTranslations ? "XtInheritTranslations" : "primitive_translations");
                printf("\t/* arm_and_activate        */ %s,\n", ptr->arm_and_activate == NULL ? "NULL" : ptr->arm_and_activate == XmInheritArmAndActivate ? "XmInheritArmAndActivate" : "arm_and_activate");
                printf("\t/* syn_resources           */ %s,\n", ptr->syn_resources == NULL ? "NULL" : "syn_resources");
                printf("\t/* num_syn_resources       */ %s,\n", ptr->syn_resources == NULL ? "0" : "XtNumber(syn_resources)");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "PrimClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmLabelClassRec)
        {
        XmLabelClassPart *ptr = &(((XmLabelClassRec *)base)->label_class);

                printf(",\n\t/* Label class part */\n\t{\n");
                printf("\t/* setOverrideCallback     */ %s,\n", ptr->setOverrideCallback == NULL ? "NULL" : ptr->setOverrideCallback == XmInheritSetOverrideCallback ? "XmInheritSetOverrideCallback" : "setOverrideCallback");
                printf("\t/* menuProcs               */ %s,\n", ptr->menuProcs == NULL ? "NULL" : ptr->menuProcs == XmInheritMenuProc ? "XmInheritMenuProc" : "menuProcs");
                printf("\t/* translations            */ %s\n", ptr->translations == NULL ? "NULL" : ptr->translations == XtInheritTranslations ? "XtInheritTranslations" : "label_translations");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "LabelClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmArrowButtonClassRec)
        {
        XmArrowButtonClassPart *ptr = &(((XmArrowButtonClassRec *)base)->arrowbutton_class);

                printf(",\n\t/* ArrowButton class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ArrowButtonClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmListClassRec)
        {
        XmListClassPart *ptr = &(((XmListClassRec *)base)->list_class);

                printf(",\n\t/* List class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ListClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmSashClassRec)
        {
        XmSashClassPart *ptr = &(((XmSashClassRec *)base)->sash_class);

                printf(",\n\t/* Sash class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "SashClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmScrollBarClassRec)
        {
        XmScrollBarClassPart *ptr = &(((XmScrollBarClassRec *)base)->scrollBar_class);

                printf(",\n\t/* ScrollBar class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ScrollBarClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmSeparatorClassRec)
        {
        XmSeparatorClassPart *ptr = &(((XmSeparatorClassRec *)base)->separator_class);

                printf(",\n\t/* Separator class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "SeparatorClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmTextFieldClassRec)
        {
#ifdef LESSTIF_VERSION
        XmTextFieldClassPart *ptr = &(((XmTextFieldClassRec *)base)->textfield_class);
#else
        XmTextFieldClassPart *ptr = &(((XmTextFieldClassRec *)base)->text_class);
#endif

                printf(",\n\t/* TextField class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "TextFieldClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmTextClassRec)
        {
        XmTextClassPart *ptr = &(((XmTextClassRec *)base)->text_class);

                printf(",\n\t/* Text class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "TextClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&compositeClassRec)
        {
        CompositeClassPart *ptr = &(((CompositeClassRec *)base)->composite_class);

                printf(",\n\t/* Composite class part */\n\t{\n");
                printf("\t/* geometry_manager        */ %s,\n", ptr->geometry_manager == NULL ? "NULL" : ptr->geometry_manager == XtInheritGeometryManager ? "XtInheritGeometryManager" : "geometry_manager");
                printf("\t/* change_managed          */ %s,\n", ptr->change_managed == NULL ? "NULL" : ptr->change_managed == XtInheritChangeManaged ? "XtInheritChangeManaged" : "change_managed");
                printf("\t/* insert_child            */ %s,\n", ptr->insert_child == NULL ? "NULL" : ptr->insert_child == XtInheritInsertChild ? "XtInheritInsertChild" : "insert_child");
                printf("\t/* delete_child            */ %s,\n", ptr->delete_child == NULL ? "NULL" : ptr->delete_child == XtInheritDeleteChild ? "XtInheritDeleteChild" : "delete_child");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "CompositeClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&constraintClassRec)
        {
        ConstraintClassPart *ptr = &(((ConstraintClassRec *)base)->constraint_class);

                printf(",\n\t/* Constraint class part */\n\t{\n");
                printf("\t/* resources               */ %s,\n", ptr->resources == NULL ? "NULL" : "constraint_resources");
                printf("\t/* num_resources           */ %s,\n", ptr->resources == NULL ? "0" : "XtNumber(constraint_resources)");
                printf("\t/* constraint_size       */   sizeof(%sRec),\n", base->core_class.class_name);
                printf("\t/* initialize              */ %s,\n", ptr->initialize == NULL ? "NULL" : /*ptr->initialize == XtInheritChangeManaged ? "XtInheritChangeManaged" :*/ "constraint_initialize");
                printf("\t/* destroy                 */ %s,\n", ptr->destroy == NULL ? "NULL" : /*ptr->destroy == XtInheritInsertChild ? "XtInheritInsertChild" :*/ "constraint_destroy");
                printf("\t/* set_values              */ %s,\n", ptr->set_values == NULL ? "NULL" : /*ptr->set_values == XtInheritDeleteChild ? "XtInheritDeleteChild" :*/ "constraint_set_values");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "CompositeClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmManagerClassRec)
        {
        XmManagerClassPart *ptr = &(((XmManagerClassRec *)base)->manager_class);

                printf(",\n\t/* Manager class part */\n\t{\n");
                printf("\t/* translations            */ %s\n", ptr->translations == NULL ? "NULL" : ptr->translations == XtInheritTranslations ? "XtInheritTranslations" : "manager_translations");
                printf("\t/* syn_resources           */ %s,\n", ptr->syn_resources == NULL ? "NULL" : "manager_syn_resources");
                printf("\t/* num_syn_resources       */ %s,\n", ptr->syn_resources == NULL ? "0" : "XtNumber(manager_syn_resources)");
                printf("\t/* syn_constraint_resources*/ %s,\n", ptr->syn_constraint_resources == NULL ? "NULL" : "manager_syn_constraint_resources");
                printf("\t/* num_syn_constraint_resources*/     %s,\n", ptr->syn_constraint_resources == NULL ? "0" : "XtNumber(manager_syn_constraint_resources)");
                printf("\t/* parent_process          */ %s,\n", ptr->parent_process == NULL ? "NULL" : ptr->parent_process == XmInheritParentProcess ? "XmInheritParentProcess" : "parent_process");
                if (ptr->parent_process != NULL && ptr->parent_process != XmInheritParentProcess)
                {
                    SaveItem(base->core_class.class_name, "parent_process", (XtPointer)ptr->parent_process);
                }
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ManagerClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmBulletinBoardClassRec)
        {
        XmBulletinBoardClassPart *ptr = &(((XmBulletinBoardClassRec *)base)->bulletin_board_class);

                printf(",\n\t/* BulletinBoard class part */\n\t{\n");
                printf("\t/* always_install_accelerators*/      %s,\n", ptr->always_install_accelerators == False ? "False" : "True");
                printf("\t/* geo_matrix_create       */ %s,\n", ptr->geo_matrix_create == NULL ? "NULL" : ptr->geo_matrix_create == XmInheritGeoMatrixCreate ? "XmInheriteGeoMatrixCreate" : "geo_matrix_create");
                printf("\t/* focus_moved_proc        */ %s,\n", ptr->focus_moved_proc == NULL ? "NULL" : ptr->focus_moved_proc == XmInheritFocusMovedProc ? "XmInheritFocusMovedProc" : "focus_moved_proc");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "BulletinBoardClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmSelectionBoxClassRec)
        {
        XmSelectionBoxClassPart *ptr = &(((XmSelectionBoxClassRec *)base)->selection_box_class);

                printf(",\n\t/* SelectionBox class part */\n\t{\n");
#ifdef LESSTIF_VERSION
                printf("********/* list_callback (missing in LessTif) */        \n");
#else
                printf("\t/* list_callback           */ %s,\n", ptr->list_callback == NULL ? "NULL" : "list_callback");
#endif
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "SelectionBoxClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmFileSelectionBoxClassRec)
        {
        XmFileSelectionBoxClassPart *ptr = &(((XmFileSelectionBoxClassRec *)base)->file_selection_box_class);

                printf(",\n\t/* FileSelectionBox class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "FileSelectionBoxClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmCascadeButtonClassRec)
        {
        XmCascadeButtonClassPart *ptr = &(((XmCascadeButtonClassRec *)base)->cascade_button_class);

                printf(",\n\t/* CascadeButton class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "CascadeButtonClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmCommandClassRec)
        {
        XmCommandClassPart *ptr = &(((XmCommandClassRec *)base)->command_class);

                printf(",\n\t/* Command class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "CommandClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDrawingAreaClassRec)
        {
        XmDrawingAreaClassPart *ptr = &(((XmDrawingAreaClassRec *)base)->drawing_area_class);

                printf(",\n\t/* DrawingArea class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DrawingAreaClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDrawnButtonClassRec)
        {
        XmDrawnButtonClassPart *ptr = &(((XmDrawnButtonClassRec *)base)->drawnbutton_class);

                printf(",\n\t/* DrawnButton class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DrawnButtonClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmFormClassRec)
        {
        XmFormClassPart *ptr = &(((XmFormClassRec *)base)->form_class);

                printf(",\n\t/* Form class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "FormClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmScrolledWindowClassRec)
        {
#ifdef LESSTIF_VERSION
        XmScrolledWindowClassPart *ptr = &(((XmScrolledWindowClassRec *)base)->scrolled_window_class);
#else
        XmScrolledWindowClassPart *ptr = &(((XmScrolledWindowClassRec *)base)->swindow_class);
#endif

                printf(",\n\t/* ScrolledWindow class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ScrolledWindowClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmMainWindowClassRec)
        {
#ifdef LESSTIF_VERSION
        XmMainWindowClassPart *ptr = &(((XmMainWindowClassRec *)base)->main_window_class);
#else
        XmMainWindowClassPart *ptr = &(((XmMainWindowClassRec *)base)->mwindow_class);
#endif

                printf(",\n\t/* MainWindow class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "MainWindowClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmMessageBoxClassRec)
        {
#ifdef LESSTIF_VERSION
        XmMessageBoxClassPart *ptr = &(((XmMessageBoxClassRec *)base)->messagebox_class);
#else
        XmMessageBoxClassPart *ptr = &(((XmMessageBoxClassRec *)base)->message_box_class);
#endif

                printf(",\n\t/* MessageBox class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "MessageBoxClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmFrameClassRec)
        {
        XmFrameClassPart *ptr = &(((XmFrameClassRec *)base)->frame_class);

                printf(",\n\t/* Frame class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "FrameClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmPanedWindowClassRec)
        {
        XmPanedWindowClassPart *ptr = &(((XmPanedWindowClassRec *)base)->paned_window_class);

                printf(",\n\t/* PanedWindow class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "PanedWindowClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmPushButtonClassRec)
        {
        XmPushButtonClassPart *ptr = &(((XmPushButtonClassRec *)base)->pushbutton_class);

                printf(",\n\t/* PushButton class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "PushButtonClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmRowColumnClassRec)
        {
        XmRowColumnClassPart *ptr = &(((XmRowColumnClassRec *)base)->row_column_class);

                printf(",\n\t/* RowColumn class part */\n\t{\n");
                printf("\t/* menuProcedures          */ %s,\n", ptr->menuProcedures == NULL ? "NULL" : "menuProcedures");
                printf("\t/* armAndActivate          */ %s,\n", ptr->armAndActivate == NULL ? "NULL" : "armAndActivate");
                printf("\t/* traversalHandler        */ %s,\n", ptr->traversalHandler == NULL ? "NULL" : "traversalHandler");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "RowColumnClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&shellClassRec)
        {
        ShellClassPart *ptr = &(((ShellClassRec *)base)->shell_class);

                printf(",\n\t/* Shell class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&wmShellClassRec)
        {
        WMShellClassPart *ptr = &(((WMShellClassRec *)base)->wm_shell_class);

                printf(",\n\t/* WMShell class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WMShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&vendorShellClassRec)
        {
        VendorShellClassPart *ptr = &(((VendorShellClassRec *)base)->vendor_shell_class);

                printf(",\n\t/* VendorShell class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "VendorShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&transientShellClassRec)
        {
        TransientShellClassPart *ptr = &(((TransientShellClassRec *)base)->transient_shell_class);

                printf(",\n\t/* TransientShell class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "TransientShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDialogShellClassRec)
        {
#ifdef LESSTIF_VERSION
        XmDialogShellClassPart *ptr = &(((XmDialogShellClassRec *)base)->dialogshell_class);
#else
        XmDialogShellClassPart *ptr = &(((XmDialogShellClassRec *)base)->dialog_shell_part);
/* older Motif versions may need this?! :
        XmDialogShellClassPart *ptr = &(((XmDialogShellClassRec *)base)->dialog_shell_class);
*/
#endif

                printf(",\n\t/* XmDialogShell class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DialogShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&overrideShellClassRec)
        {
        OverrideShellClassPart *ptr = &(((OverrideShellClassRec *)base)->override_shell_class);

                printf(",\n\t/* OverrideShell class part */\n\t{\n");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "OverrideShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmMenuShellClassRec)
        {
        XmMenuShellClassPart *ptr = &(((XmMenuShellClassRec *)base)->menu_shell_class);

                printf(",\n\t/* MenuShell class part */\n\t{\n");
                printf("\t/* popdownOne              */ %s\n", ptr->popdownOne == NULL ? "NULL" : "popdownOne");
                printf("\t/* popdownEveryone         */ %s\n", ptr->popdownEveryone == NULL ? "NULL" : "popdownEveryone");
                printf("\t/* popdownDone             */ %s\n", ptr->popdownDone == NULL ? "NULL" : "popdownDone");
                printf("\t/* popupSharedMenupane     */ %s\n", ptr->popupSharedMenupane == NULL ? "NULL" : "popupSharedMenupane");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "MenuShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmGadgetClassRec)
        {
        XmGadgetClassPart *ptr = &(((XmGadgetClassRec *)base)->gadget_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* border_highlight        */ %s\n", ptr->border_highlight == NULL ? "NULL" : ptr->border_highlight == XmInheritBorderHighlight ? "XmInheritBorderHighlight" : "border_highlight");
                printf("\t/* border_unhighlight      */ %s\n", ptr->border_unhighlight == NULL ? "NULL" : ptr->border_unhighlight == XmInheritBorderUnhighlight ? "XmInheritBorderUnhighlight" : "border_unhighlight");
                printf("\t/* arm_and_activate        */ %s\n", ptr->arm_and_activate == NULL ? "NULL" : ptr->arm_and_activate == XmInheritArmAndActivate ? "XmInheritArmAndActivate" : "arm_and_activate");
                printf("\t/* input_dispatch          */ %s\n", ptr->input_dispatch == NULL ? "NULL" : ptr->input_dispatch == XmInheritInputDispatch ? "XmInheritInputDispatch" : "input_dispatch");
                printf("\t/* visual_change           */ %s\n", ptr->visual_change == NULL ? "NULL" : ptr->visual_change == XmInheritVisualChange ? "XmInheritVisualChange" : "visual_change");
                printf("\t/* syn_resources           */ %s\n", ptr->syn_resources == NULL ? "NULL" : "gadget_syn_resources");
                printf("\t/* num_syn_resources       */ %s\n", ptr->syn_resources == NULL ? "0" : "gadget_num_syn_resources");
                printf("\t/* cache_part              */ %s\n", ptr->cache_part == NULL ? "NULL" : "cache_part");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "GadgetClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmLabelGadgetClassRec)
        {
        XmLabelGadgetClassPart *ptr = &(((XmLabelGadgetClassRec *)base)->label_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* setOverrideCallback     */ %s\n", ptr->setOverrideCallback == NULL ? "NULL" : ptr->setOverrideCallback == XmInheritSetOverrideCallback ? "XmInheritSetOverrideCallback" : "setOverrideCallback");
                printf("\t/* menuProcs               */ %s\n", ptr->menuProcs == NULL ? "NULL" : ptr->menuProcs == XmInheritMenuProc ? "XmInheritMenuProc" : "menuProcs");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "LabelGadgetClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmScaleClassRec)
        {
        XmScaleClassPart *ptr = &(((XmScaleClassRec *)base)->scale_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ScaleClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmToggleButtonClassRec)
        {
        XmToggleButtonClassPart *ptr = &(((XmToggleButtonClassRec *)base)->toggle_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ToggleButtonClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmArrowButtonGadgetClassRec)
        {
#ifdef LESSTIF_VERSION
        XmArrowButtonGadgetClassPart *ptr = &(((XmArrowButtonGadgetClassRec *)base)->arrowbutton_class);
#else
        XmArrowButtonGadgetClassPart *ptr = &(((XmArrowButtonGadgetClassRec *)base)->arrow_button_class);
#endif

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ArrowButtonGadgetClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmCascadeButtonGadgetClassRec)
        {
        XmCascadeButtonGadgetClassPart *ptr = &(((XmCascadeButtonGadgetClassRec *)base)->cascade_button_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "CascadeButtonGadgetClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmExtClassRec)
        {
        XmExtClassPart *ptr = &(((XmExtClassRec *)base)->ext_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* syn_resources           */ %s\n", ptr->syn_resources == NULL ? "NULL" : "gadget_syn_resources");
                printf("\t/* num_syn_resources       */ %s\n", ptr->syn_resources == NULL ? "0" : "gadget_num_syn_resources");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ExtClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDesktopClassRec)
        {
        XmDesktopClassPart *ptr = &(((XmDesktopClassRec *)base)->desktop_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* child_class             */ %s\n", ptr->child_class == NULL ? "NULL" : ptr->child_class->core_class.class_name);
                printf("\t/* insert_child            */ %s\n", ptr->insert_child == NULL ? "NULL" : ptr->insert_child == XtInheritInsertChild ? "XtInheritInsertChild" : "insert_child");
                printf("\t/* delete_child            */ %s\n", ptr->delete_child == NULL ? "NULL" : ptr->delete_child == XtInheritDeleteChild ? "XtInheritDeleteChild" : "delete_child");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DesktopClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmVendorShellExtClassRec)
        {
        XmVendorShellExtClassPart *ptr = &(((XmVendorShellExtClassRec *)base)->vendor_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* delete_window_handlerr  */ %s\n", ptr->delete_window_handler == NULL ? "NULL" : "delete_window_handler");
                printf("\t/* offset_handler          */ %s\n", ptr->offset_handler == NULL ? "NULL" : "offset_handler");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "VendorShellExtClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDialogShellExtClassRec)
        {
        XmDialogShellExtClassPart *ptr = &(((XmDialogShellExtClassRec *)base)->dialog_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DialogShellExtClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDragContextClassRec)
        {
        XmDragContextClassPart *ptr = &(((XmDragContextClassRec *)base)->drag_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* start                   */ %s\n", ptr->start == NULL ? "NULL" : "start");
                printf("\t/* cancel                  */ %s\n", ptr->cancel == NULL ? "NULL" : "cancel");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DragContextClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDragIconClassRec)
        {
        XmDragIconClassPart *ptr = &(((XmDragIconClassRec *)base)->dragIcon_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DragIconClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDragOverShellClassRec)
        {
        XmDragOverShellClassPart *ptr = &(((XmDragOverShellClassRec *)base)->dragOver_shell_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DragOverShellClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmShellExtClassRec)
        {
        XmShellExtClassPart *ptr = &(((XmShellExtClassRec *)base)->shell_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* structureNotifyHandler  */ %s\n", ptr->structureNotifyHandler == NULL ? "NULL" : "structureNotifyHandler");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DesktopClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDropTransferClassRec)
        {
        XmDropTransferClassPart *ptr = &(((XmDropTransferClassRec *)base)->dropTransfer_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* start_drop_transfer     */ %s\n", ptr->start_drop_transfer == NULL ? "NULL" : "start_drop_transfer");
                printf("\t/* add_drop_transfer       */ %s\n", ptr->add_drop_transfer == NULL ? "NULL" : "add_drop_transfer");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "DropTransferClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmPushButtonGadgetClassRec)
        {
        XmPushButtonGadgetClassPart *ptr = &(((XmPushButtonGadgetClassRec *)base)->pushbutton_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "PushButtonGadgetClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmSeparatorGadgetClassRec)
        {
        XmSeparatorGadgetClassPart *ptr = &(((XmSeparatorGadgetClassRec *)base)->separator_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "SeparatorGadgetClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmTearOffButtonClassRec)
        {
        XmTearOffButtonClassPart *ptr = &(((XmTearOffButtonClassRec *)base)->tearoffbutton_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* translations            */ %s\n", ptr->translations == NULL ? "NULL" : "tearoffbutton_translations");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmToggleButtonGadgetClassRec)
        {
        XmToggleButtonGadgetClassPart *ptr = &(((XmToggleButtonGadgetClassRec *)base)->toggle_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "ToggleButtonGadgetClassExtRec");
                printf("\t}");
        }
#ifdef WORLD
        else if (class == (WidgetClass)&xmWorldClassRec)
        {
        XmWorldClassPart *ptr = &(((XmWorldClassRec *)base)->world_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
#endif
        else if (class == (WidgetClass)&topLevelShellClassRec)
        {
        TopLevelShellClassPart *ptr = &(((TopLevelShellClassRec *)base)->top_level_shell_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&applicationShellClassRec)
        {
        ApplicationShellClassPart *ptr = &(((ApplicationShellClassRec *)base)->application_shell_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDisplayClassRec)
        {
        XmDisplayClassPart *ptr = &(((XmDisplayClassRec *)base)->display_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* GetDisplay              */ %s\n", ptr->GetDisplay == NULL ? "NULL" : "GetDisplay");
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmProtocolClassRec)
        {
        XmProtocolClassPart *ptr = &(((XmProtocolClassRec *)base)->protocol_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmScreenClassRec)
        {
        XmScreenClassPart *ptr = &(((XmScreenClassRec *)base)->screen_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
        else if (class == (WidgetClass)&xmDropSiteManagerClassRec)
        {
        XmDropSiteManagerClassPart *ptr = &(((XmDropSiteManagerClassRec *)base)->dropManager_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* createInfo              */ %s\n", ptr->createInfo == NULL ? "NULL" : "createInfo");
                printf("\t/* destroyInfo             */ %s\n", ptr->destroyInfo == NULL ? "NULL" : "destroyInfo");
                printf("\t/* startUpdate             */ %s\n", ptr->startUpdate == NULL ? "NULL" : "startUpdate");
                printf("\t/* retrieveInfo            */ %s\n", ptr->retrieveInfo == NULL ? "NULL" : "retrieveInfo");
                printf("\t/* updateInfo              */ %s\n", ptr->updateInfo == NULL ? "NULL" : "updateInfo");
                printf("\t/* endUpdate               */ %s\n", ptr->endUpdate == NULL ? "NULL" : "endUpdate");
                printf("\t/* updateDSM               */ %s\n", ptr->updateDSM == NULL ? "NULL" : "updateDSM");
                printf("\t/* processMotion           */ %s\n", ptr->processMotion == NULL ? "NULL" : "processMotion");
                printf("\t/* processDrop             */ %s\n", ptr->processDrop == NULL ? "NULL" : "processDrop");
                printf("\t/* operationChanged        */ %s\n", ptr->operationChanged == NULL ? "NULL" : "operationChanged");
                printf("\t/* changeRoot              */ %s\n", ptr->changeRoot == NULL ? "NULL" : "changeRoot");
                printf("\t/* insertInfo              */ %s\n", ptr->insertInfo == NULL ? "NULL" : "insertInfo");
                printf("\t/* removeInfo              */ %s\n", ptr->removeInfo == NULL ? "NULL" : "removeInfo");
                printf("\t/* syncTree                */ %s\n", ptr->syncTree == NULL ? "NULL" : "syncTree");
                printf("\t/* getTreeFromDSM          */ %s\n", ptr->getTreeFromDSM == NULL ? "NULL" : "getTreeFromDSM");
                printf("\t/* createTable             */ %s\n", ptr->createTable == NULL ? "NULL" : "createTable");
                printf("\t/* destroyTable            */ %s\n", ptr->destroyTable == NULL ? "NULL" : "destroyTable");
                printf("\t/* registerInfo            */ %s\n", ptr->registerInfo == NULL ? "NULL" : "registerInfo");
                printf("\t/* widgetToInfo            */ %s\n", ptr->widgetToInfo == NULL ? "NULL" : "widgetToInfo");
                printf("\t/* unregisterInfo          */ %s\n", ptr->unregisterInfo == NULL ? "NULL" : "unregisterInfo");

                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
#if 0
        /* Motif does not seem to have this */
        else if (class == (WidgetClass)&xmTextInnerClassRec)
        {
        XmTextClassPart *ptr = &(((XmTextClassRec *)base)->text_class);

                printf(",\n\t/* %s class part */\n\t{\n",class->core_class.class_name);
                printf("\t/* extension               */ %s%s%s\n", ptr->extension == NULL ? "NULL" : "(XtPointer)&_",ptr->extension == NULL ? "" : base->core_class.class_name,ptr->extension == NULL ? "" : "WorldClassExtRec");
                printf("\t}");
        }
#endif
        else
        {
                printf(",\n\t/* %s class part */\n\t{\n", class->core_class.class_name);
                printf("Unknown class is %s\n",class->core_class.class_name);
                printf("\t}");
        }
}

static void
DoClassRecord(WidgetClass class)
{
        printf("/* *INDENT-OFF* */\n");
        printf("%sClassRec %c%c%sClassRec = {\n", class->core_class.class_name, 
                tolower(class->core_class.class_name[0]),
                tolower(class->core_class.class_name[1]),
                &(class->core_class.class_name[2]) );
        DoClass(class, class);
        printf("\n};\n/* *INDENT-ON* */\n\n");
        printf("WidgetClass %c%c%sWidgetClass = (WidgetClass)&%c%c%sClassRec;\n", 
                tolower(class->core_class.class_name[0]),
                tolower(class->core_class.class_name[1]),
                &(class->core_class.class_name[2]),
                tolower(class->core_class.class_name[0]),
                tolower(class->core_class.class_name[1]),
                &(class->core_class.class_name[2]) );
}

static void
DoSynResources(WidgetClass base, WidgetClass class)
{
        if (class != (WidgetClass)&rectObjClassRec && class != &coreClassRec && class->core_class.superclass != NULL)
        {
            DoSynResources(base, class->core_class.superclass);
        }
        if (class == (WidgetClass)&coreClassRec)
        {
        CoreClassPart *ptr = &(base->core_class);
        }
        else if (class == (WidgetClass)&xmPrimitiveClassRec)
        {
        int i;
        XmPrimitiveClassPart *ptr = &(((XmPrimitiveClassRec *)base)->primitive_class);

                if (ptr->num_syn_resources > 0)
                {
                        printf("#define Offset(field) XtOffsetOf(%sRec, %s.field)\n",
                                class->core_class.class_name, Class2Part(class));
                        printf("static XmSyntheticResource syn_resources[] =\n{\n");
                        for (i=0; i<ptr->num_syn_resources; i++)
                        {
                                printf("\t{\n\t\tXmN%s,\n\t\t/* size */ %d, Offset(%s),\n\t\t%s, %s\n\t},\n", 
                                        ptr->syn_resources[i].resource_name,
                                        ptr->syn_resources[i].resource_size,
                                        ResourceName2Variable(ptr->syn_resources[i].resource_name),
                                        ptr->syn_resources[i].export_proc == NULL ? "NULL" : "_XmExportProc",
                                        ptr->syn_resources[i].import_proc == NULL ? "NULL" : "_XmImportProc");
                        }
                        printf("};\n\n");
                        printf("#undef Offset\n");
                }
        }
        else if (class == (WidgetClass)&xmExtClassRec)
        {
        int i;
        XmExtClassPart *ptr = &(((XmExtClassRec *)base)->ext_class);

                if (ptr->num_syn_resources > 0)
                {
                        printf("#define Offset(field) XtOffsetOf(%sRec, %s.field)\n",
                                class->core_class.class_name, Class2Part(class));
                        printf("static XmSyntheticResource ext_syn_resources[] =\n{\n");
                        for (i=0; i<ptr->num_syn_resources; i++)
                        {
                                printf("\t{\n\t\tXmN%s,\n\t\t/* size */ %d, Offset(%s),\n\t\t%s, %s\n\t},\n", 
                                        ptr->syn_resources[i].resource_name,
                                        ptr->syn_resources[i].resource_size,
                                        ResourceName2Variable(ptr->syn_resources[i].resource_name),
                                        ptr->syn_resources[i].export_proc == NULL ? "NULL" : "_XmExportProc",
                                        ptr->syn_resources[i].import_proc == NULL ? "NULL" : "_XmImportProc");
                        }
                        printf("};\n\n");
                        printf("#undef Offset\n");
                }
        }
        else if (class == (WidgetClass)&xmManagerClassRec)
        {
        XmManagerClassPart *ptr = &(((XmManagerClassRec *)base)->manager_class);
                int i;
                if (ptr->num_syn_resources > 0)
                {
                        printf("#define Offset(field) XtOffsetOf(%sRec, %s.field)\n",
                                class->core_class.class_name, Class2Part(class));
                        printf("static XmSyntheticResource manager_syn_resources[] =\n{\n");
                        for (i=0; i<ptr->num_syn_resources; i++)
                        {
                                printf("\t{\n\t\tXmN%s,\n\t\t/* size */ %d, Offset(%s),\n\t\t%s, %s\n\t},\n", 
                                        ptr->syn_resources[i].resource_name,
                                        ptr->syn_resources[i].resource_size,
                                        ResourceName2Variable(ptr->syn_resources[i].resource_name),
                                        ptr->syn_resources[i].export_proc == NULL ? "NULL" : "_XmExportProc",
                                        ptr->syn_resources[i].import_proc == NULL ? "NULL" : "_XmImportProc");
                        }
                        printf("};\n\n");
                        printf("#undef Offset\n");
                }
                if (ptr->num_syn_constraint_resources > 0)
                {
                        printf("#define Offset(field) XtOffsetOf(%sRec, %s.field)\n",
                                class->core_class.class_name, Class2Part(class));
                        printf("static XmSyntheticResource manager_syn_constraint_resources[] =\n{\n");
                        for (i=0; i<ptr->num_syn_constraint_resources; i++)
                        {
                                printf("\t{\n\t\tXmN%s,\n\t\t/* size */ %d, Offset(%s),\n\t\t%s, %s\n\t},\n", 
                                        ptr->syn_constraint_resources[i].resource_name,
                                        ptr->syn_constraint_resources[i].resource_size,
                                        ptr->syn_constraint_resources[i].resource_name,
                                        ptr->syn_constraint_resources[i].export_proc == NULL ? "NULL" : "_XmExportProc",
                                        ptr->syn_constraint_resources[i].import_proc == NULL ? "NULL" : "_XmImportProc");
                        }
                        printf("};\n\n");
                        printf("#undef Offset\n");
                }
        }
}

static void
DoActions(WidgetClass base, WidgetClass class)
{
int i;

        if (class != (WidgetClass)&rectObjClassRec && class != &coreClassRec && class->core_class.superclass != NULL)
        {
            DoActions(base, class->core_class.superclass);
        }
        if (class == (WidgetClass)&coreClassRec)
        {
        CoreClassPart *ptr = &(base->core_class);

                if (ptr->num_actions > 0)
                {
                    printf("static XtActionsRec actions[] =\n{\n");
                    for (i=0; i<ptr->num_actions; i++)
                    {
                            SaveItem( base->core_class.class_name,
                                ptr->actions[i].string,
                                (XtPointer)ptr->actions[i].proc);
                            printf("\t{\"%s\", _Xm%s},\n", 
                                    ptr->actions[i].string, 
                                    ptr->actions[i].string/*, 
                                    ptr->actions[i].proc*/);
                    }
                    printf("};\n\n");
                }
                if (ptr->tm_table != NULL &&
                    ptr->tm_table != XtInheritTranslations)
                {
                        printf("static String tm_table = \n\"%s\n\";\n\n",ptr->tm_table);
                }
        }
        else if (class == (WidgetClass)&xmPrimitiveClassRec)
        {
        XmPrimitiveClassPart *ptr = &(((XmPrimitiveClassRec *)base)->primitive_class);

                if (ptr->translations != NULL &&
                    ptr->translations != XtInheritTranslations)
                {
                        printf("static String primitive_translations = \n\"%s\n\";\n\n",ptr->translations);
                }
        }
        else if (class == (WidgetClass)&xmLabelClassRec)
        {
        XmLabelClassPart *ptr = &(((XmLabelClassRec *)base)->label_class);

                if (ptr->translations != NULL &&
                    ptr->translations != XtInheritTranslations)
                {
                        printf("static String label_translations = \n\"%s\n\";\n\n",ptr->translations);
                }
        }
        else if (class == (WidgetClass)&xmManagerClassRec)
        {
        XmManagerClassPart *ptr = &(((XmManagerClassRec *)base)->manager_class);

                if (ptr->translations != NULL &&
                    ptr->translations != XtInheritTranslations)
                {
                        printf("static String manager_translations = \n\"%s\n\";\n\n",ptr->translations);
                }
        }
        else if (class == (WidgetClass)&xmTearOffButtonClassRec)
        {
        XmTearOffButtonClassPart *ptr = &(((XmTearOffButtonClassRec *)base)->tearoffbutton_class);

                if (ptr->translations != NULL &&
                    ptr->translations != XtInheritTranslations)
                {
                        printf("static String tearoffbutton_translations = \n\"%s\n\";\n\n",ptr->translations);
                }
        }
}

static void
DoResources(WidgetClass base, WidgetClass class)
{
int i;

        if (class != (WidgetClass)&rectObjClassRec && class != &coreClassRec && class->core_class.superclass != NULL)
        {
            DoResources(base, class->core_class.superclass);
        }
        if (class == (WidgetClass)&coreClassRec ||
            class == (WidgetClass)&rectObjClassRec ||
            class == (WidgetClass)&objectClassRec)
        {
        CoreClassPart *ptr = &(base->core_class);

            class = base;
            if (ptr->num_resources > 0)
            {
                    printf("#define Offset(field) XtOffsetOf(%sRec, %s.field)\n",
                            class->core_class.class_name, Class2Part(class));
                    printf("static XtResource resources[] =\n{\n");
                    for (i=0; i<ptr->num_resources; i++)
                    {
                            printf("\t{\n\t\tXmN%s, XmC%s, XmR%s\n\t\tsizeof(%s) , offset(%s)\n\t\tXmR%s, (XtPointer)%s\n\t},\n",
                                    ptr->resources[i].resource_name,
                                    ptr->resources[i].resource_class,
                                    ptr->resources[i].resource_type,
                                    ptr->resources[i].resource_type,
                                    ResourceName2Variable(ptr->resources[i].resource_name),
                                    ptr->resources[i].default_type,
                                    Resource2String(&(ptr->resources[i])) );
                    }
                    printf("};\n\n");
                    printf("#undef Offset\n");
            }
        }
        else if (class == (WidgetClass)&constraintClassRec)
        {
        ConstraintClassPart *ptr = &(((ConstraintClassRec *)base)->constraint_class);

            if (ptr->num_resources > 0)
            {
                    printf("#define Offset(field) XtOffsetOf(%sRec, %s.field)\n",
                            class->core_class.class_name, Class2Part(class));
                    printf("static XtResource constraint_resources[] =\n{\n");
                    for (i=0; i<ptr->num_resources; i++)
                    {
                            printf("\t{\n\t\tXmN%s, XmC%s, XmR%s\n\t\tsizeof(%s) , offset(%s)\n\t\tXmR%s, (XtPointer)%s\n\t},\n",
                                    ptr->resources[i].resource_name,
                                    ptr->resources[i].resource_class,
                                    ptr->resources[i].resource_type,
                                    ptr->resources[i].resource_type,
                                    ResourceName2Variable(ptr->resources[i].resource_name),
                                    ptr->resources[i].default_type,
                                    Resource2String(&(ptr->resources[i])) );
                    }
                    printf("};\n\n");
                    printf("#undef Offset\n");
            }
        }
}

static void
WriteStub(String name, String type, Boolean Stub)
{
        printf("static void %s(", name);
        if (strcmp("XtWidgetClassProc", type) == 0)
        {
                printf("WidgetClass widget_class)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, NULL, \"%s %%s\",\n\t\twidget_class->core_class.class_name));\n",name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XmGetSecResDataFunc", type) == 0 ||
                strcmp("XmWidgetNavigableProc", type) == 0 ||
                strcmp("XmFocusChangeProc", type) == 0)
        {
                printf("dunno)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, NULL, \"%s \"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtGeometryHandler", type) == 0)
        {
                printf("Widget w, \n\tXtWidgetGeometry *request, XtWidgetGeometry *reply)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s %%s\",\n\t\tXdbGeometry2String(request)));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtWidgetProc", type) == 0)
        {
                printf("Widget w)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s \"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtRealizeProc", type) == 0)
        {
                printf("Widget w, XtValueMask *value_mask, \n\tXSetWindowAttributes *attributes)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s \"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtActionProc", type) == 0)
        {
                printf("Widget w, XEvent *event, String *params, Cardinal *num_params)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s \"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XmWidgetBaselineProc", type) == 0)
        {
                printf("Widget w, Dimension **baselines, int *num_baselines)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s\"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XmWidgetDisplayRectProc", type) == 0)
        {
                printf("Widget w, XRectangle *rect)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s\"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XmWidgetMarginsProc", type) == 0)
        {
                printf("Widget w, XmBaselineMargins *margins)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s\"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XmMenuProc", type) == 0)
        {
                printf("int function, Widget w, ...)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s\"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtExposeProc", type) == 0)
        {
                printf("Widget w, XEvent *event, Region region)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s\"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtProc", type) == 0)
        {
                printf(")");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, NULL, \"%s\"));\n", name);
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtSetValuesFunc", type) == 0)
        {
                printf("Widget current, Widget request, Widget new_w, \n\tArgList args, Cardinal *num_args)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, NULL, \"%s: %%i args\\n\n"\
                                "\t\t\\tcurrent X %%5i Y %%5i W %%5i H %%5i\\n\n"\
                                "\t\t\\trequest X %%5i Y %%5i W %%5i H %%5i\\n\n"\
                                "\t\t\\t  new_w X %%5i Y %%5i W %%5i H %%5i\\n\",\n"\
                                "\t\t*num_args,\n"\
                                "\t\tXtX(current), XtY(current),\n"\
                                "\t\tXtWidth(current), XtHeight(current),\n"\
                                "\t\tXtX(request), XtY(request),\n"\
                                "\t\tXtWidth(request), XtHeight(request),\n"\
                                "\t\tXtX(new_w), XtY(new_w),\n"\
                                "\t\tXtWidth(new_w), XtHeight(new_w) ));\n", name);
                        printf("\tDEBUGOUT(XdbPrintArgList(__FILE__, new_w, args, *num_args, False));\n");
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtArgsProc", type) == 0)
        {
                printf("Widget w, \n\tArgList args, Cardinal *num_args)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, w, \"%s %%i args\",\n\t\t*num_args));\n", name);
                        printf("\tDEBUGOUT(XdbPrintArgList(__FILE__, new_w, args, *num_args, False));\n");
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else if (strcmp("XtInitProc", type) == 0)
        {
                printf("Widget request, Widget new_w, \n\tArgList args, Cardinal *num_args)");
                if (Stub)
                {
                        printf("\n{\n");
                        printf("\tDEBUGOUT(XdbDebug(__FILE__, NULL, \"%s: %%i args\\n\n"\
                                "\t\t\\trequest X %%5i Y %%5i W %%5i H %%5i\\n\n"\
                                "\t\t\\t  new_w X %%5i Y %%5i W %%5i H %%5i\\n\",\n"\
                                "\t\t*num_args,\n"\
                                "\t\tXtX(request), XtY(request),\n"\
                                "\t\tXtWidth(request), XtHeight(request),\n"\
                                "\t\tXtX(new_w), XtY(new_w),\n"\
                                "\t\tXtWidth(new_w), XtHeight(new_w) ));\n", name);
                        printf("\tDEBUGOUT(XdbPrintArgList(__FILE__, new_w, args, *num_args, False));\n");
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
        else
        {
                printf(" UNKNOWN %s )",type);
                if (Stub)
                {
                        printf("\n{\n");
                        printf("}\n\n");
                }
                else
                {
                        printf(";\n");
                }
        }
}

static void
DoForwardDeclarations(WidgetClass base, WidgetClass class, Boolean Stub)
{
        if (class != (WidgetClass)&rectObjClassRec && class != &coreClassRec && class->core_class.superclass != NULL)
        {
            DoForwardDeclarations(base, class->core_class.superclass, Stub);
        }
        if (class == (WidgetClass)&coreClassRec)
        {
        CoreClassPart *ptr = &(base->core_class);

                printf("/* Core Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                if (ptr->class_initialize != NULL)
                {
                        WriteStub("class_initialize", "XtProc", Stub);
                }
                if (ptr->class_part_initialize != NULL)
                {
                        WriteStub("class_part_initialize", "XtWidgetClassProc", Stub);
                }
                if (ptr->initialize != NULL)
                {
                        WriteStub("initialize", "XtInitProc", Stub);
                }
                if (ptr->initialize_hook != NULL)
                {
                        WriteStub("initialize_hook", "XtArgsProc", Stub);
                }
                if (ptr->realize != XmInheritRealize && ptr->realize != NULL)
                {
                        WriteStub("realize", "XtRealizeProc", Stub);
                }
                if (ptr->destroy != NULL)
                {
                        WriteStub("destroy", "XtWidgetProc", Stub);
                }
                if (ptr->resize != XmInheritResize && ptr->resize != NULL)
                {
                        WriteStub("resize", "XtWidgetProc", Stub);
                }
                if (ptr->expose != XtInheritExpose && ptr->expose == NULL)
                {
                        WriteStub("expose", "XtExposeProc", Stub);
                }
                if (ptr->set_values != NULL)
                {
                        WriteStub("set_values", "XtSetValuesFunc", Stub);
                }
                if (ptr->set_values_hook != NULL)
                {
                        WriteStub("set_values_hook", "XtArgsFunc", Stub);
                }
                if (ptr->set_values_almost != XtInheritSetValuesAlmost && ptr->set_values_almost == NULL)
                {
                        WriteStub("set_values_almost", "XtAlmostProc", Stub);
                }
                if (ptr->get_values_hook != NULL)
                {
                        WriteStub("get_values_hook", "XtArgsProc", Stub);
                }
                if (ptr->accept_focus != XtInheritAcceptFocus && ptr->accept_focus != NULL)
                {
                        WriteStub("accept_focus", "XtAcceptFocusProc", Stub);
                }
                if (ptr->query_geometry != XtInheritQueryGeometry && ptr->query_geometry != NULL)
                {
                        WriteStub("query_geometry", "XtGeometryHandler", Stub);
                }
                if (ptr->display_accelerator != XtInheritDisplayAccelerator && ptr->display_accelerator != NULL)
                {
                        WriteStub("display_accelerator", "XtStringProc", Stub);
                }
                if (ptr->extension != NULL)
                {
                XmBaseClassExt ext = (XmBaseClassExt)(ptr->extension);

                    printf("/* Core Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                    if (ext->initializePrehook != XmInheritInitializePrehook && 
                        ext->initializePrehook != NULL)
                    {
                            WriteStub("initialize_prehook", "XtInitProc", Stub);
                    }
                    if (ext->setValuesPrehook != XmInheritSetValuesPrehook && 
                        ext->setValuesPrehook != NULL)
                    {
                            WriteStub("setValuesPrehook", "XtSetValuesFunc", Stub);
                    }
                    if (ext->initializePosthook != XmInheritInitializePosthook && 
                        ext->initializePosthook != NULL)
                    {
                            WriteStub("initializePosthook", "XtInitProc", Stub);
                    }
                    if (ext->setValuesPosthook != XmInheritSetValuesPosthook && 
                        ext->setValuesPosthook != NULL)
                    {
                            WriteStub("setValuesPosthook", "XtSetValuesFunc", Stub);
                    }
                    if (ext->getSecResData != XmInheritGetSecResData && 
                        ext->getSecResData != NULL)
                    {
                            WriteStub("getSecResData", "XmGetSecResDataFunc", Stub);
                    }
                            WriteStub("getValuesPrehook", "XtArgsProc", Stub);
                            WriteStub("getValuesPosthook", "XtArgsProc", Stub);
                            WriteStub("classPartInitPrethook", "XtWidgetClassProc", Stub);
                            WriteStub("classPartInitPosthook", "XtWidgetClassProc", Stub);
                            WriteStub("widgetNavigable", "XmWidgetNavigableProc", Stub);
                            WriteStub("focusChange", "XmFocusChangeProc", Stub);
                    printf("/* End Core Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                }
                printf("/* End Core Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
        }
        else if (class == (WidgetClass)&xmPrimitiveClassRec)
        {
        XmPrimitiveClassPart *ptr = &(((XmPrimitiveClassRec *)base)->primitive_class);

                printf("/* Primitive Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                    if (ptr->border_highlight != XmInheritBorderHighlight && 
                        ptr->border_highlight != NULL)
                    {
                            WriteStub("border_highlight", "XtWidgetProc", Stub);
                    }
                    if (ptr->border_unhighlight != XmInheritBorderUnhighlight && 
                        ptr->border_unhighlight != NULL)
                    {
                            WriteStub("border_unhighlight", "XtWidgetProc", Stub);
                    }
                    if (ptr->arm_and_activate != XmInheritArmAndActivate && 
                        ptr->arm_and_activate != NULL)
                    {
                            WriteStub("arm_and_activate", "XtActionProc", Stub);
                    }
                if (ptr->extension != NULL)
                {
                XmPrimitiveClassExt ext = (XmPrimitiveClassExt)(ptr->extension);

                    printf("/* Primitive Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                    if (ext->widget_baseline != XmInheritBaselineProc && 
                        ext->widget_baseline != NULL)
                    {
                            WriteStub("widget_baseline", "XmWidgetBaselineProc", Stub);
                    }
                    if (ext->widget_display_rect != XmInheritDisplayRectProc && 
                        ext->widget_display_rect != NULL)
                    {
                            WriteStub("widget_display_rect", "XmWidgetDisplayRectProc", Stub);
                    }
                    if (ext->widget_margins != XmInheritMarginsProc && 
                        ext->widget_margins != NULL)
                    {
                            WriteStub("widget_margins", "XmWidgetMarginsProc", Stub);
                    }
                    printf("/* End Primitive Class Extension%s */\n\n", Stub ? "Methods" : "Forward Declarations");
                }
                printf("/* End Primitive Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
        }
        else if (class == (WidgetClass)&xmLabelClassRec)
        {
        XmLabelClassPart *ptr = &(((XmLabelClassRec *)base)->label_class);

                printf("/* Label Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                    if (ptr->setOverrideCallback != XmInheritSetOverrideCallback && 
                        ptr->setOverrideCallback != NULL)
                    {
                            WriteStub("setOverrideCallback", "XtWidgetProc", Stub);
                    }
                    if (ptr->menuProcs != XmInheritMenuProc && 
                        ptr->menuProcs != NULL)
                    {
                            WriteStub("menuProcs", "XmMenuProc", Stub);
                    }
                if (ptr->extension != NULL)
                {
                        printf("/* Label class extension is not known to me */\n");
                }
                printf("/* End Label Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
        }
        else if (class == (WidgetClass)&xmArrowButtonClassRec)
        {
        XmArrowButtonClassPart *ptr = &(((XmArrowButtonClassRec *)base)->arrowbutton_class);

                if (ptr->extension != NULL)
                {
                        printf("/* ArrowButton class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&xmListClassRec)
        {
        XmListClassPart *ptr = &(((XmListClassRec *)base)->list_class);

                if (ptr->extension != NULL)
                {
                        printf("/* List class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&xmSashClassRec)
        {
        XmSashClassPart *ptr = &(((XmSashClassRec *)base)->sash_class);

                if (ptr->extension != NULL)
                {
                        printf("/* Sash class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&xmScrollBarClassRec)
        {
        XmScrollBarClassPart *ptr = &(((XmScrollBarClassRec *)base)->scrollBar_class);

                if (ptr->extension != NULL)
                {
                        printf("/* ScrollBar class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&xmSeparatorClassRec)
        {
        XmSeparatorClassPart *ptr = &(((XmSeparatorClassRec *)base)->separator_class);

                if (ptr->extension != NULL)
                {
                        printf("/* Separator class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&xmTextFieldClassRec)
        {
#ifdef LESSTIF_VERSION
        XmTextFieldClassPart *ptr = &(((XmTextFieldClassRec *)base)->textfield_class);
#else
        XmTextFieldClassPart *ptr = &(((XmTextFieldClassRec *)base)->text_class);
#endif

                if (ptr->extension != NULL)
                {
                        printf("/* TextField class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&xmTextClassRec)
        {
        XmTextClassPart *ptr = &(((XmTextClassRec *)base)->text_class);

                if (ptr->extension != NULL)
                {
                        printf("/* Text class extension is not known to me */\n");
                }
        }
        else if (class == (WidgetClass)&compositeClassRec)
        {
        CompositeClassPart *ptr = &(((CompositeClassRec *)base)->composite_class);

                printf("/* Composite Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                if (ptr->geometry_manager != XtInheritGeometryManager && 
                    ptr->geometry_manager != NULL)
                {
                        WriteStub("geometry_manager", "XtGeometryHandler", Stub);
                }
                if (ptr->change_managed != XtInheritChangeManaged && 
                    ptr->change_managed != NULL)
                {
                        WriteStub("change_managed", "XtWidgetProc", Stub);
                }
                if (ptr->insert_child != XtInheritInsertChild && 
                    ptr->insert_child != NULL)
                {
                        WriteStub("insert_child", "XtWidgetProc", Stub);
                }
                if (ptr->delete_child != XtInheritDeleteChild && 
                    ptr->delete_child != NULL)
                {
                        WriteStub("insert_child", "XtWidgetProc", Stub);
                }
                if (ptr->extension != NULL)
                {
                CompositeClassExtension ext = (CompositeClassExtension)(ptr->extension);

                        printf("/* Composite Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                        printf("/* End Composite Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                }
                printf("/* End Composite Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
        }
        else if (class == (WidgetClass)&constraintClassRec)
        {
        ConstraintClassPart *ptr = &(((ConstraintClassRec *)base)->constraint_class);

                printf("/* Constraint Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                if (/*ptr->initialize != XtInheritInitialize && */
                    ptr->initialize != NULL)
                {
                        WriteStub("constraint_initialize", "XtInitProc", Stub);
                }
                if (/*ptr->destroy != XtInheritDestroy && */
                    ptr->destroy != NULL)
                {
                        WriteStub("constraint_destroy", "XtWidgetProc", Stub);
                }
                if (/*ptr->destroy != XtInheritDestroy && */
                    ptr->set_values != NULL)
                {
                        WriteStub("constraint_set_values", "XtSetValuesFunc", Stub);
                }
                if (ptr->extension != NULL)
                {
                ConstraintClassExtension ext = (ConstraintClassExtension)(ptr->extension);

                        printf("/* Constraint Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                        if (/*ext->get_values_hook != XtInheritGetValuesHook && */
                            ext->get_values_hook != NULL)
                        {
                                WriteStub("constraint_get_values_hook", "XtArgsProc", Stub);
                        }
                        printf("/* End Constraint Class Extension %s */\n\n", Stub ? "Methods" : "Forward Declarations");
                }
                printf("/* End Constraint Class %s */\n\n", Stub ? "Methods" : "Forward Declarations");
        }
        else if (class == (WidgetClass)&xmCascadeButtonClassRec)
        {
        XmCascadeButtonClassPart *ptr = &(((XmCascadeButtonClassRec *)base)->cascade_button_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmSelectionBoxClassRec)
        {
        XmSelectionBoxClassPart *ptr = &(((XmSelectionBoxClassRec *)base)->selection_box_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmCommandClassRec)
        {
        XmCommandClassPart *ptr = &(((XmCommandClassRec *)base)->command_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmDrawingAreaClassRec)
        {
        XmDrawingAreaClassPart *ptr = &(((XmDrawingAreaClassRec *)base)->drawing_area_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmDrawnButtonClassRec)
        {
        XmDrawnButtonClassPart *ptr = &(((XmDrawnButtonClassRec *)base)->drawnbutton_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmFileSelectionBoxClassRec)
        {
        XmFileSelectionBoxClassPart *ptr = &(((XmFileSelectionBoxClassRec *)base)->file_selection_box_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmFormClassRec)
        {
        XmFormClassPart *ptr = &(((XmFormClassRec *)base)->form_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmFrameClassRec)
        {
        int i;
        XmFrameClassPart *ptr = &(((XmFrameClassRec *)base)->frame_class);

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmScrolledWindowClassRec)
        {
#ifdef LESSTIF_VERSION
        XmScrolledWindowClassPart *ptr = &(((XmScrolledWindowClassRec *)base)->scrolled_window_class);
#else
        XmScrolledWindowClassPart *ptr = &(((XmScrolledWindowClassRec *)base)->swindow_class);
#endif

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmMainWindowClassRec)
        {
#ifdef LESSTIF_VERSION
        XmMainWindowClassPart *ptr = &(((XmMainWindowClassRec *)base)->main_window_class);
#else
        XmMainWindowClassPart *ptr = &(((XmMainWindowClassRec *)base)->mwindow_class);
#endif

                if (ptr->extension != NULL)
                {
                        printf("/* %s unknown class extension */\n\n",base->core_class.class_name);
                }
        }
        else if (class == (WidgetClass)&xmManagerClassRec)
        {
        XmManagerClassPart *ptr = &(((XmManagerClassRec *)base)->manager_class);

                printf("/* %s Class %s */\n\n", base->core_class.class_name, Stub ? "Methods" : "Forward Declarations");
                if (*ptr->parent_process != XmInheritParentProcess &&
                    ptr->parent_process != NULL)
                {
                        WriteStub("parent_process", "XmParentProcessProc", Stub);
                }
                if (ptr->extension != NULL)
                {
                XmManagerClassExt ext = (XmManagerClassExt)(ptr->extension);

                    printf("/* %s Class Extension %s */\n\n", base->core_class.class_name, Stub ? "Methods" : "Forward Declarations");
                    if (*ext->traversal_children != XmInheritTraversalChildrenProc &&
                        ext->traversal_children != NULL)
                    {
                            WriteStub("traversal_children", "XmTraversalChildrenProc", Stub);
                    }
                    printf("/* End %s Class Extension %s */\n\n", base->core_class.class_name, Stub ? "Methods" : "Forward Declarations");
                }
                printf("/* End %s Class %s */\n\n", base->core_class.class_name, Stub ? "Methods" : "Forward Declarations");
        }
        else if (class == (WidgetClass)&xmBulletinBoardClassRec)
        {
        XmBulletinBoardClassPart *ptr = &(((XmBulletinBoardClassRec *)base)->bulletin_board_class);

                printf("/* %s Class %s */\n\n", base->core_class.class_name, Stub ? "Methods" : "Forward Declarations");
                if (*ptr->geo_matrix_create != XmInheritGeoMatrixCreate &&
                    ptr->geo_matrix_create != NULL)
                {
                        WriteStub("geo_matrix_create", "XmGeoCreateProc", Stub);
                }
                if (ptr->extension != NULL)
                {
                    printf("/* %s Unknown Class Extension */\n\n", base->core_class.class_name);
                }
                printf("/* End %s Class %s */\n\n", base->core_class.class_name, Stub ? "Methods" : "Forward Declarations");
        }
        else
        {
                printf("\n\nunknown class %s\n\n",class->core_class.class_name);
        }
}

int
main(int argc, char *argv[])
{
   int i;
   WidgetClass class;
   const WidgetClass *tmp;

   const WidgetClass *class_list[] = {
        &xmPrimitiveWidgetClass,
        &xmArrowButtonWidgetClass,
        &xmCascadeButtonWidgetClass,
        &xmCommandWidgetClass,
        &xmDrawingAreaWidgetClass,
        &xmDrawnButtonWidgetClass,
        &xmFileSelectionBoxWidgetClass,
        &xmFormWidgetClass,
        &xmFrameWidgetClass,
        &xmLabelWidgetClass,
        &xmListWidgetClass,
        &xmMainWindowWidgetClass,
        &xmMessageBoxWidgetClass,
        &xmPanedWindowWidgetClass,
        &xmPushButtonWidgetClass,
        &xmRowColumnWidgetClass,
        &xmSashWidgetClass,
        &xmScaleWidgetClass,
        &xmScrollBarWidgetClass,
        &xmScrolledWindowWidgetClass,
        &xmSelectionBoxWidgetClass,
        &xmSeparatorWidgetClass,
        &xmTextFieldWidgetClass,
        &xmTextWidgetClass,
        &xmToggleButtonWidgetClass,

        &vendorShellWidgetClass,
        &xmArrowButtonGadgetClass,
        &xmBulletinBoardWidgetClass,
        &xmCascadeButtonGadgetClass,
        &xmDesktopClass,         /* this is redundant!!!! */
#ifdef DESKTOP
        &xmDesktopClass,
#else
        &xmDesktopObjectClass,
#endif
        &xmDialogShellExtObjectClass,
        &xmDialogShellWidgetClass,
#ifdef DISPLAY
        &xmDisplayClass,
#else
        &xmDisplayObjectClass,
#endif
        &xmDragContextClass,
        &xmDragIconObjectClass,
        &xmDragOverShellWidgetClass,
        &xmDropSiteManagerObjectClass,
        &xmDropTransferObjectClass,
        &xmExtObjectClass,
        &xmGadgetClass,
        &xmLabelGadgetClass,
        &xmManagerWidgetClass,
        &xmMenuShellWidgetClass,
#ifdef PROTOCOL
        (WidgetClass*) &xmProtocolClassRec,
#else
        &xmProtocolObjectClass,
#endif
        &xmPushButtonGadgetClass,
#ifdef SCREEN
        &xmScreenClass,
#else
        &xmScreenObjectClass,
#endif
        &xmSeparatorGadgetClass,
#ifdef SHELL
        (WidgetClass*) &xmShellExtClassRec,
#else
        &xmShellExtObjectClass,
#endif
        &xmTearOffButtonWidgetClass,
        &xmToggleButtonGadgetClass,
        &xmVendorShellExtObjectClass,
#ifdef WORLD
        &xmWorldClass,
#endif
        /* Motif does not seem to have this
        &xmTextInnerObjectClass,
        */

        NULL
        }; /* class_list */

        printf("%s\n", rcsid);
	i=0;
        tmp = class_list[i];
        while (tmp != NULL)
        {
                class = *tmp;
                printf("********** %s **********\n",class->core_class.class_name);
                /*
                DoForwardDeclarations(class, class, False);
                */
                DoResources(class, class);
                DoSynResources(class, class);
                DoActions(class, class);
                DoExtensions(class, class);
                DoClassRecord(class);
                /*
                DoForwardDeclarations(class, class, True);
                */
		i++;
	        tmp=class_list[i];
        } /* while */

        printf("NumSavedActions %i\n",NumSavedActions);
        for (i=0; i<NumSavedActions; i++)
        {
                printf("%08x %s %s\n", 
                        SavedActions[i].addr,
                        (char *)SavedActions[i].name,
                        SavedActions[i].class);
        }
        exit(0);
}
