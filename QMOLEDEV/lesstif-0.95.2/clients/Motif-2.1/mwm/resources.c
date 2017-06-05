/**
 *
 * $Id: resources.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <Xm/XmP.h>
#include <Xm/MwmUtil.h>
#include <Xm/XmStrDefs.h>

#include "mwm.h"
#include "mwmstrings.h"

/*
 * mwm strings
 */
const char mwm_strings[] =
"autoKeyFocus\0"
"AutoKeyFocus\0"
"autoRaiseDelay\0"
"AutoRaiseDelay\0"
"bitmapDirectory\0"
"BitmapDirectory\0"
"buttonBindings\0"
"ButtonBindings\0"
"cleanText\0"
"CleanText\0"
"clientAutoPlace\0"
"ClientAutoPlace\0"
"colormapFocusPolicy\0"
"ColormapFocusPolicy\0"
"configFile\0"
"ConfigFile\0"
"deiconifyKeyFocus\0"
"DeiconifyKeyFocus\0"
"doubleClickTime\0"
"DoubleClickTime\0"
"enableWarp\0"
"EnableWarp\0"
"enforceKeyFocus\0"
"EnforceKeyFocus\0"
"fadeNormalIcon\0"
"FadeNormalIcon\0"
"feedbackGeometry\0"
"FeedbackGeometry\0"
"freezeOnConfig\0"
"FreezeOnConfig\0"
"frameBorderWidth\0"
"FrameBorderWidth\0"
"iconAutoPlace\0"
"IconAutoPlace\0"
"iconBoxGeometry\0"
"IconBoxGeometry\0"
"iconBoxName\0"
"IconBoxName\0"
"iconBoxSBDisplayPolicy\0"
"IconBoxSBDisplayPolicy\0"
"iconBoxScheme\0"
"IconBoxScheme\0"
"iconBoxTitle\0"
"IconBoxTitle\0"
"iconClick\0"
"IconClick\0"
"iconDecoration\0"
"IconDecoration\0"
"iconImageMaximum\0"
"IconImageMaximum\0"
"iconImageMinimum\0"
"IconImageMinimum\0"
"iconPlacement\0"
"IconPlacement\0"
"iconPlacementMargin\0"
"IconPlacementMargin\0"
"interactivePlacement\0"
"InteractivePlacement\0"
"keyBindings\0"
"KeyBindings\0"
"limitResize\0"
"LimitResize\0"
"lowerOnIconify\0"
"LowerOnIconify\0"
"maximumMaximumSize\0"
"MaximumMaximumSize\0"
"moveThreshold\0"
"MoveThreshold\0"
"multiScreen\0"
"MultiScreen\0"
"passButtons\0"
"PassButtons\0"
"passSelectButton\0"
"PassSelectButton\0"
"positionIsFrame\0"
"PositionIsFrame\0"
"positionOnScreen\0"
"PositionOnScreen\0"
"quitTimeout\0"
"QuitTimeout\0"
"raiseKeyFocus\0"
"RaiseKeyFocus\0"
"resizeBorderWidth\0"
"FrameBorderWidth\0"
"resizeCursors\0"
"ResizeCursors\0"
"screens\0"
"Screens\0"
"showFeedback\0"
"ShowFeedback\0"
"startupKeyFocus\0"
"StartupKeyFocus\0"
"transientDecoration\0"
"TransientDecoration\0"
"transientFunctions\0"
"TransientFunctions\0"
"useIconBox\0"
"UseIconBox\0"
"wMenuButtonClick\0"
"WMenuButtonClick\0"
"wMenuButtonClick2\0"
"WMenuButtonClick2\0"
"clientDecoration\0"
"ClientDecoration\0"
"clientFunctions\0"
"ClientFunctions\0"
"focusAutoRaise\0"
"FocusAutoRaise\0"
"iconImage\0"
"IconImage\0"
"iconImageBackground\0"
"IconImageBackground\0"
"iconImageBottomShadowColor\0"
"IconImageBottomShadowColor\0"
"iconImageBottomShadowPixmap\0"
"IconImageBottomShadowPixmap\0"
"iconImageForeground\0"
"IconImageForeground\0"
"iconImageTopShadowColor\0"
"IconImageTopShadowColor\0"
"iconImageTopShadowPixmap\0"
"IconImageTopShadowPixmap\0"
"matteBackground\0"
"MatteBackground\0"
"matteBottomShadowColor\0"
"MatteBottomShadowColor\0"
"matteBottomShadowPixmap\0"
"MatteBottomShadowPixmap\0"
"matteForeground\0"
"MatteForeground\0"
"matteTopShadowColor\0"
"MatteTopShadowColor\0"
"matteTopShadowPixmap\0"
"MatteTopShadowPixmap\0"
"matteWidth\0"
"MatteWidth\0"
"maximumClientSize\0"
"MaximumClientSize\0"
"useClientIcon\0"
"UseClientIcon\0"
"usePPosition\0"
"UsePPosition\0"
"windowMenu\0"
"WindowMenu\0"
"activeBackground\0"
"activeForeground\0"
"activeBottomShadowColor\0"
"activeBottomShadowPixmap\0"
"activeTopShadowColor\0"
"activeBackgroundPixmap\0"
"activeTopShadowPixmap\0"
"WmCFocus\0"
"WmCDecor\0"
"WmCFunc\0"
"WmIDecor\0"
"WmIPlace\0"
"WmKFocus\0"
"WmSize\0"
"WmShowFeedback\0"
"WmUsePPosition\0"
"clientFlags\0"
"ClientFlags\0"
"WmCFlags\0"
"usePager\0"
"UsePager\0"
"edgeScrollX\0"
"EdgeScrollX\0"
"edgeScrollY\0"
"EdgeScrollY\0"
"pagerX\0"
"PagerX\0"
"pagerY\0"
"PagerY\0"
"virtualX\0"
"VirtualX\0"
"virtualY\0"
"VirtualY\0"
"clickTime\0"
"ClickTime\0"
"smartPlacement\0"
"SmartPlacement\0"
"\0";

/*
 * Syntax:
 * "Mwm*resource_name: resource_value"
 */
#undef Offset
#define Offset(field)	XtOffsetOf(MwmInternalInfo, field)
static XtResource mwm_resources[] =
{
    {
	XmNautoKeyFocus, XmCAutoKeyFocus, XmRBoolean,
	sizeof(Boolean), Offset(auto_key_focus),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNautoRaiseDelay, XmCAutoRaiseDelay, XmRInt,
	sizeof(Time), Offset(auto_raise_delay),
	XmRImmediate, (XtPointer)500
    },
    {
	XmNbitmapDirectory, XmCBitmapDirectory, XmRString,
	sizeof(String), Offset(bitmap_directory),
	XmRString, (XtPointer)MWM_ICONDIR
    },
    {
	XmNclientAutoPlace, XmCClientAutoPlace, XmRBoolean,
	sizeof(Boolean), Offset(client_auto_place),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNcolormapFocusPolicy, XmCColormapFocusPolicy, XmRWmCFocus,
	sizeof(unsigned char), Offset(colormap_focus_policy),
	XmRString, (XtPointer)"keyboard"
    },
    {
	XmNconfigFile, XmCConfigFile, XmRString,
	sizeof(String), Offset(config_file),
	XmRImmediate, (XtPointer)HOME_MWMRC
    },
    {
	XmNdeiconifyKeyFocus, XmCDeiconifyKeyFocus, XmRBoolean,
	sizeof(Boolean), Offset(deiconify_key_focus),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNclickTime, XmCClickTime, XmRInt,
	sizeof(Time), Offset(click_time),
	XmRImmediate, (XtPointer)100
    },
    {
	XmNdoubleClickTime, XmCDoubleClickTime, XmRInt,
	sizeof(Time), Offset(double_click_time),
	XmRCallProc, (XtPointer)_WmMultiClickTimeDefault
    },
    {
	XmNenableWarp, XmCEnableWarp, XmRBoolean,
	sizeof(Boolean), Offset(enable_warp),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNenforceKeyFocus, XmCEnforceKeyFocus, XmRBoolean,
	sizeof(Boolean), Offset(enforce_key_focus),
	XmRImmediate, (XtPointer)True
    },
#if 0
    {
	XmNfreezeOnConfig, XmCFreezeOnConfig, XmRBoolean,
    },
#endif
    {
	XmNiconAutoPlace, XmCIconAutoPlace, XmRBoolean,
	sizeof(Boolean), Offset(icon_auto_place),
	XmRImmediate, (XtPointer)True
    },
#if 0
    {
	XmNiconBoxScheme, XmCIconBoxScheme,
    },
#endif
    {
	XmNiconClick, XmCIconClick, XmRBoolean,
	sizeof(Boolean), Offset(icon_click),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNinteractivePlacement, XmCInteractivePlacement, XmRBoolean,
	sizeof(Boolean), Offset(interactive_placement),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNkeyboardFocusPolicy, XmCKeyboardFocusPolicy, XmRWmKFocus,
	sizeof(unsigned char), Offset(keyboard_focus_policy),
	XmRString, (XtPointer)"explicit"
    },
    {
	XmNlowerOnIconify, XmCLowerOnIconify, XmRBoolean,
	sizeof(Boolean), Offset(lower_on_iconify),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNmoveThreshold, XmCMoveThreshold, XmRDimension,
	sizeof(Dimension), Offset(move_threshold),
	XmRImmediate, (XtPointer)4
    },
    {
	XmNmultiScreen, XmCMultiScreen, XmRBoolean,
	sizeof(Boolean), Offset(multi_screen),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpassButtons, XmCPassButtons, XmRBoolean,
	sizeof(Boolean), Offset(pass_buttons),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpassSelectButton, XmCPassSelectButton, XmRBoolean,
	sizeof(Boolean), Offset(pass_selection_buttons),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpositionIsFrame, XmCPositionIsFrame, XmRBoolean,
	sizeof(Boolean), Offset(position_is_frame),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNpositionOnScreen, XmCPositionOnScreen, XmRBoolean,
	sizeof(Boolean), Offset(position_on_screen),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNquitTimeout, XmCQuitTimeout, XmRInt,
	sizeof(Time), Offset(quit_timeout),
	XmRImmediate, (XtPointer)1000
    },
    {
	XmNraiseKeyFocus, XmCRaiseKeyFocus, XmRBoolean,
	sizeof(Boolean), Offset(raise_key_focus),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNscreens, XmCScreens, XmRString,
	sizeof(String), Offset(screens),
	XmRImmediate, (XtPointer)":0.0"
    },
    {
	XmNshowFeedback, XmCShowFeedback, XmRWmShowFeedback,
	sizeof(long), Offset(show_feedback),
	XmRString, (XtPointer)"all"
    },
    {
	XmNstartupKeyFocus, XmCStartupKeyFocus, XmRBoolean,
	sizeof(Boolean), Offset(startup_key_focus),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNusePager, XmCUsePager, XmRBoolean,
	sizeof(Boolean), Offset(use_pager),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNsmartPlacement, XmCSmartPlacement, XmRBoolean,
	sizeof(Boolean), Offset(smart_placement),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNedgeScrollX, XmCEdgeScrollX, XmRInt,
	sizeof(int), Offset(edge_scroll_x),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNedgeScrollY, XmCEdgeScrollY, XmRInt,
	sizeof(int), Offset(edge_scroll_y),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNpagerX, XmCPagerX, XmRInt,
	sizeof(int), Offset(pager_x),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNpagerY, XmCPagerY, XmRInt,
	sizeof(int), Offset(pager_y),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNvirtualX, XmCVirtualX, XmRInt,
	sizeof(int), Offset(virtual_x),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNvirtualY, XmCVirtualY, XmRInt,
	sizeof(int), Offset(virtual_y),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNwMenuButtonClick, XmCWMenuButtonClick, XmRBoolean,
	sizeof(Boolean), Offset(w_menu_button_click),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNwMenuButtonClick2, XmCWMenuButtonClick2, XmRBoolean,
	sizeof(Boolean), Offset(w_menu_button_click_2),
	XmRImmediate, (XtPointer)True
    }
};

/*
 * Syntax:
 * "Mwm*screen_name*resource_name: resource_value"
 */
#undef Offset
#define Offset(field)   XtOffsetOf(ScreenInfo, field)
static XtResource mwm_screen_resources[] =
{
    {
	XmNbuttonBindings, XmCButtonBindings, XmRString,
	sizeof(String), Offset(button_bindings),
	XmRString, (XtPointer)DEFAULT_BUTTON_BINDING_NAME
    },
    {
	XmNcleanText, XmCCleanText, XmRBoolean,
	sizeof(Boolean), Offset(clean_text),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNfadeNormalIcon, XmCFadeNormalIcon, XmRBoolean,
	sizeof(Boolean), Offset(fade_normal_icon),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNfeedbackGeometry, XmCFeedbackGeometry, XmRGeometry,
	sizeof(Geometry), Offset(feedback_geometry),
	XmRCallProc, (XtPointer)NULL
    },
    {
	XmNframeBorderWidth, XmCFrameBorderWidth, XmRDimension,
	sizeof(Dimension), Offset(frame_border_width),
	XmRCallProc, (XtPointer)_WmDefaultBorderWidth
    },
    {
	XmNiconBoxGeometry, XmCIconBoxGeometry, XmRGeometry,
	sizeof(Geometry), Offset(icon_box_geometry),
	XmRCallProc, (XtPointer)NULL
    },
    {
	XmNiconBoxName, XmCIconBoxName, XmRString,
	sizeof(String), Offset(icon_box_name),
	XmRImmediate, (XtPointer)"iconbox"
    },
    {
	XmNiconBoxSBDisplayPolicy, XmCIconBoxSBDisplayPolicy, XmRString,
	sizeof(String), Offset(icon_box_sb_display_policy),
	XmRImmediate, (XtPointer)"all"
    },
    {
	XmNiconBoxTitle, XmCIconBoxTitle, XmRXmString,
	sizeof(XmString), Offset(icon_box_title),
	XmRString, (XtPointer)"Icons"
    },
    {
	XmNiconDecoration, XmCIconDecoration, XmRWmIDecor,
	sizeof(long), Offset(icon_decoration),
	XmRString, (XtPointer)"activelabel label image"
    },
    {
	XmNiconImageMaximum, XmCIconImageMaximum, XmRWmSize,
	sizeof(Size), Offset(icon_image_maximum),
	XmRString, (XtPointer)"50x50"
    },
    {
	XmNiconImageMinimum, XmCIconImageMinimum, XmRWmSize,
	sizeof(Size), Offset(icon_image_minimum),
	XmRString, (XtPointer)"16x16"
    },
    {
	XmNiconPlacement, XmCIconPlacement, XmRWmIPlace,
	sizeof(unsigned char), Offset(icon_placement),
	XmRString, (XtPointer)"left bottom"
    },
    {
	XmNiconPlacementMargin, XmCIconPlacementMargin, XmRDimension,
	sizeof(Dimension), Offset(icon_placement_margin),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNkeyBindings, XmCKeyBindings, XmRString,
	sizeof(String), Offset(key_bindings),
	XmRImmediate, (XtPointer)DEFAULT_KEY_BINDING_NAME
    },
    {
	XmNlimitResize, XmCLimitResize, XmRBoolean,
	sizeof(Boolean), Offset(limit_resize),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNmaximumMaximumSize, XmCMaximumMaximumSize, XmRWmSize,
	sizeof(String), Offset(maximum_maximum_size),
	XmRCallProc, (XtPointer)NULL
    },
    {
	XmNmoveOpaque, XmCMoveOpaque, XmRBoolean,
	sizeof(Boolean), Offset(move_opaque),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNresizeBorderWidth, XmCFrameBorderWidth, XmRDimension,
	sizeof(Dimension), Offset(resize_border_width),
	XmRCallProc, (XtPointer)_WmDefaultResizeBorderWidth
    },
    {
	XmNresizeCursors, XmCResizeCursors, XmRBoolean,
	sizeof(Boolean), Offset(resize_cursors),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNtransientDecoration, XmCTransientDecoration, XmRWmCDecor,
	sizeof(long), Offset(transient_decoration),
	XmRString, (XtPointer)"menu title"
    },
    {
	XmNtransientFunctions, XmCTransientFunctions, XmRWmCFunc,
	sizeof(long), Offset(transient_functions),
	XmRString, (XtPointer)"-minimize maximize"
    },
    {
	XmNuseIconBox, XmCUseIconBox, XmRBoolean,
	sizeof(Boolean), Offset(use_icon_box),
	XmRImmediate, (XtPointer)False
    }
};

/*
 * Syntax:
 * "Mwm*client_name*resource_name: resource_value"
 */
#undef Offset
#define Offset(field)	XtOffsetOf(MwmWindow, field)
static XtResource mwm_client_resources[] =
{
    {
	XmNclientDecoration, XmCClientDecoration, XmRWmCDecor,
	sizeof(long), Offset(client_decoration),
	XmRString, (XtPointer)"all"
    },
    {
	XmNclientFunctions, XmCClientFunctions, XmRWmCFunc,
	sizeof(long), Offset(client_functions),
	XmRString, (XtPointer)"all"
    },
    {
	XmNclientFlags, XmCClientFlags, XmRWmCFlags,
	sizeof(long), Offset(flags),
	XmRString, (XtPointer)"none"
    },
    {
	XmNfocusAutoRaise, XmCFocusAutoRaise, XmRBoolean,
	sizeof(Boolean), Offset(focus_auto_raise),
	XmRCallProc, (XtPointer)_WmFocusAutoRaiseDefault
    },
    {
	XmNiconImage, XmCIconImage, XmRString,
	sizeof(String), Offset(icon_image),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNiconImageBackground, XmCIconImageBackground, XmRPixel,
	sizeof(Pixel), Offset(icon_image_background),
	XmRCallProc, (XtPointer)_WmIconImageBDefault
    },
    {
	XmNiconImageBottomShadowColor, XmCIconImageBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(icon_image_bottom_shadow_color),
	XmRCallProc, (XtPointer)_WmIconImageBSCDefault
    },
    {
     XmNiconImageBottomShadowPixmap, XmCIconImageBottomShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(icon_image_bottom_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmIconImageBSPDefault
    },
    {
	XmNiconImageForeground, XmCIconImageForeground, XmRPixel,
	sizeof(Pixel), Offset(icon_image_foreground),
	XmRCallProc, (XtPointer)_WmIconImageFDefault
    },
    {
	XmNiconImageTopShadowColor, XmCIconImageTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(icon_image_top_shadow_color),
	XmRCallProc, (XtPointer)_WmIconImageTSCDefault
    },
    {
	XmNiconImageTopShadowPixmap, XmCIconImageTopShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(icon_image_top_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmIconImageTSPDefault
    },
    {
	XmNmatteBackground, XmCMatteBackground, XmRPixel,
	sizeof(Pixel), Offset(matte_background),
	XmRCallProc, (XtPointer)_WmMatteBDefault
    },
    {
	XmNmatteBottomShadowColor, XmCMatteBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(matte_bottom_shadow_color),
	XmRCallProc, (XtPointer)_WmMatteBSCDefault
    },
    {
	XmNmatteBottomShadowPixmap, XmCMatteBottomShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(matte_bottom_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmMatteBSPDefault
    },
    {
	XmNmatteForeground, XmCMatteForeground, XmRPixel,
	sizeof(Pixel), Offset(matte_foreground),
	XmRCallProc, (XtPointer)_WmMatteFDefault
    },
    {
	XmNmatteTopShadowColor, XmCMatteTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(matte_top_shadow_color),
	XmRCallProc, (XtPointer)_WmMatteTSCDefault
    },
    {
	XmNmatteTopShadowPixmap, XmCMatteTopShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(matte_top_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmMatteTSPDefault
    },
    {
	XmNmatteWidth, XmCMatteWidth, XmRDimension,
	sizeof(Dimension), Offset(matte_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmaximumClientSize, XmCMaximumClientSize, XmRWmSize,
	sizeof(Size), Offset(maximum_client_size),
	XmRCallProc, (XtPointer)NULL
    },
    {
	XmNuseClientIcon, XmCUseClientIcon, XmRBoolean,
	sizeof(Boolean), Offset(use_client_icon),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNusePPosition, XmCUsePPosition, XmRWmUsePPosition,
	sizeof(unsigned char), Offset(use_p_position),
	XmRString, (XtPointer)"nonzero"
    },
    {
	XmNwindowMenu, XmCWindowMenu, XmRString,
	sizeof(String), Offset(window_menu),
	XmRImmediate, (XtPointer)DEFAULT_WIN_MENU_NAME
    }
};

/*
 * Syntax:
 * "Mwm*[component_name]*resource_name: resource_value"
 *
 * BTW, just in case some copyrighters come along:  I got the function names
 * by runnings "strings" on mwm on Solaris...
 */
#undef Offset
#define Offset(field)	XtOffsetOf(ComponentInfo, field)
static XtResource mwm_component_resources[] =
{
    {
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), Offset(background),
	XmRCallProc, (XtPointer)_WmBackgroundDefault
    },
    {
	XmNforeground, XmCForeground, XmRPixel,
	sizeof(XFontStruct *), Offset(foreground),
	XmRCallProc, (XtPointer)_WmForegroundDefault
    },
    {
	XmNbackgroundPixmap, XmCBackgroundPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(background_pixmap),
	XmRCallProc, (XtPointer)_WmBackgroundPixmapDefault
    },
    {
	XmNbottomShadowColor, XmCBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(bottom_shadow_color),
	XmRCallProc, (XtPointer)_WmBottomShadowColorDefault
    },
    {
	XmNbottomShadowPixmap, XmCBottomShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(bottom_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmBottomShadowPixmapDefault
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font_list),
	XmRString, (XtPointer)"fixed"
    },
    {
	XmNsaveUnder, XmCSaveUnder, XmRBoolean,
	sizeof(Boolean), Offset(save_under),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNtopShadowColor, XmCTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(top_shadow_color),
	XmRCallProc, (XtPointer)_WmTopShadowColorDefault
    },
    {
	XmNtopShadowPixmap, XmCTopShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(top_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmTopShadowPixmapDefault
    }
};

/*
 * frame/icon resources
 */
#undef Offset
#define Offset(field)	XtOffsetOf(ComponentInfo, field)
static XtResource mwm_component_fi_resources[] =
{
    {
	XmNactiveBackground, XmCBackground, XmRPixel,
	sizeof(Pixel), Offset(active_background),
	XmRCallProc, (XtPointer)_WmABackgroundDefault
    },
    {
	XmNactiveForeground, XmCBackground, XmRPixel,
	sizeof(Pixel), Offset(active_foreground),
	XmRCallProc, (XtPointer)_WmAForegroundDefault
    },
    {
	XmNactiveBottomShadowColor, XmCBackground, XmRPixel,
	sizeof(Pixel), Offset(active_bottom_shadow_color),
	XmRCallProc, (XtPointer)_WmABottomShadowColorDefault
    },
    {
	XmNactiveBottomShadowPixmap, XmCBottomShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(active_bottom_shadow_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNactiveTopShadowColor, XmCBackground, XmRPixel,
	sizeof(Pixel), Offset(active_top_shadow_color),
	XmRCallProc, (XtPointer)_WmATopShadowColorDefault
    },
    {
	XmNactiveBackgroundPixmap, XmCBackgroundPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(active_background_pixmap),
	XmRCallProc, (XtPointer)_WmABackgroundPixmapDefault
    },
    {
	XmNactiveTopShadowPixmap, XmCTopShadowPixmap, XmRPixmap,
	sizeof(Pixmap), Offset(active_top_shadow_pixmap),
	XmRCallProc, (XtPointer)_WmATopShadowPixmapDefault
    }
};

static char *
  colormap_focus_policies[] =
{
    "explicit",
    "pointer",
    "keyboard",
    NULL
};

static char *
  client_decorations[] =
{
    "all",
    "border",
    "resize",
    "title",
    "menu",
    "minimize",
    "maximize",
    NULL
};

static char *
  client_functions[] =
{
    "all",
    "resize",
    "move",
    "minimize",
    "maximize",
    "close",
    NULL
};

static char *
  icon_decorations[] =
{
    "activelabel",
    "image",
    "label",
    NULL
};

static char *
  icon_placements[] =
{
    "top",
    "bottom",
    "left",
    "right",
    "tight",
    NULL
};

static char *
  keyboard_focus_policies[] =
{
    "explicit",
    "pointer",
    NULL
};

static char *
  show_feedback[] =
{
    "all",
    "behavior",
    "kill",
    "move",
    "placement",
    "quit",
    "resize",
    "restart",
    NULL
};

static char *
  use_p_positions[] =
{
    "on",
    "off",
    "nonzero",
    NULL
};

static char *
  client_flags[] =
{
    "sticky",
    "circulate_skip",
    "start_iconic",
    "list_skip",
    NULL
};

static Boolean
WmCvtStringToCDecor(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		    XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static long val;
    char *ptr;
    int i;
    Boolean reverse = False;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToCDecor",
		     "XmToolkitError", "String to Client Decorations"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    if (strcmp(ptr, "all") == 0)
    {
	val = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH | MWM_DECOR_TITLE |
	    MWM_DECOR_MENU | MWM_DECOR_MAXIMIZE | MWM_DECOR_MINIMIZE;

	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmCDecor);
	    return False;
	}
    }

    if (strcmp(ptr, "none") == 0)
    {
	val = 0;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmCDecor);
	    return False;
	}
    }

    if (*ptr == '-')
    {
	val = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH | MWM_DECOR_TITLE |
	    MWM_DECOR_MENU | MWM_DECOR_MAXIMIZE | MWM_DECOR_MINIMIZE;
	reverse = True;
    }
    else
	val = 0;

    for (i = 0; client_decorations[i] != NULL; i++)
    {
	if (strstr(ptr, client_decorations[i]) != NULL)
	{
	    if (reverse)
		val &= ~(1L << i);
	    else
		val |= (1L << i);
	}
    }
    if (to->addr == NULL)
    {
	to->addr = (XPointer)&val;
	to->size = sizeof(long);
	return True;
    }
    else if (to->size >= sizeof(long))
    {
	*((long *)to->addr) = val;
	to->size = sizeof(long);
	return True;
    }
    else
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					 XmRWmCDecor);
	return False;
    }
}

static Boolean
WmCvtStringToCFocus(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		    XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static unsigned char val;
    char *ptr;
    int i;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToCFocus",
		     "XmToolkitError", "String to Colormap Focus Policy"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    for (i = 0; colormap_focus_policies[i] != NULL; i++)
    {
	if (strcmp(ptr, colormap_focus_policies[i]) == 0)
	{
	    val = i;
	    if (to->addr == NULL)
	    {
		to->addr = (XPointer)&val;
		to->size = sizeof(unsigned char);
		return True;
	    }
	    else if (to->size >= sizeof(unsigned char))
	    {
		*((unsigned char *)to->addr) = val;
		to->size = sizeof(unsigned char);
		return True;
	    }
	    else
	    {
		XtDisplayStringConversionWarning(dpy, (char *)from->addr,
						 XmRWmCFocus);
		return False;
	    }
	}
    }
    XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRWmCFocus);
    return False;
}

static Boolean
WmCvtStringToCFunc(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		   XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static long val;
    char *ptr;
    int i;
    Boolean reverse = False;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToCFunc",
		     "XmToolkitError", "String to Client Functions"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    if (strcmp(ptr, "all") == 0)
    {
	val = MWM_FUNC_RESIZE | MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE |
	    MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmCFunc);
	    return False;
	}
    }

    if (strcmp(ptr, "none") == 0)
    {
	val = 0;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmCFunc);
	    return False;
	}
    }

    if (*ptr == '-')
    {
	val = MWM_FUNC_RESIZE | MWM_FUNC_MOVE | MWM_FUNC_MINIMIZE |
	    MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE;
	reverse = True;
    }
    else
	val = 0;

    for (i = 0; client_functions[i] != NULL; i++)
    {
	if (strstr(ptr, client_functions[i]) != NULL)
	{
	    if (reverse)
		val &= ~(1L << i);
	    else
		val |= (1L << i);
	}
    }
    if (to->addr == NULL)
    {
	to->addr = (XPointer)&val;
	to->size = sizeof(long);
	return True;
    }
    else if (to->size >= sizeof(long))
    {
	*((long *)to->addr) = val;
	to->size = sizeof(long);
	return True;
    }
    else
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					 XmRWmCFunc);
	return False;
    }
}

static Boolean
WmCvtStringToCFlags(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		    XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static long val;
    char *ptr;
    int i;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToCFlags",
		     "XmToolkitError", "String to Client Flags"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    if (strcmp(ptr, "all") == 0)
    {
	val = STICKY | CIRCULATESKIP | STARTICONIC | WINDOWLISTSKIP;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmCFlags);
	    return False;
	}
    }

    if (strcmp(ptr, "none") == 0)
    {
	val = 0;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmCFlags);
	    return False;
	}
    }

    for (i = 0; client_flags[i] != NULL; i++)
    {
	if (strstr(ptr, client_flags[i]) != NULL)
	{
	    val |= (1L << i);
	}
    }
    if (to->addr == NULL)
    {
	to->addr = (XPointer)&val;
	to->size = sizeof(long);
	return True;
    }
    else if (to->size >= sizeof(long))
    {
	*((long *)to->addr) = val;
	to->size = sizeof(long);
	return True;
    }
    else
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					 XmRWmCFlags);
	return False;
    }
}

static Boolean
WmCvtStringToIDecor(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		    XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static long val;
    char *ptr;
    int i;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToIDecor",
		     "XmToolkitError", "String to Icon Decorations"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    val = 0;
    ptr = ((char *)(from->addr));
    for (i = 0; icon_decorations[i] != NULL; i++)
    {
	if (strstr(ptr, icon_decorations[i]) != 0)
	{
	    val |= (1L << i);
	}
    }
    if (val == 0)
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRWmIPlace);
	return False;
    }
    if (to->addr == NULL)
    {
	to->addr = (XPointer)&val;
	to->size = sizeof(long);
	return True;
    }
    else if (to->size >= sizeof(long))
    {
	*((long *)to->addr) = val;
	to->size = sizeof(long);
	return True;
    }
    else
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					 XmRWmCFlags);
	return False;
    }
}

static Boolean
WmCvtStringToIPlace(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		    XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static unsigned char val;
    char *ptr;
    int i;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToIPlace",
		     "XmToolkitError", "String to Icon Placement"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    val = 0;
    for (i = 0; icon_placements[i] != NULL; i++)
    {
	if (strstr(ptr, icon_placements[i]) != NULL)
	{
	    val |= (1L << i);
	}
    }
    if (val == 0)
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRWmIPlace);
	return False;
    }
    if (to->addr == NULL)
    {
	to->addr = (XPointer)&val;
	to->size = sizeof(unsigned char);
	return True;
    }
    else if (to->size >= sizeof(unsigned char))
    {
	*((long *)to->addr) = val;
	to->size = sizeof(unsigned char);
	return True;
    }
    else
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					 XmRWmIPlace);
	return False;
    }
}

static Boolean
WmCvtStringToKFocus(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		    XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static unsigned char val;
    char *ptr;
    int i;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToKFocus",
		     "XmToolkitError", "String to Keyboard Focus Policy"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    for (i = 0; keyboard_focus_policies[i] != NULL; i++)
    {
	if (strcmp(ptr, keyboard_focus_policies[i]) == 0)
	{
	    val = i;
	    if (to->addr == NULL)
	    {
		to->addr = (XPointer)&val;
		to->size = sizeof(unsigned char);
		return True;
	    }
	    else if (to->size >= sizeof(unsigned char))
	    {
		*((unsigned char *)to->addr) = val;
		to->size = sizeof(unsigned char);
		return True;
	    }
	    else
	    {
		XtDisplayStringConversionWarning(dpy, (char *)from->addr,
						 XmRWmKFocus);
		return False;
	    }
	}
    }
    XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRWmKFocus);
    return False;
}

static Boolean
WmCvtStringToShowFeedback(Display *dpy, XrmValuePtr args, Cardinal *num_args,
			  XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static long val;
    char *ptr;
    int i;
    Boolean reverse = False;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToShowFeedback",
		     "XmToolkitError", "String to Show Feedback"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    if (strcmp(ptr, "all") == 0)
    {
	val = MWM_FEEDBACK_BEHAVIOR | MWM_FEEDBACK_KILL | MWM_FEEDBACK_MOVE |
	    MWM_FEEDBACK_PLACEMENT | MWM_FEEDBACK_QUIT | MWM_FEEDBACK_RESIZE |
	    MWM_FEEDBACK_RESTART;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmShowFeedback);
	    return False;
	}
    }

    if (strcmp(ptr, "none") == 0)
    {
	val = 0;
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(long);
	    return True;
	}
	else if (to->size >= sizeof(long))
	{
	    *((long *)to->addr) = val;
	    to->size = sizeof(long);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmShowFeedback);
	    return False;
	}
    }

    if (*ptr == '-')
    {
	val = MWM_FEEDBACK_BEHAVIOR | MWM_FEEDBACK_KILL | MWM_FEEDBACK_MOVE |
	    MWM_FEEDBACK_PLACEMENT | MWM_FEEDBACK_QUIT | MWM_FEEDBACK_RESIZE |
	    MWM_FEEDBACK_RESTART;
	reverse = True;
    }
    else
	val = 0;

    for (i = 0; show_feedback[i] != NULL; i++)
    {
	if (strstr(ptr, show_feedback[i]) != NULL)
	{
	    if (reverse)
		val &= ~(1L << i);
	    else
		val |= (1L << i);
	}
    }
    if (to->addr == NULL)
    {
	to->addr = (XPointer)&val;
	to->size = sizeof(long);
	return True;
    }
    else if (to->size >= sizeof(long))
    {
	*((long *)to->addr) = val;
	to->size = sizeof(long);
	return True;
    }
    else
    {
	XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					 XmRWmShowFeedback);
	return False;
    }
}

static Boolean
WmCvtStringToSize(Display *dpy, XrmValuePtr args, Cardinal *num_args,
		  XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static Size val;
    char *ptr;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToSize",
		     "XmToolkitError", "String to Size"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    memset((void *)&val, 0, sizeof(Size));
    if (sscanf(ptr, "%hdx%hd", &val.width, &val.height) == 2)
    {
	if (to->addr == NULL)
	{
	    to->addr = (XPointer)&val;
	    to->size = sizeof(Size);
	    return True;
	}
	else if (to->size >= sizeof(Size))
	{
	    *((Size *)to->addr) = val;
	    to->size = sizeof(Size);
	    return True;
	}
	else
	{
	    XtDisplayStringConversionWarning(dpy, (char *)from->addr,
					     XmRWmSize);
	    return False;
	}
    }
    XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRWmSize);
    return False;
}

static Boolean
WmCvtStringToUsePPosition(Display *dpy, XrmValuePtr args, Cardinal *num_args,
			  XrmValuePtr from, XrmValuePtr to, XtPointer *data)
{
    static unsigned char val;
    char *ptr;
    int i;

    if (*num_args != 0)
    {
	XtWarningMsg("wrongParameters", "cvtStringToUsePPosition",
		     "XmToolkitError", "String to Use PPosition"
		     " conversion needs no extra arguments",
		     (String *)NULL, (Cardinal *)NULL);
    }

    ptr = ((char *)(from->addr));

    for (i = 0; use_p_positions[i] != NULL; i++)
    {
	if (strcmp(ptr, use_p_positions[i]) == 0)
	{
	    val = i;
	    if (to->addr == NULL)
	    {
		to->addr = (XPointer)&val;
		to->size = sizeof(unsigned char);
		return True;
	    }
	    else if (to->size >= sizeof(unsigned char))
	    {
		*((unsigned char *)to->addr) = val;
		to->size = sizeof(unsigned char);
		return True;
	    }
	    else
	    {
		XtDisplayStringConversionWarning(dpy, (char *)from->addr,
						 XmRWmUsePPosition);
		return False;
	    }
	}
    }
    XtDisplayStringConversionWarning(dpy, (char *)from->addr, XmRWmUsePPosition);
    return False;
}

/*
 * get resources for a client window
 */
void
RES_GetClientDefaults(ScreenInfo *scr, MwmWindow *tmp_win,
		      char *name_s, char *class_s)
{
    working_base = tmp_win;
    rscr = scr;
    XtGetSubresources(toplevel, (XtPointer)tmp_win,
		      name_s, class_s,
		      mwm_client_resources,
		      XtNumber(mwm_client_resources), NULL, 0);
}

void
RES_Initialize(void)
{
    XmRegisterConverters();

    XtSetTypeConverter(XmRString,
		       XmRWmCDecor,
		       WmCvtStringToCDecor,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmCFocus,
		       WmCvtStringToCFocus,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmCFunc,
		       WmCvtStringToCFunc,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmIDecor,
		       WmCvtStringToIDecor,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmIPlace,
		       WmCvtStringToIPlace,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmKFocus,
		       WmCvtStringToKFocus,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmShowFeedback,
		       WmCvtStringToShowFeedback,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmSize,
		       WmCvtStringToSize,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmUsePPosition,
		       WmCvtStringToUsePPosition,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtSetTypeConverter(XmRString,
		       XmRWmCFlags,
		       WmCvtStringToCFlags,
		       (XtConvertArgList)NULL,
		       0,
		       XtCacheAll,
		       NULL);

    XtGetApplicationResources(toplevel, (XtPointer)&Mwm,
			      mwm_resources, XtNumber(mwm_resources), NULL, 0);
}

/*
 * get the resources for each screen
 */
void
RES_GetScreenDefaults(ScreenInfo *scr)
{
    rscr = scr;
#if 0
    /*FIXME: do we need this here? A.R.*/
    working_base = scr;
#endif
    XtGetSubresources(toplevel, (XtPointer)scr,
                      scr->screen_name, scr->screen_name,
                      mwm_screen_resources,
                      XtNumber(mwm_screen_resources), NULL, 0);
}

/*
 * get the resources for the components in each screen
 */
void
RES_GetComponentDefaults(ScreenInfo *scr)
{
    Widget client;

    /*
     * MLM: OK, seems the resource manager won't pay attention to me if
     * I ask it to find recursive structures.  This is a hack, and I'd
     * appreciate it if someone would correct the name/class values so
     * I don't have to do this.
     */
    rscr = scr;
    working_base = &(scr->components[MWM_MENU]);
    scr->components[MWM_MENU].type = MWM_MENU;
    XtGetSubresources(toplevel, (XtPointer)&(scr->components[MWM_MENU]),
		      "menu", "menu",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);

    working_base = &(scr->components[MWM_FEEDBACK]);
    scr->components[MWM_FEEDBACK].type = MWM_FEEDBACK;
    XtGetSubresources(toplevel, (XtPointer)&(scr->components[MWM_FEEDBACK]),
		      "feedback", "feedback",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);

    working_base = &(scr->components[MWM_PAGER]);
    scr->components[MWM_PAGER].type = MWM_PAGER;
    XtGetSubresources(toplevel, (XtPointer)&(scr->components[MWM_PAGER]),
		      "pager", "pager",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);

    working_base = &(scr->components[MWM_ICON]);
    scr->components[MWM_ICON].type = MWM_ICON;
    XtGetSubresources(toplevel, (XtPointer)&(scr->components[MWM_ICON]),
		      "icon", "icon",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(toplevel, (XtPointer)&(scr->components[MWM_ICON]),
		      "icon", "icon",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);

    client = XtVaCreateWidget("client", (WidgetClass)&objectClassRec,
			      toplevel, NULL);

    working_base = &(scr->components[MWM_TITLE_A]);
    scr->components[MWM_TITLE_A].type = MWM_TITLE_A;
    XtGetSubresources(client, (XtPointer)working_base,
		      "title", "title",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(client, (XtPointer)working_base,
		      "title", "title",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);

    working_base = &(scr->components[MWM_RESIZE_H]);
    scr->components[MWM_RESIZE_H].type = MWM_RESIZE_H;
    XtGetSubresources(client, (XtPointer)working_base,
		      "resizeh", "resizeh",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(client, (XtPointer)working_base,
		      "resizeh", "resizeh",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);

    working_base = &(scr->components[MWM_BORDER]);
    scr->components[MWM_BORDER].type = MWM_BORDER;
    XtGetSubresources(client, (XtPointer)working_base,
		      "border", "border",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(client, (XtPointer)working_base,
		      "border", "border",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);

    working_base = &(scr->components[MWM_MAXIMIZE_B]);
    scr->components[MWM_MAXIMIZE_B].type = MWM_MAXIMIZE_B;
    XtGetSubresources(client, (XtPointer)working_base,
		      "maximizeb", "maximizeb",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(client, (XtPointer)working_base,
		      "maximizeb", "maximizeb",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);

    working_base = &(scr->components[MWM_MINIMIZE_B]);
    scr->components[MWM_MINIMIZE_B].type = MWM_MINIMIZE_B;
    XtGetSubresources(client, (XtPointer)working_base,
		      "minimizeb", "minimizeb",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(client, (XtPointer)working_base,
		      "minimizeb", "minimizeb",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);

    working_base = &(scr->components[MWM_MENU_B]);
    scr->components[MWM_MENU_B].type = MWM_MENU_B;
    XtGetSubresources(client, (XtPointer)working_base,
		      "menub", "menub",
		      mwm_component_resources,
		      XtNumber(mwm_component_resources), NULL, 0);
    XtGetSubresources(client, (XtPointer)working_base,
		      "menub", "menub",
		      mwm_component_fi_resources,
		      XtNumber(mwm_component_fi_resources), NULL, 0);
}
