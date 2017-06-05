/********************************************************************************
*                                                                               *
*                    M u l t i - L i ne   T e x t   O b j e c t                 *
*                                                                               *
*********************************************************************************
* Copyright (C) 1998,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXText.cpp,v 1.348.2.3 2007/06/29 13:47:37 fox Exp $                         *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxkeys.h"
#include "fxascii.h"
#include "fxunicode.h"
#include "FXHash.h"
#include "FXThread.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXRex.h"
#include "FXSize.h"
#include "FXPoint.h"
#include "FXRectangle.h"
#include "FXObject.h"
#include "FXRegistry.h"
#include "FXAccelTable.h"
#include "FXApp.h"
#include "FXDCWindow.h"
#include "FXFont.h"
#include "FXGIFIcon.h"
#include "FXScrollBar.h"
#include "FXText.h"
#include "FXInputDialog.h"
#include "FXReplaceDialog.h"
#include "FXSearchDialog.h"
#include "FX88591Codec.h"
#include "FXCP1252Codec.h"
#include "FXUTF16Codec.h"
#include "FXComposeContext.h"
#include "icons.h"



/*
  Notes:
  - Line start array is one longer than number of visible lines.
  - We want both tab translated to spaces as well as tab-stops array.
  - Control characters in the buffer are OK (e.g. ^L)
  - Drag cursor should be same as normal one until drag starts!
  - Change of cursor only implies makePositionVisible() if done by user.
  - Breaking:
    Soft-hyphen     173  \xAD
    No break space  240  \xF0
  - Buffer layout:

    Content  :  A  B  C  .  .  .  .  .  .  .  .  D  E  F  G
    Position :  0  1  2 			 3  4  5  6    length=7
    Addresss :  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14    buffersize=7+11-3=15
             		 ^			 ^
	     		 |			 |
	     	      gapstart=3	       gapend=11       gaplen=11-3=8

    The gap is moved around the buffer so newly added text
    can be entered into the gap; when the gap becomes too small,
    the buffer is resized.  This gapped-buffer technique minimizes
    the number of resizes of the buffer, and minimizes the number
    of block moves.

    The tail end of the visrows array will look like:

    visrow[0]= 0: "Q R S T U V W \n"
    visrow[1]= 8: "X Y Z"
    visrow[2]=11: <no text>
    visrow[3]=11: <no text>            length = 11

    The last legal position is length = 11.

  - While resizing window, keep track of a position which should remain visible,
    i.e. toppos=rowStart(position).  The position is changed same as toppos, except during
    resize.
  - When changing text, if we're looking at the tail end of the buffer, avoid jumping
    the top lines when the content hight shrinks.
  - Add undo capability. (First undo will turn mod flag back off).
  - Add incremental search, search/replace, selection search.
  - Style table stuff.
  - Need to allow for one single routine to update style buffer same as text buffer
  - Suggested additional bindings:
    Ctl-F   Find
    Ctl-R   Replace
    Ctl-G   Find again (Shift-Ctl-G Find again backward)
    Ctl-T   Replace again (Shift-Ctl-G Replace again backward)
    Ctl-L   Goto line number
    Ctl-E   Goto selected
    Ctl-I   Indent (shift 1 character right)
    Ctl-U   Unindent (shift 1 character left)
    Ctl-Z   undo
    Ctl-Y   redo
    Ctl-M   Goto matching (Shift-Ctl-M backward)
    Insert  toggle overstrike mode
    Brace matching
  - Maybe put all keyboard bindings into accelerator table.
  - Variable cursorcol should take tabcolumns into account.
  - Italic fonts are bit problematic on border between selected/unselected text
    due to kerning.
  - Tab should work as tabcolumns columns when computing a column.
  - Need rectangular selection capability.
  - Perhaps split off buffer management into separate text buffer class (allows for multiple views).
  - Need to implement regex search/replace.
  - Add better support for subclassing (syntax coloring e.g.).
  - Add support for line numbers.
  - Improve book keeping based on line/column numbers, not rows/characters.
  - If there is a style table, the style buffer is used as index into the style table,
    allowing for up to 255 styles (style index==0 is the default style).
    The style member in the FXHiliteStyle struct is used for underlining, strikeouts,
    and other effects.
    If there is NO style table but there is a style buffer, the style buffer can still
    be used for underlining, strikeouts, and other effects.
  - Sending SEL_CHANGED is pretty useless; should only be sent AFTER text change,
    and void* should contain some sensible info.
  - When in overstrike mode and having a selection, entering a character should
    replace the selection, not delete the selection and then overstrike the character
    after the selection.
  - Middle mouse paste does not paste inside selection, and does not kill selection.
  - When pasting or dropping whole lines, insert at begin of line instead of at cursor;
    question:- how to know we're pasting whole lines?
  - Need block cursor when in overstrike mode.
  - Inserting lots of stuff should show cursor.
  - Perhaps change text and style buffer to FXString for further complexity
    reduction.
*/


#define MINSIZE   80                  // Minimum gap size
#define NVISROWS  20                  // Initial visible rows

#define TEXT_MASK   (TEXT_FIXEDWRAP|TEXT_WORDWRAP|TEXT_OVERSTRIKE|TEXT_READONLY|TEXT_NO_TABS|TEXT_AUTOINDENT|TEXT_SHOWACTIVE|TEXT_AUTOSCROLL)

using namespace FX;

/*******************************************************************************/

namespace FX {


// Map
FXDEFMAP(FXText) FXTextMap[]={
  FXMAPFUNC(SEL_PAINT,0,FXText::onPaint),
  FXMAPFUNC(SEL_MOTION,0,FXText::onMotion),
  FXMAPFUNC(SEL_DRAGGED,0,FXText::onDragged),
  FXMAPFUNC(SEL_TIMEOUT,FXText::ID_BLINK,FXText::onBlink),
  FXMAPFUNC(SEL_TIMEOUT,FXText::ID_AUTOSCROLL,FXText::onAutoScroll),
  FXMAPFUNC(SEL_TIMEOUT,FXText::ID_FLASH,FXText::onFlash),
  FXMAPFUNC(SEL_FOCUSIN,0,FXText::onFocusIn),
  FXMAPFUNC(SEL_FOCUSOUT,0,FXText::onFocusOut),
  FXMAPFUNC(SEL_BEGINDRAG,0,FXText::onBeginDrag),
  FXMAPFUNC(SEL_ENDDRAG,0,FXText::onEndDrag),
  FXMAPFUNC(SEL_LEFTBUTTONPRESS,0,FXText::onLeftBtnPress),
  FXMAPFUNC(SEL_LEFTBUTTONRELEASE,0,FXText::onLeftBtnRelease),
  FXMAPFUNC(SEL_MIDDLEBUTTONPRESS,0,FXText::onMiddleBtnPress),
  FXMAPFUNC(SEL_MIDDLEBUTTONRELEASE,0,FXText::onMiddleBtnRelease),
  FXMAPFUNC(SEL_RIGHTBUTTONPRESS,0,FXText::onRightBtnPress),
  FXMAPFUNC(SEL_RIGHTBUTTONRELEASE,0,FXText::onRightBtnRelease),
  FXMAPFUNC(SEL_UNGRABBED,0,FXText::onUngrabbed),
  FXMAPFUNC(SEL_DND_ENTER,0,FXText::onDNDEnter),
  FXMAPFUNC(SEL_DND_LEAVE,0,FXText::onDNDLeave),
  FXMAPFUNC(SEL_DND_DROP,0,FXText::onDNDDrop),
  FXMAPFUNC(SEL_DND_MOTION,0,FXText::onDNDMotion),
  FXMAPFUNC(SEL_DND_REQUEST,0,FXText::onDNDRequest),
  FXMAPFUNC(SEL_SELECTION_LOST,0,FXText::onSelectionLost),
  FXMAPFUNC(SEL_SELECTION_GAINED,0,FXText::onSelectionGained),
  FXMAPFUNC(SEL_SELECTION_REQUEST,0,FXText::onSelectionRequest),
  FXMAPFUNC(SEL_CLIPBOARD_LOST,0,FXText::onClipboardLost),
  FXMAPFUNC(SEL_CLIPBOARD_GAINED,0,FXText::onClipboardGained),
  FXMAPFUNC(SEL_CLIPBOARD_REQUEST,0,FXText::onClipboardRequest),
  FXMAPFUNC(SEL_KEYPRESS,0,FXText::onKeyPress),
  FXMAPFUNC(SEL_KEYRELEASE,0,FXText::onKeyRelease),
  FXMAPFUNC(SEL_QUERY_TIP,0,FXText::onQueryTip),
  FXMAPFUNC(SEL_QUERY_HELP,0,FXText::onQueryHelp),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_TOGGLE_EDITABLE,FXText::onUpdToggleEditable),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_TOGGLE_OVERSTRIKE,FXText::onUpdToggleOverstrike),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_CURSOR_ROW,FXText::onUpdCursorRow),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_CURSOR_COLUMN,FXText::onUpdCursorColumn),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_CUT_SEL,FXText::onUpdHaveSelection),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_COPY_SEL,FXText::onUpdHaveSelection),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_PASTE_SEL,FXText::onUpdYes),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_DELETE_SEL,FXText::onUpdHaveSelection),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_SELECT_ALL,FXText::onUpdSelectAll),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_UPPER_CASE,FXText::onUpdHaveSelection),
  FXMAPFUNC(SEL_UPDATE,FXText::ID_LOWER_CASE,FXText::onUpdHaveSelection),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_TOP,FXText::onCmdCursorTop),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_BOTTOM,FXText::onCmdCursorBottom),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_HOME,FXText::onCmdCursorHome),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_END,FXText::onCmdCursorEnd),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_RIGHT,FXText::onCmdCursorRight),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_LEFT,FXText::onCmdCursorLeft),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_UP,FXText::onCmdCursorUp),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_DOWN,FXText::onCmdCursorDown),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_WORD_LEFT,FXText::onCmdCursorWordLeft),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_WORD_RIGHT,FXText::onCmdCursorWordRight),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_WORD_START,FXText::onCmdCursorWordStart),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_WORD_END,FXText::onCmdCursorWordEnd),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_PAGEDOWN,FXText::onCmdCursorPageDown),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_PAGEUP,FXText::onCmdCursorPageUp),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_SCRNTOP,FXText::onCmdCursorScreenTop),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_SCRNBTM,FXText::onCmdCursorScreenBottom),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_SCRNCTR,FXText::onCmdCursorScreenCenter),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_PAR_HOME,FXText::onCmdCursorParHome),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_PAR_END,FXText::onCmdCursorParEnd),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SCROLL_UP,FXText::onCmdScrollUp),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SCROLL_DOWN,FXText::onCmdScrollDown),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_MARK,FXText::onCmdMark),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_EXTEND,FXText::onCmdExtend),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_OVERST_STRING,FXText::onCmdOverstString),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_INSERT_STRING,FXText::onCmdInsertString),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_INSERT_NEWLINE,FXText::onCmdInsertNewline),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_INSERT_TAB,FXText::onCmdInsertTab),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CUT_SEL,FXText::onCmdCutSel),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_COPY_SEL,FXText::onCmdCopySel),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DELETE_SEL,FXText::onCmdDeleteSel),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_PASTE_SEL,FXText::onCmdPasteSel),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_PASTE_MIDDLE,FXText::onCmdPasteMiddle),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SELECT_CHAR,FXText::onCmdSelectChar),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SELECT_WORD,FXText::onCmdSelectWord),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SELECT_LINE,FXText::onCmdSelectLine),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SELECT_ALL,FXText::onCmdSelectAll),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DESELECT_ALL,FXText::onCmdDeselectAll),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_BACKSPACE,FXText::onCmdBackspace),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_BACKSPACE_WORD,FXText::onCmdBackspaceWord),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_BACKSPACE_BOL,FXText::onCmdBackspaceBol),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DELETE,FXText::onCmdDelete),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DELETE_WORD,FXText::onCmdDeleteWord),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DELETE_EOL,FXText::onCmdDeleteEol),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DELETE_ALL,FXText::onCmdDeleteAll),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_DELETE_LINE,FXText::onCmdDeleteLine),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_TOGGLE_EDITABLE,FXText::onCmdToggleEditable),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_TOGGLE_OVERSTRIKE,FXText::onCmdToggleOverstrike),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_ROW,FXText::onCmdCursorRow),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_CURSOR_COLUMN,FXText::onCmdCursorColumn),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SETSTRINGVALUE,FXText::onCmdSetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_GETSTRINGVALUE,FXText::onCmdGetStringValue),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_UPPER_CASE,FXText::onCmdChangeCase),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_LOWER_CASE,FXText::onCmdChangeCase),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_GOTO_MATCHING,FXText::onCmdGotoMatching),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_GOTO_SELECTED,FXText::onCmdGotoSelected),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_GOTO_LINE,FXText::onCmdGotoLine),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SELECT_MATCHING,FXText::onCmdSelectMatching),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SEARCH,FXText::onCmdSearch),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_REPLACE,FXText::onCmdReplace),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SEARCH_FORW,FXText::onCmdSearchNext),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SEARCH_BACK,FXText::onCmdSearchNext),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SEARCH_FORW_SEL,FXText::onCmdSearchSel),
  FXMAPFUNC(SEL_COMMAND,FXText::ID_SEARCH_BACK_SEL,FXText::onCmdSearchSel),
  FXMAPFUNCS(SEL_COMMAND,FXText::ID_SELECT_BRACE,FXText::ID_SELECT_ANG,FXText::onCmdSelectBlock),
  FXMAPFUNCS(SEL_COMMAND,FXText::ID_LEFT_BRACE,FXText::ID_LEFT_ANG,FXText::onCmdBlockBeg),
  FXMAPFUNCS(SEL_COMMAND,FXText::ID_RIGHT_BRACE,FXText::ID_RIGHT_ANG,FXText::onCmdBlockEnd),
  FXMAPFUNCS(SEL_COMMAND,FXText::ID_CLEAN_INDENT,FXText::ID_SHIFT_TABRIGHT,FXText::onCmdShiftText),
  };


// Object implementation
FXIMPLEMENT(FXText,FXScrollArea,FXTextMap,ARRAYNUMBER(FXTextMap))


// Delimiters
const FXchar FXText::textDelimiters[]="~.,/\\`'!@#$%^&*()-=+{}|[]\":;<>?";

/*******************************************************************************/


// Absolute value
static inline FXint fxabs(FXint a){ return a<0?-a:a; }


// For deserialization
FXText::FXText(){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  buffer=NULL;
  sbuffer=NULL;
  visrows=NULL;
  length=0;
  nrows=1;
  nvisrows=0;
  gapstart=0;
  gapend=0;
  toppos=0;
  keeppos=0;
  toprow=0;
  selstartpos=0;
  selendpos=0;
  hilitestartpos=0;
  hiliteendpos=0;
  anchorpos=0;
  cursorpos=0;
  revertpos=0;
  cursorstart=0;
  cursorend=0;
  cursorrow=0;
  cursorcol=0;
  prefcol=-1;
  wrapwidth=80;
  wrapcolumns=80;
  tabwidth=8;
  tabcolumns=8;
  barwidth=0;
  barcolumns=0;
  hilitestyles=NULL;
  textWidth=0;
  textHeight=0;
  searchflags=SEARCH_EXACT;
  delimiters=textDelimiters;
  vrows=0;
  vcols=0;
  matchtime=0;
  modified=FALSE;
  mode=MOUSE_NONE;
  grabx=0;
  graby=0;
  }


// Text widget
FXText::FXText(FXComposite *p,FXObject* tgt,FXSelector sel,FXuint opts,FXint x,FXint y,FXint w,FXint h,FXint pl,FXint pr,FXint pt,FXint pb):
  FXScrollArea(p,opts,x,y,w,h){
  flags|=FLAG_ENABLED|FLAG_DROPTARGET;
  target=tgt;
  message=sel;
  FXCALLOC(&buffer,FXchar,MINSIZE);
  sbuffer=NULL;
  FXCALLOC(&visrows,FXint,NVISROWS+1);
  length=0;
  nrows=1;
  nvisrows=NVISROWS;
  gapstart=0;
  gapend=MINSIZE;
  toppos=0;
  keeppos=0;
  toprow=0;
  selstartpos=0;
  selendpos=0;
  hilitestartpos=0;
  hiliteendpos=0;
  anchorpos=0;
  cursorpos=0;
  revertpos=0;
  cursorstart=0;
  cursorend=0;
  cursorrow=0;
  cursorcol=0;
  prefcol=-1;
  margintop=pt;
  marginbottom=pb;
  marginleft=pl;
  marginright=pr;
  wrapwidth=80;
  wrapcolumns=80;
  tabwidth=8;
  tabcolumns=8;
  barwidth=0;
  barcolumns=0;
  font=getApp()->getNormalFont();
  hilitestyles=NULL;
  defaultCursor=getApp()->getDefaultCursor(DEF_TEXT_CURSOR);
  dragCursor=getApp()->getDefaultCursor(DEF_TEXT_CURSOR);
  textColor=getApp()->getForeColor();
  selbackColor=getApp()->getSelbackColor();
  seltextColor=getApp()->getSelforeColor();
  hilitebackColor=FXRGB(255,128,128);
  hilitetextColor=getApp()->getForeColor();
  activebackColor=backColor;
  cursorColor=getApp()->getForeColor();
  numberColor=textColor;
  barColor=backColor;
  textWidth=0;
  textHeight=0;
  searchflags=SEARCH_EXACT;
  delimiters=textDelimiters;
  vrows=0;
  vcols=0;
  matchtime=0;
  modified=FALSE;
  mode=MOUSE_NONE;
  grabx=0;
  graby=0;
  }


// Create window
void FXText::create(){
  FXScrollArea::create();
  font->create();
  if(!deleteType){ deleteType=getApp()->registerDragType(deleteTypeName); }
  if(!textType){ textType=getApp()->registerDragType(textTypeName); }
  if(!utf8Type){ utf8Type=getApp()->registerDragType(utf8TypeName); }
  if(!utf16Type){ utf16Type=getApp()->registerDragType(utf16TypeName); }
//  if(options&TEXT_FIXEDWRAP){ wrapwidth=wrapcolumns*font->getTextWidth("x",1); }
  tabwidth=tabcolumns*font->getTextWidth(" ",1);
  barwidth=barcolumns*font->getTextWidth("8",1);
  recalc();
  }


// Detach window
void FXText::detach(){
  FXScrollArea::detach();
  font->detach();
  deleteType=0;
  textType=0;
  utf8Type=0;
  utf16Type=0;
  }


// If window can have focus
bool FXText::canFocus() const {
  return true;
  }


// Into focus chain
void FXText::setFocus(){
  FXScrollArea::setFocus();
  setDefault(TRUE);
  flags&=~FLAG_UPDATE;
  if(getApp()->hasInputMethod()){
    createComposeContext();
    }
  }


// Out of focus chain
void FXText::killFocus(){
  FXScrollArea::killFocus();
  setDefault(MAYBE);
  flags|=FLAG_UPDATE;
  if(getApp()->hasInputMethod()){
    destroyComposeContext();
    }
  }


// Get default width
FXint FXText::getDefaultWidth(){
  if(0<vcols){ return marginleft+barwidth+marginright+vcols*font->getTextWidth("8",1); }
  return FXScrollArea::getDefaultWidth();
  }


// Get default height
FXint FXText::getDefaultHeight(){
  if(0<vrows){ return margintop+marginbottom+vrows*font->getFontHeight(); }
  return FXScrollArea::getDefaultHeight();
  }


// Enable the window
void FXText::enable(){
  if(!(flags&FLAG_ENABLED)){
    FXScrollArea::enable();
    update(0,0,viewport_w,viewport_h);
    }
  }


// Disable the window
void FXText::disable(){
  if(flags&FLAG_ENABLED){
    FXScrollArea::disable();
    update(0,0,viewport_w,viewport_h);
    }
  }


// Propagate size change
void FXText::recalc(){
  FXScrollArea::recalc();
  flags|=FLAG_RECALC;
  }


/*******************************************************************************/


// Make a valid position, at the start of a wide character
FXint FXText::validPos(FXint pos) const {
  register const FXchar *ptr=pos<gapstart ? buffer : buffer-gapstart+gapend;
  if(pos<=0) return 0;
  if(pos>=length) return length;
  return (FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos), pos;
  }


// Decrement; a wide character does not cross the gap, so if pos is at
// or below below the gap, we read from the segment below the gap
FXint FXText::dec(FXint pos) const {
  register const FXchar *ptr=pos<=gapstart ? buffer : buffer-gapstart+gapend;
  return (--pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos<=0 || FXISUTF(ptr[pos]) || --pos), pos;
  }


// Increment; since a wide character does not cross the gap, if we
// start under the gap the last character accessed is below the gap
FXint FXText::inc(FXint pos) const {
  register const FXchar *ptr=pos<gapstart ? buffer : buffer-gapstart+gapend;
  return (++pos>=length || FXISUTF(ptr[pos]) || ++pos>=length || FXISUTF(ptr[pos]) || ++pos>=length || FXISUTF(ptr[pos]) || ++pos>=length || FXISUTF(ptr[pos]) || ++pos>=length || FXISUTF(ptr[pos]) || ++pos), pos;
  }


// Get byte
FXint FXText::getByte(FXint pos) const {
  return (FXuchar)buffer[pos<gapstart ? pos : pos-gapstart+gapend];
  }


// Get character, assuming that gap never inside utf8 encoding
FXwchar FXText::getChar(FXint pos) const {
  register const FXuchar* ptr=(pos<gapstart)?(FXuchar*)(buffer+pos):(FXuchar*)(buffer+pos-gapstart+gapend);
  register FXwchar w=ptr[0];
  if(0xC0<=w){ w=(w<<6)^ptr[1]^0x3080;
  if(0x800<=w){ w=(w<<6)^ptr[2]^0x20080;
  if(0x10000<=w){ w=(w<<6)^ptr[3]^0x400080;
  if(0x200000<=w){ w=(w<<6)^ptr[4]^0x8000080;
  if(0x4000000<=w){ w=(w<<6)^ptr[5]^0x80; }}}}}
  return w;
  }


// Get length of wide character at position pos
FXint FXText::getCharLen(FXint pos) const {
  return FXString::utfBytes[(FXuchar)buffer[pos<gapstart ? pos : pos-gapstart+gapend]];
  }


// Get style
FXint FXText::getStyle(FXint pos) const {
  return (FXuchar)sbuffer[pos<gapstart ? pos : pos-gapstart+gapend];
  }


// Move the gap; gap is never moved inside utf character
void FXText::movegap(FXint pos){
  register FXint gaplen=gapend-gapstart;
  FXASSERT(0<=pos && pos<=length);
  FXASSERT(0<=gapstart && gapstart<=length);
  if(gapstart<pos){
    memmove(&buffer[gapstart],&buffer[gapend],pos-gapstart);
    if(sbuffer){memmove(&sbuffer[gapstart],&sbuffer[gapend],pos-gapstart);}
    gapend=pos+gaplen;
    gapstart=pos;
    }
  else if(pos<gapstart){
    memmove(&buffer[pos+gaplen],&buffer[pos],gapstart-pos);
    if(sbuffer){memmove(&sbuffer[pos+gaplen],&sbuffer[pos],gapstart-pos);}
    gapend=pos+gaplen;
    gapstart=pos;
    }
  }


// Size gap
void FXText::sizegap(FXint sz){
  register FXint gaplen=gapend-gapstart;
  FXASSERT(0<=gapstart && gapstart<=length);
  if(sz>=gaplen){
    sz+=MINSIZE;
    if(!FXRESIZE(&buffer,FXchar,length+sz)){
      fxerror("%s::sizegap: out of memory.\n",getClassName());
      }
    memmove(&buffer[gapstart+sz],&buffer[gapend],length-gapstart);
    if(sbuffer){
      if(!FXRESIZE(&sbuffer,FXchar,length+sz)){
        fxerror("%s::sizegap: out of memory.\n",getClassName());
        }
      memmove(&sbuffer[gapstart+sz],&sbuffer[gapend],length-gapstart);
      }
    gapend=gapstart+sz;
    }
  }


// Squeeze out the gap by moving it to the end of the buffer
void FXText::squeezegap(){
  if(gapstart!=length){
    memmove(&buffer[gapstart],&buffer[gapend],length-gapstart);
    if(sbuffer){memmove(&sbuffer[gapstart],&sbuffer[gapend],length-gapstart);}
    gapend=length+gapend-gapstart;
    gapstart=length;
    }
  }


/*******************************************************************************/

// FIXME
// Its a little bit more complex than this:
// We need to deal with diacritics, i.e. non-spacing stuff.  When wrapping, scan till
// the next starter-character [the one with charCombining(c)==0].  Then measure the
// string from that point on. This means FXFont::getCharWidth() is really quite useless.
// Next, we also have the issue of ligatures [fi, AE] and kerning-pairs [VA].
// With possible kerning pairs, we should really measure stuff from the start of the
// line [but this is *very* expensive!!].  We may want to just back up a few characters;
// perhaps to the start of the word, or just the previous character, if not a space.
// Need to investigate this some more; for now assume Normalization Form C.

// Character width
FXint FXText::charWidth(FXwchar ch,FXint indent) const {
  if(ch<' '){
    if(ch!='\t'){
      return font->getCharWidth('^')+font->getCharWidth(ch|0x40);
      }
    return (tabwidth-indent%tabwidth);
    }
  return font->getCharWidth(ch);
  }


// Start of next wrapped line
FXint FXText::wrap(FXint start) const {
  register FXint lw,cw,p,s,c;
  FXASSERT(0<=start && start<=length);
  lw=0;
  p=s=start;
  while(p<length){
    c=getChar(p);
    if(c=='\n') return p+1;     // Newline always breaks
    cw=charWidth(c,lw);
    if(lw+cw>wrapwidth){        // Technically, a tab-before-wrap should be as wide as space!
      if(s>start) return s;     // We remembered the last space we encountered; break there!
      if(p==start) p++;         // Always at least one character on each line!
      return p;
      }
    lw+=cw;
    p+=getCharLen(p);
    if(Unicode::isSpace(c)) s=p;// Remember potential break point!
    }
  return length;
  }


// Count number of newlines
FXint FXText::countLines(FXint start,FXint end) const {
  register FXint p,nl=0;
  FXASSERT(0<=start && end<=length+1);
  p=start;
  while(p<end){
    if(p>=length) return nl+1;
    if(getByte(p)=='\n') nl++;
    p++;
    }
  return nl;
  }


// Count number of rows; start should be on a row start
FXint FXText::countRows(FXint start,FXint end) const {
  register FXint p,q,s,w=0,c,cw,nr=0;
  FXASSERT(0<=start && end<=length+1);
  if(options&TEXT_WORDWRAP){
    p=q=s=start;
    while(q<end){
      if(p>=length) return nr+1;
      c=getChar(p);
      if(c=='\n'){                      // Break at newline
        nr++;
        w=0;
        p=q=s=p+1;
        continue;
        }
      cw=charWidth(c,w);
      if(w+cw>wrapwidth){               // Break due to wrap
        nr++;
        w=0;
        if(s>q){                        // Break past last space seen
          p=q=s;
          continue;
          }
        if(p==q) p+=getCharLen(p);      // Break anywhere, but at least one character on each line
        q=s=p;
        continue;
        }
      w+=cw;
      p+=getCharLen(p);
      if(Unicode::isSpace(c)) s=p;
      }
    }
  else{
    p=start;
    while(p<end){
      if(p>=length) return nr+1;
      c=getByte(p);
      if(c=='\n') nr++;
      p++;
      }
    }
  return nr;
  }


// Count number of columns; start should be on a row start
FXint FXText::countCols(FXint start,FXint end) const {
  register FXint nc=0,in=0,ch;
  FXASSERT(0<=start && end<=length);
  while(start<end){
    ch=getChar(start);
    if(ch=='\n'){
      if(in>nc) nc=in;
      in=0;
      }
    else if(ch=='\t'){
      in+=(tabcolumns-nc%tabcolumns);
      }
    else{
      in++;
      }
    start+=getCharLen(start);
    }
  if(in>nc) nc=in;
  return nc;
  }


// Measure lines; start and end should be on a row start
FXint FXText::measureText(FXint start,FXint end,FXint& wmax,FXint& hmax) const {
  register FXint nr=0,w=0,c,cw,p,q,s;
  FXASSERT(0<=start && end<=length+1);
  if(options&TEXT_WORDWRAP){
    wmax=wrapwidth;
    p=q=s=start;
    while(q<end){
      if(p>=length){
        nr++;
        break;
        }
      c=getChar(p);
      if(c=='\n'){                      // Break at newline
        nr++;
        w=0;
        p=q=s=p+1;
        continue;
        }
      cw=charWidth(c,w);
      if(w+cw>wrapwidth){               // Break due to wrap
        nr++;
        w=0;
        if(s>q){                        // Break past last space seen
          p=q=s;
          continue;
          }
        if(p==q) p+=getCharLen(p);      // Break anywhere, but at least one character on each line
        q=s=p;
        continue;
        }
      w+=cw;
      p+=getCharLen(p);
      if(Unicode::isSpace(c)) s=p;
      }
    }
  else{
    wmax=0;
    p=start;
    while(p<end){
      if(p>=length){
        if(w>wmax) wmax=w;
        nr++;
        break;
        }
      c=getChar(p);
      if(c=='\n'){                      // Break at newline
        if(w>wmax) wmax=w;
        nr++;
        w=0;
        }
      else{
        w+=charWidth(c,w);
        }
      p+=getCharLen(p);
      }
    }
  hmax=nr*font->getFontHeight();
  return nr;
  }


// Check if w is delimiter
static FXbool isdelimiter(const FXchar *delimiters,FXwchar w){
  return w<128 && strchr(delimiters,w); // FIXME for w>=128
  }


// Find end of previous word
FXint FXText::leftWord(FXint pos) const {
  register FXint ch;
  if(pos>length) pos=length;
  if(0<pos){
    ch=getChar(dec(pos));
    if(isdelimiter(delimiters,ch)) return dec(pos);
    }
  while(0<pos){
    ch=getChar(dec(pos));
    if(isdelimiter(delimiters,ch)) return pos;
    if(Unicode::isSpace(ch)) break;
    pos=dec(pos);
    }
  while(0<pos){
    ch=getChar(dec(pos));
    if(!Unicode::isSpace(ch)) return pos;
    pos=dec(pos);
    }
  return 0;
  }


// Find begin of next word
FXint FXText::rightWord(FXint pos) const {
  register FXint ch;
  if(pos<0) pos=0;
  if(pos<length){
    ch=getChar(pos);
    if(isdelimiter(delimiters,ch)) return inc(pos);
    }
  while(pos<length){
    ch=getChar(pos);
    if(isdelimiter(delimiters,ch)) return pos;
    if(Unicode::isSpace(ch)) break;
    pos=inc(pos);
    }
  while(pos<length){
    ch=getChar(pos);
    if(!Unicode::isSpace(ch)) return pos;
    pos=inc(pos);
    }
  return length;
  }


// Find begin of a word
FXint FXText::wordStart(FXint pos) const {
  register FXint c=' ';
  if(pos<=0) return 0;
  if(pos<length) c=getChar(pos); else pos=length;
  if(c==' ' || c=='\t'){
    while(0<pos){
      c=getChar(dec(pos));
      if(c!=' ' && c!='\t') return pos;
      pos=dec(pos);
      }
    }
  else if(isdelimiter(delimiters,c)){
    while(0<pos){
      c=getChar(dec(pos));
      if(!isdelimiter(delimiters,c)) return pos;
      pos=dec(pos);
      }
    }
  else{
    while(0<pos){
      c=getChar(dec(pos));
      if(isdelimiter(delimiters,c) || Unicode::isSpace(c)) return pos;
      pos=dec(pos);
      }
    }
  return 0;
  }


// Find end of word
FXint FXText::wordEnd(FXint pos) const {
  register FXint c=' ';
  if(pos>=length) return length;
  if(0<=pos) c=getChar(pos); else pos=0;
  if(c==' ' || c=='\t'){
    while(pos<length){
      c=getChar(pos);
      if(c!=' ' && c!='\t') return pos;
      pos=inc(pos);
      }
    }
  else if(isdelimiter(delimiters,c)){
    while(pos<length){
      c=getChar(pos);
      if(!isdelimiter(delimiters,c)) return pos;
      pos=inc(pos);
      }
    }
  else{
    while(pos<length){
      c=getChar(pos);
      if(isdelimiter(delimiters,c) || Unicode::isSpace(c)) return pos;
      pos=inc(pos);
      }
    }
  return length;
  }


// Return position of begin of paragraph
FXint FXText::lineStart(FXint pos) const {
  FXASSERT(0<=pos && pos<=length);
  while(0<pos){ if(getByte(pos-1)=='\n') return pos; pos--; }
  return 0;
  }


// Return position of end of paragraph
FXint FXText::lineEnd(FXint pos) const {
  FXASSERT(0<=pos && pos<=length);
  while(pos<length){ if(getByte(pos)=='\n') return pos; pos++; }
  return length;
  }


// Return start of next line
FXint FXText::nextLine(FXint pos,FXint nl) const {
  FXASSERT(0<=pos && pos<=length);
  if(nl<=0) return pos;
  while(pos<length){
    if(getByte(pos)=='\n' && --nl==0) return pos+1;
    pos++;
    }
  return length;
  }


// Return start of previous line
FXint FXText::prevLine(FXint pos,FXint nl) const {
  FXASSERT(0<=pos && pos<=length);
  if(nl<=0) return pos;
  while(0<pos){
    if(getByte(pos-1)=='\n' && nl--==0) return pos;
    pos--;
    }
  return 0;
  }


// Return row start
FXint FXText::rowStart(FXint pos) const {
  register FXint p,t;
  FXASSERT(0<=pos && pos<=length);
  p=lineStart(pos);
  if(!(options&TEXT_WORDWRAP)) return p;
  while(p<pos && (t=wrap(p))<=pos && t<length) p=t;
  FXASSERT(0<=p && p<=pos);
  return p;
  }


// Return row end
FXint FXText::rowEnd(FXint pos) const {
  register FXint p;
  FXASSERT(0<=pos && pos<=length);
  if(!(options&TEXT_WORDWRAP)) return lineEnd(pos);
  p=lineStart(pos);
  while(p<length && p<=pos) p=wrap(p);
  FXASSERT(0<=p && p<=length);
  if(pos<p && Unicode::isSpace(getChar(dec(p)))) p=dec(p);
  FXASSERT(pos<=p && p<=length);
  return p;
  }


// Move to next row given start of line
FXint FXText::nextRow(FXint pos,FXint nr) const {
  register FXint p;
  FXASSERT(0<=pos && pos<=length);
  if(!(options&TEXT_WORDWRAP)) return nextLine(pos,nr);
  if(nr<=0) return pos;
  p=rowStart(pos);
  while(p<length && 0<nr){ p=wrap(p); nr--; }
  FXASSERT(0<=p && p<=length);
  return p;
  }


// Move to previous row given start of line
FXint FXText::prevRow(FXint pos,FXint nr) const {
  register FXint p,q,t;
  FXASSERT(0<=pos && pos<=length);
  if(!(options&TEXT_WORDWRAP)) return prevLine(pos,nr);
  if(nr<=0) return pos;
  while(0<pos){
    p=lineStart(pos);
    for(q=p; q<pos && (t=wrap(q))<=pos && t<length; q=t) nr--;
    if(nr==0) return p;
    if(nr<0){
      do{p=wrap(p);}while(++nr);
      FXASSERT(0<=p && p<=length);
      return p;
      }
    pos=p-1;
    nr--;
    }
  return 0;
  }


// Backs up to the begin of the line preceding the line containing pos, or the
// start of the line containing pos if the preceding line terminated in a newline
FXint FXText::changeBeg(FXint pos) const {
  register FXint p1,p2,t;
  FXASSERT(0<=pos && pos<=length);
  p1=p2=lineStart(pos);
  if(!(options&TEXT_WORDWRAP)) return p1;
  while(p2<pos && (t=wrap(p2))<=pos){
    p1=p2;
    p2=t;
    }
  FXASSERT(0<=p1 && p1<=length);
  return p1;
  }


// Scan forward to the end of affected area, which is the start of the next
// paragraph; a change can cause the rest of the paragraph to reflow.
FXint FXText::changeEnd(FXint pos) const {
  FXASSERT(0<=pos && pos<=length);
  while(pos<length){
    if(getByte(pos)=='\n') return pos+1;
    pos++;
    }
  return length+1;  // YES, one more!
  }


// Calculate line width
FXint FXText::lineWidth(FXint pos,FXint n) const {
  register FXint end=pos+n,w=0;
  FXASSERT(0<=pos && end<=length);
  while(pos<end){ w+=charWidth(getChar(pos),w); pos+=getCharLen(pos); }
  return w;
  }


// Determine indent of position pos relative to start
FXint FXText::indentFromPos(FXint start,FXint pos) const {
  register FXint p=start;
  register FXint in=0;
  register FXwchar c;
  FXASSERT(0<=start && pos<=length);
  while(p<pos){
    c=getChar(p);
    if(c=='\n'){
      in=0;
      }
    else if(c=='\t'){
      in+=(tabcolumns-in%tabcolumns);
      }
    else{
      in+=1;
      }
    p+=getCharLen(p);
    }
  return in;
  }


// Determine position of indent relative to start
FXint FXText::posFromIndent(FXint start,FXint indent) const {
  register FXint pos=start;
  register FXint in=0;
  register FXwchar c;
  FXASSERT(0<=start && start<=length);
  while(in<indent && pos<length){
    c=getChar(pos);
    if(c=='\n'){
      break;
      }
    else if(c=='\t'){
      in+=(tabcolumns-in%tabcolumns);
      }
    else{
      in+=1;
      }
    pos+=getCharLen(pos);
    }
  return pos;
  }



// Search forward for match
FXint FXText::matchForward(FXint pos,FXint end,FXwchar l,FXwchar r,FXint level) const {
  register FXwchar c;
  FXASSERT(0<=end && end<=length);
  FXASSERT(0<=pos && pos<=length);
  while(pos<end){
    c=getChar(pos);
    if(c==r){
      level--;
      if(level<=0) return pos;
      }
    else if(c==l){
      level++;
      }
    pos=inc(pos);
    }
  return -1;
  }


// Search backward for match
FXint FXText::matchBackward(FXint pos,FXint beg,FXwchar l,FXwchar r,FXint level) const {
  register FXwchar c;
  FXASSERT(0<=beg && beg<=length);
  FXASSERT(0<=pos && pos<=length);
  while(beg<=pos){
    c=getChar(pos);
    if(c==l){
      level--;
      if(level<=0) return pos;
      }
    else if(c==r){
      level++;
      }
    pos=dec(pos);
    }
  return -1;
  }


// Search for matching character
FXint FXText::findMatching(FXint pos,FXint beg,FXint end,FXwchar ch,FXint level) const {
  FXASSERT(0<=level);
  FXASSERT(0<=pos && pos<=length);
  switch(ch){
    case '{': return matchForward(pos+1,end,'{','}',level);
    case '}': return matchBackward(pos-1,beg,'{','}',level);
    case '[': return matchForward(pos+1,end,'[',']',level);
    case ']': return matchBackward(pos-1,beg,'[',']',level);
    case '(': return matchForward(pos+1,end,'(',')',level);
    case ')': return matchBackward(pos-1,beg,'(',')',level);
    }
  return -1;
  }


// Flash matching braces or parentheses, if within visible part of buffer
void FXText::flashMatching(){
  FXint matchpos;
  killHighlight();
  getApp()->removeTimeout(this,ID_FLASH);
  if(matchtime && 0<cursorpos){
    matchpos=findMatching(cursorpos-1,visrows[0],visrows[nvisrows],getByte(cursorpos-1),1);
    if(0<=matchpos){
      getApp()->addTimeout(this,ID_FLASH,matchtime);
      setHighlight(matchpos,1);
      }
    }
  }


// Search for text
FXbool FXText::findText(const FXString& string,FXint* beg,FXint* end,FXint start,FXuint flgs,FXint npar){
  register FXint rexmode;
  FXRex rex;

  // Compile flags
  rexmode=REX_VERBATIM;
  if(1<npar) rexmode|=REX_CAPTURE;
  if(flgs&SEARCH_REGEX) rexmode&=~REX_VERBATIM;
  if(flgs&SEARCH_IGNORECASE) rexmode|=REX_ICASE;

  // Try parse the regex
  if(rex.parse(string,rexmode)==REGERR_OK){

    // Make all characters contiguous in the buffer
    squeezegap();

    // Search backward
    if(flgs&SEARCH_BACKWARD){

      // Search from start to begin of buffer
      if(rex.match(buffer,length,beg,end,REX_BACKWARD,npar,0,start)) return TRUE;

      if(!(flgs&SEARCH_WRAP)) return FALSE;

      // Search from end of buffer backwards
      if(rex.match(buffer,length,beg,end,REX_BACKWARD,npar,start,length)) return TRUE;
      }

    // Search forward
    else{

      // Search from start to end of buffer
      if(rex.match(buffer,length,beg,end,REX_FORWARD,npar,start,length)) return TRUE;

      if(!(flgs&SEARCH_WRAP)) return FALSE;

      // Search from begin of buffer forwards
      if(rex.match(buffer,length,beg,end,REX_FORWARD,npar,0,start)) return TRUE;
      }
    }
  return FALSE;
  }


/*******************************************************************************/


// See if pos is a visible position
FXbool FXText::posVisible(FXint pos) const {
  return visrows[0]<=pos && pos<=visrows[nvisrows];
  }


// See if position is in the selection, and the selection is non-empty
FXbool FXText::isPosSelected(FXint pos) const {
  return selstartpos<selendpos && selstartpos<=pos && pos<=selendpos;
  }


// Find line number from visible pos
FXint FXText::posToLine(FXint pos,FXint ln) const {
  FXASSERT(0<=ln && ln<nvisrows);
  FXASSERT(visrows[ln]<=pos && pos<=visrows[nvisrows]);
  while(ln<nvisrows-1 && visrows[ln+1]<=pos && visrows[ln]<visrows[ln+1]) ln++;
  FXASSERT(0<=ln && ln<nvisrows);
  FXASSERT(visrows[ln]<=pos && pos<=visrows[ln+1]);
  return ln;
  }


// Localize position at x,y
FXint FXText::getPosAt(FXint x,FXint y) const {
  register FXint row,ls,le,cx,cw,ch;
  y=y-pos_y-margintop;
  row=y/font->getFontHeight();
  if(row<0) return 0;               // Before first row
  if(row>=nrows) return length;     // Below last row
  if(row<toprow){                   // Above visible area
    ls=prevRow(toppos,toprow-row);
    le=nextRow(ls,1);
    }
  else if(row>=toprow+nvisrows){    // Below visible area
    ls=nextRow(toppos,row-toprow);
    le=nextRow(ls,1);
    }
  else{                             // Inside visible area
    ls=visrows[row-toprow];
    le=visrows[row-toprow+1];
    }
  x=x-pos_x-marginleft-barwidth;    // Before begin of line
  if(x<0) return ls;
  FXASSERT(0<=ls);
  FXASSERT(ls<=le);
  FXASSERT(le<=length);
  if(ls<le && (((ch=getByte(le-1))=='\n') || (le<length && Ascii::isSpace(ch)))) le--;
  cx=0;
  while(ls<le){
    ch=getChar(ls);
    cw=charWidth(ch,cx);
    if(x<=(cx+(cw>>1))) return ls;
    cx+=cw;
    ls+=getCharLen(ls);
    }
  return le;
  }


// Determine Y from position pos
FXint FXText::getYOfPos(FXint pos) const {
  register FXint h=font->getFontHeight();
  register FXint n,y;
  if(pos>length) pos=length;
  if(pos<0) pos=0;

  // Above visible part of buffer
  if(pos<visrows[0]){
    n=countRows(rowStart(pos),visrows[0]);
    y=(toprow-n)*h;
    FXTRACE((150,"getYOfPos(%d < visrows[0]=%d) = %d\n",pos,visrows[0],margintop+y));
    }

  // Below visible part of buffer
  else if(pos>=visrows[nvisrows]){
    n=countRows(visrows[nvisrows-1],pos);
    y=(toprow+nvisrows-1+n)*h;
    FXTRACE((150,"getYOfPos(%d > visrows[%d]=%d) = %d\n",pos,nvisrows,visrows[nvisrows],margintop+y));
    }

  // In visible part of buffer
  else{
    n=posToLine(pos,0);
    y=(toprow+n)*h;
    FXTRACE((150,"getYOfPos(visrows[0]=%d <= %d <= visrows[%d]=%d) = %d\n",visrows[0],pos,nvisrows,visrows[nvisrows],margintop+y));
    }
  return margintop+y;
  }


// Calculate X position of pos
FXint FXText::getXOfPos(FXint pos) const {
  register FXint base=rowStart(pos);
  return marginleft+barwidth+lineWidth(base,pos-base);
  }


// Force position to become fully visible
void FXText::makePositionVisible(FXint pos){
  register FXint x,y,nx,ny;

  // Valid position
  pos=validPos(pos);

  // Get coordinates of position
  x=getXOfPos(pos);
  y=getYOfPos(pos);

  // Old scroll position
  ny=pos_y;
  nx=pos_x;

  // Check vertical visibility
  if(pos_y+y<margintop){
    ny=margintop-y;
    nx=0;
    }
  else if(pos_y+y+font->getFontHeight()>viewport_h-marginbottom){
    ny=viewport_h-font->getFontHeight()-marginbottom-y;
    nx=0;
    }

  // Check Horizontal visibility
  if(pos_x+x<marginleft+barwidth){
    nx=marginleft+barwidth-x;
    }
  else if(pos_x+x>viewport_w-marginright){
    nx=viewport_w-marginright-x;
    }

  // If needed, scroll
  if(nx!=pos_x || ny!=pos_y){
    setPosition(nx,ny);
    }
  }


// Return TRUE if position is visible
FXbool FXText::isPosVisible(FXint pos) const {
  if(visrows[0]<=pos && pos<=visrows[nvisrows]){
    register FXint h=font->getFontHeight();
    register FXint y=pos_y+margintop+(toprow+posToLine(pos,0))*h;
    return margintop<=y && y+h<=viewport_h-marginbottom;
    }
  return FALSE;
  }


// Make line containing pos the top visible line
void FXText::setTopLine(FXint pos){
  setPosition(pos_x,margintop-getYOfPos(pos));
  }


// Make line containing pos the bottom visible line
void FXText::setBottomLine(FXint pos){
  setPosition(pos_x,viewport_h-font->getFontHeight()-marginbottom-getYOfPos(pos));
  }


// Center line of pos in the middle of the screen
void FXText::setCenterLine(FXint pos){
  setPosition(pos_x,viewport_h/2+font->getFontHeight()/2-getYOfPos(pos));
  }


// Get top line
FXint FXText::getTopLine() const {
  return visrows[0];
  }


// Get bottom line
FXint FXText::getBottomLine() const {
  return visrows[nvisrows-1];
  }


// Move content
void FXText::moveContents(FXint x,FXint y){
  register FXint delta,i,dx,dy;

  // Erase fragments of cursor overhanging margins
  eraseCursorOverhang();

  // Number of lines scrolled
  delta=-y/font->getFontHeight() - toprow;

  // Scrolled up one or more lines
  if(delta<0){
    if(toprow+delta<=0){
      toppos=0;
      toprow=0;
      }
    else{
      toppos=prevRow(toppos,-delta);
      toprow=toprow+delta;
      }
    if(-delta<nvisrows){
      for(i=nvisrows; i>=-delta; i--) visrows[i]=visrows[delta+i];
      calcVisRows(0,-delta);
      }
    else{
      calcVisRows(0,nvisrows);
      }
    }

  // Scrolled down one or more lines
  else if(delta>0){
    if(toprow+delta>=nrows-1){
      toppos=rowStart(length);
      toprow=nrows-1;
      }
    else{
      toppos=nextRow(toppos,delta);
      toprow=toprow+delta;
      }
    if(delta<nvisrows){
      for(i=0; i<=nvisrows-delta; i++) visrows[i]=visrows[delta+i];
      calcVisRows(nvisrows-delta,nvisrows);
      }
    else{
      calcVisRows(0,nvisrows);
      }
    }

  // This is now the new keep position
  keeppos=toppos;               // FIXME but bottom line for log mode

  // Hopefully, all is still in range
  FXASSERT(0<=toprow && toprow<=nrows-1);
  FXASSERT(0<=toppos && toppos<=length);

  // Scroll the contents
  dx=x-pos_x;
  dy=y-pos_y;
  pos_x=x;
  pos_y=y;

  // Scroll stuff in the bar only vertically
  scroll(0,0,barwidth,viewport_h,0,dy);

  // Scroll the text
  scroll(marginleft+barwidth,margintop,viewport_w-marginleft-barwidth-marginright,viewport_h-margintop-marginbottom,dx,dy);
  }


/*******************************************************************************/


// Recalculate line starts
void FXText::calcVisRows(FXint startline,FXint endline){
  register FXint line,pos;
  FXASSERT(nvisrows>0);
  if(startline<0)
    startline=0;
  else if(startline>nvisrows)
    startline=nvisrows;
  if(endline<0)
    endline=0;
  else if(endline>nvisrows)
    endline=nvisrows;
  if(startline<=endline){
    if(startline==0){
      FXASSERT(0<=toppos && toppos<=length);
      visrows[0]=toppos;
      startline=1;
      }
    pos=visrows[startline-1];
    line=startline;
    if(options&TEXT_WORDWRAP){
      while(line<=endline && pos<length){
        pos=wrap(pos);
        FXASSERT(0<=pos && pos<=length);
        visrows[line++]=pos;
        }
      }
    else{
      while(line<=endline && pos<length){
        pos=nextLine(pos);
        FXASSERT(0<=pos && pos<=length);
        visrows[line++]=pos;
        }
      }
    while(line<=endline){
      visrows[line++]=length;
      }
    }
  }


// FIXME
// when TEXT_AUTOSCROLL is on, we need to anchor text buffer changes to the
// last line of the buffer [if scrolled to the end].
// This will affect mutation() and perhaps replace() functions below...

// There has been a mutation in the buffer
void FXText::mutation(FXint pos,FXint ncins,FXint ncdel,FXint nrins,FXint nrdel){
  register FXint ncdelta=ncins-ncdel;
  register FXint nrdelta=nrins-nrdel;
  register FXint line,i,x,y;

  FXTRACE((150,"BEFORE: pos=%d ncins=%d ncdel=%d nrins=%d nrdel=%d toppos=%d toprow=%d nrows=%d nvisrows=%d\n",pos,ncins,ncdel,nrins,nrdel,toppos,toprow,nrows,nvisrows));

  // All of the change is below the last visible line
  if(visrows[nvisrows]<pos){
    FXTRACE((150,"change below visible\n"));
    nrows+=nrdelta;
    }

  // All change above first visible line
  else if(pos+ncdel<=visrows[0]){
    FXTRACE((150,"change above visible\n"));
    nrows+=nrdelta;
    toprow+=nrdelta;
    toppos+=ncdelta;
    keeppos=toppos;
    for(i=0; i<=nvisrows; i++) visrows[i]+=ncdelta;
    pos_y-=nrdelta*font->getFontHeight();
    FXASSERT(0<=toppos && toppos<=length);
    if(nrdelta) update(0,0,barwidth,height);
    }

  // Top visible part unchanged
  else if(visrows[0]<=pos){
    line=posToLine(pos,0);
    FXTRACE((150,"change below visible line %d\n",line));

    // More lines means paint the bottom half
    if(nrdelta>0){
      FXTRACE((150,"inserted %d rows\n",nrdelta));
      nrows+=nrdelta;
      for(i=nvisrows; i>line+nrdelta; i--) visrows[i]=visrows[i-nrdelta]+ncdelta;
      calcVisRows(line+1,line+nrins);
      FXASSERT(0<=toppos && toppos<=length);
      y=pos_y+margintop+(toprow+line)*font->getFontHeight();
      update(barwidth,y,width-barwidth,height-y);
      }

    // Less lines means paint bottom half also
    else if(nrdelta<0){
      FXTRACE((150,"deleted %d rows\n",-nrdelta));
      nrows+=nrdelta;
      for(i=line+1; i<=nvisrows+nrdelta; i++) visrows[i]=visrows[i-nrdelta]+ncdelta;
      calcVisRows(nvisrows+nrdelta,nvisrows);
      calcVisRows(line+1,line+nrins);
      FXASSERT(0<=toppos && toppos<=length);
      y=pos_y+margintop+(toprow+line)*font->getFontHeight();
      update(barwidth,y,width-barwidth,height-y);
      }

    // Same lines means paint the changed area only
    else{
      FXTRACE((150,"same number of rows\n"));
      for(i=line+1; i<=nvisrows; i++) visrows[i]=visrows[i]+ncdelta;
      calcVisRows(line+1,line+nrins);
      FXASSERT(0<=toppos && toppos<=length);
      if(nrins==0){
        x=pos_x+marginleft+barwidth+lineWidth(visrows[line],pos-visrows[line]);
        y=pos_y+margintop+(toprow+line)*font->getFontHeight();
        update(x,y,width-x,font->getFontHeight());
        FXTRACE((150,"update(%d,%d,%d,%d)\n",x,y,width-x,font->getFontHeight()));
        }
      else{
        y=pos_y+margintop+(toprow+line)*font->getFontHeight();
        update(barwidth,y,width-barwidth,nrins*font->getFontHeight());
        FXTRACE((150,"update(%d,%d,%d,%d)\n",0,y,width,nrins*font->getFontHeight()));
        }
      }
    }

  // Bottom visible part unchanged
  else if(pos+ncdel<visrows[nvisrows-1]){
    nrows+=nrdelta;
    line=1+posToLine(pos+ncdel,0);
    FXASSERT(0<=line && line<nvisrows);
    FXASSERT(pos+ncdel<=visrows[line]);
    FXTRACE((150,"change above visible line %d\n",line));

    // Too few lines left to display
    if(toprow+nrdelta<=line){
      FXTRACE((150,"reset to top\n"));
      toprow=0;
      toppos=0;
      keeppos=0;
      pos_y=0;
      calcVisRows(0,nvisrows);
      FXASSERT(0<=toppos && toppos<=length);
      update();
      }

    // Redisplay only the top
    else{
      FXTRACE((150,"redraw top %d lines\n",line));
      toprow+=nrdelta;
      toppos=prevRow(visrows[line]+ncdelta,line);
      keeppos=toppos;
      pos_y-=nrdelta*font->getFontHeight();
      calcVisRows(0,nvisrows);
      FXASSERT(0<=toppos && toppos<=length);
      update(barwidth,0,width-barwidth,pos_y+margintop+(toprow+line)*font->getFontHeight());
      if(nrdelta) update(0,0,barwidth,height);
      }
    }

  // All visible text changed
  else{
    FXTRACE((150,"change all visible lines\n"));
    nrows+=nrdelta;

    // Reset to top because too few lines left
    if(toprow>=nrows){
      FXTRACE((150,"reset to top\n"));
      toprow=0;
      toppos=0;
      keeppos=0;
      FXASSERT(0<=toppos && toppos<=length);
      pos_y=0;
      }

    // Maintain same row as before
    else{
      FXTRACE((150,"set to same row %d\n",toprow));
      toppos=nextRow(0,toprow);
      keeppos=toppos;
      FXASSERT(0<=toppos && toppos<=length);
      }
    calcVisRows(0,nvisrows);
    update();
    }

  FXTRACE((150,"AFTER : pos=%d ncins=%d ncdel=%d nrins=%d nrdel=%d toppos=%d toprow=%d nrows=%d\n",pos,ncins,ncdel,nrins,nrdel,toppos,toprow,nrows));
  }



// Replace m characters at pos by n characters
void FXText::replace(FXint pos,FXint m,const FXchar *text,FXint n,FXint style){
  register FXint nrdel,nrins,ncdel,ncins,wbeg,wend,del;
  FXint wdel,hdel,wins,hins;
  drawCursor(0);    // FIXME can we do without this?

  FXTRACE((150,"pos=%d mdel=%d nins=%d\n",pos,m,n));

  // Delta in characters
  del=n-m;

  // Bracket potentially affected character range for wrapping purposes
  wbeg=changeBeg(pos);
  wend=changeEnd(pos+m);

  // Measure stuff prior to change
  nrdel=measureText(wbeg,wend,wdel,hdel);
  ncdel=wend-wbeg;

  FXTRACE((150,"wbeg=%d wend=%d nrdel=%d ncdel=%d length=%d wdel=%d hdel=%d\n",wbeg,wend,nrdel,ncdel,length,wdel,hdel));

  // Modify the buffer
  sizegap(del);
  movegap(pos);
  memcpy(&buffer[pos],text,n);
  if(sbuffer){memset(&sbuffer[pos],style,n);}
  gapstart+=n;
  gapend+=m;
  length+=del;

  // Measure stuff after change
  nrins=measureText(wbeg,wend+n-m,wins,hins);
  ncins=wend+n-m-wbeg;

  FXTRACE((150,"wbeg=%d wend+n-m=%d nrins=%d ncins=%d length=%d wins=%d hins=%d\n",wbeg,wend+n-m,nrins,ncins,length,wins,hins));

  // Update stuff
  mutation(wbeg,ncins,ncdel,nrins,nrdel);

  // Fix text metrics
  textHeight=textHeight+hins-hdel;
  textWidth=FXMAX(textWidth,wins);

  // Fix selection range
  FXASSERT(selstartpos<=selendpos);
  if(pos+m<=selstartpos){
    selstartpos+=del;
    selendpos+=del;
    }
  else if(pos<selendpos){
    if(selendpos<=pos+m) selendpos=pos+n; else selendpos+=del;
    if(pos<=selstartpos) selstartpos=pos+n;
    }

  // Fix highlight range
  FXASSERT(hilitestartpos<=hiliteendpos);
  if(pos+m<=hilitestartpos){
    hilitestartpos+=del;
    hiliteendpos+=del;
    }
  else if(pos<hiliteendpos){
    if(hiliteendpos<=pos+m) hiliteendpos=pos+n; else hiliteendpos+=del;
    if(pos<=hilitestartpos) hilitestartpos=pos+n;
    }

  // Fix anchor position
  if(pos+m<=anchorpos) anchorpos+=del;
  else if(pos<=anchorpos) anchorpos=pos+n;

  // Cursor is beyond changed area, so simple update
  if(wend<=cursorpos){
    cursorpos+=del;
    cursorstart+=del;
    cursorend+=del;
    cursorrow+=nrins-nrdel;
    }

  // Cursor inside changed area, recompute cursor data
  else if(wbeg<=cursorpos){
    if(pos+m<=cursorpos) cursorpos+=del;                // Beyond changed text
    else if(pos<=cursorpos) cursorpos=pos+n;            // Inside changed text
    cursorstart=rowStart(cursorpos);
    cursorend=nextRow(cursorstart);
    cursorcol=indentFromPos(cursorstart,cursorpos);
    if(cursorstart<toppos){
      cursorrow=toprow-countRows(cursorstart,toppos);
      }
    else{
      cursorrow=toprow+countRows(toppos,cursorstart);
      }
    }

  // Reconcile scrollbars
  FXScrollArea::layout();     // FIXME:- scrollbars, but no layout

  // Forget preferred column
  prefcol=-1;
  }


// Replace m characters at pos by n characters
void FXText::replaceStyledText(FXint pos,FXint m,const FXchar *text,FXint n,FXint style,FXbool notify){
  FXTextChange textchange;
  if(n<0 || m<0 || pos<0 || length<pos+m){ fxerror("%s::replaceStyledText: bad argument.\n",getClassName()); }
  FXTRACE((130,"replaceStyledText(%d,%d,text,%d)\n",pos,m,n));
  textchange.pos=pos;
  textchange.ndel=m;
  textchange.nins=n;
  textchange.ins=(FXchar*)text;
  FXMALLOC(&textchange.del,FXchar,m);
  extractText(textchange.del,pos,m);
  replace(pos,m,text,n,style);
  if(notify && target){
    target->tryHandle(this,FXSEL(SEL_REPLACED,message),(void*)&textchange);
    target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)cursorpos);
    }
  FXFREE(&textchange.del);
  }


// Replace m characters at pos by n characters
void FXText::replaceStyledText(FXint pos,FXint m,const FXString& text,FXint style,FXbool notify){
  replaceStyledText(pos,m,text.text(),text.length(),style,notify);
  }


// Replace text by other text
void FXText::replaceText(FXint pos,FXint m,const FXchar *text,FXint n,FXbool notify){
  replaceStyledText(pos,m,text,n,0,notify);
  }


// Replace text by other text
void FXText::replaceText(FXint pos,FXint m,const FXString& text,FXbool notify){
  replaceText(pos,m,text.text(),text.length(),notify);
  }


// Add text at the end
void FXText::appendStyledText(const FXchar *text,FXint n,FXint style,FXbool notify){
  FXTextChange textchange;
  if(n<0){ fxerror("%s::appendStyledText: bad argument.\n",getClassName()); }
  FXTRACE((130,"appendStyledText(text,%d)\n",n));
  textchange.pos=length;
  textchange.ndel=0;
  textchange.nins=n;
  textchange.ins=(FXchar*)text;
  textchange.del=(FXchar*)"";
  replace(length,0,text,n,style);
  if(notify && target){
    target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)&textchange);
    target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)cursorpos);
    }
  }


// Add text at the end
void FXText::appendStyledText(const FXString& text,FXint style,FXbool notify){
  appendStyledText(text.text(),text.length(),style,notify);
  }


// Add text at the end
void FXText::appendText(const FXchar *text,FXint n,FXbool notify){
  appendStyledText(text,n,0,notify);
  }


// Add text at the end
void FXText::appendText(const FXString& text,FXbool notify){
  appendText(text.text(),text.length(),notify);
  }


// Insert some text at pos
void FXText::insertStyledText(FXint pos,const FXchar *text,FXint n,FXint style,FXbool notify){
  FXTextChange textchange;
  if(n<0 || pos<0 || length<pos){ fxerror("%s::insertStyledText: bad argument.\n",getClassName()); }
  FXTRACE((130,"insertStyledText(%d,text,%d)\n",pos,n));
  textchange.pos=pos;
  textchange.ndel=0;
  textchange.nins=n;
  textchange.ins=(FXchar*)text;
  textchange.del=(FXchar*)"";
  replace(pos,0,text,n,style);
  if(notify && target){
    target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)&textchange);
    target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)cursorpos);
    }
  }


// Insert some text at pos
void FXText::insertStyledText(FXint pos,const FXString& text,FXint style,FXbool notify){
  insertStyledText(pos,text.text(),text.length(),style,notify);
  }


// Insert some text at pos
void FXText::insertText(FXint pos,const FXchar *text,FXint n,FXbool notify){
  insertStyledText(pos,text,n,0,notify);
  }


// Insert some text at pos
void FXText::insertText(FXint pos,const FXString& text,FXbool notify){
  insertText(pos,text.text(),text.length(),notify);
  }


// Remove some text at pos
void FXText::removeText(FXint pos,FXint n,FXbool notify){
  FXTextChange textchange;
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::removeText: bad argument.\n",getClassName()); }
  FXTRACE((130,"removeText(%d,%d)\n",pos,n));
  textchange.pos=pos;
  textchange.ndel=n;
  textchange.nins=0;
  textchange.ins=(FXchar*)"";
  FXMALLOC(&textchange.del,FXchar,n);
  extractText(textchange.del,pos,n);
  replace(pos,n,NULL,0,0);
  if(notify && target){
    target->tryHandle(this,FXSEL(SEL_DELETED,message),(void*)&textchange);
    target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)cursorpos);
    }
  FXFREE(&textchange.del);
  }


// Grab range of text
void FXText::extractText(FXchar *text,FXint pos,FXint n) const {
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::extractText: bad argument.\n",getClassName()); }
  FXASSERT(0<=n && 0<=pos && pos+n<=length);
  if(pos+n<=gapstart){
    memcpy(text,&buffer[pos],n);
    }
  else if(pos>=gapstart){
    memcpy(text,&buffer[pos-gapstart+gapend],n);
    }
  else{
    memcpy(text,&buffer[pos],gapstart-pos);
    memcpy(&text[gapstart-pos],&buffer[gapend],pos+n-gapstart);
    }
  }


// Grab range of text
void FXText::extractText(FXString& text,FXint pos,FXint n) const {
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::extractText: bad argument.\n",getClassName()); }
  FXASSERT(0<=n && 0<=pos && pos+n<=length);
  text.length(n);
  if(pos+n<=gapstart){
    text.replace(0,n,&buffer[pos],n);
    }
  else if(pos>=gapstart){
    text.replace(0,n,&buffer[pos-gapstart+gapend],n);
    }
  else{
    text.replace(0,gapstart-pos,&buffer[pos],gapstart-pos);
    text.replace(gapstart-pos,pos+n-gapstart,&buffer[gapend],pos+n-gapstart);
    }
  }


// Grab range of style
void FXText::extractStyle(FXchar *style,FXint pos,FXint n) const {
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::extractStyle: bad argument.\n",getClassName()); }
  FXASSERT(0<=n && 0<=pos && pos+n<=length);
  if(sbuffer){
    if(pos+n<=gapstart){
      memcpy(style,&sbuffer[pos],n);
      }
    else if(pos>=gapstart){
      memcpy(style,&sbuffer[pos-gapstart+gapend],n);
      }
    else{
      memcpy(style,&sbuffer[pos],gapstart-pos);
      memcpy(&style[gapstart-pos],&sbuffer[gapend],pos+n-gapstart);
      }
    }
  }


// Grab range of style
void FXText::extractStyle(FXString& style,FXint pos,FXint n) const {
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::extractStyle: bad argument.\n",getClassName()); }
  FXASSERT(0<=n && 0<=pos && pos+n<=length);
  style.assign('\0',n);
  if(sbuffer){
    if(pos+n<=gapstart){
      style.replace(0,n,&sbuffer[pos],n);
      }
    else if(pos>=gapstart){
      style.replace(0,n,&sbuffer[pos-gapstart+gapend],n);
      }
    else{
      style.replace(0,gapstart-pos,&sbuffer[pos],gapstart-pos);
      style.replace(gapstart-pos,pos+n-gapstart,&sbuffer[gapend],pos+n-gapstart);
      }
    }
  }


// Change style of text range
void FXText::changeStyle(FXint pos,FXint n,FXint style){
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::changeStyle: bad argument.\n",getClassName()); }
  if(sbuffer){
    if(pos+n<=gapstart){
      memset(&sbuffer[pos],style,n);
      }
    else if(pos>=gapstart){
      memset(&sbuffer[pos-gapstart+gapend],style,n);
      }
    else{
      memset(&sbuffer[pos],style,gapstart-pos);
      memset(&sbuffer[gapend],style,pos+n-gapstart);
      }
    updateRange(pos,pos+n);
    }
  }


// Change style of text range from style-array
void FXText::changeStyle(FXint pos,const FXchar* style,FXint n){
  if(n<0 || pos<0 || length<pos+n){ fxerror("%s::changeStyle: bad argument.\n",getClassName()); }
  if(sbuffer && style){
    if(pos+n<=gapstart){
      memcpy(&sbuffer[pos],style,n);
      }
    else if(pos>=gapstart){
      memcpy(&sbuffer[pos-gapstart+gapend],style,n);
      }
    else{
      memcpy(&sbuffer[pos],style,gapstart-pos);
      memcpy(&sbuffer[gapend],&style[gapstart-pos],pos+n-gapstart);
      }
    updateRange(pos,pos+n);
    }
  }


// Change style of text range from style-array
void FXText::changeStyle(FXint pos,const FXString& style){
  changeStyle(pos,style.text(),style.length());
  }


// Change the text in the buffer to new text
void FXText::setStyledText(const FXchar* text,FXint n,FXint style,FXbool notify){
  FXTextChange textchange;
  if(n<0){ fxerror("%s::setStyledText: bad argument.\n",getClassName()); }
  if(!FXRESIZE(&buffer,FXchar,n+MINSIZE)){
    fxerror("%s::setStyledText: out of memory.\n",getClassName());
    }
  memcpy(buffer,text,n);
  if(sbuffer){
    if(!FXRESIZE(&sbuffer,FXchar,n+MINSIZE)){
      fxerror("%s::setStyledText: out of memory.\n",getClassName());
      }
    memset(sbuffer,style,n);
    }
  gapstart=n;
  gapend=gapstart+MINSIZE;
  length=n;
  toppos=0;
  toprow=0;
  keeppos=0;
  selstartpos=0;
  selendpos=0;
  hilitestartpos=0;
  hiliteendpos=0;
  anchorpos=0;
  cursorpos=0;
  cursorstart=0;
  cursorend=0;
  cursorrow=0;
  cursorcol=0;
  prefcol=-1;
  pos_x=0;
  pos_y=0;
  textchange.pos=0;
  textchange.ndel=0;
  textchange.nins=n;
  textchange.ins=(FXchar*)text;
  textchange.del=(FXchar*)"";
  if(notify && target){
    target->tryHandle(this,FXSEL(SEL_INSERTED,message),(void*)&textchange);
    target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)cursorpos);
    }
  recalc();
  layout();
  update();
  }


// Change all of the text
void FXText::setStyledText(const FXString& text,FXint style,FXbool notify){
  setStyledText(text.text(),text.length(),style,notify);
  }


// Change the text in the buffer to new text
void FXText::setText(const FXchar* text,FXint n,FXbool notify){
  setStyledText(text,n,0,notify);
  }


// Change all of the text
void FXText::setText(const FXString& text,FXbool notify){
  setText(text.text(),text.length(),notify);
  }


// Retrieve text into buffer
void FXText::getText(FXchar* text,FXint n) const {
  extractText(text,0,n);
  }


// Retrieve text into buffer
void FXText::getText(FXString& text) const {
  extractText(text,0,getLength());
  }


// We return a constant copy of the buffer
FXString FXText::getText() const {
  FXString value;
  FXASSERT(0<=gapstart && gapstart<=length);
  value.append(buffer,gapstart);
  value.append(&buffer[gapend],length-gapstart);
  return value;
  }


// Completely reflow the text, because font, wrapwidth, or all of the
// text may have changed and everything needs to be recomputed
void FXText::recompute(){
  FXint ww1,ww2,ww3,hh1,hh2,hh3,hh;

  // Make it point somewhere sensible
  if(keeppos<0) keeppos=0;
  if(keeppos>length) keeppos=length;

  // Make sure we're pointing to the start of a row again
  toppos=rowStart(keeppos);           // FIXME in log mode, we may want to keep bottom line anchored [if visible]

  // Font height
  hh=font->getFontHeight();

  // Get start
  cursorstart=rowStart(cursorpos);
  cursorend=nextRow(cursorstart);
  cursorcol=indentFromPos(cursorstart,cursorpos);

  // Avoid measuring huge chunks of text twice!
  if(cursorstart<toprow){
    cursorrow=measureText(0,cursorstart,ww1,hh1);
    toprow=cursorrow+measureText(cursorstart,toppos,ww2,hh2);
    nrows=toprow+measureText(toppos,length+1,ww3,hh3);
    }
  else{
    toprow=measureText(0,toppos,ww1,hh1);
    cursorrow=toprow+measureText(toppos,cursorstart,ww2,hh2);
    nrows=cursorrow+measureText(cursorstart,length+1,ww3,hh3);
    }

  textWidth=FXMAX3(ww1,ww2,ww3);
  textHeight=hh1+hh2+hh3;

  // Adjust position, keeping same fractional position
  pos_y=-toprow*hh-(-pos_y%hh);

  // Number of visible lines has changed
  nvisrows=(height-margintop-marginbottom+hh+hh-1)/hh;
  if(nvisrows<1) nvisrows=1;

  // Resize line start array
  FXRESIZE(&visrows,FXint,nvisrows+1);

  // Recompute line starts
  calcVisRows(0,nvisrows);

  FXTRACE((150,"recompute : toprow=%d toppos=%d nrows=%d nvisrows=%d textWidth=%d textHeight=%d length=%d cursorrow=%d cursorcol=%d\n",toprow,toppos,nrows,nvisrows,textWidth,textHeight,length,cursorrow,cursorcol));

  // Done with that
  flags&=~FLAG_RECALC;
  }


/*******************************************************************************/


// Determine content width of scroll area
FXint FXText::getContentWidth(){
  if(flags&FLAG_RECALC) recompute();
  return marginleft+barwidth+marginright+textWidth;
  }


// Determine content height of scroll area
FXint FXText::getContentHeight(){
  if(flags&FLAG_RECALC) recompute();
  return margintop+marginbottom+textHeight;
  }


// Recalculate layout
void FXText::layout(){
  FXint fh=font->getFontHeight();
  FXint fw=font->getFontWidth();
  FXint ovv=nvisrows;
  FXint oww=wrapwidth;

  // Compute new wrap width; needed to reflow text
  if(options&TEXT_FIXEDWRAP){
    wrapwidth=wrapcolumns*font->getTextWidth("x",1);
    }
  else{
    wrapwidth=width-marginleft-barwidth-marginright;
    if(!(options&VSCROLLER_NEVER)) wrapwidth-=vertical->getDefaultWidth();
    }

  // Wrap width changed, so reflow; when using fixed pitch font,
  // we only reflow if the number of columns has changed.
  if((options&TEXT_WORDWRAP) && (wrapwidth!=oww)){
    if(!font->isFontMono() || (wrapwidth/fw!=oww/fw)) flags|=FLAG_RECALC;
    }

  // Scrollbars adjusted
  FXScrollArea::layout();

  // Number of visible lines may have changed
  nvisrows=(height-margintop-marginbottom+fh+fh-1)/fh;
  if(nvisrows<1) nvisrows=1;

  // Number of visible lines changed
  if(nvisrows!=ovv){

    // Resize line start array
    FXRESIZE(&visrows,FXint,nvisrows+1);

    // Recompute line starts
    calcVisRows(0,nvisrows);
    }

  // Set line size based on font
  vertical->setLine(fh);
  horizontal->setLine(fw);

  // Force repaint
  update();

  // Done
  flags&=~FLAG_DIRTY;
  }


/*******************************************************************************/


// Blink the cursor
long FXText::onBlink(FXObject*,FXSelector,void*){
  drawCursor(flags^FLAG_CARET);
  getApp()->addTimeout(this,ID_BLINK,getApp()->getBlinkSpeed());
  return 0;
  }


// Gained focus
long FXText::onFocusIn(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusIn(sender,sel,ptr);
  getApp()->addTimeout(this,ID_BLINK,getApp()->getBlinkSpeed());
  drawCursor(FLAG_CARET);
  return 1;
  }


// Lost focus
long FXText::onFocusOut(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onFocusOut(sender,sel,ptr);
  getApp()->removeTimeout(this,ID_BLINK);
  drawCursor(0);
  flags|=FLAG_UPDATE;
  return 1;
  }


// We were asked about tip text
long FXText::onQueryTip(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryTip(sender,sel,ptr)) return 1;
  if((flags&FLAG_TIP) && !tip.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&tip);
    return 1;
    }
  return 0;
  }


// We were asked about status text
long FXText::onQueryHelp(FXObject* sender,FXSelector sel,void* ptr){
  if(FXWindow::onQueryHelp(sender,sel,ptr)) return 1;
  if((flags&FLAG_HELP) && !help.empty()){
    sender->handle(this,FXSEL(SEL_COMMAND,ID_SETSTRINGVALUE),(void*)&help);
    return 1;
    }
  return 0;
  }


// Flash matching brace
long FXText::onFlash(FXObject*,FXSelector,void*){
  killHighlight();
  return 0;
  }


// Pressed left button
long FXText::onLeftBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint pos;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;

    // Select characters
    if(event->click_count==1){
      pos=getPosAt(event->win_x,event->win_y);
      FXTRACE((150,"getPosAt(%d,%d) = %d getYOfPos(%d) = %d getXOfPos(%d)=%d\n",event->win_x,event->win_y,pos,pos,getYOfPos(pos),pos,getXOfPos(pos)));
      setCursorPos(pos,TRUE);
      makePositionVisible(cursorpos);
      if(event->state&SHIFTMASK){
        extendSelection(cursorpos,SELECT_CHARS,TRUE);
        }
      else{
        killSelection(TRUE);
        setAnchorPos(cursorpos);
        flashMatching();
        }
      mode=MOUSE_CHARS;
      }

    // Select words
    else if(event->click_count==2){
      setAnchorPos(cursorpos);
      extendSelection(cursorpos,SELECT_WORDS,TRUE);
      mode=MOUSE_WORDS;
      }

    // Select lines
    else{
      setAnchorPos(cursorpos);
      extendSelection(cursorpos,SELECT_LINES,TRUE);
      mode=MOUSE_LINES;
      }
    return 1;
    }
  return 0;
  }


// Released left button
long FXText::onLeftBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    ungrab();
    mode=MOUSE_NONE;
    stopAutoScroll();
    if(target && target->tryHandle(this,FXSEL(SEL_LEFTBUTTONRELEASE,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }


// Pressed middle button
long FXText::onMiddleBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint pos;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONPRESS,message),ptr)) return 1;
    pos=getPosAt(event->win_x,event->win_y);

    // Move over
    setCursorPos(pos,TRUE);
    makePositionVisible(cursorpos);

    // Start text drag
    if(isPosSelected(cursorpos)){
      mode=MOUSE_TRYDRAG;
      }
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released middle button
long FXText::onMiddleBtnRelease(FXObject*,FXSelector,void* ptr){
  FXuint md=mode;
  if(isEnabled()){
    ungrab();
    stopAutoScroll();
    mode=MOUSE_NONE;
    if(target && target->tryHandle(this,FXSEL(SEL_MIDDLEBUTTONRELEASE,message),ptr)) return 1;

    // Drop text somewhere
    if(md==MOUSE_DRAG){
      handle(this,FXSEL(SEL_ENDDRAG,0),ptr);
      }

    // Paste selection
    else{
      handle(this,FXSEL(SEL_COMMAND,ID_PASTE_MIDDLE),NULL);
      }
    return 1;
    }
  return 0;
  }


// Pressed right button
long FXText::onRightBtnPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  handle(this,FXSEL(SEL_FOCUS_SELF,0),ptr);
  if(isEnabled()){
    grab();
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONPRESS,message),ptr)) return 1;
    grabx=event->win_x-pos_x;
    graby=event->win_y-pos_y;
    mode=MOUSE_SCROLL;
    flags&=~FLAG_UPDATE;
    return 1;
    }
  return 0;
  }


// Released right button
long FXText::onRightBtnRelease(FXObject*,FXSelector,void* ptr){
  if(isEnabled()){
    ungrab();
    mode=MOUSE_NONE;
    if(target && target->tryHandle(this,FXSEL(SEL_RIGHTBUTTONRELEASE,message),ptr)) return 1;
    return 1;
    }
  return 0;
  }


// The widget lost the grab for some reason
long FXText::onUngrabbed(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onUngrabbed(sender,sel,ptr);
  mode=MOUSE_NONE;
  flags|=FLAG_UPDATE;
  stopAutoScroll();
  return 1;
  }


// Autoscroll timer fired
long FXText::onAutoScroll(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint pos;
  FXScrollArea::onAutoScroll(sender,sel,ptr);
  switch(mode){
    case MOUSE_CHARS:
      if((fxabs(event->win_x-event->click_x)>getApp()->getDragDelta())||(fxabs(event->win_y-event->click_y)>getApp()->getDragDelta())){
        pos=getPosAt(event->win_x,event->win_y);
        setCursorPos(pos,TRUE);
        extendSelection(cursorpos,SELECT_CHARS,TRUE);
        }
      return 1;
    case MOUSE_WORDS:
      if((fxabs(event->win_x-event->click_x)>getApp()->getDragDelta())||(fxabs(event->win_y-event->click_y)>getApp()->getDragDelta())){
        pos=getPosAt(event->win_x,event->win_y);
        setCursorPos(pos,TRUE);
        extendSelection(cursorpos,SELECT_WORDS,TRUE);
        }
      return 1;
    case MOUSE_LINES:
      if((fxabs(event->win_x-event->click_x)>getApp()->getDragDelta())||(fxabs(event->win_y-event->click_y)>getApp()->getDragDelta())){
        pos=getPosAt(event->win_x,event->win_y);
        setCursorPos(pos,TRUE);
        extendSelection(cursorpos,SELECT_LINES,TRUE);
        }
      return 1;
    }
  return 0;
  }


// Handle real or simulated mouse motion
long FXText::onMotion(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXint pos;
  switch(mode){
    case MOUSE_CHARS:
      if(startAutoScroll(event,FALSE)) return 1;
      if((fxabs(event->win_x-event->click_x)>getApp()->getDragDelta())||(fxabs(event->win_y-event->click_y)>getApp()->getDragDelta())){
        pos=getPosAt(event->win_x,event->win_y);
        setCursorPos(pos,TRUE);
        extendSelection(cursorpos,SELECT_CHARS,TRUE);
        }
      return 1;
    case MOUSE_WORDS:
      if(startAutoScroll(event,FALSE)) return 1;
      if((fxabs(event->win_x-event->click_x)>getApp()->getDragDelta())||(fxabs(event->win_y-event->click_y)>getApp()->getDragDelta())){
        pos=getPosAt(event->win_x,event->win_y);
        setCursorPos(pos,TRUE);
        extendSelection(cursorpos,SELECT_WORDS,TRUE);
        }
      return 1;
    case MOUSE_LINES:
      if(startAutoScroll(event,FALSE)) return 1;
      if((fxabs(event->win_x-event->click_x)>getApp()->getDragDelta())||(fxabs(event->win_y-event->click_y)>getApp()->getDragDelta())){
        pos=getPosAt(event->win_x,event->win_y);
        setCursorPos(pos,TRUE);
        extendSelection(cursorpos,SELECT_LINES,TRUE);
        }
      return 1;
    case MOUSE_SCROLL:
      setPosition(event->win_x-grabx,event->win_y-graby);
      return 1;
    case MOUSE_DRAG:
      handle(this,FXSEL(SEL_DRAGGED,0),ptr);
      return 1;
    case MOUSE_TRYDRAG:
      if(event->moved){
        mode=MOUSE_NONE;
        if(handle(this,FXSEL(SEL_BEGINDRAG,0),ptr)){
          mode=MOUSE_DRAG;
          }
        }
      return 1;
    }
  return 0;
  }


/*******************************************************************************/


// Start a drag operation
long FXText::onBeginDrag(FXObject* sender,FXSelector sel,void* ptr){
  FXDragType types[4];
  if(FXScrollArea::onBeginDrag(sender,sel,ptr)) return 1;
  types[0]=stringType;
  types[1]=textType;
  types[2]=utf8Type;
  types[3]=utf16Type;
  beginDrag(types,4);
  setDragCursor(getApp()->getDefaultCursor(DEF_DNDSTOP_CURSOR));
  return 1;
  }


// End drag operation
long FXText::onEndDrag(FXObject* sender,FXSelector sel,void* ptr){
  if(FXScrollArea::onEndDrag(sender,sel,ptr)) return 1;
  endDrag((didAccept()!=DRAG_REJECT));
  setDragCursor(getApp()->getDefaultCursor(DEF_TEXT_CURSOR));
  return 1;
  }


// Dragged stuff around
long FXText::onDragged(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDragAction action;
  if(FXScrollArea::onDragged(sender,sel,ptr)) return 1;
  action=DRAG_COPY;
  if(isEditable()){
    if(isDropTarget()) action=DRAG_MOVE;
    if(event->state&CONTROLMASK) action=DRAG_COPY;
    if(event->state&SHIFTMASK) action=DRAG_MOVE;
    }
  handleDrag(event->root_x,event->root_y,action);
  if(didAccept()!=DRAG_REJECT){
    if(action==DRAG_MOVE)
      setDragCursor(getApp()->getDefaultCursor(DEF_DNDMOVE_CURSOR));
    else
      setDragCursor(getApp()->getDefaultCursor(DEF_DNDCOPY_CURSOR));
    }
  else{
    setDragCursor(getApp()->getDefaultCursor(DEF_DNDSTOP_CURSOR));
    }
  return 1;
  }


// Handle drag-and-drop enter
long FXText::onDNDEnter(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onDNDEnter(sender,sel,ptr);
  drawCursor(FLAG_CARET);
  revertpos=cursorpos;
  return 1;
  }


// Handle drag-and-drop leave
long FXText::onDNDLeave(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onDNDLeave(sender,sel,ptr);
  stopAutoScroll();
  drawCursor(0);
  setCursorPos(revertpos,TRUE);
  return 1;
  }


// Handle drag-and-drop motion
long FXText::onDNDMotion(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent* event=(FXEvent*)ptr;

  // Scroll into view
  if(startAutoScroll(event,TRUE)) return 1;

  // Handled elsewhere
  if(FXScrollArea::onDNDMotion(sender,sel,ptr)) return 1;

  // Correct drop type
  if(offeredDNDType(FROM_DRAGNDROP,textType) || offeredDNDType(FROM_DRAGNDROP,stringType) || offeredDNDType(FROM_DRAGNDROP,utf8Type) || offeredDNDType(FROM_DRAGNDROP,utf16Type)){

    // Is target editable?
    if(isEditable()){
      FXDragAction action=inquireDNDAction();

      // Check for legal DND action
      if(action==DRAG_COPY || action==DRAG_MOVE){

        // Get the suggested drop position
        FXint pos=getPosAt(event->win_x,event->win_y);

        // Move cursor to new position
        setCursorPos(pos,TRUE);
        makePositionVisible(cursorpos);

        // We don't accept a drop on the selection
        if(!isPosSelected(pos)){
          acceptDrop(DRAG_ACCEPT);
          }
        }
      }
    return 1;
    }

  // Didn't handle it here
  return 0;
  }


// Handle drag-and-drop drop
long FXText::onDNDDrop(FXObject* sender,FXSelector sel,void* ptr){

  // Stop scrolling
  stopAutoScroll();
  drawCursor(0);

  // Try handling it in base class first
  if(FXScrollArea::onDNDDrop(sender,sel,ptr)) return 1;

  // Should really not have gotten this if non-editable
  if(isEditable()){
    FXString string;
    FXString junk;

    // First, try UTF-8
    if(getDNDData(FROM_DRAGNDROP,utf8Type,string)){
      FXTRACE((100,"Paste UTF8\n"));
      if(inquireDNDAction()==DRAG_MOVE){
        getDNDData(FROM_DRAGNDROP,deleteType,junk);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)string.text());
      return 1;
      }

    // Next, try UTF-16
    if(getDNDData(FROM_DRAGNDROP,utf16Type,string)){
      FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
      FXTRACE((100,"Paste UTF16\n"));
      if(inquireDNDAction()==DRAG_MOVE){
        getDNDData(FROM_DRAGNDROP,deleteType,junk);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)unicode.mb2utf(string).text());
      return 1;
      }

    // Next, try good old Latin-1
    if(getDNDData(FROM_DRAGNDROP,textType,string)){
      FX88591Codec ascii;
      FXTRACE((100,"Paste ASCII\n"));
      if(inquireDNDAction()==DRAG_MOVE){
        getDNDData(FROM_DRAGNDROP,deleteType,junk);
        }
      handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)ascii.mb2utf(string).text());
      return 1;
      }
    return 1;
    }
  return 0;
  }


// Service requested DND data
long FXText::onDNDRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Perhaps the target wants to supply its own data
  if(FXScrollArea::onDNDRequest(sender,sel,ptr)) return 1;

  // Recognize the request?
  if(event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type){
    FXString string;

    // Get selected fragment
    extractText(string,selstartpos,selendpos-selstartpos);

    // Return text of the selection as UTF-8
    if(event->target==utf8Type){
      FXTRACE((100,"Request UTF8\n"));
      setDNDData(FROM_DRAGNDROP,event->target,string);
      return 1;
      }

    // Return text of the selection translated to 8859-1
    if(event->target==stringType || event->target==textType){
      FX88591Codec ascii;
      FXTRACE((100,"Request ASCII\n"));
      setDNDData(FROM_DRAGNDROP,event->target,ascii.utf2mb(string));
      return 1;
      }

    // Return text of the selection translated to UTF-16
    if(event->target==utf16Type){
      FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
      FXTRACE((100,"Request UTF16\n"));
      setDNDData(FROM_DRAGNDROP,event->target,unicode.utf2mb(string));
      return 1;
      }
    }

  // Delete dragged text, if editable
  if(event->target==deleteType){
    if(isEditable()){
      handle(this,FXSEL(SEL_COMMAND,ID_DELETE_SEL),NULL);
      }
    return 1;
    }

  return 0;
  }


/*******************************************************************************/


// We now really do have the selection
long FXText::onSelectionGained(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onSelectionGained(sender,sel,ptr);
  return 1;
  }


// We lost the selection somehow
long FXText::onSelectionLost(FXObject* sender,FXSelector sel,void* ptr){
  FXint what[2];
  FXScrollArea::onSelectionLost(sender,sel,ptr);
  if(target){
    what[0]=selstartpos;
    what[1]=selendpos-selstartpos;
    target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)what);
    }
  updateRange(selstartpos,selendpos);
  selstartpos=0;
  selendpos=0;
  return 1;
  }


// Somebody wants our selection
long FXText::onSelectionRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Perhaps the target wants to supply its own data for the selection
  if(FXScrollArea::onSelectionRequest(sender,sel,ptr)) return 1;

  // Recognize the request?
  if(event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type){
    FXString string;

    // Get selected fragment
    extractText(string,selstartpos,selendpos-selstartpos);

    // Return text of the selection as UTF-8
    if(event->target==utf8Type){
      FXTRACE((100,"Request UTF8\n"));
      setDNDData(FROM_SELECTION,event->target,string);
      return 1;
      }

    // Return text of the selection translated to 8859-1
    if(event->target==stringType || event->target==textType){
      FX88591Codec ascii;
      FXTRACE((100,"Request ASCII\n"));
      string=ascii.utf2mb(string);
      setDNDData(FROM_SELECTION,event->target,string);
      return 1;
      }

    // Return text of the selection translated to UTF-16
    if(event->target==utf16Type){
      FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
      FXTRACE((100,"Request UTF16\n"));
      string=unicode.utf2mb(string);
      setDNDData(FROM_SELECTION,event->target,string);
      return 1;
      }
    }
  return 0;
  }


/*******************************************************************************/


// We now really do have the selection
long FXText::onClipboardGained(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onClipboardGained(sender,sel,ptr);
  return 1;
  }


// We lost the selection somehow
long FXText::onClipboardLost(FXObject* sender,FXSelector sel,void* ptr){
  FXScrollArea::onClipboardLost(sender,sel,ptr);
  clipped.clear();
  return 1;
  }


// Somebody wants our selection
long FXText::onClipboardRequest(FXObject* sender,FXSelector sel,void* ptr){
  FXEvent *event=(FXEvent*)ptr;

  // Try handling it in base class first
  if(FXScrollArea::onClipboardRequest(sender,sel,ptr)) return 1;

  // Requested data from clipboard
  if(event->target==stringType || event->target==textType || event->target==utf8Type || event->target==utf16Type){
    FXString string=clipped;

    // Expand newlines to CRLF on Windows
#ifdef WIN32
    unixToDos(string);
#endif

    // Return clipped text as as UTF-8
    if(event->target==utf8Type){
      FXTRACE((100,"Request UTF8\n"));
      setDNDData(FROM_CLIPBOARD,event->target,string);
      return 1;
      }

    // Return clipped text translated to 8859-1
    if(event->target==stringType || event->target==textType){
      FX88591Codec ascii;
      FXTRACE((100,"Request ASCII\n"));
      setDNDData(FROM_CLIPBOARD,event->target,ascii.utf2mb(string));
      return 1;
      }

    // Return text of the selection translated to UTF-16
    if(event->target==utf16Type){
      FXUTF16LECodec unicode;             // FIXME maybe other endianness for unix
      FXTRACE((100,"Request UTF16\n"));
      setDNDData(FROM_CLIPBOARD,event->target,unicode.utf2mb(string));
      return 1;
      }
    }
  return 0;
  }


/*******************************************************************************/

// Keyboard press
long FXText::onKeyPress(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  flags&=~FLAG_TIP;
  if(isEnabled()){
    FXTRACE((200,"%s::onKeyPress keysym=0x%04x state=%04x\n",getClassName(),event->code,event->state));
    if(target && target->tryHandle(this,FXSEL(SEL_KEYPRESS,message),ptr)) return 1;
    flags&=~FLAG_UPDATE;
    switch(event->code){
      case KEY_Shift_L:
      case KEY_Shift_R:
      case KEY_Control_L:
      case KEY_Control_R:
        if(mode==MOUSE_DRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
        return 1;
      case KEY_Up:
      case KEY_KP_Up:
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_SCROLL_UP),NULL);
          }
        else{
          if(!(event->state&SHIFTMASK)){
            handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
            }
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_UP),NULL);
          if(event->state&SHIFTMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
            }
          else{
            handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
            }
          }
        return 1;
      case KEY_Down:
      case KEY_KP_Down:
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_SCROLL_DOWN),NULL);
          }
        else{
          if(!(event->state&SHIFTMASK)){
            handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
            }
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_DOWN),NULL);
          if(event->state&SHIFTMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
            }
          else{
            handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
            }
          }
        return 1;
      case KEY_Left:
      case KEY_KP_Left:
        if(!(event->state&SHIFTMASK)){
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          }
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_WORD_LEFT),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_LEFT),NULL);
          }
        if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
          }
        return 1;
      case KEY_Right:
      case KEY_KP_Right:
        if(!(event->state&SHIFTMASK)){
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          }
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_WORD_RIGHT),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_RIGHT),NULL);
          }
        if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
          }
        return 1;
      case KEY_Home:
      case KEY_KP_Home:
        if(!(event->state&SHIFTMASK)){
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          }
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_TOP),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_HOME),NULL);
          }
        if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
          }
        return 1;
      case KEY_End:
      case KEY_KP_End:
        if(!(event->state&SHIFTMASK)){
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          }
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_BOTTOM),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_END),NULL);
          }
        if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
          }
        return 1;
      case KEY_Page_Up:
      case KEY_KP_Page_Up:
        if(!(event->state&SHIFTMASK)){
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          }
        handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_PAGEUP),NULL);
        if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
          }
        return 1;
      case KEY_Page_Down:
      case KEY_KP_Page_Down:
        if(!(event->state&SHIFTMASK)){
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          }
        handle(this,FXSEL(SEL_COMMAND,ID_CURSOR_PAGEDOWN),NULL);
        if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_EXTEND),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_MARK),NULL);
          }
        return 1;
      case KEY_Insert:
      case KEY_KP_Insert:
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_COPY_SEL),NULL);
          }
        else if(event->state&SHIFTMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_PASTE_SEL),NULL);
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_TOGGLE_OVERSTRIKE),NULL);
          }
        return 1;
      case KEY_Delete:
      case KEY_KP_Delete:
        if(isPosSelected(cursorpos)){
          if(event->state&SHIFTMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_CUT_SEL),NULL);
            }
          else{
            handle(this,FXSEL(SEL_COMMAND,ID_DELETE_SEL),NULL);
            }
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
          if(event->state&CONTROLMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_DELETE_WORD),NULL);
            }
          else if(event->state&SHIFTMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_DELETE_EOL),NULL);
            }
          else{
            handle(this,FXSEL(SEL_COMMAND,ID_DELETE),NULL);
            }
          }
        return 1;
      case KEY_BackSpace:
        if(isPosSelected(cursorpos)){
          handle(this,FXSEL(SEL_COMMAND,ID_DELETE_SEL),NULL);
          }
        else{
         handle(this,FXSEL(SEL_COMMAND,ID_DESELECT_ALL),NULL);
         if(event->state&CONTROLMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_BACKSPACE_WORD),NULL);
            }
          else if(event->state&SHIFTMASK){
            handle(this,FXSEL(SEL_COMMAND,ID_BACKSPACE_BOL),NULL);
            }
          else{
            handle(this,FXSEL(SEL_COMMAND,ID_BACKSPACE),NULL);
            }
          }
        return 1;
      case KEY_Return:
      case KEY_KP_Enter:
        handle(this,FXSEL(SEL_COMMAND,ID_INSERT_NEWLINE),NULL);
        return 1;
      case KEY_Tab:
      case KEY_KP_Tab:
        if(event->state&CONTROLMASK){
          handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)"\t");
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_INSERT_TAB),NULL);
          }
        return 1;
      case KEY_a:
        if(!(event->state&CONTROLMASK)) goto ins;
        handle(this,FXSEL(SEL_COMMAND,ID_SELECT_ALL),NULL);
        return 1;
      case KEY_x:
        if(!(event->state&CONTROLMASK)) goto ins;
      case KEY_F20:                               // Sun Cut key
        handle(this,FXSEL(SEL_COMMAND,ID_CUT_SEL),NULL);
        return 1;
      case KEY_c:
        if(!(event->state&CONTROLMASK)) goto ins;
      case KEY_F16:                               // Sun Copy key
        handle(this,FXSEL(SEL_COMMAND,ID_COPY_SEL),NULL);
        return 1;
      case KEY_v:
        if(!(event->state&CONTROLMASK)) goto ins;
      case KEY_F18:                               // Sun Paste key
        handle(this,FXSEL(SEL_COMMAND,ID_PASTE_SEL),NULL);
        return 1;
      default:
ins:    if((event->state&(CONTROLMASK|ALTMASK)) || ((FXuchar)event->text[0]<32)) return 0;
        if(isOverstrike()){
          handle(this,FXSEL(SEL_COMMAND,ID_OVERST_STRING),(void*)event->text.text());
          }
        else{
          handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)event->text.text());
          }
        return 1;
      }
    }
  return 0;
  }


// Keyboard release
long FXText::onKeyRelease(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  if(isEnabled()){
    FXTRACE((200,"%s::onKeyRelease keysym=0x%04x state=%04x\n",getClassName(),event->code,event->state));
    if(target && target->tryHandle(this,FXSEL(SEL_KEYRELEASE,message),ptr)) return 1;
    switch(event->code){
      case KEY_Shift_L:
      case KEY_Shift_R:
      case KEY_Control_L:
      case KEY_Control_R:
        if(mode==MOUSE_DRAG){handle(this,FXSEL(SEL_DRAGGED,0),ptr);}
        return 1;
      }
    }
  return 0;
  }


/*******************************************************************************/

// Move cursor to top of buffer
long FXText::onCmdCursorTop(FXObject*,FXSelector,void*){
  setCursorPos(0,TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Move cursor to bottom of buffer
long FXText::onCmdCursorBottom(FXObject*,FXSelector,void*){
  setCursorPos(length,TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Move cursor to begin of line
long FXText::onCmdCursorHome(FXObject*,FXSelector,void*){
  setCursorPos(rowStart(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Move cursor to end of line
long FXText::onCmdCursorEnd(FXObject*,FXSelector,void*){
  setCursorPos(rowEnd(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Move cursor right
long FXText::onCmdCursorRight(FXObject*,FXSelector,void*){
  if(cursorpos>=length) return 1;
  setCursorPos(inc(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Move cursor left
long FXText::onCmdCursorLeft(FXObject*,FXSelector,void*){
  if(cursorpos<=0) return 1;
  setCursorPos(dec(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Move cursor to previous line
long FXText::onCmdCursorUp(FXObject*,FXSelector,void*){
  FXint newrow,newpos,col;
  col=(0<=prefcol) ? prefcol : cursorcol;
  newrow=prevRow(cursorpos);
  newpos=posFromIndent(newrow,col);
  setCursorPos(newpos,TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  prefcol=col;
  return 1;
  }


// Move cursor to next line
long FXText::onCmdCursorDown(FXObject*,FXSelector,void*){
  FXint newrow,newpos,col;
  col=(0<=prefcol) ? prefcol : cursorcol;
  newrow=nextRow(cursorpos);
  newpos=posFromIndent(newrow,col);
  setCursorPos(newpos,TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  prefcol=col;
  return 1;
  }


// Page down
long FXText::onCmdCursorPageDown(FXObject*,FXSelector,void*){
  FXint newrow,newpos,col;
  col=(0<=prefcol) ? prefcol : cursorcol;
  newrow=nextRow(cursorpos,(viewport_h)/font->getFontHeight());
  newpos=posFromIndent(newrow,col);
  setTopLine(nextRow(toppos,viewport_h/font->getFontHeight()));
  setCursorPos(newpos,TRUE);
  makePositionVisible(cursorpos);
  prefcol=col;
  return 1;
  }


// Page up
long FXText::onCmdCursorPageUp(FXObject*,FXSelector,void*){
  FXint newrow,newpos,col;
  col=(0<=prefcol) ? prefcol : cursorcol;
  newrow=prevRow(cursorpos,(viewport_h)/font->getFontHeight());
  newpos=posFromIndent(newrow,col);
  setTopLine(prevRow(toppos,viewport_h/font->getFontHeight()));
  setCursorPos(newpos,TRUE);
  makePositionVisible(cursorpos);
  prefcol=col;
  return 1;
  }


// Word Left
long FXText::onCmdCursorWordLeft(FXObject*,FXSelector,void*){
  setCursorPos(leftWord(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Word Right
long FXText::onCmdCursorWordRight(FXObject*,FXSelector,void*){
  setCursorPos(rightWord(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Word Start
long FXText::onCmdCursorWordStart(FXObject*,FXSelector,void*){
  setCursorPos(wordStart(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Word End
long FXText::onCmdCursorWordEnd(FXObject*,FXSelector,void*){
  setCursorPos(wordEnd(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  flashMatching();
  return 1;
  }


// Cursor pos to top of screen
long FXText::onCmdCursorScreenTop(FXObject*,FXSelector,void*){
  setTopLine(cursorpos);
  return 1;
  }


// Cursor pos to bottom of screen
long FXText::onCmdCursorScreenBottom(FXObject*,FXSelector,void*){
  setBottomLine(cursorpos);
  return 1;
  }


// Cursor pos to center of screen
long FXText::onCmdCursorScreenCenter(FXObject*,FXSelector,void*){
  setCenterLine(cursorpos);
  return 1;
  }


// Scroll up one line, leaving cursor in place
long FXText::onCmdScrollUp(FXObject*,FXSelector,void*){
  setTopLine(prevRow(toppos,1));
  return 1;
  }


// Scroll down one line, leaving cursor in place
long FXText::onCmdScrollDown(FXObject*,FXSelector,void*){
  setTopLine(nextRow(toppos,1));
  return 1;
  }


// Move cursor to begin of paragraph
long FXText::onCmdCursorParHome(FXObject*,FXSelector,void*){
  setCursorPos(lineStart(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  return 1;
  }


// Move cursor to end of paragraph
long FXText::onCmdCursorParEnd(FXObject*,FXSelector,void*){
  setCursorPos(lineEnd(cursorpos),TRUE);
  makePositionVisible(cursorpos);
  return 1;
  }


// Mark
long FXText::onCmdMark(FXObject*,FXSelector,void*){
  setAnchorPos(cursorpos);
  return 1;
  }


// Extend
long FXText::onCmdExtend(FXObject*,FXSelector,void*){
  extendSelection(cursorpos,SELECT_CHARS,TRUE);
  return 1;
  }


// Overstrike a string
long FXText::onCmdOverstString(FXObject*,FXSelector,void* ptr){
  if(isEditable()){
    FXint sindent,oindent,nindent,pos,ch,reppos,replen;
    FXchar* string=(FXchar*)ptr;
    FXint len=strlen(string);
    if(isPosSelected(cursorpos)){
      reppos=selstartpos;
      replen=selendpos-selstartpos;
      }
    else{
      sindent=0;
      pos=lineStart(cursorpos);
      while(pos<cursorpos){                               // Measure indent of reppos
        if(getByte(pos)=='\t')
          sindent+=(tabcolumns-sindent%tabcolumns);
        else
          sindent+=1;
        pos++;
        }
      nindent=sindent;
      pos=0;
      while(pos<len){                                     // Measure indent of new string
        if(string[pos]=='\t')
          nindent+=(tabcolumns-nindent%tabcolumns);
        else
          nindent+=1;
        pos++;
        }
      oindent=sindent;
      pos=cursorpos;
      while(pos<length && (ch=getByte(pos))!='\n'){       // Measure indent of old string
        if(ch=='\t')
          oindent+=(tabcolumns-oindent%tabcolumns);
        else
          oindent+=1;
        if(oindent==nindent){                             // Same indent
          pos++;                                          // Include last character
          break;
          }
        if(oindent>nindent){                              // Greater indent
          if(ch!='\t') pos++;                             // Don't include last character if it was a tab
          break;
          }
        pos++;
        }
      reppos=cursorpos;
      replen=pos-reppos;
      }
    replaceText(reppos,replen,string,len,TRUE);
    killSelection(TRUE);
    setCursorPos(reppos+len,TRUE);
    makePositionVisible(cursorpos);
    flashMatching();
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Insert a string
long FXText::onCmdInsertString(FXObject*,FXSelector,void* ptr){
  if(isEditable()){
    FXchar* string=(FXchar*)ptr;
    FXint len=strlen(string);
    FXint reppos=cursorpos;
    FXint replen=0;
    if(isPosSelected(cursorpos)){
      reppos=selstartpos;
      replen=selendpos-selstartpos;
      }
    replaceText(reppos,replen,string,len,TRUE);
    killSelection(TRUE);
    setCursorPos(reppos+len,TRUE);
    makePositionVisible(cursorpos);
    flashMatching();
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Insert a character
long FXText::onCmdInsertNewline(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint reppos=cursorpos;
    FXint replen=0;
    FXint len=1;
    if(isPosSelected(cursorpos)){
      reppos=selstartpos;
      replen=selendpos-selstartpos;
      }
    if(options&TEXT_AUTOINDENT){
      FXint start=lineStart(reppos);
      FXint end=start;
      FXchar *string;
      while(end<reppos){
        if(!Ascii::isSpace(getByte(end))) break;
        end++;
        }
      len=end-start+1;
      FXMALLOC(&string,FXchar,len);
      string[0]='\n';
      extractText(&string[1],start,end-start);
      replaceText(reppos,replen,string,len,TRUE);
      FXFREE(&string);
      }
    else{
      replaceText(reppos,replen,"\n",1,TRUE);
      }
    setCursorPos(reppos+len,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }



// Insert a character
long FXText::onCmdInsertTab(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint reppos=cursorpos;
    FXint replen=0;
    FXint len=1;
    if(isPosSelected(cursorpos)){
      reppos=selstartpos;
      replen=selendpos-selstartpos;
      }
    if(options&TEXT_NO_TABS){
      FXint start=lineStart(reppos);
      FXint indent=0;
      FXchar *string;
      while(start<reppos){
        if(getByte(start)=='\t')
          indent+=(tabcolumns-indent%tabcolumns);
        else
          indent+=1;
        start++;
        }
      len=tabcolumns-indent%tabcolumns;
      FXMALLOC(&string,FXchar,len);
      memset(string,' ',len);
      replaceText(reppos,replen,string,len,TRUE);
      FXFREE(&string);
      }
    else{
      replaceText(reppos,replen,"\t",1,TRUE);
      }
    setCursorPos(reppos+len,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Cut
long FXText::onCmdCutSel(FXObject*,FXSelector,void*){
  if(isEditable()){
    if(selstartpos<selendpos){
      FXDragType types[4];
      types[0]=stringType;
      types[1]=textType;
      types[2]=utf8Type;
      types[3]=utf16Type;
      if(acquireClipboard(types,4)){
        FXASSERT(selstartpos<=selendpos);
        extractText(clipped,selstartpos,selendpos-selstartpos);
        handle(this,FXSEL(SEL_COMMAND,ID_DELETE_SEL),NULL);
        }
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Copy
long FXText::onCmdCopySel(FXObject*,FXSelector,void*){
  if(selstartpos<selendpos){
    FXDragType types[4];
    types[0]=stringType;
    types[1]=textType;
    types[2]=utf8Type;
    types[3]=utf16Type;
    if(acquireClipboard(types,4)){
      FXASSERT(selstartpos<=selendpos);
      extractText(clipped,selstartpos,selendpos-selstartpos);
      }
    }
  return 1;
  }


// Delete selection
long FXText::onCmdDeleteSel(FXObject*,FXSelector,void*){
  if(isEditable()){
    if(selstartpos<selendpos){
      removeText(selstartpos,selendpos-selstartpos,TRUE);
      killSelection(TRUE);
      setCursorPos(cursorpos,TRUE);
      makePositionVisible(cursorpos);
      flags|=FLAG_CHANGED;
      modified=TRUE;
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Paste clipboard
long FXText::onCmdPasteSel(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXString string;

    // Delete existing selection
    if(hasSelection()){
      handle(this,FXSEL(SEL_COMMAND,ID_DELETE_SEL),NULL);
      }

    // First, try UTF-8
    if(getDNDData(FROM_CLIPBOARD,utf8Type,string)){
      FXTRACE((100,"Paste UTF8\n"));
#ifdef WIN32
      dosToUnix(string);
#endif
      handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)string.text());
      return 1;
      }

    // Next, try UTF-16
    if(getDNDData(FROM_CLIPBOARD,utf16Type,string)){
      FXUTF16LECodec unicode;           // FIXME maybe other endianness for unix
      FXTRACE((100,"Paste UTF16\n"));
      string=unicode.mb2utf(string);
#ifdef WIN32
      dosToUnix(string);
#endif
      handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)string.text());
      return 1;
      }

    // Next, try good old Latin-1
    if(getDNDData(FROM_CLIPBOARD,stringType,string)){
      FX88591Codec ascii;
      FXTRACE((100,"Paste ASCII\n"));
      string=ascii.mb2utf(string);
#ifdef WIN32
      dosToUnix(string);
#endif
      handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)string.text());
      return 1;
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Paste selection
long FXText::onCmdPasteMiddle(FXObject*,FXSelector,void*){
  if(isEditable()){
    if(selstartpos==selendpos || cursorpos<=selstartpos || selendpos<=cursorpos){ // Avoid paste inside selection
      FXString string;

      // First, try UTF-8
      if(getDNDData(FROM_SELECTION,utf8Type,string)){
        FXTRACE((100,"Paste UTF8\n"));
        handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)string.text());
        return 1;
        }

      // Next, try UTF-16
      if(getDNDData(FROM_SELECTION,utf16Type,string)){
        FXUTF16LECodec unicode;                 // FIXME maybe other endianness for unix
        FXTRACE((100,"Paste UTF16\n"));
        handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)unicode.mb2utf(string).text());
        return 1;
        }

      // Finally, try good old 8859-1
      if(getDNDData(FROM_SELECTION,stringType,string)){
        FX88591Codec ascii;
        FXTRACE((100,"Paste ASCII\n"));
        handle(this,FXSEL(SEL_COMMAND,ID_INSERT_STRING),(void*)ascii.mb2utf(string).text());
        return 1;
        }
      }
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Select character
long FXText::onCmdSelectChar(FXObject*,FXSelector,void*){
  setAnchorPos(cursorpos);
  extendSelection(inc(cursorpos),SELECT_CHARS,TRUE);
  return 1;
  }


// Select Word
long FXText::onCmdSelectWord(FXObject*,FXSelector,void*){
  setAnchorPos(cursorpos);
  extendSelection(cursorpos,SELECT_WORDS,TRUE);
  return 1;
  }


// Select Line
long FXText::onCmdSelectLine(FXObject*,FXSelector,void*){
  setAnchorPos(cursorpos);
  extendSelection(cursorpos,SELECT_LINES,TRUE);
  return 1;
  }


// Select All
long FXText::onCmdSelectAll(FXObject*,FXSelector,void*){
  setAnchorPos(0);
  extendSelection(length,SELECT_CHARS,TRUE);
  return 1;
  }


// Deselect All
long FXText::onCmdDeselectAll(FXObject*,FXSelector,void*){
  killSelection(TRUE);
  return 1;
  }


// Backspace character
long FXText::onCmdBackspace(FXObject*,FXSelector,void*){
  if(isEditable() && 0<cursorpos){
    FXint pos=dec(cursorpos);
    removeText(pos,cursorpos-pos,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Backspace word
long FXText::onCmdBackspaceWord(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint pos=leftWord(cursorpos);
    removeText(pos,cursorpos-pos,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Backspace bol
long FXText::onCmdBackspaceBol(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint pos=rowStart(cursorpos);
    removeText(pos,cursorpos-pos,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Delete character
long FXText::onCmdDelete(FXObject*,FXSelector,void*){
  if(isEditable() && cursorpos<length){
    FXint pos=inc(cursorpos);
    removeText(cursorpos,pos-cursorpos,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Delete word
long FXText::onCmdDeleteWord(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint pos=rightWord(cursorpos);
    removeText(cursorpos,pos-cursorpos,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Delete to end of line
long FXText::onCmdDeleteEol(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint pos=rowEnd(cursorpos);
    removeText(cursorpos,pos-cursorpos,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Delete line
long FXText::onCmdDeleteLine(FXObject*,FXSelector,void*){
  if(isEditable()){
    FXint beg=rowStart(cursorpos);
    FXint end=nextRow(cursorpos);
    removeText(beg,end-beg,TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Delete all text
long FXText::onCmdDeleteAll(FXObject*,FXSelector,void*){
  if(isEditable()){
    removeText(0,length,TRUE);
    setCursorPos(0,TRUE);
    makePositionVisible(0);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Make selected text upper case
long FXText::onCmdChangeCase(FXObject*,FXSelector sel,void*){
  if(isEditable()){
    FXString text;
    FXint pos=selstartpos;
    FXint num=selendpos-selstartpos;
    extractText(text,pos,num);
    if(FXSELID(sel)==ID_UPPER_CASE){
      text.upper();
      }
    else{
      text.lower();
      }
    replaceText(pos,num,text,TRUE);
    setSelection(pos,text.length(),TRUE);
    setCursorPos(cursorpos,TRUE);
    makePositionVisible(cursorpos);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Shift text by certain amount
FXint FXText::shiftText(FXint start,FXint end,FXint amount,FXbool notify){
  FXint white,p,len,size,c;
  FXchar *text;
  if(start<0) start=0;
  if(end>length) end=length;
  FXASSERT(0<tabcolumns);
  if(start<end){
    p=start;
    white=0;
    size=0;
    while(p<end){
      c=getByte(p++);
      if(c==' '){
        white++;
        }
      else if(c=='\t'){
        white+=(tabcolumns-white%tabcolumns);
        }
      else if(c=='\n'){
        size++; white=0;
        }
      else{
        white+=amount;
        if(white<0) white=0;
        if(!(options&TEXT_NO_TABS)){ size+=(white/tabcolumns+white%tabcolumns); } else { size+=white; }
        size++;
        while(p<end){
          c=getByte(p++);
          size++;
          if(c=='\n') break;
          }
        white=0;
        }
      }
    FXMALLOC(&text,FXchar,size);
    p=start;
    white=0;
    len=0;
    while(p<end){
      c=getByte(p++);
      if(c==' '){
        white++;
        }
      else if(c=='\t'){
        white+=(tabcolumns-white%tabcolumns);
        }
      else if(c=='\n'){
        text[len++]='\n'; white=0;
        }
      else{
        white+=amount;
        if(white<0) white=0;
        if(!(options&TEXT_NO_TABS)){ while(white>=tabcolumns){ text[len++]='\t'; white-=tabcolumns;} }
        while(white>0){ text[len++]=' '; white--; }
        text[len++]=c;
        while(p<end){
          c=getByte(p++);
          text[len++]=c;
          if(c=='\n') break;
          }
        white=0;
        }
      }
    FXASSERT(len<=size);
    replaceText(start,end-start,text,len,notify);
    FXFREE(&text);
    return len;
    }
  return 0;
  }


// Shift selected lines left or right
long FXText::onCmdShiftText(FXObject*,FXSelector sel,void*){
  if(isEditable()){
    FXint start,end,len,amount;
    amount=0;
    switch(FXSELID(sel)){
      case ID_SHIFT_LEFT: amount=-1; break;
      case ID_SHIFT_RIGHT: amount=1; break;
      case ID_SHIFT_TABLEFT: amount=-tabcolumns; break;
      case ID_SHIFT_TABRIGHT: amount=tabcolumns; break;
      }
    if(selstartpos<selendpos){
      FXASSERT(0<=selstartpos && selstartpos<=length);
      FXASSERT(0<=selendpos && selendpos<=length);
      start=lineStart(selstartpos);
      end=selendpos;
      if(0<end && getByte(end-1)!='\n') end=nextLine(end);
      }
    else{
      start=lineStart(cursorpos);
      end=lineEnd(cursorpos);
      if(end<length) end++;
      }
    len=shiftText(start,end,amount,TRUE);
    setAnchorPos(start);
    extendSelection(start+len,SELECT_CHARS,TRUE);
    setCursorPos(start,TRUE);
    flags|=FLAG_CHANGED;
    modified=TRUE;
    }
  else{
    getApp()->beep();
    }
  return 1;
  }


// Goto matching character
long FXText::onCmdGotoMatching(FXObject*,FXSelector,void*){
  if(0<cursorpos){
    FXchar ch=getByte(cursorpos-1);
    FXint pos=findMatching(cursorpos-1,0,length,ch,1);
    if(0<=pos){
      setCursorPos(pos+1);
      makePositionVisible(cursorpos);
      return 1;
      }
    }
  getApp()->beep();
  return 1;
  }


// Select text till matching character
long FXText::onCmdSelectMatching(FXObject*,FXSelector,void*){
  if(0<cursorpos){
    FXchar ch=getByte(cursorpos-1);
    FXint pos=findMatching(cursorpos-1,0,length,ch,1);
    if(0<=pos){
      if(pos>cursorpos){
        setAnchorPos(cursorpos-1);
        extendSelection(pos+1,SELECT_CHARS,TRUE);
        }
      else{
        setAnchorPos(pos);
        extendSelection(cursorpos,SELECT_CHARS,TRUE);
        }
      return 1;
      }
    }
  getApp()->beep();
  return 1;
  }


static const FXchar righthand[]="}])>";
static const FXchar lefthand[]="{[(<";


// Select entire enclosing block
long FXText::onCmdSelectBlock(FXObject*,FXSelector sel,void*){
  FXint beg,end,what,level=1;
  while(1){
    what=FXSELID(sel)-ID_SELECT_BRACE;
    beg=matchBackward(cursorpos-1,0,lefthand[what],righthand[what],level);
    end=matchForward(cursorpos,length,lefthand[what],righthand[what],level);
    if(0<=beg && beg<end){
      if(isPosSelected(beg) && isPosSelected(end+1)){ level++; continue; }
      setAnchorPos(beg);
      extendSelection(end+1,SELECT_CHARS,TRUE);
      return 1;
      }
    getApp()->beep();
    break;
    }
  return 1;
  }


// Goto start of enclosing block
long FXText::onCmdBlockBeg(FXObject*,FXSelector sel,void*){
  FXint what=FXSELID(sel)-ID_LEFT_BRACE;
  FXint beg=cursorpos-1;
  if(0<beg){
    if(getByte(beg)==lefthand[what]) beg--;
    FXint pos=matchBackward(beg,0,lefthand[what],righthand[what],1);
    if(0<=pos){
      setCursorPos(pos+1);
      makePositionVisible(cursorpos);
      return 1;
      }
    }
  getApp()->beep();
  return 1;
  }


// Goto end of enclosing block
long FXText::onCmdBlockEnd(FXObject*,FXSelector sel,void*){
  FXint what=FXSELID(sel)-ID_RIGHT_BRACE;
  FXint start=cursorpos;
  if(start<length){
    if(getByte(start)==righthand[what]) start++;
    FXint pos=matchForward(start,length,lefthand[what],righthand[what],1);
    if(0<=pos){
      setCursorPos(pos);
      makePositionVisible(cursorpos);
      return 1;
      }
    }
  getApp()->beep();
  return 1;
  }


// Search for selected text
long FXText::onCmdSearchSel(FXObject*,FXSelector sel,void*){
  FXString string;
  FXint pos=cursorpos;
  FXint beg,end;

  // First, try UTF-8
  if(getDNDData(FROM_SELECTION,utf8Type,string)){
    FXTRACE((100,"Search UTF8\n"));
    searchstring=string;
    }

  // Next, try UTF-16
  else if(getDNDData(FROM_SELECTION,utf16Type,string)){
    FXTRACE((100,"Search UTF16\n"));
    FXUTF16LECodec unicode;                 // FIXME maybe other endianness for unix
    searchstring=unicode.mb2utf(string);
    }

  // Finally, try good old 8859-1
  else if(getDNDData(FROM_SELECTION,stringType,string)){
    FXTRACE((100,"Search ASCII\n"));
    FX88591Codec ascii;
    searchstring=ascii.mb2utf(string);
    }

  // No dice!
  else{
    goto x;
    }

  // Search direction
  if(FXSELID(sel)==ID_SEARCH_FORW_SEL){
    if(isPosSelected(pos)) pos=selendpos;
    searchflags=SEARCH_EXACT|SEARCH_FORWARD;
    }
  else{
    if(isPosSelected(pos)) pos=selstartpos-1;
    searchflags=SEARCH_EXACT|SEARCH_BACKWARD;
    }

  // Perform search
  if(findText(searchstring,&beg,&end,pos,searchflags|SEARCH_WRAP)){
    if(beg!=selstartpos || end!=selendpos){
      setAnchorPos(beg);
      extendSelection(end,SELECT_CHARS,TRUE);
      setCursorPos(end);
      makePositionVisible(beg);
      makePositionVisible(end);
      return 1;
      }
    }

  // Beep
x:getApp()->beep();
  return 1;
  }


// Search for next occurence
long FXText::onCmdSearchNext(FXObject*,FXSelector sel,void*){
  if(!searchstring.empty()){
    FXint pos=cursorpos;
    FXint beg[10];
    FXint end[10];
    if(FXSELID(sel)==ID_SEARCH_FORW){
      if(isPosSelected(pos)) pos=selendpos;
      searchflags&=~SEARCH_BACKWARD;
      }
    else{
      if(isPosSelected(pos)) pos=selstartpos-1;
      searchflags|=SEARCH_BACKWARD;
      }
    if(findText(searchstring,beg,end,pos,searchflags|SEARCH_WRAP,10)){
      if(beg[0]!=selstartpos || end[0]!=selendpos){
        setAnchorPos(beg[0]);
        extendSelection(end[0],SELECT_CHARS,TRUE);
        setCursorPos(end[0]);
        makePositionVisible(beg[0]);
        makePositionVisible(end[0]);
        return 1;
        }
      }
    }
  getApp()->beep();
  return 1;
  }


// Search text
long FXText::onCmdSearch(FXObject*,FXSelector,void*){
  FXGIFIcon icon(getApp(),searchicon);
  FXSearchDialog searchdialog(this,tr("Search"),&icon);
  FXint beg[10];
  FXint end[10];
  FXint pos;
  FXuint code;
  do{
    code=searchdialog.execute();
    if(code==FXSearchDialog::DONE) return 1;
    searchstring=searchdialog.getSearchText();
    searchflags=searchdialog.getSearchMode();
    pos=isPosSelected(cursorpos) ? (searchflags&SEARCH_BACKWARD) ? selstartpos-1 : selendpos : cursorpos;
    if(findText(searchstring,beg,end,pos,searchflags|SEARCH_WRAP,10)){
      setAnchorPos(beg[0]);
      extendSelection(end[0],SELECT_CHARS,TRUE);
      setCursorPos(end[0],TRUE);
      makePositionVisible(beg[0]);
      makePositionVisible(end[0]);
      }
    else{
      getApp()->beep();
      }
    }
  while(code==FXSearchDialog::SEARCH_NEXT);
  return 1;
  }


// Replace text; we assume that findText has called squeezegap()!
long FXText::onCmdReplace(FXObject*,FXSelector,void*){
  FXGIFIcon icon(getApp(),searchicon);
  FXReplaceDialog replacedialog(this,tr("Replace"),&icon);
  FXint beg[10],end[10],fm,to,len,pos;
  FXuint searchflags,code;
  FXString searchstring;
  FXString replacestring;
  FXString replacevalue;
  do{
    code=replacedialog.execute();
    if(code==FXReplaceDialog::DONE) return 1;
    searchflags=replacedialog.getSearchMode();
    searchstring=replacedialog.getSearchText();
    replacestring=replacedialog.getReplaceText();
    replacevalue=FXString::null;
    fm=-1;
    to=-1;
    if(code==FXReplaceDialog::REPLACE_ALL){
      searchflags&=~SEARCH_BACKWARD;
      pos=0;
      while(findText(searchstring,beg,end,pos,searchflags,10)){
        if(0<=fm) replacevalue.append(&buffer[pos],beg[0]-pos);
        replacevalue.append(FXRex::substitute(buffer,length,beg,end,replacestring,10));
        if(fm<0) fm=beg[0];
        to=end[0];
        pos=end[0];
        if(beg[0]==end[0]) pos++;
        }
      }
    else{
      pos=isPosSelected(cursorpos) ? (searchflags&SEARCH_BACKWARD) ? selstartpos-1 : selendpos : cursorpos;
      if(findText(searchstring,beg,end,pos,searchflags|SEARCH_WRAP,10)){
        replacevalue=FXRex::substitute(buffer,length,beg,end,replacestring,10);
        fm=beg[0];
        to=end[0];
        }
      }
    if(0<=fm){
      len=replacevalue.length();
      replaceText(fm,to-fm,replacevalue.text(),len,TRUE);
      setCursorPos(fm+len,TRUE);
      makePositionVisible(getCursorPos());
      modified=TRUE;
      }
    else{
      getApp()->beep();
      }
    }
  while(code==FXReplaceDialog::REPLACE_NEXT);
  return 1;
  }


// Goto selected line number
long FXText::onCmdGotoSelected(FXObject*,FXSelector,void*){
  FXString string;
  if(getDNDData(FROM_SELECTION,stringType,string)){
    FXint s=string.find_first_of("0123456789");
    if(0<=s){
      FXint row=0;
      while(Ascii::isDigit(string[s])){
        row=row*10+Ascii::digitValue(string[s]);
        s++;
        }
      if(1<=row){
        setCursorRow(row-1,TRUE);
        makePositionVisible(cursorpos);
        return 1;
        }
      }
    }
  getApp()->beep();
  return 1;
  }


// Goto line number
long FXText::onCmdGotoLine(FXObject*,FXSelector,void*){
  FXGIFIcon icon(getApp(),gotoicon);
  FXint row=cursorrow+1;
  if(FXInputDialog::getInteger(row,this,tr("Goto Line"),tr("&Goto line number:"),&icon,1,2147483647)){
    update();
    setCursorRow(row-1,TRUE);
    makePositionVisible(cursorpos);
    }
  return 1;
  }


/*******************************************************************************/

// Editable toggle
long FXText::onCmdToggleEditable(FXObject*,FXSelector,void*){
  setEditable(!isEditable());
  return 1;
  }


// Update editable toggle
long FXText::onUpdToggleEditable(FXObject* sender,FXSelector,void*){
  sender->handle(this,isEditable()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


// Overstrike toggle
long FXText::onCmdToggleOverstrike(FXObject*,FXSelector,void*){
  setOverstrike(!isOverstrike());
  return 1;
  }


// Update overstrike toggle
long FXText::onUpdToggleOverstrike(FXObject* sender,FXSelector,void*){
  sender->handle(this,isOverstrike()?FXSEL(SEL_COMMAND,ID_CHECK):FXSEL(SEL_COMMAND,ID_UNCHECK),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SHOW),NULL);
  sender->handle(this,FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


// Move cursor to indicated row
long FXText::onCmdCursorRow(FXObject* sender,FXSelector,void*){
  FXint row=cursorrow+1;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&row);
  setCursorRow(row-1,TRUE);
  makePositionVisible(cursorpos);
  return 1;
  }


// Being asked about current row number
long FXText::onUpdCursorRow(FXObject* sender,FXSelector,void*){
  FXint row=cursorrow+1;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_SETINTVALUE),(void*)&row);
  return 1;
  }


// Move cursor to indicated column
long FXText::onCmdCursorColumn(FXObject* sender,FXSelector,void*){
  FXint col=cursorcol;
  sender->handle(this,FXSEL(SEL_COMMAND,ID_GETINTVALUE),(void*)&col);
  setCursorColumn(col,TRUE);
  makePositionVisible(cursorpos);
  return 1;
  }


// Being asked about current column
long FXText::onUpdCursorColumn(FXObject* sender,FXSelector,void*){
  sender->handle(this,FXSEL(SEL_COMMAND,FXWindow::ID_SETINTVALUE),(void*)&cursorcol);
  return 1;
  }


// Update somebody who works on the selection
long FXText::onUpdHaveSelection(FXObject* sender,FXSelector,void*){
  sender->handle(this,(selstartpos<selendpos)?FXSEL(SEL_COMMAND,ID_ENABLE):FXSEL(SEL_COMMAND,ID_DISABLE),NULL);
  return 1;
  }


// Update somebody who works on the selection
long FXText::onUpdSelectAll(FXObject* sender,FXSelector,void*){
  sender->handle(this,(length==0)?FXSEL(SEL_COMMAND,ID_DISABLE):FXSEL(SEL_COMMAND,ID_ENABLE),NULL);
  return 1;
  }


/*******************************************************************************/

// FIXME
// Runs should preferably be whole combining sequence
// Deal with non-rectangular selections.

// Draw fragment of text in given style
void FXText::drawBufferText(FXDCWindow& dc,FXint x,FXint y,FXint,FXint,FXint pos,FXint n,FXuint style) const {
  register FXuint index=(style&STYLE_MASK);
  register FXuint usedstyle=style;                                              // Style flags from style buffer
  register FXColor color;
  FXchar str[2];
  color=0;
  if(hilitestyles && index){                                                    // Get colors from style table
    usedstyle=hilitestyles[index-1].style;                                      // Style flags now from style table
    if(style&STYLE_SELECTED) color=hilitestyles[index-1].selectForeColor;
    else if(style&STYLE_HILITE) color=hilitestyles[index-1].hiliteForeColor;
    if(color==0) color=hilitestyles[index-1].normalForeColor;                   // Fall back on normal foreground color
    }
  if(color==0){                                                                 // Fall back to default style
    if(style&STYLE_SELECTED) color=seltextColor;
    else if(style&STYLE_HILITE) color=hilitetextColor;
    if(color==0) color=textColor;                                               // Fall back to normal text color
    }
  dc.setForeground(color);
  if(style&STYLE_CONTROL){
    y+=font->getFontAscent();
    str[0]='^';
    while(pos<gapstart && 0<n){
      str[1]=buffer[pos]|0x40;
      dc.drawText(x,y,str,2);
      if(usedstyle&STYLE_BOLD) dc.drawText(x+1,y,str,2);
      x+=font->getTextWidth(str,2);
      pos++;
      n--;
      }
    while(0<n){
      str[1]=buffer[pos-gapstart+gapend]|0x40;
      dc.drawText(x,y,str,2);
      if(usedstyle&STYLE_BOLD) dc.drawText(x+1,y,str,2);
      x+=font->getTextWidth(str,2);
      pos++;
      n--;
      }
    }
  else{
    y+=font->getFontAscent();
    if(pos+n<=gapstart){
      dc.drawText(x,y,&buffer[pos],n);
      if(usedstyle&STYLE_BOLD) dc.drawText(x+1,y,&buffer[pos],n);
      }
    else if(pos>=gapstart){
      dc.drawText(x,y,&buffer[pos-gapstart+gapend],n);
      if(usedstyle&STYLE_BOLD) dc.drawText(x+1,y,&buffer[pos-gapstart+gapend],n);
      }
    else{
      dc.drawText(x,y,&buffer[pos],gapstart-pos);
      if(usedstyle&STYLE_BOLD) dc.drawText(x+1,y,&buffer[pos],gapstart-pos);
      x+=font->getTextWidth(&buffer[pos],gapstart-pos);
      dc.drawText(x,y,&buffer[gapend],pos+n-gapstart);
      if(usedstyle&STYLE_BOLD) dc.drawText(x+1,y,&buffer[gapend],pos+n-gapstart);
      }
    }
  }


// Fill fragment of background in given style
void FXText::fillBufferRect(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h,FXuint style) const {
  register FXuint index=(style&STYLE_MASK);
  register FXuint usedstyle=style;                                              // Style flags from style buffer
  register FXColor bgcolor,fgcolor;
  bgcolor=fgcolor=0;
  if(hilitestyles && index){                                                    // Get colors from style table
    usedstyle=hilitestyles[index-1].style;                                      // Style flags now from style table
    if(style&STYLE_SELECTED){
      bgcolor=hilitestyles[index-1].selectBackColor;
      fgcolor=hilitestyles[index-1].selectForeColor;
      }
    else if(style&STYLE_HILITE){
      bgcolor=hilitestyles[index-1].hiliteBackColor;
      fgcolor=hilitestyles[index-1].hiliteForeColor;
      }
    else if(style&STYLE_ACTIVE){
      bgcolor=hilitestyles[index-1].activeBackColor;
      }
    else{
      bgcolor=hilitestyles[index-1].normalBackColor;
      }
    if(fgcolor==0){                                                             // Fall back to normal foreground color
      fgcolor=hilitestyles[index-1].normalForeColor;
      }
    }
  if(bgcolor==0){                                                               // Fall back to default background colors
    if(style&STYLE_SELECTED) bgcolor=selbackColor;
    else if(style&STYLE_HILITE) bgcolor=hilitebackColor;
    else if(style&STYLE_ACTIVE) bgcolor=activebackColor;
    else bgcolor=backColor;
    }
  if(fgcolor==0){                                                               // Fall back to default foreground colors
    if(style&STYLE_SELECTED) fgcolor=seltextColor;
    else if(style&STYLE_HILITE) fgcolor=hilitetextColor;
    if(fgcolor==0) fgcolor=textColor;                                           // Fall back to text color
    }
  dc.setForeground(bgcolor);
  dc.fillRectangle(x,y,w,h);
  if(usedstyle&STYLE_UNDERLINE){
    dc.setForeground(fgcolor);
    dc.fillRectangle(x,y+font->getFontAscent()+1,w,1);
    }
  if(usedstyle&STYLE_STRIKEOUT){
    dc.setForeground(fgcolor);
    dc.fillRectangle(x,y+font->getFontAscent()/2,w,1);
    }
  }


// Obtain text style at position pos; note pos may be outside of text
// to allow for rectangular selections!
FXuint FXText::style(FXint row,FXint,FXint end,FXint pos) const {
  register FXuint s=0;
  register FXchar ch;

  // Selected part of text
  if(selstartpos<=pos && pos<selendpos) s|=STYLE_SELECTED;

  // Highlighted part of text
  if(hilitestartpos<=pos && pos<hiliteendpos) s|=STYLE_HILITE;

  // Current active line
  if((row==cursorrow)&&(options&TEXT_SHOWACTIVE)) s|=STYLE_ACTIVE;

  // Blank part of line
  if(pos>=end) return s;

  // Special style for control characters
  ch=getByte(pos);

  // Get value from style buffer
  if(sbuffer) s|=getStyle(pos);

  // Tabs are just fill
  if(ch == '\t') return s;

  // Spaces are just fill
  if(ch == ' ') return s;

  // Newlines are just fill
  if(ch == '\n') return s;

  // Get special style for control codes
  if((FXuchar)ch < ' ') return s|STYLE_CONTROL|STYLE_TEXT;

  return s|STYLE_TEXT;
  }


// Draw partial text line with correct style
void FXText::drawTextRow(FXDCWindow& dc,FXint line,FXint left,FXint right) const {
  register FXint x,y,w,h,linebeg,lineend,truelineend,cw,sp,ep,row,edge;
  register FXuint curstyle,newstyle;
  linebeg=visrows[line];
  lineend=truelineend=visrows[line+1];
  if(linebeg<lineend && Ascii::isSpace(getByte(lineend-1))) lineend--;         // Back off last space
  x=0;
  w=0;
  h=font->getFontHeight();
  y=pos_y+margintop+(toprow+line)*h;
  edge=pos_x+marginleft+barwidth;
  row=toprow+line;

  // Scan ahead till until we hit the end or the left edge
  for(sp=linebeg; sp<lineend; sp+=getCharLen(sp)){
    cw=charWidth(getChar(sp),x);
    if(x+edge+cw>=left) break;
    x+=cw;
    }

  // First style to display
  curstyle=style(row,linebeg,lineend,sp);

  // Draw until we hit the end or the right edge
  for(ep=sp; ep<lineend; ep+=getCharLen(ep)){
    newstyle=style(row,linebeg,truelineend,ep);
    if(newstyle!=curstyle){
      fillBufferRect(dc,edge+x,y,w,h,curstyle);
      if(curstyle&STYLE_TEXT) drawBufferText(dc,edge+x,y,w,h,sp,ep-sp,curstyle);
      curstyle=newstyle;
      sp=ep;
      x+=w;
      w=0;
      }
    cw=charWidth(getChar(ep),x+w);
    if(x+edge+w>=right) break;
    w+=cw;
    }

  // Draw unfinished fragment
  fillBufferRect(dc,edge+x,y,w,h,curstyle);
  if(curstyle&STYLE_TEXT) drawBufferText(dc,edge+x,y,w,h,sp,ep-sp,curstyle);
  x+=w;

  // Fill any left-overs outside of text
  if(x+edge<right){
    curstyle=style(row,linebeg,truelineend,ep);
    fillBufferRect(dc,edge+x,y,right-edge-x,h,curstyle);
    }
  }


// Draw the cursor
void FXText::drawCursor(FXuint state){
  register FXint xx,yt,yb,xlo,xhi,fh;
  if((state^flags)&FLAG_CARET){
    if(xid){
      FXASSERT(0<=cursorpos && cursorpos<=length);
      FXASSERT(0<=cursorrow && cursorrow<=nrows);
      if(toprow<=cursorrow && cursorrow<toprow+nvisrows){
        xx=pos_x+marginleft+barwidth+lineWidth(cursorstart,cursorpos-cursorstart)-1;
        if(barwidth<=xx+3 && xx-2<viewport_w){
          FXDCWindow dc(this);
          fh=font->getFontHeight();
          yt=pos_y+margintop+cursorrow*fh;
          yb=yt+fh-1;

          // Cursor can overhang margins but not line number bar
          dc.setClipRectangle(barwidth,0,viewport_w-barwidth,viewport_h);

          // Draw I beam
          if(state&FLAG_CARET){

            // Draw I-beam
            dc.setForeground(cursorColor);
            dc.fillRectangle(xx,yt,2,yb-yt);
            dc.fillRectangle(xx-2,yt,6,1);
            dc.fillRectangle(xx-2,yb,6,1);
            }

          // Erase I-beam
          else{

            // Erase I-beam, plus the text immediately surrounding it
            dc.setForeground(backColor);
            dc.fillRectangle(xx-2,yt,6,yb-yt+1);

            // Clip the text to the margins AND the rectangle that was
            // just erased.  We don't want to overdraw any existing
            // characters, because of ClearType.
            xlo=FXMAX(xx-2,marginleft+barwidth);
            xhi=FXMIN(xx+4,viewport_w-marginright);
            dc.setClipRectangle(xlo,margintop,xhi-xlo,viewport_h-margintop-marginbottom);

            // Restore text
            dc.setFont(font);
            drawTextRow(dc,cursorrow-toprow,xx-3,xx+4);
            }
          }
        }
      }
    flags^=FLAG_CARET;
    }
  }


// Erase cursor overhang outside of margins
void FXText::eraseCursorOverhang(){
  register FXint xx,yt,yb,fh;
  FXASSERT(0<=cursorpos && cursorpos<=length);
  FXASSERT(0<=cursorrow && cursorrow<=nrows);
  if(toprow<=cursorrow && cursorrow<toprow+nvisrows){
    xx=pos_x+marginleft+barwidth+lineWidth(cursorstart,cursorpos-cursorstart)-1;
    if(barwidth<=xx+3 && xx-2<viewport_w){
      FXDCWindow dc(this);
      fh=font->getFontHeight();
      yt=pos_y+margintop+cursorrow*fh;
      yb=yt+fh-1;
      dc.setClipRectangle(barwidth,0,viewport_w-barwidth,viewport_h);
      if(xx-2<=marginleft+barwidth && barwidth<=xx+3){
        dc.setForeground(backColor);
        dc.fillRectangle(barwidth,yt,marginleft,fh);
        }
      if(viewport_w-marginright<=xx+3 && xx-2<=viewport_w){
        dc.setForeground(backColor);
        dc.fillRectangle(viewport_w-marginright,yt,marginright,fh);
        }
      if(yt<=margintop && 0<=yb){
        dc.setForeground(backColor);
        dc.fillRectangle(xx-2,0,5,margintop);
        }
      if(viewport_h-marginbottom<=yb && yt<viewport_h){
        dc.setForeground(backColor);
        dc.fillRectangle(xx-2,viewport_h-marginbottom,5,marginbottom);
        }
      }
    }
  }


// Repaint lines of text
void FXText::drawContents(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXint hh=font->getFontHeight();
  register FXint yy=pos_y+margintop+toprow*hh;
  register FXint tl=(y-yy)/hh;
  register FXint bl=(y+h-yy)/hh;
  register FXint ln;
  if(tl<0) tl=0;
  if(bl>=nvisrows) bl=nvisrows-1;
  for(ln=tl; ln<=bl; ln++){
    drawTextRow(dc,ln,x,x+w);
    }
  }


// Repaint line numbers
void FXText::drawNumbers(FXDCWindow& dc,FXint x,FXint y,FXint w,FXint h) const {
  register FXint hh=font->getFontHeight();
  register FXint yy=pos_y+margintop+toprow*hh;
  register FXint tl=(y-yy)/hh;
  register FXint bl=(y+h-yy)/hh;
  register FXint ln,n,tw;
  FXchar lineno[20];
  if(tl<0) tl=0;
  if(bl>=nvisrows) bl=nvisrows-1;
  dc.setForeground(barColor);
  dc.fillRectangle(x,y,w,h);
  dc.setForeground(numberColor);
  for(ln=tl; ln<=bl; ln++){
    n=sprintf(lineno,"%d",toprow+ln+1);
    tw=font->getTextWidth(lineno,n);
    dc.drawText(barwidth-tw,yy+ln*hh+font->getFontAscent(),lineno,n);
    }
  }


// Repaint text range
void FXText::updateRange(FXint beg,FXint end) const {
  register FXint tl,bl,fc,lc,ty,by,lx,rx,t;
  if(beg>end){t=beg;beg=end;end=t;}
  if(beg<visrows[nvisrows] && visrows[0]<end && beg<end){
    if(beg<visrows[0]) beg=visrows[0];
    if(end>visrows[nvisrows]) end=visrows[nvisrows];
    tl=posToLine(beg,0);
    bl=posToLine(end,tl);
    if(tl==bl){
      fc=beg-visrows[tl];
      lc=end-visrows[tl];
      ty=pos_y+margintop+(toprow+tl)*font->getFontHeight();
      by=ty+font->getFontHeight();
      lx=pos_x+marginleft+barwidth+lineWidth(visrows[tl],fc);
      if(end<=(visrows[tl+1]-1)) rx=pos_x+marginleft+barwidth+lineWidth(visrows[tl],lc); else rx=width;
      }
    else{
      ty=pos_y+margintop+(toprow+tl)*font->getFontHeight();
      by=pos_y+margintop+(toprow+bl+1)*font->getFontHeight();
      lx=barwidth;
      rx=width;
      }
    update(lx,ty,rx-lx,by-ty);
    }
  }


// Draw item list
long FXText::onPaint(FXObject*,FXSelector,void* ptr){
  FXEvent* event=(FXEvent*)ptr;
  FXDCWindow dc(this,event);
  dc.setFont(font);
//dc.setForeground(FXRGB(255,0,0));
//dc.fillRectangle(event->rect.x,event->rect.y,event->rect.w,event->rect.h);

  // Paint top margin
  if(event->rect.y<=margintop){
    dc.setForeground(backColor);
    dc.fillRectangle(barwidth,0,viewport_w-barwidth,margintop);
    }

  // Paint bottom margin
  if(event->rect.y+event->rect.h>=viewport_h-marginbottom){
    dc.setForeground(backColor);
    dc.fillRectangle(barwidth,viewport_h-marginbottom,viewport_w-barwidth,marginbottom);
    }

  // Paint left margin
  if(event->rect.x<barwidth+marginleft){
    dc.setForeground(backColor);
    dc.fillRectangle(barwidth,margintop,marginleft,viewport_h-margintop-marginbottom);
    }

  // Paint right margin
  if(event->rect.x+event->rect.w>=viewport_w-marginright){
    dc.setForeground(backColor);
    dc.fillRectangle(viewport_w-marginright,margintop,marginright,viewport_h-margintop-marginbottom);
    }

  // Paint line numbers
  if(event->rect.x<barwidth){
    dc.setClipRectangle(0,0,barwidth,height);
    drawNumbers(dc,event->rect.x,event->rect.y,event->rect.w,event->rect.h);
    }

  // Paint text
  dc.setClipRectangle(marginleft+barwidth,margintop,viewport_w-marginright-marginleft-barwidth,viewport_h-margintop-marginbottom);
  drawContents(dc,event->rect.x,event->rect.y,event->rect.w,event->rect.h);

  // FIXME make sure its drawn if we have focus
  drawCursor(flags);

  return 1;
  }


/*******************************************************************************/


// Move the cursor
void FXText::setCursorPos(FXint pos,FXbool notify){
  register FXint cursorstartold,cursorendold;
  pos=validPos(pos);
  if(cursorpos!=pos){
    drawCursor(0);
    if(pos<cursorstart || cursorend<=pos){    // Move to other line?
      cursorstartold=cursorstart;
      cursorendold=cursorend;
      cursorstart=rowStart(pos);
      cursorend=nextRow(cursorstart);
      if(cursorstart<cursorstartold){
        cursorrow=cursorrow-countRows(cursorstart,cursorstartold);
        }
      else{
        cursorrow=cursorrow+countRows(cursorstartold,cursorstart);
        }
      if(options&TEXT_SHOWACTIVE){
        updateRange(cursorstartold,cursorendold);
        updateRange(cursorstart,cursorend);
        }
      }
    cursorcol=indentFromPos(cursorstart,pos);
    cursorpos=pos;
    drawCursor(FLAG_CARET);
    prefcol=-1;
    if(target && notify){
      target->tryHandle(this,FXSEL(SEL_CHANGED,message),(void*)(FXival)cursorpos);
      }
    }
  }


// Set cursor row
void FXText::setCursorRow(FXint row,FXbool notify){
  register FXint col,newrow,newpos;
  if(row!=cursorrow){
    if(row<0) row=0;
    if(row>=nrows) row=nrows-1;
    col=(0<=prefcol) ? prefcol : cursorcol;
    if(row>cursorrow){
      newrow=nextRow(cursorpos,row-cursorrow);
      }
    else{
      newrow=prevRow(cursorpos,cursorrow-row);
      }
    newpos=posFromIndent(newrow,col);
    setCursorPos(newpos,notify);
    prefcol=col;
    }
  }


// Set cursor column
void FXText::setCursorColumn(FXint col,FXbool notify){
  register FXint newpos;
  if(cursorcol!=col){
    newpos=posFromIndent(cursorstart,col);
    setCursorPos(newpos,notify);
    }
  }


// Set anchor position
void FXText::setAnchorPos(FXint pos){
  anchorpos=validPos(pos);
  }


// Select all text
FXbool FXText::selectAll(FXbool notify){
  return setSelection(0,length,notify);
  }


// Extend selection
FXbool FXText::extendSelection(FXint pos,FXTextSelectionMode select,FXbool notify){
  register FXint sp,ep;

  // Validate position
  pos=validPos(pos);

  // Did position change?
  switch(select){

    // Selecting words
    case SELECT_WORDS:
      if(pos<=anchorpos){
        sp=wordStart(pos);
        ep=wordEnd(anchorpos);
        }
      else{
        sp=wordStart(anchorpos);
        ep=wordEnd(pos);
        }
      break;

    // Selecting lines
    case SELECT_LINES:
      if(pos<=anchorpos){
        sp=rowStart(pos);
        ep=nextRow(anchorpos);
        }
      else{
        sp=rowStart(anchorpos);
        ep=nextRow(pos);
        }
      break;

    // Selecting characters
    default:
      if(pos<=anchorpos){
        sp=pos;
        ep=anchorpos;
        }
      else{
        sp=anchorpos;
        ep=pos;
        }
      break;
    }

  // Select the new range
  return setSelection(sp,ep-sp,notify);
  }


// Set selection
FXbool FXText::setSelection(FXint pos,FXint len,FXbool notify){
  register FXint ep,sp;
  FXDragType types[4];
  FXint what[2];

  // Validate positions
  sp=validPos(pos);
  ep=validPos(pos+len);

  // Something changed?
  if(selstartpos!=sp || selendpos!=ep){

    // Release selection
    if(sp==ep){
      if(notify && target){
        what[0]=selstartpos;
        what[1]=selendpos-selstartpos;
        target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)what);
        }
      if(hasSelection()) releaseSelection();
      }

    // Minimally update
    if(ep<=selstartpos || selendpos<=sp){
      updateRange(selstartpos,selendpos);
      updateRange(sp,ep);
      }
    else{
      updateRange(sp,selstartpos);
      updateRange(selendpos,ep);
      }

    selstartpos=sp;
    selendpos=ep;

    // Acquire selection
    if(sp!=ep){
      types[0]=stringType;
      types[1]=textType;
      types[2]=utf8Type;
      types[3]=utf16Type;
      if(!hasSelection()){
        acquireSelection(types,4);
        }
      if(notify && target){
        what[0]=selstartpos;
        what[1]=selendpos-selstartpos;
        target->tryHandle(this,FXSEL(SEL_SELECTED,message),(void*)what);
        }
      }
    return TRUE;
    }
  return FALSE;
  }


// Kill the selection
FXbool FXText::killSelection(FXbool notify){
  FXint what[2];
  if(selstartpos<selendpos){
    if(notify && target){
      what[0]=selstartpos;
      what[1]=selendpos-selstartpos;
      target->tryHandle(this,FXSEL(SEL_DESELECTED,message),(void*)what);
      }
    if(hasSelection()) releaseSelection();
    updateRange(selstartpos,selendpos);
    selstartpos=0;
    selendpos=0;
    return TRUE;
    }
  return FALSE;
  }


// Set highlight
FXbool FXText::setHighlight(FXint pos,FXint len){
  register FXint hs,he;

  // Validate positions
  hs=validPos(pos);
  he=validPos(pos+len);

  // Anything changed?
  if(hs!=hilitestartpos || he!=hiliteendpos){

    // Minimally update
    if(he<=hilitestartpos || hiliteendpos<=hs){
      updateRange(hilitestartpos,hiliteendpos);
      updateRange(hs,he);
      }
    else{
      updateRange(hs,hilitestartpos);
      updateRange(hiliteendpos,he);
      }

    // Keep new range
    hilitestartpos=hs;
    hiliteendpos=he;

    return TRUE;
    }
  return FALSE;
  }


// Unhighlight the text
FXbool FXText::killHighlight(){
  if(hilitestartpos<hiliteendpos){
    updateRange(hilitestartpos,hiliteendpos);
    hilitestartpos=0;
    hiliteendpos=0;
    return TRUE;
    }
  return FALSE;
  }


/*******************************************************************************/


// Change top margin
void FXText::setMarginTop(FXint mt){
  if(margintop!=mt){
    margintop=mt;
    recalc();
    update();
    }
  }

// Change bottom margin
void FXText::setMarginBottom(FXint mb){
  if(marginbottom!=mb){
    marginbottom=mb;
    recalc();
    update();
    }
  }


// Change left margin
void FXText::setMarginLeft(FXint ml){
  if(marginleft!=ml){
    marginleft=ml;
    recalc();
    update();
    }
  }


// Change right margin
void FXText::setMarginRight(FXint mr){
  if(marginright!=mr){
    marginright=mr;
    recalc();
    update();
    }
  }


// Change the font
void FXText::setFont(FXFont* fnt){
  if(!fnt){ fxerror("%s::setFont: NULL font specified.\n",getClassName()); }
  if(font!=fnt){
    font=fnt;
    recalc();
    tabwidth=tabcolumns*font->getTextWidth(" ",1);
    barwidth=barcolumns*font->getTextWidth("8",1);
//    if(options&TEXT_FIXEDWRAP){ wrapwidth=wrapcolumns*font->getTextWidth("x",1); }
    recalc();
    update();
    }
  }


// Set wrap columns
void FXText::setWrapColumns(FXint cols){
  if(cols<=0) cols=1;
  if(cols!=wrapcolumns){
    wrapcolumns=cols;
//    if(options&TEXT_FIXEDWRAP){ wrapwidth=wrapcolumns*font->getTextWidth("x",1); }
    recalc();
    update();
    }
  }


// Set tab columns
void FXText::setTabColumns(FXint cols){
  if(cols<=0) cols=1;
  if(cols!=tabcolumns){
    tabcolumns=cols;
    tabwidth=tabcolumns*font->getTextWidth(" ",1);
    recalc();
    update();
    }
  }

// Change number of columns used for line numbers
void FXText::setBarColumns(FXint cols){
  if(cols<=0) cols=0;
  if(cols!=barcolumns){
    barcolumns=cols;
    barwidth=barcolumns*font->getTextWidth("8",1);
    recalc();
    update();
    }
  }

// Set text color
void FXText::setTextColor(FXColor clr){
  if(clr!=textColor){
    textColor=clr;
    update(barwidth,0,width-barwidth,height);
    }
  }


// Set select background color
void FXText::setSelBackColor(FXColor clr){
  if(clr!=selbackColor){
    selbackColor=clr;
    updateRange(selstartpos,selendpos);
    }
  }


// Set selected text color
void FXText::setSelTextColor(FXColor clr){
  if(clr!=seltextColor){
    seltextColor=clr;
    updateRange(selstartpos,selendpos);
    }
  }


// Change highlighted text color
void FXText::setHiliteTextColor(FXColor clr){
  if(clr!=hilitetextColor){
    hilitetextColor=clr;
    updateRange(hilitestartpos,hiliteendpos);
    }
  }


// Change highlighted background color
void FXText::setHiliteBackColor(FXColor clr){
  if(clr!=hilitebackColor){
    hilitebackColor=clr;
    updateRange(hilitestartpos,hiliteendpos);
    }
  }


// Change active background color
void FXText::setActiveBackColor(FXColor clr){
  if(clr!=activebackColor){
    activebackColor=clr;
    update(barwidth,0,width-barwidth,height);
    }
  }

// Change line number color
void FXText::setNumberColor(FXColor clr){
  if(clr!=numberColor){
    numberColor=clr;
    update(0,0,barwidth,height);
    }
  }


// Change bar color
void FXText::setBarColor(FXColor clr){
  if(clr!=barColor){
    barColor=clr;
    update(0,0,barwidth,height);
    }
  }


// Set cursor color
void FXText::setCursorColor(FXColor clr){
  if(clr!=cursorColor){
    cursorColor=clr;
    updateRange(cursorstart,cursorend);
    }
  }


// Change text style
void FXText::setTextStyle(FXuint style){
  FXuint opts=(options&~TEXT_MASK) | (style&TEXT_MASK);
  if(options!=opts){
    options=opts;
//    if(options&TEXT_FIXEDWRAP){ wrapwidth=wrapcolumns*font->getTextWidth("x",1); }
    recalc();
    update();
    }
  }


// Get text style
FXuint FXText::getTextStyle() const {
  return (options&TEXT_MASK);
  }


// Return true if editable
FXbool FXText::isEditable() const {
  return (options&TEXT_READONLY)==0;
  }


// Set widget is editable or not
void FXText::setEditable(FXbool edit){
  if(edit) options&=~TEXT_READONLY; else options|=TEXT_READONLY;
  }


// Return TRUE if text is in overstrike mode
FXbool FXText::isOverstrike() const {
  return (options&TEXT_OVERSTRIKE)!=0;
  }


// Set overstrike mode
void FXText::setOverstrike(FXbool over){
  if(over) options|=TEXT_OVERSTRIKE; else options&=~TEXT_OVERSTRIKE;
  }


// Set styled text mode
void FXText::setStyled(FXbool styled){
  if(styled && !sbuffer){
    if(!FXCALLOC(&sbuffer,FXchar,length+gapend-gapstart)){fxerror("%s::setStyled: out of memory.\n",getClassName());}
    update();
    }
  if(!styled && sbuffer){
    FXFREE(&sbuffer);
    update();
    }
  }


// Set highlight styles
void FXText::setHiliteStyles(const FXHiliteStyle* styles){
  hilitestyles=styles;
  update();
  }


// Change number of visible rows
void FXText::setVisibleRows(FXint rows){
  if(rows<0) rows=0;
  if(vrows!=rows){
    vrows=rows;
    recalc();
    }
  }


// Change number of visible columns
void FXText::setVisibleColumns(FXint cols){
  if(cols<0) cols=0;
  if(vcols!=cols){
    vcols=cols;
    recalc();
    }
  }


/*******************************************************************************/


// Update value from a message
long FXText::onCmdSetStringValue(FXObject*,FXSelector,void* ptr){
  setText(*((FXString*)ptr));
  return 1;
  }


// Obtain value from text
long FXText::onCmdGetStringValue(FXObject*,FXSelector,void* ptr){
  getText(*((FXString*)ptr));
  return 1;
  }


/*******************************************************************************/


// Save object to stream
void FXText::save(FXStream& store) const {
  FXScrollArea::save(store);
  store << length;
  store.save(buffer,gapstart);
  store.save(buffer+gapend,length-gapstart);
  store << nvisrows;
  store.save(visrows,nvisrows+1);
  store << wrapcolumns;
  store << tabcolumns;
  store << margintop;
  store << marginbottom;
  store << marginleft;
  store << marginright;
  store << font;
  store << textColor;
  store << selbackColor;
  store << seltextColor;
  store << hilitebackColor;
  store << hilitetextColor;
  store << cursorColor;
  store << help;
  store << tip;
  store << matchtime;
  }


// Load object from stream
void FXText::load(FXStream& store){
  FXScrollArea::load(store);
  store >> length;
  FXMALLOC(&buffer,FXchar,length+MINSIZE);    // FIXME should we save text&style?
  store.load(buffer,length);
  gapstart=length;
  gapend=length+MINSIZE;
  store >> nvisrows;
  FXMALLOC(&visrows,FXint,nvisrows+1);
  store.load(visrows,nvisrows+1);
  store >> wrapcolumns;
  store >> tabcolumns;
  store >> margintop;
  store >> marginbottom;
  store >> marginleft;
  store >> marginright;
  store >> font;
  store >> textColor;
  store >> selbackColor;
  store >> seltextColor;
  store >> hilitebackColor;
  store >> hilitetextColor;
  store >> cursorColor;
  store >> help;
  store >> tip;
  store >> matchtime;
  }


// Clean up
FXText::~FXText(){
  getApp()->removeTimeout(this,ID_BLINK);
  getApp()->removeTimeout(this,ID_FLASH);
  FXFREE(&buffer);
  FXFREE(&sbuffer);
  FXFREE(&visrows);
  buffer=(FXchar*)-1L;
  sbuffer=(FXchar*)-1L;
  visrows=(FXint*)-1L;
  font=(FXFont*)-1L;
  hilitestyles=(FXHiliteStyle*)-1L;
  }

}
