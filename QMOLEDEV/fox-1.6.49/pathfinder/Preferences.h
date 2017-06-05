/********************************************************************************
*                                                                               *
*                        P r e f e r e n c e s   D i a l o g                    *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This program is free software; you can redistribute it and/or modify          *
* it under the terms of the GNU General Public License as published by          *
* the Free Software Foundation; either version 2 of the License, or             *
* (at your option) any later version.                                           *
*                                                                               *
* This program is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                 *
* GNU General Public License for more details.                                  *
*                                                                               *
* You should have received a copy of the GNU General Public License             *
* along with this program; if not, write to the Free Software                   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: Preferences.h,v 1.13 2006/01/22 17:58:15 fox Exp $                       *
********************************************************************************/
#ifndef PREFERENCES_H
#define PREFERENCES_H



class PathFinderMain;


// Preferences for PathFinder
class Preferences : public FXDialogBox {
  FXDECLARE(Preferences)
protected:
  FXText          *pattern;
  FXTextField     *editor;
  FXTextField     *terminal;
  FXCheckButton   *preview;
  FXCheckButton   *blending;
  FXText          *icondirs;
  FXList          *extensions;
  FXComboBox      *mimetypes;
  FXButton        *bigopen;
  FXButton        *bigclosed;
  FXButton        *smallopen;
  FXButton        *smallclosed;
  FXCheckButton   *runinterminal;
  FXCheckButton   *changedirectory;
  FXTextField     *description;
  FXTextField     *command;
  FXIcon          *brw;
  FXIcon          *pat;
  FXIcon          *icp;
  FXIcon          *mim;
  FXIcon          *dir;
private:
  Preferences(){}
  Preferences(const Preferences&);
  Preferences& operator=(const Preferences&);
public:
  long onCmdBrowseEditor(FXObject*,FXSelector,void*);
  long onCmdBrowseTerminal(FXObject*,FXSelector,void*);
  long onCmdBrowseCommand(FXObject*,FXSelector,void*);
  long onCmdBrowseIcon(FXObject*,FXSelector,void*);

  long onCmdCommand(FXObject*,FXSelector,void*);
  long onCmdMimeType(FXObject*,FXSelector,void*);
  long onCmdDescription(FXObject*,FXSelector,void*);

  long onCmdAppendExtension(FXObject*,FXSelector,void*);
  long onCmdRemoveExtension(FXObject*,FXSelector,void*);
  long onCmdSelectExtension(FXObject*,FXSelector,void*);
  long onUpdSelectExtension(FXObject*,FXSelector,void*);
public:
  enum{
    ID_BROWSE_EDITOR=FXDialogBox::ID_LAST,
    ID_BROWSE_TERMINAL,
    ID_BROWSE_COMMAND,
    ID_COMMAND,
    ID_MIMETYPE,
    ID_DESCRIPTION,
    ID_BROWSE_BIGICON,
    ID_BROWSE_SMALLICON,
    ID_BROWSE_BIGICONOPEN,
    ID_BROWSE_SMALLICONOPEN,
    ID_SELECT_EXTENSION,
    ID_APPEND_EXTENSION,
    ID_REMOVE_EXTENSION
    };
public:

  // Create preferences dialog
  Preferences(PathFinderMain *owner);

  // Get/set filename patterns
  void setPatterns(const FXString& pat){ pattern->setText(pat); }
  FXString getPatterns() const { return pattern->getText(); }

  // Get/set text editor
  void setEditor(const FXString& edit){ editor->setText(edit); }
  FXString getEditor() const { return editor->getText(); }

  // Get/set terminal
  void setTerminal(const FXString& term){ terminal->setText(term); }
  FXString getTerminal() const { return terminal->getText(); }

  // Set image preview
  void setPreview(FXbool prev){ preview->setCheck(prev); }
  FXbool getPreview() const { return preview->getCheck(); }

  // Set image preview
  void setBlend(FXbool blend){ blending->setCheck(blend); }
  FXbool getBlend() const { return blending->getCheck(); }

  // Get/set icon path
  void setIconPath(const FXString& text){ icondirs->setText(text); }
  FXString getIconPath() const { return icondirs->getText(); }

  // Clean up
  virtual ~Preferences();
  };

#endif

