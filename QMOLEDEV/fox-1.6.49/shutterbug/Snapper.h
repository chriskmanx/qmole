/********************************************************************************
*                                                                               *
*                           S n a p p e r   W i d g e t                         *
*                                                                               *
*********************************************************************************
* Copyright (C) 2003,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
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
* $Id: Snapper.h,v 1.5 2006/01/22 17:58:16 fox Exp $                            *
********************************************************************************/
#ifndef SNAPPER_H
#define SNAPPER_H


// Red snapper
class Snapper : public FXShell {
  FXDECLARE(Snapper)
protected:
  Snapper();
  virtual bool doesOverrideRedirect() const;
private:
  Snapper(const Snapper&);
  Snapper& operator=(const Snapper&);
public:
  Snapper(FXApp* a,FXObject* tgt=NULL,FXSelector sel=0,FXuint opts=0,FXint x=0,FXint y=0,FXint w=0,FXint h=0);
  virtual FXint getDefaultWidth();
  virtual FXint getDefaultHeight();
  virtual bool doesSaveUnder() const;
  };


#endif
