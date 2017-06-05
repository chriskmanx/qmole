/**
 *
 * $Id: uil.h,v 1.1 2004/08/28 19:22:35 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 
#ifndef _UIL_H
#define _UIL_H

typedef struct {
	char Name[256];
	int lineno;
} FileData;

typedef struct {
  unsigned int width, height;
  char *ColorTable;
  char *data;
} PixmapType;

typedef struct {
  unsigned int width, height;
  int x_hot, y_hot;
  char *data;
} BitMapType;

typedef struct {
  int DirectionRtoL;
  int WideChar;
  char *name;
} FontSetType;

typedef struct _StringType {
  struct _StringType *Next;
  char IsAddr;
  char IsSeparator;
  char *theString;
  char *theFontSet;
} StringType;

typedef struct _ColorDefType {
  char *name;
  unsigned int r;
  unsigned int g;
  unsigned int b;
} ColorDefType;
 
enum { UID_COMPOUND_STRING, UID_ARGS, UID_INTEGER, UID_CALLBACK, UID_CREATE};

#endif /* _UIL_H */
