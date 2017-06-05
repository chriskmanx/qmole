/**
 *
 * $Id: MrmDecls.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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

#ifndef _MRM_MRMDECLS_H
#define _MRM_MRMDECLS_H

#ifdef __cplusplus
extern "C" {
#endif

extern const char _MrmMsg_0000[];
extern const char _MrmMsg_0001[];
extern const char _MrmMsg_0002[];
extern const char _MrmMsg_0003[];
extern const char _MrmMsg_0004[];
extern const char _MrmMsg_0005[];
extern const char _MrmMsg_0006[];
extern const char _MrmMsg_0007[];
extern const char _MrmMsg_0008[];
extern const char _MrmMsg_0009[];
extern const char _MrmMsg_0010[];
extern const char _MrmMsg_0011[];
extern const char _MrmMsg_0012[];
extern const char _MrmMsg_0013[];
extern const char _MrmMsg_0014[];
extern const char _MrmMsg_0015[];
extern const char _MrmMsg_0016[];
extern const char _MrmMsg_0017[];
extern const char _MrmMsg_0018[];
extern const char _MrmMsg_0019[];
extern const char _MrmMsg_0020[];
extern const char _MrmMsg_0021[];
extern const char _MrmMsg_0022[];
extern const char _MrmMsg_0023[];
extern const char _MrmMsg_0024[];
extern const char _MrmMsg_0025[];
extern const char _MrmMsg_0026[];
extern const char _MrmMsg_0027[];
extern const char _MrmMsg_0028[];
extern const char _MrmMsg_0029[];
extern const char _MrmMsg_0030[];
extern const char _MrmMsg_0031[];
extern const char _MrmMsg_0032[];
extern const char _MrmMsg_0033[];
extern const char _MrmMsg_0034[];
extern const char _MrmMsg_0035[];
extern const char _MrmMsg_0036[];
extern const char _MrmMsg_0037[];
extern const char _MrmMsg_0038[];
extern const char _MrmMsg_0039[];
extern const char _MrmMsg_0040[];
extern const char _MrmMsg_0041[];
extern const char _MrmMsg_0042[];
extern const char _MrmMsg_0043[];
extern const char _MrmMsg_0044[];
extern const char _MrmMsg_0045[];
extern const char _MrmMsg_0046[];
extern const char _MrmMsg_0047[];
extern const char _MrmMsg_0048[];
extern const char _MrmMsg_0049[];
extern const char _MrmMsg_0050[];
extern const char _MrmMsg_0051[];
extern const char _MrmMsg_0052[];
extern const char _MrmMsg_0053[];
extern const char _MrmMsg_0054[];
extern const char _MrmMsg_0055[];
extern const char _MrmMsg_0056[];
extern const char _MrmMsg_0057[];
extern const char _MrmMsg_0058[];
extern const char _MrmMsg_0059[];
extern const char _MrmMsg_0060[];
extern const char _MrmMsg_0061[];
extern const char _MrmMsg_0062[];
extern const char _MrmMsg_0063[];
extern const char _MrmMsg_0064[];
extern const char _MrmMsg_0065[];
extern const char _MrmMsg_0066[];
extern const char _MrmMsg_0067[];
extern const char _MrmMsg_0068[];
extern const char _MrmMsg_0069[];
extern const char _MrmMsg_0070[];
extern const char _MrmMsg_0071[];
extern const char _MrmMsg_0072[];
extern const char _MrmMsg_0073[];
extern const char _MrmMsg_0074[];
extern const char _MrmMsg_0075[];
extern const char _MrmMsg_0076[];
extern const char _MrmMsg_0077[];
extern const char _MrmMsg_0078[];
extern const char _MrmMsg_0079[];
extern const char _MrmMsg_0080[];
extern const char _MrmMsg_0081[];
extern const char _MrmMsg_0082[];
extern const char _MrmMsg_0083[];
extern const char _MrmMsg_0084[];
extern const char _MrmMsg_0085[];
extern const char _MrmMsg_0086[];
extern const char _MrmMsg_0087[];
extern const char _MrmMsg_0088[];
extern const char _MrmMsg_0089[];
extern const char _MrmMsg_0090[];
extern const char _MrmMsg_0091[];
extern const char _MrmMsg_0092[];
extern const char _MrmMsg_0093[];
extern const char _MrmMsg_0094[];
extern const char _MrmMsg_0095[];
extern const char _MrmMsg_0096[];
extern const char _MrmMsg_0097[];
extern const char _MrmMsg_0098[];
extern const char _MrmMsg_0099[];
extern const char _MrmMsg_0100[];
extern const char _MrmMsg_0101[];
extern const char _MrmMsg_0102[];
extern const char _MrmMsg_0103[];
extern const char _MrmMsg_0104[];
extern const char _MrmMsg_0105[];
extern const char _MrmMsg_0106[];
extern const char _MrmMsg_0107[];
extern const char _MrmMsg_0108[];
extern const char _MrmMsg_0109[];
extern const char _MrmMsg_0110[];


void MrmInitialize( void );

Cardinal MrmFetchLiteral(MrmHierarchy hierarchy_id,
				String index,
				Display *display,
				XtPointer *value_return,
				MrmCode *type_return);

Cardinal MrmFetchIconLiteral(MrmHierarchy hierarchy_id,
				    String index,
				    Screen *screen,
				    Display *display,
				    Pixel fgpix,
				    Pixel bgpix,
				    Pixmap *pixmap_return);

Cardinal MrmFetchBitmapLiteral(MrmHierarchy hierarchy_id,
				      String index,
				      Screen *screen,
				      Display *display,
				      Pixmap *pixmap_return,
				      Dimension *width,
				      Dimension *height);

Cardinal MrmFetchColorLiteral(MrmHierarchy hierarchy_id,
				     String index,
				     Display *display,
				     Colormap cmap,
				     Pixel *pixel_return);

Cardinal MrmOpenHierarchy(MrmCount num_files,
				 String *name_list,
				 MrmOsOpenParamPtr *os_ext_list,
				 MrmHierarchy *hierarchy_id_return);

Cardinal MrmOpenHierarchyFromBuffer(unsigned char uid_buffer,
                                    MrmHierarchy *hierarchy_id);

Cardinal MrmOpenHierarchyPerDisplay(Display *display,
					   MrmCount num_files,
					   String *name_list,
					   MrmOsOpenParamPtr *os_ext_list,
					   MrmHierarchy *hierarchy_id_return);

Cardinal MrmRegisterNames(MrmRegisterArglist reglist, MrmCount num_reg);

Cardinal MrmRegisterNamesInHierarchy(MrmHierarchy hierarchy_id,
					    MrmRegisterArglist reglist,
					    MrmCount num_reg);

/*
extern Cardinal MrmRegisterClass(MrmType class_code,
				 String class_name,
				 String create_name,
				 Widget (*creator)(),
				 WidgetClass class_record);

 * Is there a way to make the compiler happy?
 * Widget (*creator)()
 * gives "function declaration isn't a prototype",
 * but adding (...) for __STDC__ like for __cplusplus gives
 * "ANSI C requires a named argument before `...'".
 *
 * No, OSF/Motif seems to violate ANSI C here :-(
 * Our approach is therefore to fix the creator's signature
 * and advice later on to use casts. This shouldn't cause any harm ...
 */
Cardinal MrmRegisterClass(MrmType class_code,
				 String class_name,
				 String create_name,
				 Widget (*creator)(void),
				 WidgetClass class_record);

Cardinal MrmCloseHierarchy(MrmHierarchy hierarchy_id);

Cardinal MrmFetchInterfaceModule(MrmHierarchy hierarchy_id ,
					char *module_name,
					Widget parent,
					Widget *w_return);

Cardinal MrmFetchWidget(MrmHierarchy hierarchy_id,
			       String index,
			       Widget parent,
			       Widget *w_return,
			       MrmType *class_return);

Cardinal MrmFetchWidgetOverride(MrmHierarchy hierarchy_id,
				       String index,
				       Widget parent,
				       String ov_name,
				       ArgList ov_args,
				       Cardinal ov_num_args,
				       Widget *w_return,
				       MrmType *class_return);

Cardinal MrmFetchSetValues(MrmHierarchy hierarchy_id,
				  Widget w,
				  ArgList args,
				  Cardinal num_args);

#ifdef __cplusplus
}
#endif

#endif /* _MRM_MRMDECLS_H */
