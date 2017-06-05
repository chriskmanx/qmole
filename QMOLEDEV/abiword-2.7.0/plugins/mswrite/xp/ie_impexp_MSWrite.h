/* AbiWord
 * Copyright (C) 2001 Sean Young <sean@mess.org>
 * Copyright (C) 2001 Hubert Figuiere
 * Copyright (C) 2001 Dom Lachowicz
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  
 * 02111-1307, USA.
 */

#ifndef __IE_IMPEXP_MSWRITE_H__
#define __IE_IMPEXP_MSWRITE_H__

#include <stdio.h>
#include "ut_types.h"

/*****************************************************************/
/* Write file format definitions                                 */
/*****************************************************************/

/* 
 * first of all, the general structure for all structures.
 */

typedef struct wri_struct {
    int		value;
    char	*data;
    short	size;	/* in bytes */
    short	type;
    const char	*name;	
} wri_struct;

#define	CT_IGNORE	0
#define CT_VALUE	1
#define CT_BLOB		2

int read_wri_struct (struct wri_struct *cfg, GsfInput *f);
int read_wri_struct_mem (struct wri_struct *cfg, unsigned char*);
void dump_wri_struct (struct wri_struct *cfg);
void free_wri_struct (struct wri_struct *cfg);
int wri_struct_value (const struct wri_struct *cfg, const char *name);

/* helper macros word endianness -- Endian neutral they are. Swear ! */
#define READ_WORD(p)	((*(p)) + ((*((p)+1)) << 8))

#define READ_DWORD(p)	((*(p)) + (((*((p)+1)) << 8)) + (((*((p)+2)) << 16)) + \
			(((*((p)+3)) << 24)))

// wri_ole.h
int wri_ole_read (unsigned char *data, int size, GsfOutput *f);

#endif /* IE_IMPEXP_MSWRITE_H */








