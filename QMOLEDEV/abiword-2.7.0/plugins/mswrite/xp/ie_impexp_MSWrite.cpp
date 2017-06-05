/* AbiWord
 * Copyright (C) 2001 Sean Young <sean@mess.org>
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

#include <stdlib.h>
#include <string.h>
#include <memory.h>
/*#include <png.h>*/
#include <gsf/gsf-input.h>
#include <gsf/gsf-output.h>
#include "ie_impexp_MSWrite.h"

#include "ut_debugmsg.h"
#include "ut_assert.h"

/***************************************************************************/
/* wri_struct.c */
/***************************************************************************/

int read_wri_struct_mem (struct wri_struct *cfg, unsigned char *blob) {
    int i, n;

    for (i=0;cfg[i].name;i++) {
	switch (cfg[i].type) {
	case CT_VALUE:
	    n = cfg[i].size;
	    cfg[i].value = 0;
	    while (n--) 
		cfg[i].value = (cfg[i].value * 256) + blob[n];
	    break;
	case CT_BLOB:
	    cfg[i].data = static_cast<char*>(malloc (cfg[i].size));
	    if (!cfg[i].data) {
		fprintf (stderr, "Out of memory!\n");
		return 1;
	    }
	    memcpy (cfg[i].data, blob, cfg[i].size);
	    break;
	case CT_IGNORE:
	    break;
	}
	blob += cfg[i].size;
    }
    return 0;
}

int read_wri_struct (struct wri_struct *cfg, GsfInput *f) {
    int size, i;
    unsigned char *blob;

    /* first we need to calculate the size */
    i = size = 0;
    while (cfg[i].name) size += cfg[i++].size;

    /* got the size, read the blob */
    blob = static_cast<unsigned char*>(malloc (size));
    if (!blob) {
	fprintf (stderr, "Out of memory!\n");
	return 1;
    }
    if (!gsf_input_read(f, size, blob)) {
	fprintf (stderr, "File not big enough!\n");
	return 1;
    }
    
    size = read_wri_struct_mem (cfg, blob);
    free (blob);

    return size;
}

void dump_wri_struct (struct wri_struct *cfg) {
    int i = 0;

    while (cfg[i].name) {
	switch (cfg[i].type) {
	case CT_VALUE:
	    printf ("%s:\t%x\n", cfg[i].name, cfg[i].value);
	    break;
	case CT_IGNORE:
	    printf ("%s:\tignored\n", cfg[i].name);
	    break;
	case CT_BLOB:
	    printf ("%s:\tblob (%d)\n", cfg[i].name, cfg[i].size);
	    break;
	}
	i++;
    }
}

void free_wri_struct (struct wri_struct *cfg) {
    int i = 0;
    while (cfg[i].name) {
	if (cfg[i].data) free (cfg[i].data);
	i++;
    }
}
   
int wri_struct_value (const struct wri_struct *cfg, const char *name) {
    int  i = 0;
    while (cfg[i].name) {
	if (!strcmp (cfg[i].name, name) ) return cfg[i].value;
	i++;
    }
    /* this shouldn't happen! */
    printf ("%s not found, internal error.\n", name);
    exit (1);
    return 0;
}

/***************************************************************************/
/* wri_ole.c */
/***************************************************************************/

#if 0
/* OLE stuff */

static struct wri_struct write_ole[] = {
        /* value, data, size, type, name */		/* word no. */
	{ 0,	NULL, 	2,  CT_VALUE, "mm" },		/* 0 */
	{ 0, 	NULL,	4,  CT_IGNORE, "not_used" },	/* 1-2 */
	{ 0,	NULL, 	2,  CT_VALUE, "objectType" },	/* 3 */
	{ 0,	NULL,	2,  CT_VALUE, "dxaOffset" },	/* 4 */
	{ 0,	NULL,	2,  CT_VALUE, "dxaSize" },	/* 5 */
	{ 0,	NULL,	2,  CT_VALUE, "dyaSize" },	/* 6 */
	{ 0,	NULL,	2,  CT_IGNORE, "not_used2" },	/* 7 */
	{ 0,	NULL,	4,  CT_VALUE, "dwDataSize" },	/* 8-9 */
	{ 0, 	NULL,	4,  CT_IGNORE, "not_used3" },	/* 10-11 */
	{ 0, 	NULL,	4,  CT_VALUE, "dwObjNum" },	/* 12-13 */
	{ 0,	NULL,	2,  CT_VALUE, "not_used4" },	/* 14 */
	{ 0,	NULL,	2,  CT_VALUE, "cbHeader" },	/* 15 */
	{ 0,	NULL,   4,  CT_IGNORE, "not_used5" },	/* 16-17 */
	{ 0,	NULL,	2,  CT_VALUE, "mx" },		/* 18 */
	{ 0,	NULL,	2,  CT_VALUE, "my" },		/* 19 */
        { 0,    NULL,   0,  CT_IGNORE, NULL }           /* EOF */
};
#endif 

int wri_ole_read (unsigned char */*data*/, int /*size*/, GsfOutput */*fout*/) {
#if 0
   FILE *f; 

    read_wri_struct_mem (write_ole, data);
    dump_wri_struct (write_ole);

    f = fopen ("ole.dump", "wb");
    fwrite (data + 40, 1, wri_struct_value (write_ole, "dwDataSize"), f);
    fclose (f);
#endif
    
    return 0;
}
