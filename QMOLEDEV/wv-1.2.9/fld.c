/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include "wv.h"

/*
 flt value live/dead field type

 1                   unknown keyword

 2         live      possible bookmark (syntax matches bookmark name)

 3         live      bookmark reference

 4         dead      index entry

 5         live      footnote reference

 6         live      Set command (for Print Merge)

 7         live      If command (for Print Merge)

 8         live      create index

 9         dead      table of contents entry

 10        live      Style reference

 11        dead      document reference

 12        live      sequence mark

 13        live      create table-of-contents

 14        live      quote Info variable

 15        live      quote Title variable

 16        live      quote Subject variable

 17        live      quote Author variable

 18        live      quote Keywords variable

 19        live      quote Comments variable

 20        live      quote Last Revised By variable

 21        live      quote Creation Date variable

 22        live      quote Revision Date variable

 23        live      quote Print Date variable

 24        live      quote Revision Number variable

 25        live      quote Edit Time variable

 26        live      quote Number of Pages variable

 27        live      quote Number of Words variable

 28        live      quote Number of Characters variable

 29        live      quote File Name variable

 30        live      quote Document Template Name variable

 31        live      quote Current Date variable

 32        live      quote Current Time variable

 33        live      quote Current Page variable

 34        live      evaluate expression

 35        live      insert literal text

 36        live      Include command (Print Merge)

 37        live      page reference

 38        live      Ask command (Print Merge)

 39        live      Fill-in command to display prompt (Print Merge)

 40        live      Data command (Print Merge)

 41        live      Next command (Print Merge)

 42        live      NextIf command (Print Merge)

 43        live      SkipIf (Print Merge)

 44        live      inserts number of current Print Merge record

 45        live      DDE reference

 46        live      DDE automatic reference

 47        live      Inserts Glossary Entry

 48        live      sends characters to printer without translation

 49        live      Formula definition

 50        live      Goto Button

 51        live      Macro Button

 52        live      insert auto numbering field in outline format

 53        live      insert auto numbering field in legal format

 54        live      insert auto numbering field in Arabic number format

 55        live      reads a TIFF file

 56        live      Link

 57        live      Symbol

 58        live      Embedded Object

 59        live      Merge fields

 60        live      User Name

 61        live      User Initial

 62        live      User Address

 63        live      Bar code

 64        live      Document variable

 65        live      Section

 66        live      Section pages

 67        live      Include Picture

 68        live      Include Text

 69        live      File Size

 70        live      Form Text Box

 71        live      Form Check Box

 72        live      Note Reference

 73        live      Create Table of Authorities

 74        dead      Mark Table of Authorities Entry

 75        live      Merge record sequence number

 76        either    Macro

 77        dead      Private

 78        live      Insert Database

 79        live      Autotext

 80        live      Compare two values

 81        live      Plug-in module private

 82        live      Subscriber

 83        live      Form List Box

 84        live      Advance

 85        live      Document property

 86        live

 87        live      OCX

 88        live      Hyperlink

 89        live      AutoTextList

 90        live      List element

 91        live      HTML control

Since dead fields have no entry in the plcffld, the string in the field code
must be used to determine the field type. All versions of Word '97 use
English field code strings, except French, German, and Spanish versions of
Word. The strings for all languages for all possible dead fields are listed
below.

 flt      English    French     German     Spanish       field type
 value    string     string     string     string

 4        XE         EX         XE         E             index entry

 9        TC         TE         INHALT     TC            table of contents
                                                         entry

 11       RD         RD         RD         RD            document reference

 74       TA         TA         TA         TA            Mark Table of
                                                         Authorities Entry

 76                                                      Macro

 77       PRIVATE    PRIVE      PRIVATE    PRIVATESPA    Private
*/


void
wvGetFLD (FLD * item, wvStream * fd)
{
    U8 temp8;
    U8 ch;

    temp8 = read_8ubit (fd);
    ch = temp8 & 0x1f;
    if (ch == 19)
      {
	  item->var1.ch = temp8 & 0x1f;
	  item->var1.reserved = (temp8 & 0xe0) >> 5;
	  item->var1.flt = read_8ubit (fd);
      }
    else
      {
	  item->var2.ch = temp8 & 0x1f;
	  item->var2.reserved = (temp8 & 0xe0) >> 5;
	  temp8 = read_8ubit (fd);
	  item->var2.fDiffer = temp8 & 0x01;
	  item->var2.fZombieEmbed = (temp8 & 0x02) >> 1;
	  item->var2.fResultDirty = (temp8 & 0x04) >> 2;
	  item->var2.fResultEdited = (temp8 & 0x08) >> 3;
	  item->var2.fLocked = (temp8 & 0x10) >> 4;
	  item->var2.fPrivateResult = (temp8 & 0x20) >> 5;
	  item->var2.fNested = (temp8 & 0x40) >> 6;
	  item->var2.fHasSep = (temp8 & 0x80) >> 7;
      }
}


int
wvGetFLD_PLCF (FLD ** fld, U32 ** pos, U32 * nofld, U32 offset, U32 len,
	       wvStream * fd)
{
    U32 i;
    if (len == 0)
      {
	  *fld = NULL;
	  *pos = NULL;
	  *nofld = 0;
      }
    else
      {
	  *nofld = (len - 4) / 6;
	  *pos = (U32 *) wvMalloc ((*nofld + 1) * sizeof (U32));
	  if (*pos == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  (*nofld + 1) * sizeof (U32)));
		return (1);
	    }

	  *fld = (FLD *) wvMalloc (*nofld * sizeof (FLD));
	  if (*fld == NULL)
	    {
		wvError (
			 ("NO MEM 1, failed to alloc %d bytes\n",
			  *nofld * sizeof (FLD)));
		wvFree (pos);
		return (1);
	    }
	  wvStream_goto (fd, offset);
	  for (i = 0; i <= *nofld; i++)
	      (*pos)[i] = read_32ubit (fd);
	  for (i = 0; i < *nofld; i++)
	      wvGetFLD (&((*fld)[i]), fd);
      }
    return (0);
}
