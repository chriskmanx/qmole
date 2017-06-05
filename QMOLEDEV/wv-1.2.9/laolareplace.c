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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wv.h"

#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-input-stdio.h>

int wvOLEDecode_gsf (wvParseStruct * ps, GsfInput *path, wvStream ** mainfd, wvStream ** tablefd0,
		     wvStream ** tablefd1, wvStream ** data, wvStream ** summary)
{
    GsfInfile *ole_file = NULL;
    int result = 5;

    if (!path) {
      return result;
    }

    ole_file = gsf_infile_msole_new (path, NULL);

    if (ole_file != NULL)
      {
	  GsfInput *temp_stream;

	  ps->ole_file = GSF_INPUT(ole_file);

	  wvTrace (("Opened VFS\n"));
	  if ((temp_stream = gsf_infile_child_by_name (ole_file, "WordDocument")) == NULL)
	    {
		*mainfd = NULL;
		wvTrace (("Opening \"WordDocument\" stream\n"));
	    }
	  else
	    {
		wvTrace (("Opened \"WordDocument\" stream\n"));
		wvStream_gsf_create (mainfd, temp_stream);
	    }

	  if ((temp_stream = gsf_infile_child_by_name (ole_file, "1Table")) == NULL)
	    {
		*tablefd1 = NULL;
		wvTrace (("Opening \"1Table\" stream\n"));
	    }
	  else
	    {
		wvTrace (("Opened \"1Table\" stream\n"));
		wvStream_gsf_create (tablefd1, temp_stream);
	    }

	  if ((temp_stream = gsf_infile_child_by_name (ole_file, "0Table")) == NULL)
	    {
		*tablefd0 = NULL;
		wvTrace (("Opening \"0Table\" stream\n"));
	    }
	  else
	    {
		wvTrace (("Opened \"0Table\" stream\n"));
		wvStream_gsf_create (tablefd0, temp_stream);

	    }
	  
	  if ((temp_stream = gsf_infile_child_by_name (ole_file, "Data")) == NULL)
	    {
		*data = NULL;
		wvTrace (("Opening \"Data\" stream\n"));
	    }
	  else
	    {
		wvTrace (("Opened \"Data\" stream\n"));
		wvStream_gsf_create (data, temp_stream);
	    }

	  if ((temp_stream = gsf_infile_child_by_name (ole_file, "\005SummaryInformation")) == NULL)
	    {
		*summary = NULL;
		wvTrace (("Opening \"\\005SummaryInformation\" stream\n"));
	    }
	  else
	    {
		wvTrace (("Opened \"\\005SummaryInformation\" stream\n"));
		wvStream_gsf_create (summary, temp_stream);
	    }

	  result = 0;
      }

    return (result);
}

int
wvOLEDecode (wvParseStruct * ps, char *path, wvStream ** mainfd, wvStream ** tablefd0,
	     wvStream ** tablefd1, wvStream ** data, wvStream ** summary)
{
  GsfInput * input = gsf_input_stdio_new (path, NULL);

  int rval = wvOLEDecode_gsf (ps, input, mainfd, tablefd0, tablefd1, data, summary);
  
  return rval;
}
