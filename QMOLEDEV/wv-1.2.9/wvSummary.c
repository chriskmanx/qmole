/* wvWare
 * Copyright (C) Dom Lachowicz and others
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

#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-msole-utils.h>
#include <gsf/gsf-docprop-vector.h>
#include <gsf/gsf-meta-names.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

/* prepare for i18n */
#define N_(X) (X)
#define _(X) (X)

static const struct {
  char * metadata_key;
  char * human_readable_key;
} metadata_names[] = {
  { GSF_META_NAME_TITLE, N_("Title") },
  { GSF_META_NAME_DESCRIPTION, N_("Description") },
  { GSF_META_NAME_SUBJECT, N_("Subject") },
  { GSF_META_NAME_DATE_MODIFIED, N_("Last Modified") },
  { GSF_META_NAME_DATE_CREATED, N_("Created") },
  { GSF_META_NAME_KEYWORDS, N_("Keywords") },
  { GSF_META_NAME_LANGUAGE, N_("Language") },
  { GSF_META_NAME_REVISION_COUNT, N_("Revision") },
  { GSF_META_NAME_EDITING_DURATION, N_("Editing Duration") },
  { GSF_META_NAME_TABLE_COUNT, N_("Number of Tables") },
  { GSF_META_NAME_IMAGE_COUNT, N_("Number of Images") },
  { GSF_META_NAME_OBJECT_COUNT, N_("Number of Objects") },
  { GSF_META_NAME_PAGE_COUNT, N_("Number of Pages") },
  { GSF_META_NAME_PARAGRAPH_COUNT, N_("Number of Paragraphs") },
  { GSF_META_NAME_WORD_COUNT, N_("Number of Words") },
  { GSF_META_NAME_CHARACTER_COUNT, N_("Number of Characters") },
  { GSF_META_NAME_CELL_COUNT, N_("Number of Spreadsheet Cells") },
  { GSF_META_NAME_SPREADSHEET_COUNT, N_("Number of Sheets") },
  { GSF_META_NAME_CREATOR, N_("Creator") },
  { GSF_META_NAME_TEMPLATE, N_("Template") },
  { GSF_META_NAME_LAST_SAVED_BY, N_("Last Saved by") },
  { GSF_META_NAME_LAST_PRINTED, N_("Last Printed") },
  { GSF_META_NAME_SECURITY, N_("Security Level") },
  { GSF_META_NAME_CATEGORY, N_("Category") },
  { GSF_META_NAME_PRESENTATION_FORMAT, N_("") },
  { GSF_META_NAME_THUMBNAIL, N_("Thumbnail") },
  { GSF_META_NAME_GENERATOR, N_("Generator") },
  { GSF_META_NAME_LINE_COUNT, N_("Number of Lines") },
  { GSF_META_NAME_SLIDE_COUNT, N_("Number of Slides") },
  { GSF_META_NAME_NOTE_COUNT, N_("Number of Notes") },
  { GSF_META_NAME_HIDDEN_SLIDE_COUNT, N_("Number of Hidden Slides") },
  { GSF_META_NAME_MM_CLIP_COUNT, N_("Number of 'Multi-Media' Clips") },
  { GSF_META_NAME_BYTE_COUNT, N_("Number of Bytes in the Document") },
  { GSF_META_NAME_SCALE, N_("Scale") },
  { GSF_META_NAME_HEADING_PAIRS, N_("Document Pairs") },
  { GSF_META_NAME_DOCUMENT_PARTS, N_("Document Parts") },
  { GSF_META_NAME_MANAGER, N_("Manager") },
  { GSF_META_NAME_COMPANY, N_("Company") },
  { GSF_META_NAME_LINKS_DIRTY, N_("Links Dirty") },
  { GSF_META_NAME_MSOLE_UNKNOWN_17, N_("Unknown1") },
  { GSF_META_NAME_MSOLE_UNKNOWN_18, N_("Unknown2") },
  { GSF_META_NAME_MSOLE_UNKNOWN_19, N_("Unknown3") },
  { GSF_META_NAME_MSOLE_UNKNOWN_20, N_("Unknown4") },
  { GSF_META_NAME_MSOLE_UNKNOWN_21, N_("Unknown5") },
  { GSF_META_NAME_MSOLE_UNKNOWN_22, N_("Unknown6") },
  { GSF_META_NAME_MSOLE_UNKNOWN_23, N_("Unknown7") },
  { GSF_META_NAME_DICTIONARY, N_("Dictionary") },
  { GSF_META_NAME_LOCALE_SYSTEM_DEFAULT, N_("Default Locale") },
  { GSF_META_NAME_CASE_SENSITIVE, N_("Case Sensitive") }
};
static const gsize nr_metadata_names = G_N_ELEMENTS(metadata_names);

static void
cb_print_property (char const *name, GsfDocProp const *prop, GHashTable * human_readable_names)
{
  GValue const *val = gsf_doc_prop_get_val  (prop);
  char *tmp;
  char const * _name;

  if((_name = g_hash_table_lookup(human_readable_names, name)) == NULL)
    _name = name;
  
  if (gsf_doc_prop_get_link (prop) != NULL)
    fprintf (stdout, "\t%s LINKED TO  -> '%s'\n",
	     _name, gsf_doc_prop_get_link (prop));
  else
    fprintf (stdout, "\t%s = ", _name);
  
  if (VAL_IS_GSF_DOCPROP_VECTOR ((GValue *)val)) {
    GValueArray *va = gsf_value_get_docprop_varray (val);
    unsigned i;
    
    fprintf(stdout, "[");
    for (i = 0 ; i < va->n_values; i++) {
      tmp = g_strdup_value_contents (g_value_array_get_nth (va, i));
      if(i != 0)
	fprintf(stdout, ", ");
      fprintf (stdout, "(%u, %s)", i, tmp);
      g_free (tmp);
    }
    fprintf(stdout, "]");
  } else {
    tmp = g_strdup_value_contents (val);
    fprintf (stdout, "%s", tmp);
    g_free (tmp);
  }

  fprintf (stdout, "\n");
}

static void print_summary_stream (GsfInfile * msole,
				  const char * file_name,
				  const char * stream_name,
				  GHashTable * human_readable_names)
{
  GsfInput * stream = gsf_infile_child_by_name (msole, stream_name);
  if (stream != NULL) {
    GsfDocMetaData *meta_data = gsf_doc_meta_data_new ();
    GError    *err = NULL;    

    err = gsf_msole_metadata_read (stream, meta_data);
    if (err != NULL) {
      g_warning ("Error getting metadata for %s->%s: %s", 
		 file_name, stream_name, err->message);
      g_error_free (err);
      err = NULL;
    } else
      gsf_doc_meta_data_foreach (meta_data,
				 (GHFunc) cb_print_property, human_readable_names);
    
    g_object_unref (meta_data);
    g_object_unref (G_OBJECT (stream));
  }
}

int
main (int argc, char *argv[])
{
  GHashTable * human_readable_keys;
  int i;

  if (argc < 2)
      {
	fprintf (stderr, _("Usage: wvSummary doc1 [... docN]\n"));
	return 1;
      }
  
  wvInit();
  human_readable_keys = g_hash_table_new (g_str_hash,
					  g_str_equal);

  for (i = 0; i < nr_metadata_names; i++)
    g_hash_table_insert (human_readable_keys,
			 metadata_names[i].metadata_key,
			 _(metadata_names[i].human_readable_key));

  for (i = 1 ; i < argc ; i++)
    {      
      GsfInput  *input;
      GsfInfile *msole;
      GError    *err = NULL;

      input = gsf_input_stdio_new (argv[i], &err);

      if(!input)
	{
	  fprintf (stderr, _("Problem with getting metadata from %s:%s\n"),
		   argv[i], err ? err->message : "");
	  g_error_free (err);
	  continue;
	}

      input = gsf_input_uncompress (input);
      msole = gsf_infile_msole_new (input, &err);
      if(!msole)
	{
	  fprintf (stderr, _("Problem with getting metadata from %s:%s\n"),
		   argv[i], err ? err->message : "");
	  g_error_free (err);
	  continue;
	}

      fprintf (stdout, _("Metadata for %s:\n"), argv[i]);      
      print_summary_stream (msole, argv[i], "\05SummaryInformation", human_readable_keys);
      print_summary_stream (msole, argv[i], "\05DocumentSummaryInformation", human_readable_keys);
      
      g_object_unref (G_OBJECT (msole));
      g_object_unref (G_OBJECT (input));
    }

  g_hash_table_destroy (human_readable_keys);
  wvShutdown();

  return 0;
}
