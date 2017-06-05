#include "config.h"

#include "orbit-idl-c-backend.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <glib/gstdio.h>

static FILE *out_for_pass(const char *input_filename, int pass, 
			  OIDL_Run_Info *rinfo);

gboolean
orbit_idl_output_c (IDL_tree       tree,
		    OIDL_Run_Info *rinfo)
{
  int i;
  char *ctmp;
  OIDL_C_Info ci;

  ci.base_name = g_path_get_basename(rinfo->input_filename);
  ctmp = strrchr(ci.base_name, '.');
  g_assert(ctmp);
  *ctmp = '\0';

  ci.c_base_name = g_strdup(ci.base_name);
  if(!isalpha((guchar)ci.c_base_name[0]))
    ci.c_base_name[0] = '_';
  for(i = 0; ci.c_base_name[i]; i++) {
    if(!isalnum((guchar)ci.c_base_name[i])) ci.c_base_name[i] = '_';
  }

  ci.ext_dcls = g_string_new(NULL);

  ci.do_impl_hack = 1;
  ci.do_skel_defs = rinfo->do_skel_defs;
  for(i = 0; i < OUTPUT_NUM_PASSES; i++) {
    if( (1 << i) & rinfo->enabled_passes) {
      ci.fh = out_for_pass(rinfo->input_filename, 1 << i, rinfo);
      
      switch(1 << i) {
      case OUTPUT_STUBS:
	orbit_idl_output_c_stubs(tree, rinfo, &ci);
	break;
      case OUTPUT_SKELS:
	orbit_idl_output_c_skeletons(tree, rinfo, &ci);
	break;
      case OUTPUT_COMMON:
	orbit_idl_output_c_common(tree, rinfo, &ci);
	break;
      case OUTPUT_HEADERS:
	orbit_idl_output_c_headers(tree, rinfo, &ci);
	break;
      case OUTPUT_SKELIMPL:
	orbit_idl_output_c_skelimpl(tree, rinfo, &ci);
	break;
      case OUTPUT_IMODULE:
	orbit_idl_output_c_imodule(tree, rinfo, &ci);
	break;
      case OUTPUT_DEPS:
	orbit_idl_output_c_deps(tree, rinfo, &ci);
	break;
      }
      fclose(ci.fh);
    }
  }
  g_string_free(ci.ext_dcls,TRUE);

  return TRUE;
}

char *
orbit_idl_c_filename_for_pass (const char *input_filename, 
                               int pass)
{
	char *filename;
	char *basename;
	char *dot;
	const char *tack_on = NULL;
  
	basename = g_path_get_basename (input_filename);
	dot = strrchr (basename, '.');
	if (dot != NULL)
		*dot = '\0';

	switch (pass) {
	case OUTPUT_STUBS:
		tack_on = "-stubs.c";
		break;
	case OUTPUT_SKELS:
		tack_on = "-skels.c";
		break;
	case OUTPUT_COMMON:
		tack_on = "-common.c";
		break;
	case OUTPUT_HEADERS:
		tack_on = ".h";
		break;
	case OUTPUT_SKELIMPL:
		tack_on = "-skelimpl.c";
		break;
	case OUTPUT_IMODULE:
		tack_on = "-imodule.c";
		break;
	default:
		g_error("Unknown output pass");
		break;
	}

	filename = g_strconcat (basename, tack_on, NULL);
	g_free (basename);

	return filename;
}

static FILE *
out_for_pass (const char    *input_filename,
	      int            pass,
	      OIDL_Run_Info *rinfo)
{
	FILE *fp;
	char *output_filename;
	gchar *output_full_path = NULL;


        if ((strlen(rinfo->output_directory)) && (!g_file_test (rinfo->output_directory, G_FILE_TEST_IS_DIR))) {
		g_error ("ouput directory '%s' does not exist",
			 rinfo->output_directory);
		return NULL;
	}
	
	if (pass == OUTPUT_DEPS) {
		if (!g_file_test (".deps", G_FILE_TEST_IS_DIR)) {
			if (g_mkdir (".deps", 0775) < 0) {
				g_warning ("failed to create '.deps' directory '%s'",
					   g_strerror (errno));
				return NULL;
			}
		}
		
		if (rinfo->deps_file)
			fp =  g_fopen (rinfo->deps_file, "w");
		else
			fp = NULL;

		if (fp == NULL) 
			g_warning ("failed to open '%s': %s\n",
				   rinfo->deps_file, g_strerror (errno));
		
	} else {
		output_filename = orbit_idl_c_filename_for_pass (input_filename, pass);
		output_full_path = g_build_path (G_DIR_SEPARATOR_S, rinfo->output_directory, output_filename, NULL);
		g_free (output_filename);

		fp = g_fopen (output_full_path, "w+");
		if (fp == NULL)
			g_error ("failed to fopen '%s': %s\n", output_full_path, g_strerror(errno));

		g_free (output_full_path);
	}

	return fp;
}
