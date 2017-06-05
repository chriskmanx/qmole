#ifndef ORBIT_IDL3_TYPES_H
#define ORBIT_IDL3_TYPES_H 1

#include <errno.h>
#include <stdio.h>
#include <libIDL/IDL.h>
#include <orbit/util/orbit-util.h>
#include <orbit/orbit-config.h>

typedef struct _OIDL_Marshal_Context OIDL_Marshal_Context;

#define OUTPUT_NUM_PASSES 7

typedef struct {
  char *cpp_args;
  int debug_level;
  int idl_warn_level;
  int show_cpp_errors;
  int is_pidl;
  int do_skel_defs;	/* gen defs within the header file */

  enum { OUTPUT_STUBS=1<<0,
	 OUTPUT_SKELS=1<<1,
	 OUTPUT_COMMON=1<<2,
	 OUTPUT_HEADERS=1<<3,
	 OUTPUT_SKELIMPL=1<<4,
	 OUTPUT_IMODULE=1<<5,
	 OUTPUT_DEPS=1<<6 /* Make sure this is always the last pass or dep output will break. */
  } enabled_passes;

  char *output_language;
  char *input_filename;
  char *backend_directory;
  char *deps_file;
  char *header_guard_prefix;
  char *output_directory;
  gboolean onlytop;
  gboolean idata;

  IDL_ns ns; /* Use ns instead of namespace because that's a C++ reserved keyword */
} OIDL_Run_Info;

typedef struct {
  IDL_tree op1;
  IDL_tree op2;
} OIDL_Attr_Info;

#endif
