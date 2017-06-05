#ifndef ORBIT_IDL2_H
#define ORBIT_IDL2_H 1

#include "orbit-idl3-types.h"

gboolean orbit_idl_to_backend     (const char    *filename,
				   OIDL_Run_Info *rinfo);

gboolean orbit_idl_backend_output (OIDL_Run_Info *rinfo,
				   IDL_tree       tree);

/* Utils */
void     orbit_idl_attr_fake_ops         (IDL_tree      attr,
					  IDL_ns        ns);
void     orbit_idl_print_node            (IDL_tree      node,
					  int           indent_level);
void     IDL_tree_traverse_parents       (IDL_tree      p,
					  GFunc         f,
					  gconstpointer func_data);
void     IDL_tree_traverse_parents_full  (IDL_tree      p,
					  GFunc         f,
					  gconstpointer func_data, 
					  gboolean      include_self);
gboolean orbit_cbe_type_contains_complex (IDL_tree      ts);
void     orbit_idl_check_oneway_op       (IDL_tree      op);

typedef enum { DATA_IN=1, DATA_INOUT=2, DATA_OUT=4, DATA_RETURN=8 } IDL_ParamRole;
gint oidl_param_info(IDL_tree param, IDL_ParamRole role, gboolean *isSlice);

gboolean orbit_cbe_type_is_fixed_length(IDL_tree ts);
IDL_tree orbit_cbe_get_typespec(IDL_tree node);
IDL_ParamRole oidl_attr_to_paramrole(enum IDL_param_attr attr);

#define ORBIT_RETVAL_VAR_NAME "_ORBIT_retval"
#define ORBIT_EPV_VAR_NAME    "_ORBIT_epv"

#endif /* ORBIT_IDL2_H */
