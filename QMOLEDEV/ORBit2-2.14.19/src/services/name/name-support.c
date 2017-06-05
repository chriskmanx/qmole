#include "name-support.h"

CosNaming_Name*
ORBit_string_to_CosNaming_Name (const CORBA_char *string,
				CORBA_Environment * ev)
{
  CosNaming_Name *retval = CosNaming_Name__alloc ();
  GPtrArray *ids = g_ptr_array_new ();
  GPtrArray *kinds = g_ptr_array_new ();
  gint pos = 0, i, len;
  gboolean used = FALSE;
  GPtrArray *append_to;

  g_ptr_array_add (ids, g_string_new (""));
  g_ptr_array_add (kinds, g_string_new (""));

  append_to = ids;

  while (*string)
    {
      gchar append;
      switch (*string)
	{
	case '.':
	  used = TRUE;
	  g_return_val_if_fail (append_to != kinds, NULL);  
	  append_to = kinds;
	  append = '\0';
	  break;
	case '/':
	  if (used)
	    {
	      pos++;
	      g_ptr_array_add (ids, g_string_new (""));
	      g_ptr_array_add (kinds, g_string_new (""));
	      g_assert (ids->len == pos + 1 && kinds->len == pos + 1);
	    }
	  used = FALSE;
	  append_to = ids;
	  append = '\0';
	  break;
	case '\\':
	  string++;
	  g_return_val_if_fail (*string == '.' || 
				*string == '/' || *string == '\\', NULL);  
	  append = *string;
	  break;
	default:
	  append = *string;
	  used = TRUE;
	  break;
	}

      if (append)
	g_string_append_c (g_ptr_array_index (append_to, pos), append);

      string++;
    }

  len = used ? pos + 1 : pos;

  retval->_buffer = CORBA_sequence_CosNaming_NameComponent_allocbuf (len);
  retval->_length = len;
  retval->_maximum = len;

  for (i = 0; i < len; i++)
    {  
      GString *id = g_ptr_array_index (ids, i);
      GString *kind = g_ptr_array_index (kinds, i);
      
      retval->_buffer[i].id = CORBA_string_dup (id->str);
      retval->_buffer[i].kind = CORBA_string_dup (kind->str);
    }
  
  for (i = 0; i <= pos; i++)
    {  
      g_string_free (g_ptr_array_index (ids, i), TRUE);
      g_string_free (g_ptr_array_index (kinds, i), TRUE);
    }

  g_ptr_array_free (ids, TRUE);
  g_ptr_array_free (kinds, TRUE);

  return retval;
}

CORBA_char *
ORBit_CosNaming_NameComponent_to_string (const CosNaming_NameComponent *comp)
{
  CORBA_char *retval;
  GString *str = g_string_new ("");
  CORBA_char *pos;
  gboolean id_ready = FALSE;

  if (!*comp->id && !*comp->kind)
    g_string_append_c (str, '.');
  else
    {
      pos = comp->id;
      while (!id_ready || *pos)
	{
	  if (!*pos)
	    {
	      /* id_ready must be FALSE */
	      id_ready = TRUE;
	      pos = comp->kind;
	      if (pos && *pos)
		g_string_append_c (str, '.');
	      continue;
	    }
	  if (*pos == '\\' || *pos == '.' || *pos == '/')
	    g_string_append_c (str, '\\');
	  g_string_append_c (str, *pos);
	  pos++;
	}
    }

  retval = CORBA_string_dup (str->str);
  g_string_free (str, TRUE);

  return retval;
}

CORBA_char *
ORBit_CosNaming_Name_to_string (const CosNaming_Name *name)
{
  GString *str = g_string_new ("");  
  CORBA_char *retval;
  gint i;

  for (i = 0; i < name->_length; i++)
    {
      CORBA_char *comp = 
	ORBit_CosNaming_NameComponent_to_string (&name->_buffer[i]);
      g_string_append_c (str, '/');
      g_string_append (str, comp);
    }
  retval = CORBA_string_dup (str->str);
  g_string_free (str, TRUE);

  return retval;
}

