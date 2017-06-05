/* ATK -  Accessibility Toolkit
 * Copyright 2001 Sun Microsystems Inc.
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
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <atk/atk.h>

#include <string.h>

static gboolean  test_state_set (void);
static gboolean  test_state (void);

static gboolean
test_state_set (void)
{
  AtkStateSet *state_set1, *state_set2, *state_set3;
  AtkStateType state_array[3];
  gboolean b_val;

  state_set1 = atk_state_set_new ();

  b_val = atk_state_set_is_empty (state_set1);  
  if (b_val)
  {
    g_print ("New state set is not empty\n");
    return FALSE;
  }

  b_val = atk_state_set_add_state (state_set1, ATK_STATE_ACTIVE);
  if (!b_val)
  {
    g_print ("Adding new state set failed\n");
    return FALSE;
  }

  b_val = atk_state_set_is_empty (state_set1);  
  if (!b_val)
  {
    g_print ("New state set is empty when it should not be\n");
    return FALSE;
  }

  b_val = atk_state_set_add_state (state_set1, ATK_STATE_ACTIVE);
  if (b_val)
  {
    g_print ("Adding new state set succeeded when it should not have\n");
    return FALSE;
  }

  state_array[0] = ATK_STATE_ACTIVE;
  state_array[1] = ATK_STATE_VISIBLE;
  state_array[2] = ATK_STATE_BUSY;
  atk_state_set_add_states (state_set1, state_array, 3);

  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_ACTIVE);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_ACTIVE but should not have\n");
    return FALSE;
  }
 
  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_VISIBLE);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_VISIBLE but should not have\n");
    return FALSE;
  }
 
  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_BUSY);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_BUSY but should not have\n");
    return FALSE;
  }
 
  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_VERTICAL);
  if (b_val)
  {
    g_print ("Contains state succeeded for ATK_STATE_VERTICAL but should not have\n");
    return FALSE;
  }
 
  atk_state_set_remove_state (state_set1, ATK_STATE_BUSY);
  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_BUSY);
  if (b_val)
  {
    g_print ("Contains state succeeded for ATK_STATE_BUSY but should not have\n");
    return FALSE;
  }
  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_VISIBLE);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_VISIBLE but should not have\n");
    return FALSE;
  }

  b_val = atk_state_set_contains_states (state_set1, state_array, 3);
  if (b_val)
  {
    g_print ("Contains states succeeded should not have\n");
    return FALSE;
  }

  b_val = atk_state_set_contains_states (state_set1, state_array, 2);
  if (!b_val)
  {
    g_print ("Contains states failed should not have\n");
    return FALSE;
  }

  state_array[0] = ATK_STATE_SINGLE_LINE;
  state_array[1] = ATK_STATE_VISIBLE;
  state_array[2] = ATK_STATE_VERTICAL;
 
  state_set2 = atk_state_set_new();
  atk_state_set_add_states (state_set2, state_array, 3);

  state_set3 = atk_state_set_and_sets (state_set1, state_set2);
  b_val = atk_state_set_contains_state (state_set3, ATK_STATE_VISIBLE);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_VISIBLE after and but should not have\n");
    return FALSE;
  }
  b_val = atk_state_set_contains_state (state_set3, ATK_STATE_BUSY);
  if (b_val)
  {
    g_print ("Contains state succeeded for ATK_STATE_BUSY after and but should not have\n");
    return FALSE;
  }
  g_object_unref (state_set3);

  atk_state_set_remove_state (state_set1, ATK_STATE_VISIBLE);
  state_set3 = atk_state_set_and_sets (state_set1, state_set2);
  if (state_set3)
  {
    g_print ("state_set 3 is not NULL after and but should be\n");
    return FALSE;
  }
 
  state_set3 = atk_state_set_or_sets (state_set1, state_set2);
  b_val = atk_state_set_contains_state (state_set3, ATK_STATE_VISIBLE);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_VISIBLE after or but should not have\n");
    return FALSE;
  }

  b_val = atk_state_set_contains_state (state_set3, ATK_STATE_INVALID);
  if (b_val)
  {
    g_print ("Contains state succeeded for ATK_STATE_INVALID after or but should not have\n");
    return FALSE;
  }
  g_object_unref (state_set3);

  b_val = atk_state_set_add_state (state_set1, ATK_STATE_VISIBLE);
  if (!b_val)
  {
    g_print ("Adding new state set failed\n");
    return FALSE;
  }
  state_set3 = atk_state_set_xor_sets (state_set1, state_set2);
  b_val = atk_state_set_contains_state (state_set3, ATK_STATE_VISIBLE);
  if (b_val)
  {
    g_print ("Contains state succeeded for ATK_STATE_VISIBLE after xor but should not have\n");
    return FALSE;
  }

  b_val = atk_state_set_contains_state (state_set3, ATK_STATE_ACTIVE);
  if (!b_val)
  {
    g_print ("Contains state failed for ATK_STATE_ACTIVE after xor but should not have\n");
    return FALSE;
  }

  atk_state_set_clear_states (state_set1);
  b_val = atk_state_set_contains_state (state_set1, ATK_STATE_ACTIVE);
  if (b_val)
  {
    g_print ("Contains state succeeded for ATK_STATE_ACTIVE but should not have\n");
    return FALSE;
  }

  g_object_unref (state_set1);
  g_object_unref (state_set2);
  g_object_unref (state_set3);
  return TRUE;

}

static gboolean
test_state (void)
{
  AtkStateType type1, type2;
  const gchar *name;

  name = atk_state_type_get_name (ATK_STATE_VISIBLE);
  g_return_val_if_fail (name, FALSE);
  if (strcmp (name, "visible") != 0)
  {
    g_print ("Unexpected name for ATK_STATE_VISIBLE %s\n", name);
    return FALSE;
  }

  name = atk_state_type_get_name (ATK_STATE_MODAL);
  g_return_val_if_fail (name, FALSE);
  if (strcmp (name, "modal") != 0)
  {
    g_print ("Unexpected name for ATK_STATE_MODAL %s\n", name);
    return FALSE;
  }

  type1 = atk_state_type_for_name ("focused");
  if (type1 != ATK_STATE_FOCUSED)
  {
    g_print ("Unexpected type for focused\n");
    return FALSE;
  }

  type1 = atk_state_type_register ("test-state");
  name = atk_state_type_get_name (type1);
  g_return_val_if_fail (name, FALSE);
  if (strcmp (name, "test-state") != 0)
  {
    g_print ("Unexpected name for test-state %s\n", name);
    return FALSE;
  }
  type2 = atk_state_type_for_name ("test-state");
  g_return_val_if_fail (name, FALSE);
  if (type1 != type2)
  {
    g_print ("Unexpected type for test-state %d %d\n", type1, type2);
    return FALSE;
  }
  type2 = atk_state_type_for_name ("TEST_STATE");
  if (type2 != 0)
  {
    g_print ("Unexpected type for TEST_STATE\n");
    return FALSE;
  }
  /*
   * Check that a non-existent type returns NULL
   */
  name = atk_state_type_get_name (ATK_STATE_LAST_DEFINED +2);
  if (name)
  {
    g_print ("Unexpected name for undefined type\n");
    return FALSE;
  }
  return TRUE;
}

int
gtk_module_init (gint argc, char* argv[])
{
  gboolean b_ret;

  g_print("State Set test module loaded\n");

  b_ret = test_state_set ();
  if (b_ret)
  {
    g_print ("State Set tests succeeded\n");
  }
  else
  {
    g_print ("State Set tests failed\n");
  }
  b_ret = test_state ();
  if (b_ret)
  {
    g_print ("State tests succeeded\n");
  }
  else
  {
    g_print ("State tests failed\n");
  }
  return 0;
}
