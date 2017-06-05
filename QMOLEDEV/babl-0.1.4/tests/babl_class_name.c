/* babl - dynamically extendable universal pixel conversion library.
 * Copyright (C) 2005, Øyvind Kolås.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public License along with this library; if not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include <string.h>
#include <stdio.h>
#include "babl-internal.h"

struct
{
  long klass;                          const char *name;
} reference[] = {
  { BABL_INSTANCE,          "BablInstance"         },
  { BABL_TYPE,              "BablType"             },
  { BABL_TYPE_INTEGER,      "BablTypeInteger"      },
  { BABL_TYPE_FLOAT,        "BablTypeFloat"        },
  { BABL_SAMPLING,          "BablSampling"         },
  { BABL_COMPONENT,         "BablComponent"        },
  { BABL_MODEL,             "BablModel"            },
  { BABL_FORMAT,            "BablFormat"           },
  { BABL_CONVERSION,        "BablConversion"       },
  { BABL_CONVERSION_LINEAR, "BablConversionLinear" },
  { BABL_CONVERSION_PLANE,  "BablConversionPlane"  },
  { BABL_CONVERSION_PLANAR, "BablConversionPlanar" },
  { BABL_FISH,              "BablFish"             },
  { BABL_FISH_REFERENCE,    "BablFishReference"    },
  { BABL_IMAGE,             "BablImage"            },
  { BABL_SKY,               "BablSky"              },
  { 0,                      NULL                   }
};

static int
test (void)
{
  int i  = 0;
  int OK = 1;

  while (reference[i].klass)
    {
      if (strcmp (reference[i].name, babl_class_name (reference[i].klass)))
        {
          OK = 0;
          babl_log ("'%s'!='%s'\n", reference[i].name, babl_class_name (reference[i].klass));
        }
      i++;
    }
  return !OK;
}

int
main (int    argc,
      char **argv)
{
  babl_init ();
  if (test ())
    return -1;
  babl_exit ();
  return 0;
}
